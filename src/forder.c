#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

// #define TIMING_ON

// For dev test only

// Only fastorder() is meant for use by other C code in data.table, hence all functions here other than fastorder are static.
// They make use of global variables, so access must be via fastorder only.
// The coding techniques deployed here are for efficiency.

static int *gs[2] = {NULL};    // gs = groupsizes e.g. 23,12,87,2,1,34,...
static int flip = 0;           // two vectors flip flopped: flip and 1-flip
static int gsalloc[2] = {0};   // allocated stack size
static int gsngrp[2] = {0};    // used
static int gsmax[2] = {0};     // max grpn so far
static int gsmaxalloc = 0;     // max size of stack, set by forder to nrows
static Rboolean stackgrps = TRUE;  // switched off for last column when not needed by setkey
static Rboolean sortStr = TRUE;    // TRUE for setkey, FALSE for by=

static void growstack(int newlen) {
    if (newlen==0) newlen=100000;   // no link to icount range restriction, just 100,000 seems a good minimum at 0.4MB.
    if (newlen>gsmaxalloc) newlen=gsmaxalloc;
    gs[flip] = realloc(gs[flip], newlen*sizeof(int));
    if (gs[flip] == NULL) error("Failed to realloc working memory stack to %d*4bytes (flip=%d)", newlen, flip);
    gsalloc[flip] = newlen;
}

static void push(int x) {
    if (!stackgrps) return;
    if (gsalloc[flip] == gsngrp[flip]) growstack(gsngrp[flip]*2);
    gs[flip][gsngrp[flip]++] = x;
    if (x > gsmax[flip]) gsmax[flip] = x;
}

static void mpush(int x, int n) {
    if (!stackgrps) return;
    if (gsalloc[flip] < gsngrp[flip]+n) growstack((gsngrp[flip]+n)*2);
    for (int i=0; i<n; i++) gs[flip][gsngrp[flip]++] = x;
    if (x > gsmax[flip]) gsmax[flip] = x;
}

static void flipflop() {
    flip = 1-flip;
    gsngrp[flip] = 0;
    gsmax[flip] = 0;
    if (gsalloc[flip] < gsalloc[1-flip]) growstack(gsalloc[1-flip]*2);
}

static void gsfree() {
    free(gs[0]); free(gs[1]);
    gs[0] = NULL; gs[1]= NULL;
    flip = 0;
    gsalloc[0] = gsalloc[1] = 0;
    gsngrp[0] = gsngrp[1] = 0;
    gsmax[0] = gsmax[1] = 0;
    gsmaxalloc = 0;
}

#ifdef TIMING_ON
  // many calls to clock() can be expensive, hence compiled out rather than switch(verbose) 
  #include <time.h>
  #define NBLOCK 20
  static void binary(unsigned long long n);
  static clock_t tblock[NBLOCK], tstart;
  static int nblock[NBLOCK];
  #define TBEG() tstart = clock();
  #define TEND(i) tblock[i] += clock()-tstart; nblock[i]++; tstart = clock();
#else
  #define TBEG()
  #define TEND(i)
#endif



static int range, off;   // used by both icount and forder

static void setRange(int *x, int n)
{
    int i, tmp;
    int xmin = NA_INTEGER, xmax = NA_INTEGER;
    
    off = 0;   // nalast^decreasing ? 0 : 1;
    i = 0;
    while(i<n && x[i]==NA_INTEGER) i++;
    if (i<n) xmax = xmin = x[i];
    for(; i < n; i++) {
	    tmp = x[i];
	    if(tmp == NA_INTEGER) continue;
        // TO DO: test and document that negatives are fine. Have filed wish to remove one line error("negatives") from base sort.c
	    if (tmp > xmax) xmax = tmp;
	    else if (tmp < xmin) xmin = tmp;
    }
    if(xmin == NA_INTEGER) {range = NA_INTEGER; return;} // all NAs, nothing to do
    range = xmax-xmin+1;
    off = -xmin+1;
    return;
}

static void icount(int *x, int *o, int n)
   // Places the ordering into o directly, overwriting whatever was there
   // Doesn't change x
   // Pushes group sizes onto stack
{
    int i=0, tmp;
    int napos = 0;   // increasing ? 0 : range
    static unsigned int counts[100000+1] = {0};
    // static is important, countingsort called repetitively. counts are set back to 0 at the end efficiently.
    // 1e5 = 0.4MB i.e tiny. We'll only use the front part of it, as large as range. So it's just reserving space, not using it.
    // TO DO: #define 100000.
    if (range > 100000) error("Internal error: range=%d; isorted can't handle range>1e5", range);
    for(i=0; i<n; i++) {
	    if (x[i] == NA_INTEGER) counts[napos]++;
	    else counts[off+x[i]]++;
    }
    // TO DO: at this point if the last count==n then it's all the same number and we can stop now.
    // Idea from Terdiman, then improved on that by not needing to loop through counts.
    
    tmp = 0;                  // *** BLOCK 4 ***
    for (i=0; i<=range; i++) {   // no point in adding tmp<n && i<=range, since range includes max, need to go to max, unlike 256 loops elsewhere in forder.c
        if (counts[i]) {      // cummulate but not through 0's.  Helps resetting zeros when n<range, below.
            push(counts[i]);
            counts[i] = (tmp += counts[i]);
        }
    }
    for(i=n-1; i>=0; i--) {
		tmp = x[i];
		o[--counts[(tmp == NA_INTEGER) ? napos : off+tmp]] = (int)(i+1);	
	}
    // counts were cummulated above so leaves non zero. Faster to clear up now ready for next time.
    if (n < range) {
        // Many zeros in counts already. Loop through n instead, doesn't matter if we set to 0 several times on any repeats
        counts[napos]=0;
        for (i=0; i<n; i++) {
            tmp=x[i];
            if (tmp!=NA_INTEGER) counts[off+tmp]=0;
        }
    } else {
	    memset(counts, 0, (range+1)*sizeof(int));     // *** BLOCK 6 ***
	}
	return;
}

static void iinsert(int *x, int *o, int n)
// orders both x and o by reference in-place. Fast for small vectors, low overhead.
// don't be tempted to binsearch backwards here, have to shift anyway; many memmove would have overhead and do the same thing
{
    int i, j, xtmp, otmp, tt;
    for (i=1; i<n; i++) {
        xtmp = x[i];
        if (xtmp < x[i-1]) {
            j = i-1;
            otmp = o[i];
            while (j>=0 && xtmp < x[j]) { x[j+1] = x[j]; o[j+1] = o[j]; j--; }
            x[j+1] = xtmp;
            o[j+1] = otmp;
        }
    }
    tt = 0;
    for (i=1; i<n; i++) if (x[i]==x[i-1]) tt++; else { push(tt+1); tt=0; }
    push(tt+1);
}


/*
 iradix is a counting sort performed forwards from MSB to LSB, with some tricks and short circuits building on Terdiman and Herf.
  http://codercorner.com/RadixSortRevisited.htm 
  http://stereopsis.com/radix.html
 Note they are LSD, but we do MSD here which is more complicated, for efficiency.
 NAs need no special treatment as NA is the most negative integer in R (checked in init.c once, for efficiency) so NA naturally sort to the front.
 Using 4-pass 1-byte radix for the following reasons :
 *  11-bit (Herf) reduces to 3-passes (3*11=33) yes, and LSD need random access to o vector in each pass 1:n so reduction in passes
 *  is good, but ...
 *  ... Terdiman's idea to skip a radix if all values are equal occurs less the wider the radix. A narrower radix benefits more from that.
 *      That's detected here using a single 'if', an improvement on Terdiman's exposition of a single loop to find if any count==n
 *  The pass through counts bites when radix is wider, because we repetitively call this iradix from fastorder forwards.
 *  Herf's parallel histogramming is neat. In 4-pass 1-byte it needs 4*256 storage, that's tiny, and can be static. 4*256 << 3*2048
 *  4-pass 1-byte is simpler and tighter code than 3-pass 11-bit, giving modern optimizers and modern CPUs a better chance. We may get
 *  lucky anyway, if one or two of the 4-passes are skipped.
 Recall: there are no comparisons at all in counting and radix, there is wide random access in each LSD radix pass, though.
*/

static unsigned int iradixcounts[4][257] = {{0}};
static int skip[8];   // 8 because we reuse for dradix below
// global because iradix and iradix_r interact and are called repetitively. 
// counts are set back to 0 after each use, to benefit from skipped radix.
static void *radix_xsub=NULL;
static int radix_xsuballoc=0;

static int *otmp=NULL;
static void *xtmp=NULL;  // TO DO: save one of these if possible
static int oxtmpalloc = 0;
// TO DO:  central allocator function here,  which could change oxtmpalloc in terms of bytes,  so would distinguish int and double

static void iradix_r(int *xsub, int *osub, int n, int radix);

static void iradix(int *x, int *o, int n)
   // As icount :
   //   Places the ordering into o directly, overwriting whatever was there
   //   Doesn't change x
   //   Pushes group sizes onto stack
{
    int i, j, radix, nextradix, itmp, thisgrpn, maxgrpn;
    unsigned int thisx=0, shift, *thiscounts;
    
    for (i=0;i<n;i++) {   // parallel histogramming pass; i.e. count occurrences of 0:255 in each byte.  Sequential so almost negligible.
        thisx = (unsigned int)x[i];
        iradixcounts[0][thisx & 0xFF]++;        // unrolled since inside n-loop
        iradixcounts[1][thisx >> 8 & 0xFF]++;
        iradixcounts[2][thisx >> 16 & 0xFF]++;
        iradixcounts[3][thisx >> 24 & 0xFF]++;
    }
    for (radix=0; radix<4; radix++) {
        // any(count == n) => all radix must have been that value => last x (still thisx) was that value
        i = thisx >> (radix*8) & 0xFF;
        skip[radix] = iradixcounts[radix][i] == n;
        if (skip[radix]) iradixcounts[radix][i] = 0;  // clear it now, the other counts must be 0 already
    }
    
    radix = 3;  // MSD
    while (radix>=0 && skip[radix]) radix--;
    if (radix==-1) { for (i=0; i<n; i++) o[i]=i+1; push(n); return; }  // All radix are skipped; i.e. one number repeated n times.
    for (i=radix-1; i>=0; i--) {
        if (!skip[i]) memset(iradixcounts[i], 0, 257*sizeof(unsigned int));
        // clear the counts as we only needed the parallel pass for skip[] and we're going to use iradixcounts again below. Can't use parallel lower counts in MSD radix, unlike LSD.
    }
    thiscounts = iradixcounts[radix];
    shift = radix * 8;
    
    itmp = thiscounts[0];
    maxgrpn = itmp;
    for (i=1; itmp<n && i<256; i++) {
        thisgrpn = thiscounts[i];
        if (thisgrpn) {  // don't cummulate through 0s, important below
            if (thisgrpn>maxgrpn) maxgrpn = thisgrpn;
            thiscounts[i] = (itmp += thisgrpn);
        }
    }
    for (i=n-1; i>=0; i--) {
        thisx = (unsigned int)x[i] >> shift & 0xFF;
        o[--thiscounts[thisx]] = i+1;
    }
    if (radix_xsuballoc < maxgrpn) {
        // The largest group according to the first non-skipped radix, so could be big (if radix is needed on first column)
        // TO DO: could include extra bits to divide the first radix up more. Often the MSD has groups in just 0-4 out of 256.
        // free'd at the end of forder once we're done calling iradix repetitively
        radix_xsub = (int *)realloc(radix_xsub, maxgrpn*sizeof(double));  // realloc(NULL) == malloc
        if (!radix_xsub) error("Failed to realloc working memory %d*8bytes (xsub in iradix), radix=%d", maxgrpn, radix);
        radix_xsuballoc = maxgrpn;
    }
    if (oxtmpalloc < maxgrpn) {
        otmp = (int *)realloc(otmp, maxgrpn * sizeof(int));
        if (otmp == NULL) error("Failed to realloc working memory %d*4bytes (otmp in iradix), radix=%d", maxgrpn, radix);
        xtmp = (double *)realloc(xtmp, maxgrpn * sizeof(double));  // TO DO: centralize with dradix
        if (xtmp == NULL) error("Failed to realloc working memory %d*8bytes (xtmp in iradix), radix=%d", maxgrpn, radix);
        oxtmpalloc = maxgrpn;
    }
    nextradix = radix-1;
    while (nextradix>=0 && skip[nextradix]) nextradix--;
    if (thiscounts[0] != 0) error("Internal error. thiscounts[0]=%d but should have been decremented to 0. dradix=%d", thiscounts[0], radix);
    thiscounts[256] = n;
    itmp = 0;
    for (i=1; itmp<n && i<=256; i++) {
        if (thiscounts[i] == 0) continue;
        thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
        if (thisgrpn == 1 || nextradix==-1) {
            push(thisgrpn);
        } else {
            for (j=0; j<thisgrpn; j++) ((int *)radix_xsub)[j] = x[o[itmp+j]-1];  // this is why this xsub here can't be the same memory as xsub in forder
            iradix_r(radix_xsub, o+itmp, thisgrpn, nextradix);          // changes xsub and o by reference recursively.
        }
        itmp = thiscounts[i];
        thiscounts[i] = 0;
    }
}


static void iradix_r(int *xsub, int *osub, int n, int radix)
    // xsub is a recursive offset into xsub working memory above in iradix, reordered by reference.
    // osub is a an offset into the main answer o, reordered by reference.
    // radix iterates 3,2,1,0
{
    int i, j, itmp, thisx, thisgrpn, nextradix, shift;
    unsigned int *thiscounts;
    
    if (n<200) {   // 200 is guess based on limited testing. Needs calibrate(). Was 50 based on sum(1:50)=1275 worst -vs- 256 cummulate + 256 memset + allowance since reverse order is unlikely
        iinsert(xsub, osub, n);
        return;
    }
    
    shift = radix*8;
    thiscounts = iradixcounts[radix];
    
    for (i=0; i<n; i++) {
        thisx = (unsigned int)xsub[i];   // sequential in xsub
        thiscounts[thisx >> shift & 0xFF]++;
    }
    itmp = thiscounts[0];
    for (i=1; itmp<n && i<256; i++)
        if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);  // don't cummulate through 0s, important below
    for (i=n-1; i>=0; i--) {
        thisx = (unsigned int)xsub[i] >> shift & 0xFF;
        j = --thiscounts[thisx];
        otmp[j] = osub[i];
        ((int *)xtmp)[j] = xsub[i];
    }
    memcpy(osub, otmp, n*sizeof(int));
    memcpy(xsub, xtmp, n*sizeof(int));
    
    nextradix = radix-1;
    while (nextradix>=0 && skip[nextradix]) nextradix--;
    // TO DO:  If nextradix==-1 AND no further columns from forder AND !retGrp,  we're done. We have o. Remember to memset thiscounts before returning.
    
    if (thiscounts[0] != 0) error("Logical error. thiscounts[0]=%d but should have been decremented to 0. radix=%d", thiscounts[0], radix);
    thiscounts[256] = n;
    itmp = 0;
    for (i=1; itmp<n && i<=256; i++) {
        if (thiscounts[i] == 0) continue;
        thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
        if (thisgrpn == 1 || nextradix==-1) {
            push(thisgrpn);
        } else {
            iradix_r(xsub+itmp, osub+itmp, thisgrpn, nextradix);
        }
        itmp = thiscounts[i];
        thiscounts[i] = 0;
    }
}


// dradix from Arun's fastradixdouble.c
// + changed to MSD and hooked into forder framework here.
// + replaced tolerance with rounding s.f.

#define FLIP_SIGN_BIT   0x8000000000000000
#define RESET_NA_NAN    0x0000ffffffffffff
#define SET_NA_NAN_COMP 0xfff8000000000000 // we can directly set complement in twiddle()

static union {double d; unsigned long long ll;} u;

static unsigned long long twiddle(void *p)
{
    u.d = *(double *)p;
    if (ISNAN(u.d)) {
        u.ll = (u.ll & RESET_NA_NAN) | SET_NA_NAN_COMP;  // flip NaN/NA sign bit so that they sort to front (data.table's rule to make binary search easier and consistent)
    } else {
        //Rprintf("Before: "); binary(u.ll);
        if (u.ll >> 15 & 0x1)
            u.ll = ((u.ll >> 16) + 1) << 16;  // round
        else
            u.ll = (u.ll >> 16) << 16;  // clear final 16 as these interact with mask otherwise, it seems
        //Rprintf(" After: "); binary(u.ll);
    }
    unsigned long long mask = -(long long)(u.ll >> 63) | FLIP_SIGN_BIT;   // leaves bits in lowest 16 (ok, will be skipped)
    //Rprintf("  Mask: "); binary(mask);
    return(u.ll ^ mask);
}


static unsigned int dradixcounts[8][257] = {{0}};
// reuse the same skip[] as iradix which was allocated for 6 above

static void dradix_r(unsigned long long *xsub, int *osub, int n, int radix);

static void dradix(double *x, int *o, int n)
{
    int i, j, radix, nextradix, itmp, thisgrpn, maxgrpn;
    unsigned int shift, *thiscounts;
    unsigned long long thisx=0;
    // see comments in iradix for structure.  This follows the same.
    for (i=0;i<n;i++) {
        thisx = twiddle(&x[i]);
        dradixcounts[2][thisx >> 16 & 0xFF]++;   // unrolled since inside n-loop
        dradixcounts[3][thisx >> 24 & 0xFF]++;
        dradixcounts[4][thisx >> 32 & 0xFF]++;
        dradixcounts[5][thisx >> 40 & 0xFF]++;
        dradixcounts[6][thisx >> 48 & 0xFF]++;
        dradixcounts[7][thisx >> 56 & 0xFF]++;
    }
    skip[0] = skip[1] = 1; // the lowest 16 bits were rounded in twiddle
    for (radix=2; radix<8; radix++) {
        i = thisx >> (radix*8) & 0xFF;    // thisx is the last x after loop above
        skip[radix] = dradixcounts[radix][i] == n;
        if (skip[radix]) dradixcounts[radix][i] = 0;  // clear it now, the other counts must be 0 already
    }
    radix = 7;  // MSD
    while (radix>=0 && skip[radix]) radix--;
    if (radix==-1) { for (i=0; i<n; i++) o[i]=i+1; push(n); return; }  // All radix are skipped; i.e. one number repeated n times.
    for (i=radix-1; i>=0; i--) {  // clear the lower radix counts, we only did them to know skip. will be reused within each group
        if (!skip[i]) memset(dradixcounts[i], 0, 257*sizeof(unsigned int));
    }
    thiscounts = dradixcounts[radix];
    shift = radix * 8;
    itmp = thiscounts[0];
    maxgrpn = itmp;
    for (i=1; itmp<n && i<256; i++) {
        thisgrpn = thiscounts[i];
        if (thisgrpn) {  // don't cummulate through 0s, important below
            if (thisgrpn>maxgrpn) maxgrpn = thisgrpn;
            thiscounts[i] = (itmp += thisgrpn);
        }
    }
    for (i=n-1; i>=0; i--) {
        thisx = twiddle(&x[i]);
        o[ --thiscounts[thisx >> shift & 0xFF] ] = i+1;
    }
    if (radix_xsuballoc < maxgrpn) {   // TO DO: centralize this alloc
        // The largest group according to the first non-skipped radix, so could be big (if radix is needed on first column)
        // TO DO: could include extra bits to divide the first radix up more. Often the MSD has groups in just 0-4 out of 256.
        // free'd at the end of forder once we're done calling iradix repetitively
        radix_xsub = (double *)realloc(radix_xsub, maxgrpn*sizeof(double));  // realloc(NULL) == malloc
        if (!radix_xsub) error("Failed to realloc working memory %d*8bytes (xsub in dradix), radix=%d", maxgrpn, radix);
        radix_xsuballoc = maxgrpn;
    }
    if (oxtmpalloc < maxgrpn) {
        otmp = (int *)realloc(otmp, maxgrpn * sizeof(int));
        if (otmp == NULL) error("Failed to realloc working memory %d*4bytes (otmp in dradix), radix=%d", maxgrpn, radix);
        xtmp = (double *)realloc(xtmp, maxgrpn * sizeof(double));
        if (xtmp == NULL) error("Failed to realloc working memory %d*4bytes (xtmp in dradix), radix=%d", maxgrpn, radix);
        oxtmpalloc = maxgrpn;
    }
    nextradix = radix-1;
    while (nextradix>=0 && skip[nextradix]) nextradix--;
    if (thiscounts[0] != 0) error("Logical error. thiscounts[0]=%d but should have been decremented to 0. dradix=%d", thiscounts[0], radix);
    thiscounts[256] = n;
    itmp = 0;
    for (i=1; itmp<n && i<=256; i++) {
        if (thiscounts[i] == 0) continue;
        thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
        if (thisgrpn == 1 || nextradix==-1) {
            push(thisgrpn);
        } else {
            for (j=0; j<thisgrpn; j++) ((unsigned long long *)radix_xsub)[j] = twiddle(&x[o[itmp+j]-1]);
            dradix_r(radix_xsub, o+itmp, thisgrpn, nextradix);          // changes xsub and o by reference recursively.
        }
        itmp = thiscounts[i];
        thiscounts[i] = 0;
    }
}

static void dinsert(unsigned long long *x, int *o, int n)
// orders both x and o by reference in-place. Fast for small vectors, low overhead.
// don't be tempted to binsearch backwards here, have to shift anyway; many memmove would have overhead and do the same thing
{
    int i, j, otmp, tt;
    unsigned long long xtmp;
    for (i=1; i<n; i++) {
        xtmp = x[i];
        if (xtmp < x[i-1]) {
            j = i-1;
            otmp = o[i];
            while (j>=0 && xtmp < x[j]) { x[j+1] = x[j]; o[j+1] = o[j]; j--; }
            x[j+1] = xtmp;
            o[j+1] = otmp;
        }
    }
    tt = 0;
    for (i=1; i<n; i++) if (x[i]==x[i-1]) tt++; else { push(tt+1); tt=0; }
    push(tt+1);
}

static void dradix_r(unsigned long long *xsub, int *osub, int n, int radix)
    // xsub is a recursive offset into xsub working memory above in dradix, reordered by reference.
    // osub is a an offset into the main answer o, reordered by reference.
    // dradix iterates 7,6,5,4,3,2,1,0
{
    int i, j, itmp, thisgrpn, nextradix, shift;
    unsigned int *thiscounts;
    
    if (n<200) {   // 200 is guess based on limited testing. Needs calibrate(). Was 50 based on sum(1:50)=1275 worst -vs- 256 cummulate + 256 memset + allowance since reverse order is unlikely
        dinsert(xsub, osub, n);
        return;
    }

    shift = radix*8;
    thiscounts = dradixcounts[radix];
    
    for (i=0; i<n; i++)
        thiscounts[xsub[i] >> shift & 0xFF]++;
    itmp = thiscounts[0];
    for (i=1; itmp<n && i<256; i++)
        if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);  // don't cummulate through 0s, important below
    for (i=n-1; i>=0; i--) {
        j = --thiscounts[ xsub[i] >> shift & 0xFF ];
        otmp[j] = osub[i];
        ((unsigned long long *)xtmp)[j] = xsub[i];
    }
    memcpy(osub, otmp, n*sizeof(int));
    memcpy(xsub, xtmp, n*sizeof(unsigned long long));
    
    nextradix = radix-1;
    while (nextradix>=0 && skip[nextradix]) nextradix--;
    // TO DO:  If nextradix==-1 and no further columns from forder,  we're done. We have o. Remember to memset thiscounts before returning.
    
    if (thiscounts[0] != 0) error("Logical error. thiscounts[0]=%d but should have been decremented to 0. radix=%d", thiscounts[0], radix);    
    thiscounts[256] = n;
    itmp = 0;
    for (i=1; itmp<n && i<=256; i++) {
        if (thiscounts[i] == 0) continue;
        thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
        if (thisgrpn == 1 || nextradix==-1) {
            push(thisgrpn);
        } else {
            dradix_r(xsub+itmp, osub+itmp, thisgrpn, nextradix);
        }
        itmp = thiscounts[i];
        thiscounts[i] = 0;
    }
}

// TO DO: dcount.  Find step size, then range = (max-min)/step.  Many fixed precision floats (such as prices) may be suitable.
//                 However, due to radix skipping, dradix may not be much worse.

static int isorted(int *x, int n)
{
    // base:is.unsorted does any(is.na(x)) at R level (inefficient), and returns NA (missing sorted cases where NA are at the start).
    // Here we deal with NAs in C and return true if NAs are all at the beginning.
    // We also return -1 if x is sorted in _strictly_ reverse order; a common case we optimize in fastorder.
    // If a vector is in decreasing order *with ties*, then an in-place reverse (no sort) would result in instability of ties.
    // For internal use only in fastorder, which now returns NULL if already sorted (hence no need for separate is.sorted).
    // TO DO: test in big steps first to return faster if unsortedness is at the end (a common case of rbind'ing data to end)
    int i=1, tmp;
    if (n==1) { push(1); return(1); }
    if (x[1]<x[0]) {
        i = 2;
        while (i<n && x[i]<x[i-1]) i++;
        if (i==n) { mpush(1,n); return(-1);}  // strictly decreasing order, no ties; e.g. no more than one NA at the end
        else return(0);                       // TO DO: improve to be stable for ties in reverse
    }
    int old = gsngrp[flip];
    int tt = 1;
    for (i=1; i<n; i++) {
        tmp = x[i]-x[i-1];
        if (tmp<0) { gsngrp[flip] = old; return(0); }
        if (tmp==0) tt++; else { push(tt); tt=1; }
    }
    push(tt);
    return(1);  // increasing order, possibly with ties
}

static int *cradix_counts = NULL;
static int cradix_counts_alloc = 0;
static int maxlen = 0;
static SEXP *cradix_xtmp = NULL;
static int cradix_xtmp_alloc = 0;

static void cradix_r(SEXP *xsub, int n, int radix)
// xsub is a unique set of CHARSXP, to be ordered by reference
// First time, radix==0, and xsub==x. Then recursively moves SEXP together for L1 cache efficiency.
// Quite different to iradix because 
//   1) x is known to be unique so fits in cache (wide random access not an issue)
//   2) they're variable length character strings
//   3) no need to maintain o.  Just simply reorder x. No grps or push.
// Fortunately, UTF sorts in the same order if treated as ASCII, so we can simplify by doing it by bytes.  TO DO: confirm
// a forwards (MSD) radix for efficiency, although more complicated
// This part has nothing to do with truelength. The truelength stuff is to do with finding the unique strings.
{
    int i, j, itmp, *thiscounts, thisgrpn=0, thisx=0;
    
    // TO DO?: chmatch to existing sorted vector, then grow it.
    // TO DO?: if (n<200) insert sort, then loop through groups via ==
    
    thiscounts = cradix_counts + radix*256;
    for (i=0; i<n; i++) {
        thisx = radix<LENGTH(xsub[i]) ? (int)CHAR(xsub[i])[radix] : 0;
        thiscounts[ thisx ]++;
    }
    if (thiscounts[thisx] == n && radix < maxlen-1) {   // this also catches when subx has shorter strings than the rest, thiscounts[0]==n and we'll recurse very quickly through to the overall maxlen with no 256 overhead each time
        cradix_r(xsub, n, radix+1);
        thiscounts[thisx] = 0;  // the rest must be 0 already, save the memset
        return;
    }
    itmp = thiscounts[0];
    for (i=1; i<256; i++)
        if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);  // don't cummulate through 0s, important below
    for (i=n-1; i>=0; i--) {
        thisx = radix<LENGTH(xsub[i]) ? (int)CHAR(xsub[i])[radix] : 0;
        j = --thiscounts[thisx];
        cradix_xtmp[j] = xsub[i];
    }
    memcpy(xsub, cradix_xtmp, n*sizeof(SEXP));
    if (radix == maxlen-1) {
        memset(thiscounts, 0, 256*sizeof(int)); 
        return;
    }
    if (thiscounts[0] != 0) error("Logical error. counts[0]=%d in cradix but should have been decremented to 0. radix=%d", thiscounts[0], radix);
    itmp = 0;
    for (i=1;i<256;i++) {
        if (thiscounts[i] == 0) continue;
        thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
        if (thisgrpn >= 2) cradix_r(xsub+itmp, thisgrpn, radix+1);   // TO DO: cinsert at the top (or here?) and special case thisgrpn==2
        itmp = thiscounts[i];
        thiscounts[i] = 0;  // set to 0 now since we're here, saves memset afterwards. Important to clear! Also more portable for machines where 0 isn't all bits 0 (?!)
    }
    if (itmp<n-1) cradix_r(xsub+itmp, n-itmp, radix+1);  // final group
}

extern void savetl_init(), savetl(SEXP s), savetl_end();

static SEXP *ustr = NULL;
static int ustr_alloc = 0;

static void ccount(SEXP *x, int *o, int n)
// As icount :
//   Places the ordering into o directly, overwriting whatever was there
//   Doesn't change x
//   Pushes group sizes onto stack
{
    SEXP s;
    int i, k, cumsum, un;
    
    // savetl_init() is called once at the start of forder
    maxlen = 0;  // set the global maxlen to save looping inside cradix_r too much 
    un = 0;
    for(i=0; i<n; i++) {
        s = x[i];
        if (TRUELENGTH(s)<0) {                   // this case first as it's the most frequent
            SET_TRUELENGTH(s, TRUELENGTH(s)-1);  // use negative counts so as to detect R's own (positive) usage of tl on CHARSXP
            continue;
        }
        if (TRUELENGTH(s)>0) {  // Save any of R's own usage of tl (assumed positive, so we can both count and save in one scan), to restore
            savetl(s);          // afterwards. From R 2.14.0, tl is initialized to 0, prior to that it was random so this step saved too much.
            SET_TRUELENGTH(s,0);
        }
        if (ustr_alloc<=un) {
            ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;  // 10000 = 78k of 8byte pointers. Small initial guess, negligible time to alloc. 
            if (ustr_alloc>n) ustr_alloc = n;
            ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
        }
        SET_TRUELENGTH(s, -1); 
        ustr[un++] = s;
        if (sortStr && s!=NA_STRING && LENGTH(s)>maxlen) maxlen=LENGTH(s);  // length on CHARSXP is the nchar of char * (excluding \0)
    }
    // TO DO:  Test all "" and all NA.  Does NA_character_ come before ""? (should do).
    if (sortStr) {
        if (cradix_counts_alloc < maxlen) {
            cradix_counts_alloc = cradix_counts_alloc == 0 ? 20 : maxlen + 20;   // +20 to save many reallocs for very long and varying strings
            cradix_counts = (int *)realloc(cradix_counts, cradix_counts_alloc * 256 * sizeof(int) );  // stack of counts
            if (!cradix_counts) error("Failed to alloc cradix_counts");
            memset(cradix_counts, 0, cradix_counts_alloc * 256 * sizeof(int));
        }
        if (cradix_xtmp_alloc < n) {
            cradix_xtmp = (SEXP *)realloc( cradix_xtmp,  n * sizeof(SEXP) );  // TO DO: Reuse the one we have in forder. Does it need to be n length?
            if (!cradix_xtmp) error("Failed to alloc cradix_tmp");
            cradix_xtmp_alloc = n;
        }
        cradix_r(ustr,un,0);  // changes ustr by reference
    }
    cumsum = 0;
    for(i=0; i<un; i++) {                                    // 0.000
        push(-TRUELENGTH(ustr[i]));                            
        SET_TRUELENGTH(ustr[i], cumsum += -TRUELENGTH(ustr[i]));
    }
    for(i=n-1; i>=0; i--) {                     
        s = x[i];                                            // 0.400 (page fetches on string cache)
        SET_TRUELENGTH(s, k = TRUELENGTH(s)-1);
        o[k] = i+1;                                          // 0.800 (random access to o)
    }
    for(i=0; i<un; i++) SET_TRUELENGTH(ustr[i],0);     // The cumsum meant the counts are left non zero, so reset for next time (0.00).
    //   savetl_end()  is at the end of forder, once
}

//
// TO DO: for dradix, if treatment of Inf,-Inf,NA and Nan is biting anywhere, they can we swept to the front in one pass by shifting the next non-special back however many specials have been observed so far.
//




SEXP forder(SEXP DT, SEXP by, SEXP retGrp, SEXP sortStrArg)
// TO DO: check fast for a unique integer vector such as origorder = iradixorder(firstofeachgroup) in [.data.table ad hoc by (no groups needed).
// TO DO: attach group sizes to result, if retGrp is TRUE
// sortChar TRUE from setkey, FALSE from by=
{
    int i, j, k, grp, tmp, *osub, thisgrpn, n, col;
    Rboolean isSorted = TRUE;
    SEXP x;
    void *xd;
#ifdef TIMING_ON
    memset(tblock, 0, NBLOCK*sizeof(clock_t));
    memset(nblock, 0, NBLOCK*sizeof(int));
    clock_t tstart;  // local variable to mask the global tstart for ease when timing sub funs
#endif
    TBEG()
    savetl_init();
    
    if (isNewList(DT)) {
        if (!length(DT)) error("DT is an empty list() of 0 columns");
        if (!isInteger(by) || !length(by)) error("DT has %d columns but 'by' is either not integer or length 0", length(DT));  // seq_along(x) at R level
        for (i=0; i<LENGTH(by); i++) if (INTEGER(by)[i] < 1 || INTEGER(by)[i] > length(DT)) error("'by' value %d out of range [1,%d]", INTEGER(by)[i], length(DT));
        n = length(VECTOR_ELT(DT,0));
        x = VECTOR_ELT(DT,INTEGER(by)[0]-1);        
    } else {
        if (!isNull(by)) error("Input is a single vector but 'by' is not NULL");
        n = length(DT);
        x = DT;
    }
    if (!isLogical(retGrp) || LENGTH(retGrp)!=1 || INTEGER(retGrp)[0]==NA_LOGICAL) error("retGrp must be TRUE or FALSE");
    if (!isLogical(sortStrArg) || LENGTH(sortStrArg)!=1 || INTEGER(sortStrArg)[0]==NA_LOGICAL ) error("sortStr must be TRUE or FALSE");
    sortStr = LOGICAL(sortStrArg)[0];
    gsmaxalloc = n;  // upper limit for stack size (all size 1 groups). We'll detect and avoid that limit, but if just one non-1 group (say 2), that can't be avoided.
    
    
    
    SEXP ans = PROTECT(allocVector(INTSXP, n));  // once for the result, needs to be length n.
    int *o = INTEGER(ans);                       // TO DO: save allocation if NULL is returned (isSorted==TRUE)
    xd = DATAPTR(x);
    
    //for (i=0; i<n; i++) binary( ((unsigned long long *)xd)[i] );
    //Rprintf("\n");
    //for (i=0; i<n; i++) binary( twiddle(&((double *)xd)[i]) );
    
    stackgrps = length(by)>1 || LOGICAL(retGrp)[0];
    
    if (isString(x)) {
        ccount(xd, o, n);
        isSorted = FALSE;  // TO DO: check o for sortedness if ccount is fast, or create csorted.
    } else if (isReal(x)) {
        dradix(xd, o, n);
        isSorted = FALSE;
    } else {
        if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP) error("First column being ordered is type '%s', not yet supported", type2char(TYPEOF(x)));
        if ((tmp = isorted(xd, n))) {
            if (tmp==1) {
                isSorted = TRUE;
                for (i=0; i<n; i++) o[i] = i+1;
            }
            else {  // tmp==-1, strictly decreasing
                isSorted = FALSE;
                for (i=0; i<n; i++) o[i] = n-i;
            }
        } else {
            isSorted = FALSE;
            setRange(xd, n);
            if (range == NA_INTEGER) error("The all-NA case should have been caught by isorted above");
            if (range <= 100000) {
                icount(xd, o, n);
            } else {
                iradix(xd, o, n);
            }
        }
    }
    TEND(0)
    
    int maxgrpn = gsmax[flip];  // biggest group in the first column
    // TO DO: if more columns {
    void *xsub = (void *)malloc(maxgrpn * sizeof(double));   // double is the largest type, 8. TO DO: check in init.c
    if (xsub==NULL) error("Couldn't allocate xsub in forder. TO DO: improve error message.");
    int *newo = (int *)malloc(maxgrpn * sizeof(int));
    if (newo==NULL) error("Couldn't allocate newo in forder. TO DO: improve error message.");
    // }
    
    TEND(1)
    void (*f)();
    
    for (col=2; col<=length(by); col++) {
        x = VECTOR_ELT(DT,INTEGER(by)[col-1]-1);
        xd = DATAPTR(x);
        int ngrp = gsngrp[flip];
        if (ngrp == n) break;
        flipflop();
        stackgrps = col!=LENGTH(by) || LOGICAL(retGrp)[0];
        i = 0;
        if (isString(x) || isReal(x)) {
            if (isString(x)) f = &ccount; else f = &dradix;  
            isSorted = FALSE;  // TO DO: include csorted and dsorted
            for (grp=0; grp<ngrp; grp++) {
                thisgrpn = gs[1-flip][grp];
                if (thisgrpn == 1) {i++; push(1); continue;}
                TBEG()
                osub = o+i;
                for (j=0; j<thisgrpn; j++) ((SEXP *)xsub)[j] = ((SEXP *)xd)[o[i++]-1];  // same size as double. TO DO: tidy and 32bit
                TEND(2)
                // TO DO: if thisgrpn==2
                if (isReal(x)) {
                    for (j=0; j<thisgrpn; j++) ((unsigned long long *)xsub)[j] = twiddle(&((double *)xsub)[j]);
                    if (thisgrpn < 200) {
                        dinsert(xsub, osub, thisgrpn);
                        continue;
                    }
                }
                (*f)(xsub, newo, thisgrpn);
                TEND(3)
                for (j=0; j<thisgrpn; j++) ((int *)xsub)[j] = osub[ newo[j]-1 ];   // reuse xsub to reorder osub
                memcpy(osub, xsub, thisgrpn*sizeof(int));
                TEND(4)
            }
        } else {
            if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP) error("Column %d being ordered is type '%s', not yet supported", col, type2char(TYPEOF(x)));
            for (grp=0; grp<ngrp; grp++) {
                thisgrpn = gs[1-flip][grp];
                if (thisgrpn == 1) {i++; push(1); continue;}
                TBEG()
                osub = o+i;
                for (j=0; j<thisgrpn; j++) ((int *)xsub)[j] = ((int *)xd)[o[i++]-1];
                TEND(5)
              
                // continue;  // BASELINE short circuit timing point. Up to here is the cost of creating xsub.
              
                if (thisgrpn==2) {
                    if (((int *)xsub)[1]<((int *)xsub)[0]) { isSorted=FALSE; tmp=osub[0]; osub[0]=osub[1]; osub[1]=tmp; }
                    push(1); push(1);
                    continue;
                }
                tmp = isorted(xsub, thisgrpn);  // very low cost and localised to thisgrp
                TEND(6)
                if (tmp) {
                    // isorted will have already push()'d the groups
                    if (tmp<0) {
                        isSorted = FALSE;
                        for (k=0;k<thisgrpn/2;k++) {      // reverse the order in-place using no function call or working memory 
                            tmp = osub[k];                // isorted only returns -1 for _strictly_ decreasing order, otherwise ties wouldn't be stable
                            osub[k] = osub[thisgrpn-1-k];
                            osub[thisgrpn-1-k] = tmp;
                        }
                        TEND(7)
                    }
                    continue;
                }
                isSorted = FALSE;
                if (thisgrpn<200) {    // see comment above in iradix_r re 200.
                    iinsert(xsub, osub, thisgrpn);  // pushes inside too
                    TEND(8)
                    continue;
                }
                setRange((int *)xsub, thisgrpn);   // Tighter range (e.g. copes better with a few abormally large values in some groups), but also, when setRange was once at colum level that caused an extra scan of (long) x first. 10,000 calls to setRange takes just 0.04s i.e. negligible.
                if (range==NA_INTEGER) error("2nd column is all NA in this subgroup. issorted should have caught this above");
                TEND(9)
                if (range < 10000) {   // 1e4 rather than 1e5 here because iterated
                    icount(xsub, newo, thisgrpn);  
                    TEND(10)
                } else {                            // was (thisgrpn < 200 || range > 20000) then radix
                    iradix(xsub, newo, thisgrpn);   // since a short vector with large range can bite icount when iterated (BLOCK 4 and 6)
                    TEND(11)                         // TO DO: add calibrate() to init.c
                }
                for (j=0; j<thisgrpn; j++) ((int *)xsub)[j] = osub[ newo[j]-1 ];   // reuse xsub to reorder osub
                memcpy(osub, xsub, thisgrpn*sizeof(int));
                TEND(12)
            }
        }
    }
#ifdef TIMING_ON
    for (i=0; i<NBLOCK; i++) {
        Rprintf("Timing block %d = %8.3f   %8d\n", i, 1.0*tblock[i]/CLOCKS_PER_SEC, nblock[i]);
        if (i==12) Rprintf("\n");
    }
    Rprintf("Found %d groups and maxgrpn=%d\n", gsngrp[flip], gsmax[flip]);
#endif
    
    savetl_end();  // hence important no early returns or error()s above. TO DO: wrap error() with globError() to clear up.
    
    if (isSorted) ans = R_NilValue;
    if (LOGICAL(retGrp)[0]) {
        SEXP grps = PROTECT(allocVector(INTSXP, gsngrp[flip]));
        memcpy(INTEGER(grps), gs[flip], gsngrp[flip] * sizeof(int));
        setAttrib(ans, install("grps"), grps);
        UNPROTECT(1);
    }
    
    gsfree();
    free(radix_xsub);          radix_xsub=NULL;    radix_xsuballoc=0;
    free(xsub); free(newo);    xsub=newo=NULL;
    free(xtmp); free(otmp);    xtmp=otmp=NULL;     oxtmpalloc=0;

    free(ustr);                ustr=NULL;          ustr_alloc=0;
    free(cradix_counts);       cradix_counts=NULL; cradix_counts_alloc=0;
    free(cradix_xtmp);         cradix_xtmp=NULL;   cradix_xtmp_alloc=0;   // TO DO: use xtmp already got
    
    UNPROTECT(1);
    return( ans );
}

/*
    // LSD approach ...
    
    Rboolean firstTime = TRUE;
    for (radix=0; radix<4; radix++) {
        if (skip[radix]) continue;
        tmp = ord; ord = ans; ans = tmp;     // flip-flop
        shift = radix * 8;
        thiscounts = counts[radix];
        for (i=1;i<256;i++) thiscounts[i] += thiscounts[i-1];
        tt = clock();
        if (firstTime) {
            for(i = n-1; i >= 0; i--) {
		        thisx = x[i] >> shift & 0xFF;
		        ans[--thiscounts[thisx]] = i+1;
            }
        } else {
            for(i = n-1; i >= 0; i--) {
		        thisx = x[ord[i]-1] >> shift & 0xFF;
		        ans[--thiscounts[thisx]] = ord[i];
            }
        }
        memset(thiscounts, 0, 256*sizeof(unsigned int));
        t[6+radix] += clock()-tt;
        firstTime = FALSE;
    }
    if (firstTime) error("Internal error: x is one repeated number so isorted should have skipped iradix, but iradix has been called and skipped all 4 radix");
}

*/

#ifdef TIMING_ON
static void binary(unsigned long long n)
// trace utility for dev, since couldn't get stdlib::atoi() to link
{
    int sofar = 0;
    for(int shift=sizeof(long long)*8-1; shift>=0; shift--)
    {
       if (n >> shift & 1)
         Rprintf("1");
       else
         Rprintf("0");
       if (++sofar == 1 || sofar == 12) Rprintf(" ");
    }
    printf("\n");
}
#endif

