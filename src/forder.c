#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

// #define TIMING_ON

// Only forder() is meant for use by other C code in data.table, hence all functions here other than forder are static.
// They make use of global variables, so access must be via forder only.
// The coding techniques deployed here are for efficiency.

static int *gs[2] = {NULL};    // gs = groupsizes e.g. 23,12,87,2,1,34,...
static int flip = 0;           // two vectors flip flopped: flip and 1-flip
static int gsalloc[2] = {0};   // allocated stack size
static int gsngrp[2] = {0};    // used
static int gsmax[2] = {0};     // max grpn so far
static int gsmaxalloc = 0;     // max size of stack, set by forder to nrows
static Rboolean stackgrps = TRUE;  // switched off for last column when not needed by setkey
static Rboolean sortStr = TRUE;    // TRUE for setkey, FALSE for by=
static int *newo = NULL;       // used by forder and [i|d|c]sort to reorder order. not needed if length(by)==1

extern size_t sizes[];             // See dogroups.c for this shared variable
#define SIZEOF(x) sizes[TYPEOF(x)]

static void growstack(int newlen) {
    if (newlen==0) newlen=100000;   // no link to icount range restriction, just 100,000 seems a good minimum at 0.4MB.
    if (newlen>gsmaxalloc) newlen=gsmaxalloc;
    gs[flip] = realloc(gs[flip], newlen*sizeof(int));
    if (gs[flip] == NULL) error("Failed to realloc working memory stack to %d*4bytes (flip=%d)", newlen, flip);
    gsalloc[flip] = newlen;
}

static void push(int x) {
    if (!stackgrps || x==0) return;
    if (gsalloc[flip] == gsngrp[flip]) growstack(gsngrp[flip]*2);
    gs[flip][gsngrp[flip]++] = x;
    if (x > gsmax[flip]) gsmax[flip] = x;
}

static void mpush(int x, int n) {
    if (!stackgrps || x==0) return;
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
  static clock_t tblock[NBLOCK], tstart;
  static int nblock[NBLOCK];
  #define TBEG() tstart = clock();
  #define TEND(i) tblock[i] += clock()-tstart; nblock[i]++; tstart = clock();
  static void binary(unsigned long long n);  //utility function for dev, nothing to do with timing, but not used, so avoids compile warning
#else
  #define TBEG()
  #define TEND(i)
#endif


// icount originally copied from do_radixsort in src/main/sort.c @ rev 51389. Then reworked here again in forder.c in v1.8.11
// base::sort.list(method="radix") turns out not to be a radix sort, but a counting sort, and we like it.
// See http://r.789695.n4.nabble.com/method-radix-in-sort-list-isn-t-actually-a-radix-sort-tp3309470p3309470.html
// Main changes :
//   1. Negatives are fine. Wish raised for simple change to base R : <link needed>
//   2. Doesn't cummulate through 0's for speed in repeated calls of sparse counts by saving memset back to 0 of many 0
//   3. Separated setRange so forder can redirect to iradix

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

static unsigned int iradixcounts[4][257] = {{0}};   // TO DO: combine iradixcounts and dradixcounts,  size 8,  with 2 unused.
static int skip[8];   // 8 because we reuse for dradix below
// global because iradix and iradix_r interact and are called repetitively. 
// counts are set back to 0 after each use, to benefit from skipped radix.
static void *radix_xsub=NULL;
static int radix_xsuballoc=0;

static int *otmp=NULL, otmp_alloc=0;
static void alloc_otmp(int n) {
    if (otmp_alloc >= n) return;
    otmp = (int *)realloc(otmp, n * sizeof(int));
    if (otmp == NULL) error("Failed to allocate working memory for otmp. Requested %d * %d bytes", n, sizeof(int));
    otmp_alloc = n;
}

static void *xtmp=NULL; int xtmp_alloc=0;  // TO DO: save xtmp if possible, see allocs in forder
static void alloc_xtmp(int n) {        // TO DO: currently always the largest type (double) but could be int if that's all that's needed
    if (xtmp_alloc >= n) return;
    xtmp = (double *)realloc(xtmp, n * sizeof(double));
    if (xtmp == NULL) error("Failed to allocate working memory for xtmp. Requested %d * %d bytes", n, sizeof(double));
    xtmp_alloc = n;
}

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
        thisx = (unsigned int)x[i] - INT_MIN;
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
        thisx = ((unsigned int)x[i] - INT_MIN) >> shift & 0xFF;
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
    
    alloc_otmp(maxgrpn);   // TO DO: can we leave this to forder and remove these calls??
    alloc_xtmp(maxgrpn);
    
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
        thisx = (unsigned int)xsub[i] - INT_MIN;   // sequential in xsub
        thiscounts[thisx >> shift & 0xFF]++;
    }
    itmp = thiscounts[0];
    for (i=1; itmp<n && i<256; i++)
        if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);  // don't cummulate through 0s, important below
    for (i=n-1; i>=0; i--) {
        thisx = ((unsigned int)xsub[i] - INT_MIN) >> shift & 0xFF;
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
    if (R_FINITE(u.d)) {
        //Rprintf("Before: "); binary(u.ll);
        if (u.ll >> 15 & 0x1)   // bit 49
            u.ll = ((u.ll >> 16) + 1) << 16;  // round
        else
            u.ll = (u.ll >> 16) << 16;  // clear final 16 as these interact with mask otherwise, it seems
        //Rprintf(" After: "); binary(u.ll);
    } else if (ISNAN(u.d)) {
        u.ll = (u.ll & RESET_NA_NAN) | SET_NA_NAN_COMP;  // flip NaN/NA sign bit so that they sort to front (data.table's rule to make binary search easier and consistent)
    }
    unsigned long long mask = -(long long)(u.ll >> 63) | FLIP_SIGN_BIT;   // leaves bits in lowest 16 (ok, will be skipped)
    //Rprintf("  Mask: "); binary(mask);
    return(u.ll ^ mask);
}

// binPrint(SEXP x)   // utility function for dev, to do.
// for (i=0; i<n; i++) binary( ((unsigned long long *)xd)[i] );
// Rprintf("\n");
// for (i=0; i<n; i++) binary( twiddle(&((double *)xd)[i]) );

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
    
    alloc_otmp(maxgrpn);   // TO DO: leave to forder and remove these calls?
    alloc_xtmp(maxgrpn);
    
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

// TO DO?: dcount. Find step size, then range = (max-min)/step and proceed as icount. Many fixed precision floats (such as prices)
// may be suitable. Fixed precision such as 1.10, 1.15, 1.20, 1.25, 1.30 ... do use all bits so dradix skipping may not help.

static int *cradix_counts = NULL;
static int cradix_counts_alloc = 0;
static int maxlen = 1;
static SEXP *cradix_xtmp = NULL;
static int cradix_xtmp_alloc = 0;

int StrCmp(SEXP x, SEXP y)            // also used by bmerge and chmatch
{
    if (x == y) return 0;             // same cached pointer (including NA_STRING==NA_STRING)
    if (x == NA_STRING) return -1;    // x<y
    if (y == NA_STRING) return 1;     // x>y
    return strcmp(CHAR(x), CHAR(y));  // can return 0 here for the same string in known and unknown encodings, good if the unknown string is in that encoding but not if not
}                                     // ordering is ascii only (C locale). TO DO: revisit and allow user to change to strcoll, and take account of Encoding
                                      // see comments in bmerge()

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
// We may be able to improve CHARSXP derefencing by submitting patch to R to make R's string cache contiguous
// but would likely be difficult. If we strxfrm, then it'll then be contiguous and compact then anyway.
{
    int i, j, itmp, *thiscounts, thisgrpn=0, thisx=0;
    SEXP stmp;
    
    // TO DO?: chmatch to existing sorted vector, then grow it.
    // TO DO?: if (n<200) insert sort, then loop through groups via ==
    if (n<=1) return;
    if (n==2) {
        if (StrCmp(xsub[1],xsub[0])<0) { stmp=xsub[0]; xsub[0]=xsub[1]; xsub[1]=stmp; }
        return;
    }
    // TO DO: if (n<50) cinsert (continuing from radix offset into CHAR) or using StrCmp. But 256 is narrow, so quick and not too much an issue.
    
    thiscounts = cradix_counts + radix*256;
    for (i=0; i<n; i++) {
        thisx = xsub[i]==NA_STRING ? 0 : (radix<LENGTH(xsub[i]) ? (unsigned char)(CHAR(xsub[i])[radix]) : 1);
        thiscounts[ thisx ]++;   // 0 for NA,  1 for ""
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
        thisx = xsub[i]==NA_STRING ? 0 : (radix<LENGTH(xsub[i]) ? (unsigned char)(CHAR(xsub[i])[radix]) : 1);
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
        cradix_r(xsub+itmp, thisgrpn, radix+1);
        itmp = thiscounts[i];
        thiscounts[i] = 0;  // set to 0 now since we're here, saves memset afterwards. Important to clear! Also more portable for machines where 0 isn't all bits 0 (?!)
    }
    if (itmp<n-1) cradix_r(xsub+itmp, n-itmp, radix+1);  // final group
}

extern void savetl_init(), savetl(SEXP s), savetl_end();

static SEXP *ustr = NULL;
static int ustr_alloc = 0, ustr_n = 0;

static void cgroup(SEXP *x, int *o, int n)
// As icount :
//   Places the ordering into o directly, overwriting whatever was there
//   Doesn't change x
//   Pushes group sizes onto stack
// Only run when sortStr==FALSE. Basically a counting sort, in first appearance order, directly.
// Since it doesn't sort the strings, the name is cgroup.
{
    SEXP s;
    int i, k, cumsum;
    // savetl_init() is called once at the start of forder
    // there is no _pre for this.  ustr created and cleared each time.
    if (ustr_n != 0) error("Internal error. ustr isn't empty when starting cgroup: ustr_n=%d, ustr_alloc=%d", ustr_n, ustr_alloc);
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
        if (ustr_alloc<=ustr_n) {
            ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;  // 10000 = 78k of 8byte pointers. Small initial guess, negligible time to alloc. 
            if (ustr_alloc>n) ustr_alloc = n;
            ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
        }
        SET_TRUELENGTH(s, -1); 
        ustr[ustr_n++] = s;
    }
    // TO DO: the same string in different encodings will be considered different here. Sweep through ustr and merge counts where equal (sort needed therefore, unfortunately?, only if there are any marked encodings present)
    cumsum = 0;
    for(i=0; i<ustr_n; i++) {                                    // 0.000
        push(-TRUELENGTH(ustr[i]));                            
        SET_TRUELENGTH(ustr[i], cumsum += -TRUELENGTH(ustr[i]));
    }
    int *target = o[0] ? newo : o;
    for(i=n-1; i>=0; i--) {                     
        s = x[i];                                            // 0.400 (page fetches on string cache)
        SET_TRUELENGTH(s, k = TRUELENGTH(s)-1);
        target[k] = i+1;                                     // 0.800 (random access to o)
    }
    for(i=0; i<ustr_n; i++) SET_TRUELENGTH(ustr[i],0);     // The cummulate meant counts are left non zero, so reset for next time (0.00s).
    ustr_n = 0;
}

static int *csort_otmp=NULL, csort_otmp_alloc=0;
static void alloc_csort_otmp(int n) {
    if (csort_otmp_alloc >= n) return;
    csort_otmp = (int *)realloc(csort_otmp, n * sizeof(int));
    if (csort_otmp == NULL) error("Failed to allocate working memory for csort_otmp. Requested %d * %d bytes", n, sizeof(int));
    csort_otmp_alloc = n;
}

static void csort(SEXP *x, int *o, int n)
// As icount :
//   Places the ordering into o directly, overwriting whatever was there
//   Doesn't change x
//   Pushes group sizes onto stack
// Requires csort_pre() to have created and sorted ustr already
{
    int i;
    // can't use otmp, since iradix might be called here and that uses otmp (and xtmp).
    // alloc_csort_otmp(n) is called from forder for either n=nrow if 1st column, or n=maxgrpn if onwards columns
    for(i=0; i<n; i++) csort_otmp[i] = -TRUELENGTH(x[i]);
    if (n<200) {   // TO DO: calibrate() 200
        if (o[0]==0) for (i=0; i<n; i++) o[i] = i+1;  // else use o from caller directly (not 1st column)
        iinsert(csort_otmp, o, n);
    } else {
        setRange(csort_otmp, n);
        if (range == NA_INTEGER) error("Internal error. csort's otmp contains all-NA");
        int *target = o[0] ? newo : o; 
        if (range<=10000) // && range<n)  TO DO: calibrate(). radix was faster (9.2s "range<=10000" instead of 11.6s "range<=100000 && range<n") for run(7) where range=100000 n=10000000
            icount(csort_otmp, target, n);
        else
            iradix(csort_otmp, target, n);
    }
    // all i* push onto stack. Using their counts may be faster here than thrashing SEXP fetches over several passes as cgroup does
    // (but cgroup needs that to keep orginal order, and cgroup saves the sort in csort_pre).
}

static void csort_pre(SEXP *x, int n)
// Finds ustr and sorts it.
// Runs once for each column (if sortStr==TRUE), then ustr is used by csort within each group
// ustr is grown on each character column, to save sorting the same strings again if several columns cotain the same strings
{
    SEXP s;
    int i, old_un, new_un;
    // savetl_init() is called once at the start of forder
    // maxlen = 1;  initialized to 1, and reset to 1 in csort_post. So that cradix_r knows the maximum radix. 1 not 0 for when only "" and NA
    old_un = ustr_n;
    for(i=0; i<n; i++) {
        s = x[i];
        if (TRUELENGTH(s)<0) continue;   // this case first as it's the most frequent. Already in ustr, this negative is its ordering.
        if (TRUELENGTH(s)>0) {  // Save any of R's own usage of tl (assumed positive, so we can both count and save in one scan), to restore
            savetl(s);          // afterwards. From R 2.14.0, tl is initialized to 0, prior to that it was random so this step saved too much.
            SET_TRUELENGTH(s,0);
        }
        if (ustr_alloc<=ustr_n) {
            ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;  // 10000 = 78k of 8byte pointers. Small initial guess, negligible time to alloc. 
            if (ustr_alloc > old_un+n) ustr_alloc = old_un + n;
            ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
        }
        SET_TRUELENGTH(s, -1);  // this -1 will become its ordering later below
        ustr[ustr_n++] = s;
        if (s!=NA_STRING && LENGTH(s)>maxlen) maxlen=LENGTH(s);  // length on CHARSXP is the nchar of char * (excluding \0), and treats marked encodings as if ascii.
    }
    new_un = ustr_n;
    if (new_un == old_un) return;  // No new strings observed, seen them all before in previous column. ustr already sufficient.
    // If we ever make ustr permanently held by data.table, we'll just need to make the final loop to set -i-1 before returning here.
    // sort ustr.  TO DO: just sort new ones and merge them in.
    // These allocs are here, to save them being in the recursive cradix_r()
    if (cradix_counts_alloc < maxlen) {
        cradix_counts_alloc = maxlen + 10;   // +10 to save too many reallocs
        cradix_counts = (int *)realloc(cradix_counts, cradix_counts_alloc * 256 * sizeof(int) );  // stack of counts
        if (!cradix_counts) error("Failed to alloc cradix_counts");
        memset(cradix_counts, 0, cradix_counts_alloc * 256 * sizeof(int));
    }
    if (cradix_xtmp_alloc < ustr_n) {
        cradix_xtmp = (SEXP *)realloc( cradix_xtmp,  ustr_n * sizeof(SEXP) );  // TO DO: Reuse the one we have in forder. Does it need to be n length?
        if (!cradix_xtmp) error("Failed to alloc cradix_tmp");
        cradix_xtmp_alloc = ustr_n;
    }
    cradix_r(ustr, ustr_n, 0);  // sorts ustr in-place by reference
    for(i=0; i<ustr_n; i++)     // save ordering in the CHARSXP. negative so as to distinguish with R's own usage.
        SET_TRUELENGTH(ustr[i], -i-1);
}

// functions to test vectors for sortedness: isorted, dsorted and csorted
// base:is.unsorted uses any(is.na(x)) at R level (inefficient), and returns NA in the presence of any NA.
// Hence, here we deal with NAs in C and return true if NAs are all at the beginning (what we need in data.table).
// We also return -1 if x is sorted in _strictly_ reverse order; a common case we optimize in forder.
// If a vector is in decreasing order *with ties*, then an in-place reverse (no sort) would result in instability of ties (TO DO).
// For use by forder only, which now returns NULL if already sorted (hence no need for separate is.sorted).
// TO DO: test in big steps first to return faster if unsortedness is at the end (a common case of rbind'ing data to end)
// These are all sequential access to x, so very quick and cache efficient.

static int isorted(int *x, int n)
{
    // Relies on NA_INTEGER==INT_MIN, checked in init.c
    int i=1;
    if (n<=1) { push(n); return(1); }
    if (x[1]<x[0]) {
        i = 2;
        while (i<n && x[i]<x[i-1]) i++;
        if (i==n) { mpush(1,n); return(-1);}  // strictly decreasing order, no ties; e.g. no more than one NA at the end
        else return(0);
    }
    int old = gsngrp[flip];
    int tt = 1;
    for (i=1; i<n; i++) {
        if (x[i]<x[i-1]) { gsngrp[flip] = old; return(0); }
        if (x[i]==x[i-1]) tt++; else { push(tt); tt=1; }
    }
    push(tt);
    return(1);  // increasing order, NAs at the beginning, possibly with ties
}

static int dsorted(double *x, int n)
{
    int i=1;
    unsigned long long prev, this;
    if (n<=1) { push(n); return(1); }
    prev = twiddle(&x[0]);
    this = twiddle(&x[1]);
    if (this<prev) {
        i = 2;
        prev=this;
        while (i<n && (this=twiddle(&x[i]))<prev) {i++; prev=this; }
        if (i==n) { mpush(1,n); return(-1);}  // strictly decreasing order, no ties; e.g. no more than one NA at the end
        else return(0);                       // TO DO: improve to be stable for ties in reverse
    }
    int old = gsngrp[flip];
    int tt = 1;
    for (i=1; i<n; i++) {
        this = twiddle(&x[i]);    // TO DO: once we get past -Inf, NA and NaN at the bottom,  and +Inf at the top, the middle only need be twiddled for tolerance (worth it?)
        if (this<prev) { gsngrp[flip] = old; return(0); }
        if (this==prev) tt++; else { push(tt); tt=1; }
        prev = this;
    }
    push(tt);
    return(1);  // increasing order, possibly with ties
}

static int csorted(SEXP *x, int n)
{
    int i=1, tmp;
    if (n<=1) { push(n); return(1); }
    if (StrCmp(x[1],x[0])<0) {
        i = 2;
        while (i<n && StrCmp(x[i],x[i-1])<0) i++;
        if (i==n) { mpush(1,n); return(-1);}  // strictly decreasing order, no ties; e.g. no more than one NA at the end
        else return(0);
    }
    int old = gsngrp[flip];
    int tt = 1;
    for (i=1; i<n; i++) {
        tmp = StrCmp(x[i],x[i-1]);
        if (tmp<0) { gsngrp[flip] = old; return(0); }
        if (tmp==0) tt++; else { push(tt); tt=1; }
    }
    push(tt);
    return(1);  // increasing order, possibly with ties
}

static void isort(int *x, int *o, int n)
{
    if (n<=2) error("Internal error: isort received n=%d. isorted should have dealt with this (e.g. as a reverse sorted vector) already",n);
    if (n<200 && o[0]) {    // see comment above in iradix_r re 200.
        // if not o[0] then can't just populate with 1:n here, since x is changed by ref too (so would need to be copied).
        iinsert(x, o, n);  // pushes inside too.  changes x and o by reference, so not suitable in first column when o hasn't been populated yet and x is the actual column in DT (hence check on o[0])
        return;
    }
    setRange(x, n);   // Tighter range (e.g. copes better with a few abormally large values in some groups), but also, when setRange was once at colum level that caused an extra scan of (long) x first. 10,000 calls to setRange takes just 0.04s i.e. negligible.
    if (range==NA_INTEGER) error("Internal error: isort passed all-NA. isorted should have caught this before this point");
    int *target = o[0] ? newo : o;
    if (range<=100000 && range<=n) {   // was range<10000 for subgroups, but 1e5 for the first column, tried to generalise here.  1e4 rather than 1e5 here because iterated
        icount(x, target, n);
    } else {                    // was (thisgrpn < 200 || range > 20000) then radix
        iradix(x, target, n);   // a short vector with large range can bite icount when iterated (BLOCK 4 and 6)
    }
    // TO DO: add calibrate() to init.c
}

static void dsort(double *x, int *o, int n)
{
    if (n<=2) error("Internal error: dsort received n=%d. dsorted should have dealt with this (e.g. as a reverse sorted vector) already",n); 
    if (n<200 && o[0]) {    // see comment above in iradix_r re 200,  and isort for o[0]
        for (int i=0; i<n; i++) ((unsigned long long *)x)[i] = twiddle(&x[i]);
        dinsert((unsigned long long *)x, o, n);
    } else {
        dradix(x, o[0] ? newo : o, n);
    }
}

SEXP forder(SEXP DT, SEXP by, SEXP retGrp, SEXP sortStrArg)
// sortStr TRUE from setkey, FALSE from by=
{
    int i, j, k, grp, ngrp, tmp, *osub, thisgrpn, n, col;
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
    o[0] = 0;  // so [i|c|d]sort know they can populate o directly with no working memory needed to reorder existing order
    xd = DATAPTR(x);
    
    stackgrps = length(by)>1 || LOGICAL(retGrp)[0];
    
    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP :
        tmp = isorted(xd, n); break;
    case REALSXP :
        tmp = dsorted(xd, n); break;
    case STRSXP :
        tmp = csorted(xd, n); break;
    default :
        error("First column being ordered is type '%s', not yet supported", type2char(TYPEOF(x)));
    }
    if (tmp) {  // -1 or 1
        if (tmp==1) {
            isSorted = TRUE;
            for (i=0; i<n; i++) o[i] = i+1;   // TO DO: we don't need this if returning NULL? Save it? Unlikely huge gain, though.
        } else {  // -1 (or -n for result of strcmp), strictly decreasing
            isSorted = FALSE;
            for (i=0; i<n; i++) o[i] = n-i;
        }
    } else {
        isSorted = FALSE;
        switch(TYPEOF(x)) {
        case INTSXP : case LGLSXP :
            isort(xd, o, n); break;
            break;
        case REALSXP :
            dsort(xd, o, n); break;
        case STRSXP :
            if (sortStr) { csort_pre(xd, n); alloc_csort_otmp(n); csort(xd, o, n); }
            else cgroup(xd, o, n);
            break;
        default :
            error("Internal error: previous default should have caught unsupported type");
        }
    }
    TEND(0)
    
    int maxgrpn = gsmax[flip];  // biggest group in the first column
    void *xsub = NULL;  // local to forder
    int (*f)(); void (*g)();
    
    if (length(by)>1 && gsngrp[flip]<n) {
        xsub = (void *)malloc(maxgrpn * sizeof(double));   // double is the largest type, 8. TO DO: check in init.c
        if (xsub==NULL) error("Couldn't allocate xsub in forder, requested %d * %d bytes.", maxgrpn, sizeof(double));
        newo = (int *)malloc(maxgrpn * sizeof(int));  // global variable, used by isort, dsort, sort and cgroup
        if (newo==NULL) error("Couldn't allocate newo in forder, requested %d * %d bytes.", maxgrpn, sizeof(int));
    }
    TEND(1)  // should be negligible time to malloc even large blocks, but time it anyway to be sure
    
    for (col=2; col<=length(by); col++) {
        x = VECTOR_ELT(DT,INTEGER(by)[col-1]-1);
        xd = DATAPTR(x);
        ngrp = gsngrp[flip];
        if (ngrp == n) break;
        flipflop();
        stackgrps = col!=LENGTH(by) || LOGICAL(retGrp)[0];
        switch(TYPEOF(x)) {
        case INTSXP : case LGLSXP :
            f = &isorted; g = &isort; break;
        case REALSXP :
            f = &dsorted; g = &dsort; break;
        case STRSXP :
            f = &csorted;
            if (sortStr) { csort_pre(xd, n); alloc_csort_otmp(gsmax[1-flip]); g = &csort; }
            else g = &cgroup;
            break;
        default:
           error("Column %d of 'by' (%d) is type '%s', not yet supported", col, INTEGER(by)[col-1], type2char(TYPEOF(x)));
        }
        size_t size = SIZEOF(x);
        if (size!=4 && size!=8) error("Column %d of 'by' is supported type '%s' but size (%d) isn't 4 or 8\n", col, type2char(TYPEOF(x)), size);
        // sizes of int and double are checked to be 4 and 8 respectively in init.c
        i = 0;
        for (grp=0; grp<ngrp; grp++) {
            thisgrpn = gs[1-flip][grp];
            if (thisgrpn == 1) {i++; push(1); continue;}
            TBEG()
            osub = o+i;
            // ** TO DO **: if isSorted,  we can just point xsub into x directly. If (*f)() returns 0, though, will have to copy x at that point
            //        When doing this,  xsub could be allocated at that point for the first time.
            if (size==4)
                for (j=0; j<thisgrpn; j++) ((int *)xsub)[j] = ((int *)xd)[o[i++]-1];
            else
                for (j=0; j<thisgrpn; j++) ((double *)xsub)[j] = ((double *)xd)[o[i++]-1];
            TEND(2)
            
            // continue;  // BASELINE short circuit timing point. Up to here is the cost of creating xsub.
            
            tmp = (*f)(xsub, thisgrpn);     // [i|d|c]sorted(); very low cost, sequential
            TEND(3)
            
            if (tmp) {
                // *sorted will have already push()'d the groups
                if (tmp==-1) {
                    isSorted = FALSE;
                    for (k=0;k<thisgrpn/2;k++) {      // reverse the order in-place using no function call or working memory 
                        tmp = osub[k];                // isorted only returns -1 for _strictly_ decreasing order, otherwise ties wouldn't be stable
                        osub[k] = osub[thisgrpn-1-k];
                        osub[thisgrpn-1-k] = tmp;
                    }
                    TEND(4)
                }
                continue;
            }
            isSorted = FALSE;
            newo[0] = 0;
            (*g)(xsub, osub, thisgrpn);  // may update osub directly, or if not will put the result in global newo
            TEND(5)
            
            if (newo[0]) {
                for (j=0; j<thisgrpn; j++) ((int *)xsub)[j] = osub[ newo[j]-1 ];   // reuse xsub to reorder osub
                memcpy(osub, xsub, thisgrpn*sizeof(int));            
            }
            TEND(6)
        }
    }
#ifdef TIMING_ON
    for (i=0; i<NBLOCK; i++) {
        Rprintf("Timing block %d = %8.3f   %8d\n", i, 1.0*tblock[i]/CLOCKS_PER_SEC, nblock[i]);
        if (i==12) Rprintf("\n");
    }
    Rprintf("Found %d groups and maxgrpn=%d\n", gsngrp[flip], gsmax[flip]);
#endif
    
    if (!sortStr && ustr_n!=0) error("Internal error: at the end of forder sortStr==FALSE but ustr_n!=0 [%d]", ustr_n);
    for(int i=0; i<ustr_n; i++)
        SET_TRUELENGTH(ustr[i],0);
    maxlen = 1;  // reset global. Minimum needed to count "" and NA
    ustr_n = 0;
    savetl_end();  // hence important no early returns or error()s above. TO DO: wrap error() with globError() to clear up.
    free(ustr);                ustr=NULL;          ustr_alloc=0;
    
    if (isSorted) {
        UNPROTECT(1);  // The existing o vector, which we may save in future, if in future we only create when isSorted becomes FALSE
        ans = PROTECT(allocVector(INTSXP, 0));  // Can't attach attributes to NULL
    }
    if (LOGICAL(retGrp)[0]) {
        ngrp = gsngrp[flip];
        setAttrib(ans, install("starts"), x = allocVector(INTSXP, ngrp));
        //if (isSorted || LOGICAL(sort)[0])
            for (INTEGER(x)[0]=1, i=1; i<ngrp; i++) INTEGER(x)[i] = INTEGER(x)[i-1] + gs[flip][i-1];
        //else {
            // it's not sorted already and we want to keep original group order
        //    cumsum = 0;
        //    for (i=0; i<ngrp; i++) { INTEGER(x)[i] = o[i+cumsum]; cumsum+=gs[flip][i]; }
        //    isort(INTEGER(x), ngrp);
        //}
        setAttrib(ans, install("maxgrpn"), ScalarInteger(gsmax[flip]));
    }
    
    gsfree();
    free(radix_xsub);          radix_xsub=NULL;    radix_xsuballoc=0;
    free(xsub); free(newo);    xsub=newo=NULL;
    free(xtmp);                xtmp=NULL;          xtmp_alloc=0;
    free(otmp);                otmp=NULL;          otmp_alloc=0;
    free(csort_otmp);          csort_otmp=NULL;    csort_otmp_alloc=0;

    free(cradix_counts);       cradix_counts=NULL; cradix_counts_alloc=0;
    free(cradix_xtmp);         cradix_xtmp=NULL;   cradix_xtmp_alloc=0;   // TO DO: use xtmp already got
    
    UNPROTECT(1);
    return( ans );
}


SEXP fsorted(SEXP x)
{
    // Just checks if ordered and returns FALSE early if not (and don't return ordering if so, unlike forder).
    int tmp,n;
    void *xd;
    n = length(x);
    if (n <= 1) return(ScalarLogical(TRUE));
    if (!isVectorAtomic(x)) error("is.sorted (R level) and fsorted (C level) only to be used on vectors. If needed on a list/data.table, you'll need the order anyway if not sorted, so use if (length(o<-forder(...))) for efficiency in one step, or equivalent at C level");
    xd = DATAPTR(x);
    stackgrps = FALSE;
    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP :
        tmp = isorted(xd, n); break;
    case REALSXP :
        tmp = dsorted(xd, n); break;
    case STRSXP :
        tmp = csorted(xd, n); break;
    default :
        error("type '%s' is not yet supported", type2char(TYPEOF(x)));
    }
    return(ScalarLogical( tmp==1 ? TRUE : FALSE ));
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
// nothing to do with TIMING, but it isn't used so inside TIMING_ON (for dev) avoids not-used compile warning
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


