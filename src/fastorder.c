
#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

//#define TIMING_ON

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
  #include <time.h>
  static clock_t tblock[10], tstart;
  static int nblock[10];
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
    for (i=0; i<=range; i++) {
        if (counts[i]) {      // cummulate but not through 0's.  Helps resetting zeros when n<range, below.
            push(counts[i]);
            counts[i] = (tmp += counts[i]);
        }
        // TO DO: we can stop cummulating when tmp==n, too. Similar to Terdiman's idea again.
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

static unsigned int radixcounts[4][256] = {{0}};
static int skip[4];
// global because iradix and iradix_r interact and are called repetitively. 
// counts are set back to 0 after each use, to benefit from skipped radix.
static int *radix_xsub=NULL;
static int radix_xsuballoc=0;

static int *otmp=NULL, *xtmp=NULL;  // TO DO: save one of these if possible
static int oxtmpalloc = 0;

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
        radixcounts[0][thisx & 0xFF]++;        // unrolled since inside n-loop
        radixcounts[1][thisx >> 8 & 0xFF]++;
        radixcounts[2][thisx >> 16 & 0xFF]++;
        radixcounts[3][thisx >> 24 & 0xFF]++;
    }
    for (radix=0; radix<4; radix++) {
        // any(count == n) => all radix must have been that value => last x (still thisx) was that value
        i = thisx >> (radix*8) & 0xFF;
        skip[radix] = radixcounts[radix][i] == n;
        if (skip[radix]) radixcounts[radix][i] = 0;  // clear it now, the other counts must be 0 already
    }
    
    radix = 3;  // MSD
    while (radix>=0 && skip[radix]) radix--;
    if (radix==-1) error("All radix are skipped!  Not yet coded.");
    for (i=radix-1; i>=0; i--) {
        if (!skip[i]) memset(radixcounts[i], 0, 256*sizeof(unsigned int));
        // clear the counts as we only needed the parallel pass for skip[] and we're going to use radixcounts again below. Can't use parallel lower counts in MSD radix, unlike LSD.
    }
    thiscounts = radixcounts[radix];
    shift = radix * 8;
    
    itmp = thiscounts[0];
    maxgrpn = itmp;
    for (i=1; i<256; i++) {
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
        radix_xsub = (int *)realloc(radix_xsub, maxgrpn*sizeof(int));  // realloc(NULL) == malloc
        if (!radix_xsub) error("Failed to realloc working memory %d*4bytes (xsub in iradix), radix=%d", maxgrpn, radix);
        radix_xsuballoc = maxgrpn;
    }
    if (oxtmpalloc < maxgrpn) {
        otmp = (int *)realloc(otmp, maxgrpn * sizeof(int));
        if (otmp == NULL) error("Failed to realloc working memory %d*4bytes (otmp in iradix), radix=%d", maxgrpn, radix);
        xtmp = (int *)realloc(xtmp, maxgrpn * sizeof(int));
        if (xtmp == NULL) error("Failed to realloc working memory %d*4bytes (xtmp in iradix), radix=%d", maxgrpn, radix);
        oxtmpalloc = maxgrpn;
    }
    
    itmp = n;
    for (i=255;i>=0;i--)   // undo cummulate earlier
        if (thiscounts[i]) itmp -= (thiscounts[i] = itmp - thiscounts[i]);
    thiscounts[0] = itmp;  // the first non-zero may not have been at 0 originally, but that's ok; now only need the sizes in sequence.
    nextradix = radix-1;
    while (nextradix>=0 && skip[nextradix]) nextradix--;
    itmp = 0;
    for (i=0;i<256;i++) {
        thisgrpn = thiscounts[i];
        if (thisgrpn==0) continue;
        if (thisgrpn == 1 || nextradix==-1) {
            push(thisgrpn);
        } else {
            for (j=0; j<thisgrpn; j++) radix_xsub[j] = x[o[itmp+j]-1];  // this is why this xsub here can't be the same memory as xsub in forder
            iradix_r(radix_xsub, o+itmp, thisgrpn, nextradix);          // changes xsub and o by reference recursively.
        }
        itmp += thisgrpn;
    }
    memset(thiscounts, 0, 256*sizeof(unsigned int));
    // all free()s are at the end of forder.
}

static void iradix_r(int *xsub, int *osub, int n, int radix)
    // xsub is a recursive offset into xsub working memory above in iradix, reordered by reference.
    // osub is a an offset into the main answer o, reordered by reference.
    // radix iterates 3,2,1,0
{
    int i, j, itmp, thisx, thisgrpn, nextradix, shift;
    unsigned int *thiscounts;
    
    shift = radix*8;
    thiscounts = radixcounts[radix];
    
    for (i=0; i<n; i++) {
        thisx = (unsigned int)xsub[i];   // sequential in xsub
        thiscounts[thisx >> shift & 0xFF]++;
    }
    itmp = thiscounts[0];
    for (i=1; i<256; i++)
        if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);  // don't cummulate through 0s, important below
    for (i=n-1; i>=0; i--) {
        thisx = (unsigned int)xsub[i] >> shift & 0xFF;
        j = --thiscounts[thisx];
        otmp[j] = osub[i];
        xtmp[j] = xsub[i];
    }
    memcpy(osub, otmp, n*sizeof(int));
    memcpy(xsub, xtmp, n*sizeof(int));
    
    nextradix = radix-1;
    while (nextradix>=0 && skip[nextradix]) nextradix--;
    // TO DO:  If nextradix==-1 and no further columns from forder,  we're done. We have o. Remember to memset thiscounts before returning.
    
    if (thiscounts[0] != 0) error("Logical error. thiscounts[0]=%d but should have been decremented to 0. radix=%d", thiscounts[0], radix);
    itmp = n;
    for (i=255;i>=0;i--)   // undo cummulate earlier
        if (thiscounts[i]) itmp -= (thiscounts[i] = itmp - thiscounts[i]);
    thiscounts[0] = itmp;  // the first non-zero may not have been at 0 originally, but that's ok; now only need the sizes in sequence.
    itmp = 0;
    for (i=0;i<256;i++) {
        thisgrpn = thiscounts[i];  
        if (thisgrpn==0) continue;
        if (thisgrpn == 1 || nextradix==-1) {
            push(thisgrpn);
        } else {
            if (thisgrpn<200) {   // 200 is guess based on limited testing. Needs calibrate(). Was 50 based on sum(1:50)=1275 worst -vs- 256 cummulate + 256 memset + allowance since reverse order is unlikely
                iinsert(xsub+itmp, osub+itmp, thisgrpn);
            } else {
                iradix_r(xsub+itmp, osub+itmp, thisgrpn, nextradix);
            }
        }
        itmp += thisgrpn;
    }
    memset(thiscounts, 0, 256*sizeof(unsigned int));
}

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
    TBEG()
    for (i=1; i<n; i++) {
        tmp = x[i]-x[i-1];
        if (tmp<0) { gsngrp[flip] = old; return(0); }
        if (tmp==0) tt++; else { push(tt); tt=1; }
    }
    TEND(9)
    push(tt);
    return(1);  // increasing order, possibly with ties
}

// TO DO: for dradix, if treatment of Inf,-Inf,NA and Nan is biting anywhere, they can we swept to the front in one pass by shifting the next non-special back however many specials have been observed so far.

SEXP forder(SEXP DT)
// TO DO: add argument for which columns to order by.
// TO DO: allow DT to be a vector and fast for a unique integer vector such as origorder = iradixorder(firstofeachgroup) in [.data.table ad hoc by 
// TO DO: attach group sizes to the result, if returnGroups is TRUE
{
    int i, j, k, grp, tmp, *osub, thisgrpn;
    Rboolean isSorted = TRUE;
#ifdef TIMING_ON
    memset(tblock, 0, 10*sizeof(clock_t));
    memset(nblock, 0, 10*sizeof(int));
#endif
    TBEG()
    
    int n = length(VECTOR_ELT(DT,0));
    int *x = INTEGER(VECTOR_ELT(DT,0));
    gsmaxalloc = n;  // upper limit for stack size (all size 1 groups). We'll detect and avoid that limit, but if just one non-1 group (say 2), that can't be avoided.
    
    SEXP ans = PROTECT(allocVector(INTSXP, n));  // once for the result, needs to be length n.
    int *o = INTEGER(ans);                       // TO DO: save allocation if NULL is returned (isSorted==TRUE)
    
    // TO DO: if more than 1 column || returnGroups
    stackgrps = TRUE;
    
    if ((tmp = isorted(x, n))) {
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
        setRange(x, n);
        if (range == NA_INTEGER) error("The all-NA case should have been caught by isorted above");
        if (range <= 100000) {
            icount(x, o, n);
        } else {
            iradix(x, o, n);
        }
    }
    
    int maxgrpn = gsmax[flip];  // biggest group in the first column
    // TO DO: if more columns {
    int *xsub = (int *)malloc(maxgrpn * sizeof(int));
    if (xsub==NULL) error("Couldn't allocate xsub in forder. TO DO: improve error message.");
    int *newo = (int *)malloc(maxgrpn * sizeof(int));
    if (newo==NULL) error("Couldn't allocate newo in forder. TO DO: improve error message.");
    // }
    
    TEND(0)
    
    // TO DO: for each column ...
    x = INTEGER(VECTOR_ELT(DT,1));
    int ngrp = gsngrp[flip];
    if (ngrp < n) {
    flipflop();
    // TO DO: if last column && !returnGroups
    stackgrps = FALSE;
    i = 0;
    for (grp=0; grp<ngrp; grp++) {
      thisgrpn = gs[1-flip][grp];
      if (thisgrpn == 1) {i++; push(1); continue;}
      TBEG()
      osub = o+i;
      for (j=0; j<thisgrpn; j++) xsub[j] = x[o[i++]-1];
      TEND(1)
      
      // continue;  // BASELINE short circuit timing point. Up to here is the cost of creating xsub.
      
      if (thisgrpn==2) {
          if (xsub[1]<xsub[0]) { isSorted=FALSE; tmp=osub[0]; osub[0]=osub[1]; osub[1]=tmp; }
          push(1); push(1);
          continue;
      }
      tmp = isorted(xsub, thisgrpn);  // very low cost and localised to thisgrp
      TEND(2)
      if (tmp) {
          // isorted will have already push()'d the groups
          if (tmp<0) {
              isSorted = FALSE;
              for (k=0;k<thisgrpn/2;k++) {      // reverse the order in-place using no function call or working memory 
                  tmp = osub[k];                // isorted only returns -1 for _strictly_ decreasing order, otherwise ties wouldn't be stable
                  osub[k] = osub[thisgrpn-1-k];
                  osub[thisgrpn-1-k] = tmp;
              }
              TEND(3)
          }
          continue;
      }
      isSorted = FALSE;
      if (thisgrpn<200) {    // see comment above in iradix_r re 200.
          iinsert(xsub, osub, thisgrpn);  // pushes inside too
          TEND(4)
          continue;
      }
      setRange(xsub, thisgrpn);   // Tighter range (e.g. copes better with a few abormally large values in some groups), but also, when setRange was once at colum level that caused an extra scan of (long) x first. 10,000 calls to setRange takes just 0.04s i.e. negligible.
      if (range==NA_INTEGER) error("2nd column is all NA in this subgroup. issorted should have caught this above");
      TEND(5)
      if (range < 10000) {   // 1e4 rather than 1e5 here because iterated
          icount(xsub, newo, thisgrpn);  
          TEND(6)
      } else {                            // was (thisgrpn < 200 || range > 20000) then radix
          iradix(xsub, newo, thisgrpn);   // since a short vector with large range can bite icount when iterated (BLOCK 4 and 6)
          TEND(7)                         // TO DO: add calibrate() to init.c
      }
      for (k=0; k<j; k++) xsub[k] = osub[ newo[k]-1 ];   // reuse xsub to reorder osub
      memcpy(osub, xsub, thisgrpn*sizeof(int)); 
      TEND(8)
    }
    }
#ifdef TIMING_ON
    for (i=0; i<10; i++) Rprintf("Timing block %d = %8.3f   %8d\n", i, 1.0*tblock[i]/CLOCKS_PER_SEC, nblock[i]);
    Rprintf("Found %d groups and maxgrpn=%d\n", gsngrp[flip], gsmax[flip]);
#endif
    
    gsfree();
    free(radix_xsub);          radix_xsub=NULL;    radix_xsuballoc=0;
    free(xsub); free(newo);    xsub=newo=NULL;
    free(xtmp); free(otmp);    xtmp=otmp=NULL;     oxtmpalloc=0;
    UNPROTECT(1);

    return( isSorted ? R_NilValue : ans );
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

