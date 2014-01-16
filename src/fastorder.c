
#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <time.h>

// For dev test only

clock_t t[10];
int xmax, xmin, off, napos;

void setRange(int *x, int n)
{
    int i, tmp;
    xmin = xmax = NA_INTEGER;
    off = 0; // nalast^decreasing ? 0 : 1;
    i = 0;
    while(i<n && x[i]==NA_INTEGER) i++;
    if (i<n) xmax = xmin = x[i];
    for(; i < n; i++) {
	    tmp = x[i];
	    if(tmp == NA_INTEGER) continue;
        // TO DO: test and document that negatives are fine
	    if (tmp > xmax) xmax = tmp;
	    else if (tmp < xmin) xmin = tmp;
    }
    if(xmin == NA_INTEGER) return; // all NAs, nothing to do
    xmax -= xmin;
    napos = off ? 0 : xmax + 1;
    off -= xmin;
    return;
}

void countingsort(int *x, int n, R_xlen_t *counts, int *ans)
   // No order of x supplied here because x is looped through several times, hence more
   // page efficient to suffer page hits once outside.
{
    R_xlen_t i=0;
    int tmp;
    // clock_t tt = clock();
    // setRange taken outside
    // counts has memset to 0 when created, and at the end of this function, for efficiency.
    if (xmax+1 > 100000) error("Internal error: xmax=range=%d; isorted can't handle range>1e5", xmax+1);  // TO DO: #define 100,000
    for(i = 0; i < n; i++) {
	    if(x[i] == NA_INTEGER) counts[napos]++;
	    else counts[off+x[i]]++;
    }
    // TO DO: at this point if the last count==n then it's all the same number and we can stop now.
    // Idea from Terdiman, then improved on that by not needing to loop through counts.
    //t[3] += clock()-tt; tt = clock();
    tmp = counts[0];                        // *** BLOCK 4 ***
    for (i = 1; i<= xmax+1; i++) {
        if (counts[i]) counts[i] = (tmp += counts[i]);  // when sparse, don't fill in. Helps zero setting below when n<xmax.
        //  counts[i] += counts[i-1];    standard algo as in base
    }
    //t[4] += clock()-tt; tt = clock();
    for(i = n-1; i >= 0; i--) {
		tmp = x[i];
		ans[--counts[(tmp == NA_INTEGER) ? napos : off+tmp]] = (int)(i+1);	
	}
	//t[5] += clock()-tt; tt = clock();
    // counts were cummulated above so leaves non zero. Faster to clear up now ready for next time.
    if (n < xmax) {
        // Many zeros in counts already. Loop through n instead, doesn't matter if we set to 0 several times on any repeats
        counts[napos]=0;
        for (i=0; i<n; i++) {
            tmp=x[i];
            if (tmp!=NA_INTEGER) counts[off+tmp]=0;
        }
    } else {
	    memset(counts, 0, (xmax+2)*sizeof(R_xlen_t));     // *** BLOCK 6 ***
	}
	//t[6] += clock()-tt;
	return;
}


static unsigned int radixcounts[4][256];
// static is important, iradix is called repetitively
// global static rather than local static because first memset is in fastorder so that (skipped) memset can be at the end of iradix

int *iradix(int *x, R_len_t n, int *ans1, int *ans2)
/*
 ans1 and ans2 are flip-flop working memory orderings, same length as x, allocated once in fastorder. Initial contents ignored.
 input x is not modified
 returns either ans1 or ans2, whichever the last non-skipped radix pass leaves the ordering in

 This is counting sort performed backwards from LSB to MSB, with some tricks and short circuits building on Terdiman and Herf.
  http://codercorner.com/RadixSortRevisited.htm 
  http://stereopsis.com/radix.html
 NAs need no special treatment as NA is the most negative integer in R (checked in init.c once, for efficiency) so they naturally sort to the front.
 Using 4-pass 1-byte radix for the following reasons :
 *  11-bit (Herf) reduces to 3-passes (3*11=33) yes, and we need random access to ordering in each pass 1:n so reduction in passes
 *  is good, but ...
 *  ... Terdiman's idea to skip a radix if all values are equal occurs less the wider the radix. A narrower radix benefits more from that.
 *      That's detected here using a single 'if', an improvement on Terdiman's exposition of a single loop to find if any count==n
 *  The pass through counts bites when radix is wider, because we repetitively call this iradix from fastorder forwards.
 *  Herf's parallel histogramming is neat. In 4-pass 1-byte it needs 4*256 storage, that's tiny, and can be static. 4*256 << 3*2048
 *  4-pass 1-byte is simpler and tighter code than 3-pass 11-bit, giving modern optimizers and modern CPUs a better chance. We may get
 *  lucky anyway if one or two of the 4-passes are skipped.
 Recall: there are no comparisons at all in counting and radix, there is wide random access in each radix pass, though.
 We call this on short vectors from fastorder forwards, so x is in cache. This mitigates the issue of worst-case 5 random access passes over n.
 Also used on long vectors where range is too large for direct counting sort.
*/
{
    int i, radix;
    unsigned int thisx=0, shift, *thiscounts;
    int *ans, *ord, *tmp;
    static int skip[4];
    // radixcounts (static global) are memset to 0 in fastorder initially and set back to 0 here at the end to benefit from skipped radix
    
    for (i=0;i<n;i++) {   // parallel histogramming pass; i.e. count occurrences of 0:255 in each byte
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
    ans = ans1;      // initialize flip-flop
    ord = ans2;
    for (i=0; i<n; i++)   // for coding brevity of x[ord[i]-1] below. First pass could be split out to save this, however, it's a sequential pass to setup 1:n, so should be relatively negligible, and again the needless hop via ord in the first pass is sequential too so should also be negligible
        ans[i] = i+1;     // but also it's possible all 4 radix are skipped (x contains the same number n times), hence this 1:n would be returned.
                          // +1 because R is 1-based. Better these tiny +1, then another sweep through n at the end to +1.
                          // TO DO: do away with these +1 when we change fastorder to be 0-based.
    for (radix=0; radix<4; radix++) {
        if (skip[radix]) continue;
        tmp = ord; ord = ans; ans = tmp;     // flip-flop
        shift = radix * 8;
        thiscounts = radixcounts[radix];
        for (i=1;i<256;i++) thiscounts[i] += thiscounts[i-1];
        for(i = n-1; i >= 0; i--) {
		    thisx = x[ord[i]-1] >> shift & 0xFF;
		    ans[--thiscounts[thisx]] = ord[i];
        }
        memset(thiscounts, 0, 256*sizeof(unsigned int));
    }
    return(ans);
}

int isorted(int *x, int n)
{
    // base:is.unsorted does any(is.na(x)) at R level (inefficient), and returns NA (missing sorted cases where NA are at the start).
    // Here we deal with NAs in C and return true if NAs are all at the beginning.
    // We also return -1 if x is sorted in _strictly_ reverse order; a common case we optimize in fastorder.
    // If a vector is in decreasing order *with ties*, then an in-place reverse (no sort) would result in instability of ties.
    // For internal use only in fastorder, which now returns NULL if already sorted (hence no need for separate is.sorted).
    // TO DO: test in big steps first to return faster if unsortedness is at the end (a common case of rbind'ing data to end)
    int i=1;
    // taken out of deep loop ... if (NA_INTEGER != INT_MIN) error("assumed NA is largest negative integer");
    while (i<n && x[i]>=x[i-1]) i++;
    if (i==n) return(1);    // increasing order, possibly with ties
    if (i==1) {
        while (i<n && x[i]<x[i-1]) i++;  
        if (i==n) return(-1);   // strictly decreasing order, no ties; e.g. no more than one NA at the end
    }
    return(0);
}

// TO DO:  If treatment of Inf,-Inf,NA and Nan is biting anywhere, they can we swept to the front in one pass by shifting the next non-special back however many specials have been observed so far.

SEXP forder(SEXP DT) // TO DO: add argument for which columns to order by.  Allow DT to be a vector, too.
{
    int i, j, k, tmp, lastx, thisx, lasti, thisi, range, *otmp;
    Rboolean isSorted = TRUE;
    
    memset(t, 0, 10*sizeof(clock_t));
    clock_t tt = clock();
    
    int n = length(VECTOR_ELT(DT,0));
    int *x = INTEGER(VECTOR_ELT(DT,0));
    
    memset(radixcounts, 0, 4*256*sizeof(unsigned int));
    // memset once here so that memset only needed on the non-skipped radix at the end of iradix

    R_xlen_t *counts = Calloc(100000+2, R_xlen_t); // TO DO: #define 100000.   These are the direct countingsort counts.
                                                   // Worth allocating smaller? We will still find xmin and max to save looping,
                                                   // so it's just reserving enough space here, really.
    memset(counts, 0, (100000+2)*sizeof(R_xlen_t));  // just 0.4MB and we only use the front part of it as large as range, anyway
    
    int *y = Calloc(n, int);    // TO DO: y won't be needed in the 1-column case where counting sort is ok.
    
    int *ans1 = Calloc(n, int); // flip-flop storage of orderings for iradix.  TO DO: won't be needed if radix isn't needed on first long column (2 needless n-length vectors)
    int *ans2 = Calloc(n, int); // n doesn't matter too much as the unused space at the end won't make it into cache.  Just total memory footprint, not speed.
    int *newo;                  // TO DO: use largest group (max(counts)) (<< n), from counting sort (other than iradix may be invoked for first column)
    
    SEXP ans = PROTECT(allocVector(INTSXP, n));  // once for the result, needs to be length n.  TO DO: save allocation if NULL is returned (isSorted==TRUE)
    
    if ((tmp = isorted(x, n))) {
        if (tmp==1)
            for (i=0; i<n; i++) INTEGER(ans)[i] = i+1;
        else {  // tmp==-1, strictly decreasing
            isSorted = FALSE;
            for (i=0; i<n; i++) INTEGER(ans)[i] = n-i;
        }
    } else {
        isSorted = FALSE;
        setRange(x, n);
        if (xmax == NA_INTEGER) error("The all-NA case should have been caught by isorted above");
        range = xmax;   // just a nicer variable name than xmax 
        if (range <= 100000) {
            countingsort(x, n, counts, INTEGER(ans));
            // returns 1-based by ref. Deliberately for the case of 1 column input.  TO DO: change to 0-based and sweep a +1 in the last step.
        } else {
            memcpy(INTEGER(ans), iradix(x, n, ans1, ans2), n*sizeof(int));   // TO DO: save this memcpy, minor though
        }
    }
    int *o = INTEGER(ans);
    
    t[0] = clock()-tt;
    
    int *z = INTEGER(VECTOR_ELT(DT,1));  // hard coded for now for dev test. TO DO: fix
    setRange(z, n);
    if (xmax==NA_INTEGER) error("2nd column is all NA. TO DO: catch here and skip");
    range = xmax;   // just a nicer variable name than xmax
    
    lasti = o[0]-1;
    lastx = x[lasti];
    i=1; j=0;
    while (i<n) {
      thisi = o[i]-1;
      thisx = x[thisi];
      if ( thisx == lastx ) {  // NA are by now sorted to start, no switch needed here for NA since NA_INTEGER==NA_INTEGER ok
        y[j++] = z[lasti];     // trailing random access to z to avoid fetch of z in size 1 subgroups
        lasti = thisi;
        if (++i<n) continue;  //  TO DO: remove this 'if' but still deal with the last group? Only an 'if' at n-level, though, not deeper.
      }
      lastx = thisx;
      i++;
      if (j==0) {lasti = thisi; continue;}  // skip subgroup with 1 item
      // else if j==1 then the next line will make j==2
      y[j++] = z[lasti];   // enter final item of group into y
      lasti = thisi;
      
      // j = 0; continue;  // BASELINE short circuit timing point. Up to here is the cost of creating the subgroup.
      
      if (j==2) {
          if (y[1]<y[0]) { isSorted=FALSE; tmp=o[i-2]; o[i-2]=o[i-3]; o[i-3]=tmp; }   // i++ was already done above, plus it's trailing access
          j = 0;
          continue;
      }
      // Now j (group length) >= 3.
      
      // TO DO: in-place insert sort for j<10, before calling issorted (since when sorted, insert sort is the same as isorted)

      if ((tmp = isorted(y, j))) {
          // isorted ultra low cost, and localised to this group too. Tested on 1m:100m rows, 10:100k levels
          if (tmp<0) {
              isSorted = FALSE;
              otmp = o+i-j-1;
              for (k=0;k<j/2;k++) {      // reverse the order in-place using no function call or working memory 
                  tmp = otmp[k];         // isorted only returns -1 for _strictly_ decreasing order, otherwise ties wouldn't be stable
                  otmp[k] = otmp[j-1-k];
                  otmp[j-1-k] = tmp;
              }
          }
          j=0;
          continue;
      }
      isSorted = FALSE;
      
      // j<10 already dealt with above, before isorted (TO DO)
      if (j < 200 || range > 20000) {
          newo = iradix(y, j, ans1, ans2);   // counting limited to range 1e5, but when looped, iradix's 256 range is better
      } else {
          countingsort(y, j, counts, ans1);  // for j<200, a range>256 can bite when looped (BLOCK 4 and 6), hence iradix better with 256 range
          newo = ans1;
      }
      // TO DO: no need to calc range if j<200 for all subgroups. OTOH, calc of range should be ultra quick, as sequential over n.
      // TO DO: develop bench() into calibrate()
      otmp = newo==ans1 ? ans2 : ans1;
      for (k=0; k<j; k++) otmp[k] = o[ i-j-1 + newo[k]-1 ];  // optimizer will take constants inside [] outside the loop 
      memcpy(o+i-j-1, otmp, j*sizeof(int)); 
      j = 0;
    }
    Free(y);
    Free(counts);
    Free(ans1);
    Free(ans2);
    UNPROTECT(1);
    // for (i=0; i<10; i++) Rprintf("Timing block %d = %8.3f\n", i, 1.0*t[i]/CLOCKS_PER_SEC);
    return( isSorted ? R_NilValue : ans );
}



