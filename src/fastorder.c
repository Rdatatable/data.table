
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
    //clock_t tt = clock();
    // setRange taken outside
    // counts has memset to 0 when created, and at the end of this function, for efficiency.
    for(i = 0; i < n; i++) {
	    if(x[i] == NA_INTEGER) counts[napos]++;
	    else counts[off+x[i]]++;
    }
    //t[3] += clock()-tt;
    //tt = clock();
    tmp = counts[0];    // *** BLOCK 4
    for (i = 1; i<= xmax+1; i++) {
        if (counts[i]) counts[i] = (tmp += counts[i]);  // when sparse, don't fill in. Helps zero setting below when n<xmax.
        //  counts[i] += counts[i-1];    standard algo as in base
    }
    //t[4] += clock()-tt;
    //tt = clock();
    for(i = n-1; i >= 0; i--) {
		tmp = x[i];
		ans[--counts[(tmp == NA_INTEGER) ? napos : off+tmp]] = (int)(i+1);	
	}
	//t[5] += clock()-tt;
    //tt = clock();
    // counts was cummulated above so leaves non zero. Faster to clear up now ready for next time.
    if (n < xmax) {
        // Many zeros in counts already. Loop through n instead, doesn't matter if we set to 0 several times on any repeats
        counts[napos]=0;
        for (i=0; i<n; i++) {
            tmp=x[i];
            if (tmp!=NA_INTEGER) counts[off+tmp]=0;
        }
    } else {
	    memset(counts, 0, (xmax+2)*sizeof(R_xlen_t));     // *** BLOCK 6
	}
	//t[6] += clock()-tt;
	return;
}


// radix order from Arun's file, with some tweaks, mainly taking working memory outside.
// Tested and timed, but not yet used as counting and qsort were fine for levels<=10000. TO DO: retest for large range and large j and hook up.
#define _0(x) (x & 0x7FF)
#define _1(x) (x >> 11 & 0x7FF)
#define _2(x) (x >> 22)
static unsigned int flip_int(unsigned int f) {     //*** Trusting optimizer will inline appropriately.  Explicit inline didn't compile on Mac. 
    return ((int)(f ^ 0x80000000));
} 
static void flip_int_ref(unsigned int *f) {
    *f = ((int)(*f ^ 0x80000000));
}
//static unsigned int invert_flip_int(unsigned int f) {
//    return ((int)(f ^ 0x80000000));
//}

// TO DO: if the flips are for NA (?), NA can be swept to the beginning first in a single forward pass.
// Loop through and move next non-NA back by however many NA so far.
// The flips are so fast though, that an extra sweep wouldn't be worth it.

// 3 histograms on the stack;
#define stack_hist 2048
unsigned int b0[stack_hist * 3];

void iradix(int *x, R_len_t n, int *indx, int *ans, int *ordertmp)   
       // ans is working memory on x
       // ordertmp is working memory on indx
       // !! input x is garbled by reference deliberately !! ok because of the way fastorder uses it, no copy needed.
       // No initial ordering is needed in indx or ordertmp
{
    int i;
    unsigned int pos, fi, si;
    unsigned int sum0 = 0, sum1 = 0, sum2 = 0, tsum;    
    
    int *order = indx;  // TO DO: just rename argument order and remove this line
    unsigned int *array = (unsigned int*)x;  // TO DO: better name than array and sort
    unsigned int *sort = (unsigned int*)ans;
    
    memset(b0, 0, stack_hist*3*sizeof(unsigned int));
    unsigned int *b1 = b0 + stack_hist;
    unsigned int *b2 = b1 + stack_hist;
    
    // Step 1:  parallel histogramming pass
    for (i=0;i<n;i++) {
        fi = flip_int((unsigned int)array[i]);
        b0[_0(fi)]++;
        b1[_1(fi)]++;
        b2[_2(fi)]++;
    }
    for (i=0;i<stack_hist;i++) {
        tsum = b0[i] + sum0;
        b0[i] = sum0 - 1;
        sum0 = tsum;

        tsum = b1[i] + sum1;
        b1[i] = sum1 - 1;
        sum1 = tsum;

        tsum = b2[i] + sum2;
        b2[i] = sum2 - 1;
        sum2 = tsum;
    }
    for (i=0;i<n;i++) {
        fi = array[i];
        flip_int_ref(&fi);
        pos = _0(fi);
        sort[++b0[pos]] = fi;    // sorted input is needed even just for order, since sort used again below.
        order[b0[pos]] = i;
    }
    for (i=0;i<n;i++) {
        si = sort[i];
        pos = _1(si);
        array[++b1[pos]] = si;    // writes the input here.  But this is to y in suborder below, so ok 
        ordertmp[b1[pos]] = order[i];
    }
    for (i=0;i<n;i++) {
        fi = array[i];
        pos = _2(fi);
        //sort[++b2[pos]] = invert_flip_int(fi);   // not needed as only final order is needed, to order the next column
        order[++b2[pos]] = ordertmp[i]+1;  // +1 for 1-based output
    }
    return;
}


int *y;

int cmpfunc (const void * a, const void * b)   // passed to qsort
{
   // !! TO DO: change to 0:n-1 throughout !!
   int i=*(int *)a, j=*(int *)b;
   int ans = y[i-1] - y[j-1];      // hop via 1:n to i) compute order rather than sort in place and ii) provide i-j on next line
   return ( ans==0 ? i-j : ans );  // never 0, to guarantee qsort stability
   // ?: is ultra fast. It's not worth returning y[i-1]-y[j-1] and then checking afterwards (and resolving) any non-stability. Tested repetitively on in-cache 1k vector (saved just 0.5s on 18s at expense of more complex code).
}


SEXP forder(SEXP DT) // TO DO: add argument for which columns to order by
{
    int i, j, k, lastx, thisx, lasti, thisi, range;
    
    memset(t, 0, 10*sizeof(clock_t));
    clock_t tt = clock();
    
    int n = length(VECTOR_ELT(DT,0));
    int *x = INTEGER(VECTOR_ELT(DT,0));
    
    R_xlen_t *counts = Calloc(100000+2, R_xlen_t); // or on the stack, like base.  Test.
                                                   // Worth allocating smaller? We will still find xmin and max to save looping,
                                                   // so it's just reserving enough space here, really.
    memset(counts, 0, (100000+2)*sizeof(R_xlen_t));  // 0.4MB
    
    SEXP ans = PROTECT(allocVector(INTSXP, n));
    setRange(x, n);
    // if xmax is NA_INTEGER, column is all NA. TO DO: catch here and skip
    // TO DO:  if range>100000 allocated, can't use counting
    countingsort(x, n, counts, INTEGER(ans));  // returns 1-based by ref. Deliberately for the case of 1 column input
    int *o = INTEGER(ans);
    
    y = Calloc(n, int);     // y is a global so cmpfun can see it.   >> 100,000 so not suitable for stack
    int *otmp = Calloc(n, int);  // But could use largest group (max(counts)) (<< n), from counting sort
    int *newo = Calloc(n, int);  // Doesn't matter as the unused space at the end won't make it into cache.  Just total memory footprint, not speed.
    
    // 2 working memory vectors that iradix needs
    int *radix_x1 = Calloc(n, int); // becomes 'ans' in iradix
    int *radix_o1 = Calloc(n, int); // becomes 'ordertmp' in iradix
    
    t[0] = clock()-tt;
    
    int *z = INTEGER(VECTOR_ELT(DT,1));  // hard coded for now for dev test
    setRange(z, n);
    // if xmax is NA_INTEGER, column is all NA. TO DO: catch here and skip
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
        if (++i<n) continue;  //  TO DO: remove this 'if' but still deal with the last group
      }
      lastx = thisx;
      i++;
      if (j==0) {lasti = thisi; continue;}  // skip subgroup with 1 item
      y[j++] = z[lasti];   // enter final item of group into y
      lasti = thisi;
      
      //j = 0; continue;  // BASELINE short circuit
      // Now j >= 2  (group length)
      
      // TO DO: special case j=2  (one 'if'), j=3, more?
      
      // Is y sorted?  Turned off currently to test the benefit on ordered input (to simulate almost ordered input)
      // k=0; while(++k<j && y[k-1]<=y[k]);  // ultra low cost, and localised to the this group too. Tested on CJ(1m:100m rows,10:100k levels). 
      // if (k==j) { j=0; continue; }
      // TO DO:  turn back on for production.
      
      if (j < 200 || range > 20000) {
          for (k=0; k<j; k++) newo[k] = k+1;
          qsort(newo, (size_t)j, sizeof(int), cmpfunc);
      } else {
          // TO DO:  if range>100000 allocated, can't use counting.  Won't happen here with range switch above, but we're depending on that here.
          countingsort(y, j, counts, newo);
      }
      //  iradix(y, j, newo, radix_x1, radix_o1);  // This garbles y by reference, but that's deliberate and fine.
      
      for (k=0; k<j; k++) otmp[k] = o[ i-j+1 + newo[k]-3 ];
      memcpy(o+i-j-1, otmp, j*sizeof(int)); 
      j = 0;
    }
    Free(y);
    Free(otmp);
    Free(newo);
    Free(counts);
    Free(radix_x1);
    Free(radix_o1);
    UNPROTECT(1);
    // for (i=0; i<10; i++) Rprintf("Timing block %d = %8.3f\n", i, 1.0*t[i]/CLOCKS_PER_SEC);
    return( ans );
}



