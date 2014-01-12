
#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <time.h>

// For dev test only

clock_t t[10];

void countingsort(int *x, int n, R_xlen_t *counts, int *ans)
   // No order of x supplied here because x is looped through several times, hence more
   // page efficient to suffer page hits once outside (untested).
{
    R_xlen_t i=0;
    int tmp, xmax = NA_INTEGER, xmin = NA_INTEGER, off, napos;
    off = 0;  // nalast^decreasing ? 0 : 1;
    
    clock_t tt = clock();
    
    while(i<n && x[i]==NA_INTEGER) i++;
    if (i<n) xmax = xmin = x[i];
    for(; i < n; i++) {
	    tmp = x[i];
	    if(tmp == NA_INTEGER) continue;
	    //  if(tmp < 0) error(_("negative value in 'x'"));  counting sort handles negatives fine: TO DO: test
	    if (tmp > xmax) xmax = tmp;
	    else if (tmp < xmin) xmin = tmp;
    }
    if(xmin == NA_INTEGER) {  // all NAs, nothing to do
	    for(i = 0; i < n; i++) ans[i] = i+1;
	    return;
    }
    //Rprintf("xmin = %d, xmax = %d\n", xmin, xmax);
    xmax -= xmin;
    if(xmax > 100000) error("too large a range of values in 'x'");  // TO DO: increase to 1e6? 4 * 1e6 / 1024^2 < 4MB < cache
    napos = off ? 0 : xmax + 1;
    off -= xmin;
    
    t[2] += clock()-tt;
    tt = clock();

    // counts has memset to 0 when created, and at the end of this function
    
    for(i = 0; i < n; i++) {
	    if(x[i] == NA_INTEGER) counts[napos]++;
	    else counts[off+x[i]]++;
    }
    t[3] += clock()-tt;
    tt = clock();
    for(i = 1; i <= xmax+1; i++) counts[i] += counts[i-1];
    t[4] += clock()-tt;
    tt = clock();
    for(i = n-1; i >= 0; i--) {
		tmp = x[i];
		ans[--counts[(tmp == NA_INTEGER) ? napos : off+tmp]] = (int)(i+1);	
	}
	// NB: counts has been cummulated above so is left populated with non 
	// zero; memset now while counts still in cache.
	t[5] += clock()-tt;
    tt = clock();
	memset(counts, 0, (xmax+2)*sizeof(R_xlen_t));
	t[6] += clock()-tt;
	return;
}



SEXP forder(SEXP DT) // TO DO: add argument for which columns to order by
{
    int lastx, nextx, i, j, k;
    
    memset(t, 0, 10*sizeof(clock_t));
    clock_t tt = clock();
    
    int n = length(VECTOR_ELT(DT,0));
    int *x = INTEGER(VECTOR_ELT(DT,0));
    
    R_xlen_t *counts = Calloc(100000+2, R_xlen_t); // or on the stack, like base.  Test.
                                                   // Worth alloting smaller? We will still find xmin and max to save looping,
                                                   // so it's just reserving enough space here, really.
    memset(counts, 0, (100000+2)*sizeof(R_xlen_t));  // 0.4MB
    
    SEXP ans = PROTECT(allocVector(INTSXP, n));
    countingsort(x, n, counts, INTEGER(ans));  // returns 1-based by ref. Deliberately for the case of 1 column input
    int *o = INTEGER(ans);
    
    int *y = Calloc(n, int);     // >> 100,000 so not suitable for stack
    int *otmp = Calloc(n, int);  // But could use largest group (max(counts)) (<< n), from counting sort
    int *newo = Calloc(n, int);
    
    t[0] = clock()-tt;
    
    int *z = INTEGER(VECTOR_ELT(DT,1));  // hard coded for now for dev test
    
    lastx = x[o[0]-1]; 
    i=0; j=0;
    while (i<n) {
      nextx = x[o[i]-1];
      //Rprintf("Loop %d with nextx = %d\n",i, nextx);
      if ( nextx == lastx ) {  // NA are sorted to start, no switch needed
        y[j++] = z[o[i]-1];
        if (i++<n-1) continue;
      }
      //int dd; for (dd=0; dd<j; dd++) Rprintf("%d ", y[dd]); Rprintf("\n");
      tt = clock();
      countingsort(y, j, counts, newo);
      t[1] += clock()-tt;
      for (k=0; k<j; k++) otmp[k] = o[ i-j+1 + newo[k]-2 ];
      //for (k=0; k<j; k++) Rprintf("Suborder %d otmp = %d\n", newo[k], otmp[k]);
      memcpy(o+i-j, otmp, k*sizeof(int)); 
      // Dive here to next column, recurse by ref to o
      //for (dd=0; dd<j; dd++) Rprintf("%d ", o[dd]); Rprintf("\n");
      if (i<n) { y[0] = z[o[i]-1]; j=1; lastx = nextx; i++; }
    }
    Free(y);
    Free(otmp);
    Free(newo);
    Free(counts);
    UNPROTECT(1);
    // for (i=0; i<10; i++) Rprintf("Timing block %d = %8.3f\n", i, 1.0*t[i]/CLOCKS_PER_SEC);
    return( ans );
}



