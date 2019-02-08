#include "data.table.h"
//#include <time.h>

static int ngrp = 0;         // number of groups
static int *grpsize = NULL;  // size of each group, used by gmean (and gmedian) not gsum
static int nrow = 0;         // length of underlying x; same as length(ghigh) and length(glow)
static int *irows;           // GForce support for subsets in 'i' (TODO: joins in 'i')
static int irowslen = -1;    // -1 is for irows = NULL
static uint16_t *high=NULL, *low=NULL;  // the group of each x item; a.k.a. which-group-am-I
static int *restrict grp;    // TODO: eventually this can be made local for gforce as won't be needed globally when all functions here use gather
static size_t highSize;
static int shift, mask;
static char *gx=NULL;

static size_t nBatch, batchSize, lastBatchSize;
static int *counts, *tmpcounts;

// for gmedian
static int maxgrpn = 0;
static int *oo = NULL;
static int *ff = NULL;
static int isunsorted = 0;
static union {
  double d;
  long long ll;
} u;

// from R's src/cov.c (for variance / sd)
#ifdef HAVE_LONG_DOUBLE
# define SQRTL sqrtl
#else
# define SQRTL sqrt
#endif

static int nbit(int n)
{
  // returns position of biggest bit; i.e. floor(log2(n))+1 without using fpa
  // not needed to be fast. Just a helper function.
  int nb=0;
  while (n) { nb++; n>>=1; }
  return nb;
}

SEXP gforce(SEXP env, SEXP jsub, SEXP o, SEXP f, SEXP l, SEXP irowsArg) {
  // double started = wallclock();
  if (TYPEOF(env) != ENVSXP) error("env is not an environment");
  // The type of jsub is pretty flexbile in R, so leave checking to eval() below.
  if (!isInteger(o)) error("o is not an integer vector");
  if (!isInteger(f)) error("f is not an integer vector");
  if (!isInteger(l)) error("l is not an integer vector");
  if (isNull(irowsArg)) {
    irows = NULL;
    irowslen = -1;
  }
  else if (isInteger(irowsArg)) {
    irows = INTEGER(irowsArg);
    irowslen = LENGTH(irowsArg);
  }
  else error("irowsArg is neither an integer vector nor NULL");  // # nocov
  ngrp = LENGTH(l);
  if (LENGTH(f) != ngrp) error("length(f)=%d != length(l)=%d", LENGTH(f), ngrp);
  nrow=0;
  grpsize = INTEGER(l);
  maxgrpn = 0;
  for (int i=0; i<ngrp; i++) {
    nrow+=grpsize[i];
    if (grpsize[i]>maxgrpn) maxgrpn = grpsize[i];  // old comment to be checked: 'needed for #2046 and #2111 when maxgrpn attribute is not attached to empty o'
  }
  if (LENGTH(o) && LENGTH(o)!=nrow) error("o has length %d but sum(l)=%d", LENGTH(o), nrow);
  {
    SEXP tt = getAttrib(o, install("maxgrpn"));
    if (length(tt)==1 && INTEGER(tt)[0]!=maxgrpn) error("Internal error: o's maxgrpn attribute mismatches recalculated maxgrpn"); // # nocov
  }

  int nb = nbit(ngrp-1);
  shift = nb/2;    // /2 so that high and low can be uint16_t, and no limit (even for nb=4) to stress-test.
  // shift=MAX(nb-8,0); if (shift>16) shift=nb/2;     // TODO: when we have stress-test off mode, do this
  mask = (1<<shift)-1;
  highSize = ((ngrp-1)>>shift) + 1;

  grp = (int *)R_alloc(nrow, sizeof(int));   // TODO: use malloc and made this local as not needed globally when all functions here use gather
                                             // maybe better to malloc to avoid R's heap. This grp isn't global, so it doesn't need to be R_alloc
  const int *restrict fp = INTEGER(f);

  nBatch = MIN((nrow+1)/2, getDTthreads()*2);  // *2 to reduce last-thread-home. TODO: experiment. The higher this is though, the bigger is counts[]
  batchSize = MAX(1, (nrow-1)/nBatch);
  lastBatchSize = nrow - (nBatch-1)*batchSize;
  // We deliberate use, for example, 40 batches of just 14 rows, to stress-test tests. This strategy proved to be a good one as #3204 immediately came to light.
  // TODO: enable stress-test mode in tests only (#3205) which can be turned off by default in release to decrease overhead on small data
  //       if that is established to be biting (it may be fine).
  if (nBatch<1 || batchSize<1 || lastBatchSize<1) {
    error("Internal error: nrow=%d  ngrp=%d  nbit=%d  shift=%d  highSize=%d  nBatch=%d  batchSize=%d  lastBatchSize=%d\n",  // # nocov
           nrow, ngrp, nb, shift, highSize, nBatch, batchSize, lastBatchSize);                                              // # nocov
  }
  // initial population of g:
  #pragma omp parallel for num_threads(getDTthreads())
  for (int g=0; g<ngrp; g++) {
    int *elem = grp + fp[g]-1;
    for (int j=0; j<grpsize[g]; j++)  elem[j] = g;
  }
  //Rprintf("gforce initial population of grp took %.3f\n", wallclock()-started); started=wallclock();
  if (LENGTH(o)) {
    isunsorted = 1; // for gmedian

    // What follows is more cache-efficient version of this scattered assign :
    // for (int g=0; g<ngrp; g++) {
    //  const int *elem = op + fp[g]-1;
    //  for (int j=0; j<grpsize[g]; j++)  grp[ elem[j]-1 ] = g;
    //}

    const int *restrict op = INTEGER(o);  // o is a permutation of 1:nrow
    int nb = nbit(nrow-1);
    int shift = MAX(nb-8, 0);  // TODO: experiment nb/2.  Here it doesn't have to be /2 currently.
    int highSize = ((nrow-1)>>shift) + 1;
    //Rprintf("When assigning grp[o] = g, highSize=%d  nb=%d  shift=%d  nBatch=%d\n", highSize, nb, shift, nBatch);
    int *counts = calloc(nBatch*highSize, sizeof(int));  // TODO: cache-line align and make highSize a multiple of 64
    int *TMP   = malloc(nrow*2*sizeof(int));
    if (!counts || !TMP ) error("Internal error: Failed to allocate counts or TMP when assigning g in gforce");
    #pragma omp parallel for num_threads(getDTthreads())   // schedule(dynamic,1)
    for (int b=0; b<nBatch; b++) {
      const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
      const int *my_o = op + b*batchSize;
      int *restrict my_counts = counts + b*highSize;
      for (int i=0; i<howMany; i++) {
        const int w = (my_o[i]-1) >> shift;
        my_counts[w]++;
      }
      for (int i=0, cum=0; i<highSize; i++) {
        int tmp = my_counts[i];
        my_counts[i] = cum;
        cum += tmp;
      }
      const int *restrict my_g = grp + b*batchSize;
      int *restrict my_tmp = TMP + b*2*batchSize;
      for (int i=0; i<howMany; i++) {
        const int w = (my_o[i]-1) >> shift;   // could use my_high but may as well use my_pg since we need my_pg anyway for the lower bits next too
        int *p = my_tmp + 2*my_counts[w]++;
        *p++ = my_o[i]-1;
        *p   = my_g[i];
      }
    }
    //Rprintf("gforce assign TMP (o,g) pairs took %.3f\n", wallclock()-started); started=wallclock();
    #pragma omp parallel for num_threads(getDTthreads())
    for (int h=0; h<highSize; h++) {  // very important that high is first loop here
      for (int b=0; b<nBatch; b++) {
        const int start = h==0 ? 0 : counts[ b*highSize + h - 1 ];
        const int end   = counts[ b*highSize + h ];
        const int *restrict p = TMP + b*2*batchSize + start*2;
        for (int k=start; k<end; k++, p+=2) {
          grp[p[0]] = p[1];  // TODO: could write high here, and initial low.   ** If so, same in initial population when o is missing **
        }
      }
    }
    free(counts);
    free(TMP);
    //Rprintf("gforce assign TMP [ (o,g) pairs ] back to grp took %.3f\n", wallclock()-started); started=wallclock();
  }

  high = (uint16_t *)R_alloc(nrow, sizeof(uint16_t));  // maybe better to malloc to avoid R's heap, but safer to R_alloc since it's done via eval()
  low  = (uint16_t *)R_alloc(nrow, sizeof(uint16_t));
  // global ghigh and glow because the g* functions (inside jsub) share this common memory

  gx = (char *)R_alloc(nrow, sizeof(double));  // enough for a copy of one column (or length(irows) if supplied)


  counts = (int *)S_alloc(nBatch*highSize, sizeof(int));  // (S_ zeros) TODO: cache-line align and make highSize a multiple of 64
  tmpcounts = (int *)R_alloc(getDTthreads()*highSize, sizeof(int));

  const int *restrict gp = grp;
  #pragma omp parallel for num_threads(getDTthreads())   // schedule(dynamic,1)
  for (int b=0; b<nBatch; b++) {
    int *restrict my_counts = counts + b*highSize;
    uint16_t *restrict my_high = high + b*batchSize;
    const int *my_pg = gp + b*batchSize;
    const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
    for (int i=0; i<howMany; i++) {
      const int w = my_pg[i] >> shift;
      my_counts[w]++;
      my_high[i] = (uint16_t)w;  // reduce 4 bytes to 2
    }
    for (int i=0, cum=0; i<highSize; i++) {
      int tmp = my_counts[i];
      my_counts[i] = cum;
      cum += tmp;
    }
    uint16_t *restrict my_low = low + b*batchSize;
    int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
    memcpy(my_tmpcounts, my_counts, highSize*sizeof(int));
    for (int i=0; i<howMany; i++) {
      const int w = my_pg[i] >> shift;   // could use my_high but may as well use my_pg since we need my_pg anyway for the lower bits next too
      my_low[my_tmpcounts[w]++] = (uint16_t)(my_pg[i] & mask);
    }
    // counts is now cumulated within batch (with ending values) and we leave it that way
    // memcpy(counts + b*256, myCounts, 256*sizeof(int));  // save cumulate for later, first bucket contains position of next. For ease later in the very last batch.
  }
  //Rprintf("gforce assign high and low took %.3f\n", wallclock()-started); started=wallclock();

  oo = INTEGER(o);
  ff = INTEGER(f);

  SEXP ans = PROTECT( eval(jsub, env) );
  //Rprintf("gforce eval took %.3f\n", wallclock()-started);
  // if this eval() fails with R error, R will release grp for us. Which is why we use R_alloc above.
  if (isVectorAtomic(ans)) {
    SEXP tt = ans;
    ans = PROTECT(allocVector(VECSXP, 1));
    SET_VECTOR_ELT(ans, 0, tt);
    UNPROTECT(1);
  }
  ngrp = 0; maxgrpn=0; irowslen = -1; isunsorted = 0;

  UNPROTECT(1);
  return(ans);
}

void *gather(SEXP x, bool *anyNA)
{
  //double started = wallclock();
  switch (TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *restrict thisx = INTEGER(x);
    #pragma omp parallel for num_threads(getDTthreads())
    for (int b=0; b<nBatch; b++) {
      int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
      memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(int));   // original cumulated   // already cumulated for this batch
      int *restrict my_gx = (int *)gx + b*batchSize;
      const uint16_t *my_high = high + b*batchSize;
      const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
      bool my_anyNA = false;
      if (irowslen==-1) {
        const int *my_x = thisx + b*batchSize;
        for (int i=0; i<howMany; i++) {
          const int elem = my_x[i];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          if (elem==NA_INTEGER) my_anyNA = true;
        }
      } else {
        const int *my_x = irows + b*batchSize;
        for (int i=0; i<howMany; i++) {
          int elem = thisx[ my_x[i]-1 ];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          if (elem==NA_INTEGER) my_anyNA = true;
        }
      }
      if (my_anyNA) *anyNA = true;  // naked write ok since just bool and always writing true; and no performance issue as maximum nBatch writes
    }
  } break;
  case REALSXP: {
    if (!INHERITS(x, char_integer64)) {
      const double *restrict thisx = REAL(x);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int b=0; b<nBatch; b++) {
        int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
        memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(int));
        double *restrict my_gx = (double *)gx + b*batchSize;
        const uint16_t *my_high = high + b*batchSize;
        const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
        bool my_anyNA = false;
        if (irowslen==-1) {
          const double *my_x = thisx + b*batchSize;
          for (int i=0; i<howMany; i++) {
            const double elem = my_x[i];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (ISNAN(elem)) my_anyNA = true;   // R's ISNAN includes NA; i.e. defined as C isnan with some platform specific differences (perhaps historic)
          }
        } else {
          const int *my_x = irows + b*batchSize;
          for (int i=0; i<howMany; i++) {
            double elem = thisx[ my_x[i]-1 ];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (ISNAN(elem)) my_anyNA = true;
          }
        }
        if (my_anyNA) *anyNA = true;
      }
    } else {
      const int64_t *restrict thisx = (int64_t *)REAL(x);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int b=0; b<nBatch; b++) {
        int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
        memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(int));
        int64_t *restrict my_gx = (int64_t *)gx + b*batchSize;
        const uint16_t *my_high = high + b*batchSize;
        const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
        bool my_anyNA = false;
        if (irowslen==-1) {
          const int64_t *my_x = thisx + b*batchSize;
          for (int i=0; i<howMany; i++) {
            const int64_t elem = my_x[i];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (elem==INT64_MIN) my_anyNA = true;
          }
        } else {
          const int *my_x = irows + b*batchSize;
          for (int i=0; i<howMany; i++) {
            int64_t elem = thisx[ my_x[i]-1 ];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (elem==INT64_MIN) my_anyNA = true;
          }
        }
        if (my_anyNA) *anyNA = true;
      }
    }
  } break;
  default :
    error("gather implemented for INTSXP and REALSXP but not '%s'", type2char(TYPEOF(x)));   // # nocov
  }
  //Rprintf("gather took %.3fs\n", wallclock()-started);
  return gx;
}

SEXP gsum(SEXP x, SEXP narmArg)
{
  if (!isLogical(narmArg) || LENGTH(narmArg)!=1 || LOGICAL(narmArg)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
  const bool narm = LOGICAL(narmArg)[0];
  if (!isVectorAtomic(x)) error("GForce sum can only be applied to columns, not .SD or similar. To sum all items in a list such as .SD, either add the prefix base::sum(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,sum),by=,.SDcols=]'");
  if (inherits(x, "factor")) error("sum is not meaningful for factors.");
  const int n = (irowslen == -1) ? length(x) : irowslen;
  //clock_t start = clock();
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gsum", nrow, n);
  bool anyNA=false;
  SEXP ans;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(INTSXP, ngrp));
    int *restrict ansp = INTEGER(ans);
    memset(ansp, 0, ngrp*sizeof(int));
    bool overflow=false;
    //double started = wallclock();
    if (!anyNA) {
      #pragma omp parallel for num_threads(getDTthreads()) //schedule(dynamic,1)
      for (int h=0; h<highSize; h++) {   // very important that high is first loop here
        int *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const int *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const int a = _ans[my_low[i]];
            const int b = my_gx[i];
            if ((a>0 && b>INT_MAX-a) || (a<0 && b<NA_INTEGER+1-a)) overflow=true;
            else _ans[my_low[i]] += b;  // naked by design; each thread does all of each h for all batches
          }
        }
      }
    } else {
      #pragma omp parallel for num_threads(getDTthreads())
      for (int h=0; h<highSize; h++) {
        int *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const int *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const int a = _ans[my_low[i]];
            if (a==NA_INTEGER) continue;
            const int b = my_gx[i];
            if (b==NA_INTEGER) {
              if (!narm) _ans[my_low[i]]=NA_INTEGER;
              continue;
            }
            if ((a>0 && b>INT_MAX-a) || (a<0 && b<NA_INTEGER+1-a)) overflow=true;
            else _ans[my_low[i]] += b;
          }
        }
      }
    }
    //Rprintf("gsum int took %.3f\n", wallclock()-started);
    if (overflow) {
      UNPROTECT(1); // discard the result with overflow
      warning("The sum of an integer column for a group was more than type 'integer' can hold so the result has been coerced to 'numeric' automatically for convenience.");
      ans = PROTECT(allocVector(REALSXP, ngrp));
      double *restrict ansp = REAL(ans);
      memset(ansp, 0, ngrp*sizeof(double));
      #pragma omp parallel for num_threads(getDTthreads())
      for (int h=0; h<highSize; h++) {
        double *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const int *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          // rare and slower so no need to switch on anyNA
          for (int i=0; i<howMany; i++) {
            const int elem = my_gx[i];
            if (elem==NA_INTEGER) {
              if (!narm) _ans[my_low[i]]=NA_REAL;
              continue;
            }
            _ans[my_low[i]] += elem;  // let NA_REAL propagate
          }
        }
      }
    }
  } break;
  case REALSXP: {
    const double *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(REALSXP, ngrp));
    double *restrict ansp = REAL(ans);
    memset(ansp, 0, ngrp*sizeof(double));
    if (!narm || !anyNA) {
      #pragma omp parallel for num_threads(getDTthreads())
      for (int h=0; h<highSize; h++) {
        double *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const double *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            _ans[my_low[i]] += my_gx[i];  // let NA propagate when !narm
          }
        }
      }
    } else {
      // narm==true and anyNA==true
      #pragma omp parallel for num_threads(getDTthreads())
      for (int h=0; h<highSize; h++) {
        double *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const double *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const double elem = my_gx[i];
            if (!ISNAN(elem)) _ans[my_low[i]] += elem;
          }
        }
      }
    }
  } break;
  default:
    error("Type '%s' not supported by GForce sum (gsum). Either add the prefix base::sum(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  // Rprintf("this gsum took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return(ans);
}

SEXP gmean(SEXP x, SEXP narm)
{
  SEXP ans;
  int protecti=0;
  //clock_t start = clock();
  if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
  if (!isVectorAtomic(x)) error("GForce mean can only be applied to columns, not .SD or similar. Likely you're looking for 'DT[,lapply(.SD,mean),by=,.SDcols=]'. See ?data.table.");
  if (inherits(x, "factor")) error("mean is not meaningful for factors.");
  if (!LOGICAL(narm)[0]) {
    ans = PROTECT(gsum(x,narm)); protecti++;
    switch(TYPEOF(ans)) {
    case LGLSXP: case INTSXP:
      ans = PROTECT(coerceVector(ans, REALSXP)); protecti++;
    case REALSXP: {
      double *xd = REAL(ans);
      for (int i=0; i<ngrp; i++) *xd++ /= grpsize[i];  // let NA propogate
    } break;
    default :
      error("Internal error: gsum returned type '%s'. typeof(x) is '%s'", type2char(TYPEOF(ans)), type2char(TYPEOF(x))); // # nocov
    }
    UNPROTECT(protecti);
    return(ans);
  }
  // na.rm=TRUE.  Similar to gsum, but we need to count the non-NA as well for the divisor
  const int n = (irowslen == -1) ? length(x) : irowslen;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gsum", nrow, n);

  long double *s = calloc(ngrp, sizeof(long double));
  if (!s) error("Unable to allocate %d * %d bytes for sum in gmean na.rm=TRUE", ngrp, sizeof(long double));

  int *c = calloc(ngrp, sizeof(int));
  if (!c) error("Unable to allocate %d * %d bytes for counts in gmean na.rm=TRUE", ngrp, sizeof(int));

  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP:
    for (int i=0; i<n; i++) {
      int thisgrp = grp[i];
      int ix = (irowslen == -1) ? i : irows[i]-1;
      if(INTEGER(x)[ix] == NA_INTEGER) continue;
      s[thisgrp] += INTEGER(x)[ix];  // no under/overflow here, s is long double
      c[thisgrp]++;
    }
    break;
  case REALSXP:
    for (int i=0; i<n; i++) {
      int thisgrp = grp[i];
      int ix = (irowslen == -1) ? i : irows[i]-1;
      if (ISNAN(REAL(x)[ix])) continue;
      s[thisgrp] += REAL(x)[ix];
      c[thisgrp]++;
    }
    break;
  default:
    free(s); free(c);
    error("Type '%s' not supported by GForce mean (gmean) na.rm=TRUE. Either add the prefix base::mean(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  ans = PROTECT(allocVector(REALSXP, ngrp));
  for (int i=0; i<ngrp; i++) {
    if (c[i]==0) { REAL(ans)[i] = R_NaN; continue; }  // NaN to follow base::mean
    s[i] /= c[i];
    if (s[i] > DBL_MAX) REAL(ans)[i] = R_PosInf;
    else if (s[i] < -DBL_MAX) REAL(ans)[i] = R_NegInf;
    else REAL(ans)[i] = (double)s[i];
  }
  free(s); free(c);
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  // Rprintf("this gmean na.rm=TRUE took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return(ans);
}

// gmin
SEXP gmin(SEXP x, SEXP narm)
{
  if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
  if (!isVectorAtomic(x)) error("GForce min can only be applied to columns, not .SD or similar. To find min of all items in a list such as .SD, either add the prefix base::min(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,min),by=,.SDcols=]'");
  if (inherits(x, "factor") && !inherits(x, "ordered")) error("min is not meaningful for factors.");
  R_len_t i, ix, thisgrp=0;
  int n = (irowslen == -1) ? length(x) : irowslen;
  //clock_t start = clock();
  SEXP ans;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gmin", nrow, n);
  int protecti=0;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP:
    ans = PROTECT(allocVector(INTSXP, ngrp)); protecti++;
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<ngrp; i++) INTEGER(ans)[i] = INT_MAX;
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (INTEGER(x)[ix] < INTEGER(ans)[thisgrp])   // NA_INTEGER==INT_MIN checked in init.c
          INTEGER(ans)[thisgrp] = INTEGER(x)[ix];
      }
    } else {
      for (i=0; i<ngrp; i++) INTEGER(ans)[i] = NA_INTEGER;
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (INTEGER(x)[ix] == NA_INTEGER) continue;
        if (INTEGER(ans)[thisgrp] == NA_INTEGER || INTEGER(x)[ix] < INTEGER(ans)[thisgrp])
          INTEGER(ans)[thisgrp] = INTEGER(x)[ix];
      }
      for (i=0; i<ngrp; i++) {
        if (INTEGER(ans)[i] == NA_INTEGER) {
          warning("No non-missing values found in at least one group. Coercing to numeric type and returning 'Inf' for such groups to be consistent with base");
          ans = PROTECT(coerceVector(ans, REALSXP)); protecti++;
          for (i=0; i<ngrp; i++) {
            if (ISNA(REAL(ans)[i])) REAL(ans)[i] = R_PosInf;
          }
          break;
        }
      }
    }
    break;
  case STRSXP:
    ans = PROTECT(allocVector(STRSXP, ngrp)); protecti++;
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<ngrp; i++) SET_STRING_ELT(ans, i, R_BlankString);
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (STRING_ELT(x, ix) == NA_STRING) {
          SET_STRING_ELT(ans, thisgrp, NA_STRING);
        } else {
          if (STRING_ELT(ans, thisgrp) == R_BlankString ||
            (STRING_ELT(ans, thisgrp) != NA_STRING && strcmp(CHAR(STRING_ELT(x, ix)), CHAR(STRING_ELT(ans, thisgrp))) < 0 )) {
            SET_STRING_ELT(ans, thisgrp, STRING_ELT(x, ix));
          }
        }
      }
    } else {
      for (i=0; i<ngrp; i++) SET_STRING_ELT(ans, i, NA_STRING);
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (STRING_ELT(x, ix) == NA_STRING) continue;
        if (STRING_ELT(ans, thisgrp) == NA_STRING ||
          strcmp(CHAR(STRING_ELT(x, ix)), CHAR(STRING_ELT(ans, thisgrp))) < 0) {
          SET_STRING_ELT(ans, thisgrp, STRING_ELT(x, ix));
        }
      }
      for (i=0; i<ngrp; i++) {
        if (STRING_ELT(ans, i)==NA_STRING) {
          warning("No non-missing values found in at least one group. Returning 'NA' for such groups to be consistent with base");
          break;
        }
      }
    }
    break;
  case REALSXP:
    ans = PROTECT(allocVector(REALSXP, ngrp)); protecti++;
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<ngrp; i++) REAL(ans)[i] = R_PosInf;
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (ISNAN(REAL(x)[ix]) || REAL(x)[ix] < REAL(ans)[thisgrp])
          REAL(ans)[thisgrp] = REAL(x)[ix];
      }
    } else {
      for (i=0; i<ngrp; i++) REAL(ans)[i] = NA_REAL;
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (ISNAN(REAL(x)[ix])) continue;
        if (ISNAN(REAL(ans)[thisgrp]) || REAL(x)[ix] < REAL(ans)[thisgrp])
          REAL(ans)[thisgrp] = REAL(x)[ix];
      }
      for (i=0; i<ngrp; i++) {
        if (ISNAN(REAL(ans)[i])) {
          warning("No non-missing values found in at least one group. Returning 'Inf' for such groups to be consistent with base");
          for (; i<ngrp; i++) if (ISNAN(REAL(ans)[i])) REAL(ans)[i] = R_PosInf;
          break;
        }
      }
    }
    break;
  default:
    error("Type '%s' not supported by GForce min (gmin). Either add the prefix base::min(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
  UNPROTECT(protecti);  // ans + maybe 1 coerced ans
  // Rprintf("this gmin took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return(ans);
}

// gmax
SEXP gmax(SEXP x, SEXP narm)
{
  if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
  if (!isVectorAtomic(x)) error("GForce max can only be applied to columns, not .SD or similar. To find max of all items in a list such as .SD, either add the prefix base::max(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,max),by=,.SDcols=]'");
  if (inherits(x, "factor") && !inherits(x, "ordered")) error("max is not meaningful for factors.");
  R_len_t i, ix, thisgrp=0;
  int n = (irowslen == -1) ? length(x) : irowslen;
  //clock_t start = clock();
  SEXP ans;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gmax", nrow, n);

  // TODO rework gmax in the same way as gmin and remove this *update
  char *update = (char *)R_alloc(ngrp, sizeof(char));
  for (int i=0; i<ngrp; i++) update[i] = 0;
  int protecti=0;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP:
    ans = PROTECT(allocVector(INTSXP, ngrp)); protecti++;
    for (i=0; i<ngrp; i++) INTEGER(ans)[i] = 0;
    if (!LOGICAL(narm)[0]) { // simple case - deal in a straightforward manner first
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (INTEGER(x)[ix] != NA_INTEGER && INTEGER(ans)[thisgrp] != NA_INTEGER) {
          if ( update[thisgrp] != 1 || INTEGER(ans)[thisgrp] < INTEGER(x)[ix] ) {
            INTEGER(ans)[thisgrp] = INTEGER(x)[ix];
            if (update[thisgrp] != 1) update[thisgrp] = 1;
          }
        } else  INTEGER(ans)[thisgrp] = NA_INTEGER;
      }
    } else {
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (INTEGER(x)[ix] != NA_INTEGER) {
          if ( update[thisgrp] != 1 || INTEGER(ans)[thisgrp] < INTEGER(x)[ix] ) {
            INTEGER(ans)[thisgrp] = INTEGER(x)[ix];
            if (update[thisgrp] != 1) update[thisgrp] = 1;
          }
        } else {
          if (update[thisgrp] != 1) {
            INTEGER(ans)[thisgrp] = NA_INTEGER;
          }
        }
      }
      for (i=0; i<ngrp; i++) {
        if (update[i] != 1)  {// equivalent of INTEGER(ans)[thisgrp] == NA_INTEGER
          warning("No non-missing values found in at least one group. Coercing to numeric type and returning 'Inf' for such groups to be consistent with base");
          ans = PROTECT(coerceVector(ans, REALSXP)); protecti++;
          for (i=0; i<ngrp; i++) {
            if (update[i] != 1) REAL(ans)[i] = -R_PosInf;
          }
          break;
        }
      }
    }
    break;
  case STRSXP:
    ans = PROTECT(allocVector(STRSXP, ngrp)); protecti++;
    for (i=0; i<ngrp; i++) SET_STRING_ELT(ans, i, mkChar(""));
    if (!LOGICAL(narm)[0]) { // simple case - deal in a straightforward manner first
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (STRING_ELT(x,ix) != NA_STRING && STRING_ELT(ans, thisgrp) != NA_STRING) {
          if ( update[thisgrp] != 1 || strcmp(CHAR(STRING_ELT(ans, thisgrp)), CHAR(STRING_ELT(x,ix))) < 0 ) {
            SET_STRING_ELT(ans, thisgrp, STRING_ELT(x, ix));
            if (update[thisgrp] != 1) update[thisgrp] = 1;
          }
        } else  SET_STRING_ELT(ans, thisgrp, NA_STRING);
      }
    } else {
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if (STRING_ELT(x, ix) != NA_STRING) {
          if ( update[thisgrp] != 1 || strcmp(CHAR(STRING_ELT(ans, thisgrp)), CHAR(STRING_ELT(x, ix))) < 0 ) {
            SET_STRING_ELT(ans, thisgrp, STRING_ELT(x, ix));
            if (update[thisgrp] != 1) update[thisgrp] = 1;
          }
        } else {
          if (update[thisgrp] != 1) {
            SET_STRING_ELT(ans, thisgrp, NA_STRING);
          }
        }
      }
      for (i=0; i<ngrp; i++) {
        if (update[i] != 1)  {// equivalent of INTEGER(ans)[thisgrp] == NA_INTEGER
          warning("No non-missing values found in at least one group. Returning 'NA' for such groups to be consistent with base");
          break;
        }
      }
    }
    break;
  case REALSXP:
    ans = PROTECT(allocVector(REALSXP, ngrp)); protecti++;
    for (i=0; i<ngrp; i++) REAL(ans)[i] = 0;
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if ( !ISNA(REAL(x)[ix]) && !ISNA(REAL(ans)[thisgrp]) ) {
          if ( update[thisgrp] != 1 || REAL(ans)[thisgrp] < REAL(x)[ix] ||
             (ISNAN(REAL(x)[ix]) && !ISNAN(REAL(ans)[thisgrp])) ) { // #1461
            REAL(ans)[thisgrp] = REAL(x)[ix];
            if (update[thisgrp] != 1) update[thisgrp] = 1;
          }
        } else REAL(ans)[thisgrp] = NA_REAL;
      }
    } else {
      for (i=0; i<n; i++) {
        thisgrp = grp[i];
        ix = (irowslen == -1) ? i : irows[i]-1;
        if ( !ISNAN(REAL(x)[ix]) ) { // #1461
          if ( update[thisgrp] != 1 || REAL(ans)[thisgrp] < REAL(x)[ix] ) {
            REAL(ans)[thisgrp] = REAL(x)[ix];
            if (update[thisgrp] != 1) update[thisgrp] = 1;
          }
        } else {
          if (update[thisgrp] != 1) {
            REAL(ans)[thisgrp] = -R_PosInf;
          }
        }
      }
      // everything taken care of already. Just warn if all NA groups have occurred at least once
      for (i=0; i<ngrp; i++) {
        if (update[i] != 1)  { // equivalent of REAL(ans)[thisgrp] == -R_PosInf
          warning("No non-missing values found in at least one group. Returning '-Inf' for such groups to be consistent with base");
          break;
        }
      }
    }
    break;
  default:
    error("Type '%s' not supported by GForce max (gmax). Either add the prefix base::max(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
  UNPROTECT(protecti);
  // Rprintf("this gmax took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return(ans);
}

// gmedian, always returns numeric type (to avoid as.numeric() wrap..)
SEXP gmedian(SEXP x, SEXP narm) {

  if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
  if (!isVectorAtomic(x)) error("GForce median can only be applied to columns, not .SD or similar. To find median of all items in a list such as .SD, either add the prefix stats::median(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,median),by=,.SDcols=]'");
  if (inherits(x, "factor")) error("median is not meaningful for factors.");
  R_len_t i=0, j=0, k=0, imed=0, thisgrpsize=0, medianindex=0, nacount=0;
  double val = 0.0;
  Rboolean isna = FALSE, isint64 = FALSE;
  SEXP ans, sub, klass;
  void *ptr;
  int n = (irowslen == -1) ? length(x) : irowslen;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gmedian", nrow, n);
  switch(TYPEOF(x)) {
  case REALSXP:
    klass = getAttrib(x, R_ClassSymbol);
    isint64 = (isString(klass) && STRING_ELT(klass, 0) == char_integer64);
    ans = PROTECT(allocVector(REALSXP, ngrp));
    sub = PROTECT(allocVector(REALSXP, maxgrpn)); // allocate once upfront
    ptr = REAL(sub);
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<ngrp; i++) {
        isna = FALSE;
        thisgrpsize = grpsize[i];
        SETLENGTH(sub, thisgrpsize);
        for (j=0; j<thisgrpsize; j++) {
          k = ff[i]+j-1;
          if (isunsorted) k = oo[k]-1;
          k = (irowslen == -1) ? k : irows[k]-1;
          // TODO: raise this if-statement?
          if (!isint64) {
            if (!ISNAN(REAL(x)[k])) {
              REAL(sub)[j] = REAL(x)[k];
            } else {
              REAL(ans)[i] = NA_REAL;
              isna = TRUE; break;
            }
          } else {
            u.d = REAL(x)[k];
            if (u.ll != NA_INT64_LL) {
              REAL(sub)[j] = (double)u.ll;
            } else {
              REAL(ans)[i] = NA_REAL;
              isna = TRUE; break;
            }
          }
        }
        if (isna) continue;
        medianindex = (R_len_t)(ceil((double)(thisgrpsize)/2));
        REAL(ans)[i] = dquickselect(ptr, thisgrpsize, medianindex-1); // 0-indexed
        // all elements to the left of thisgrpsize/2 is < the value at that index
        // we just need to get min of last half
        if (thisgrpsize % 2 == 0) {
          val = REAL(sub)[medianindex]; // 0-indexed
          for (imed=medianindex+1; imed<thisgrpsize; imed++) {
            val = REAL(sub)[imed] > val ? val : REAL(sub)[imed];
          }
          REAL(ans)[i] = (REAL(ans)[i] + val)/2.0;
        }
      }
    } else {
      for (i=0; i<ngrp; i++) {
        nacount = 0;
        thisgrpsize = grpsize[i];
        for (j=0; j<thisgrpsize; j++) {
          k = ff[i]+j-1;
          if (isunsorted) k = oo[k]-1;
          k = (irowslen == -1) ? k : irows[k]-1;
          // TODO: raise this if-statement?
          if (!isint64) {
            if (!ISNAN(REAL(x)[k])) {
              REAL(sub)[j-nacount] = REAL(x)[k];
            } else { nacount++; continue; }
          } else {
            u.d = REAL(x)[k];
            if (u.ll != NA_INT64_LL) {
              REAL(sub)[j-nacount] = (double)u.ll;
            } else { nacount++; continue; }
          }
        }
        if (nacount == thisgrpsize) {
          REAL(ans)[i] = NA_REAL; // all NAs
          continue;
        }
        thisgrpsize -= nacount;
        SETLENGTH(sub, thisgrpsize);
        medianindex = (R_len_t)(ceil((double)(thisgrpsize)/2));
        REAL(ans)[i] = dquickselect(ptr, thisgrpsize, medianindex-1);
        if (thisgrpsize % 2 == 0) {
          // all elements to the left of thisgrpsize/2 is < the value at that index
          // we just need to get min of last half
          val = REAL(sub)[medianindex]; // 0-indexed
          for (imed=medianindex+1; imed<thisgrpsize; imed++) {
            val = REAL(sub)[imed] > val ? val : REAL(sub)[imed];
          }
          REAL(ans)[i] = (REAL(ans)[i] + val)/2.0;
        }
      }
    }
    SETLENGTH(sub, maxgrpn);
    break;
  case LGLSXP: case INTSXP:
    ans = PROTECT(allocVector(REALSXP, ngrp));
    sub = PROTECT(allocVector(INTSXP, maxgrpn)); // allocate once upfront
    ptr = INTEGER(sub);
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<ngrp; i++) {
        isna = FALSE;
        thisgrpsize = grpsize[i];
        SETLENGTH(sub, thisgrpsize);
        for (j=0; j<thisgrpsize; j++) {
          k = ff[i]+j-1;
          if (isunsorted) k = oo[k]-1;
          k = (irowslen == -1) ? k : irows[k]-1;
          if (INTEGER(x)[k] != NA_INTEGER) {
            INTEGER(sub)[j] = INTEGER(x)[k];
          } else {
            REAL(ans)[i] = NA_REAL;
            isna = TRUE; break;
          }
        }
        if (isna) continue;
        medianindex = (R_len_t)(ceil((double)(thisgrpsize)/2));
        REAL(ans)[i] = iquickselect(ptr, thisgrpsize, medianindex-1); // 0-indexed
        // all elements to the left of thisgrpsize/2 is < the value at that index
        // we just need to get min of last half
        if (thisgrpsize % 2 == 0) {
          val = INTEGER(sub)[medianindex]; // 0-indexed
          for (imed=medianindex+1; imed<thisgrpsize; imed++) {
            val = INTEGER(sub)[imed] > val ? val : INTEGER(sub)[imed];
          }
          REAL(ans)[i] = (REAL(ans)[i] + val)/2.0;
        }
      }
    } else {
      for (i=0; i<ngrp; i++) {
        nacount = 0;
        thisgrpsize = grpsize[i];
        for (j=0; j<thisgrpsize; j++) {
          k = ff[i]+j-1;
          if (isunsorted) k = oo[k]-1;
          k = (irowslen == -1) ? k : irows[k]-1;
          if (INTEGER(x)[k] != NA_INTEGER) {
            INTEGER(sub)[j-nacount] = INTEGER(x)[k];
          } else { nacount++; continue; }
        }
        if (nacount == thisgrpsize) {
          REAL(ans)[i] = NA_REAL; // all NAs
          continue;
        }
        thisgrpsize -= nacount;
        SETLENGTH(sub, thisgrpsize);
        medianindex = (R_len_t)(ceil((double)(thisgrpsize)/2));
        REAL(ans)[i] = iquickselect(ptr, thisgrpsize, medianindex-1);
        if (thisgrpsize % 2 == 0) {
          // all elements to the left of thisgrpsize/2 is < the value at that index
          // we just need to get min of last half
          val = INTEGER(sub)[medianindex]; // 0-indexed
          for (imed=medianindex+1; imed<thisgrpsize; imed++) {
            val = INTEGER(sub)[imed] > val ? val : INTEGER(sub)[imed];
          }
          REAL(ans)[i] = (REAL(ans)[i] + val)/2.0;
        }
      }
    }
    SETLENGTH(sub, maxgrpn);
    break;
  default:
    error("Type '%s' not supported by GForce median (gmedian). Either add the prefix stats::median(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  UNPROTECT(2);
  return(ans);
}

SEXP glast(SEXP x) {

  if (!isVectorAtomic(x)) error("GForce tail can only be applied to columns, not .SD or similar. To get tail of all items in a list such as .SD, either add the prefix utils::tail(.SD) or turn off GForce optimization using options(datatable.optimize=1).");

  R_len_t i,k;
  int n = (irowslen == -1) ? length(x) : irowslen;
  SEXP ans;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gtail", nrow, n);
  switch(TYPEOF(x)) {
  case LGLSXP:
    ans = PROTECT(allocVector(LGLSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]+grpsize[i]-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      LOGICAL(ans)[i] = LOGICAL(x)[k];
    }
    break;
  case INTSXP:
    ans = PROTECT(allocVector(INTSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]+grpsize[i]-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      INTEGER(ans)[i] = INTEGER(x)[k];
    }
    break;
  case REALSXP:
    ans = PROTECT(allocVector(REALSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]+grpsize[i]-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      REAL(ans)[i] = REAL(x)[k];
    }
    break;
  case STRSXP:
    ans = PROTECT(allocVector(STRSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]+grpsize[i]-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      SET_STRING_ELT(ans, i, STRING_ELT(x, k));
    }
    break;
  case VECSXP:
    ans = PROTECT(allocVector(VECSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]+grpsize[i]-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, k));
    }
    break;
  default:
    error("Type '%s' not supported by GForce tail (gtail). Either add the prefix utils::tail(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  return(ans);
}

SEXP gfirst(SEXP x) {

  if (!isVectorAtomic(x)) error("GForce head can only be applied to columns, not .SD or similar. To get head of all items in a list such as .SD, either add the prefix utils::head(.SD) or turn off GForce optimization using options(datatable.optimize=1).");

  R_len_t i,k;
  int n = (irowslen == -1) ? length(x) : irowslen;
  SEXP ans;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in ghead", nrow, n);
  switch(TYPEOF(x)) {
  case LGLSXP:
    ans = PROTECT(allocVector(LGLSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]-1;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      LOGICAL(ans)[i] = LOGICAL(x)[k];
    }
    break;
  case INTSXP:
    ans = PROTECT(allocVector(INTSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]-1;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      INTEGER(ans)[i] = INTEGER(x)[k];
    }
    break;
  case REALSXP:
    ans = PROTECT(allocVector(REALSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]-1;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      REAL(ans)[i] = REAL(x)[k];
    }
    break;
  case STRSXP:
    ans = PROTECT(allocVector(STRSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]-1;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      SET_STRING_ELT(ans, i, STRING_ELT(x, k));
    }
    break;
  case VECSXP:
    ans = PROTECT(allocVector(VECSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      k = ff[i]-1;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, k));
    }
    break;
  default:
    error("Type '%s' not supported by GForce head (ghead). Either add the prefix utils::head(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  return(ans);
}

SEXP gtail(SEXP x, SEXP valArg) {
  if (!isInteger(valArg) || LENGTH(valArg)!=1 || INTEGER(valArg)[0]!=1) error("Internal error, gtail is only implemented for n=1. This should have been caught before. please report to data.table issue tracker."); // # nocov
  return (glast(x));
}

SEXP ghead(SEXP x, SEXP valArg) {
  if (!isInteger(valArg) || LENGTH(valArg)!=1 || INTEGER(valArg)[0]!=1) error("Internal error, ghead is only implemented for n=1. This should have been caught before. please report to data.table issue tracker."); // # nocov
  return (gfirst(x));
}

SEXP gnthvalue(SEXP x, SEXP valArg) {

  if (!isInteger(valArg) || LENGTH(valArg)!=1 || INTEGER(valArg)[0]<=0) error("Internal error, `g[` (gnthvalue) is only implemented single value subsets with positive index, e.g., .SD[2]. This should have been caught before. please report to data.table issue tracker."); // # nocov
  R_len_t i,k, val=INTEGER(valArg)[0];
  int n = (irowslen == -1) ? length(x) : irowslen;
  SEXP ans;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in ghead", nrow, n);
  switch(TYPEOF(x)) {
  case LGLSXP:
    ans = PROTECT(allocVector(LGLSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      if (val > grpsize[i]) { LOGICAL(ans)[i] = NA_LOGICAL; continue; }
      k = ff[i]+val-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      LOGICAL(ans)[i] = LOGICAL(x)[k];
    }
    break;
  case INTSXP:
    ans = PROTECT(allocVector(INTSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      if (val > grpsize[i]) { INTEGER(ans)[i] = NA_INTEGER; continue; }
      k = ff[i]+val-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      INTEGER(ans)[i] = INTEGER(x)[k];
    }
    break;
  case REALSXP:
    ans = PROTECT(allocVector(REALSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      if (val > grpsize[i]) { REAL(ans)[i] = NA_REAL; continue; }
      k = ff[i]+val-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      REAL(ans)[i] = REAL(x)[k];
    }
    break;
  case STRSXP:
    ans = PROTECT(allocVector(STRSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      if (val > grpsize[i]) { SET_STRING_ELT(ans, i, NA_STRING); continue; }
      k = ff[i]+val-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      SET_STRING_ELT(ans, i, STRING_ELT(x, k));
    }
    break;
  case VECSXP:
    ans = PROTECT(allocVector(VECSXP, ngrp));
    for (i=0; i<ngrp; i++) {
      if (val > grpsize[i]) { SET_VECTOR_ELT(ans, i, R_NilValue); continue; }
      k = ff[i]+val-2;
      if (isunsorted) k = oo[k]-1;
      k = (irowslen == -1) ? k : irows[k]-1;
      SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, k));
    }
    break;
  default:
    error("Type '%s' not supported by GForce subset `[` (gnthvalue). Either add the prefix utils::head(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  return(ans);
}

// TODO: gwhich.min, gwhich.max
// implemented this similar to gmedian to balance well between speed and memory usage. There's one extra allocation on maximum groups and that's it.. and that helps speed things up extremely since we don't have to collect x's values for each group for each step (mean, residuals, mean again and then variance).
SEXP gvarsd1(SEXP x, SEXP narm, Rboolean isSD)
{
  if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
  if (!isVectorAtomic(x)) error("GForce var/sd can only be applied to columns, not .SD or similar. To find var/sd of all items in a list such as .SD, either add the prefix stats::var(.SD) (or stats::sd(.SD)) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,var),by=,.SDcols=]'");
  if (inherits(x, "factor")) error("var/sd is not meaningful for factors.");
  long double m, s, v;
  R_len_t i, j, ix, thisgrpsize = 0, n = (irowslen == -1) ? length(x) : irowslen;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gvar", nrow, n);
  SEXP sub, ans = PROTECT(allocVector(REALSXP, ngrp));
  Rboolean ans_na;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP:
    sub = PROTECT(allocVector(INTSXP, maxgrpn)); // allocate once upfront
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<ngrp; i++) {
        m=0.; s=0.; v=0.; ans_na = FALSE;
        if (grpsize[i] != 1) {
          thisgrpsize = grpsize[i];
          SETLENGTH(sub, thisgrpsize); // to gather this group's data
          for (j=0; j<thisgrpsize; j++) {
            ix = ff[i]+j-1;
            if (isunsorted) ix = oo[ix]-1;
            ix = (irowslen == -1) ? ix : irows[ix]-1;
            if (INTEGER(x)[ix] == NA_INTEGER) { ans_na = TRUE; break; }
            INTEGER(sub)[j] = INTEGER(x)[ix];
            m += INTEGER(sub)[j]; // sum
          }
          if (ans_na) { REAL(ans)[i] = NA_REAL; continue; }
          m = m/thisgrpsize; // mean, first pass
          for (j=0; j<thisgrpsize; j++) s += (INTEGER(sub)[j]-m); // residuals
          m += (s/thisgrpsize); // mean, second pass
          for (j=0; j<thisgrpsize; j++) { // variance
            v += (INTEGER(sub)[j]-(double)m) * (INTEGER(sub)[j]-(double)m);
          }
          REAL(ans)[i] = (double)v/(thisgrpsize-1);
          if (isSD) REAL(ans)[i] = SQRTL(REAL(ans)[i]);
        } else REAL(ans)[i] = NA_REAL;
      }
    } else {
      for (i=0; i<ngrp; i++) {
        m=0.; s=0.; v=0.; thisgrpsize = 0;
        if (grpsize[i] != 1) {
          SETLENGTH(sub, grpsize[i]); // to gather this group's data
          for (j=0; j<grpsize[i]; j++) {
            ix = ff[i]+j-1;
            if (isunsorted) ix = oo[ix]-1;
            ix = (irowslen == -1) ? ix : irows[ix]-1;
            if (INTEGER(x)[ix] == NA_INTEGER) continue;
            INTEGER(sub)[thisgrpsize] = INTEGER(x)[ix];
            m += INTEGER(sub)[thisgrpsize]; // sum
            thisgrpsize++;
          }
          if (thisgrpsize <= 1) { REAL(ans)[i] = NA_REAL; continue; }
          m = m/thisgrpsize; // mean, first pass
          for (j=0; j<thisgrpsize; j++) s += (INTEGER(sub)[j]-m); // residuals
          m += (s/thisgrpsize); // mean, second pass
          for (j=0; j<thisgrpsize; j++) { // variance
            v += (INTEGER(sub)[j]-(double)m) * (INTEGER(sub)[j]-(double)m);
          }
          REAL(ans)[i] = (double)v/(thisgrpsize-1);
          if (isSD) REAL(ans)[i] = SQRTL(REAL(ans)[i]);
        } else REAL(ans)[i] = NA_REAL;
      }
    }
    SETLENGTH(sub, maxgrpn);
    break;
  case REALSXP:
    sub = PROTECT(allocVector(REALSXP, maxgrpn)); // allocate once upfront
    if (!LOGICAL(narm)[0]) {
      for (i=0; i<ngrp; i++) {
        m=0.; s=0.; v=0.; ans_na = FALSE;
        if (grpsize[i] != 1) {
          thisgrpsize = grpsize[i];
          SETLENGTH(sub, thisgrpsize); // to gather this group's data
          for (j=0; j<thisgrpsize; j++) {
            ix = ff[i]+j-1;
            if (isunsorted) ix = oo[ix]-1;
            ix = (irowslen == -1) ? ix : irows[ix]-1;
            if (ISNAN(REAL(x)[ix])) { ans_na = TRUE; break; }
            REAL(sub)[j] = REAL(x)[ix];
            m += REAL(sub)[j]; // sum
          }
          if (ans_na) { REAL(ans)[i] = NA_REAL; continue; }
          m = m/thisgrpsize; // mean, first pass
          for (j=0; j<thisgrpsize; j++) s += (REAL(sub)[j]-m); // residuals
          m += (s/thisgrpsize); // mean, second pass
          for (j=0; j<thisgrpsize; j++) { // variance
            v += (REAL(sub)[j]-(double)m) * (REAL(sub)[j]-(double)m);
          }
          REAL(ans)[i] = (double)v/(thisgrpsize-1);
          if (isSD) REAL(ans)[i] = SQRTL(REAL(ans)[i]);
        } else REAL(ans)[i] = NA_REAL;
      }
    } else {
      for (i=0; i<ngrp; i++) {
        m=0.; s=0.; v=0.; thisgrpsize = 0;
        if (grpsize[i] != 1) {
          SETLENGTH(sub, grpsize[i]); // to gather this group's data
          for (j=0; j<grpsize[i]; j++) {
            ix = ff[i]+j-1;
            if (isunsorted) ix = oo[ix]-1;
            ix = (irowslen == -1) ? ix : irows[ix]-1;
            if (ISNAN(REAL(x)[ix])) continue;
            REAL(sub)[thisgrpsize] = REAL(x)[ix];
            m += REAL(sub)[thisgrpsize]; // sum
            thisgrpsize++;
          }
          if (thisgrpsize <= 1) { REAL(ans)[i] = NA_REAL; continue; }
          m = m/thisgrpsize; // mean, first pass
          for (j=0; j<thisgrpsize; j++) s += (REAL(sub)[j]-m); // residuals
          m += (s/thisgrpsize); // mean, second pass
          for (j=0; j<thisgrpsize; j++) { // variance
            v += (REAL(sub)[j]-(double)m) * (REAL(sub)[j]-(double)m);
          }
          REAL(ans)[i] = (double)v/(thisgrpsize-1);
          if (isSD) REAL(ans)[i] = SQRTL(REAL(ans)[i]);
        } else REAL(ans)[i] = NA_REAL;
      }
    }
    SETLENGTH(sub, maxgrpn);
    break;
  default:
      if (isSD) {
        error("Type '%s' not supported by GForce var (gvar). Either add the prefix stats::var(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
      } else {
        error("Type '%s' not supported by GForce sd (gsd). Either add the prefix stats::sd(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
      }
  }
  UNPROTECT(2);
  return (ans);
}

SEXP gvar(SEXP x, SEXP narm) {
  return (gvarsd1(x, narm, FALSE));
}

SEXP gsd(SEXP x, SEXP narm) {
  return (gvarsd1(x, narm, TRUE));
}

SEXP gprod(SEXP x, SEXP narm)
{
  if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
  if (!isVectorAtomic(x)) error("GForce prod can only be applied to columns, not .SD or similar. To multiply all items in a list such as .SD, either add the prefix base::prod(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,prod),by=,.SDcols=]'");
  if (inherits(x, "factor")) error("prod is not meaningful for factors.");
  int i, ix, thisgrp;
  int n = (irowslen == -1) ? length(x) : irowslen;
  //clock_t start = clock();
  SEXP ans;
  if (nrow != n) error("nrow [%d] != length(x) [%d] in gprod", nrow, n);
  long double *s = malloc(ngrp * sizeof(long double));
  if (!s) error("Unable to allocate %d * %d bytes for gprod", ngrp, sizeof(long double));
  for (i=0; i<ngrp; i++) s[i] = 1.0;
  ans = PROTECT(allocVector(REALSXP, ngrp));
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP:
    for (i=0; i<n; i++) {
      thisgrp = grp[i];
      ix = (irowslen == -1) ? i : irows[i]-1;
      if(INTEGER(x)[ix] == NA_INTEGER) {
        if (!LOGICAL(narm)[0]) s[thisgrp] = NA_REAL;  // Let NA_REAL propogate from here. R_NaReal is IEEE.
        continue;
      }
      s[thisgrp] *= INTEGER(x)[ix];  // no under/overflow here, s is long double (like base)
    }
    for (i=0; i<ngrp; i++) {
      if (s[i] > DBL_MAX) REAL(ans)[i] = R_PosInf;
      else if (s[i] < -DBL_MAX) REAL(ans)[i] = R_NegInf;
      else REAL(ans)[i] = (double)s[i];
    }
    break;
  case REALSXP:
    for (i=0; i<n; i++) {
      thisgrp = grp[i];
      ix = (irowslen == -1) ? i : irows[i]-1;
      if(ISNAN(REAL(x)[ix]) && LOGICAL(narm)[0]) continue;  // else let NA_REAL propogate from here
      s[thisgrp] *= REAL(x)[ix];  // done in long double, like base
    }
    for (i=0; i<ngrp; i++) {
      if (s[i] > DBL_MAX) REAL(ans)[i] = R_PosInf;
      else if (s[i] < -DBL_MAX) REAL(ans)[i] = R_NegInf;
      else REAL(ans)[i] = (double)s[i];
    }
    break;
  default:
    free(s);
    error("Type '%s' not supported by GForce prod (gprod). Either add the prefix base::prod(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
  }
  free(s);
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  // Rprintf("this gprod took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return(ans);
}
