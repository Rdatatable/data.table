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
static int bitshift, mask;
static char *gx=NULL;

static size_t nBatch, batchSize, lastBatchSize;
static int *counts, *tmpcounts;

// for gmedian
static int maxgrpn = 0;
static int *oo = NULL;
static int *ff = NULL;
static int isunsorted = 0;

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

/*
  Functions with GForce optimization are internally parallelized to speed up
    grouped summaries over a large data.table. OpenMP is used here to
    parallelize operations involved in calculating common group-wise statistics.
*/
SEXP gforce(SEXP env, SEXP jsub, SEXP o, SEXP f, SEXP l, SEXP irowsArg) {
  double started = wallclock();
  const bool verbose = GetVerbose();
  if (TYPEOF(env) != ENVSXP) error(_("env is not an environment"));
  // The type of jsub is pretty flexible in R, so leave checking to eval() below.
  if (!isInteger(o)) error(_("%s is not an integer vector"), "o");
  if (!isInteger(f)) error(_("%s is not an integer vector"), "f");
  if (!isInteger(l)) error(_("%s is not an integer vector"), "l");
  if (isNull(irowsArg)) {
    irows = NULL;
    irowslen = -1;
  }
  else if (isInteger(irowsArg)) {
    irows = INTEGER(irowsArg);
    irowslen = LENGTH(irowsArg);
  }
  else error(_("irowsArg is neither an integer vector nor NULL"));  // # nocov
  ngrp = LENGTH(l);
  if (LENGTH(f) != ngrp)
    error("length(f)=%d != length(l)=%d", LENGTH(f), ngrp); // # notranslate
  nrow=0;
  grpsize = INTEGER(l);
  maxgrpn = 0;
  for (int i=0; i<ngrp; i++) {
    nrow+=grpsize[i];
    if (grpsize[i]>maxgrpn) maxgrpn = grpsize[i];  // old comment to be checked: 'needed for #2046 and #2111 when maxgrpn attribute is not attached to empty o'
  }
  if (LENGTH(o) && LENGTH(o)!=nrow) error(_("o has length %d but sum(l)=%d"), LENGTH(o), nrow);
  {
    SEXP tt = getAttrib(o, install("maxgrpn"));
    if (length(tt)==1 && INTEGER(tt)[0]!=maxgrpn) internal_error(__func__, "o's maxgrpn attribute mismatches recalculated maxgrpn"); // # nocov
  }

  int nb = nbit(ngrp-1);
  bitshift = nb/2;    // /2 so that high and low can be uint16_t, and no limit (even for nb=4) to stress-test.
  // bitshift=MAX(nb-8,0); if (bitshift>16) bitshift=nb/2;     // TODO: when we have stress-test off mode, do this
  mask = (1<<bitshift)-1;
  highSize = ((ngrp-1)>>bitshift) + 1;

  grp = (int *)R_alloc(nrow, sizeof(*grp));   // TODO: use malloc and made this local as not needed globally when all functions here use gather
                                             // maybe better to malloc to avoid R's heap. This grp isn't global, so it doesn't need to be R_alloc
  const int *restrict fp = INTEGER(f);

  nBatch = MIN((nrow+1)/2, getDTthreads(nrow, true)*2);  // *2 to reduce last-thread-home. TODO: experiment. The higher this is though, the bigger is counts[]
  batchSize = MAX(1, (nrow-1)/nBatch);
  lastBatchSize = nrow - (nBatch-1)*batchSize;
  // We deliberate use, for example, 40 batches of just 14 rows, to stress-test tests. This strategy proved to be a good one as #3204 immediately came to light.
  // TODO: enable stress-test mode in tests only (#3205) which can be turned off by default in release to decrease overhead on small data
  //       if that is established to be biting (it may be fine).
  if (nBatch<1 || batchSize<1 || lastBatchSize<1) {
    internal_error(__func__, "nrow=%d  ngrp=%d  nbit=%d  bitshift=%d  highSize=%zu  nBatch=%zu  batchSize=%zu  lastBatchSize=%zu\n",  // # nocov
                   nrow, ngrp, nb, bitshift, highSize, nBatch, batchSize, lastBatchSize);                                   // # nocov
  }
  // initial population of g:
  #pragma omp parallel for num_threads(getDTthreads(ngrp, false))
  for (int g=0; g<ngrp; g++) {
    int *elem = grp + fp[g]-1;
    for (int j=0; j<grpsize[g]; j++)  elem[j] = g;
  }
  if (verbose) { Rprintf(_("gforce initial population of grp took %.3f\n"), wallclock()-started); started=wallclock(); }
  isunsorted = 0;
  if (LENGTH(o)) {
    isunsorted = 1; // for gmedian

    // What follows is more cache-efficient version of this scattered assign :
    // for (int g=0; g<ngrp; g++) {
    //  const int *elem = op + fp[g]-1;
    //  for (int j=0; j<grpsize[g]; j++)  grp[ elem[j]-1 ] = g;
    //}

    const int *restrict op = INTEGER(o);  // o is a permutation of 1:nrow
    int nb = nbit(nrow-1);
    int bitshift = MAX(nb-8, 0);  // TODO: experiment nb/2.  Here it doesn't have to be /2 currently.
    int highSize = ((nrow-1)>>bitshift) + 1;
    //Rprintf(_("When assigning grp[o] = g, highSize=%d  nb=%d  bitshift=%d  nBatch=%d\n"), highSize, nb, bitshift, nBatch);
    int *counts = calloc(nBatch*highSize, sizeof(*counts));  // TODO: cache-line align and make highSize a multiple of 64
    int *TMP   = malloc(sizeof(*TMP) * nrow*2l); // must multiple the long int otherwise overflow may happen, #4295
    if (!counts || !TMP ) {
      free(counts); free(TMP); // # nocov
      error(_("Failed to allocate counts or TMP when assigning g in gforce")); // # nocov
    }
    #pragma omp parallel for num_threads(getDTthreads(nBatch, false))   // schedule(dynamic,1)
    for (int b=0; b<nBatch; b++) {
      const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
      const int *my_o = op + b*batchSize;
      int *restrict my_counts = counts + b*highSize;
      for (int i=0; i<howMany; i++) {
        const int w = (my_o[i]-1) >> bitshift;
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
        const int w = (my_o[i]-1) >> bitshift;   // could use my_high but may as well use my_pg since we need my_pg anyway for the lower bits next too
        int *p = my_tmp + 2*my_counts[w]++;
        *p++ = my_o[i]-1;
        *p   = my_g[i];
      }
    }
    //Rprintf(_("gforce assign TMP (o,g) pairs took %.3f\n"), wallclock()-started); started=wallclock();
    #pragma omp parallel for num_threads(getDTthreads(highSize, false))
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
    //Rprintf(_("gforce assign TMP [ (o,g) pairs ] back to grp took %.3f\n"), wallclock()-started); started=wallclock();
  }

  high = (uint16_t *)R_alloc(nrow, sizeof(*high));  // maybe better to malloc to avoid R's heap, but safer to R_alloc since it's done via eval()
  low  = (uint16_t *)R_alloc(nrow, sizeof(*low));
  // global ghigh and glow because the g* functions (inside jsub) share this common memory

  gx = (char *)R_alloc(nrow, sizeof(Rcomplex));  // enough for a copy of one column (or length(irows) if supplied)
  // TODO: reduce to the largest type present; won't be faster (untouched RAM won't be fetched) but it will increase the largest size that works.

  counts = (int *)S_alloc(nBatch*highSize, sizeof(*counts));  // (S_ zeros) TODO: cache-line align and make highSize a multiple of 64
  tmpcounts = (int *)R_alloc(getDTthreads(nBatch, false)*highSize, sizeof(*tmpcounts));

  const int *restrict gp = grp;
  #pragma omp parallel for num_threads(getDTthreads(nBatch, false))   // schedule(dynamic,1)
  for (int b=0; b<nBatch; b++) {
    int *restrict my_counts = counts + b*highSize;
    uint16_t *restrict my_high = high + b*batchSize;
    const int *my_pg = gp + b*batchSize;
    const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
    for (int i=0; i<howMany; i++) {
      const int w = my_pg[i] >> bitshift;
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
    memcpy(my_tmpcounts, my_counts, highSize*sizeof(*my_tmpcounts));
    for (int i=0; i<howMany; i++) {
      const int w = my_pg[i] >> bitshift;   // could use my_high but may as well use my_pg since we need my_pg anyway for the lower bits next too
      my_low[my_tmpcounts[w]++] = (uint16_t)(my_pg[i] & mask);
    }
    // counts is now cumulated within batch (with ending values) and we leave it that way
    // memcpy(counts + b*256, myCounts, 256*sizeof(int));  // save cumulate for later, first bucket contains position of next. For ease later in the very last batch.
  }
  if (verbose) { Rprintf(_("gforce assign high and low took %.3f\n"), wallclock()-started); started=wallclock(); }

  oo = INTEGER(o);
  ff = INTEGER(f);

  SEXP ans = PROTECT( eval(jsub, env) );
  if (verbose) { Rprintf(_("gforce eval took %.3f\n"), wallclock()-started); started=wallclock(); }
  // if this eval() fails with R error, R will release grp for us. Which is why we use R_alloc above.
  if (isVectorAtomic(ans)) {
    SEXP tt = PROTECT(allocVector(VECSXP, 1));
    SET_VECTOR_ELT(tt, 0, ans);
    UNPROTECT(2);
    return tt;
  }
  UNPROTECT(1);
  return ans;
}

void *gather(SEXP x, bool *anyNA)
{
  double started=wallclock();
  const bool verbose = GetVerbose();
  switch (TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *restrict thisx = INTEGER(x);
    #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
    for (int b=0; b<nBatch; b++) {
      int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
      memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(*my_tmpcounts));   // original cumulated   // already cumulated for this batch
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
          int elem = my_x[i]==NA_INTEGER ? NA_INTEGER : thisx[ my_x[i]-1 ];
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
      #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
      for (int b=0; b<nBatch; b++) {
        int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
        memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(*my_tmpcounts));
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
            double elem = my_x[i]==NA_INTEGER ? NA_REAL : thisx[ my_x[i]-1 ];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (ISNAN(elem)) my_anyNA = true;
          }
        }
        if (my_anyNA) *anyNA = true;
      }
    } else {
      const int64_t *restrict thisx = (int64_t *)REAL(x);
      #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
      for (int b=0; b<nBatch; b++) {
        int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
        memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(*my_tmpcounts));
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
            int64_t elem = my_x[i]==NA_INTEGER ? NA_INTEGER64 : thisx[ my_x[i]-1 ];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (elem==INT64_MIN) my_anyNA = true;
          }
        }
        if (my_anyNA) *anyNA = true;
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict thisx = COMPLEX(x);
    #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
    for (int b=0; b<nBatch; b++) {
      int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
      memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(*my_tmpcounts));
      Rcomplex *restrict my_gx = (Rcomplex *)gx + b*batchSize;
      const uint16_t *my_high = high + b*batchSize;
      const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
      bool my_anyNA = false;
      if (irowslen==-1) {
        const Rcomplex *my_x = thisx + b*batchSize;
        for (int i=0; i<howMany; i++) {
          const Rcomplex elem = my_x[i];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          // typically just checking one component would be enough,
          //   but ?complex suggests there may be some edge cases; better to be safe
          if (ISNAN(elem.r) && ISNAN(elem.i)) my_anyNA = true;
        }
      } else {
        const int *my_x = irows + b*batchSize;
        for (int i=0; i<howMany; i++) {
          Rcomplex elem = my_x[i]==NA_INTEGER ? NA_CPLX : thisx[ my_x[i]-1 ];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          if (ISNAN(elem.r) && ISNAN(elem.i)) my_anyNA = true;
        }
      }
      if (my_anyNA) *anyNA = true;  // naked write ok since just bool and always writing true; and no performance issue as maximum nBatch writes
    }
  } break;
  default : // # nocov
    error(_("gather implemented for INTSXP, REALSXP, and CPLXSXP but not '%s'"), type2char(TYPEOF(x)));   // # nocov
  }
  if (verbose) { Rprintf(_("gather took %.3fs\n"), wallclock()-started); }
  return gx;
}

SEXP gsum(SEXP x, SEXP narmArg)
{
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  const bool narm = LOGICAL(narmArg)[0];
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "sum");
  const int n = (irowslen == -1) ? length(x) : irowslen;
  double started = wallclock();
  const bool verbose=GetVerbose();
  if (verbose) Rprintf(_("This gsum (narm=%s) took ... "), narm?"TRUE":"FALSE");
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gsum");
  bool anyNA=false;
  SEXP ans;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(INTSXP, ngrp));
    int *restrict ansp = INTEGER(ans);
    memset(ansp, 0, ngrp*sizeof(*ansp));
    bool overflow=false;
    //double started = wallclock();
    if (!anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false)) //schedule(dynamic,1)
      for (int h=0; h<highSize; h++) {   // very important that high is first loop here
        int *restrict _ans = ansp + (h<<bitshift);
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
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        int *restrict _ans = ansp + (h<<bitshift);
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
    //Rprintf(_("gsum int took %.3f\n"), wallclock()-started);
    if (overflow) {
      UNPROTECT(1); // discard the result with overflow
      warning(_("The sum of an integer column for a group was more than type 'integer' can hold so the result has been coerced to 'numeric' automatically for convenience."));
      ans = PROTECT(allocVector(REALSXP, ngrp));
      double *restrict ansp = REAL(ans);
      memset(ansp, 0, ngrp*sizeof(double));
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        double *restrict _ans = ansp + (h<<bitshift);
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
    if (!INHERITS(x, char_integer64)) {
      const double *restrict gx = gather(x, &anyNA);
      ans = PROTECT(allocVector(REALSXP, ngrp));
      double *restrict ansp = REAL(ans);
      memset(ansp, 0, ngrp*sizeof(double));
      if (!narm || !anyNA) {
        #pragma omp parallel for num_threads(getDTthreads(highSize, false))
        for (int h=0; h<highSize; h++) {
          double *restrict _ans = ansp + (h<<bitshift);
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
        #pragma omp parallel for num_threads(getDTthreads(highSize, false))
        for (int h=0; h<highSize; h++) {
          double *restrict _ans = ansp + (h<<bitshift);
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
    } else { // int64
      const int64_t *restrict gx = gather(x, &anyNA);
      ans = PROTECT(allocVector(REALSXP, ngrp));
      int64_t *restrict ansp = (int64_t *)REAL(ans);
      memset(ansp, 0, ngrp*sizeof(*ansp));
      if (!anyNA) {
        #pragma omp parallel for num_threads(getDTthreads(highSize, false))
        for (int h=0; h<highSize; h++) {
          int64_t *restrict _ans = ansp + (h<<bitshift);
          for (int b=0; b<nBatch; b++) {
            const int pos = counts[ b*highSize + h ];
            const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
            const int64_t *my_gx = gx + b*batchSize + pos;
            const uint16_t *my_low = low + b*batchSize + pos;
            for (int i=0; i<howMany; i++) {
              _ans[my_low[i]] += my_gx[i]; // does not propagate INT64 for !narm
            }
          }
        }
      } else { // narm==true/false and anyNA==true
        if (!narm) {
          #pragma omp parallel for num_threads(getDTthreads(highSize, false))
          for (int h=0; h<highSize; h++) {
            int64_t *restrict _ans = ansp + (h<<bitshift);
            for (int b=0; b<nBatch; b++) {
              const int pos = counts[ b*highSize + h ];
              const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
              const int64_t *my_gx = gx + b*batchSize + pos;
              const uint16_t *my_low = low + b*batchSize + pos;
              for (int i=0; i<howMany; i++) {
                const int64_t elem = my_gx[i];
                if (elem!=INT64_MIN) {
                  _ans[my_low[i]] += elem;
                } else {
                  _ans[my_low[i]] = INT64_MIN;
                  break;
                }
              }
            }
          }
        } else {
          #pragma omp parallel for num_threads(getDTthreads(highSize, false))
          for (int h=0; h<highSize; h++) {
            int64_t *restrict _ans = ansp + (h<<bitshift);
            for (int b=0; b<nBatch; b++) {
              const int pos = counts[ b*highSize + h ];
              const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
              const int64_t *my_gx = gx + b*batchSize + pos;
              const uint16_t *my_low = low + b*batchSize + pos;
              for (int i=0; i<howMany; i++) {
                const int64_t elem = my_gx[i];
                if (elem!=INT64_MIN) _ans[my_low[i]] += elem;
              }
            }
          }
        }
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(CPLXSXP, ngrp));
    Rcomplex *restrict ansp = COMPLEX(ans);
    memset(ansp, 0, ngrp*sizeof(Rcomplex));
    if (!narm || !anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<bitshift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            _ans[my_low[i]].r += my_gx[i].r;  // let NA propagate when !narm
            _ans[my_low[i]].i += my_gx[i].i;
          }
        }
      }
    } else {
      // narm==true and anyNA==true
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<bitshift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const Rcomplex elem = my_gx[i];
            if (!ISNAN(elem.r)) _ans[my_low[i]].r += elem.r;
            if (!ISNAN(elem.i)) _ans[my_low[i]].i += elem.i;
          }
        }
      }
    }
  } break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)), "sum (gsum)", "base::sum(.)");
  }
  copyMostAttrib(x, ans);
  if (verbose) { Rprintf(_("%.3fs\n"), wallclock()-started); }
  UNPROTECT(1);
  return(ans);
}

SEXP gmean(SEXP x, SEXP narmArg)
{
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "mean");
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  const bool narm = LOGICAL(narmArg)[0];
  const int n = (irowslen == -1) ? length(x) : irowslen;
  double started = wallclock();
  const bool verbose=GetVerbose();
  if (verbose) Rprintf(_("This gmean took (narm=%s) ... "), narm?"TRUE":"FALSE"); // narm=TRUE only at this point
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gmean");
  bool anyNA=false;
  SEXP ans=R_NilValue;
  int protecti=0;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP:
    x = PROTECT(coerceVector(x, REALSXP)); protecti++;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) {
      SEXP as = PROTECT(ScalarReal(1));
      x = PROTECT(coerceAs(x, as, /*copyArg=*/ScalarLogical(TRUE))); protecti++;
      UNPROTECT(2); PROTECT(x); // PROTECT() is stack-based, UNPROTECT() back to 'as' then PROTECT() 'x' again
    }
    const double *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(REALSXP, ngrp)); protecti++;
    double *restrict ansp = REAL(ans);
    memset(ansp, 0, ngrp*sizeof(double));
    if (!narm || !anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        double *restrict _ans = ansp + (h<<bitshift);
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
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) ansp[i] /= grpsize[i];
    } else {
      // narm==true and anyNA==true
      int *restrict nna_counts = calloc(ngrp, sizeof(*nna_counts));
      if (!nna_counts)
        error(_("Unable to allocate %d * %zu bytes for non-NA counts in gmean na.rm=TRUE"), ngrp, sizeof(*nna_counts)); // # nocov
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
          double *restrict _ans = ansp + (h<<bitshift);
          int *restrict _nna = nna_counts + (h<<bitshift);
          for (int b=0; b<nBatch; b++) {
            const int pos = counts[ b*highSize + h ];
            const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
            const double *my_gx = gx + b*batchSize + pos;
            const uint16_t *my_low = low + b*batchSize + pos;
            for (int i=0; i<howMany; i++) {
              const double elem = my_gx[i];
              if (!ISNAN(elem)) {
                _ans[my_low[i]] += elem;
                _nna[my_low[i]]++;
              }
            }
          }
        }
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) ansp[i] /= nna_counts[i];
      free(nna_counts);
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(CPLXSXP, ngrp)); protecti++;
    Rcomplex *restrict ansp = COMPLEX(ans);
    memset(ansp, 0, ngrp*sizeof(Rcomplex));
    if (!narm || !anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<bitshift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            _ans[my_low[i]].r += my_gx[i].r;  // let NA propagate when !narm
            _ans[my_low[i]].i += my_gx[i].i;
          }
        }
      }
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) {
        ansp[i].i /= grpsize[i];
        ansp[i].r /= grpsize[i];
      }
    } else {
      // narm==true and anyNA==true
      int *restrict nna_counts_r = calloc(ngrp, sizeof(*nna_counts_r));
      int *restrict nna_counts_i = calloc(ngrp, sizeof(*nna_counts_i));
      if (!nna_counts_r || !nna_counts_i) {
        // # nocov start
        free(nna_counts_r); free(nna_counts_i);
        error(_("Unable to allocate %d * %zu bytes for non-NA counts in gmean na.rm=TRUE"), ngrp, sizeof(int));
        // # nocov end
      }
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<bitshift);
        int *restrict _nna_r = nna_counts_r + (h<<bitshift);
        int *restrict _nna_i = nna_counts_i + (h<<bitshift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const Rcomplex elem = my_gx[i];
            if (!ISNAN(elem.r)) {
              _ans[my_low[i]].r += elem.r;
              _nna_r[my_low[i]]++;
            }
            if (!ISNAN(elem.i)) {
              _ans[my_low[i]].i += elem.i;
              _nna_i[my_low[i]]++;
            }
          }
        }
      }
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) {
        ansp[i].r /= nna_counts_r[i];
        ansp[i].i /= nna_counts_i[i];
      }
      free(nna_counts_r);
      free(nna_counts_i);
    }
  } break;
  default:
    error(_("Type '%s' not supported by GForce mean (gmean). Either add the prefix base::mean(.) or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  if (verbose) { Rprintf(_("%.3fs\n"), wallclock()-started); }
  UNPROTECT(protecti);
  return(ans);
}

static SEXP gminmax(SEXP x, SEXP narm, const bool min)
{
  if (!IS_TRUE_OR_FALSE(narm))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  if (!isVectorAtomic(x)) error(_("GForce min/max can only be applied to columns, not .SD or similar. To find min/max of all items in a list such as .SD, either add the prefix base::min(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,min),by=,.SDcols=]'"));
  if (inherits(x, "factor") && !inherits(x, "ordered"))
    error(_("%s is not meaningful for factors."), min?"min":"max");
  const bool nosubset = irowslen==-1;
  const int n = nosubset ? length(x) : irowslen;
  //clock_t start = clock();
  SEXP ans;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gminmax");
  // GForce guarantees each group has at least one value; i.e. we don't need to consider length-0 per group here
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    ans = PROTECT(allocVector(INTSXP, ngrp));
    int *ansd = INTEGER(ans);
    const int *xd = INTEGER(x);
    if (!LOGICAL(narm)[0]) {
      const int init = min ? INT_MAX : INT_MIN+1;  // NA_INTEGER==INT_MIN checked in init.c
      for (int i=0; i<ngrp; ++i) ansd[i] = init;
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_INTEGER) continue;  // once an NA has been observed in this group, it doesn't matter what the remaining values in this group are
        const int elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER : xd[irows[i]-1]);
        if (elem==NA_INTEGER || (elem<ansd[thisgrp])==min)
          ansd[thisgrp] = elem;  // always true on the first value in the group (other than if the first value is INT_MAX or INT_MIN-1 which is fine too)
      }
    } else {
      for (int i=0; i<ngrp; ++i) ansd[i] = NA_INTEGER;  // in the all-NA case we now return NA for type consistency
      for (int i=0; i<n; ++i) {
        const int elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER : xd[irows[i]-1]);
        if (elem==NA_INTEGER) continue;
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_INTEGER || (elem<ansd[thisgrp])==min)
          ansd[thisgrp] = elem;
      }
    }}
    break;
  case STRSXP: {
    ans = PROTECT(allocVector(STRSXP, ngrp));
    const SEXP *ansd = STRING_PTR_RO(ans);
    const SEXP *xd = STRING_PTR_RO(x);
    if (!LOGICAL(narm)[0]) {
      const SEXP init = min ? char_maxString : R_BlankString;  // char_maxString == "\xFF\xFF..." in init.c
      for (int i=0; i<ngrp; ++i) SET_STRING_ELT(ans, i, init);
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_STRING) continue;
        const SEXP elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_STRING : xd[irows[i]-1]);
        if (elem==NA_STRING || (strcmp(CHAR(elem), CHAR(ansd[thisgrp]))<0)==min)
          SET_STRING_ELT(ans, thisgrp, elem);
      }
    } else {
      for (int i=0; i<ngrp; ++i) SET_STRING_ELT(ans, i, NA_STRING); // all missing returns NA consistent with base
      for (int i=0; i<n; ++i) {
        const SEXP elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_STRING : xd[irows[i]-1]);
        if (elem==NA_STRING) continue;
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_STRING || (strcmp(CHAR(elem), CHAR(ansd[thisgrp]))<0)==min)
          SET_STRING_ELT(ans, thisgrp, elem);
      }
    }}
    break;
  case REALSXP: {
    ans = PROTECT(allocVector(REALSXP, ngrp));
    if (INHERITS(x, char_integer64)) {
      int64_t *ansd = (int64_t *)REAL(ans);
      const int64_t *xd = (const int64_t *)REAL(x);
      if (!LOGICAL(narm)[0]) {
        const int64_t init = min ? INT64_MAX : INT64_MIN+1;
        for (int i=0; i<ngrp; ++i) ansd[i] = init;
        for (int i=0; i<n; ++i) {
          const int thisgrp = grp[i];
          if (ansd[thisgrp]==NA_INTEGER64) continue;
          const int64_t elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER64 : xd[irows[i]-1]);
          if (elem==NA_INTEGER64 || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      } else {
        for (int i=0; i<ngrp; ++i) ansd[i] = NA_INTEGER64;
        for (int i=0; i<n; ++i) {
          const int64_t elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER64 : xd[irows[i]-1]);
          if (elem==NA_INTEGER64) continue;
          const int thisgrp = grp[i];
          if (ansd[thisgrp]==NA_INTEGER64 || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      }
    } else {
      double *ansd = REAL(ans);
      const double *xd = REAL(x);
      if (!LOGICAL(narm)[0]) {
        const double init = min ? R_PosInf : R_NegInf;
        for (int i=0; i<ngrp; ++i) ansd[i] = init;
        for (int i=0; i<n; ++i) {
          const int thisgrp = grp[i];
          if (ISNAN(ansd[thisgrp])) continue;
          const double elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_REAL : xd[irows[i]-1]);
          if (ISNAN(elem) || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      } else {
        for (int i=0; i<ngrp; ++i) ansd[i] = NA_REAL;
        for (int i=0; i<n; ++i) {
          const double elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_REAL : xd[irows[i]-1]);
          if (ISNAN(elem)) continue;
          const int thisgrp = grp[i];
          if (ISNAN(ansd[thisgrp]) || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      }
    }
  }
    break;
  case CPLXSXP:
    error(_("Type 'complex' has no well-defined min/max"));
    break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"),
          type2char(TYPEOF(x)), min?"min (gmin)":"max (gmax)", min?"base::min(.)":"base::max(.)");
  }
  copyMostAttrib(x, ans); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
  UNPROTECT(1);  // ans
  // Rprintf(_("this gminmax took %8.3f\n"), 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return(ans);
}

SEXP gmin(SEXP x, SEXP narm)
{
  return gminmax(x, narm, true);
}

SEXP gmax(SEXP x, SEXP narm)
{
  return gminmax(x, narm, false);
}

// gmedian, always returns numeric type (to avoid as.numeric() wrap..)
SEXP gmedian(SEXP x, SEXP narmArg) {
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  if (!isVectorAtomic(x)) error(_("GForce median can only be applied to columns, not .SD or similar. To find median of all items in a list such as .SD, either add the prefix stats::median(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,median),by=,.SDcols=]'"));
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "median");
  const bool isInt64 = INHERITS(x, char_integer64), narm = LOGICAL(narmArg)[0];
  const int n = (irowslen == -1) ? length(x) : irowslen;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gmedian");
  SEXP ans = PROTECT(allocVector(REALSXP, ngrp));
  double *ansd = REAL(ans);
  const bool nosubset = irowslen==-1;
  switch(TYPEOF(x)) {
  case REALSXP: {
    double *subd = REAL(PROTECT(allocVector(REALSXP, maxgrpn))); // allocate once upfront and reuse
    int64_t *xi64 = (int64_t *)REAL(x);
    double  *xd = REAL(x);
    for (int i=0; i<ngrp; ++i) {
      int thisgrpsize = grpsize[i], nacount=0;
      for (int j=0; j<thisgrpsize; ++j) {
        int k = ff[i]+j-1;
        if (isunsorted) k = oo[k]-1;
        k = nosubset ? k : (irows[k]==NA_INTEGER ? NA_INTEGER : irows[k]-1);
        if (k==NA_INTEGER || (isInt64 ? xi64[k]==NA_INTEGER64 : ISNAN(xd[k]))) nacount++;
        else subd[j-nacount] = xd[k];
      }
      thisgrpsize -= nacount;  // all-NA is returned as NA_REAL via n==0 case inside *quickselect
      ansd[i] = (nacount && !narm) ? NA_REAL : (isInt64 ? i64quickselect((void *)subd, thisgrpsize) : dquickselect(subd, thisgrpsize));
    }}
    break;
  case LGLSXP: case INTSXP: {
    int *subi = INTEGER(PROTECT(allocVector(INTSXP, maxgrpn)));
    int *xi = INTEGER(x);
    for (int i=0; i<ngrp; i++) {
      const int thisgrpsize = grpsize[i];
      int nacount=0;
      for (int j=0; j<thisgrpsize; ++j) {
        int k = ff[i]+j-1;
        if (isunsorted) k = oo[k]-1;
        if (nosubset ? xi[k]==NA_INTEGER : (irows[k]==NA_INTEGER || (k=irows[k]-1,xi[k]==NA_INTEGER))) nacount++;
        else subi[j-nacount] = xi[k];
      }
      ansd[i] = (nacount && !narm) ? NA_REAL : iquickselect(subi, thisgrpsize-nacount);
    }}
    break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)), "median (gmedian)", "stats::median(.)");
  }
  if (!isInt64) copyMostAttrib(x, ans);
  // else the integer64 class needs to be dropped since double is always returned by gmedian
  UNPROTECT(2);  // ans, subd|subi
  return ans;
}

static SEXP gfirstlast(SEXP x, const bool first, const int w, const bool headw) {
  // w: which item (1 other than for gnthvalue when could be >1)
  // headw: select 1:w of each group when first=true, and (n-w+1):n when first=false (i.e. tail)
  const bool nosubset = irowslen == -1;
  const bool issorted = !isunsorted; // make a const-bool for use inside loops
  const int n = nosubset ? length(x) : irowslen;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, first?"gfirst":"glast");
  if (w==1 && headw) internal_error(__func__, "headw should only be true when w>1");
  int anslen = ngrp;
  if (headw) {
    anslen = 0;
    for (int i=0; i<ngrp; ++i) {
      anslen += MIN(w, grpsize[i]);
    }
  }
  SEXP ans = PROTECT(allocVector(TYPEOF(x), anslen));
  int ansi = 0;
  #define DO(CTYPE, RTYPE, RNA, ASSIGN) {                                                          \
    const CTYPE *xd = (const CTYPE *)RTYPE(x);                                                     \
    if (headw) {                                                                                   \
      /* returning more than 1 per group; w>1 */                                                   \
      for (int i=0; i<ngrp; ++i) {                                                                 \
        const int grpn = grpsize[i];                                                               \
        const int thisn = MIN(w, grpn);                                                            \
        const int jstart = ff[i]-1+ (!first)*(grpn-thisn);                                         \
        const int jend = jstart+thisn;                                                             \
        for (int j=jstart; j<jend; ++j) {                                                          \
          const int k = issorted ? j : oo[j]-1;                                                    \
          /* ternary on const-bool assumed to be branch-predicted and ok inside loops */           \
          const CTYPE val = nosubset ? xd[k] : (irows[k]==NA_INTEGER ? RNA : xd[irows[k]-1]);      \
          ASSIGN;                                                                                  \
        }                                                                                          \
      }                                                                                            \
    } else if (w==1) {                                                                             \
      for (int i=0; i<ngrp; ++i) {                                                                 \
        const int j = ff[i]-1 + (first ? 0 : grpsize[i]-1);                                        \
        const int k = issorted ? j : oo[j]-1;                                                      \
        const CTYPE val = nosubset ? xd[k] : (irows[k]==NA_INTEGER ? RNA : xd[irows[k]-1]);        \
        ASSIGN;                                                                                    \
      }                                                                                            \
    } else if (w>1 && first) {                                                                     \
      /* gnthvalue */                                                                              \
      for (int i=0; i<ngrp; ++i) {                                                                 \
        const int grpn = grpsize[i];                                                               \
        if (w>grpn) { const CTYPE val=RNA; ASSIGN; continue; }                                     \
        const int j = ff[i]-1+w-1;                                                                 \
        const int k = issorted ? j : oo[j]-1;                                                      \
        const CTYPE val = nosubset ? xd[k] : (irows[k]==NA_INTEGER ? RNA : xd[irows[k]-1]);        \
        ASSIGN;                                                                                    \
      }                                                                                            \
    } else {                                                                                       \
      /* w>1 && !first not supported because -i in R means everything-but-i and gnthvalue */       \
      /* currently takes n>0 only. However, we could still support n'th from the end, somehow */   \
      internal_error(__func__, "unanticipated case first=%d w=%d headw=%d", first, w, headw);      \
    }                                                                                              \
  }
  switch(TYPEOF(x)) {
  case LGLSXP:  { int      *ansd=LOGICAL(ans); DO(int,      LOGICAL, NA_LOGICAL,   ansd[ansi++]=val) } break;
  case INTSXP:  { int      *ansd=INTEGER(ans); DO(int,      INTEGER, NA_INTEGER,   ansd[ansi++]=val) } break;
  case REALSXP: if (INHERITS(x, char_integer64)) {
           int64_t *ansd=(int64_t *)REAL(ans); DO(int64_t,  REAL,    NA_INTEGER64, ansd[ansi++]=val) }
           else { double      *ansd=REAL(ans); DO(double,   REAL,    NA_REAL,      ansd[ansi++]=val) } break;
  case CPLXSXP: { Rcomplex *ansd=COMPLEX(ans); DO(Rcomplex, COMPLEX, NA_CPLX,      ansd[ansi++]=val) } break;
  case STRSXP:  DO(SEXP, STRING_PTR_RO, NA_STRING,              SET_STRING_ELT(ans,ansi++,val))        break;
  case VECSXP:  DO(SEXP, SEXPPTR_RO, ScalarLogical(NA_LOGICAL), SET_VECTOR_ELT(ans,ansi++,val))        break;
  default:
    error(_("Type '%s' is not supported by GForce head/tail/first/last/`[`. Either add the namespace prefix (e.g. utils::head(.)) or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  return(ans);
}

SEXP glast(SEXP x) {
  return gfirstlast(x, false, 1, false);
}

SEXP gfirst(SEXP x) {
  return gfirstlast(x, true, 1, false);
}

SEXP gtail(SEXP x, SEXP nArg) {
  if (!isInteger(nArg) || LENGTH(nArg)!=1 || INTEGER(nArg)[0]<1) internal_error(__func__, "gtail is only implemented for n>0. This should have been caught before"); // # nocov
  const int n=INTEGER(nArg)[0];
  return n==1 ? glast(x) : gfirstlast(x, false, n, true);
}

SEXP ghead(SEXP x, SEXP nArg) {
  if (!isInteger(nArg) || LENGTH(nArg)!=1 || INTEGER(nArg)[0]<1) internal_error(__func__, "gtail is only implemented for n>0. This should have been caught before"); // # nocov
  const int n=INTEGER(nArg)[0];
  return n==1 ? gfirst(x) : gfirstlast(x, true, n, true);
}

SEXP gnthvalue(SEXP x, SEXP nArg) {
  if (!isInteger(nArg) || LENGTH(nArg)!=1 || INTEGER(nArg)[0]<1) internal_error(__func__, "`g[` (gnthvalue) is only implemented single value subsets with positive index, e.g., .SD[2]. This should have been caught before"); // # nocov
  return gfirstlast(x, true, INTEGER(nArg)[0], false);
}

// TODO: gwhich.min, gwhich.max
// implemented this similar to gmedian to balance well between speed and memory usage. There's one extra allocation on maximum groups and that's it.. and that helps speed things up extremely since we don't have to collect x's values for each group for each step (mean, residuals, mean again and then variance).
static SEXP gvarsd1(SEXP x, SEXP narmArg, bool isSD)
{
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  if (!isVectorAtomic(x)) error(_("GForce var/sd can only be applied to columns, not .SD or similar. For the full covariance matrix of all items in a list such as .SD, either add the prefix stats::var(.SD) (or stats::sd(.SD)) or turn off GForce optimization using options(datatable.optimize=1). Alternatively, if you only need the diagonal elements, 'DT[,lapply(.SD,var),by=,.SDcols=]' is the optimized way to do this."));
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), isSD ? "sd" : "var");
  const int n = (irowslen == -1) ? length(x) : irowslen;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gvar");
  SEXP sub, ans = PROTECT(allocVector(REALSXP, ngrp));
  double *ansd = REAL(ans);
  const bool nosubset = irowslen==-1;
  const bool narm = LOGICAL(narmArg)[0];
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    sub = PROTECT(allocVector(INTSXP, maxgrpn)); // allocate once upfront
    int *subd = INTEGER(sub);
    const int *xd = INTEGER(x);
    for (int i=0; i<ngrp; ++i) {
      const int thisgrpsize = grpsize[i];
      if (thisgrpsize==1) {
        ansd[i] = NA_REAL;
      } else {
        int nna = 0;  // how many not-NA
        long double m=0., s=0., v=0.;
        for (int j=0; j<thisgrpsize; ++j) {
          int ix = ff[i]+j-1;
          if (isunsorted) ix = oo[ix]-1;
          if (nosubset ? xd[ix]==NA_INTEGER : (irows[ix]==NA_INTEGER || (ix=irows[ix]-1,xd[ix]==NA_INTEGER))) {
            if (narm) continue; else break;
          }
          m += (subd[nna++]=xd[ix]); // sum
        }
        if (nna!=thisgrpsize && (!narm || nna<=1)) { ansd[i]=NA_REAL; continue; }
        m = m/nna; // mean, first pass
        for (int j=0; j<nna; ++j) s += (subd[j]-m); // residuals
        m += (s/nna); // mean, second pass
        for (int j=0; j<nna; ++j) { // variance
          v += (subd[j]-(double)m) * (subd[j]-(double)m);
        }
        ansd[i] = (double)v/(nna-1);
        if (isSD) ansd[i] = SQRTL(ansd[i]);
      }
    }}
    break;
  case REALSXP: {
    sub = PROTECT(allocVector(REALSXP, maxgrpn)); // allocate once upfront
    double *subd = REAL(sub);
    const double *xd = REAL(x);
    for (int i=0; i<ngrp; ++i) {
      const int thisgrpsize = grpsize[i];
      if (thisgrpsize==1) {
        ansd[i] = NA_REAL;
      } else {
        int nna = 0;  // how many not-NA
        long double m=0., s=0., v=0.;
        for (int j=0; j<thisgrpsize; ++j) {
          int ix = ff[i]+j-1;
          if (isunsorted) ix = oo[ix]-1;
          if (nosubset ? ISNAN(xd[ix]) : (irows[ix]==NA_INTEGER || (ix=irows[ix]-1,ISNAN(xd[ix])))) {
            if (narm) continue; else break;
          }
          m += (subd[nna++]=xd[ix]); // sum
        }
        if (nna!=thisgrpsize && (!narm || nna<=1)) { ansd[i]=NA_REAL; continue; }
        m = m/nna; // mean, first pass
        for (int j=0; j<nna; ++j) s += (subd[j]-m); // residuals
        m += (s/nna); // mean, second pass
        for (int j=0; j<nna; ++j) { // variance
          v += (subd[j]-(double)m) * (subd[j]-(double)m);
        }
        ansd[i] = (double)v/(nna-1);
        if (isSD) ansd[i] = SQRTL(ansd[i]);
      }
    }}
    break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"),
            type2char(TYPEOF(x)), isSD?"sd (gsd)":"var (gvar)", isSD?"stats::sd(.)":"stats::var(.)");
  }
  // no copyMostAttrib(x, ans) since class (e.g. Date) unlikely applicable to sd/var
  UNPROTECT(2); // ans,sub
  return ans;
}

SEXP gvar(SEXP x, SEXP narm) {
  return (gvarsd1(x, narm, FALSE));
}

SEXP gsd(SEXP x, SEXP narm) {
  return (gvarsd1(x, narm, TRUE));
}

SEXP gprod(SEXP x, SEXP narmArg) {
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  const bool narm=LOGICAL(narmArg)[0];
  if (!isVectorAtomic(x))
    error(_("GForce prod can only be applied to columns, not .SD or similar. To multiply all items in a list such as .SD, either add the prefix base::prod(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,prod),by=,.SDcols=]'"));
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "prod");
  const bool nosubset = irowslen==-1;
  const int n = nosubset ? length(x) : irowslen;
  //clock_t start = clock();
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gprod");
  long double *s = malloc(sizeof(*s) * ngrp);
  if (!s)
    error(_("Unable to allocate %d * %zu bytes for gprod"), ngrp, sizeof(long double)); // # nocov
  for (int i=0; i<ngrp; ++i) s[i] = 1.0;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *xd = INTEGER(x);
    for (int i=0; i<n; ++i) {
      const int thisgrp = grp[i];
      const int elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER : xd[irows[i]-1]);
      if (elem==NA_INTEGER) {
        if (!narm) s[thisgrp] = NA_REAL;  // Let NA_REAL propagate from here. R_NaReal is IEEE.
        continue;
      }
      s[thisgrp] *= elem; // no under/overflow here, s is long double (like base)
    }}
    break;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) {
      const int64_t *xd = (const int64_t *)REAL(x);
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        const int64_t elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER64 : xd[irows[i]-1]);
        if (elem==NA_INTEGER64) {
          if (!narm) s[thisgrp] = NA_REAL;
          continue;
        }
        s[thisgrp] *= elem;
      }
    } else {
      const double *xd = REAL(x);
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        const double elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_REAL : xd[irows[i]-1]);
        if (ISNAN(elem)) {
          if (!narm) s[thisgrp] = NA_REAL;
          continue;
        }
        s[thisgrp] *= elem;
      }
    }
  } break;
  default:
    free(s);
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)), "prod (gprod)", "base::prod(.)");
  }
  SEXP ans = PROTECT(allocVector(REALSXP, ngrp));
  if (INHERITS(x, char_integer64)) {
    int64_t *ansd = (int64_t *)REAL(ans);
    for (int i=0; i<ngrp; ++i) {
      ansd[i] = (ISNAN(s[i]) || s[i]>INT64_MAX || s[i]<=INT64_MIN) ? NA_INTEGER64 : (int64_t)s[i];
    }
  } else {
    double *ansd = REAL(ans);
    for (int i=0; i<ngrp; ++i) {
      if (s[i] > DBL_MAX) ansd[i] = R_PosInf;
      else if (s[i] < -DBL_MAX) ansd[i] = R_NegInf;
      else ansd[i] = (double)s[i];
    }
  }
  free(s);
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  // Rprintf(_("this gprod took %8.3f\n"), 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return ans;
}

SEXP gshift(SEXP x, SEXP nArg, SEXP fillArg, SEXP typeArg) {
  const bool nosubset = irowslen == -1;
  const bool issorted = !isunsorted;
  const int n = nosubset ? length(x) : irowslen;
  if (nrow != n) internal_error(__func__, "nrow [%d] != length(x) [%d] in %s", nrow, n, "gshift");

  int nprotect=0;
  enum {LAG, LEAD/*, SHIFT*/,CYCLIC} stype = LAG;
  if (!(length(fillArg) == 1))
    error(_("fill must be a vector of length 1"));

  if (!isString(typeArg) || length(typeArg) != 1)
    internal_error(__func__, "invalid type, should have been caught before"); // # nocov
  if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "lag")) stype = LAG;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "lead")) stype = LEAD;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "shift")) stype = LAG;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "cyclic")) stype = CYCLIC;
  else internal_error(__func__, "invalid type, should have been caught before"); // # nocov

  bool lag;
  const bool cycle = stype == CYCLIC;

  R_xlen_t nk = length(nArg);
  if (!isInteger(nArg))
    internal_error(__func__, "n must be integer"); // # nocov
  const int *kd = INTEGER(nArg);
  for (int i=0; i<nk; i++) if (kd[i]==NA_INTEGER) error(_("Item %d of n is NA"), i+1);

  SEXP ans = PROTECT(allocVector(VECSXP, nk)); nprotect++;
  SEXP thisfill = PROTECT(coerceAs(fillArg, x, ScalarLogical(0))); nprotect++;
  for (int g=0; g<nk; g++) {
    lag = stype == LAG || stype == CYCLIC;
    int m = kd[g];
    // switch
    if (m < 0) {
      m = m * (-1);
      lag = !lag;
    }
    R_xlen_t ansi = 0;
    SEXP tmp;
    SET_VECTOR_ELT(ans, g, tmp=allocVector(TYPEOF(x), n));
    #define SHIFT(CTYPE, RTYPE, ASSIGN) {                                                                         \
      const CTYPE *xd = (const CTYPE *)RTYPE(x);                                                                  \
      const CTYPE fill = RTYPE(thisfill)[0];                                                                      \
      for (int i=0; i<ngrp; ++i) {                                                                                \
        const int grpn = grpsize[i];                                                                              \
        const int mg = cycle ? (((m-1) % grpn) + 1) : m;                                                          \
        const int thisn = MIN(mg, grpn);                                                                          \
        const int jstart = ff[i]-1+ (!lag)*(thisn);                                                               \
        const int jend = jstart+ MAX(0, grpn-mg); /*if m > grpn -> jend = jstart */                               \
        if (lag) {                                                                                                \
          const int o = ff[i]-1+(grpn-thisn);                                                                     \
          for (int j=0; j<thisn; ++j) {                                                                           \
          const int k = issorted ? (o+j) : oo[o+j]-1;                                                             \
            const CTYPE val = cycle ? (nosubset ? xd[k] : (irows[k]==NA_INTEGER ? fill : xd[irows[k]-1])) : fill; \
            ASSIGN;                                                                                               \
          }                                                                                                       \
        }                                                                                                         \
        for (int j=jstart; j<jend; ++j) {                                                                         \
          const int k = issorted ? j : oo[j]-1;                                                                   \
          const CTYPE val = nosubset ? xd[k] : (irows[k]==NA_INTEGER ? fill : xd[irows[k]-1]);                    \
          ASSIGN;                                                                                                 \
        }                                                                                                         \
        if (!lag) {                                                                                               \
          const int o = ff[i]-1;                                                                                  \
          for (int j=0; j<thisn; ++j) {                                                                           \
            const int k = issorted ? (o+j) : oo[o+j]-1;                                                           \
            const CTYPE val = cycle ? (nosubset ? xd[k] : (irows[k]==NA_INTEGER ? fill : xd[irows[k]-1])) : fill; \
            ASSIGN;                                                                                               \
          }                                                                                                       \
        }                                                                                                         \
      }                                                                                                           \
    }
    switch(TYPEOF(x)) {
      case RAWSXP:  { Rbyte *ansd=RAW(tmp);               SHIFT(Rbyte,   RAW,       ansd[ansi++]=val); } break;
      case LGLSXP:  { int *ansd=LOGICAL(tmp);             SHIFT(int,     LOGICAL,   ansd[ansi++]=val); } break;
      case INTSXP:  { int *ansd=INTEGER(tmp);             SHIFT(int,     INTEGER,   ansd[ansi++]=val); } break;
      case REALSXP: { double *ansd=REAL(tmp);             SHIFT(double,  REAL,      ansd[ansi++]=val); } break;
                    // integer64 is shifted as if it's REAL; and assigning fill=NA_INTEGER64 is ok as REAL
      case CPLXSXP: { Rcomplex *ansd=COMPLEX(tmp);        SHIFT(Rcomplex, COMPLEX,  ansd[ansi++]=val); } break;
      case STRSXP: { SHIFT(SEXP, STRING_PTR_RO,                       SET_STRING_ELT(tmp,ansi++,val)); } break;
      //case VECSXP: { SHIFT(SEXP, SEXPPTR_RO,                          SET_VECTOR_ELT(tmp,ansi++,val)); } break;
      default:
        error(_("Type '%s' is not supported by GForce gshift. Either add the namespace prefix (e.g. data.table::shift(.)) or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)));
    }
    copyMostAttrib(x, tmp); // needed for integer64 because without not the correct class of int64 is assigned
  }
  UNPROTECT(nprotect);
  // consistency with plain shift(): "strip" the list in the 1-input case, for convenience
  return isVectorAtomic(x) && length(ans) == 1 ? VECTOR_ELT(ans, 0) : ans;
}
