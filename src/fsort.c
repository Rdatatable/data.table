#include "data.table.h"

#define INSERT_THRESH 200  // TODO: expose via api and test

static void dinsert(double *x, const int n) {   // TODO: if and when twiddled, double => ull
  if (n<2) return;
  for (int i=1; i<n; ++i) {
    double xtmp = x[i];
    int j = i-1;
    if (xtmp<x[j]) {
      x[j+1] = x[j];
      j--;
      while (j>=0 && xtmp<x[j]) { x[j+1] = x[j]; --j; }
      x[j+1] = xtmp;
    }
  }
}

static uint64_t minULL;

static void dradix_r(  // single-threaded recursive worker
  double *in,          // n doubles to be sorted
  double *working,     // working memory to put the sorted items before copying over *in; must not overlap *in
  uint64_t n,          // number of items to sort.  *in and *working must be at least n long
  int fromBit,         // After twiddle to ordered ull, the bits [fromBit,toBit] are used to count
  int toBit,           //   fromBit<toBit; bit 0 is the least significant; fromBit is right shift amount too
  uint64_t *counts     // already zero'd counts vector, 2^(toBit-fromBit+1) long. A stack of these is reused.
) {
  uint64_t width = 1ULL<<(toBit-fromBit+1);
  uint64_t mask = width-1;

  const double *tmp=in;
  for (uint64_t i=0; i<n; ++i) {
    counts[(*(uint64_t *)tmp - minULL) >> fromBit & mask]++;
    tmp++;
  }
  int last = (*(uint64_t *)--tmp - minULL) >> fromBit & mask;
  if (counts[last] == n) {
    // Single value for these bits here. All counted in one bucket which must be the bucket for the last item.
    counts[last] = 0;  // clear ready for reuse. All other counts must be zero already so save time by not setting to 0.
    if (fromBit > 0)   // move on to next bits (if any remain) to resolve
      dradix_r(in, working, n, fromBit<8 ? 0 : fromBit-8, toBit-8, counts+256);
    return;
  }

  uint64_t cumSum=0;
  for (uint64_t i=0; cumSum<n; ++i) { // cumSum<n better than i<width as may return early
    uint64_t tmp;
    if ((tmp=counts[i])) {  // don't cumulate through 0s, important below to save a wasteful memset to zero
      counts[i] = cumSum;
      cumSum += tmp;
    }
  } // leaves cumSum==n && 0<i && i<=width

  tmp=in;
  for (uint64_t i=0; i<n; ++i) {  // go forwards not backwards to give cpu pipeline better chance
    int thisx = (*(uint64_t *)tmp - minULL) >> fromBit & mask;
    working[ counts[thisx]++ ] = *tmp;
    tmp++;
  }

  memcpy(in, working, n*sizeof(double));

  if (fromBit==0) {
    // nothing left to do other than reset the counts to 0, ready for next recursion
    // the final bucket must contain n and it might be close to the start. After that must be all 0 so no need to reset.
    // Also this way, we don't need to know how big thisCounts is and therefore no possibility of getting that wrong.
    // wasteful thisCounts[i]=0 even when already 0 is better than a branch. We are highly recursive at this point
    // so avoiding memset() is known to be worth it.
    for (int i=0; counts[i]<n; ++i) counts[i]=0;
    return;
  }

  cumSum=0;
  for (int i=0; cumSum<n; ++i) {   // again, cumSum<n better than i<width as it can return early
    if (counts[i] == 0) continue;
    uint64_t thisN = counts[i] - cumSum;  // undo cummulate; i.e. diff
    if (thisN <= INSERT_THRESH) {
      dinsert(in+cumSum, thisN);  // for thisN==1 this'll return instantly. Probably better than several branches here.
    } else {
      dradix_r(in+cumSum, working, thisN, fromBit<=8 ? 0 : fromBit-8, toBit-8, counts+256);
    }
    cumSum = counts[i];
    counts[i] = 0; // reset to 0 to save wasteful memset afterwards
  }
}

uint64_t *qsort_data;
// would have liked to define cmp inside fsort where qsort is called but wasn't sure that's portable
int qsort_cmp(const void *a, const void *b) {
  // return >0 if the element a goes after the element b
  // doesn't master if stable or not
  uint64_t x = qsort_data[*(int *)a];
  uint64_t y = qsort_data[*(int *)b];
  // return x-y;  would like this, but this is long and the cast to int return may not preserve sign
  // We have long vectors in mind (1e10(74GB), 1e11(740GB)) where extreme skew may feasibly mean the largest count
  // is greater than 2^32. The first split is (currently) 16 bits so should be very rare but to be safe keep 64bit counts.
  return (x<y)-(x>y);   // largest first in a safe branchless way casting long to int
}

static size_t shrinkMSB(size_t MSBsize, uint64_t *msbCounts, int *order, Rboolean verbose) {
  size_t oldMSBsize = MSBsize;
  while (MSBsize>0 && msbCounts[order[MSBsize-1]] < 2)
    MSBsize--;
  if (verbose && oldMSBsize != MSBsize) {
    Rprintf(_("Reduced MSBsize from %zu to %zu by excluding 0 and 1 counts\n"), oldMSBsize, MSBsize);
  }
  return MSBsize;
}

/*
  OpenMP is used here to find the range and distribution of data for efficient
    grouping and sorting.
*/
SEXP fsort(SEXP x, SEXP verboseArg) {
  double t[10];
  t[0] = wallclock();
  if (!IS_TRUE_OR_FALSE(verboseArg))
    error(_("%s must be TRUE or FALSE"), "verbose");
  Rboolean verbose = LOGICAL(verboseArg)[0];
  if (!isNumeric(x)) error(_("x must be a vector of type double currently"));
  // TODO: not only detect if already sorted, but if it is, just return x to save the duplicate

  SEXP ansVec = PROTECT(allocVector(REALSXP, xlength(x)));
  int nprotect = 1;
  double *ans = REAL(ansVec);
  // allocate early in case fails if not enough RAM
  // TODO: document this is much cheaper than a copy followed by in-place.

  int nth = getDTthreads(xlength(x), true);
  int nBatch=nth*2;  // at least nth; more to reduce last-man-home; but not too large to keep counts small in cache
  if (verbose)
    Rprintf("nth=%d, nBatch=%d\n", nth, nBatch); // # notranslate

  size_t batchSize = (xlength(x)-1)/nBatch + 1;
  if (batchSize < 1024) batchSize = 1024; // simple attempt to work reasonably for short vector. 1024*8 = 2 4kb pages
  nBatch = (xlength(x)-1)/batchSize + 1;
  size_t lastBatchSize = xlength(x) - (nBatch-1)*batchSize;
  // could be that lastBatchSize == batchSize when i) xlength(x) is multiple of nBatch
  // and ii) for small vectors with just one batch

  t[1] = wallclock();
  double *mins = (double *)malloc(nBatch * sizeof(double));
  double *maxs = (double *)malloc(nBatch * sizeof(double));
  if (!mins || !maxs) {
    free(mins); free(maxs); // # nocov
    error(_("Failed to allocate %d bytes in fsort()."), (int)(2 * nBatch * sizeof(double))); // # nocov
  }
  const double *restrict xp = REAL(x);
  #pragma omp parallel for schedule(dynamic) num_threads(getDTthreads(nBatch, false))
  for (int batch=0; batch<nBatch; ++batch) {
    uint64_t thisLen = (batch==nBatch-1) ? lastBatchSize : batchSize;
    const double *restrict d = xp + batchSize*batch;
    double myMin=*d, myMax=*d;
    d++;
    for (uint64_t j=1; j<thisLen; ++j) {
      // TODO: test for sortedness here as well.
      if (*d<myMin) myMin=*d;
      else if (*d>myMax) myMax=*d;
      d++;
    }
    mins[batch] = myMin;
    maxs[batch] = myMax;
  }
  t[2] = wallclock();
  double min=mins[0], max=maxs[0];
  for (int i=1; i<nBatch; ++i) {
    // TODO: if boundaries are sorted then we only need sort the unsorted batches known above
    if (mins[i]<min) min=mins[i];
    if (maxs[i]>max) max=maxs[i];
  }
  free(mins); free(maxs);
  if (verbose) Rprintf(_("Range = [%g,%g]\n"), min, max);
  if (min < 0.0) error(_("Cannot yet handle negatives."));
  // TODO: -0ULL should allow negatives
  //       avoid twiddle function call as expensive in recent tests (0.34 vs 2.7)
  //       possibly twiddle once to *ans, then untwiddle at the end in a fast parallel sweep

  union {double d; uint64_t u64;} u;
  u.d = max;
  uint64_t maxULL = u.u64;
  u.d = min;
  minULL = u.u64;  // set static global for use by dradix_r

  int maxBit = floor(log(maxULL-minULL) / log(2));  // 0 is the least significant bit
  int MSBNbits = maxBit > 15 ? 16 : maxBit+1;       // how many bits make up the MSB
  int shift = maxBit + 1 - MSBNbits;                // the right shift to leave the MSB bits remaining
  size_t MSBsize = 1LL<<MSBNbits;                   // the number of possible MSB values (16 bits => 65,536)
  if (verbose)
    Rprintf("maxBit=%d; MSBNbits=%d; shift=%d; MSBsize=%zu\n", maxBit, MSBNbits, shift, MSBsize); // # notranslate

  uint64_t *counts = (uint64_t *)R_alloc(nBatch*MSBsize, sizeof(uint64_t));
  memset(counts, 0, nBatch*MSBsize*sizeof(uint64_t));
  // provided MSBsize>=9, each batch is a multiple of at least one 4k page, so no page overlap

  if (verbose) Rprintf(_("counts is %dMB (%d pages per nBatch=%d, batchSize=%"PRIu64", lastBatchSize=%"PRIu64")\n"),
                       (int)(nBatch*MSBsize*sizeof(uint64_t)/(1024*1024)),
                       (int)(nBatch*MSBsize*sizeof(uint64_t)/(4*1024*nBatch)),
                       nBatch, (uint64_t)batchSize, (uint64_t)lastBatchSize);
  t[3] = wallclock();
  #pragma omp parallel for num_threads(nth)
  for (int batch=0; batch<nBatch; ++batch) {
    uint64_t thisLen = (batch==nBatch-1) ? lastBatchSize : batchSize;
    const uint64_t *restrict tmp = (uint64_t *)(xp + batchSize*batch);
    uint64_t *restrict thisCounts = counts + batch*MSBsize;
    for (uint64_t j=0; j<thisLen; ++j) {
      thisCounts[(*tmp - minULL) >> shift]++;
      tmp++;
    }
  }

  // cumulate columnwise; parallel histogram; small so no need to parallelize
  uint64_t rollSum=0;
  for (int msb=0; msb<MSBsize; ++msb) {
    int j = msb;
    for (int batch=0; batch<nBatch; ++batch) {
      uint64_t tmp = counts[j];
      counts[j] = rollSum;
      rollSum += tmp;
      j += MSBsize;  // deliberately non-contiguous here
    }
  }  // leaves msb cumSum in the last batch i.e. last row of the matrix

  t[4] = wallclock();
  uint64_t *restrict ansi64 = (uint64_t *)ans;
  #pragma omp parallel for num_threads(nth)
  for (int batch=0; batch<nBatch; ++batch) {
    uint64_t thisLen = (batch==nBatch-1) ? lastBatchSize : batchSize;
    const uint64_t *restrict source = (uint64_t *)(xp + batchSize*batch);
    uint64_t *restrict thisCounts = counts + batch*MSBsize;
    for (uint64_t j=0; j<thisLen; ++j) {
      ansi64[ thisCounts[(*source - minULL) >> shift]++ ] = *source;
      // This assignment to ans is not random access as it may seem, but cache efficient by
      // design since target pages are written to contiguously. MSBsize * 4k < cache.
      // TODO: therefore 16 bit MSB seems too big for this step. Time this step and reduce 16 a lot.
      //       20MB cache / nth / 4k => MSBsize=160
      source++;
    }
  }
  // Done with batches now. Will not use batch dimension again.
  t[5] = wallclock();

  if (shift > 0) { // otherwise, no more bits left to resolve ties and we're done
    int toBit = shift-1;
    int fromBit = toBit>7 ? toBit-7 : 0;

    // sort bins by size, largest first to minimise last-man-home
    uint64_t *msbCounts = counts + (nBatch-1)*MSBsize;
    // msbCounts currently contains the ending position of each MSB (the starting location of the next) even across empty
    if (msbCounts[MSBsize-1] != xlength(x)) internal_error(__func__, "counts[nBatch-1][MSBsize-1] != length(x)"); // # nocov
    uint64_t *msbFrom = (uint64_t *)R_alloc(MSBsize, sizeof(uint64_t));
    int *order = (int *)R_alloc(MSBsize, sizeof(int));
    uint64_t cumSum = 0;
    for (int i=0; i<MSBsize; ++i) {
      msbFrom[i] = cumSum;
      msbCounts[i] = msbCounts[i] - cumSum;
      cumSum += msbCounts[i];
      order[i] = i;
    }
    qsort_data = msbCounts;
    qsort(order, MSBsize, sizeof(int), qsort_cmp);  // find order of the sizes, largest first
    // Would have liked to define qsort_cmp() inside this function right here, but not sure that's fully portable.
    // TODO: time this qsort but likely insignificant.

    if (verbose) {
      Rprintf(_("Top 20 MSB counts: ")); for(int i=0; i<MIN(MSBsize,20); i++) Rprintf(_("%"PRId64" "), (int64_t)msbCounts[order[i]]); Rprintf(_("\n"));
    }
    MSBsize = shrinkMSB(MSBsize, msbCounts, order, verbose);

    bool failed=false, alloc_fail=false, non_monotonic=false; // shared bools only ever assigned true; no need for atomic or critical assign
    t[6] = wallclock();
    #pragma omp parallel num_threads(getDTthreads(MSBsize, false))
    {
      // each thread has its own small stack of counts
      // don't use VLAs here: perhaps too big for stack yes but more that VLAs apparently fail with schedule(dynamic)
      uint64_t *restrict mycounts = calloc((toBit/8 + 1)*256, sizeof(uint64_t));
      if (!mycounts) {
        failed=true; alloc_fail=true;  // # nocov
      }
      double *restrict myworking = NULL;
      // the working memory for the largest group per thread is allocated when the thread receives its first iteration
      int myfirstmsb = -1;  // for the monotonicity check

      #pragma omp for schedule(monotonic_dynamic,1)
      // We require here that a thread can never be assigned to an earlier iteration; e.g. threads 0:(nth-1)
      // get iterations 0:(nth-1), possibly out of order, then first-come-first-served in order after that.
      // If a thread deals with an msb earlier than the first one it dealt with, then its *working will be too small.
      // This is not true in clang-11 using OpenMP 5.0 (_OPENMP==201811); #4786. But the monotonic: modifier (OpenMP 4.5+)
      // makes it true. To continue support of OpenMP<4.5:
      //      i) myomp.h defines monotonic_dynamic since macro name cannot contain colon
      // and ii) we now check monotonicity is true, otherwise halt with helpful error. It's likely true anyway in older implementations
      //         since that's the simplest implementation and what we thought was the case anyway. I guess that clang-11 is doing
      //         some new advanced dynamic optimizations; e.g. allocating threads to iterations based on knowledge of data locality rather
      //         than simple monotonic first-come-first-served. However, we have arranged for the iterations to be in size order, so
      //         monotonic:dynamic is the right schedule here.
      for (int msb=0; msb<MSBsize; ++msb) {
        if (failed) continue;

        uint64_t from = msbFrom[order[msb]];
        uint64_t thisN = msbCounts[order[msb]];

        if (myworking==NULL) {
          myworking = malloc(thisN * sizeof(double));
          if (!myworking) {
            failed=true; alloc_fail=true; continue;  // # nocov
          }
          myfirstmsb = msb;
        }
        if (myfirstmsb==-1 || msb<myfirstmsb) {
          failed=true; non_monotonic=true; continue;  // # nocov
        }

        // Depends on msbCounts being sorted largest first before this parallel loop
        // Could be significant RAM saving if the largest msb is
        // a lot larger than the 2nd largest msb, especially as nth grows to perhaps 128 on X1.
        // However, the initial split is so large (16bits => 65,536) that the largest MSB should be
        // relatively small anyway (n/65,536 if uniformly distributed).
        // For msb>=nth, that thread's *myworking will already be big enough because
        // the smallest *myworking (for thread nth-1) is big enough for all iterations following.
        // Progressively, less and less of the working will be needed by the thread (just the first thisN will be
        // used) and the unused lines will simply not be cached.

        if (thisN <= INSERT_THRESH) {
          dinsert(ans+from, thisN);
        } else {
          dradix_r(ans+from, myworking, thisN, fromBit, toBit, mycounts);
        }
      }
      free(mycounts);
      free(myworking);
    }
    if (non_monotonic)
      error(_("OpenMP %d did not assign threads to iterations monotonically. Please search Stack Overflow for this message."), MY_OPENMP); // # nocov; #4786 in v1.13.4
    if (alloc_fail)
      error(_("Unable to allocate working memory")); // # nocov
  }
  t[7] = wallclock();

  // TODO: parallel sweep to check sorted using <= on original input. Feasible that twiddling messed up.
  //       After a few years of heavy use remove this check for speed, and move into unit tests.
  //       It's a perfectly contiguous and cache efficient parallel scan so should be relatively negligible.

  double tot = t[7]-t[0];
  if (verbose) for (int i=1; i<=7; ++i) {
    Rprintf(_("%d: %.3f (%4.1f%%)\n"), i, t[i]-t[i-1], 100.*(t[i]-t[i-1])/tot);
  }
  UNPROTECT(nprotect);
  return(ansVec);
}

