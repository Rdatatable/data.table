#include "data.table.h"

static union {
  double d;
  unsigned long long ull;
} u;

SEXP fsort(SEXP x) {
  Rboolean verbose = TRUE;
  if (!isNumeric(x)) error("x must be a vector of type 'double' currently");
  // TODO: not only detect if already sorted, but if it is, just return x to save the duplicate
  #ifndef _OPENMP
  Rprintf("Your platform/environment has not detected OpenMP support. fsort() will still work, but slower in single threaded mode.\n");
  // Rprintf rather than warning() because warning() would cause test.data.table() to error about the unexpected warnings
  #endif
  
  int nBatch=128;
  // At least as many cores.
  // A few times more to reduce last-man-home time.
  // Not too many though, otherwise counts too big for cache
  
  if (xlength(x) < nBatch) error("Length of input must be at least %d, currently.", nBatch);
  long batchSize = (xlength(x)-1)/nBatch + 1;
  nBatch = (xlength(x)-1)/batchSize + 1;
  long lastBatchSize = xlength(x) - (nBatch-1)*batchSize;  // could be that lastBatchSize == batchSize
  
  SEXP ansVec = PROTECT(allocVector(REALSXP, xlength(x)));  // much cheaper than a copy followed by in-place  TODO: document
  double *ans = (double *)DATAPTR(ansVec);
  
  double mins[nBatch], maxs[nBatch];
  #pragma omp parallel for
  for (int batch=0; batch<nBatch; batch++) {
    long thisLen = (batch==nBatch-1) ? lastBatchSize : batchSize;
    double *d = &REAL(x)[batchSize * batch];
    double myMin=*d, myMax=*d;
    d++;
    for (long j=1; j<thisLen; j++) {
      if (*d<myMin) myMin=*d;
      else if (*d>myMax) myMax=*d;
      d++;
    }
    mins[batch] = myMin;
    maxs[batch] = myMax;
  }
  double min=mins[0], max=maxs[0];
  for (int i=1; i<nBatch; i++) {
    if (mins[i]<min) min=mins[i];
    if (maxs[i]>max) max=maxs[i];
  }
  Rprintf("Range = [%g,%g]\n", min, max);
  if (min < 0.0) error("Cannot yet handle negatives.");   // TODO:  -0ULL should allow negatives
  u.d = max;
  unsigned long long maxULL = u.ull;
  u.d = min;
  unsigned long long minULL = u.ull;
  
  int maxBit = 1 + floor(log(maxULL-minULL) / log(2));
  int shift = maxBit > 8 ? maxBit-8 : 0;
  
  Rprintf("maxBit=%d; shift=%d\n", maxBit, shift);
  
  long counts[nBatch][256];  // each N has 4 * 4K pages (so no page invalidation due to overlap) ... 8*8*256 = 16K
  memset(counts, 0, sizeof(counts));
  
  if (verbose) Rprintf("counts is %dMB (%d pages per nBatch=%d, batchSize=%lld)\n", sizeof(counts)/(1024*1024), sizeof(counts)/(4*1024*nBatch), nBatch, batchSize);
  
  // TODO Test skipping batch numbers by 2 to avoid page invalidation on counts
  // TODO Report time of each part when verbose
  // TODO: could also find range in this single sweep. And if already sorted.
  // NB: twiddle expensive iteratively here (0.34 vs 2.7).
  // unsigned long long d = dtwiddle(x, off+j, /*order*/1);
  // TODO: undo twiddle from radix order as well
  // NB: no need for radix skipping now
      
  #pragma omp parallel for
  for (int batch=0; batch<nBatch; batch++) {
    long thisLen = (batch==nBatch-1) ? lastBatchSize : batchSize;
    double *tmp = &REAL(x)[batchSize * batch];
    for (long j=0; j<thisLen; j++) {
      counts[batch][(*(unsigned long long *)tmp - minULL) >> shift]++;
      tmp++;
    }
  }
  
  // now decide strategy.  e.g. if range is 0, or already sorted then return early.
  // If sorted already by chunk, just sort within the unsorted chunks.
  
  // i) cumulate columnwise across batches for the msb
  // ii) total the byte counts as we'll descend into those groups later
  long byteCounts[256] = {0};
  long rollSum=0;
  for (int byte=0; byte<256; byte++) {
    for (int batch=0; batch<nBatch; batch++) {
      long tmp = counts[batch][byte];
      byteCounts[byte] += tmp;
      counts[batch][byte] = rollSum;
      rollSum += tmp;
    }
  }
  
  #pragma omp parallel for
  for (int batch=0; batch<nBatch; batch++) {
    long thisLen = (batch==nBatch-1) ? lastBatchSize : batchSize;
    double *source = &REAL(x)[batchSize * batch];
    for (long j=0; j<thisLen; j++) {
      ans[ counts[batch][(*(unsigned long long *)source - minULL) >> shift]++ ] = *source;
      // This assignment to ans is not random access, but cache efficient by design
      // TODO: move counts[batch] outside j loop
      source++;
    }
  }  
  // Done with batches now. Will not use batch dimension again.
  
  // Cummulate byteCounts
  for (int byte=1; byte<256; byte++)
    byteCounts[byte] += byteCounts[byte-1];
  
  // DONE: schedule(dynamic) makes a big difference here
  #pragma omp parallel for schedule(dynamic)   
  for (int byte=0; byte<256; byte++) {
    long from= (byte>0) ? (byteCounts[byte-1]+1) : 1;
    long to = byteCounts[byte];
    if (from < to) R_qsort(ans, from, to);  // 1-based from and to
  }
  // TODO: omp_set_nested(1) to handle skew
  
  // TODO: parallel sweep to check sorted using <= on original input. Feasible that twiddling messed up.
  //       After a few years of heavy use remove this check for speed, and move into unit tests.
  //       It's a perfectly contiguous and cache efficient parallel scan so should be relatively negligible.
  
  UNPROTECT(1);
  return(ansVec);
}


