#include "data.table.h"
/*
  Inspired by :
  icount in do_radixsort in src/main/sort.c @ rev 51389.
  base::sort.list(method="radix") turns out not to be a radix sort, but a counting sort :
    http://r.789695.n4.nabble.com/method-radix-in-sort-list-isn-t-actually-a-radix-sort-tp3309470p3309470.html
  Wish granted for R to allow negatives (simple change) : https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=15644
  Terdiman and Herf (LSB radix for floating point and other efficiency tricks) :
  http://codercorner.com/RadixSortRevisited.htm
  http://stereopsis.com/radix.html

  Previous version of this file was promoted into base R, see ?base::sort.
  Denmark useR! presentation <link>
  Stanford DSC presentation <link>
  JSM presentation <link>
  Techniques used :
    nested parallelism for skew
    finds unique bins to save 256 sweeping
    skips already-grouped yet unsorted
    flip flop group-size stack
    reuses R's global character cache (truelength on CHARSXP)
    compressed column can cross byte boundaries to use spare bits (e.g. 2 16-level columns in one byte)
    just the remaining part of key is reordered as the radix progresses
    columnar byte-key for within-radix MT cache efficiency

  Only forder() and dtwiddle() functions are meant for use by other C code in data.table, hence all other functions here are static.
  The coding techniques deployed here are for efficiency. The static functions are recursive or called repetitively and we wish to minimise
  overhead. They reach outside themselves to place results in the end result directly rather than returning many small pieces of memory.
*/

// #define TIMING_ON
static int *gs[2] = {NULL};          // gs = groupsizes e.g. 23,12,87,2,1,34,...
static int flip = 0;                 // two vectors flip flopped: flip and 1-flip
static int gsalloc[2] = {0};         // allocated stack size
static int gsngrp[2] = {0};          // used
static int gsmaxalloc = 0;           // max size of stack, set by forder to nrows
static bool stackgrps = true;        // switched off for last radix (most dense final step) when group sizes aren't needed by caller
static int  *cradix_counts = NULL;
static SEXP *cradix_xtmp   = NULL;
static SEXP *ustr = NULL;
static int ustr_alloc = 0;
static int ustr_n = 0;
static int ustr_maxlen = 0;
static int sort = 0;                 // 0 no sort; -1 descending, +1 ascending

#define Error(...) do {cleanup(); error(__VA_ARGS__);} while(0)      // http://gcc.gnu.org/onlinedocs/cpp/Swallowing-the-Semicolon.html#Swallowing-the-Semicolon
#undef warning
#define warning(...) Do not use warning in this file                // since it can be turned to error via warn=2
/* Using OS realloc() in this file to benefit from (often) in-place realloc() to save copy
 * We have to trap on exit anyway to call savetl_end().
 * NB: R_alloc() would be more convenient (fails within) and robust (auto free) but there is no R_realloc(). Implementing R_realloc() would be an alloc and copy, iiuc.
 *     Calloc/Realloc needs to be Free'd, even before error() [R-exts$6.1.2]. An oom within Calloc causes a previous Calloc to leak so Calloc would still needs to be trapped anyway.
 * Therefore, using <<if (!malloc()) Error("helpful context msg")>> approach to cleanup() on error.
 */

static void free_ustr() {
  for(int i=0; i<ustr_n; i++)
    SET_TRUELENGTH(ustr[i],0);
  free(ustr); ustr=NULL;
  ustr_alloc=0; ustr_n=0; ustr_maxlen=0;
}

static void cleanup() {
  free(gs[0]); gs[0] = NULL;
  free(gs[1]); gs[1] = NULL;
  flip = 0;
  gsalloc[0] = gsalloc[1] = 0;
  gsngrp[0] = gsngrp[1] = 0;
  gsmaxalloc = 0;
  free(cradix_counts); cradix_counts=NULL;
  free(cradix_xtmp);   cradix_xtmp=NULL;
  free_ustr();
  savetl_end();  // Restore R's own usage of tl. Must run after the for loop in free_ustr() since only CHARSXP which had tl>0 (R's usage) are stored there.
}

static void growstack(uint64_t newlen) {
  if (newlen==0) newlen=4096;                                       // thus initially 4 pages since sizeof(int)==4
  if (newlen>gsmaxalloc) newlen=gsmaxalloc;                         // gsmaxalloc is type int so newlen can now be cast to int ok
  gs[flip] = realloc(gs[flip], newlen*sizeof(int));
  if (gs[flip] == NULL) Error("Failed to realloc working memory stack to %d*4bytes (flip=%d)", (int)newlen, flip);
  gsalloc[flip] = newlen;
}

static void newpush(int *x, int n) {
  if (!stackgrps || x==0) return;
  if (gsalloc[flip] < gsngrp[flip]+n) growstack(((uint64_t)(gsngrp[flip])+n)*2);
  memcpy(gs[flip]+gsngrp[flip], x, n*sizeof(int));
  gsngrp[flip]+=n;
}

static void push(int x) {
  if (!stackgrps || x==0) return;
  if (gsalloc[flip] == gsngrp[flip]) growstack((uint64_t)(gsngrp[flip])*2);
  gs[flip][gsngrp[flip]++] = x;
}

static void flipflop() {
  flip = 1-flip;
  gsngrp[flip] = 0;
  if (gsalloc[flip] < gsalloc[1-flip]) growstack((uint64_t)(gsalloc[1-flip])*2);
}

#ifdef TIMING_ON
  // many calls to clock() can be expensive, hence compiled out rather than switch(verbose)
  #include <time.h>
  #define NBLOCK 20
  static clock_t tblock[NBLOCK], tstart;
  static int nblock[NBLOCK];
  #define TBEG() tstart = clock();
  #define TEND(i) tblock[i] += clock()-tstart; nblock[i]++; tstart = clock();
#else
  #define TBEG()
  #define TEND(i)
#endif

// range_* functions return [min,max] of the non-NAs as common uint64_t type
// TODO parallelize

static void range_i32(int32_t *x, int n, uint64_t *out_min, uint64_t *out_max, int *out_na_count)
{
  int32_t min = NA_INTEGER;
  int32_t max = NA_INTEGER;
  int i=0;
  while(i<n && x[i]==NA_INTEGER) i++;
  int na_count = i;
  if (i<n) max = min = x[i++];
  for(; i<n; i++) {
    int tmp = x[i];
    if (tmp>max) max=tmp;
    else if (tmp<min) {
      if (tmp==NA_INTEGER) na_count++;
      else min=tmp;
    }
  }
  *out_na_count = na_count;
  *out_min = min ^ 0x80000000u;  // map [-2147483648(INT32_MIN), 2147483647(INT32_MAX)] => [0, 4294967295(UINT32_MAX)]
  *out_max = max ^ 0x80000000u;
}

static void range_i64(int64_t *x, int n, uint64_t *out_min, uint64_t *out_max, int *out_na_count)
{
  int64_t min = INT64_MIN;
  int64_t max = INT64_MIN;
  int i=0;
  while(i<n && x[i]==INT64_MIN) i++;
  int na_count = i;
  if (i<n) max = min = x[i++];
  for(; i<n; i++) {
    int64_t tmp = x[i];
    if (tmp>max) max=tmp;
    else if (tmp<min) {
      if (tmp==INT64_MIN) na_count++;
      else min=tmp;
    }
  }
  *out_na_count = na_count;
  // map [INT64_MIN, INT64_MAX] => [0, UINT64_MAX]
  *out_min = min ^ 0x8000000000000000u;
  *out_max = max ^ 0x8000000000000000u;
}

static void range_d(double *x, int n, uint64_t *out_min, uint64_t *out_max, int *out_na_count)
// return range of finite numbers (excluding NA, NaN, -Inf, +Inf) and a count of
// strictly NA (not NaN) so caller knows if it's all NA or any Inf|-Inf|NaN are present
{
  uint64_t min = 0;
  uint64_t max = 0;
  int na_count = 0;
  int i=0;
  while(i<n && !R_FINITE(x[i])) { na_count+=ISNA(x[i]); i++; }
  if (i<n) { max = min = dtwiddle(x, i++);}
  for(; i<n; i++) {
    if (!R_FINITE(x[i])) { na_count+=ISNA(x[i]); continue; }
    uint64_t tmp = dtwiddle(x, i);
    if (tmp>max) max=tmp;
    else if (tmp<min) min=tmp;
  }
  *out_na_count = na_count;
  *out_min = min;
  *out_max = max;
}

// non-critical function also used by bmerge and chmatch
int StrCmp(SEXP x, SEXP y)
{
  if (x == y) return 0;             // same cached pointer (including NA_STRING==NA_STRING)
  if (x == NA_STRING) return -1;    // x<y
  if (y == NA_STRING) return 1;     // x>y
  return strcmp(CHAR(ENC2UTF8(x)), CHAR(ENC2UTF8(y)));
}
/* ENC2UTF8 handles encoding issues by converting all marked non-utf8 encodings alone to utf8 first. The function could be wrapped
   in the first if-statement already instead of at the last stage, but this is to ensure that all-ascii cases are handled with maximum efficiency.
   This seems to fix the issues as far as I've checked. Will revisit if necessary.
   OLD COMMENT: can return 0 here for the same string in known and unknown encodings, good if the unknown string is in that encoding but not if not ordering is ascii only (C locale).
   TO DO: revisit and allow user to change to strcoll, and take account of Encoding. see comments in bmerge().  10k calls of strcmp = 0.37s, 10k calls of strcoll = 4.7s. See ?Comparison, ?Encoding, Scollate in R internals.
   TO DO: check that all unknown encodings are ascii; i.e. no non-ascii unknowns are present, and that either Latin1
          or UTF-8 is used by user, not both. Then error if not. If ok, then can proceed with byte level. ascii is never marked known by R, but
          non-ascii (i.e. knowable encoding) could be marked unknown. Does R API provide is_ascii?
*/

static void cradix_r(SEXP *xsub, int n, int radix)
// xsub is a unique set of CHARSXP, to be ordered by reference
// First time, radix==0, and xsub==x. Then recursively moves SEXP together for cache efficiency.
// Quite different to iradix because
//   1) x is known to be unique so fits in cache (wide random access not an issue)
//   2) they're variable length character strings
//   3) no need to maintain o.  Just simply reorder x. No grps or push.
{
  if (n<=1) return;
  int *thiscounts = cradix_counts + radix*256;
  uint8_t lastx = 0;  // the last x is used to test its bin
  for (int i=0; i<n; i++) {
    lastx = radix<LENGTH(xsub[i]) ? (uint8_t)(CHAR(xsub[i])[radix]) : 1;  // no NA_STRING present,  1 for "" (could use 0 too maybe since NA_STRING not present)
    thiscounts[ lastx ]++;
  }
  if (thiscounts[lastx]==n && radix<ustr_maxlen-1) {
    cradix_r(xsub, n, radix+1);
    thiscounts[lastx] = 0;  // all x same value, the rest must be 0 already, save the memset
    return;
  }
  int itmp = thiscounts[0];
  for (int i=1; i<256; i++) {
    if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);  // don't cummulate through 0s, important below
  }
  for (int i=n-1; i>=0; i--) {
    uint8_t thisx = radix<LENGTH(xsub[i]) ? (uint8_t)(CHAR(xsub[i])[radix]) : 1;
    cradix_xtmp[--thiscounts[thisx]] = xsub[i];
  }
  memcpy(xsub, cradix_xtmp, n*sizeof(SEXP));
  if (radix == ustr_maxlen-1) {
    memset(thiscounts, 0, 256*sizeof(int));
    return;
  }
  if (thiscounts[0] != 0) Error("Logical error. counts[0]=%d in cradix but should have been decremented to 0. radix=%d", thiscounts[0], radix);
  itmp = 0;
  for (int i=1; i<256; i++) {
    if (thiscounts[i] == 0) continue;
    int thisgrpn = thiscounts[i] - itmp;  // undo cumulate; i.e. diff
    cradix_r(xsub+itmp, thisgrpn, radix+1);
    itmp = thiscounts[i];
    thiscounts[i] = 0;  // set to 0 now since we're here, saves memset afterwards. Important to do this ready for this memory's reuse
  }
  if (itmp<n-1) cradix_r(xsub+itmp, n-itmp, radix+1);  // final group
}

static void cradix(SEXP *x, int n)
{
  cradix_counts = (int *)calloc(ustr_maxlen*256, sizeof(int));  // counts for the letters of left-aligned strings
  if (!cradix_counts) Error("Failed to alloc cradix_counts");
  cradix_xtmp = (SEXP *)malloc(ustr_n*sizeof(SEXP));
  if (!cradix_xtmp) Error("Failed to alloc cradix_tmp");
  cradix_r(x, n, 0);
  free(cradix_counts); cradix_counts=NULL;
  free(cradix_xtmp);   cradix_xtmp=NULL;
}

static void range_str(SEXP *x, int n, uint64_t *out_min, uint64_t *out_max, int *out_na_count)
// group numbers are left in truelength to be fetched by WRITE_KEY
{
  int na_count=0;
  bool anyneedutf8=false;
  if (ustr_n!=0) Error("Internal error: ustr isn't empty when starting range_str: ustr_n=%d, ustr_alloc=%d", ustr_n, ustr_alloc);  // # nocov
  if (ustr_maxlen!=0) Error("Internal error: ustr_maxlen isn't 0 when starting range_str");  // # nocov
  // savetl_init() has already been called at the start of forder
  #pragma omp parallel for num_threads(getDTthreads())
  for(int i=0; i<n; i++) {
    SEXP s = x[i];
    if (s==NA_STRING) {
      #pragma omp atomic update
      na_count++;
      continue;
    }
    if (TRUELENGTH(s)<0) continue;  // seen this group before
    #pragma omp critical
    if (TRUELENGTH(s)>=0) {  // another thread may have set it while I was waiting, so check it again
      if (TRUELENGTH(s)>0)   // save any of R's own usage of tl (assumed positive, so we can both count and save in one scan), to restore
        savetl(s);           // afterwards. From R 2.14.0, tl is initialized to 0, prior to that it was random so this step saved too much.
      // now save unique SEXP in ustr so i) we can loop through them afterwards and reset TRUELENGTH to 0 and ii) sort uniques when sorting too
      if (ustr_alloc<=ustr_n) {
        ustr_alloc = (ustr_alloc==0) ? 16384 : ustr_alloc*2;  // small initial guess, negligible time to alloc 128KB (32 pages)
        if (ustr_alloc>n) ustr_alloc = n;  // clamp at n. Reaches n when fully unique (no dups)
        ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
        if (ustr==NULL) Error("Unable to realloc %d * %d bytes in range_str", ustr_alloc, sizeof(SEXP));  // # nocov
      }
      ustr[ustr_n++] = s;
      SET_TRUELENGTH(s, -ustr_n);  // unique in any order is fine. first-appearance order is achieved later in count_group
      if (LENGTH(s)>ustr_maxlen) ustr_maxlen=LENGTH(s);
      if (!anyneedutf8 && NEED2UTF8(s)) anyneedutf8=true;
    }
  }
  *out_na_count = na_count;
  if (ustr_n==0) {  // all na
    *out_min = 0;
    *out_max = 0;
    return;
  }
  if (anyneedutf8) {
    SEXP ustr2 = PROTECT(allocVector(STRSXP, ustr_n));
    for (int i=0; i<ustr_n; i++) SET_STRING_ELT(ustr2, i, ENC2UTF8(ustr[i]));
    SEXP *ustr3 = (SEXP *)malloc(ustr_n * sizeof(SEXP));
    if (!ustr3) Error("Failed to alloc ustr3 when converting strings to UTF8");  // # nocov
    memcpy(ustr3, STRING_PTR(ustr2), ustr_n*sizeof(SEXP));
    for (int i=0; i<ustr_n; i++) {
      SEXP s = ustr3[i];
      if (TRUELENGTH(s)>0) savetl(s);
    }
    cradix(ustr3, ustr_n);  // sort to detect possible duplicates after converting; e.g. two different non-utf8 map to the same utf8
    SET_TRUELENGTH(ustr3[0], -1);
    int o = -1;
    for (int i=1; i<ustr_n; i++) {
      if (ustr3[i] == ustr3[i-1]) continue;  // use the same o for duplicates
      SET_TRUELENGTH(ustr3[i], --o);
    }
    // now use the 1-1 mapping from ustr to ustr2 to get the ordering back into original ustr, being careful to reset tl to 0
    int *tl = (int *)malloc(ustr_n * sizeof(int));
    if (!tl) Error("Failed to alloc tl when converting strings to UTF8");  // # nocov
    SEXP *tt = STRING_PTR(ustr2);
    for (int i=0; i<ustr_n; i++) tl[i] = TRUELENGTH(tt[i]);   // fetches the o in ustr3 into tl which is ordered by ustr
    for (int i=0; i<ustr_n; i++) SET_TRUELENGTH(ustr3[i], 0);    // reset to 0 tl of the UTF8 (and possibly non-UTF in ustr too)
    for (int i=0; i<ustr_n; i++) SET_TRUELENGTH(ustr[i], tl[i]); // put back the o into ustr's tl
    free(tl);
    free(ustr3);
    UNPROTECT(1);
    *out_min = 1;
    *out_max = -o;  // could be less than ustr_n if there are duplicates in the utf8s
  } else {
    *out_min = 1;
    *out_max = ustr_n;
    if (sort) {
      // that this is always ascending; descending is done in WRITE_KEY using max-this
      cradix(ustr, ustr_n);  // sorts ustr in-place by reference. assumes NA_STRING not present.
      for(int i=0; i<ustr_n; i++)     // save ordering in the CHARSXP. negative so as to distinguish with R's own usage.
        SET_TRUELENGTH(ustr[i], -i-1);
    }
    // else group appearance order was already saved to tl in the first pass
  }
}

static bool sort_ugrp(uint8_t *x, const int n)
// x contains n unique bytes; sort them in-place insert sort
// always ascending. desc and na.last are done in WRITE_KEY because columns may cross byte boundaries
// maximum value for n is 256 which is one too big for uint8_t, hence int n
{
  bool skip = true;            // was x already sorted? if so, the caller can skip reordering
  for (int i=1; i<n; i++) {
    uint8_t tmp = x[i];
    if (tmp>x[i-1]) continue;  // x[i-1]==x[i] doesn't happen because x is unique
    skip = false;
    int j = i-1;
    do {
      x[j+1] = x[j];
    } while (--j>=0 && tmp<x[j]);
    x[j+1] = tmp;
  }
  return skip;
}

static void count_group(const uint8_t *x, const int n, bool *out_skip, uint16_t *out_order, int *out_ngrp, int *out_grpsize)
// returns order and group sizes; caller uses the order to reorder subsequent radix
// if not sorting (!sort) then whether bytes are grouped are detected and first-appearance order maintained
{
  uint8_t ugrp[256];  // uninitialized is fine. unique groups; avoids sweeping all 256 counts when very often very few of them are used
  uint16_t counts[256] = {0};  // Need to be all-0 on entry. This ={0} initialization should be fast as it's on stack. But if not then pass in my_counts
                               // to save initialization and rely on last line of this function which resets just the non-zero's to 0
  ugrp[0] = x[0];     // first item is always first of first group
  counts[x[0]] = 1;
  int ngrp = 1;       // number of groups (items in ugrp[])
  bool skip = true;   // i) if already grouped and sort=false then caller can skip. ii) if already sorted when sort=true then caller can skip

  for (int i=1; i<n; i++) {
    uint8_t elem = x[i];
    if (counts[elem]==0) {
      // have not seen this value before
      ugrp[ngrp++]=elem;
    } else if (skip && elem!=x[i-1]) {
      // seen this value before and it isn't the previous value, so data is not grouped
      // including "skip &&" first is to avoid the != comparison
      skip=false;
    }
    counts[elem]++;
  }

  if (sort && !sort_ugrp(ugrp, ngrp))  // sorts ugrp by reference
    skip=false;

  *out_skip = skip;
  *out_ngrp = ngrp;   //  ngrp==1 => skip==true (common case important to skip efficiently here)
  for (int i=0; i<ngrp; i++) { uint8_t w=ugrp[i]; out_grpsize[i]=counts[w]; }  // no need to sweep == in caller to find groups; we already have sizes here
  if (!skip) {
    // this is where the potentially random access comes in but only to at-most 256 points, each contigously
    for (int i=0, sum=0; i<ngrp; i++) { uint8_t w=ugrp[i]; int tmp=counts[w]; counts[w]=sum; sum+=tmp; }  // prepare for forwards-assign to help cpu prefetch on next line
    for (int i=0; i<n; i++) { uint8_t elem=x[i]; out_order[counts[elem]++] = i; }  // it's this second sweep through x[] that benefits from x[] being in cache from last time above, so keep it small.
  }
  for (int i=0; i<ngrp; i++) counts[ugrp[i]] = 0;  // ready for next time to save initializing the 256 vector
}

static int dround=0;      // No rounding by default, for now. Handles #1642, #1728, #1463, #485
static uint64_t dmask=0;

SEXP setNumericRounding(SEXP droundArg)
// init.c has initial call with default of 2
{
  if (!isInteger(droundArg) || LENGTH(droundArg)!=1) error("Must an integer or numeric vector length 1");
  if (INTEGER(droundArg)[0] < 0 || INTEGER(droundArg)[0] > 2) error("Must be 2, 1 or 0");
  dround = INTEGER(droundArg)[0];
  dmask = dround ? 1 << (8*dround-1) : 0;
  return R_NilValue;
}

SEXP getNumericRounding()
{
  return ScalarInteger(dround);
}

int getNumericRounding_C()
// for use in uniqlist.c
{
  return dround;
}

// for signed integers it's easy: flip sign bit to swap positives and negatives; the resulting unsigned is in the right order with INT_MIN ending up as 0
// for floating point finite you have to flip the other bits too if it was signed: http://stereopsis.com/radix.html
uint64_t dtwiddle(void *p, int i)
{
  union {
    double d;
    uint64_t u64;
  } u;  // local for thread safety
  u.d = ((double *)p)[i];
  if (R_FINITE(u.d)) {
    if (u.d==0) u.d=0; // changes -0.0 to 0.0,  issue #743
    u.u64 ^= (u.u64 & 0x8000000000000000) ? 0xffffffffffffffff : 0x8000000000000000; // always flip sign bit and if negative (sign bit was set) flip other bits too
    u.u64 += (u.u64 & dmask) << 1;   // when dround==1|2, if 8th|16th bit is set, round up before chopping last 1|2 bytes
    return u.u64 >> (dround*8);
  }
  if (ISNAN(u.d)) return ISNA(u.d)    ? 0 /*NA*/   : 1 /*NaN*/;  // also normalises a difference between NA on 32bit R (bit 13 set) and 64bit R (bit 13 not set)
  if (isinf(u.d)) return signbit(u.d) ? 2 /*-Inf*/ : (0xffffffffffffffff>>(dround*8)) /*+Inf*/;
  Error("Unknown non-finite value; not NA, NaN, -Inf or +Inf");
}

SEXP forder(SEXP DT, SEXP by, SEXP retGrp, SEXP sortStrArg, SEXP orderArg, SEXP naArg)
// sortStr TRUE from setkey, FALSE from by=
{
  /*
#ifdef TIMING_ON
  memset(tblock, 0, NBLOCK*sizeof(clock_t));
  memset(nblock, 0, NBLOCK*sizeof(int));
  clock_t tstart;  // local variable to mask the global tstart for ease when timing sub funs
#endif
  TBEG()
  */
  int n_protect = 0;

  if (!isNewList(DT)) {
    if (!isVectorAtomic(DT)) error("Input is not either a list of columns, or an atomic vector.");
    if (!isNull(by)) error("Input is an atomic vector (not a list of columns) but by= is not NULL");
    if (!isInteger(orderArg) || LENGTH(orderArg)!=1) error("Input is an atomic vector (not a list of columns) but order= is not a length 1 integer");
    SEXP tt = PROTECT(allocVector(VECSXP, 1)); n_protect++;
    SET_VECTOR_ELT(tt, 0, DT);
    DT = tt;
    by = PROTECT(allocVector(INTSXP, 1)); n_protect++;
    INTEGER(by)[0] = 1;
  }

  if (!length(DT)) error("DT is an empty list() of 0 columns");
  if (!isInteger(by) || !LENGTH(by)) error("DT has %d columns but 'by' is either not integer or is length 0", length(DT));  // seq_along(x) at R level
  if (!isInteger(orderArg) || LENGTH(orderArg)!=LENGTH(by)) error("Either 'order' is not integer or its length (%d) is different to 'by's length (%d)", LENGTH(orderArg), LENGTH(by));
  int n = length(VECTOR_ELT(DT,0));
  for (int i=0; i<LENGTH(by); i++) {
    if (INTEGER(by)[i] < 1 || INTEGER(by)[i] > length(DT))
      error("'by' value %d out of range [1,%d]", INTEGER(by)[i], length(DT));
    if ( n != length(VECTOR_ELT(DT, INTEGER(by)[i]-1)) )
      error("Column %d is length %d which differs from length of column 1 (%d)\n", INTEGER(by)[i], length(VECTOR_ELT(DT, INTEGER(by)[i]-1)), n);
  }

  if (!isLogical(retGrp) || LENGTH(retGrp)!=1 || INTEGER(retGrp)[0]==NA_LOGICAL) error("retGrp must be TRUE or FALSE");
  if (!isLogical(sortStrArg) || LENGTH(sortStrArg)!=1 || INTEGER(sortStrArg)[0]==NA_LOGICAL ) error("sortStr must be TRUE or FALSE");
  if (!isLogical(naArg) || LENGTH(naArg) != 1) error("na.last must be logical TRUE, FALSE or NA of length 1");
  sort = LOGICAL(sortStrArg)[0]==TRUE;  // TODO: rename sortStr to just sort.  Just one argument either TRUE/FALSE, or a vector of +1|-1.
  int nalast = (LOGICAL(naArg)[0] == NA_LOGICAL) ? -1 : LOGICAL(naArg)[0]; // 1=na last, 0=na first (default), -1=remove na
  gsmaxalloc = n;  // upper limit for stack size (all size 1 groups). We'll detect and avoid that limit, but if just one non-1 group (say 2), that can't be avoided.

  if (n==0) {
    // empty vector or 0-row DT is always sorted
    SEXP ans = PROTECT(allocVector(INTSXP, 0)); n_protect++;
    if (LOGICAL(retGrp)[0]) {
      setAttrib(ans, sym_starts, allocVector(INTSXP, 0));
      setAttrib(ans, sym_maxgrpn, ScalarInteger(0));
    }
    UNPROTECT(n_protect);
    return ans;
  }
  // if n==1, the code is left to proceed below in case one or more of the 1-row by= columns are NA and na.last=NA. Otherwise it would be easy to return now.
  bool all_skipped = true;

  SEXP ans = PROTECT(allocVector(INTSXP, n)); n_protect++;
  int *ansd = INTEGER(ans);
  for (int i=0; i<n; i++) ansd[i]=i+1;   // gdb 8.1.0.20180409-git very slow here, oddly

  savetl_init();   // from now on use Error not error

  int ncol=length(by);
  uint8_t *key[ncol*8+1];  // needs to be before loop because part II relies on part I, column-by-column. +1 because we check NULL after last one
  for (int i=0; i<ncol*8+1; i++) key[i]=NULL;
  int nradix=0; // the current byte we're writing this column to; might be squashing into it (spare>0)
  int spare=0;  // the amount of bits remaining on the right of the current nradix byte
  bool isReal=false;
  for (int col=0; col<ncol; col++) {
    // Rprintf("Finding range of column %d ...\n", col);
    SEXP x = VECTOR_ELT(DT,INTEGER(by)[col]-1);
    uint64_t min, max;     // min and max of non-NA values
    int na_count=0;
    if (sort) sort=INTEGER(orderArg)[col];  // +1(asc) -1(desc) 0(first-appearance)
    //Rprintf("sort = %d\n", sort);
    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP :  // TODO skip LGL and assume range [0,1]
      range_i32(INTEGER(x), n, &min, &max, &na_count);
      break;
    case REALSXP :
      if (inherits(x, "integer64")) {
        range_i64((int64_t *)REAL(x), n, &min, &max, &na_count);
      } else {
        range_d(REAL(x), n, &min, &max, &na_count);
        if (min==0 && na_count<n) { min=3; max=4; } // column contains no finite numbers and is not-all NA; create dummies to yield positive min-2 later
        isReal = true;
      }
      break;
    case STRSXP :
      // need2utf8 now happens inside range_str on the uniques
      range_str(STRING_PTR(x), n, &min, &max, &na_count);
      break;
    default:
       Error("Column %d of by= (%d) is type '%s', not yet supported", col+1, INTEGER(by)[col], type2char(TYPEOF(x)));
    }

    if ((min==0 && na_count==n) || (min>0 && min==max && na_count==0)) {
      // all same value; skip column as nothing to do
      // min==0 implies na_count==n anyway for all types other than real when Inf,-Inf or NaN are present (excluded from [min,max] as well as NA)
      if (min==0 && nalast==-1) { all_skipped=false; for (int i=0; i<n; i++) ansd[i]=0; }
      if (TYPEOF(x)==STRSXP) free_ustr();
      continue;
    }

    uint64_t range = max-min+1 +1/*NA*/ +isReal*3/*NaN, -Inf, +Inf*/;
    // Rprintf("range=%llu  min=%llu  max=%llu\n", range, min, max);

    int maxBit=0;
    while (range) { maxBit++; range>>=1; }
    int nbyte = 1+(maxBit-1)/8; // the number of bytes spanned by the value
    int firstBits = maxBit - (nbyte-1)*8;  // how many bits used in most significant byte
    if (spare==0) {
      spare = 8-firstBits; // left align to byte boundary to get better first split.
    } else {
      if (sort==0) {
        // can't squash into previous spare as that will change first-appearance order within that radix-byte
        //   found thanks to test 1246.55 with seed 1540402216L and added new direct test 1954
        spare = 8-firstBits;
        nradix++;
      }
      else if (spare >= firstBits) {
        // easiest case. just left shift a few bits within these nbyte to fill the spare gap
        spare -= firstBits;      // new spare is also how many bits to now left shift
      }
      else {
        if (nbyte<8) {
          spare += 8-firstBits;
          nbyte++;               // after shift, will need an extra byte
        } else {
          // range takes 8 bytes so can't shift into 9th to use spare of last byte of previous column; start with a new left-aligned byte
          nradix++;
          spare = 8-firstBits;
        }
      }
    }

    for (int b=0; b<nbyte; b++) {
      if (key[nradix+b]==NULL)
        key[nradix+b] = calloc(n, sizeof(uint8_t));  // 0 initialize so that NA's can just skip (NA is always the 0 offset)
    }

    const bool asc = (sort>=0);
    uint64_t min2=min, max2=max;
    if (nalast<1 && asc) {
      min2--; // so that x-min results in 1 for the minimum; NA coded by 0. Always provide for the NA spot even if NAs aren't present for code brevity and robustness
      if (isReal) min2--;  // Nan first
    } else {
      max2++;            // NA is max+1 so max2-elem should result in 0
      if (isReal) max2++;  // Nan last
    }
    if (isReal) { min2--; max2++; }  // -Inf and +Inf  in min-1 and max+1 spots respectively

    const uint64_t naval = ((nalast==1) == (sort>=0)) ? max+1+isReal*2 : min-1-isReal*2;
    const uint64_t nanval = ((nalast==1) == (sort>=0)) ? max+2 : min-2;  // only used when isReal

    // several columns could squash into 1 byte. due to this bit squashing is why we deal
    // with asc|desc here, otherwise it could be done in the ugrp sorting by reversing the ugrp insert sort
    #define WRITE_KEY                                   \
    elem = asc ? elem-min2 : max2-elem;                 \
    elem <<= spare;                                     \
    for (int b=nbyte-1; b>0; b--) {                     \
      key[nradix+b][i] = (uint8_t)(elem & 0xff);        \
      elem >>= 8;                                       \
    }                                                   \
    key[nradix][i] |= (uint8_t)(elem & 0xff);
    // this last |= squashes the most significant bits of this column into the the spare of the previous
    // NB: retaining group order does not retain the appearance-order in the sense that DT[,,by=] does. Because we go column-by-column, the first column values
    //     are grouped together.   Whether we do byte-column-within-column doesn't alter this and neither does bit packing across column boundaries.
    //     if input data is grouped-by-column (not grouped-by-row) then this grouping strategy may help a lot.
    //     if input data is random ordering, then a consequence of grouping-by-column (whether sorted or not) is that the output ordering will not be by
    //       row-group-appearance-order built it; the result will still need to be resorted by row number of first group item.
    //     this was the case with the previous forder as well, which went by whole-column
    //     So, we may as well bit-pack as done above.
    //     Retaining apperance-order within key-byte-column is still advatangeous for when we do encounter column-grouped data (to avoid data movement); and
    //       the ordering will be appearance-order preserving in that case.

    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP : {
      int32_t *xd = INTEGER(x);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<n; i++) {
        uint64_t elem=0;
        if (xd[i]==NA_INTEGER) {  // TODO: go branchless if na_count==0
          if (nalast==-1) {all_skipped=false; ansd[i]=0;}
          elem = naval;
        } else {
          elem = xd[i] ^ 0x80000000u;
        }
        WRITE_KEY
      }}
      break;
    case REALSXP :
      if (inherits(x, "integer64")) {
        int64_t *xd = (int64_t *)REAL(x);
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i=0; i<n; i++) {
          uint64_t elem=0;
          if (xd[i]==INT64_MIN) {
            if (nalast==-1) {all_skipped=false; ansd[i]=0;}
            elem = naval;
          } else {
            elem = xd[i] ^ 0x8000000000000000u;
          }
          WRITE_KEY
        }
      } else {
        double *xd = REAL(x);     // TODO: revisit double compression (skip bytes/mult by 10,100 etc) as currently it's often 6-8 bytes even for 3.14,3.15
        #pragma omp parallel for num_threads(getDTthreads())
        for (int i=0; i<n; i++) {
          uint64_t elem=0;
          if (!R_FINITE(xd[i])) {
            if (isinf(xd[i])) elem = signbit(xd[i]) ? min-1 : max+1;
            else {
              if (nalast==-1) {all_skipped=false; ansd[i]=0;}  // for both NA and NaN
              elem = ISNA(xd[i]) ? naval : nanval;
            }
          } else {
            elem = dtwiddle(xd, i);  // TODO: could avoid twiddle() if all positive finite which could be known from range_d.
                                     //       also R_FINITE is repeated within dtwiddle() currently, wastefully given the if() above
          }
          WRITE_KEY
        }
      }
      break;
    case STRSXP : {
      SEXP *xd = STRING_PTR(x);
      #pragma omp parallel for num_threads(getDTthreads())
      for (int i=0; i<n; i++) {
        uint64_t elem=0;
        if (xd[i]==NA_STRING) {
          if (nalast==-1) {all_skipped=false; ansd[i]=0;}
          elem = naval;
        } else {
          elem = -TRUELENGTH(xd[i]);
        }
        WRITE_KEY
      }}
      free_ustr();  // ustr could be left allocated and reused, but free now in case large and we're tight on ram
      break;
    default:
       Error("Internal error: column not supported not caught earlier");  // # nocov
    }
    nradix += nbyte-1+(spare==0);
    // Rprintf("Written key for column %d\n", col);
  }
  if (key[nradix]!=NULL) nradix++;  // nradix now number of bytes in key
  // Rprintf("nradix=%d\n", nradix);

  stackgrps = true;
  push(n);
  omp_set_nested(1);
  const int STL = MIN(n, 65535);  // Single Thread Load.  Such that counts can be uint16_t (smaller for MT cache efficiency)
  char *TMP = malloc(n*sizeof(int));

  for (int radix=0; radix<nradix; radix++) {
    /*Rprintf("Before radix %d ...", radix);
    for (int i=0; i<n; i++) {
      Rprintf("%03d: ",i);
      for (int r=0; r<nradix; r++) Rprintf("%03d ",key[r][i]);
      Rprintf("\n");
    }*/

    flipflop();
    stackgrps = radix<(nradix-1) || LOGICAL(retGrp)[0];  // if just ordering, we can save allocating and writing the last (and biggest) group size vector
    int tmp=gs[1-flip][0]; for (int i=1; i<gsngrp[1-flip]; i++) tmp=(gs[1-flip][i]+=tmp);   // cumulate, so we can parallelize

    #pragma omp parallel num_threads( n<=STL ? 1 : getDTthreads() )  // avoid starting potentially 32 threads (each with STL temps) for small input that is ST anyway
    {
      uint16_t my_order[STL];
      int my_gs[STL];     // group sizes, if all size 1 then STL will be full.  TODO: can my_gs and my_ng be uint16_t too instead of int?
      int my_ng=0;        // number of groups; number of items used in my_gs
      int my_tmp[STL];    // used to reorder ans (and thus needs to be int) and the first 1/4 is used to reorder each key[index]

      #pragma omp for ordered schedule(dynamic)
      for (int g=0; g<gsngrp[1-flip]; g++) {     // first time this will be just 1; the first split will be the nested method
        int from = g==0 ? 0 : gs[1-flip][g-1];   // this is why grps need to be pre-cumulated
        int to = gs[1-flip][g];
        const int my_n = to-from;
        bool nested = false;
        if (my_n==1) {
          my_gs[0] = 1;
          my_ng = 1;
        } else if (my_n<=STL) {
          bool skip;
          count_group(key[radix]+from, my_n, &skip, my_order, &my_ng, my_gs);
          if (!skip) {
            // reorder ansd
            all_skipped = false; // ok to write from threads (naked) in this case since bool and only ever write false
            int *osub = ansd+from;
            // if (radix==0 | direct) {  # (don't do)
            //   // will only happen when entire input<STL, so it's not worth saving the tiny waste here at the expense of a deep branch later
            //   // write the ordering directly (without going via my_tmp to reorder contents of o) because o just contains identity 1:n when radix==0
            //   if (from!=0) Error("Internal error: from!=0 in radix=0 where n<=STL");
            //   for (int i=0; i<my_n; i++) osub[i] = from+my_order[i]+1;
            // }
            for (int i=0; i<my_n; i++) my_tmp[i] = osub[my_order[i]];
            memcpy(osub, my_tmp, my_n*sizeof(int));

            // TODO - instead of creating my_order at all,  just use counts and push the values to 256-write cache lines,  with contiguous-read from osub and ksub
            //        the 256 live cache-lines is worst case;  if there are many fewer,  then only that number of write-cache lines will be written, and
            //        save allocating and populating my_order, too.  These write-cache lines will be constrained within the STL width, so should be close by in cache as well.
            //        And if there's a good degree of grouping, there is the same contiguous-read/write benefit both ways.

            // reorder remaining key columns (radix+1 onwards)
            for (int remaining_radix=radix+1; remaining_radix<nradix; remaining_radix++) {
              uint8_t *b = (uint8_t *)my_tmp;
              uint8_t *ksub = key[remaining_radix]+from;
              for (int j=0; j<my_n; j++) *b++ = ksub[my_order[j]];
              memcpy(ksub, my_tmp, my_n*1);
            }
          }
        } else {
          nested = true;
        }

        #pragma omp ordered
        {
          if (!nested) {
            newpush(my_gs, my_ng);
          } else {
            int nBatch = getDTthreads()*4;  // at least nth; more to reduce last-chunk-home; but not too large size we need nBatch*256 counts
            size_t batchSize = (my_n-1)/nBatch + 1;
            nBatch = (my_n-1)/batchSize + 1;
            int lastBatchSize = my_n - (nBatch-1)*batchSize;
            int *restrict counts = calloc(nBatch*256, sizeof(int));
            // TODO:  if (!counts) exit parallel and Error...
            //Rprintf("nested batch count...\n");
            uint8_t ugrp[256];  // head(ugrp,ngrp) contain the unique values seen so far
            bool    seen[256];  // is the value present in ugrp
            int     ngrp=0;     // max value 256 so not uint8_t
            uint8_t last_seen=0;  // the last grp seen in the previous batch.  initialized 0 is not used
            for (int i=0; i<256; i++) seen[i]=false;

            #pragma omp parallel for ordered schedule(dynamic) num_threads(getDTthreads())
            for (int batch=0; batch<nBatch; batch++) {
              int my_n = (batch==nBatch-1) ? lastBatchSize : batchSize;  // lastBatchSize == batchSize when my_n is multiple of nBatch
              const uint8_t *tmp = key[radix] + from + batch*batchSize;
              int *my_counts = counts + batch*256;            // TODO: type could switch from uint64 to uint32 depending on my_n>2^32; two loops
              uint8_t my_ugrp[256];
              int     my_ngrp=0;
              bool    my_skip=true;
              for (int i=0; i<my_n; i++) {
                if (my_counts[*tmp]==0) {
                  my_ugrp[my_ngrp++] = *tmp;
                } else if (my_skip && tmp[0]!=tmp[-1]) {
                  my_skip=false;
                }
                my_counts[*tmp++]++;
              }
              if (!my_skip) all_skipped=false;  // outside ordered to save work in prior waiting threads; fine to update bool to false without atomic
              #pragma omp ordered
              {
                for (int i=0; i<my_ngrp; i++) {
                  if (!seen[my_ugrp[i]]) {
                    seen[my_ugrp[i]] = true;
                    ugrp[ngrp++] = last_seen = my_ugrp[i];
                  } else if (all_skipped && my_ugrp[i]!=last_seen) {
                    all_skipped=false;
                  }
                }
              }
            }

            if (sort && !sort_ugrp(ugrp, ngrp))
              all_skipped=false;

            //Rprintf("cumulate...\n");
            // cumulate columnwise; parallel histogram; small so no need to parallelize
            for (int b=0, rollSum=0; b<ngrp; b++) {
              int j = ugrp[b];
              for (int batch=0; batch<nBatch; batch++) {
                int tmp = counts[j];
                counts[j] = rollSum;
                rollSum += tmp;
                j += 256;  // deliberately non-contiguous here
              }
            }

            // reorder o and the remaining radix bytes.
            // if radix==0,  then we can just write directly to.  Otherwise we have to reorder existing o (and allocate a tmp for the reorder)
            // a tmp nrow*1 byte is fine to use to reorder the key column one by one.
            // instead of creating order and then using order, which will be wide access,  we will push the value to the right place, and reuse the counts over and over.
            // first memcpy counts to private and then cumulate on-stack
            // writing to 256-locations, each contiguously, will keep hop around 256 cache lines being written, but contigious read through (large) x
            // alternatively if o was created, we would write contiguously but jump around reading from x which would be more cache-line fetches (although, read-only)

            // counts are going to have to be int,  to hold the offsets into nrow.  256*4 = 16 cache lines. We have 4096 cache lines (256 threads)
            // if (TMP_size<my_n) {
            //   TMP = (char *)realloc(TMP, my_n);
            //   // TODO  if (!TMP) Error
            //   TMP_size = my_n;
            // }
            // Rprintf("reorder remaining radix keys (from=%d)...\n", from);

            // TODO: doesn't need to happen if skipped! Point of tracking grouped/orderedness above.
            for (int remaining_radix=radix+1; remaining_radix<nradix; remaining_radix++) {
              //Rprintf("remaining radix %d...\n", remaining_radix);
              //Rprintf("TRACE:  %p %p %d %p %p %p %p\n", source, TMP, my_n, key[0], key[1], key[2], key[3]);
              #pragma omp parallel num_threads(getDTthreads())
              {
                int my_tmp_counts[256];
                #pragma omp for schedule(dynamic)
                for (int batch=0; batch<nBatch; batch++) {
                  const int my_n = (batch==nBatch-1) ? lastBatchSize : batchSize;
                  memcpy(my_tmp_counts, counts+batch*256, 256*sizeof(int));  // copy to cumulate since we need to reuse orginal to reorder other cols too.
                  const uint8_t *restrict b = key[radix]           + from + batch*batchSize;
                  const uint8_t *restrict c = key[remaining_radix] + from + batch*batchSize;
                  for (int i=0; i<my_n; i++) ((uint8_t *restrict)TMP)[my_tmp_counts[*b++]++] = *c++;
                }
              }
              memcpy(key[remaining_radix]+from, TMP, my_n);
            }

            const bool direct = false; //(radix==0); // TODO: reinstate direct and save TMP
            if (direct) {
              if (from!=0) Error("internal error: first nested for radix==0 but from!=0");
            }
            //else if (TMP_size<(my_n*sizeof(int))) {
            //  TMP = (char *)realloc(TMP, my_n*sizeof(int));
            //  // TODO  if (!TMP) Error
            //  TMP_size = my_n*sizeof(int);
            //}
            //Rprintf("reorder o (direct=%d)...\n", direct);

            // TODO: doesn't need to happen if this skipped either
            #pragma omp parallel num_threads(getDTthreads())
            {
              int my_tmp_counts[256];
              #pragma omp for schedule(dynamic)
              for (int batch=0; batch<nBatch; batch++) {
                const int my_n = (batch==nBatch-1) ? lastBatchSize : batchSize;
                memcpy(my_tmp_counts, counts+batch*256, 256*sizeof(int));  // copy to cumulate since we need to reuse orginal to reorder other cols too.
                const uint8_t *restrict b = key[radix] + from + batch*batchSize;    // TODO: use restrict everywhere,  even with const too
                if (direct) {
                  // don't need to reorder o because o contains just 1:n. Saves allocating TMP with nrow(DT)*4 bytes and hopping wastefully via it.
                  int k = batch*batchSize + 1;  // +1 because returned to 1-based R
                  for (int i=0; i<my_n; i++) ansd[my_tmp_counts[*b++]++] = k++;
                } else {
                  int *k = ansd + from + batch*batchSize;
                  for (int i=0; i<my_n; i++) ((int *)TMP)[my_tmp_counts[*b++]++] = *k++;
                }
              }
            }
            if (!direct) memcpy(ansd+from, TMP, my_n*sizeof(int));

            int my_gs[256];
            for (int i=1; i<ngrp; i++) {
              my_gs[i-1] = counts[ugrp[i]] - counts[ugrp[i-1]];
            }
            my_gs[ngrp-1] = my_n-counts[ugrp[ngrp-1]];
            newpush(my_gs, ngrp);
            // memset(counts, 0, nBatch*256*sizeof(int));
            free(counts);
            // Rprintf("nested split into %d groups from=%d\n", ngrp, from);
          }
        }
      }
    }
  }
  /*Rprintf("After last radix ...\n");
  for (int i=0; i<n; i++) {
    Rprintf("%03d: ",i);
    for (int r=0; r<nradix; r++) Rprintf("%03d ", key[r][i]);
    Rprintf("\n");
  }*/

  omp_set_nested(0);
  free(TMP); TMP=NULL; // TMP_size=0; // TODO: place in cleanup()
  for (int i=0; i<nradix; i++) {free(key[i]); key[i]=NULL;}  // TODO: place in cleanup()
  /*
  #ifdef TIMING_ON
    for (i=0; i<NBLOCK; i++) {
      Rprintf("Timing block %d = %8.3f   %8d\n", i, 1.0*tblock[i]/CLOCKS_PER_SEC, nblock[i]);
      if (i==12) Rprintf("\n");
    }
    Rprintf("Found %d groups and maxgrpn=%d\n", gsngrp[flip], gsmax[flip]);
  #endif
  */
  if (all_skipped) {   // when data is already grouped or sorted, integer() returned with group sizes attached
    // the o vector created earlier could be avoided in this case if we only create it when isSorted becomes FALSE
    ans = PROTECT(allocVector(INTSXP, 0));  // Can't attach attributes to NULL
    n_protect++;
  }

  if (LOGICAL(retGrp)[0]) {
    SEXP tt;
    setAttrib(ans, sym_starts, tt = allocVector(INTSXP, gsngrp[flip]));
    int *ss = INTEGER(tt);
    int maxgrpn = 0;
    for (int i=0, tmp=1; i<gsngrp[flip]; i++) {
      int elem = gs[flip][i];
      if (elem>maxgrpn) maxgrpn=elem;
      ss[i]=tmp;
      tmp+=elem;
    }
    setAttrib(ans, sym_maxgrpn, ScalarInteger(maxgrpn));
  }

  cleanup();
  UNPROTECT(n_protect);
  return ans;
}

SEXP fsorted(SEXP x)
{
  // Just checks if ordered and returns FALSE early if not. Does not return ordering if so, unlike forder.
  // Always increasing order with NA's first
  // Similar to base:is.unsorted but accepts NA at the beginning (standard in data.table and considered sorted) rather than returning NA when NA present.
  // TODO: test in big steps first to return faster if unsortedness is at the end (a common case of rbind'ing data to end)
  // These are all sequential access to x, so very quick and cache efficient. Could be parallel by checking continuity at batch boundaries.
  const int n = length(x);
  if (n <= 1) return(ScalarLogical(TRUE));
  if (!isVectorAtomic(x)) Error("is.sorted (R level) and fsorted (C level) only to be used on vectors. If needed on a list/data.table, you'll need the order anyway if not sorted, so use if (length(o<-forder(...))) for efficiency in one step, or equivalent at C level");
  int i=1;
  switch(TYPEOF(x)) {
  case INTSXP : case LGLSXP : {
    int *xd = INTEGER(x);
    while (i<n && xd[i]>=xd[i-1]) i++;
  } break;
  case REALSXP :
    if (inherits(x,"integer64")) {
      int64_t *xd = (int64_t *)REAL(x);
      while (i<n && xd[i]>=xd[i-1]) i++;
    } else {
      double *xd = REAL(x);
      while (i<n && dtwiddle(xd,i)>=dtwiddle(xd,i-1)) i++;
    }
    break;
  case STRSXP : {
    SEXP *xd = STRING_PTR(x);
    i = 0;
    while (i<n && xd[i]==NA_STRING) i++;
    bool need = NEED2UTF8(xd[i]);
    i++; // pass over first non-NA_STRING
    while (i<n) {
      if (xd[i]==xd[i-1]) {i++; continue;}
      if (xd[i]==NA_STRING) break;
      if (!need) need = NEED2UTF8(xd[i]);
      if ((need ? strcmp(CHAR(ENC2UTF8(xd[i])), CHAR(ENC2UTF8(xd[i-1]))) :
                  strcmp(CHAR(xd[i]), CHAR(xd[i-1]))) < 0) break;
      i++;
    }
  } break;
  default :
    Error("type '%s' is not yet supported", type2char(TYPEOF(x)));
  }
  return ScalarLogical(i==n);
}

SEXP isOrderedSubset(SEXP x, SEXP nrow)
// specialized for use in [.data.table only
// Ignores 0s but heeds NAs and any out-of-range (which result in NA)
{
  int i=0, last, elem;
  if (!length(x)) return(ScalarLogical(TRUE));
  if (!isInteger(x)) error("x has non-0 length but isn't an integer vector");
  if (!isInteger(nrow) || LENGTH(nrow)!=1 || INTEGER(nrow)[0]<0) error("nrow must be integer vector length 1 and >=0");
  if (LENGTH(x)<=1) return(ScalarLogical(TRUE));
  while (i<LENGTH(x) && INTEGER(x)[i]==0) i++;
  if (i==LENGTH(x)) return(ScalarLogical(TRUE));
  last = INTEGER(x)[i];  // the first non-0
  i++;
  for (; i<LENGTH(x); i++) {
    elem = INTEGER(x)[i];
    if (elem == 0) continue;
    if (elem < last || elem < 0 || elem > INTEGER(nrow)[0])
      return(ScalarLogical(FALSE));
    last = elem;
  }
  return(ScalarLogical(TRUE));
}

SEXP isReallyReal(SEXP x) {
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = 0;
  // return 0 (FALSE) when not type double, or is type double but contains integers
  // used to error if not passed type double but this needed extra is.double() calls in calling R code
  // which needed a repeat of the argument. Hence simpler and more robust to return 0 when not type double.
  if (isReal(x)) {
    int n=length(x), i=0;
    while (i<n &&
        ( ISNA(REAL(x)[i]) ||
        ( R_FINITE(REAL(x)[i]) && REAL(x)[i] == (int)(REAL(x)[i])))) {
      i++;
    }
    if (i<n) INTEGER(ans)[0] = i+1;  // return the location of first element which is really real; i.e. not an integer
  }
  UNPROTECT(1);
  return(ans);
}

SEXP binary(SEXP x)
// base::intToBits is close, but why does that print the result as "00 00 00 00" (raw) rather than ("0000") bits? Seems odd.
{
  char buffer[69];
  int j;
  if (!isReal(x)) error("x must be type 'double'");
  SEXP ans = PROTECT(allocVector(STRSXP, LENGTH(x)));
  uint64_t *xd = (uint64_t *)REAL(x);
  for (int i=0; i<LENGTH(x); i++) {
    uint64_t i64 = xd[i];
    j = 0;
    for(int bit=64; bit>=1; bit--)
    {
       buffer[j++] = '0' + (i64 >> (bit-1) & 1);
       if (bit==64 || bit==53 || bit==17 || bit==9) buffer[j++]=' ';
       //       ^sign      ^exponent  ^last 2 byte rounding
    }
    SET_STRING_ELT(ans, i, mkCharLen(buffer,68));  // 64 bits + 4 spaces
  }
  UNPROTECT(1);
  return ans;
}

