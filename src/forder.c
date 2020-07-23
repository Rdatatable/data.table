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
    skewed groups are split in parallel
    finds unique bytes to save 256 sweeping
    skips already-grouped yet unsorted
    recursive group gathering for cache efficiency
    reuses R's global character cache (truelength on CHARSXP)
    compressed column can cross byte boundaries to use spare bits (e.g. 2 16-level columns in one byte)
    just the remaining part of key is reordered as the radix progresses
    columnar byte-key for within-radix MT cache efficiency

  Only forder() and dtwiddle() functions are meant for use by other C code in data.table, hence all other functions here are static.
  The coding techniques deployed here are for efficiency. The static functions are recursive or called repetitively and we wish to minimise
  overhead. They reach outside themselves to place results in the end result directly rather than returning many small pieces of memory.
*/

// #define TIMING_ON

static int nth = 1;                 // number of threads to use, throttled by default; used by cleanup() to ensure no mismatch in getDTthreads() calls
static bool retgrp = true;          // return group sizes as well as the ordering vector? If so then use gs, gsalloc and gsn :
static int nrow = 0;                // used as group size stack allocation limit (when all groups are 1 row)
static int *gs = NULL;              // gs = final groupsizes e.g. 23,12,87,2,1,34,...
static int gs_alloc = 0;            // allocated size of gs
static int gs_n = 0;                // the number of groups found so far (how much of the allocated gs is used)
static int **gs_thread=NULL;        // each thread has a private buffer which gets flushed to the final gs appropriately
static int *gs_thread_alloc=NULL;
static int *gs_thread_n=NULL;
static int *TMP=NULL;               // UINT16_MAX*sizeof(int) for each thread; used by counting sort in radix_r()
static uint8_t *UGRP=NULL;          // 256 bytes for each thread; used by counting sort in radix_r() when sortType==0 (byte appearance order)

static int  *cradix_counts = NULL;
static SEXP *cradix_xtmp   = NULL;
static SEXP *ustr = NULL;
static int ustr_alloc = 0;
static int ustr_n = 0;
static int ustr_maxlen = 0;
static int sortType = 0;             // 0 just group; -1 descending, +1 ascending
static int nalast = 0;               // 1 (true i.e. last), 0 (false i.e. first), -1 (na i.e. remove)
static int nradix = 0;
static uint8_t **key = NULL;
static int *anso = NULL;
static bool notFirst=false;

static char msg[1001];
#define STOP(...) do {snprintf(msg, 1000, __VA_ARGS__); cleanup(); error(msg);} while(0)      // http://gcc.gnu.org/onlinedocs/cpp/Swallowing-the-Semicolon.html#Swallowing-the-Semicolon
// use STOP in this file (not error()) to ensure cleanup() is called first
// snprintf to msg first in case nrow (just as an example) is provided in the message because cleanup() sets nrow to 0
#undef warning
#define warning(...) Do not use warning in this file                // since it can be turned to error via warn=2
/* Using OS realloc() in this file to benefit from (often) in-place realloc() to save copy
 * We have to trap on exit anyway to call savetl_end().
 * NB: R_alloc() would be more convenient (fails within) and robust (auto free) but there is no R_realloc(). Implementing R_realloc() would be an alloc and copy, iiuc.
 *     Calloc/Realloc needs to be Free'd, even before error() [R-exts$6.1.2]. An oom within Calloc causes a previous Calloc to leak so Calloc would still needs to be trapped anyway.
 * Therefore, using <<if (!malloc()) STOP(_("helpful context msg"))>> approach to cleanup() on error.
 */

static void free_ustr() {
  for(int i=0; i<ustr_n; i++)
    SET_TRUELENGTH(ustr[i],0);
  free(ustr); ustr=NULL;
  ustr_alloc=0; ustr_n=0; ustr_maxlen=0;
}

static void cleanup() {
  free(gs); gs=NULL;
  gs_alloc = 0;
  gs_n = 0;

  if (gs_thread!=NULL) for (int i=0; i<nth; i++) free(gs_thread[i]);
  free(gs_thread);       gs_thread=NULL;
  free(gs_thread_alloc); gs_thread_alloc=NULL;
  free(gs_thread_n);     gs_thread_n=NULL;

  free(TMP); TMP=NULL;
  free(UGRP); UGRP=NULL;

  nrow = 0;
  free(cradix_counts); cradix_counts=NULL;
  free(cradix_xtmp);   cradix_xtmp=NULL;
  free_ustr();
  if (key!=NULL) { int i=0; while (key[i]!=NULL) free(key[i++]); }  // ==nradix, other than rare cases e.g. tests 1844.5-6 (#3940), and if a calloc fails
  free(key); key=NULL; nradix=0;
  savetl_end();  // Restore R's own usage of tl. Must run after the for loop in free_ustr() since only CHARSXP which had tl>0 (R's usage) are stored there.
}

static void push(const int *x, const int n) {
  if (!retgrp) return;  // clearer to have the switch here rather than before each call
  int me = omp_get_thread_num();
  int newn = gs_thread_n[me] + n;
  if (gs_thread_alloc[me] < newn) {
    gs_thread_alloc[me] = (newn < nrow/3) ? (1+(newn*2)/4096)*4096 : nrow;  // [2|3] to not overflow and 3 not 2 to avoid allocating close to nrow (nrow groups occurs when all size 1 groups)
    gs_thread[me] = realloc(gs_thread[me], gs_thread_alloc[me]*sizeof(int));
    if (gs_thread[me]==NULL) STOP(_("Failed to realloc thread private group size buffer to %d*4bytes"), (int)gs_thread_alloc[me]);
  }
  memcpy(gs_thread[me]+gs_thread_n[me], x, n*sizeof(int));
  gs_thread_n[me] += n;
}

static void flush() {
  if (!retgrp) return;
  int me = omp_get_thread_num();
  int n = gs_thread_n[me];
  int newn = gs_n + n;
  if (gs_alloc < newn) {
    gs_alloc = (newn < nrow/3) ? (1+(newn*2)/4096)*4096 : nrow;
    gs = realloc(gs, gs_alloc*sizeof(int));
    if (gs==NULL) STOP(_("Failed to realloc group size result to %d*4bytes"), (int)gs_alloc);
  }
  memcpy(gs+gs_n, gs_thread[me], n*sizeof(int));
  gs_n += n;
  gs_thread_n[me] = 0;
}

#ifdef TIMING_ON
  #define NBLOCK 64
  #define MAX_NTH 256
  static double tblock[MAX_NTH*NBLOCK];
  static int nblock[MAX_NTH*NBLOCK];
  static uint64_t stat[257];
  #define TBEG() double tstart = wallclock();   // tstart declared locally for thread safety
  #define TEND(i) {  \
    double now = wallclock(); \
    int w = omp_get_thread_num()*NBLOCK + i; \
    tblock[w] += now-tstart; \
    nblock[w]++; \
    tstart = now; \
  }
#else
  #define TBEG()
  #define TEND(i)
#endif

// range_* functions return [min,max] of the non-NAs as common uint64_t type
// TODO parallelize these; not a priority according to TIMING_ON though (contiguous read with prefetch)

static void range_i32(const int32_t *x, const int n, uint64_t *out_min, uint64_t *out_max, int *out_na_count)
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

static void range_d(double *x, int n, uint64_t *out_min, uint64_t *out_max, int *out_na_count, int *out_infnan_count)
// return range of finite numbers (excluding NA, NaN, -Inf, +Inf), a count of NA and a count of Inf|-Inf|NaN
{
  uint64_t min=0, max=0;
  int na_count=0, infnan_count=0;
  int i=0;
  while(i<n && !R_FINITE(x[i])) { ISNA(x[i++]) ? na_count++ : infnan_count++; }
  if (i<n) { max = min = dtwiddle(x, i++);}
  for(; i<n; i++) {
    if (!R_FINITE(x[i])) { ISNA(x[i]) ? na_count++ : infnan_count++; continue; }
    uint64_t tmp = dtwiddle(x, i);
    if (tmp>max) max=tmp;
    else if (tmp<min) min=tmp;
  }
  *out_na_count = na_count;
  *out_infnan_count = infnan_count;
  *out_min = min;
  *out_max = max;
}

// non-critical function also used by bmerge and chmatch
int StrCmp(SEXP x, SEXP y)
{
  if (x == y) return 0;             // same cached pointer (including NA_STRING==NA_STRING)
  if (x == NA_STRING) return -1;    // x<y
  if (y == NA_STRING) return 1;     // x>y
  return strcmp(CHAR(ENC2UTF8(x)), CHAR(ENC2UTF8(y)));  // TODO: always calling ENC2UTF8 here could be expensive 
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
  if (thiscounts[0] != 0) STOP(_("Logical error. counts[0]=%d in cradix but should have been decremented to 0. radix=%d"), thiscounts[0], radix);
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
  if (!cradix_counts) STOP(_("Failed to alloc cradix_counts"));
  cradix_xtmp = (SEXP *)malloc(ustr_n*sizeof(SEXP));
  if (!cradix_xtmp) STOP(_("Failed to alloc cradix_tmp"));
  cradix_r(x, n, 0);
  free(cradix_counts); cradix_counts=NULL;
  free(cradix_xtmp);   cradix_xtmp=NULL;
}

static void range_str(SEXP *x, int n, uint64_t *out_min, uint64_t *out_max, int *out_na_count)
// group numbers are left in truelength to be fetched by WRITE_KEY
{
  int na_count=0;
  bool anyneedutf8=false;
  if (ustr_n!=0) STOP(_("Internal error: ustr isn't empty when starting range_str: ustr_n=%d, ustr_alloc=%d"), ustr_n, ustr_alloc);  // # nocov
  if (ustr_maxlen!=0) STOP(_("Internal error: ustr_maxlen isn't 0 when starting range_str"));  // # nocov
  // savetl_init() has already been called at the start of forder
  #pragma omp parallel for num_threads(getDTthreads(n, true))
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
        if (ustr==NULL) STOP(_("Unable to realloc %d * %d bytes in range_str"), ustr_alloc, (int)sizeof(SEXP));  // # nocov
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
    if (!ustr3) STOP(_("Failed to alloc ustr3 when converting strings to UTF8"));  // # nocov
    memcpy(ustr3, STRING_PTR(ustr2), ustr_n*sizeof(SEXP));
    // need to reset ustr_maxlen because we need ustr_maxlen for utf8 strings
    ustr_maxlen = 0;
    for (int i=0; i<ustr_n; i++) {
      SEXP s = ustr3[i];
      if (LENGTH(s)>ustr_maxlen) ustr_maxlen=LENGTH(s);
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
    if (!tl) STOP(_("Failed to alloc tl when converting strings to UTF8"));  // # nocov
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
    if (sortType) {
      // that this is always ascending; descending is done in WRITE_KEY using max-this
      cradix(ustr, ustr_n);  // sorts ustr in-place by reference. assumes NA_STRING not present.
      for(int i=0; i<ustr_n; i++)     // save ordering in the CHARSXP. negative so as to distinguish with R's own usage.
        SET_TRUELENGTH(ustr[i], -i-1);
    }
    // else group appearance order was already saved to tl in the first pass
  }
}

static int dround=0;      // No rounding by default, for now. Handles #1642, #1728, #1463, #485
static uint64_t dmask=0;

SEXP setNumericRounding(SEXP droundArg)
// init.c has initial call with default of 2
{
  if (!isInteger(droundArg) || LENGTH(droundArg)!=1) error(_("Must an integer or numeric vector length 1"));
  if (INTEGER(droundArg)[0] < 0 || INTEGER(droundArg)[0] > 2) error(_("Must be 2, 1 or 0"));
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
uint64_t dtwiddle(const void *p, int i)
{
  union {
    double d;
    uint64_t u64;
  } u;  // local for thread safety
  u.d = ((double *)p)[i];
  if (R_FINITE(u.d)) {
    if (u.d==0) u.d=0; // changes -0.0 to 0.0,  issue #743
    u.u64 ^= (u.u64 & 0x8000000000000000) ? 0xffffffffffffffff : 0x8000000000000000; // always flip sign bit and if negative (sign bit was set) flip other bits too
    u.u64 += (u.u64 & dmask) << 1/*is this shift really correct. No need to shift*/  ;   // when dround==1|2, if 8th|16th bit is set, round up before chopping last 1|2 bytes
    return u.u64 >> (dround*8);
  }
  if (ISNAN(u.d)) return ISNA(u.d)    ? 0 /*NA*/   : 1 /*NaN*/;  // also normalises a difference between NA on 32bit R (bit 13 set) and 64bit R (bit 13 not set)
  if (isinf(u.d)) return signbit(u.d) ? 2 /*-Inf*/ : (0xffffffffffffffff>>(dround*8)) /*+Inf*/;
  STOP(_("Unknown non-finite value; not NA, NaN, -Inf or +Inf"));  // # nocov
}

void radix_r(const int from, const int to, const int radix);

SEXP forder(SEXP DT, SEXP by, SEXP retGrpArg, SEXP sortGroupsArg, SEXP ascArg, SEXP naArg)
// sortGroups TRUE from setkey and regular forder, FALSE from by= for efficiency so strings don't have to be sorted and can be left in appearance order
// when sortGroups is TRUE, ascArg contains +1/-1 for ascending/descending of each by column; when FALSE ascArg is ignored
{

#ifdef TIMING_ON
  memset(tblock, 0, MAX_NTH*NBLOCK*sizeof(double));
  memset(nblock, 0, MAX_NTH*NBLOCK*sizeof(int));
  memset(stat,   0, 257*sizeof(uint64_t));
  TBEG()
#endif

  int n_protect = 0;
  const bool verbose = GetVerbose();

  if (!isNewList(DT)) {
    if (!isVectorAtomic(DT))
      STOP(_("Internal error: input is not either a list of columns, or an atomic vector."));  // # nocov; caught by colnamesInt at R level, test 1962.0472
    if (!isNull(by))
      STOP(_("Internal error: input is an atomic vector (not a list of columns) but by= is not NULL"));  // # nocov; caught at R level, test 1962.043
    if (!isInteger(ascArg) || LENGTH(ascArg)!=1)
      STOP(_("Input is an atomic vector (not a list of columns) but order= is not a length 1 integer"));
    if (verbose)
      Rprintf(_("forder.c received a vector type '%s' length %d\n"), type2char(TYPEOF(DT)), length(DT));
    SEXP tt = PROTECT(allocVector(VECSXP, 1)); n_protect++;
    SET_VECTOR_ELT(tt, 0, DT);
    DT = tt;
    by = PROTECT(allocVector(INTSXP, 1)); n_protect++;
    INTEGER(by)[0] = 1;
  } else {
    if (verbose)
      Rprintf(_("forder.c received %d rows and %d columns\n"), length(VECTOR_ELT(DT,0)), length(DT));
  }
  if (!length(DT))
    STOP(_("Internal error: DT is an empty list() of 0 columns"));  // # nocov  should have been caught be colnamesInt, test 2099.1
  if (!isInteger(by) || !LENGTH(by))
    STOP(_("Internal error: DT has %d columns but 'by' is either not integer or is length 0"), length(DT));  // # nocov  colnamesInt catches, 2099.2
  if (!isInteger(ascArg) || LENGTH(ascArg)!=LENGTH(by))
    STOP(_("Either order= is not integer or its length (%d) is different to by='s length (%d)"), LENGTH(ascArg), LENGTH(by));
  nrow = length(VECTOR_ELT(DT,0));
  int n_cplx = 0;
  for (int i=0; i<LENGTH(by); i++) {
    int by_i = INTEGER(by)[i];
    if (by_i < 1 || by_i > length(DT))
      STOP(_("internal error: 'by' value %d out of range [1,%d]"), by_i, length(DT)); // # nocov # R forderv already catch that using C colnamesInt
    if ( nrow != length(VECTOR_ELT(DT, by_i-1)) )
      STOP(_("Column %d is length %d which differs from length of column 1 (%d)\n"), INTEGER(by)[i], length(VECTOR_ELT(DT, INTEGER(by)[i]-1)), nrow);
    if (TYPEOF(VECTOR_ELT(DT, by_i-1)) == CPLXSXP) n_cplx++;
  }
  if (!isLogical(retGrpArg) || LENGTH(retGrpArg)!=1 || INTEGER(retGrpArg)[0]==NA_LOGICAL)
    STOP(_("retGrp must be TRUE or FALSE"));
  retgrp = LOGICAL(retGrpArg)[0]==TRUE;
  if (!isLogical(sortGroupsArg) || LENGTH(sortGroupsArg)!=1 || INTEGER(sortGroupsArg)[0]==NA_LOGICAL )
    STOP(_("sort must be TRUE or FALSE"));
  sortType = LOGICAL(sortGroupsArg)[0]==TRUE;   // if sortType is 1, it is later flipped between +1/-1 according to ascArg. Otherwise ascArg is ignored when sortType==0
  if (!retgrp && !sortType)
    STOP(_("At least one of retGrp= or sort= must be TRUE"));
  if (!isLogical(naArg) || LENGTH(naArg) != 1)
    STOP(_("na.last must be logical TRUE, FALSE or NA of length 1"));
  nalast = (LOGICAL(naArg)[0] == NA_LOGICAL) ? -1 : LOGICAL(naArg)[0]; // 1=na last, 0=na first (default), -1=remove na

  if (nrow==0) {
    // empty vector or 0-row DT is always sorted
    SEXP ans = PROTECT(allocVector(INTSXP, 0)); n_protect++;
    if (retgrp) {
      setAttrib(ans, sym_starts, allocVector(INTSXP, 0));
      setAttrib(ans, sym_maxgrpn, ScalarInteger(0));
    }
    UNPROTECT(n_protect);
    return ans;
  }
  // if n==1, the code is left to proceed below in case one or more of the 1-row by= columns are NA and na.last=NA. Otherwise it would be easy to return now.
  notFirst = false;

  SEXP ans = PROTECT(allocVector(INTSXP, nrow)); n_protect++;
  anso = INTEGER(ans);
  TEND(0)
  #pragma omp parallel for num_threads(getDTthreads(nrow, true))
  for (int i=0; i<nrow; i++) anso[i]=i+1;   // gdb 8.1.0.20180409-git very slow here, oddly
  TEND(1)
  savetl_init();   // from now on use Error not error

  int ncol=length(by);
  int keyAlloc = (ncol+n_cplx)*8 + 1;         // +1 for NULL to mark end; calloc to initialize with NULLs
  key = calloc(keyAlloc, sizeof(uint8_t *));  // needs to be before loop because part II relies on part I, column-by-column.
  if (!key)
    STOP("Unable to allocate %"PRId64" bytes of working memory", (uint64_t)keyAlloc*sizeof(uint8_t *));  // # nocov
  nradix=0; // the current byte we're writing this column to; might be squashing into it (spare>0)
  int spare=0;  // the amount of bits remaining on the right of the current nradix byte
  bool isReal=false;
  bool complexRerun = false;   // see comments below in CPLXSXP case
  SEXP CplxPart = R_NilValue;
  if (n_cplx) { CplxPart=PROTECT(allocVector(REALSXP, nrow)); n_protect++; } // one alloc is reused for each part
  TEND(2);
  for (int col=0; col<ncol; col++) {
    // Rprintf(_("Finding range of column %d ...\n"), col);
    SEXP x = VECTOR_ELT(DT,INTEGER(by)[col]-1);
    uint64_t min=0, max=0;     // min and max of non-NA finite values
    int na_count=0, infnan_count=0;
    if (sortType) {
      sortType=INTEGER(ascArg)[col];  // if sortType!=0 (not first-appearance) then +1/-1 comes from ascArg.
      if (sortType!=1 && sortType!=-1)
        STOP(_("Item %d of order (ascending/descending) is %d. Must be +1 or -1."), col+1, sortType);
    }
    //Rprintf(_("sortType = %d\n"), sortType);
    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP :  // TODO skip LGL and assume range [0,1]
      range_i32(INTEGER(x), nrow, &min, &max, &na_count);
      break;
    case CPLXSXP : {
      // treat as if two separate columns of double
      const Rcomplex *xd = COMPLEX(x);
      double *tmp = REAL(CplxPart);
      if (!complexRerun) {
        for (int i=0; i<nrow; ++i) tmp[i] = xd[i].r;  // extract the real part on the first time
        complexRerun = true;
        col--;  // cause this loop iteration to rerun; decrement now in case of early continue below
      } else {
        for (int i=0; i<nrow; ++i) tmp[i] = xd[i].i;
        complexRerun = false;
      }
      x = CplxPart;
    } // !no break! so as to fall through to REAL case
    case REALSXP :
      if (INHERITS(x, char_integer64)) {
        range_i64((int64_t *)REAL(x), nrow, &min, &max, &na_count);
      } else {
        if (verbose && INHERITS(x, char_Date) && INTEGER(isReallyReal(x))[0]==0) {
          Rprintf(_("\n*** Column %d passed to forder is a date stored as an 8 byte double but no fractions are present. Please consider a 4 byte integer date such as IDate to save space and time.\n"), col+1);
          // Note the (slightly expensive) isReallyReal will only run when verbose is true. Prefix '***' just to make it stand out in verbose output
          // In future this could be upgraded to option warning. But I figured that's what we use verbose to do (to trace problems and look for efficiencies).
          // If an automatic coerce is desired (see discussion in #1738) then this is the point to do that in this file. Move the INTSXP case above to be
          // next, do the coerce of Date to integer now to a tmp, and then let this case fall through to INTSXP in the same way as CPLXSXP falls through to REALSXP.
        }
        range_d(REAL(x), nrow, &min, &max, &na_count, &infnan_count);
        if (min==0 && na_count<nrow) { min=3; max=4; } // column contains no finite numbers and is not-all NA; create dummies to yield positive min-2 later
        isReal = true;
      }
      break;
    case STRSXP :
      // need2utf8 now happens inside range_str on the uniques
      range_str(STRING_PTR(x), nrow, &min, &max, &na_count);
      break;
    default:
      STOP(_("Column %d passed to [f]order is type '%s', not yet supported."), col+1, type2char(TYPEOF(x)));
    }
    TEND(3);
    if (na_count==nrow || (min>0 && min==max && na_count==0 && infnan_count==0)) {
      // all same value; skip column as nothing to do;  [min,max] is just of finite values (excludes +Inf,-Inf,NaN and NA)
      if (na_count==nrow && nalast==-1) { for (int i=0; i<nrow; i++) anso[i]=0; }
      if (TYPEOF(x)==STRSXP) free_ustr();
      continue;
    }

    uint64_t range = max-min+1 +1/*NA*/ +isReal*3/*NaN, -Inf, +Inf*/;
    // Rprintf(_("range=%"PRIu64"  min=%"PRIu64"  max=%"PRIu64"  na_count==%d\n"), range, min, max, na_count);

    int maxBit=0;
    while (range) { maxBit++; range>>=1; }
    int nbyte = 1+(maxBit-1)/8; // the number of bytes spanned by the value
    int firstBits = maxBit - (nbyte-1)*8;  // how many bits used in most significant byte
    if (spare==0) {
      spare = 8-firstBits; // left align to byte boundary to get better first split.
    } else {
      if (sortType==0) {
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
      if (key[nradix+b]==NULL) {
        uint8_t *tt = calloc(nrow, sizeof(uint8_t));  // 0 initialize so that NA's can just skip (NA is always the 0 offset)
        if (!tt)
          STOP("Unable to allocate %"PRIu64" bytes of working memory", (uint64_t)nrow*sizeof(uint8_t)); // # nocov
        key[nradix+b] = tt;
      }
    }

    const bool asc = (sortType>=0);
    uint64_t min2=min, max2=max;
    if (nalast<1 && asc) {
      min2--; // so that x-min results in 1 for the minimum; NA coded by 0. Always provide for the NA spot even if NAs aren't present for code brevity and robustness
      if (isReal) min2--;  // Nan first
    } else {
      max2++;            // NA is max+1 so max2-elem should result in 0
      if (isReal) max2++;  // Nan last
    }
    if (isReal) { min2--; max2++; }  // -Inf and +Inf  in min-1 and max+1 spots respectively

    const uint64_t naval = ((nalast==1) == asc) ? max+1+isReal*2 : min-1-isReal*2;
    const uint64_t nanval = ((nalast==1) == asc) ? max+2 : min-2;  // only used when isReal
    // Rprintf(_("asc=%d  min2=%"PRIu64"  max2=%"PRIu64"  naval==%"PRIu64"  nanval==%"PRIu64"\n"), asc, min2, max2, naval, nanval);

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
    //     Retaining appearance-order within key-byte-column is still advatangeous for when we do encounter column-grouped data (to avoid data movement); and
    //       the ordering will be appearance-order preserving in that case (just at the byte level).
    // TODO: in future we could provide an option to return 'any' group order for efficiency, which would be the byte-appearance order. But for now, the
    //     the forder afterwards on o__[f__] (in [.data.table) is not significant.

    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP : {
      int32_t *xd = INTEGER(x);
      #pragma omp parallel for num_threads(getDTthreads(nrow, true))
      for (int i=0; i<nrow; i++) {
        uint64_t elem=0;
        if (xd[i]==NA_INTEGER) {  // TODO: go branchless if na_count==0
          if (nalast==-1) anso[i]=0;
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
        #pragma omp parallel for num_threads(getDTthreads(nrow, true))
        for (int i=0; i<nrow; i++) {
          uint64_t elem=0;
          if (xd[i]==INT64_MIN) {
            if (nalast==-1) anso[i]=0;
            elem = naval;
          } else {
            elem = xd[i] ^ 0x8000000000000000u;
          }
          WRITE_KEY
        }
      } else {
        double *xd = REAL(x);     // TODO: revisit double compression (skip bytes/mult by 10,100 etc) as currently it's often 6-8 bytes even for 3.14,3.15
        #pragma omp parallel for num_threads(getDTthreads(nrow, true))
        for (int i=0; i<nrow; i++) {
          uint64_t elem=0;
          if (!R_FINITE(xd[i])) {
            if (isinf(xd[i])) elem = signbit(xd[i]) ? min-1 : max+1;
            else {
              if (nalast==-1) anso[i]=0;  // for both NA and NaN
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
      #pragma omp parallel for num_threads(getDTthreads(nrow, true))
      for (int i=0; i<nrow; i++) {
        uint64_t elem=0;
        if (xd[i]==NA_STRING) {
          if (nalast==-1) anso[i]=0;
          elem = naval;
        } else {
          elem = -TRUELENGTH(xd[i]);
        }
        WRITE_KEY
      }}
      free_ustr();  // ustr could be left allocated and reused, but free now in case large and we're tight on ram
      break;
    default:
       STOP(_("Internal error: column not supported, not caught earlier"));  // # nocov
    }
    nradix += nbyte-1+(spare==0);
    TEND(4)
    // Rprintf(_("Written key for column %d\n"), col);
  }
  if (key[nradix]!=NULL) nradix++;  // nradix now number of bytes in key
  #ifdef TIMING_ON
  Rprintf(_("nradix=%d\n"), nradix);
  #endif

  nth = getDTthreads(nrow, true);  // this nth is relied on in cleanup()
  TMP =  (int *)malloc(nth*UINT16_MAX*sizeof(int)); // used by counting sort (my_n<=65536) in radix_r()
  UGRP = (uint8_t *)malloc(nth*256);                // TODO: align TMP and UGRP to cache lines (and do the same for stack allocations too)
  if (!TMP || !UGRP /*|| TMP%64 || UGRP%64*/) STOP(_("Failed to allocate TMP or UGRP or they weren't cache line aligned: nth=%d"), nth);
  if (retgrp) {
    gs_thread = calloc(nth, sizeof(int *));     // thread private group size buffers
    gs_thread_alloc = calloc(nth, sizeof(int));
    gs_thread_n = calloc(nth, sizeof(int));
    if (!gs_thread || !gs_thread_alloc || !gs_thread_n) STOP(_("Could not allocate (very tiny) group size thread buffers"));
  }
  if (nradix) {
    radix_r(0, nrow-1, 0);  // top level recursive call: (from, to, radix)
  } else {
    push(&nrow, 1);
  }

  TEND(30)

  if (anso[0]==1 && anso[nrow-1]==nrow && (nrow<3 || anso[nrow/2]==nrow/2+1)) {
    // There used to be all_skipped shared bool. But even though it was safe to update this bool to false naked (without atomic protection) :
    // i) there were a lot of updates from deeply iterated insert, so there were a lot of writes to it and that bool likely sat on a shared cache line
    // ii) there were a lot of places in the code which needed to remember to set all_skipped properly. It's simpler code just to test now almost instantly.
    // Alternatively, we could try and avoid creating anso[] until it's needed, but that has similar complexity issues as (ii)
    // Note that if nalast==-1 (remove NA) anso will contain 0's for the NAs and will be considered not-sorted.
    bool stop = false;
    #pragma omp parallel for num_threads(getDTthreads(nrow, true))
    for (int i=0; i<nrow; i++) {
      if (stop) continue;
      if (anso[i]!=i+1) stop=true;
    }
    if (!stop) {
      // data is already grouped or sorted, integer() returned with group sizes attached
      ans = PROTECT(allocVector(INTSXP, 0));  // can't attach attributes to NULL, hence an empty integer()
      n_protect++;
    }
  }
  TEND(31)

  if (retgrp) {
    SEXP tt;
    int final_gs_n = (gs_n==0) ? gs_thread_n[0] : gs_n;   // TODO: find a neater way to do this
    int *final_gs  = (gs_n==0) ? gs_thread[0] : gs;
    setAttrib(ans, sym_starts, tt = allocVector(INTSXP, final_gs_n));
    int *ss = INTEGER(tt);
    int maxgrpn = 0;
    for (int i=0, tmp=1; i<final_gs_n; i++) {
      int elem = final_gs[i];
      if (elem>maxgrpn) maxgrpn=elem;
      ss[i]=tmp;
      tmp+=elem;
    }
    setAttrib(ans, sym_maxgrpn, ScalarInteger(maxgrpn));
  }

  cleanup();
  UNPROTECT(n_protect);
  TEND(32);
  #ifdef TIMING_ON
  {
    // first sum across threads
    for (int i=0; i<NBLOCK; i++) {
      for (int j=1; j<MAX_NTH; j++) {
        tblock[i] += tblock[j*NBLOCK + i];
        nblock[i] += nblock[j*NBLOCK + i];
      }
    }
    int last=NBLOCK-1;
    while (last>=0 && nblock[last]==0) last--; // remove unused timing slots
    for (int i=0; i<=last; i++) {
      Rprintf(_("Timing block %2d%s = %8.3f   %8d\n"), i, (i>=17&&i<=19)?"(*)":"   ", tblock[i], nblock[i]);
    }
    for (int i=0; i<=256; i++) {
      if (stat[i]) Rprintf(_("stat[%03d]==%20"PRIu64"\n"), i, (uint64_t)stat[i]);
    }
  }
  #endif
  return ans;
}

static bool sort_ugrp(uint8_t *x, const int n)
// x contains n unique bytes; sort them in-place using insert sort
// always ascending. desc and nalast are done in WRITE_KEY because columns may cross byte boundaries
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

void radix_r(const int from, const int to, const int radix) {
  TBEG();
  const int my_n = to-from+1;
  if (my_n==1) {  // minor TODO: batch up the 1's instead in caller (and that's only needed when retgrp anyway)
    push(&my_n, 1);
    TEND(5);
    return;
  }
  else if (my_n<=256) {
    // if nth==1
    // Rprintf(_("insert clause: radix=%d, my_n=%d, from=%d, to=%d\n"), radix, my_n, from, to);
    // insert sort with some twists:
    // i) detects if grouped; if sortType==0 can then skip
    // ii) keeps group appearance order at byte level to minimize movement
    #ifdef TIMING_ON
      // #pragma omp atomic   // turn on manually as the atomic affects timings
      // stat[my_n]++;
    #endif

    uint8_t *restrict my_key = key[radix]+from;  // safe to write as we don't use this radix again
    uint8_t o[my_n];
    // if last key (i.e. radix+1==nradix) there are no more keys to reorder so we could reorder osub by reference directly and save allocating and populating o just
    // to use it once. However, o's type is uint8_t so many moves within this max-256 vector should be faster than many moves in osub (4 byte or 8 byte ints) [1 byte
    // type is always aligned]
    bool skip = true;
    if (sortType!=0) {
      // always ascending as desc (sortType==-1) was dealt with in WRITE_KEY
      int start = 1;
      while (start<my_n && my_key[start]>=my_key[start-1]) start++;
      if (start<my_n) {
        skip = false;  // finding start is really just to take skip out of the loop below
        for (int i=0; i<start; i++) o[i]=i;  // always at least sets o[0]=0
        for (int i=start; i<my_n; i++) {
          uint8_t ktmp = my_key[i];
          int j=i-1;
          while (j>=0 && ktmp<my_key[j]) {
            my_key[j+1] = my_key[j];
            o[j+1] = o[j];
            j--;
          }
          my_key[j+1] = ktmp;  // redundant write when while() did nothing, but that's unlikely given the common case of pre-ordered is handled by skip==true
          o[j+1] = i;          // important to initialize o[] even when while() did nothing.
        }
      }
      TEND(6)
    } else {
      // sortType==0; retain group appearance order to hopefully benefit from skip, but there is a still a sort afterwards to get back to appearance order
      // because columns are split into multiple bytes: retaining the byte appearance order within key-byte is not the same as retaining the order of
      // unique values within each by= column.  In other words, retaining group order at byte level here does not affect correctness, just efficiency.
      int second = 1;
      while (second<my_n && my_key[second]==my_key[0]) second++;  // look for the second different byte value
      int third = second+1;
      while (third<my_n && my_key[third]==my_key[second]) third++;  // look for the last of the second value (which might be a repeat of the first)
      if (third<my_n) {
        // it's not either i) all one value (xxxxx), or ii) two values (xxyyy). It could be xxyyyx.. or xxyyyz..  It's likely worth allocating seen[] now.
        bool seen[256]={false};
        seen[my_key[0]] = seen[my_key[second]] = true;  // first two different bytes have been seen
        for(; third<my_n; third++) {
          uint8_t ktmp = my_key[third];
          if (ktmp==my_key[third-1]) continue;
          if (!seen[ktmp]) { seen[ktmp]=true; continue; }
          // else different byte but we've seen it before, so this my_n is not grouped (e.g. xxyyyx)
          skip = false;
          break;
        }
        if (!skip) {
          // and now it's worth populating o[]
          for (int i=0; i<third; i++) o[i] = i;
          for (int i=third; i<my_n; i++) {
            uint8_t ktmp = my_key[i];
            if (!seen[ktmp]) { seen[ktmp]=true; o[i]=i; continue; }
            // move this byte that we've seen before back to just after the last one to group them
            int j = i-1;
            while (j>=0 && ktmp!=my_key[j]) {
              my_key[j+1] = my_key[j];
              o[j+1] = o[j];
              j--;
            }
            my_key[j+1] = ktmp;   // redundant write if my_key[i]==my_key[i-1] and while() did nothing (but that's ok as that's at least some writing since skip==false)
            o[j+1] = i;           // important to initialize o[] even if while() did nothing
          }
        }
      }
      TEND(7)
    }
    if (!skip) {
      // reorder osub and each remaining ksub
      int TMP[my_n];  // on stack fine since my_n is very small (<=256)
      const int *restrict osub = anso+from;
      for (int i=0; i<my_n; i++) TMP[i] = osub[o[i]];
      memcpy((int *restrict)(anso+from), TMP, my_n*sizeof(int));
      for (int r=radix+1; r<nradix; r++) {
        const uint8_t *restrict ksub = key[r]+from;
        for (int i=0; i<my_n; i++) ((uint8_t *)TMP)[i] = ksub[o[i]];
        memcpy((uint8_t *restrict)(key[r]+from), (uint8_t *)TMP, my_n);
      }
      TEND(8)
    }
    // my_key is now grouped (and sorted by group too if sort!=0)
    // all we have left to do is find the group sizes and either recurse or push
    if (radix+1==nradix && !retgrp) {
      return;
    }
    int ngrp=0, my_gs[my_n];  //minor TODO: could know number of groups with certainty up above
    my_gs[ngrp]=1;
    for (int i=1; i<my_n; i++) {
      if (my_key[i]!=my_key[i-1]) my_gs[++ngrp] = 1;
      else my_gs[ngrp]++;
    }
    ngrp++;
    TEND(9)
    if (radix+1==nradix || ngrp==my_n) {  // ngrp==my_n => unique groups all size 1 and we can stop recursing now
      push(my_gs, ngrp);
    } else {
      for (int i=0, f=from; i<ngrp; i++) {
        radix_r(f, f+my_gs[i]-1, radix+1);
        f+=my_gs[i];
      }
    }
    return;
  }
  else if (my_n<=UINT16_MAX) {    // UINT16_MAX==65535 (important not 65536)
    // if (nth==1) Rprintf(_("counting clause: radix=%d, my_n=%d\n"), radix, my_n);
    uint16_t my_counts[256] = {0};  // Needs to be all-0 on entry. This ={0} initialization should be fast as it's on stack. Otherwise, we have to manage
                                    // a stack of counts anyway since this is called recursively and these counts are needed to make the recursive calls.
                                    // This thread-private stack alloc has no chance of false sharing and gives omp and compiler best chance.
    uint8_t *restrict my_ugrp = UGRP + omp_get_thread_num()*256;  // uninitialized is fine; will use the first ngrp items. Only used if sortType==0
    // TODO: ensure my_counts, my_grp and my_tmp below are cache line aligned on both Linux and Windows.
    const uint8_t *restrict my_key = key[radix]+from;
    int ngrp = 0;          // number of groups (items in ugrp[]). Max value 256 but could be uint8_t later perhaps if 0 is understood as 1.
    bool skip = true;      // i) if already _grouped_ and sortType==0 then caller can skip, ii) if already _grouped and sorted__ when sort!=0 then caller can skip too
    TEND(10)
    if (sortType!=0) {
      // Always ascending sort here (even when sortType==-1) because descending was prepared in write_key
      for (int i=0; i<my_n; ++i) my_counts[my_key[i]]++;  // minimal branch-free loop first, #3647
      for (int i=1; i<my_n; ++i) {
        if (my_key[i]<my_key[i-1]) { skip=false; break; }
        // stop early as soon as not-ordered is detected; likely quickly when it isn't sorted
        // otherwise, it's worth checking if it is ordered because skip saves time later
      }
      TEND(11)
    } else {
      for (int i=0; i<my_n; i++) {
        uint8_t elem = my_key[i];
        if (++my_counts[elem]==1) {
          // first time seen this value.  i==0 always does this branch
          my_ugrp[ngrp++]=elem;
        } else if (skip && elem!=my_key[i-1]) {   // does not happen for i==0
          // seen this value before and it isn't the previous value, so data is not grouped
          // including "skip &&" first is to avoid the != comparison
          skip=false;
        }
      }
      TEND(12)
    }
    if (!skip) {
      // reorder anso and remaining radix keys

      // avoid allocating and populating order vector (my_n long); we just use counts several times to push rather than pull
      // with contiguous-read from osub and ksub, 256 write live cache-lines is worst case. However, often there are many fewer ugrp and only that number of
      // write cache lines will be active. These write-cache lines will be constrained within the UINT16_MAX width, so should be close by in cache, too.
      // If there is a good degree of grouping, there contiguous-read/write both ways happens automatically in this approach.

      // cumulate; for forwards-assign to give cpu prefetch best chance (cpu may not support prefetch backwards).
      uint16_t my_starts[256], my_starts_copy[256];
      // TODO: could be allocated up front (like my_TMP below), or are they better on stack like this? TODO: allocating up front would provide to cache-align them.
      if (sortType!=0) {
        for (int i=0, sum=0; i<256; i++) { int tmp=my_counts[i]; my_starts[i]=my_starts_copy[i]=sum; sum+=tmp; ngrp+=(tmp>0);}  // cumulate through 0's too (won't be used)
      } else {
        for (int i=0, sum=0; i<ngrp; i++) { uint8_t w=my_ugrp[i]; int tmp=my_counts[w]; my_starts[w]=my_starts_copy[w]=sum; sum+=tmp; }  // cumulate in ugrp appearance order
      }

      int *restrict my_TMP = TMP + omp_get_thread_num()*UINT16_MAX; // Allocated up front to save malloc calls which i) block internally and ii) could fail
      if (radix==0 && nalast!=-1) {
        // anso contains 1:n so skip reading and copying it. Only happens when nrow<65535. Saving worth the branch (untested) when user repeatedly calls a small-n small-cardinality order.
        for (int i=0; i<my_n; i++) anso[my_starts[my_key[i]]++] = i+1;  // +1 as R is 1-based.
        // The loop counter could be uint_fast16_t since max i here will be UINT16_MAX-1 (65534), hence ++ after last iteration won't overflow 16bits. However, have chosen signed
        // integer for counters for now, as signed probably very slightly faster than unsigned on most platforms from what I can gather.
      } else {
        const int *restrict osub = anso+from;
        for (int i=0; i<my_n; i++) my_TMP[my_starts[my_key[i]]++] = osub[i];
        memcpy(anso+from, my_TMP, my_n*sizeof(int));
      }
      TEND(13)

      // reorder remaining key columns (radix+1 onwards).   This could be done in one-step too (a single pass through x[],  with a larger TMP
      //    that's how its done in the batched approach below.  Which is better?  The way here is multiple (but contiguous) passes through (one-byte) my_key
      if (radix+1<nradix) {
        for (int r=radix+1; r<nradix; r++) {
          memcpy(my_starts, my_starts_copy, 256*sizeof(uint16_t));  // restore starting offsets
          //for (int i=0,last=0; i<256; i++) { int tmp=my_counts[i]; if (tmp==0) continue; my_counts[i]=last; last=tmp; }  // rewind ++'s to offsets
          const uint8_t *restrict ksub = key[r]+from;
          for (int i=0; i<my_n; i++) ((uint8_t *)my_TMP)[my_starts[my_key[i]]++] = ksub[i];
          memcpy(key[r]+from, my_TMP, my_n);
        }
        TEND(14)
      }
    }

    if (!retgrp && radix+1==nradix) {
      return;  // we're done. avoid allocating and populating very last group sizes for last key
    }
    int my_gs[ngrp==0 ? 256 : ngrp];  // ngrp==0 when sort and skip==true; we didn't count the non-zeros in my_counts yet in that case
    if (sortType!=0) {
      ngrp=0;
      for (int i=0; i<256; i++) if (my_counts[i]) my_gs[ngrp++]=my_counts[i];  // this casts from uint16_t to int32, too
    } else {
      for (int i=0; i<ngrp; i++) my_gs[i]=my_counts[my_ugrp[i]];
    }
    TEND(15)
    if (radix+1==nradix) {
      // aside: cannot be all size 1 (a saving used in my_n<=256 case above) because my_n>256 and ngrp<=256
      push(my_gs, ngrp);
    } else {
      // this single thread will now descend and resolve all groups, now that the groups are close in cache
      for (int i=0, my_from=from; i<ngrp; i++) {
        radix_r(my_from, my_from+my_gs[i]-1, radix+1);
        my_from+=my_gs[i];
      }
    }
    return;
  }
  // else parallel batches. This is called recursively but only once or maybe twice before resolving to UINT16_MAX branch above

  int batchSize = MIN(UINT16_MAX, 1+my_n/getDTthreads(my_n, true));  // (my_n-1)/nBatch + 1;   //UINT16_MAX == 65535
  int nBatch = (my_n-1)/batchSize + 1;   // TODO: make nBatch a multiple of nThreads?
  int lastBatchSize = my_n - (nBatch-1)*batchSize;
  uint16_t *counts = calloc(nBatch*256,sizeof(uint16_t));
  uint8_t  *ugrps =  malloc(nBatch*256*sizeof(uint8_t));
  int      *ngrps =  calloc(nBatch    ,sizeof(int));
  if (!counts || !ugrps || !ngrps) STOP(_("Failed to allocate parallel counts. my_n=%d, nBatch=%d"), my_n, nBatch);

  bool skip=true;
  const int n_rem = nradix-radix-1;   // how many radix are remaining after this one
  TEND(16)
  #pragma omp parallel num_threads(getDTthreads(nBatch, false))
  {
    int     *my_otmp = malloc(batchSize * sizeof(int)); // thread-private write
    uint8_t *my_ktmp = malloc(batchSize * sizeof(uint8_t) * n_rem);
    // TODO: move these up above and point restrict[me] to them. Easier to Error that way if failed to alloc.
    #pragma omp for
    for (int batch=0; batch<nBatch; batch++) {
      const int my_n = (batch==nBatch-1) ? lastBatchSize : batchSize;  // lastBatchSize == batchSize when my_n is a multiple of batchSize
      const int my_from = from + batch*batchSize;
      uint16_t *restrict      my_counts = counts + batch*256;
      uint8_t  *restrict      my_ugrp   = ugrps  + batch*256;
      int                     my_ngrp   = 0;
      bool                    my_skip   = true;
      const uint8_t *restrict my_key    = key[radix] + my_from;
      const uint8_t *restrict byte = my_key;
      for (int i=0; i<my_n; i++, byte++) {
        if (++my_counts[*byte]==1) {   // always true first time when i==0
          my_ugrp[my_ngrp++] = *byte;
        } else if (my_skip && byte[0]!=byte[-1]) {   // include 'my_skip &&' to save != comparison after it's realized this batch is not grouped
          my_skip=false;
        }
      }
      ngrps[batch] = my_ngrp;  // write once to this shared cache line
      if (!my_skip) {
        skip = false;          // naked write to this shared byte is ok because false is only value written
        // gather this batch's anso and remaining keys. If we sorting too, urgrp is sorted later for that. Here we want to benefit from skip within batch
        // as much as possible which is a good chance since batchSize is relatively small (65535)
        for (int i=0, sum=0; i<my_ngrp; i++) { int tmp = my_counts[my_ugrp[i]]; my_counts[my_ugrp[i]]=sum; sum+=tmp; } // cumulate counts of this batch
        const int *restrict osub = anso+my_from;
        byte = my_key;
        for (int i=0; i<my_n; i++, byte++) {
          int dest = my_counts[*byte]++;
          my_otmp[dest] = *osub++;  // wastefully copies out 1:n when radix==0, but do not optimize as unlikely worth code complexity. my_otmp is not large, for example. Use first TEND() to decide.
          for (int r=0; r<n_rem; r++) my_ktmp[r*my_n + dest] = key[radix+1+r][my_from+i];   // reorder remaining keys
        }
        // or could do multiple passes through my_key like in the my_n<=65535 approach above. Test which is better depending on if TEND() points here.

        // we haven't completed all batches, so we don't know where these groups should place yet
        // So for now we write the thread-private small now-grouped buffers back in-place. The counts and groups across all batches will be used below to move these blocks.
        memcpy(anso+my_from, my_otmp, my_n*sizeof(int));
        for (int r=0; r<n_rem; r++) memcpy(key[radix+1+r]+my_from, my_ktmp+r*my_n, my_n*sizeof(uint8_t));

        // revert cumulate back to counts ready for vertical cumulate
        for (int i=0, last=0; i<my_ngrp; i++) { int tmp = my_counts[my_ugrp[i]]; my_counts[my_ugrp[i]]-=last; last=tmp; }
      }
    }
    free(my_otmp);
    free(my_ktmp);
  }
  TEND(17 + notFirst*3)  // 3 timings in this section: 17,18,19 first main split; 20,21,22 thereon

  // If my_n input is grouped and ugrp is sorted too (to illustrate), status now would be :
  // counts:                 ugrps:   ngrps:
  // 1: 20 18  2  0  0  0    0 1 2    3
  // 2:  0  0 17 21  5  0    2 3 4    3
  // 3:  0  0  0  0 15 19    4 5      2
  // If the keys within each and every batch were grouped, skip will be true.
  // Now we test if groups occurred in order across batches (like illustration above), and if not set skip=false

  uint8_t ugrp[256];  // head(ugrp,ngrp) will contain the unique values in appearance order
  bool    seen[256];  // is the value present in ugrp already
  int     ngrp=0;     // max value 256 so not uint8_t
  uint8_t last_seen=0;  // the last grp seen in the previous batch.  initialized 0 is not used
  for (int i=0; i<256; i++) seen[i]=false;
  for (int batch=0; batch<nBatch; batch++) {
    const uint8_t *restrict my_ugrp = ugrps + batch*256;
    if (ngrp==256 && !skip) break;  // no need to carry on
    for (int i=0; i<ngrps[batch]; i++) {
      if (!seen[my_ugrp[i]]) {
        seen[my_ugrp[i]] = true;
        ugrp[ngrp++] = last_seen = my_ugrp[i];
      } else if (skip && my_ugrp[i]!=last_seen) {   // ==last_seen would occur accross batch boundaries, like 2=>17 and 5=>15 in illustration above
        skip=false;
      }
    }
  }

  // If skip==true (my_n was pre-grouped) and
  // i) sortType==0 (not sorting groups) then osub and ksub don't need reordering. ugrp may happen to be sorted too but nothing special to do in that case.
  // ii) sortType==1|-1 and ugrp is already sorted then osub and ksub don't need reordering either.

  if (sortType!=0 && !sort_ugrp(ugrp, ngrp))
    skip=false;

  // now cumulate counts vertically to see where the blocks in the batches should be placed in the result across all batches
  // the counts are uint16_t but the cumulate needs to be int32_t (or int64_t in future) to hold the offsets
  // If skip==true and we're already done, we still need the first row of this cummulate (diff to get total group sizes) to push() or recurse below

  int *starts = calloc(nBatch*256, sizeof(int));  // keep starts the same shape and ugrp order as counts
  for (int j=0, sum=0; j<ngrp; j++) {  // iterate through columns (ngrp bytes)
    uint16_t *tmp1 = counts+ugrp[j];
    int      *tmp2 = starts+ugrp[j];
    for (int batch=0; batch<nBatch; batch++) {
      *tmp2 = sum;
      tmp2 += 256;
      sum += *tmp1;
      tmp1 += 256;
    }
  }
  // the first row now (when diff'd) now contains the size of each group across all batches

  TEND(18 + notFirst*3)
  if (!skip) {
    int *TMP = malloc(my_n * sizeof(int));
    if (!TMP) STOP(_("Unable to allocate TMP for my_n=%d items in parallel batch counting"), my_n);
    #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
    for (int batch=0; batch<nBatch; batch++) {
      const int *restrict      my_starts = starts + batch*256;
      const uint16_t *restrict my_counts = counts + batch*256;
      const int *restrict      osub = anso + from + batch*batchSize;  // the groups sit here contiguously
      const uint8_t *restrict  byte = ugrps + batch*256;              // in appearance order always logged here in ugrps
      const int                my_ngrp = ngrps[batch];
      for (int i=0; i<my_ngrp; i++, byte++) {
        const uint16_t len = my_counts[*byte];
        memcpy(TMP+my_starts[*byte], osub, len*sizeof(int));
        osub += len;
      }
    }
    memcpy(anso+from, TMP, my_n*sizeof(int));

    for (int r=0; r<n_rem; r++) {    // TODO: groups of sizeof(anso)  4 byte int currently  (in future 8).  To save team startup cost (but unlikely significant anyway)
      #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
      for (int batch=0; batch<nBatch; batch++) {
        const int *restrict      my_starts = starts + batch*256;
        const uint16_t *restrict my_counts = counts + batch*256;
        const uint8_t *restrict  ksub = key[radix+1+r] + from + batch*batchSize;  // the groups sit here contiguosly
        const uint8_t *restrict  byte = ugrps + batch*256;                        // in appearance order always logged here in ugrps
        const int                my_ngrp = ngrps[batch];
        for (int i=0; i<my_ngrp; i++, byte++) {
          const uint16_t len = my_counts[*byte];
          memcpy((uint8_t *)TMP + my_starts[*byte], ksub, len);
          ksub += len;
        }
      }
      memcpy(key[radix+1+r]+from, (uint8_t *)TMP, my_n);
    }
    free(TMP);
  }
  TEND(19 + notFirst*3)
  notFirst = true;

  int my_gs[ngrp];
  for (int i=1; i<ngrp; i++) my_gs[i-1] = starts[ugrp[i]] - starts[ugrp[i-1]];   // use the first row of starts to get totals
  my_gs[ngrp-1] = my_n - starts[ugrp[ngrp-1]];

  if (radix+1==nradix) {
    // aside: ngrp==my_n (all size 1 groups) isn't a possible short-circuit here similar to my_n>256 case above, my_n>65535 but ngrp<=256
    push(my_gs, ngrp);
    TEND(23)
  }
  else {
    // TODO: explicitly repeat parallel batch for any skew bins
    bool anyBig = false;
    for (int i=0; i<ngrp; i++) if (my_gs[i]>UINT16_MAX) { anyBig=true; break; }
    if (anyBig) {
      // Likely that they're all big. If so, each will go one-by-one and each will be parallel (case my_n>65535)
      // If only one or a few are big, then we have skew. The not-big ones will be <65535 and will be very
      // fast to run anyway relative to the big one(s) and the big one(s) will go in parallel. Hence dealing
      // with skew.
      // We could try and separate the not-big ones and do those in parallel first.  However, it's critical
      // that the groups are flushed in order otherwise the group size final result won't be correct (i.e. it
      // won't be aligned with anso). This consideration is independent of maintaining group appearance order.
      // When we have skew, by design it's a special type of limited skew in that the small ones are fixed to
      // to be small at <65535 (and hence fast anyway). In contract when they are all 'big' (>65535) but there
      // is skew in that one or a few are significantly bigger than the others, then they'll all go one-by-one
      // each in parallel here and they're all dealt with in parallel. There is no nestedness here.
      for (int i=0; i<ngrp; i++) {
        int start = from + starts[ugrp[i]];
        radix_r(start, start+my_gs[i]-1, radix+1);
        flush();
      }
      TEND(24)
    } else {
      // all groups are <=65535 and radix_r() will handle each one single-threaded. Therefore, this time
      // it does make sense to start a parallel team and there will be no nestedness here either.
      if (retgrp) {
        #pragma omp parallel for ordered schedule(dynamic) num_threads(getDTthreads(ngrp, false))
        for (int i=0; i<ngrp; i++) {
          int start = from + starts[ugrp[i]];
          radix_r(start, start+my_gs[i]-1, radix+1);
          #pragma omp ordered
          flush();
        }
      } else {
        // flush() is only relevant when retgrp==true so save the redundant ordered clause
        #pragma omp parallel for schedule(dynamic) num_threads(getDTthreads(ngrp, false))
        for (int i=0; i<ngrp; i++) {
          int start = from + starts[ugrp[i]];
          radix_r(start, start+my_gs[i]-1, radix+1);
        }
      }
      TEND(25)
    }
  }
  free(counts);
  free(starts);
  free(ugrps);
  free(ngrps);
  TEND(26)
}


SEXP issorted(SEXP x, SEXP by)
{
  // Just checks if ordered and returns FALSE early if not. Does not return ordering if so, unlike forder.
  // Always increasing order with NA's first
  // Similar to base:is.unsorted but accepts NA at the beginning (standard in data.table and considered sorted) rather than
  // returning NA when NA present, and is multi-column.
  // TODO: test in big steps first to return faster if unsortedness is at the end (a common case of rbind'ing data to end)
  // These are all sequential access to x, so quick and cache efficient. Could be parallel by checking continuity at batch boundaries.
  
  if (!isNull(by) && !isInteger(by)) STOP(_("Internal error: issorted 'by' must be NULL or integer vector"));
  if (isVectorAtomic(x) || length(by)==1) {
    // one-column special case is very common so specialize it by avoiding column-type switches inside the row-loop later
    if (length(by)==1) {
      if (INTEGER(by)[0]<1 || INTEGER(by)[0]>length(x)) STOP(_("issorted 'by' [%d] out of range [1,%d]"), INTEGER(by)[0], length(x));
      x = VECTOR_ELT(x, INTEGER(by)[0]-1);
    }
    const int n = length(x);
    if (n <= 1) return(ScalarLogical(TRUE));
    if (!isVectorAtomic(x)) STOP(_("is.sorted does not work on list columns"));
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
        while (i<n && dtwiddle(xd,i)>=dtwiddle(xd,i-1)) i++;  // TODO: change to loop over any NA or -Inf at the beginning and then proceed without dtwiddle() (but rounding)
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
      STOP(_("type '%s' is not yet supported"), type2char(TYPEOF(x)));
    }
    return ScalarLogical(i==n);
  }
  const int ncol = length(by);
  const R_xlen_t nrow = xlength(VECTOR_ELT(x,0));
  // ncol>1
  // pre-save lookups to save deep switch later for each column type
  size_t *sizes =          (size_t *)R_alloc(ncol, sizeof(size_t));
  const char **ptrs = (const char **)R_alloc(ncol, sizeof(char *));
  int *types =                (int *)R_alloc(ncol, sizeof(int));
  for (int j=0; j<ncol; ++j) {
    int c = INTEGER(by)[j];
    if (c<1 || c>length(x)) STOP(_("issorted 'by' [%d] out of range [1,%d]"), c, length(x));
    SEXP col = VECTOR_ELT(x, c-1);
    sizes[j] = SIZEOF(col);
    switch(TYPEOF(col)) {
    case INTSXP: case LGLSXP:
      types[j] = 0;
      ptrs[j] = (const char *)INTEGER(col);
      break;
    case REALSXP:
      types[j] = inherits(col, "integer64") ? 2 : 1;
      ptrs[j] = (const char *)REAL(col);
      break;
    case STRSXP:
      types[j] = 3;
      ptrs[j] = (const char *)STRING_PTR(col);
      break;
    default:
      STOP(_("type '%s' is not yet supported"), type2char(TYPEOF(col)));  // # nocov
    }
  }
  for (R_xlen_t i=1; i<nrow; ++i) {
    int j = -1;
    while (++j<ncol) {
      size_t size = sizes[j];
      const char *colp = ptrs[j] + size*i;
      if (memcmp(colp, colp-size, size)==0) continue;  // in all-but-last column, we see many repeats so we can save the switch for those
      bool ok = false;
      switch (types[j]) {
      case 0 : {   // INTSXP, LGLSXP
        const int *p = (const int *)colp;
        ok = p[0]>p[-1];
      } break;
      case 1: {   // regular double in REALSXP
        const double *p = (const double *)colp;
        ok = dtwiddle(p,0)>dtwiddle(p,-1);  // TODO: avoid dtwiddle by looping over any NA at the beginning, and remove NumericRounding.
      } break;
      case 2: {  // integer64 in REALSXP
        const int64_t *p = (const int64_t *)colp;
        ok = p[0]>p[-1];
      } break;
      case 3 : { // STRSXP
        const SEXP *p = (const SEXP *)colp;
        if (*p==NA_STRING) {
          ok = false; // previous value not NA (otherwise memcmp would have returned equal above) so can't be ordered
        } else {
          ok = (NEED2UTF8(p[0]) || NEED2UTF8(p[-1]) ?  // TODO: provide user option to choose ascii-only mode
                strcmp(CHAR(ENC2UTF8(p[0])), CHAR(ENC2UTF8(p[-1]))) :
                strcmp(CHAR(p[0]), CHAR(p[-1]))) >= 0;
        }
      } break;
      default :
        STOP(_("type '%s' is not yet supported"), type2char(TYPEOF(x)));  // # nocov
      }
      if (!ok) return ScalarLogical(FALSE);  // not sorted so return early
      break; // this item is greater than previous in this column so ignore any remaining columns on this row
    }
  }
  return ScalarLogical(TRUE);
}

SEXP isOrderedSubset(SEXP x, SEXP nrowArg)
// specialized for use in [.data.table only
// Ignores 0s but heeds NAs and any out-of-range (which result in NA)
{
  if (!isNull(x) && !isInteger(x)) error(_("x must be either NULL or an integer vector"));
  if (length(x)<=1) return(ScalarLogical(TRUE));  // a single NA when length(x)==1 is ordered (e.g. tests 128 & 130) otherwise anyNA => FALSE
  if (!isInteger(nrowArg) || LENGTH(nrowArg)!=1) error(_("nrow must be integer vector length 1"));
  const int nrow = INTEGER(nrowArg)[0];
  if (nrow<0) error(_("nrow==%d but must be >=0"), nrow);
  const int *xd = INTEGER(x), xlen=LENGTH(x);
  for (int i=0, last=INT_MIN; i<xlen; ++i) {
    int elem = xd[i];
    if (elem==0) continue;
    if (elem<last || elem<0/*includes NA==INT_MIN*/ || elem>nrow)
      return(ScalarLogical(FALSE));
    last = elem;
  }
  return(ScalarLogical(TRUE));
}

SEXP binary(SEXP x)
// base::intToBits is close, but why does that print the result as "00 00 00 00" (raw) rather than ("0000") bits? Seems odd.
{
  char buffer[69];
  int j;
  if (!isReal(x)) error(_("x must be type 'double'"));
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

