#include "data.table.h"
#include <alloca.h>
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

#define TIMING_ON
static int *gs = NULL;          // gs = groupsizes e.g. 23,12,87,2,1,34,...
//static int flip = 0;                 // two vectors flip flopped: flip and 1-flip
static int gsalloc = 0;         // allocated stack size
static int gsngrp = 0;          // used
static int gsmaxalloc = 0;           // max size of stack, set by forder to nrows
static int *anchors = NULL;
static int anchor_alloc = 0;
static int n_anchor = 0;
static bool retgrp = true;           // return group sizes as well as the ordering vector?
static int  *cradix_counts = NULL;
static SEXP *cradix_xtmp   = NULL;
static SEXP *ustr = NULL;
static int ustr_alloc = 0;
static int ustr_n = 0;
static int ustr_maxlen = 0;
static int sort = 0;                 // 0 no sort; -1 descending, +1 ascending
static bool all_skipped = true;
//static int *OTMP = NULL;
//static uint8_t *KTMP = NULL;
static int nradix = 0;
static uint8_t **key = NULL;
static int *anso = NULL;
static bool notFirst=false;

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
  free(gs); gs=NULL;
  gsalloc = 0;
  gsngrp = 0;
  gsmaxalloc = 0;
  free(anchors); anchors=NULL;
  anchor_alloc = 0;
  n_anchor = 0;
  free(cradix_counts); cradix_counts=NULL;
  free(cradix_xtmp);   cradix_xtmp=NULL;
  free_ustr();
//  free(OTMP); OTMP=NULL;
//  free(KTMP); KTMP=NULL;
  if (key!=NULL) for (int i=0; i<nradix; i++) free(key[i]);
  free(key); key=NULL; nradix=0;
  savetl_end();  // Restore R's own usage of tl. Must run after the for loop in free_ustr() since only CHARSXP which had tl>0 (R's usage) are stored there.
}

static void push(int *x, int n, int anchor) {
  #pragma omp critical(push)
  {
    if (gsalloc < gsngrp+n) {
      if (gsalloc==0) gsalloc = 4096;   // initially 4 pages and then double in multiples of 4 pages
      else gsalloc = (gsngrp+n < gsmaxalloc/3) ? (gsngrp+n)*2 : gsmaxalloc;  // /[2|3] to not overflow and 3 not 2 to avoid allocating close to gsmaxalloc
      gs = realloc(gs, gsalloc*sizeof(int));
      if (gs==NULL) Error("Failed to realloc working memory stack to %d*4bytes", (int)gsalloc);
    }
    memcpy(gs+gsngrp, x, n*sizeof(int));
    gsngrp+=n;

    if (anchor_alloc==n_anchor) {
      if (anchor_alloc==0) anchor_alloc = 4096;
      else anchor_alloc = (anchor_alloc < gsmaxalloc/3) ? anchor_alloc*2 : gsmaxalloc;   // TODO: rename 'gsmaxalloc' to 'nrow' since that is what it is
      anchors = realloc(anchors, anchor_alloc*2*sizeof(int));
      if (anchors==NULL) Error("Failed to realloc anchors to %d*8bytes", (int)anchor_alloc);
    }
    anchors[n_anchor*2] = anchor;
    anchors[n_anchor*2+1] = n;
    n_anchor++;
  }
}

#ifdef TIMING_ON
  #define NBLOCK 20
  static double tblock[NBLOCK];
  static int nblock[NBLOCK];
  #define TBEG() double tstart = wallclock();   // tstart declared locally
  #define TEND(i) {tblock[i] += wallclock()-tstart; nblock[i]++; tstart = wallclock();}
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
    u.u64 += (u.u64 & dmask) << 1/*is this shift really correct. No need to shift*/  ;   // when dround==1|2, if 8th|16th bit is set, round up before chopping last 1|2 bytes
    return u.u64 >> (dround*8);
  }
  if (ISNAN(u.d)) return ISNA(u.d)    ? 0 /*NA*/   : 1 /*NaN*/;  // also normalises a difference between NA on 32bit R (bit 13 set) and 64bit R (bit 13 not set)
  if (isinf(u.d)) return signbit(u.d) ? 2 /*-Inf*/ : (0xffffffffffffffff>>(dround*8)) /*+Inf*/;
  Error("Unknown non-finite value; not NA, NaN, -Inf or +Inf");
}

void radix_r(int from, int to, int byte);

SEXP forder(SEXP DT, SEXP by, SEXP retGrpArg, SEXP sortStrArg, SEXP orderArg, SEXP naArg)
// sortStr TRUE from setkey, FALSE from by=
{

#ifdef TIMING_ON
  memset(tblock, 0, NBLOCK*sizeof(double));
  memset(nblock, 0, NBLOCK*sizeof(int));
#endif
  TBEG()

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

  if (!isLogical(retGrpArg) || LENGTH(retGrpArg)!=1 || INTEGER(retGrpArg)[0]==NA_LOGICAL) error("retGrp must be TRUE or FALSE");
  retgrp = LOGICAL(retGrpArg)[0]==TRUE;
  if (!isLogical(sortStrArg) || LENGTH(sortStrArg)!=1 || INTEGER(sortStrArg)[0]==NA_LOGICAL ) error("sortStr must be TRUE or FALSE");
  if (!isLogical(naArg) || LENGTH(naArg) != 1) error("na.last must be logical TRUE, FALSE or NA of length 1");
  sort = LOGICAL(sortStrArg)[0]==TRUE;  // TODO: rename sortStr to just sort.  Just one argument either TRUE/FALSE, or a vector of +1|-1.
  int nalast = (LOGICAL(naArg)[0] == NA_LOGICAL) ? -1 : LOGICAL(naArg)[0]; // 1=na last, 0=na first (default), -1=remove na
  gsmaxalloc = n;  // upper limit for stack size (all size 1 groups). We'll detect and avoid that limit, but if just one non-1 group (say 2), that can't be avoided.

  if (n==0) {
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
  all_skipped = true;
  notFirst = false;

  SEXP ans = PROTECT(allocVector(INTSXP, n)); n_protect++;
  anso = INTEGER(ans);
  for (int i=0; i<n; i++) anso[i]=i+1;   // gdb 8.1.0.20180409-git very slow here, oddly

  savetl_init();   // from now on use Error not error

  int ncol=length(by);
  key = calloc(ncol*8+1, sizeof(uint8_t *));  // needs to be before loop because part II relies on part I, column-by-column. +1 because we check NULL after last one
  // TODO: if key==NULL Error
  nradix=0; // the current byte we're writing this column to; might be squashing into it (spare>0)
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
      if (min==0 && nalast==-1) { all_skipped=false; for (int i=0; i<n; i++) anso[i]=0; }
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
          if (nalast==-1) {all_skipped=false; anso[i]=0;}
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
            if (nalast==-1) {all_skipped=false; anso[i]=0;}
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
              if (nalast==-1) {all_skipped=false; anso[i]=0;}  // for both NA and NaN
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
          if (nalast==-1) {all_skipped=false; anso[i]=0;}
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
  #ifdef TIMING_ON
  Rprintf("nradix=%d\n", nradix);
  #endif
  TEND(0);

  if (nradix) {
    //OTMP = malloc(n*sizeof(int));
    //KTMP = malloc((nradix-1) * n * sizeof(uint8_t *));
    int old = omp_get_nested();
    omp_set_nested(1);
    radix_r(0, n-1, 0);  // top level recursive call: (from, to, byte). Nested parallelism is constrained via ordered clause.
    omp_set_nested(old);
  } else {
    int my_gs[1] = {n};
    push(my_gs, 1, 0);
  }

  #ifdef TIMING_ON
  {
    int last=NBLOCK-1;
    while (last>=0 && nblock[last]==0) last--; // remove unused timing slots
    for (int i=0; i<=last; i++) {
      Rprintf("Timing block %2d = %8.3f   %8d\n", i, tblock[i], nblock[i]);
    }
  }
  #endif

  if (all_skipped) {   // when data is already grouped or sorted, integer() returned with group sizes attached
    // the o vector created earlier could be avoided in this case if we only create it when isSorted becomes FALSE
    ans = PROTECT(allocVector(INTSXP, 0));  // Can't attach attributes to NULL
    n_protect++;
  }

  if (retgrp) {
    SEXP tt;
    setAttrib(ans, sym_starts, tt = allocVector(INTSXP, gsngrp));
    int *ss = INTEGER(tt);
    int maxgrpn = 0;
    for (int i=0, tmp=1; i<gsngrp; i++) {
      int elem = gs[i];
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

#define STL 65535   // Single Thread Load.  Such that counts can be uint16_t (smaller for MT cache efficiency), therefore not 65536

static inline void count_group(
  const uint8_t *restrict x, const int n,                           // a set of n bytes; n<=STL
  bool *skip, uint8_t *ugrp, int *ngrp, uint16_t *restrict counts   // outputs to small fixed stack allocations in caller (initialized by caller)
) {
  ugrp[0] = x[0];     // first item is always first of first group
  counts[x[0]] = 1;
  *ngrp = 1;          // number of groups (items in ugrp[])
  *skip = true;       // i) if already grouped and sort=false then caller can skip, ii) if already sorted when sort=true then caller can skip
  for (int i=1; i<n; i++) {
    uint8_t elem = x[i];
    if (++counts[elem]==1) {
      // first time seen this value
      ugrp[(*ngrp)++]=elem;
    } else if (*skip && elem!=x[i-1]) {
      // seen this value before and it isn't the previous value, so data is not grouped
      // including "skip &&" first is to avoid the != comparison
      *skip=false;
    }
  }
  if (sort && !sort_ugrp(ugrp, *ngrp))  // sorts ugrp by reference
    *skip=false;
}

void radix_r(const int from, const int to, const int radix) {

//    #pragma omp parallel num_threads( n<=STL ? 1 : getDTthreads() )  // avoid starting potentially 32 threads (each with STL temps) for small input that is ST anyway
//    {
//      uint16_t my_order[STL];
//      int my_gs[STL];     // group sizes, if all size 1 then STL will be full.  TODO: can my_gs and my_ng be uint16_t too instead of int?
//      int my_ng=0;        // number of groups; number of items used in my_gs
//      int my_tmp[STL];    // used to reorder ans (and thus needs to be int) and the first 1/4 is used to reorder each key[index]

//      #pragma omp for ordered schedule(dynamic)
//      for (int g=0; g<gsngrp[1-flip]; g++) {     // first time this will be just 1; the first split will be the nested method
//        int from = g==0 ? 0 : gs[1-flip][g-1];   // this is why grps need to be pre-cumulated
//        int to = gs[1-flip][g];
  TBEG();
  const int my_n = to-from+1;
  if (my_n==1) {
    if (retgrp) {
      int my_gs[1] = {1};
      push(my_gs, 1, from);  // from is pushed so as to flutter sort group sizes afterwards
    }
    TEND(1);
    return;
  }
  // else // < 300 then insert sort to reinstate (but countgroup might be fast enough now using ugrp)
  if (my_n<=STL) {
    uint16_t counts[256] = {0};  // Needs to be all-0 on entry. This ={0} initialization should be fast as it's on stack. If not then allocate up front
                                 // and rely on last line of this function which resets just the non-zero's to 0. However, since recursive, it's tricky
                                 // to allocate upfront, which is why we're keeping it on stack like this.
    uint8_t ugrp[(my_n<256)?my_n:256];  // uninitialized is fine. unique groups; avoids sweeping all 256 counts when very often very few of them are used
    int ngrp;                           // int because it needs to hold the value 256. Could over-optimize down to uint8_t later perhaps.
    bool skip;
    const uint8_t *restrict my_key = key[radix]+from;
    count_group(my_key, my_n,                // inputs
                &skip, ugrp, &ngrp, counts); // outputs
    // count_group deals with 'sort' flag within it.  TODO: rename sort to sort_type (or better)
    if (!skip) {
      // reorder anso and remaining radix keys

      // avoid allocating and populating order vector (my_n long); we just use counts several times to push rather than pull
      // with contiguous-read from osub and ksub, 256 write live cache-lines is worst case. However, often there are many fewer ugrp and only that number of
      // write cache lines will be active. These write-cache lines will be constrained within the STL width, so should be close by in cache, too.
      // If there is a good degree of grouping, there contiguous-read/write both ways happens automatically in this approach.

      all_skipped = false; // ok to write from threads (naked) in this case since bool and only ever write false

      // cumulate; for forwards-assign to give cpu prefetch best chance (cpu may not support prefetch backwards)
      for (int i=0, sum=0; i<ngrp; i++) { uint8_t w=ugrp[i]; int tmp=counts[w]; counts[w]=sum; sum+=tmp; }

      if (radix==0) {
        // anso contains 1:n so skip reading and copying it. Only happens when entire input<STL. Saving worth the branch when user repeatedly calls a small-n small-cardinality order.
        for (int i=0; i<my_n; i++) anso[counts[my_key[i]]++] = i+1;  // +1 as R is 1-based
      } else {
        int *osub = anso+from;
        bool onheap = my_n>=1024;
        int *TMP = onheap ? malloc(my_n*sizeof(int)) : alloca(my_n*sizeof(int));  // OS likely faster here than us. Let's see.  TODO: int=>int64_t in future
        for (int i=0; i<my_n; i++) TMP[counts[my_key[i]]++] = osub[i];
        memcpy(osub, TMP, my_n*sizeof(int));
        if (onheap) free(TMP);
      }

      // reorder remaining key columns (radix+1 onwards).   This could be done in one-step too (a single pass through x[],  with a larger my_tmp
      //    that's how its done in the batched approach below.  Which is better?  The way here is multiple (but contiguous) passes through my_key
      if (radix+1<nradix) {
        bool onheap = my_n>=4096;
        uint8_t *TMP = onheap ? malloc(my_n) : alloca(my_n);
        for (int r=radix+1; r<nradix; r++) {
          for (int i=0,last=0; i<ngrp; i++) { int tmp=counts[ugrp[i]]; counts[ugrp[i]]=last; last=tmp; }  // rewind cumulate. When ngrp<<256, faster than memcpy
          uint8_t *restrict ksub = key[r]+from;
          for (int i=0; i<my_n; i++) TMP[counts[my_key[i]]++] = ksub[i];
          memcpy(ksub, TMP, my_n);
        }
        if (onheap) free(TMP);
      }

      // reset cumulate to counts (as they were after count_group)
      for (int i=0,last=0; i<ngrp; i++) { int tmp=counts[ugrp[i]]; counts[ugrp[i]]-=last; last=tmp; }
    }
    if (radix+1==nradix || ngrp==my_n) {
      if (retgrp) {
         int my_gs[ngrp];
         for (int i=0; i<ngrp; i++) my_gs[i]=counts[ugrp[i]];
         push(my_gs, ngrp, from);  // take the time to make my_gs and push all in one go because push is omp critical
      }
    } else {
      // this single thread will now descend and resolve all groups, now that the groups are close in cache
      // **TODO**: this could skip 1s when retgrp,  and do batches of 1s when retgrp to save flutter anchors
      for (int i=0, my_from=from; i<ngrp; i++) {
        int gs = counts[ugrp[i]];
        radix_r(my_from, my_from+gs-1, radix+1);
        my_from+=gs;
      }
    }
    // for (int i=0; i<ngrp; i++) counts[ugrp[i]] = 0;  // ready for next time to save initializing should the stack 256-initialization above ever be identified as too slow
    TEND(2)
    return;
  }
  // else parallel batches. This is called recursively but only once or maybe twice before resolving to STL branch above

  //int nBatch = getDTthreads()*1525;  // at least nth; more to reduce last-chunk-home; but not too large size we need nBatch*256 counts
  int batchSize = MIN(STL, 1+my_n/getDTthreads());  // (my_n-1)/nBatch + 1;
  int nBatch = (my_n-1)/batchSize + 1;
  int lastBatchSize = my_n - (nBatch-1)*batchSize;
  uint16_t *counts = calloc(nBatch*256,sizeof(uint16_t));  // TODO: this can be static (since just one-at-a-time);  as long as we copy out the first row onto stack later below.
  uint8_t  *ugrps =  malloc(nBatch*256*sizeof(uint8_t));
  int      *ngrps =  calloc(nBatch    ,sizeof(int));
  // TODO:  if (!counts || !ugrps || !ngrps) Error...
  //Rprintf("nested batch count...\n");

  bool skip=true;
  const int n_rem = nradix-radix-1;   // how many radix are remaining after this one
  TEND(3)
  #pragma omp parallel num_threads(getDTthreads())
  {
    int     *my_otmp = malloc(batchSize * sizeof(int)); // thread-private write. Move this up above and point this private restrict to it. Easier to Error alloc that way, if it's just as fast.
    uint8_t *my_ktmp = malloc(batchSize * sizeof(uint8_t) * n_rem);
    // TODO: if my_otmp==NULL || my_ktmp==NULL, error.
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
        // as much as possible which is a good chance since batchSize is relatively small (STL)
        for (int i=0, sum=0; i<my_ngrp; i++) { int tmp = my_counts[my_ugrp[i]]; my_counts[my_ugrp[i]]=sum; sum+=tmp; } // cumulate counts of this batch
        const int *restrict osub = anso+my_from;
        byte = my_key;
        for (int i=0; i<my_n; i++, byte++) {
          int dest = my_counts[*byte]++;
          my_otmp[dest] = *osub++;  // wastefully copies out 1:n when radix==0, but do not optimize as unlikely worth code complexity. my_otmp is not large, for example. Use first TEND() to decide.
          for (int r=0; r<n_rem; r++) my_ktmp[r*my_n + dest] = key[radix+1+r][my_from+i];   // reorder remaining keys
        }
        // or could do multiple passes through my_key like in the my_n<STL approach above. Test which is better depending on if TEND() points here.

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
  TEND(4 + notFirst*3)  // 3 timings in this section: 4,5,6 first main split; 7,8,9 thereon

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
  // i) sort==0 (not sorting groups) then osub and ksub don't need reordering. ugrp may happen to be sorted too but nothing special to do in that case.
  // ii) sort==1|-1 and ugrp is already sorted then osub and ksub don't need redordering either.

  if (sort && !sort_ugrp(ugrp, ngrp))
    skip=false;

  // now cumulate counts vertically to see where the blocks in the batches should be placed in the result across all batches
  // the counts are uint16_t due to STL. But the cumulate needs to be int32_t (or int64_t in future) to hold the offsets
  // If skip==true and we're already done, we still need the first row of this cummulate (diff to get total group sizes) to push() or recurse below

  //Rprintf("counts:\n");
  //for (int b=0; b<nBatch; b++) {
  //  for (int i=0; i<256; i++) { Rprintf("%d ",counts[b*256+i]);}  Rprintf("\n");
  //}

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

  //Rprintf("starts:\n");
  //for (int b=0; b<nBatch; b++) {
  //  for (int i=0; i<256; i++) { Rprintf("%d ",starts[b*256+i]); }  Rprintf("\n");
  //}

  TEND(5 + notFirst*3)
  if (!skip) {
    all_skipped = false;   // to save testing for 1:n at the very end before returning empty to mean already-sorted.

    // TODO this might not need to be parallel.  Just single thread might be able to do it.
    //      time how long the first one takes. If not too bad, then definitely single-thread, because these are already nested (other than the very first big one)
    int *TMP = malloc(my_n * sizeof(int));
    // TODO: if TMP==NULL Error
    #pragma omp parallel for num_threads(getDTthreads())
    for (int batch=0; batch<nBatch; batch++) {
      //const int my_from = from + batch*batchSize;
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
      #pragma omp parallel for num_threads(getDTthreads())
      for (int batch=0; batch<nBatch; batch++) {
        //const int my_from = from + batch*batchSize;
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
  TEND(6 + notFirst*3)
  notFirst = true;

  //if (radix==0) {
  //  for (int i=0; i<ngrp-1; i++) Rprintf("radix=%d  i=%d  gs=%d\n", radix, i, counts[ugrp[i+1]]-counts[ugrp[i]]);
  //  Rprintf("radix=%d  i=%d  gs=%d\n", radix, ngrp-1, my_n-counts[ugrp[ngrp-1]]);
  //}

  // TODO could push all the size 1 groups (and maybe 2) and reduce the ngrp to just decent sizes, to just parallel through decent sizes.
  if (radix+1==nradix || ngrp==my_n) {
    if (retgrp) {
      int my_gs[ngrp];
      for (int i=1; i<ngrp; i++) my_gs[i-1] = starts[ugrp[i]] - starts[ugrp[i-1]];   // use the first row of starts to get totals
      my_gs[ngrp-1] = my_n - starts[ugrp[ngrp-1]];
      push(my_gs, ngrp, from); // take the time to make my_gs and push all in one go because push is omp critical. 'from' is anchor to be flutter sorted afterwards
    }
    TEND(10)
  } else {
    #pragma omp parallel for ordered schedule(dynamic) num_threads(getDTthreads())
    for (int i=0; i<ngrp; i++) {
      int my_from = from + starts[ugrp[i]];
      int my_to   = from + (i==(ngrp-1) ? my_n : starts[ugrp[i+1]]) - 1;
      //int my_n = my_to-my_from+1;
      if (my_n<=STL) radix_r(my_from, my_to, radix+1); // will write to gs out-of-order (but not terribly out-of-order); this gets flutter sorted later
      #pragma omp ordered
      {
        if (my_n>STL) radix_r(my_from, my_to, radix+1);  // inside ordered as a way to limit recursive nestedness; doesn't need to be ordered thanks to anchor
      }
    }
    TEND(11)
  }
  free(counts);
  free(starts);
  free(ugrps);
  free(ngrps);
  TEND(12)
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

