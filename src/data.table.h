#include "dt_stdio.h"  // PRId64 and PRIu64
#include <R.h>
#include <Rversion.h>
#if !defined(R_VERSION) || R_VERSION < R_Version(3, 5, 0)  // R-exts$6.14
#  define ALTREP(x) 0     // #2866
#  define USE_RINTERNALS  // #3301
#  define DATAPTR_RO(x) ((const void *)DATAPTR(x))
#endif
#include <Rinternals.h>
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x))  // to avoid overhead of looped STRING_ELT and VECTOR_ELT
#include <stdint.h>    // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include "myomp.h"
#include "types.h"
#include "po.h"
#ifdef WIN32  // positional specifiers (%n$) used in translations; #4402
#  define snprintf dt_win_snprintf  // see our snprintf.c; tried and failed to link to _sprintf_p on Windows
#endif
#ifdef sprintf
#undef sprintf
#endif
#define sprintf USE_SNPRINTF_NOT_SPRINTF  // prevent use of sprintf in data.table source; force us to use n always

// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

#define IS_UTF8(x)  (LEVELS(x) & 8)
#define IS_ASCII(x) (LEVELS(x) & 64)
#define IS_LATIN(x) (LEVELS(x) & 4)
#define IS_TRUE(x)  (TYPEOF(x)==LGLSXP && LENGTH(x)==1 && LOGICAL(x)[0]==TRUE)
#define IS_FALSE(x) (TYPEOF(x)==LGLSXP && LENGTH(x)==1 && LOGICAL(x)[0]==FALSE)
#define IS_TRUE_OR_FALSE(x) (TYPEOF(x)==LGLSXP && LENGTH(x)==1 && LOGICAL(x)[0]!=NA_LOGICAL)

#define SIZEOF(x) __sizes[TYPEOF(x)]
#define TYPEORDER(x) __typeorder[x]

#ifdef MIN
#  undef MIN
#endif
#define MIN(a,b) (((a)<(b))?(a):(b))

#ifdef MAX
#  undef MAX
#endif
#define MAX(a,b) (((a)>(b))?(a):(b))

// for use with bit64::integer64
#define NA_INTEGER64  INT64_MIN
#define MAX_INTEGER64 INT64_MAX

// for use with CPLXSXP, no macro provided by R internals
#define ISNAN_COMPLEX(x) (ISNAN((x).r) || ISNAN((x).i)) // TRUE if either real or imaginary component is NA or NaN

// Backport macros added to R in 2017 so we don't need to update dependency from R 3.0.0
#ifndef MAYBE_SHARED
#  define MAYBE_SHARED(x) (NAMED(x) > 1)
#endif
#ifndef MAYBE_REFERENCED
#  define MAYBE_REFERENCED(x) ( NAMED(x) > 0 )
#endif

// If we find a non-ASCII, non-NA, non-UTF8 encoding, we try to convert it to UTF8. That is, marked non-ascii/non-UTF8 encodings will
// always be checked in UTF8 locale. This seems to be the best fix Arun could think of to put the encoding issues to rest.
// Since the if-statement will fail with the first condition check in "normal" ASCII cases, there shouldn't be huge penalty issues in
// most cases. Fix for #66, #69, #469 and #1293
// TODO: compare 1.9.6 performance with 1.9.7 with huge number of ASCII strings, and again after Jan 2018 when made macro.
// Matt moved this to be macro in Jan 2018 so that branch can benefit from branch prediction too wherever used inside loops.
// This IS_ASCII will dereference s and that cache fetch is the part that may bite more than the branch, though. Without a call to
// to ENC2UTF as all, the pointer value can just be compared by the calling code without deferencing it. It may still be worth
// timing the impact and manually avoiding (is there an IS_ASCII on the character vector rather than testing each item every time?)
#define NEED2UTF8(s) !(IS_ASCII(s) || (s)==NA_STRING || IS_UTF8(s))
#define ENC2UTF8(s) (!NEED2UTF8(s) ? (s) : mkCharCE(translateCharUTF8(s), CE_UTF8))

// init.c
extern SEXP char_integer64;
extern SEXP char_ITime;
extern SEXP char_IDate;
extern SEXP char_Date;
extern SEXP char_POSIXct;
extern SEXP char_POSIXt;
extern SEXP char_UTC;
extern SEXP char_nanotime;
extern SEXP char_lens;
extern SEXP char_indices;
extern SEXP char_allLen1;
extern SEXP char_allGrp1;
extern SEXP char_factor;
extern SEXP char_ordered;
extern SEXP char_datatable;
extern SEXP char_dataframe;
extern SEXP char_NULL;
extern SEXP sym_sorted;
extern SEXP sym_index;
extern SEXP sym_BY;
extern SEXP sym_starts, char_starts;
extern SEXP sym_maxgrpn;
extern SEXP sym_colClassesAs;
extern SEXP sym_verbose;
extern SEXP SelfRefSymbol;
extern SEXP sym_inherits;
extern SEXP sym_datatable_locked;
extern SEXP sym_tzone;
extern SEXP sym_old_fread_datetime_character;
extern double NA_INT64_D;
extern long long NA_INT64_LL;
extern Rcomplex NA_CPLX;  // initialized in init.c; see there for comments
extern size_t __sizes[100];     // max appears to be FUNSXP = 99, see Rinternals.h
extern size_t __typeorder[100]; // __ prefix otherwise if we use these names directly, the SIZEOF define ends up using the local one

long long DtoLL(double x);
double LLtoD(long long x);
bool GetVerbose();

// cj.c
SEXP cj(SEXP base_list);

// dogroups.c
SEXP keepattr(SEXP to, SEXP from);
SEXP growVector(SEXP x, R_len_t newlen);

// assign.c
SEXP allocNAVector(SEXPTYPE type, R_len_t n);
SEXP allocNAVectorLike(SEXP x, R_len_t n);
void writeNA(SEXP v, const int from, const int n);
void savetl_init(), savetl(SEXP s), savetl_end();
int checkOverAlloc(SEXP x);

// forder.c
int StrCmp(SEXP x, SEXP y);
uint64_t dtwiddle(const void *p, int i);
SEXP forder(SEXP DT, SEXP by, SEXP retGrp, SEXP sortStrArg, SEXP orderArg, SEXP naArg);
int getNumericRounding_C();

// reorder.c
SEXP reorder(SEXP x, SEXP order);
SEXP setcolorder(SEXP x, SEXP o);

// subset.c
void subsetVectorRaw(SEXP ans, SEXP source, SEXP idx, const bool anyNA);
SEXP subsetVector(SEXP x, SEXP idx);

// fcast.c
SEXP int_vec_init(R_len_t n, int val);

// vecseq.c
SEXP vecseq(SEXP x, SEXP len, SEXP clamp);

// uniqlist.c
SEXP uniqlist(SEXP l, SEXP order);
SEXP uniqlengths(SEXP x, SEXP n);

// chmatch.c
SEXP chmatch(SEXP x, SEXP table, int nomatch);
SEXP chin(SEXP x, SEXP table);

SEXP isOrderedSubset(SEXP, SEXP);
void setselfref(SEXP);

// fmelt.c
SEXP seq_int(int n, int start);
SEXP set_diff(SEXP x, int n);
SEXP which(SEXP x, Rboolean val);

// frank.c
SEXP dt_na(SEXP x, SEXP cols);

// assign.c
SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose);
const char *memrecycle(const SEXP target, const SEXP where, const int r, const int len, SEXP source, const int sourceStart, const int sourceLen, const int coln, const char *colname);
SEXP shallowwrapper(SEXP dt, SEXP cols);

SEXP dogroups(SEXP dt, SEXP dtcols, SEXP groups, SEXP grpcols, SEXP jiscols,
                SEXP xjiscols, SEXP grporder, SEXP order, SEXP starts,
                SEXP lens, SEXP jexp, SEXP env, SEXP lhs, SEXP newnames,
                SEXP on, SEXP verbose);

// bmerge.c
SEXP bmerge(SEXP iArg, SEXP xArg, SEXP icolsArg, SEXP xcolsArg, SEXP isorted,
                SEXP xoArg, SEXP rollarg, SEXP rollendsArg, SEXP nomatchArg,
                SEXP multArg, SEXP opArg, SEXP nqgrpArg, SEXP nqmaxgrpArg);

// quickselect
double dquickselect(double *x, int n);
double iquickselect(int *x, int n);
double i64quickselect(int64_t *x, int n);

// fread.c
double wallclock();

// openmp-utils.c
void initDTthreads();
int getDTthreads(const int64_t n, const bool throttle);
void avoid_openmp_hang_within_fork();

// froll.c
void frollmean(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int k, int align, double fill, bool narm, int hasna, bool verbose);
void frollmeanFast(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose);
void frollmeanExact(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose);
void frollsum(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int k, int align, double fill, bool narm, int hasna, bool verbose);
void frollsumFast(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose);
void frollsumExact(double *x, uint64_t nx, ans_t *ans, int k, double fill, bool narm, int hasna, bool verbose);
void frollapply(double *x, int64_t nx, double *w, int k, ans_t *ans, int align, double fill, SEXP call, SEXP rho, bool verbose);

// frolladaptive.c
void fadaptiverollmean(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose);
void fadaptiverollmeanFast(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose);
void fadaptiverollmeanExact(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose);
void fadaptiverollsum(unsigned int algo, double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose);
void fadaptiverollsumFast(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose);
void fadaptiverollsumExact(double *x, uint64_t nx, ans_t *ans, int *k, double fill, bool narm, int hasna, bool verbose);

// frollR.c
SEXP frollfunR(SEXP fun, SEXP obj, SEXP k, SEXP fill, SEXP algo, SEXP align, SEXP narm, SEXP hasNA, SEXP adaptive);
SEXP frollapplyR(SEXP fun, SEXP obj, SEXP k, SEXP fill, SEXP align, SEXP rho);

// nafill.c
void nafillDouble(double *x, uint_fast64_t nx, unsigned int type, double fill, bool nan_is_na, ans_t *ans, bool verbose);
void nafillInteger(int32_t *x, uint_fast64_t nx, unsigned int type, int32_t fill, ans_t *ans, bool verbose);
SEXP nafillR(SEXP obj, SEXP type, SEXP fill, SEXP nan_is_na_arg, SEXP inplace, SEXP cols);

// between.c
SEXP between(SEXP x, SEXP lower, SEXP upper, SEXP incbounds, SEXP NAbounds, SEXP check);

// coalesce.c
SEXP coalesce(SEXP x, SEXP inplace);

// utils.c
bool isRealReallyInt(SEXP x);
SEXP isReallyReal(SEXP x);
bool allNA(SEXP x, bool errorForBadType);
SEXP colnamesInt(SEXP x, SEXP cols, SEXP check_dups);
void coerceFill(SEXP fill, double *dfill, int32_t *ifill, int64_t *i64fill);
SEXP coerceFillR(SEXP fill);
bool INHERITS(SEXP x, SEXP char_);
bool Rinherits(SEXP x, SEXP char_);
SEXP copyAsPlain(SEXP x);
void copySharedColumns(SEXP x);
SEXP lock(SEXP x);
SEXP unlock(SEXP x);
bool islocked(SEXP x);
SEXP islockedR(SEXP x);
bool need2utf8(SEXP x);
SEXP coerceUtf8IfNeeded(SEXP x);

// types.c
char *end(char *start);
void ansMsg(ans_t *ans, int n, bool verbose, const char *func);
SEXP testMsgR(SEXP status, SEXP x, SEXP k);

//fifelse.c
SEXP fifelseR(SEXP l, SEXP a, SEXP b, SEXP na);
SEXP fcaseR(SEXP na, SEXP rho, SEXP args);

//snprintf.c
int dt_win_snprintf(char *dest, size_t n, const char *fmt, ...);

