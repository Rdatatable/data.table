#include "myomp.h"     // first for clang-13-omp, #5122
#include "dt_stdio.h"  // PRId64 and PRIu64
#include <R.h>
#include <Rversion.h>
#if R_VERSION < R_Version(3, 5, 0)  // R-exts$6.14
#  define ALTREP(x) 0     // #2866
#  define USE_RINTERNALS  // #3301
#  define DATAPTR_RO(x) ((const void *)DATAPTR(x))
#  define STRING_PTR_RO STRING_PTR
#  define INTEGER_RO INTEGER
#  define REAL_RO REAL
#  define COMPLEX_RO COMPLEX
#  define R_Calloc(x, y) Calloc(x, y)         // #6380
#  define R_Realloc(x, y, z) Realloc(x, y, z)
#  define R_Free(x) Free(x)
#endif
#if R_VERSION < R_Version(3, 4, 0)
#  define SET_GROWABLE_BIT(x)  // #3292
#endif
// TODO: remove the `R_SVN_VERSION` check when R 4.5.0 is released (circa Apr. 2025)
#if R_VERSION < R_Version(4, 5, 0) || R_SVN_REVISION < 86702
#  define isDataFrame(x) isFrame(x) // #6180
#endif
#include <Rinternals.h>
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x))  // to avoid overhead of looped STRING_ELT and VECTOR_ELT
#include <stdint.h>    // for uint64_t rather than unsigned long long
#include <stdarg.h>    // for va_list, va_start
#include <stdbool.h>
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

/* we mean the encoding bits, not CE_NATIVE in a UTF-8 locale */
#define IS_UTF8(x)  (getCharCE(x) == CE_UTF8)
#define IS_LATIN(x) (getCharCE(x) == CE_LATIN1)
// TODO: remove the `R_SVN_VERSION` check when R 4.5.0 is released (circa Apr. 2025)
#if R_VERSION < R_Version(4, 5, 0) || R_SVN_REVISION < 86789
# define IS_ASCII(x) (LEVELS(x) & 64)
#else
# define IS_ASCII(x) (Rf_charIsASCII(x)) // no CE_ASCII
#endif
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

// If we find a non-ASCII, non-NA, non-UTF8 encoding, we try to convert it to UTF8. That is, marked non-ascii/non-UTF8 encodings will
// always be checked in UTF8 locale. This seems to be the best fix Arun could think of to put the encoding issues to rest.
// Since the if-statement will fail with the first condition check in "normal" ASCII cases, there shouldn't be huge penalty issues in
// most cases. Fix for #66, #69, #469 and #1293
// TODO: compare 1.9.6 performance with 1.9.7 with huge number of ASCII strings, and again after Jan 2018 when made macro.
// Matt moved this to be macro in Jan 2018 so that branch can benefit from branch prediction too wherever used inside loops.
// This IS_ASCII will dereference s and that cache fetch is the part that may bite more than the branch, though. Without a call to
// to ENC2UTF as all, the pointer value can just be compared by the calling code without dereferencing it. It may still be worth
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
extern SEXP char_maxString;
extern SEXP char_AsIs;
extern SEXP sym_sorted;
extern SEXP sym_index;
extern SEXP sym_BY;
extern SEXP sym_starts, char_starts;
extern SEXP sym_maxgrpn;
extern SEXP sym_anyna;
extern SEXP sym_anyinfnan;
extern SEXP sym_anynotascii;
extern SEXP sym_anynotutf8;
extern SEXP sym_colClassesAs;
extern SEXP sym_verbose;
extern SEXP SelfRefSymbol;
extern SEXP sym_inherits;
extern SEXP sym_datatable_locked;
extern SEXP sym_tzone;
extern SEXP sym_old_fread_datetime_character;
extern SEXP sym_variable_table;
extern SEXP sym_as_character;
extern SEXP sym_as_posixct;
extern double NA_INT64_D;
extern long long NA_INT64_LL;
extern Rcomplex NA_CPLX;  // initialized in init.c; see there for comments
extern size_t __sizes[100];     // max appears to be FUNSXP = 99, see Rinternals.h
extern size_t __typeorder[100]; // __ prefix otherwise if we use these names directly, the SIZEOF define ends up using the local one

long long DtoLL(double x);
double LLtoD(long long x);
int GetVerbose(void);

// cj.c
SEXP cj(SEXP base_list);

// dogroups.c
SEXP keepattr(SEXP to, SEXP from);
SEXP growVector(SEXP x, R_len_t newlen);

// assign.c
SEXP allocNAVector(SEXPTYPE type, R_len_t n);
SEXP allocNAVectorLike(SEXP x, R_len_t n);
void writeNA(SEXP v, const int from, const int n, const bool listNA);
void savetl_init(void), savetl(SEXP s), savetl_end(void);
int checkOverAlloc(SEXP x);

// forder.c
int StrCmp(SEXP x, SEXP y);
uint64_t dtwiddle(double x);
SEXP forder(SEXP DT, SEXP by, SEXP retGrpArg, SEXP retStatsArg, SEXP sortGroupsArg, SEXP ascArg, SEXP naArg);
SEXP forderReuseSorting(SEXP DT, SEXP by, SEXP retGrpArg, SEXP retStatsArg, SEXP sortGroupsArg, SEXP ascArg, SEXP naArg, SEXP reuseSortingArg); // reuseSorting wrapper to forder
int getNumericRounding_C(void);
void internal_error_with_cleanup(const char *call_name, const char *format, ...);

// reorder.c
SEXP reorder(SEXP x, SEXP order);
SEXP setcolorder(SEXP x, SEXP o);

// subset.c
void subsetVectorRaw(SEXP ans, SEXP source, SEXP idx, const bool anyNA);
SEXP subsetVector(SEXP x, SEXP idx);
const char *check_idx(SEXP idx, int max, bool *anyNA_out, bool *orderedSubset_out);

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
const char *memrecycle(const SEXP target, const SEXP where, const int start, const int len, SEXP source, const int sourceStart, const int sourceLen, const int colnum, const char *colname);
SEXP shallowwrapper(SEXP dt, SEXP cols);
void warn_matrix_column(int i);

SEXP dogroups(SEXP dt, SEXP dtcols, SEXP groups, SEXP grpcols, SEXP jiscols,
                SEXP xjiscols, SEXP grporder, SEXP order, SEXP starts,
                SEXP lens, SEXP jexp, SEXP env, SEXP lhs, SEXP newnames,
                SEXP on, SEXP verbose, SEXP showProgressArg);

// bmerge.c
SEXP bmerge(SEXP idt, SEXP xdt, SEXP icolsArg, SEXP xcolsArg,
            SEXP xoArg, SEXP rollarg, SEXP rollendsArg, SEXP nomatchArg,
            SEXP multArg, SEXP opArg, SEXP nqgrpArg, SEXP nqmaxgrpArg);

// quickselect
double dquickselect(double *x, int n);
double iquickselect(int *x, int n);
double i64quickselect(int64_t *x, int n);

// fread.c
double wallclock(void);

// openmp-utils.c
void initDTthreads(void);
int getDTthreads(const int64_t n, const bool throttle);
void avoid_openmp_hang_within_fork(void);

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
bool within_int32_repres(double x);
bool within_int64_repres(double x);
bool fitsInInt32(SEXP x);
SEXP fitsInInt32R(SEXP x);
bool fitsInInt64(SEXP x);
SEXP fitsInInt64R(SEXP x);
bool allNA(SEXP x, bool errorForBadType);
SEXP colnamesInt(SEXP x, SEXP cols, SEXP check_dups, SEXP skip_absent);
bool INHERITS(SEXP x, SEXP char_);
SEXP copyAsPlain(SEXP x);
void copySharedColumns(SEXP x);
SEXP lock(SEXP x);
SEXP unlock(SEXP x);
bool islocked(SEXP x);
SEXP islockedR(SEXP x);
bool need2utf8(SEXP x);
SEXP coerceUtf8IfNeeded(SEXP x);
SEXP coerceAs(SEXP x, SEXP as, SEXP copyArg);
void internal_error(const char *call_name, const char *format, ...);

// types.c
char *end(char *start);
void ansMsg(ans_t *ans, int n, bool verbose, const char *func);
SEXP testMsgR(SEXP status, SEXP x, SEXP k);

//fifelse.c
SEXP fifelseR(SEXP l, SEXP a, SEXP b, SEXP na);
SEXP fcaseR(SEXP rho, SEXP args);

//snprintf.c
int dt_win_snprintf(char *dest, size_t n, const char *fmt, ...);

// programming.c
SEXP substitute_call_arg_namesR(SEXP expr, SEXP env);

//negate.c
SEXP notchin(SEXP x, SEXP table);

// functions called from R level .Call/.External and registered in init.c
// these now live here to pass -Wstrict-prototypes, #5477
// all arguments must be SEXP since they are called from R level
// where there are no arguments, it must be (void) not () to be a strict prototype
SEXP setattrib(SEXP, SEXP, SEXP);
SEXP assign(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP copy(SEXP);
SEXP setdt_nrows(SEXP);
SEXP alloccolwrapper(SEXP, SEXP, SEXP);
SEXP selfrefokwrapper(SEXP, SEXP);
SEXP truelength(SEXP);
SEXP setcharvec(SEXP, SEXP, SEXP);
SEXP chmatch_R(SEXP, SEXP, SEXP);
SEXP chmatchdup_R(SEXP, SEXP, SEXP);
SEXP chin_R(SEXP, SEXP);
SEXP freadR(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP fwriteR(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rbindlist(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP setlistelt(SEXP, SEXP, SEXP);
SEXP setS4elt(SEXP, SEXP, SEXP);
SEXP address(SEXP);
SEXP expandAltRep(SEXP);
SEXP fmelt(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP fcast(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP issorted(SEXP, SEXP);
SEXP gforce(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP gsum(SEXP, SEXP);
SEXP gmean(SEXP, SEXP);
SEXP gmin(SEXP, SEXP);
SEXP gmax(SEXP, SEXP);
SEXP setNumericRounding(SEXP);
SEXP getNumericRounding(void);
SEXP binary(SEXP);
SEXP subsetDT(SEXP, SEXP, SEXP);
SEXP convertNegAndZeroIdx(SEXP, SEXP, SEXP, SEXP);
SEXP frank(SEXP, SEXP, SEXP, SEXP);
SEXP lookup(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP overlaps(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP whichwrapper(SEXP, SEXP);
SEXP shift(SEXP, SEXP, SEXP, SEXP);
SEXP transpose(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP anyNA(SEXP, SEXP);
SEXP setlevels(SEXP, SEXP, SEXP);
SEXP rleid(SEXP, SEXP);
SEXP gmedian(SEXP, SEXP);
SEXP gtail(SEXP, SEXP);
SEXP ghead(SEXP, SEXP);
SEXP glast(SEXP);
SEXP gfirst(SEXP);
SEXP gnthvalue(SEXP, SEXP);
SEXP dim(SEXP);
SEXP warn_matrix_column_r(SEXP);
SEXP gvar(SEXP, SEXP);
SEXP gsd(SEXP, SEXP);
SEXP gprod(SEXP, SEXP);
SEXP gshift(SEXP, SEXP, SEXP, SEXP);
SEXP nestedid(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP setDTthreads(SEXP, SEXP, SEXP, SEXP);
SEXP getDTthreads_R(SEXP);
SEXP nqRecreateIndices(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP fsort(SEXP, SEXP);
SEXP inrange(SEXP, SEXP, SEXP, SEXP);
SEXP hasOpenMP(void);
SEXP beforeR340(void);
SEXP uniqueNlogical(SEXP, SEXP);
SEXP dllVersion(void);
SEXP initLastUpdated(SEXP);
SEXP allNAR(SEXP);
SEXP test_dt_win_snprintf(void);
SEXP dt_zlib_version(void);
SEXP dt_has_zlib(void);
SEXP startsWithAny(SEXP, SEXP, SEXP);
SEXP convertDate(SEXP, SEXP);
SEXP fastmean(SEXP);

