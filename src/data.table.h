#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
// #include <omp.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

#define SIZEOF(x) sizes[TYPEOF(x)]
#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) (((a)<(b))?(a):(b))
#define NAINT64 LLONG_MIN

// init.c
void setSizes();
SEXP char_integer64;

// dogroups.c
SEXP keepattr(SEXP to, SEXP from);
SEXP growVector(SEXP x, R_len_t newlen);
size_t sizes[100];  // max appears to be FUNSXP = 99, see Rinternals.h
SEXP SelfRefSymbol;

// assign.c
SEXP allocNAVector(SEXPTYPE type, R_len_t n);
void savetl_init(), savetl(SEXP s), savetl_end();
Rboolean isDatatable(SEXP x);

// forder.c
int StrCmp(SEXP x, SEXP y);
unsigned long long dtwiddle(void *p, int i, int order);
unsigned long long i64twiddle(void *p, int i, int order);
unsigned long long (*twiddle)(void *, int, int);
SEXP forder(SEXP DT, SEXP by, SEXP retGrp, SEXP sortStrArg, SEXP orderArg, SEXP naArg);

// reorder.c
SEXP reorder(SEXP x, SEXP order);

// fcast.c
SEXP vec_init(R_len_t n, SEXP val);

// vecseq.c
SEXP vecseq(SEXP x, SEXP len, SEXP clamp);

// uniqlist.c
SEXP uniqlist(SEXP l, SEXP order);
SEXP uniqlengths(SEXP x, SEXP n);

// chmatch.c
SEXP chmatch(SEXP x, SEXP table, R_len_t nomatch, Rboolean in);

SEXP isOrderedSubset(SEXP, SEXP);
void setselfref(SEXP);

// fmelt.c
SEXP seq_int(int n, int start);
SEXP set_diff(SEXP x, int n);
SEXP which(SEXP x, Rboolean bool);

// frank.c
SEXP dt_na(SEXP x, SEXP cols);

// assign.c
SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose);
void memrecycle(SEXP target, SEXP where, int r, int len, SEXP source);
SEXP shallowwrapper(SEXP dt, SEXP cols);

SEXP dogroups(SEXP dt, SEXP dtcols, SEXP groups, SEXP grpcols, SEXP jiscols, SEXP xjiscols, SEXP grporder, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP lhs, SEXP newnames, SEXP verbose);

// bmerge.c
SEXP bmerge(SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP isorted, SEXP xoArg, SEXP rollarg, SEXP rollends, SEXP nomatch, SEXP retFirst, SEXP retLength, SEXP allLen1);

// fcast.c
SEXP coerce_to_char(SEXP s, SEXP env);

// rbindlist.c
SEXP combineFactorLevels(SEXP factorLevels, int * factorType, Rboolean * isRowOrdered);
