#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#define cmpnum(a,b) ((R_IsNaN(a) && R_IsNaN(b)) || (a == R_PosInf && b == R_PosInf) || (a == R_NegInf && b == R_NegInf) || (R_IsNA(a) && R_IsNA(b) && !R_IsNaN(a) && !R_IsNaN(b)) || fabs(a-b)<REAL(tol)[0])
// TO DO: use forder.c:twiddle

SEXP duplist(SEXP l, SEXP ans, SEXP anslen, SEXP order, SEXP tol)  //change name to uniqlist
{
    // Returns the positions of the non-repeated rows. This works like
    // UNIX uniq as referred to by ?base::unique; i.e., it drops immediately
    // repeated rows but doesn't drop duplicates of any previous row.
    // Unless, order is provided, then it also drops any previous row.
    // l must be a list of same length vectors
    // ans is allocated first (maximum length the number of rows) and the length returned in anslen.
    // TO DO: grow ans instead.
    R_len_t i,j,nrow,ncol,len,thisi,previ;
    Rboolean b,byorder;
    SEXP v;
    if (NA_INTEGER != NA_LOGICAL || sizeof(NA_INTEGER)!=sizeof(NA_LOGICAL)) error("Have assumed NA_INTEGER == NA_LOGICAL (currently R_NaInt). If R changes this in future (seems unlikely), an extra case is required; a simple change.");
    ncol = length(l);
    nrow = length(VECTOR_ELT(l,0));
    len=1;
    INTEGER(ans)[0] = 1;    // first row is always the first of the first group
    byorder = INTEGER(order)[0] != -1;
    // Using MISSING() does not seem stable under windows. Always having arguments passed in seems a good idea anyway.
    thisi = byorder ? INTEGER(order)[0]-1 : 0;
    for (i=1; i<nrow; i++) {
        previ = thisi;
        thisi = byorder ? INTEGER(order)[i]-1 : i;
        j = ncol;  // the last column varies the most frequently so check that first and work backwards
        b = TRUE;
        while (--j>=0 && b) {
            v=VECTOR_ELT(l,j);
            switch (TYPEOF(v)) {
            case INTSXP : case LGLSXP :
                b=INTEGER(v)[thisi]==INTEGER(v)[previ]; break;
            case STRSXP :
                b=STRING_ELT(v,thisi)==STRING_ELT(v,previ); break;
            case REALSXP :
                b=cmpnum(REAL(v)[thisi],REAL(v)[previ]); break;
            default :
                error("Type '%s' not supported", type2char(TYPEOF(v))); 
            }
        }
        if (!b) INTEGER(ans)[len++] = i+1;
    }
    INTEGER(anslen)[0] = len;
    return(R_NilValue);
}

// DONE: return 'uniqlist' as a vector (same as duplist) and write a separate function to get group sizes
// Also improvements for numeric type with a hack of checking unsigned int (to overcome NA/NaN/Inf/-Inf comparisons) (> 2x speed-up)
SEXP uniqlist(SEXP l, SEXP order, SEXP tol)
{
    // This works like UNIX uniq as referred to by ?base::unique; i.e., it 
    // drops immediately repeated rows but doesn't drop duplicates of any 
    // previous row. Unless, order is provided, then it also drops any previous 
    // row. l must be a list of same length vectors ans is allocated first 
    // (maximum length the number of rows) and the length returned in anslen.
    // (Accomplished here) TO DO: grow ans instead
    Rboolean b, byorder;
    unsigned long long *ulv; // for numeric check speed-up
    SEXP v, ans;
    R_len_t i, j, nrow, ncol, len, thisi, previ, isize=1000;

    int *iidx = Calloc(isize, int); // for 'idx'
    int *n_iidx; // to catch allocation errors using Realloc!
    if (NA_INTEGER != NA_LOGICAL || sizeof(NA_INTEGER)!=sizeof(NA_LOGICAL)) 
        error("Have assumed NA_INTEGER == NA_LOGICAL (currently R_NaInt). If R changes this in future (seems unlikely), an extra case is required; a simple change.");
    ncol = length(l);
    nrow = length(VECTOR_ELT(l,0));
    len = 1;
    iidx[0] = 1; // first row is always the first of the first group
    byorder = INTEGER(order)[0] != -1;
    // Using MISSING() does not seem stable under windows. Always having arguments passed in seems a good idea anyway.
    thisi = byorder ? INTEGER(order)[0]-1 : 0;
    for (i=1; i<nrow; i++) {
        previ = thisi;
        thisi = byorder ? INTEGER(order)[i]-1 : i;
        j = ncol;  // the last column varies the most frequently so check that first and work backwards
        b = TRUE;
        while (--j>=0 && b) {
            v=VECTOR_ELT(l,j);
            switch (TYPEOF(v)) {
            case INTSXP : case LGLSXP :
                b=INTEGER(v)[thisi]==INTEGER(v)[previ]; break;
            case STRSXP :
                b=STRING_ELT(v,thisi)==STRING_ELT(v,previ); break;
            case REALSXP :
                ulv = (unsigned long long *)REAL(v); // allows for direct comparison of NA, NaN, Inf and -Inf instead of individual checks (gives >=2x speedup)
                b=ulv[thisi] == ulv[previ] || fabs(REAL(v)[thisi]-REAL(v)[previ]) < REAL(tol)[0]; break;
            default :
                error("Type '%s' not supported", type2char(TYPEOF(v))); 
            }
        }
        if (!b) iidx[len++] = i+1;
        // fixed bug (corresponding commit is 1049) should be len >= size, not len > size!!
        if (len >= isize) {
            isize = 1.1*isize*nrow/i;
            n_iidx = Realloc(iidx, isize, int);
            if (n_iidx != NULL) iidx = n_iidx; else error("Error in reallocating memory in 'uniqlist'\n");
        }
    }
    PROTECT(ans = allocVector(INTSXP, len));
    memcpy(INTEGER(ans), iidx, sizeof(int)*len); // sizeof is of type size_t - no integer overflow issues   
    Free(iidx);
    UNPROTECT(1);
    return(ans);
}

SEXP uniqlengths(SEXP x, SEXP n) {
    SEXP ans;
    R_len_t i, len;
    if (TYPEOF(x) != INTSXP || length(x) <= 0) error("Input argument 'x' to 'uniqlengths' must be of type integer of length >= 1");
    if (TYPEOF(n) != INTSXP || length(n) != 1) error("Input argument 'n' to 'uniqlengths' must be of type integer of length == 1");
    PROTECT(ans = allocVector(INTSXP, length(x)));
    len = length(x);
    for (i=1; i<len; i++) {
        INTEGER(ans)[i-1] = INTEGER(x)[i] - INTEGER(x)[i-1];
    }
    INTEGER(ans)[len-1] = INTEGER(n)[0] - INTEGER(x)[len-1] + 1;
    UNPROTECT(1);
    return(ans);
}


