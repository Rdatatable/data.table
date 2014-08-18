#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

extern SEXP char_integer64;

static union {
    double d;
    unsigned long long ull;
} u;

SEXP dt_na(SEXP x, SEXP xorderArg, SEXP xstartArg) {
    int i, j, k, n = length(xstartArg), *xstart = INTEGER(xstartArg), *xorder = INTEGER(xorderArg);
    SEXP v, ans, class;
    double *dv;
    
    if (!isNewList(x)) error("Internal error: 'x' should be a list. Please report to datatable-help");
    ans = PROTECT(allocVector(LGLSXP, n));
    for (i=0; i<n; i++) LOGICAL(ans)[i]=0;
    for (i=0; i<length(x); i++) {
        v = VECTOR_ELT(x, i);
        switch (TYPEOF(v)) {
        case LGLSXP:
            for (j=0; j<n; j++) {
                k = xorder[xstart[j]-1]-1;
                LOGICAL(ans)[j] |= (LOGICAL(v)[k] == NA_LOGICAL);
            }
            break;
        case INTSXP:
            for (j=0; j<n; j++) {
                k = xorder[xstart[j]-1]-1;
                LOGICAL(ans)[j] |= (INTEGER(v)[k] == NA_INTEGER);
            }
            break;
        case STRSXP:
            for (j=0; j<n; j++) {
                k = xorder[xstart[j]-1]-1;
                LOGICAL(ans)[j] |= (STRING_ELT(v, k) == NA_STRING);
            }
            break;
        case REALSXP:
            class = getAttrib(v, R_ClassSymbol);        
            if (isString(class) && STRING_ELT(class, 0) == char_integer64) {
                dv = (double *)REAL(v);
                for (j=0; j<n; j++) {
                    k = xorder[xstart[j]-1]-1;
                    u.d = dv[k];
                    LOGICAL(ans)[j] |= ((u.ull ^ 0x8000000000000000) == 0);
                }
            } else {
                for (j=0; j<n; j++) {
                    k = xorder[xstart[j]-1]-1;
                    LOGICAL(ans)[j] |= ISNAN(REAL(v)[k]);
                }
            }
            break;
        default:
            error("Unknown column type '%s'", type2char(TYPEOF(v)));
        }
    }        
    UNPROTECT(1);
    return(ans);
}

SEXP frank(SEXP xorderArg, SEXP xstartArg, SEXP xlenArg, SEXP ties_method) {
    int i=0, j=0, n;
    int *xstart = INTEGER(xstartArg), *xlen = INTEGER(xlenArg), *xorder = INTEGER(xorderArg);
    enum {MEAN, MAX, MIN} ties = MEAN;
    SEXP ans;

    if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "average"))  ties = MEAN;
    else if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "max")) ties = MAX;
    else if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "min")) ties = MIN;
    else error("Internal error: invalid ties.method for frankv(), should have been caught before. Please report to datatable-help");
    n = length(xorderArg);
    ans = (ties == MEAN) ? PROTECT(allocVector(REALSXP, n)) : PROTECT(allocVector(INTSXP, n));
    if (n > 0) {
        switch (ties) {
            case MEAN : 
            for (i = 0; i < length(xstartArg); i++) {
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    REAL(ans)[xorder[j]-1] = (2*xstart[i]+xlen[i]-1)/2.0;
            }
            break;
            case MAX :
            for (i = 0; i < length(xstartArg); i++) {
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    INTEGER(ans)[xorder[j]-1] = xstart[i]+xlen[i]-1;
            }
            break;
            case MIN :
            for (i = 0; i < length(xstartArg); i++) {
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    INTEGER(ans)[xorder[j]-1] = xstart[i];
            }
            break;
        }
    }
    UNPROTECT(1);
    return(ans);
}
