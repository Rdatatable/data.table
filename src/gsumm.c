#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
//#include <time.h>

static int *grp = NULL;
static int grpn = 0;
static int ngrp = 0;

SEXP gstart(SEXP o, SEXP f, SEXP l) {
    int i, j, g, *this;
    // clock_t start = clock();
    if (!isInteger(o)) error("o is not integer vector");
    if (!isInteger(f)) error("f is not integer vector");
    if (!isInteger(l)) error("l is not integer vector");
    ngrp = LENGTH(l);
    if (LENGTH(f) != ngrp) error("length(f)=%d != length(l)=%d", LENGTH(f), ngrp);
    grpn=0;
    for (i=0; i<ngrp; i++) grpn+=INTEGER(l)[i];
    if (LENGTH(o) && LENGTH(o)!=grpn) error("o has length %d but sum(l)=%d", LENGTH(o), grpn);
    grp = malloc(grpn * sizeof(int));
    if (!grp) error("Unable to allocate %d * %d bytes in gstart", grpn, sizeof(int));
    if (LENGTH(o)) {
        for (g=0; g<ngrp; g++) {
            this = INTEGER(o) + INTEGER(f)[g]-1;
            for (j=0; j<INTEGER(l)[g]; j++)  grp[ this[j]-1 ] = g;
        }
    } else {
        for (g=0; g<ngrp; g++) {
            this = grp + INTEGER(f)[g]-1;
            for (j=0; j<INTEGER(l)[g]; j++)  this[j] = g;
        }
    }
    // Rprintf("gstart took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
    return(R_NilValue);
}

SEXP gend() {
    free(grp); grp = NULL; ngrp = 0;
    return(R_NilValue);
}
    
SEXP gsum(SEXP x, SEXP narm)
{
    if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
    int i, thisgrp;
    int n = LENGTH(x);
    //clock_t start = clock();
    SEXP ans;
    if (grpn != length(x)) error("grpn [%d] != length(x) [%d] in gsum", grpn, length(x));
    long double *s = malloc(ngrp * sizeof(long double));
    if (!s) error("Unable to allocate %d * %d bytes for gsum", ngrp, sizeof(long double));
    memset(s, 0, ngrp * sizeof(long double)); // all-0 bits == (long double)0, checked in init.c
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
        for (i=0; i<n; i++) {
            thisgrp = grp[i];
            if (thisgrp<0 || thisgrp>ngrp-1) error("grp contains an integer outside the range [0,ngrp-1=%d]", ngrp);  // TO DO: remove when code is stable, but likely negligble time.
	        if(INTEGER(x)[i] == NA_INTEGER) { if (!LOGICAL(narm)[0]) s[thisgrp] = NA_REAL;  continue; }  // Let NA_REAL propogate from here. R_NaReal is IEEE.
	        s[thisgrp] += INTEGER(x)[i];  // no under/overflow here, s is long double (like base)
	        // n++;  // count the non-NA, needed for mean
	    }
	    ans = PROTECT(allocVector(INTSXP, ngrp));
        for (i=0; i<ngrp; i++) {
            if (s[i] > INT_MAX || s[i] < INT_MIN) {
                warning("Group %d summed to more than type 'integer' can hold so the result has been coerced to 'numeric' automatically, for convenience.", i+1);
                UNPROTECT(1);
                ans = PROTECT(allocVector(REALSXP, ngrp));
                for (i=0; i<ngrp; i++) REAL(ans)[i] = (double)s[i];
                break;
            } else if (ISNA(s[i])) {
                INTEGER(ans)[i] = NA_INTEGER;
            } else {
                INTEGER(ans)[i] = (int)s[i]; 
            }
        }
        break;
    case REALSXP:
        ans = PROTECT(allocVector(REALSXP, ngrp));
        for (i=0; i<n; i++) {
            thisgrp = grp[i];
            if (thisgrp<0 || thisgrp>ngrp-1) error("grp contains an integer outside the range [0,ngrp-1=%d]", ngrp);
	        if(ISNAN(REAL(x)[i]) && LOGICAL(narm)[0]) continue;  // else let NA_REAL propogate from here
	        s[thisgrp] += REAL(x)[i];  // done in long double, like base
	        // n++;  // count the non-NA, needed for mean
	    }
	    for (i=0; i<ngrp; i++) {
	        if (s[i] > DBL_MAX) REAL(ans)[i] = R_PosInf;
            else if (s[i] < -DBL_MAX) REAL(ans)[i] = R_NegInf;
            else REAL(ans)[i] = (double)s[i];
        }
	    break;
    default:
        free(s);
        error("Type '%s' not yet supported in gsum", type2char(TYPEOF(x)));
    }
    free(s);
    UNPROTECT(1);
    // Rprintf("this gsum took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
    return(ans);
}

// TO DO: gmean, gmin, gmax, gsd, gprod, gwhich.min, gwhich.max


