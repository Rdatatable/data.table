#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
//#include <time.h>

static int *grp = NULL;      // the group of each x item, like a factor
static int ngrp = 0;         // number of groups
static int *grpsize = NULL;  // size of each group, used by gmean not gsum
static int grpn = 0;         // length of underlying x == length(grp)

SEXP gstart(SEXP o, SEXP f, SEXP l) {
    int i, j, g, *this;
    // clock_t start = clock();
    if (!isInteger(o)) error("o is not integer vector");
    if (!isInteger(f)) error("f is not integer vector");
    if (!isInteger(l)) error("l is not integer vector");
    ngrp = LENGTH(l);
    if (LENGTH(f) != ngrp) error("length(f)=%d != length(l)=%d", LENGTH(f), ngrp);
    grpn=0;
    grpsize = INTEGER(l);  // l will be protected in calling R scope until gend(), too
    for (i=0; i<ngrp; i++) grpn+=grpsize[i];
    if (LENGTH(o) && LENGTH(o)!=grpn) error("o has length %d but sum(l)=%d", LENGTH(o), grpn);
    grp = malloc(grpn * sizeof(int));
    if (!grp) error("Unable to allocate %d * %d bytes in gstart", grpn, sizeof(int));
    if (LENGTH(o)) {
        for (g=0; g<ngrp; g++) {
            this = INTEGER(o) + INTEGER(f)[g]-1;
            for (j=0; j<grpsize[g]; j++)  grp[ this[j]-1 ] = g;
        }
    } else {
        for (g=0; g<ngrp; g++) {
            this = grp + INTEGER(f)[g]-1;
            for (j=0; j<grpsize[g]; j++)  this[j] = g;
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
    if (!isVectorAtomic(x)) error("GForce sum can only be applied to columns, not .SD or similar. To sum all items in a list such as .SD, either add the prefix base::sum(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lappy(.SD,sum),by=,.SDcols=]'");
    int i, thisgrp;
    int n = LENGTH(x);
    //clock_t start = clock();
    SEXP ans;
    if (grpn != length(x)) error("grpn [%d] != length(x) [%d] in gsum", grpn, length(x));
    long double *s = malloc(ngrp * sizeof(long double));
    if (!s) error("Unable to allocate %d * %d bytes for gsum", ngrp, sizeof(long double));
    memset(s, 0, ngrp * sizeof(long double)); // all-0 bits == (long double)0, checked in init.c
    switch(TYPEOF(x)) {
    case LGLSXP: case INTSXP:
        for (i=0; i<n; i++) {
            thisgrp = grp[i];
            if(INTEGER(x)[i] == NA_INTEGER) { 
	            if (!LOGICAL(narm)[0]) s[thisgrp] = NA_REAL;  // Let NA_REAL propogate from here. R_NaReal is IEEE.
	            continue;
	        }
	        s[thisgrp] += INTEGER(x)[i];  // no under/overflow here, s is long double (like base)
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
            if(ISNAN(REAL(x)[i]) && LOGICAL(narm)[0]) continue;  // else let NA_REAL propogate from here
	        s[thisgrp] += REAL(x)[i];  // done in long double, like base
	    }
	    for (i=0; i<ngrp; i++) {
	        if (s[i] > DBL_MAX) REAL(ans)[i] = R_PosInf;
            else if (s[i] < -DBL_MAX) REAL(ans)[i] = R_NegInf;
            else REAL(ans)[i] = (double)s[i];
        }
	    break;
    default:
        free(s);
        error("Type '%s' not supported by GForce sum (gsum). Either add the prefix base::sum(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
    }
    free(s);
    UNPROTECT(1);
    // Rprintf("this gsum took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
    return(ans);
}

SEXP gmean(SEXP x, SEXP narm)
{
    SEXP ans;
    int i, protecti=0, thisgrp, n;
    //clock_t start = clock();
    if (!isLogical(narm) || LENGTH(narm)!=1 || LOGICAL(narm)[0]==NA_LOGICAL) error("na.rm must be TRUE or FALSE");
    if (!isVectorAtomic(x)) error("GForce mean can only be applied to columns, not .SD or similar. Likely you're looking for 'DT[,lapply(.SD,mean),by=,.SDcols=]'. See ?data.table.");
    if (!LOGICAL(narm)[0]) {
        ans = PROTECT(gsum(x,narm)); protecti++;
        switch(TYPEOF(ans)) {
        case LGLSXP: case INTSXP:
            ans = PROTECT(coerceVector(ans, REALSXP)); protecti++;
        case REALSXP:
            for (i=0; i<ngrp; i++) REAL(ans)[i] /= grpsize[i];  // let NA propogate
            break;
        default :
            error("Internal error: gsum returned type '%s'. typeof(x) is '%s'", type2char(TYPEOF(ans)), type2char(TYPEOF(x)));
        }
        UNPROTECT(protecti);
        return(ans);
    }
    // na.rm=TRUE.  Similar to gsum, but we need to count the non-NA as well for the divisor
    n = LENGTH(x);
    if (grpn != n) error("grpn [%d] != length(x) [%d] in gsum", grpn, length(x));

    long double *s = malloc(ngrp * sizeof(long double));
    if (!s) error("Unable to allocate %d * %d bytes for sum in gmean na.rm=TRUE", ngrp, sizeof(long double));
    memset(s, 0, ngrp * sizeof(long double)); // all-0 bits == (long double)0, checked in init.c

    int *c = malloc(ngrp * sizeof(int));
    if (!c) error("Unable to allocate %d * %d bytes for counts in gmean na.rm=TRUE", ngrp, sizeof(int));
    memset(c, 0, ngrp * sizeof(int)); // all-0 bits == (int)0, checked in init.c
        
    switch(TYPEOF(x)) {
    case LGLSXP: case INTSXP:
        for (i=0; i<n; i++) {
            thisgrp = grp[i];
            if(INTEGER(x)[i] == NA_INTEGER) continue;
	        s[thisgrp] += INTEGER(x)[i];  // no under/overflow here, s is long double
	        c[thisgrp]++;
	    }
        break;
    case REALSXP:
        for (i=0; i<n; i++) {
            thisgrp = grp[i];
            if (ISNAN(REAL(x)[i])) continue;
	        s[thisgrp] += REAL(x)[i];
	        c[thisgrp]++;
	    }
	    break;
    default:
        free(s); free(c);
        error("Type '%s' not supported by GForce mean (gmean) na.rm=TRUE. Either add the prefix base::mean(.) or turn off GForce optimization using options(datatable.optimize=1)", type2char(TYPEOF(x)));
    }
    ans = PROTECT(allocVector(REALSXP, ngrp));
    for (i=0; i<ngrp; i++) {
        if (c[i]==0) { REAL(ans)[i] = R_NaN; continue; }  // NaN to follow base::mean
        s[i] /= c[i]; 
        if (s[i] > DBL_MAX) REAL(ans)[i] = R_PosInf;
        else if (s[i] < -DBL_MAX) REAL(ans)[i] = R_NegInf;
        else REAL(ans)[i] = (double)s[i];
    }
    free(s); free(c);
    UNPROTECT(1);
    // Rprintf("this gmean na.rm=TRUE took %8.3f\n", 1.0*(clock()-start)/CLOCKS_PER_SEC);
    return(ans);
}

// TO DO: gmin, gmax, gsd, gprod, gwhich.min, gwhich.max


