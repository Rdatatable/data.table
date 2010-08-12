#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP dogroups();
#endif

static SEXP growVector(SEXP x, R_len_t newlen, int size);

SEXP dogroups(SEXP dt, SEXP SD, SEXP dtcols, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP testj, SEXP byretn, SEXP byval, SEXP verbose)
{
    R_len_t i, j, k, rownum, ngrp, njval, nbyval, ansloc, maxn, r, thisansloc, thislen, any0, newlen;
    SEXP names, ans, sizes, jval, jsizes, bysizes, naint, nareal;
    if (TYPEOF(order) != INTSXP) error("order not integer");
    if (TYPEOF(starts) != INTSXP) error("starts not integer");
    if (TYPEOF(lens) != INTSXP) error("lens not integer");
    if (INTEGER(starts)[0]<1 || INTEGER(lens)[0]<1) error("starts[1]<1 or lens[1]<1");
    if(!isEnvironment(env)) error("’env’ should be an environment");
    ngrp = length(starts);  // the number of groups
    njval = length(testj);  // testj is now the result of j on the first group
    nbyval = length(byval);
    names = getAttrib(SD, R_NamesSymbol);
    // only the subset of column names that the j expression uses exist in SD, or if
    // the j uses .SD (or .SDF), then all the columns of dt (detected in the calling R). 
    PROTECT(sizes = allocVector(INTSXP, length(SD)));
    for(i = 0; i < length(SD); i++) {
        switch (TYPEOF(VECTOR_ELT(SD, i))) {
            case INTSXP :
            case LGLSXP :
                INTEGER(sizes)[i] = sizeof(int);
                break;
            case REALSXP :
                INTEGER(sizes)[i] = sizeof(double);
                break;
            case STRSXP :
                INTEGER(sizes)[i] = sizeof(SEXP);
                break;
            default:
                error("only integer,double,logical and character vectors are allowed so far. Type %d would need to be added.", TYPEOF(VECTOR_ELT(SD, i)));
        }
        defineVar(install(CHAR(STRING_ELT(names, i))), VECTOR_ELT(SD, i), env);
    }
    defineVar(install(".SD"), SD, env);
    // By installing .SD directly inside itself, R finds this symbol more quickly (if used). This then allows the
    // env's parent to be the one that called [.data.table, rather than the local scope of
    // [.data.table, for robustness and efficiency. If it were local scope of [.data.table then user's j
    // may inadvertently see interal variables names (as it used to in v1.2) and we had to name them to avoid
    // conflict e.g. "o__".  That is no longer required and things are much cleaner now.
    
    PROTECT(ans = allocVector(VECSXP, nbyval + njval));
    PROTECT(bysizes = allocVector(INTSXP, nbyval));
    PROTECT(jsizes = allocVector(INTSXP, njval));
    
    PROTECT(naint = allocVector(INTSXP, 1));
    INTEGER(naint)[0] = NA_INTEGER;
    
    PROTECT(nareal = allocVector(REALSXP, 1));
    REAL(nareal)[0] = NA_REAL;
    
    for(i = 0; i < nbyval; i++) {
        SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(byval, i)), INTEGER(byretn)[0]));
        switch (TYPEOF(VECTOR_ELT(byval, i))) {   // this structure is more for the future when by could contain character for example
            case INTSXP :
            case LGLSXP :
                INTEGER(bysizes)[i] = sizeof(int);
                break;
            case REALSXP : case STRSXP :
            default:
                error("by columns must be internally integer");
        }
    }
    for(i = 0; i < njval; i++) {
        SET_VECTOR_ELT(ans, nbyval+i, allocVector(TYPEOF(VECTOR_ELT(testj, i)), INTEGER(byretn)[0]));
        switch (TYPEOF(VECTOR_ELT(testj, i))) {  // TO DO : find a size table in a .h to use and save the switch (couldn't find one when looked)
            case INTSXP :
            case LGLSXP :
                INTEGER(jsizes)[i] = sizeof(int);
                break;
            case REALSXP :
                INTEGER(jsizes)[i] = sizeof(double);
                break;
            case STRSXP :
                INTEGER(jsizes)[i] = sizeof(SEXP);
                break;
            default:
                error("only integer,double,logical and character vectors are allowed in j expressions when 'by' is used. Type %d would need to be added.", TYPEOF(VECTOR_ELT(testj, i)));
        }
    }
    
    // copy existing result for first group into ans
    ansloc = 0;
    maxn = 0;
    any0 = 0;
    for (j=0; j<njval; j++) {
        thislen = LENGTH(VECTOR_ELT(testj,j));
        maxn = thislen>maxn ? thislen : maxn;
        if (thislen == 0) any0 = 1;
    }
    if (maxn>0 && any0) {
        for (j=0; j<njval; j++) {
            thislen = LENGTH(VECTOR_ELT(testj,j));        
            if (thislen == 0) {
                // replace the 0-length vector with a 1-length NA to be recycled below to fit.
                switch (TYPEOF(VECTOR_ELT(testj, i))) {
                case LGLSXP :
                case INTSXP :
                    SET_VECTOR_ELT(testj,j,naint);
                    break;
                case REALSXP :
                    SET_VECTOR_ELT(testj,j,nareal);
                    break;
                case STRSXP :
                    SET_VECTOR_ELT(testj,j,NA_STRING);
                    break;
                default:
                    error("Logical error. Type of column should have been checked by now");
                }
            }
        }
    }
    if (maxn > 0) {
        if (ansloc + maxn > length(VECTOR_ELT(ans,0))) error("Didn't allocate enough rows for result of first group.");
        for (j=0; j<nbyval; j++) {
            for (r=0; r<maxn; r++) {
            memcpy((char *)DATAPTR(VECTOR_ELT(ans,j)) + (ansloc+r)*INTEGER(bysizes)[j],
               (char *)DATAPTR(VECTOR_ELT(byval,j)) + 0*INTEGER(bysizes)[j],    // this is why byval must be subset in the calling R to the first row of each group, to be consistent with i join grouping
               1 * INTEGER(bysizes)[j]);
            }
        }
        for (j=0; j<njval; j++) {
            thisansloc = ansloc;
            thislen = LENGTH(VECTOR_ELT(testj,j)); // never length 0 as replaced with NA above.
            if (maxn%thislen != 0) error("maxn (%d) is not exact multiple of this j column's length (%d)",maxn,thislen); 
            for (r=0; r<(maxn/thislen); r++) {
                memcpy((char *)DATAPTR(VECTOR_ELT(ans,j+nbyval)) + thisansloc*INTEGER(jsizes)[j],
                   (char *)DATAPTR(VECTOR_ELT(testj,j)),
                   thislen * INTEGER(jsizes)[j]);
                thisansloc += thislen;
            }
        }
        ansloc += maxn;
    }
    // end copy of testj into result
    
    for(i = 1; i < ngrp; i++) {  // 2nd group onwards
        if (length(order)==0) {
            for (j=0; j<length(SD); j++) {
                memcpy((char *)DATAPTR(VECTOR_ELT(SD,j)),
                       (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1))+(INTEGER(starts)[i]-1)*INTEGER(sizes)[j],
                       INTEGER(lens)[i]*INTEGER(sizes)[j]);
            }
        } else {
            for (k=0; k<INTEGER(lens)[i]; k++) {
                rownum = INTEGER(order)[ INTEGER(starts)[i] -1 + k ] -1;
                for (j=0; j<length(SD); j++) {    
                    memcpy((char *)DATAPTR(VECTOR_ELT(SD,j)) + k*INTEGER(sizes)[j],
                           (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1)) + rownum*INTEGER(sizes)[j],
                           INTEGER(sizes)[j]);
                }
            }
        }
        for (j=0; j<length(SD); j++) SETLENGTH(VECTOR_ELT(SD,j),INTEGER(lens)[i]);
        PROTECT(jval = eval(jexp, env));
        if (length(byval)+length(jval) != length(ans)) error("j doesn't evaluate to the same number of columns for each group");  // this would be a problem even if we unlisted afterwards. This way the user finds out earlier though so he can fix and rerun sooner.
        maxn = 0;
        any0 = 0;
        for (j=0; j<njval; j++) {
            thislen = LENGTH(VECTOR_ELT(jval,j));
            maxn = thislen>maxn ? thislen : maxn;
            if (TYPEOF(VECTOR_ELT(jval, j)) != TYPEOF(VECTOR_ELT(ans, j+nbyval))) error("columns of j don't evaluate to consistent types for each group");
            if (thislen == 0) any0 = 1;
        }
        if (maxn>0 && any0) {
            for (j=0; j<njval; j++) {
                thislen = LENGTH(VECTOR_ELT(jval,j));        
                if (thislen == 0) {
                    // replace the 0-length vector with a 1-length NA to be recycled below to fit.
                    switch (TYPEOF(VECTOR_ELT(jval, i))) {
                    case LGLSXP :
                    case INTSXP :
                        SET_VECTOR_ELT(jval,j,naint);
                        break;
                    case REALSXP :
                        SET_VECTOR_ELT(jval,j,nareal);
                        break;
                    case STRSXP :
                        SET_VECTOR_ELT(jval,j,NA_STRING);
                        break;
                    default:
                        error("Logical error. Type of column should have been checked by now");
                    }
                }
            }
        }
        if (ansloc + maxn > length(VECTOR_ELT(ans,0))) {
            newlen = ansloc+maxn;
            if (LOGICAL(verbose)[0]) Rprintf("dogroups: growing from %d to %d rows\n", length(VECTOR_ELT(ans,0)), newlen);
            for (j=0; j<nbyval; j++) SET_VECTOR_ELT(ans, j, growVector(VECTOR_ELT(ans,j), newlen, INTEGER(bysizes)[j]));
            for (j=0; j<njval; j++) SET_VECTOR_ELT(ans, j+nbyval, growVector(VECTOR_ELT(ans,j+nbyval), newlen, INTEGER(jsizes)[j]));
            // TO DO: implement R_realloc(?) here
            // TO DO: more sophisticated newlen estimate based on new group's size
            // TO DO: possibly inline it.
        }
        if (maxn > 0) {
            for (j=0; j<nbyval; j++) {
                for (r=0; r<maxn; r++) {
                    memcpy((char *)DATAPTR(VECTOR_ELT(ans,j)) + (ansloc+r)*INTEGER(bysizes)[j],
                        (char *)DATAPTR(VECTOR_ELT(byval,j)) + i*INTEGER(bysizes)[j],    // this is why byval must be subset in the calling R to the first row of each group, to be consistent with i join grouping
                        1 * INTEGER(bysizes)[j]);
                }
            }
            for (j=0; j<njval; j++) {
                thisansloc = ansloc;
                thislen = LENGTH(VECTOR_ELT(jval,j));
                if (maxn%thislen != 0) error("maxn (%d) is not exact multiple of this j column's length (%d)",maxn,thislen); 
                for (r=0; r<(maxn/thislen); r++) {
                    memcpy((char *)DATAPTR(VECTOR_ELT(ans,j+nbyval)) + thisansloc*INTEGER(jsizes)[j],
                       (char *)DATAPTR(VECTOR_ELT(jval,j)),
                       thislen * INTEGER(jsizes)[j]);
                    thisansloc += thislen;
                }
            }
            ansloc += maxn; 
        }
        UNPROTECT(1);  // do we really need the PROTECT/UNPROTECT given the macros above won't gc() ?
    }
    if (ansloc < length(VECTOR_ELT(ans,0))) {
        // warning("Wrote less rows than allocated. byretn=%d but wrote %d rows",INTEGER(byretn)[0],ansloc);
        for (j=0; j<length(ans); j++) SETLENGTH(VECTOR_ELT(ans,j),ansloc);
    }
    UNPROTECT(6);
    return(ans);
}


static SEXP growVector(SEXP x, R_len_t newlen, int size)
{
    // similar to EnlargeVector in src/main/subassign.c.
    // * replaced switch and loops with one memcpy
    // * no need to cater for names
    // * much shorter and faster
    R_len_t len;
    SEXP newx;
    len = length(x);
    PROTECT(x);
    PROTECT(newx = allocVector(TYPEOF(x), newlen));
    memcpy((char *)DATAPTR(newx), (char *)DATAPTR(x), len*size);
    UNPROTECT(2);
    return newx;
}



