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

SEXP dogroups(SEXP dt, SEXP SD, SEXP dtcols, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP testj, SEXP byretn, SEXP byval)
{
    R_len_t i, j, k, rownum, ngrp, njval, nbyval, ansloc, maxn, r, thisansloc, thislen;
    SEXP names, ans, sizes, jval, jsizes, bysizes;
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
    for(i = 0; i < nbyval; i++) {
        SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(byval, i)), INTEGER(byretn)[0]));
        switch (TYPEOF(VECTOR_ELT(byval, i))) {   // this structure is more for the future when by could contain character for example
            case INTSXP :
                INTEGER(bysizes)[i] = sizeof(int);
                break;
            case LGLSXP : case REALSXP : case STRSXP :
            default:
                error("by currently must only be integer columns.");
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
    for (j=0; j<njval; j++) {
        thislen = LENGTH(VECTOR_ELT(testj,j));
        maxn = thislen>maxn ? thislen : maxn;
    }
    // if maxn is 0 the following loops will fall through  (for side-effect only j)
    if (ansloc + maxn > INTEGER(byretn)[0]) error("Didn't allocate enough rows for result of first group.");
    for (j=0; j<nbyval; j++) {
        for (r=0; r<maxn; r++) {
        memcpy((char *)DATAPTR(VECTOR_ELT(ans,j)) + (ansloc+r)*INTEGER(bysizes)[j],
               (char *)DATAPTR(VECTOR_ELT(byval,j)) + 0*INTEGER(bysizes)[j],    // this is why byval must be subset in the calling R to the first row of each group, to be consistent with i join grouping
               1 * INTEGER(bysizes)[j]);
        }
    }
    for (j=0; j<njval; j++) {
        thisansloc = ansloc;
        thislen = LENGTH(VECTOR_ELT(testj,j));
        if (maxn%thislen != 0) error("maxn (%d) is not exact multiple of this j column's length (%d)",maxn,thislen); 
        for (r=0; r<(maxn/thislen); r++) {
            memcpy((char *)DATAPTR(VECTOR_ELT(ans,j+nbyval)) + thisansloc*INTEGER(jsizes)[j],
                   (char *)DATAPTR(VECTOR_ELT(testj,j)),
                   thislen * INTEGER(jsizes)[j]);
            thisansloc += thislen;
        }
    }
    // end copy of testj into result
    ansloc += maxn;
    
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
        for (j=0; j<njval; j++) {
            thislen = LENGTH(VECTOR_ELT(jval,j));
            maxn = thislen>maxn ? thislen : maxn;
            if (TYPEOF(VECTOR_ELT(jval, j)) != TYPEOF(VECTOR_ELT(ans, j+nbyval))) error("columns of j don't evaluate to consistent types for each group");
        }
        if (ansloc + maxn > INTEGER(byretn)[0]) error("Didn't allocate enough rows. Must grow ans (to implement as we don't want default slow grow)");
        // TO DO: implement R_realloc(?) here
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
        UNPROTECT(1);  // do we really need the PROTECT/UNPROTECT given the macros above won't gc() ?
    }
    if (ansloc > INTEGER(byretn)[0]) error("Logical error as we should have stopped above before writing the last jval");
    if (ansloc < INTEGER(byretn)[0]) {
        // warning("Wrote less rows than allocated. byretn=%d but wrote %d rows",INTEGER(byretn)[0],ansloc);
        for (j=0; j<length(ans); j++) SETLENGTH(VECTOR_ELT(ans,j),ansloc);
    }
    UNPROTECT(4);
    return(ans);
}



