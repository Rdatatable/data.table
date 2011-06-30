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

int sizes[100];  // max appears to be FUNSXP = 99, see Rinternals.h

static SEXP growVector(SEXP x, R_len_t newlen, int size);
int sizesSet=0;
void setSizes()
{
    int i;
    for (i=0;i++;i<100) sizes[i]=0;
    // only these types are currently allowed as column types :
    sizes[INTSXP] = sizeof(int);     // integer and factor
    sizes[LGLSXP] = sizeof(int);     // logical
    sizes[REALSXP] = sizeof(double); // numeric
    sizes[STRSXP] = sizeof(SEXP);    // character
    sizes[VECSXP] = sizeof(SEXP *);  // a column itself can be a list()
    sizesSet=1;
}
#define SIZEOF(x) sizes[TYPEOF(x)]

SEXP dogroups(SEXP dt, SEXP dtcols, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP testj, SEXP byretn, SEXP byval, SEXP itable, SEXP icols, SEXP iSD, SEXP nomatchNA, SEXP verbose)
{
    R_len_t i, j, k, rownum, ngrp, njval, nbyval, ansloc, maxn, r, thisansloc, thislen, any0, newlen, icol, size;
    SEXP names, inames, bynames, ans, jval, naint, nareal, SD, BY, N;
    if (!sizesSet) setSizes();
    if (TYPEOF(order) != INTSXP) error("order not integer");
    if (TYPEOF(starts) != INTSXP) error("starts not integer");
    if (TYPEOF(lens) != INTSXP) error("lens not integer");
    if (INTEGER(starts)[0]<0 || INTEGER(lens)[0]<1) error("starts[1]<0 or lens[1]<1");
    if(!isEnvironment(env)) error("’env’ should be an environment");
    ngrp = length(starts);  // the number of groups
    njval = length(testj);  // testj is now the result of j on the first group
    nbyval = length(byval);
    SD = findVar(install(".SD"), env);
    BY = findVar(install(".BY"), env);
    N = findVar(install(".N"), env);
    
    names = getAttrib(SD, R_NamesSymbol);
    for(i = 0; i < length(SD); i++) {
        if (SIZEOF(VECTOR_ELT(SD, i))==0)
            error("Type %d in .SD column %d", TYPEOF(VECTOR_ELT(SD, i)), i);
        defineVar(install(CHAR(STRING_ELT(names, i))), VECTOR_ELT(SD, i), env);
    }
    // defineVar(install(".SD"), SD, env);
    // By installing .SD directly inside itself, R finds this symbol more quickly (if used).

    inames = getAttrib(itable, R_NamesSymbol);
    for(i = 0; i < length(icols); i++) {
        // JIS (used non-key variables of i). The parent of SD isn't i, but it gives that appearance.
        icol = INTEGER(icols)[i]-1;
        defineVar(install(CHAR(STRING_ELT(inames,icol))),VECTOR_ELT(iSD,i),env);
        if (SIZEOF(VECTOR_ELT(iSD, i))==0)
            error("Type %d in join inherited scope column %d", TYPEOF(VECTOR_ELT(iSD, i)), i);
    }
    bynames = getAttrib(BY, R_NamesSymbol);
    for(i = 0; i < length(byval); i++) {
        // by vars can be used by name or via .BY
        defineVar(install(CHAR(STRING_ELT(bynames,i))),VECTOR_ELT(BY,i),env);
        if (TYPEOF(VECTOR_ELT(BY,i)) != TYPEOF(VECTOR_ELT(byval,i)))
            error("Type mismatch between .BY and byval, column %d",i);
        if (SIZEOF(VECTOR_ELT(BY,i))==0)
            error("Type %d in by column %d", TYPEOF(VECTOR_ELT(BY, i)), i);
    }
    
    PROTECT(ans = allocVector(VECSXP, nbyval + njval));
    
    PROTECT(naint = allocVector(INTSXP, 1));
    INTEGER(naint)[0] = NA_INTEGER;
    
    PROTECT(nareal = allocVector(REALSXP, 1));
    REAL(nareal)[0] = NA_REAL;
    
    for(i = 0; i < nbyval; i++) {
        SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(byval, i)), INTEGER(byretn)[0]));
        switch (TYPEOF(VECTOR_ELT(byval, i))) {   // this structure is more for the future when by could contain character for example
            case INTSXP :
            case LGLSXP :
                break;
            case REALSXP : case STRSXP :
            default:
                error("by columns must be integer (currently)");
        }
    }
    for(i = 0; i < njval; i++) {
        SET_VECTOR_ELT(ans, nbyval+i, allocVector(TYPEOF(VECTOR_ELT(testj, i)), INTEGER(byretn)[0]));
        if (SIZEOF(VECTOR_ELT(testj, i))==0)
            error("Type %d in j column", TYPEOF(VECTOR_ELT(testj, i)));
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
                switch (TYPEOF(VECTOR_ELT(testj, j))) {
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
                size = SIZEOF(VECTOR_ELT(byval, j));
                memcpy((char *)DATAPTR(VECTOR_ELT(ans,j)) + (ansloc+r)*size,
                       (char *)DATAPTR(VECTOR_ELT(byval,j)) + 0*size,    // this is why byval must be subset in the calling R to the first row of each group, to be consistent with i join grouping
                       1 * size);
            }
        }
        for (j=0; j<njval; j++) {
            thisansloc = ansloc;
            thislen = LENGTH(VECTOR_ELT(testj,j)); // never length 0 as replaced with NA above.
            if (maxn%thislen != 0) error("maxn (%d) is not exact multiple of this j column's length (%d)",maxn,thislen); 
            size = SIZEOF(VECTOR_ELT(testj, j));
            for (r=0; r<(maxn/thislen); r++) {
                memcpy((char *)DATAPTR(VECTOR_ELT(ans,j+nbyval)) + thisansloc*size,
                   (char *)DATAPTR(VECTOR_ELT(testj,j)),
                   thislen * size);
                thisansloc += thislen;
            }
        }
        ansloc += maxn;
    }
    // end copy of testj into result
    
    for(i = 1; i < ngrp; i++) {  // 2nd group onwards
        thislen = INTEGER(lens)[i];
        INTEGER(N)[0] = thislen;
        for (j=0; j<length(iSD); j++) {
            size = SIZEOF(VECTOR_ELT(iSD,j));
            memcpy((char *)DATAPTR(VECTOR_ELT(iSD,j)),
                   (char *)DATAPTR(VECTOR_ELT(itable,INTEGER(icols)[j]-1))+i*size,
                   size);
        }
        for (j=0; j<length(BY); j++) {
            size = SIZEOF(VECTOR_ELT(BY,j));
            memcpy((char *)DATAPTR(VECTOR_ELT(BY,j)),
                   (char *)DATAPTR(VECTOR_ELT(byval,j))+i*size,
                   size);
        }
        if (INTEGER(starts)[i] == 0) {
            if (LOGICAL(nomatchNA)[0]) {   // the is.na(nomatch) is up in R as its easier to cope with NA(logical), NA_integer_, 0L and 0 up there.
                for (j=0; j<length(SD); j++) {
                    switch (TYPEOF(VECTOR_ELT(SD, j))) {
                    case LGLSXP :
                        LOGICAL(VECTOR_ELT(SD,j))[0] = NA_LOGICAL;
                        break;
                    case INTSXP :
                        INTEGER(VECTOR_ELT(SD,j))[0] = NA_INTEGER;
                        break;
                    case REALSXP :
                        REAL(VECTOR_ELT(SD,j))[0] = NA_REAL;
                        break;
                    case STRSXP :
                        SET_STRING_ELT(VECTOR_ELT(SD,j),0,NA_STRING);
                        break;
                    default:
                        error("Logical error. Type of column should have been checked by now");
                    }
                }
                thislen = 1;
            } else {
                thislen = 0;
            }
        } else {
            if (length(order)==0) {
                for (j=0; j<length(SD); j++) {
                    size = SIZEOF(VECTOR_ELT(SD,j));
                    memcpy((char *)DATAPTR(VECTOR_ELT(SD,j)),
                       (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1))+(INTEGER(starts)[i]-1)*size,
                       thislen*size);
                }
            } else {
                for (k=0; k<thislen; k++) {
                    rownum = INTEGER(order)[ INTEGER(starts)[i] -1 + k ] -1;
                    for (j=0; j<length(SD); j++) {
                        size = SIZEOF(VECTOR_ELT(SD,j));
                        memcpy((char *)DATAPTR(VECTOR_ELT(SD,j)) + k*size,
                           (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1)) + rownum*size,
                           size);
                    }
                }
            }
        }
        for (j=0; j<length(SD); j++) SETLENGTH(VECTOR_ELT(SD,j),thislen);
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
                    switch (TYPEOF(VECTOR_ELT(jval, j))) {
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
            for (j=0; j<nbyval; j++) SET_VECTOR_ELT(ans, j, growVector(VECTOR_ELT(ans,j), newlen, SIZEOF(VECTOR_ELT(byval, j))));
            for (j=0; j<njval; j++) SET_VECTOR_ELT(ans, j+nbyval, growVector(VECTOR_ELT(ans,j+nbyval), newlen, SIZEOF(VECTOR_ELT(testj,j))));
            // TO DO: implement R_realloc(?) here
            // TO DO: more sophisticated newlen estimate based on new group's size
            // TO DO: possibly inline it.
        }
        if (maxn > 0) {
            for (j=0; j<nbyval; j++) {
                size = SIZEOF(VECTOR_ELT(byval, j));
                for (r=0; r<maxn; r++) {
                    memcpy((char *)DATAPTR(VECTOR_ELT(ans,j)) + (ansloc+r)*size,
                        (char *)DATAPTR(VECTOR_ELT(byval,j)) + i*size,    // this is why byval must be subset in the calling R to the first row of each group, to be consistent with i join grouping
                        1 * size);
                }
            }
            for (j=0; j<njval; j++) {
                thisansloc = ansloc;
                thislen = LENGTH(VECTOR_ELT(jval,j));
                if (maxn%thislen != 0) error("maxn (%d) is not exact multiple of this j column's length (%d)",maxn,thislen);
                size = SIZEOF(VECTOR_ELT(testj,j));
                for (r=0; r<(maxn/thislen); r++) {
                    memcpy((char *)DATAPTR(VECTOR_ELT(ans,j+nbyval)) + thisansloc*size,
                       (char *)DATAPTR(VECTOR_ELT(jval,j)),
                       thislen * size);
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
    UNPROTECT(3);
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





