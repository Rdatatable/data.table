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
SEXP SelfRefSymbol;

SEXP growVector(SEXP x, R_len_t newlen, Rboolean verbose);
int sizesSet=0;

void setSizes()
{
    int i;
    for (i=0;i<100;i++) sizes[i]=0;
    // only these types are currently allowed as column types :
    sizes[INTSXP] = sizeof(int);     // integer and factor
    sizes[LGLSXP] = sizeof(int);     // logical
    sizes[REALSXP] = sizeof(double); // numeric
    sizes[STRSXP] = sizeof(SEXP *);  // character
    sizes[VECSXP] = sizeof(SEXP *);  // a column itself can be a list()
    for (i=0;i<100;i++) {
        if (sizes[i]>8) error("Type %d is sizeof() greater than 8 bytes on this machine. We haven't tested on any architecture greater than 64bit, yet.", i);
        // One place we need the largest sizeof (assumed to be 8 bytes) is the working memory malloc in reorder.c
    }
    sizesSet=1;
    SelfRefSymbol = install(".internal.selfref");
}
#define SIZEOF(x) sizes[TYPEOF(x)]


SEXP dogroups(SEXP dt, SEXP dtcols, SEXP groups, SEXP grpcols, SEXP jiscols, SEXP irows, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP testj, SEXP byretn, SEXP verbose)
{
    R_len_t i, j, k, rownum, ngrp, njval, ngrpcols, ansloc, maxn, r, thisansloc, thislen, any0, newlen, igrp, grpcol, size;
    SEXP names, bynames, ans, jval, naint, nareal, SD, BY, N, iSD, rownames, s;
    SEXP *nameSyms;
    if (!sizesSet) setSizes();
    if (TYPEOF(order) != INTSXP) error("Internal error: order not integer");
    //if (TYPEOF(starts) != INTSXP) error("Internal error: starts not integer");
    //if (TYPEOF(lens) != INTSXP) error("Internal error: lens not integer");
    // starts can now be NA (<0): if (INTEGER(starts)[0]<0 || INTEGER(lens)[0]<0) error("starts[1]<0 or lens[1]<0");
    if (!isNull(jiscols) && length(order)) error("Internal error: jiscols not NULL but o__ has length");
    if(!isEnvironment(env)) error("’env’ should be an environment");
    ngrp = length(starts);  // the number of groups  (nrow(groups) will be larger when by)
    njval = length(testj);  // the result of j on the first group was done up in R and passed in here
    ngrpcols = length(grpcols);
    SD = findVar(install(".SD"), env);
    BY = findVar(install(".BY"), env);
    N = findVar(install(".N"), env);
    iSD = findVar(install(".iSD"), env);  // 1-row and possibly no cols (if no i variables are used via JIS)
    
    // fetch rownames of .SD.  rownames[1] is set to -thislen for each group, in case .SD is passed to
    // non data.table aware package that uses rownames
    for (s = ATTRIB(SD); s != R_NilValue && TAG(s)!=R_RowNamesSymbol; s = CDR(s));
    // getAttrib0 basically but that's hidden in attrib.c
    if (s==R_NilValue) error("row.names attribute of .SD not found");
    rownames = CAR(s);
    if (!isInteger(rownames) || LENGTH(rownames)!=2 || INTEGER(rownames)[0]!=NA_INTEGER) error("row.names of .SD isn't integer length 2 with NA as first item; i.e., .set_row_names(). [%s %d %d]",type2char(TYPEOF(rownames)),LENGTH(rownames),INTEGER(rownames)[0]);
    
    // fetch names of .SD and prepare symbols. In case they are copied-on-write by user assigning to those variables
    // using <- in j (which is valid, useful and tested), they are repointed to the .SD cols for each group.
    names = getAttrib(SD, R_NamesSymbol);
    if (length(names) != length(SD)) error("length(names)!=length(SD)");
    nameSyms = Calloc(length(names), SEXP);
    if (!nameSyms) error("Calloc failed to allocate %d nameSyms in dogroups",length(names));
    for(i = 0; i < length(SD); i++) {
        if (SIZEOF(VECTOR_ELT(SD, i))==0)
            error("Type %d in .SD column %d", TYPEOF(VECTOR_ELT(SD, i)), i);
        nameSyms[i] = install(CHAR(STRING_ELT(names, i)));
    }

    if (length(iSD)!=length(jiscols)) error("length(iSD)[%d] != length(jiscols)[%d]",length(iSD),length(jiscols));
    
    /*
    inames = getAttrib(itable, R_NamesSymbol);  // the whole i table
    if (length(icols)>length(inames)) error("length(icols)[%d] > length(inames)[%d]",length(icols),length(inames));
    if (length(icols)!=length(idotnames)) error("length(icols)[%d] != length(idotnames)[%d]",length(icols),length(idotnames));
    for(i = 0; i < length(icols); i++) {
        // JIS (used non-key variables of i). The parent of SD isn't i, but this gives that appearance.
        icol = INTEGER(icols)[i]-1;
        if (icol>=length(inames)) error("icol[%d] >= length(inames)[%d]", icol, length(inames));
        defineVar(install(CHAR(STRING_ELT(inames,icol))),VECTOR_ELT(iSD,i),env);
        if (SIZEOF(VECTOR_ELT(iSD, i))==0)
            error("Type %d in join inherited scope column %d", TYPEOF(VECTOR_ELT(iSD, i)), i);
        defineVar(install(CHAR(STRING_ELT(idotnames,i))),VECTOR_ELT(iSD,i),env);
    }*/
    // TO DO: should .BY be available in bywithoutby too?
    bynames = getAttrib(BY, R_NamesSymbol);
    if (isNull(jiscols) && (length(bynames)!=length(groups) || length(bynames)!=length(grpcols))) error("!length(bynames)[%d]==length(groups)[%d]==length(grpcols)[%d]",length(bynames),length(groups),length(grpcols));
    // No .BY when bywithoutby
    for(i = 0; i < length(BY); i++) {
        // by vars can be used by name or via .BY
        defineVar(install(CHAR(STRING_ELT(bynames,i))),VECTOR_ELT(BY,i),env);
        if (TYPEOF(VECTOR_ELT(BY,i)) != TYPEOF(VECTOR_ELT(groups,i)))
            error("Type mismatch between .BY and groups, column %d",i);
        if (SIZEOF(VECTOR_ELT(BY,i))==0)
            error("Type %d in by column %d", TYPEOF(VECTOR_ELT(BY, i)), i);
    }
    
    PROTECT(ans = allocVector(VECSXP, ngrpcols + njval));
    
    PROTECT(naint = allocVector(INTSXP, 1));
    INTEGER(naint)[0] = NA_INTEGER;
    
    PROTECT(nareal = allocVector(REALSXP, 1));
    REAL(nareal)[0] = NA_REAL;
    
    for(i = 0; i < ngrpcols; i++) {
        grpcol = INTEGER(grpcols)[i]-1;
        SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(VECTOR_ELT(groups, grpcol)), INTEGER(byretn)[0]));
        switch (TYPEOF(VECTOR_ELT(groups, grpcol))) {   // this structure is more for the future when by could contain character for example
            case INTSXP :
            case LGLSXP :
            case STRSXP :
            case REALSXP :
                break;
            default:
                error("Unsupported type '%s' of group columnn '%s'", type2char(TYPEOF(VECTOR_ELT(groups, grpcol))), CHAR(STRING_ELT(getAttrib(groups, R_NamesSymbol),grpcol)));
        }
    }
    for(i = 0; i < njval; i++) {
        if (isNull(VECTOR_ELT(testj, i)))
            error("Column %d of j's result for the first group is NULL. We rely on the column types of the first result to decide the type expected for the remaining groups (and require consistency). NULL columns are acceptable for later groups (and those are replaced with NA of appropriate type and recycled) but not for the first. Please use a typed empty vector instead, such as integer() or numeric().", i+1);
        if (SIZEOF(VECTOR_ELT(testj, i))==0)
            error("Unsupported type '%s' in j column %d", type2char(TYPEOF(VECTOR_ELT(testj, i))), i+1);
        SET_VECTOR_ELT(ans, ngrpcols+i, allocVector(TYPEOF(VECTOR_ELT(testj, i)), INTEGER(byretn)[0]));
    }
    
    // copy existing result for first group into ans
    // TO DO: treat first group the same and move byretn logic (best guess at final size) 
    ansloc = 0;
    maxn = 0;
    any0 = 0;
    for (j=0; j<njval; j++) {
        thislen = LENGTH(VECTOR_ELT(testj,j));  // checked not NULL above
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
        igrp = length(order) ? INTEGER(order)[INTEGER(starts)[0]-1]-1 : (isNull(jiscols) ? INTEGER(starts)[0]-1 : 0);
        // just first group here.
        // null icols => grouping by by (where by and x irows line up). Grouping by i has non-null jiscols even when JIS not used (jiscols=integer())
        for (j=0; j<ngrpcols; j++) {
            grpcol = INTEGER(grpcols)[j]-1;
            size = SIZEOF(VECTOR_ELT(groups, grpcol));
            for (r=0; r<maxn; r++) {
                memcpy((char *)DATAPTR(VECTOR_ELT(ans,j)) + (ansloc+r)*size,
                       (char *)DATAPTR(VECTOR_ELT(groups,grpcol)) + igrp*size,
                       1 * size);
            }
        }
        for (j=0; j<njval; j++) {
            thisansloc = ansloc;
            thislen = LENGTH(VECTOR_ELT(testj,j)); // never length 0 as replaced with NA above.
            if (maxn%thislen != 0) error("maxn (%d) is not exact multiple of this j column's length (%d)",maxn,thislen); 
            size = SIZEOF(VECTOR_ELT(testj, j));
            if (TYPEOF(VECTOR_ELT(ans,j+ngrpcols)) != TYPEOF(VECTOR_ELT(testj,j))) error("Type mismatch in copying testj into ans");
            switch (TYPEOF(VECTOR_ELT(testj,j))) {
            case STRSXP :
                for (r=0; r<maxn; r++)
                    SET_STRING_ELT(VECTOR_ELT(ans,j+ngrpcols), thisansloc+r, STRING_ELT(VECTOR_ELT(testj,j),r%thislen));
                break;
            case VECSXP :
                for (r=0; r<maxn; r++)
                    SET_VECTOR_ELT(VECTOR_ELT(ans,j+ngrpcols), thisansloc+r, VECTOR_ELT(VECTOR_ELT(testj,j),r%thislen));
                break;
            default :
                for (r=0; r<(maxn/thislen); r++) {
                    memcpy((char *)DATAPTR(VECTOR_ELT(ans,j+ngrpcols)) + thisansloc*size,
                           (char *)DATAPTR(VECTOR_ELT(testj,j)),
                           thislen * size);
                    thisansloc += thislen;
                }
            }
        }
        ansloc += maxn;
    }
    // end copy of testj into result
    
    for(i = 1; i < ngrp; i++) {  // 2nd group onwards
        if (INTEGER(starts)[i] == 0) continue;   // nomatch 0 and this group has no match. 0's need to be present
                                                 // so that i lines up with f__
        thislen = INTEGER(lens)[i];
        for (j=0; j<length(iSD); j++) {   // either this or the next for() will run, not both
            size = SIZEOF(VECTOR_ELT(iSD,j));
            memcpy((char *)DATAPTR(VECTOR_ELT(iSD,j)),
                   (char *)DATAPTR(VECTOR_ELT(groups,INTEGER(jiscols)[j]-1))+i*size,
                   size);
        }
        // to delete ... igrp = length(order) ? INTEGER(order)[INTEGER(starts)[i]-1]-1 : INTEGER(starts)[i]-1;
        igrp = length(order) ? INTEGER(order)[INTEGER(starts)[i]-1]-1 : (isNull(jiscols) ? INTEGER(starts)[i]-1 : i);
        for (j=0; j<length(BY); j++) {
            size = SIZEOF(VECTOR_ELT(BY,j));
            memcpy((char *)DATAPTR(VECTOR_ELT(BY,j)),
                   (char *)DATAPTR(VECTOR_ELT(groups,j))+igrp*size,
                   size);
        }
        if (INTEGER(starts)[i] == NA_INTEGER) {
            // TO DO..delete... if (LOGICAL(nomatchNA)[0]) {
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
            //TO DO..delete... } else {
            //    continue;
            //}
        } else {
            if (length(order)==0) {
                rownum = INTEGER(starts)[i]-1;
                if (!isNull(irows)) rownum = INTEGER(irows)[rownum] -1;
                for (j=0; j<length(SD); j++) {
                    size = SIZEOF(VECTOR_ELT(SD,j));
                    memcpy((char *)DATAPTR(VECTOR_ELT(SD,j)),
                       (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1))+rownum*size,
                       thislen*size);
                }
            } else {
                for (k=0; k<thislen; k++) {
                    rownum = INTEGER(order)[ INTEGER(starts)[i]-1 + k ] -1;
                    if (!isNull(irows)) rownum = INTEGER(irows)[rownum] -1;
                    for (j=0; j<length(SD); j++) {
                        size = SIZEOF(VECTOR_ELT(SD,j));
                        memcpy((char *)DATAPTR(VECTOR_ELT(SD,j)) + k*size,
                           (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1)) + rownum*size,
                           size);
                    }
                }
            }
        }
        // TO DO: delete ...INTEGER(N)[0] = INTEGER(starts)[i] == 0 ? 0 : INTEGER(lens)[i];  
        
        INTEGER(N)[0] = INTEGER(starts)[i] == NA_INTEGER ? 0 : INTEGER(lens)[i];
        // .N is number of rows matched to ( 0 even when nomatch is NA)
        
        INTEGER(rownames)[1] = -thislen;  // the .set_row_names() of .SD
        for (j=0; j<length(SD); j++) {
            LENGTH(VECTOR_ELT(SD,j)) = thislen;
            defineVar(nameSyms[j], VECTOR_ELT(SD, j), env);
            // In case user's j assigns to the columns names (env is static) (tests 387 and 388)
            // nameSyms pre-stored to save repeated install() for efficiency.
        }
        PROTECT(jval = eval(jexp, env));
        // length(jval) may be 0 when j is plot or other side-effect only. Otherwise, even NULL in user j
        // will be auto wrapped to make list(NULL) (length 1)
        maxn = 0;
        any0 = 0;
        if (length(jval)>1 || length(VECTOR_ELT(jval,0))) {
            if (ngrpcols+length(jval) != length(ans)) error("j doesn't evaluate to the same number of columns for each group");  // this would be a problem even if we unlisted afterwards. This way the user finds out earlier though so he can fix and rerun sooner.
            if (length(jval) != njval) error("Internal logical error: length(jval)!=njval"); // Line above should be equivalent
            for (j=0; j<njval; j++) {
                thislen = length(VECTOR_ELT(jval,j));  // might be NULL, so length not LENGTH
                maxn = thislen>maxn ? thislen : maxn;
                if (TYPEOF(VECTOR_ELT(jval, j)) != TYPEOF(VECTOR_ELT(ans, j+ngrpcols))
                    && TYPEOF(VECTOR_ELT(jval, j)) != NILSXP) error("columns of j don't evaluate to consistent types for each group: result for group %d has column %d type '%s' but expecting type '%s'", i+1, j+1, type2char(TYPEOF(VECTOR_ELT(jval, j))), type2char(TYPEOF(VECTOR_ELT(ans, j+ngrpcols))));
                if (thislen == 0) any0 = 1;
            }
        } // else j was NULL or data.table(NULL) etc (tests 361-364)
        if (maxn>0 && any0) {
            // There is a correct number of columns, but one or more is length 0 (e.g. NULL). Different to when
            // user wants no rows at all for some groups (and achieves that by arranging j to return NULL).
            for (j=0; j<njval; j++) {
                thislen = length(VECTOR_ELT(jval,j));        
                if (thislen == 0) {
                    // replace the 0-length vector with a 1-length NA to be recycled below to fit.
                    switch (TYPEOF(VECTOR_ELT(ans, j+ngrpcols))) {
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
            for (j=0; j<ngrpcols; j++) SET_VECTOR_ELT(ans, j, growVector(VECTOR_ELT(ans,j), newlen, LOGICAL(verbose)[0]));
            for (j=0; j<njval; j++) SET_VECTOR_ELT(ans, j+ngrpcols, growVector(VECTOR_ELT(ans,j+ngrpcols), newlen, LOGICAL(verbose)[0]));
            // TO DO: implement R_realloc(?) here
            // TO DO: more sophisticated newlen estimate based on new group's size
            // TO DO: possibly inline it.
        }
        if (maxn > 0) {
            // to delete ..  igrp = length(order) ? INTEGER(order)[INTEGER(starts)[i]-1]-1 : (isNull(icols) ? INTEGER(starts)[i]-1 : i);
            for (j=0; j<ngrpcols; j++) {
                size = SIZEOF(VECTOR_ELT(groups, j));
                for (r=0; r<maxn; r++) {
                    memcpy((char *)DATAPTR(VECTOR_ELT(ans,j)) + (ansloc+r)*size,
                        (char *)DATAPTR(VECTOR_ELT(groups,j)) + igrp*size,
                        1 * size);
                }
            }
            for (j=0; j<njval; j++) {
                thisansloc = ansloc;
                thislen = LENGTH(VECTOR_ELT(jval,j));  // Any NULL was replaced by an NA above.
                if (maxn%thislen != 0) error("maxn (%d) is not exact multiple of this j column's length (%d)",maxn,thislen);
                if (TYPEOF(VECTOR_ELT(testj,j))==STRSXP) {
                    for (r=0; r<maxn; r++) {
                        SET_STRING_ELT(VECTOR_ELT(ans,j+ngrpcols), thisansloc, STRING_ELT(VECTOR_ELT(jval,j),r%thislen));
                        thisansloc++;
                    }
                    continue;
                    // TO DO: Replace SET_STRING_ELT and SET_VECTOR_ELT with direct CHK and CHECK_OLD_TO_NEW calls,
                    // once per item in jval, then the rest can be memcpy'd after?. But, it seems we do need to
                    // ensure objects are aged.
                }
                if (TYPEOF(VECTOR_ELT(testj,j))==VECSXP) {
                    for (r=0; r<maxn; r++) {
                        SET_VECTOR_ELT(VECTOR_ELT(ans,j+ngrpcols), thisansloc, VECTOR_ELT(VECTOR_ELT(jval,j),r%thislen));
                        thisansloc++;
                    }
                    continue;
                }
                // else integer or real
                size = SIZEOF(VECTOR_ELT(testj,j));
                for (r=0; r<(maxn/thislen); r++) {
                    memcpy((char *)DATAPTR(VECTOR_ELT(ans,j+ngrpcols)) + thisansloc*size,
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
        // Rprintf("Wrote less rows than allocated. byretn=%d but wrote %d rows\n",INTEGER(byretn)[0],ansloc);
        for (j=0; j<length(ans); j++) LENGTH(VECTOR_ELT(ans,j)) = ansloc;
        // TO DO: set truelength here (uninitialized by R) to save growing next time on insert() 
        // Important not to touch truelength for CHARSXP in R's global string cache, but we won't see those here,
        // only true vectors such as STRSXP.
    }
    UNPROTECT(3);
    Free(nameSyms);
    return(ans);
}


SEXP growVector(SEXP x, R_len_t newlen, Rboolean verbose)
{
    // similar to EnlargeVector in src/main/subassign.c.
    // * replaced switch and loops with one memcpy for INTEGER and REAL, but need to age CHAR and VEC.
    // * no need to cater for names
    // * much shorter and faster
    SEXP newx;
    R_len_t i, len = length(x);
    PROTECT(newx = allocVector(TYPEOF(x), newlen));
    switch (TYPEOF(x)) {
    case STRSXP :
        for (i=0; i<len; i++)
            SET_STRING_ELT(newx, i, STRING_ELT(x, i));
            // Using SET_ to ensure objects are aged, rather than memcpy. Perhaps theres a bulk/fast way to age CHECK_OLD_TO_NEW
        break;
    case VECSXP :
        for (i=0; i<len; i++)
            SET_VECTOR_ELT(newx, i, VECTOR_ELT(x, i));
        break;
    default :
        memcpy((char *)DATAPTR(newx), (char *)DATAPTR(x), len*SIZEOF(x));
    }
    if (verbose) Rprintf("Growing vector from %d to %d items of type '%s'\n", len, newlen, type2char(TYPEOF(x)));
    UNPROTECT(1);
    return newx;
}


