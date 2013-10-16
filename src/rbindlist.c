#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

int sizes[100];
#define SIZEOF(x) sizes[TYPEOF(x)]

SEXP rbindlist(SEXP l)
{
    R_len_t i,j,r, nrow=0, first=-1, ansloc, ncol=0, thislen;
    SEXP ans, li, lf=R_NilValue, thiscol, target, levels;
    int size;
    Rboolean coerced=FALSE;
    SEXPTYPE * maxtype = NULL;     // TODO: Should these be an R object instead of malloc/free'ing a  pointer?
    Rboolean * isColFactor = NULL; // Same comment as above
    SEXPTYPE type;
    
    SEXP factorLangSxp;
    PROTECT(factorLangSxp = allocList(2));
    SET_TYPEOF(factorLangSxp, LANGSXP);
    SETCAR(factorLangSxp, install("factor"));
    
    for (i=0;i<length(l);i++) {
        li = VECTOR_ELT(l,i);
        if (isNull(li)) continue;
        if (TYPEOF(li) != VECSXP) error("Item %d of list input is not a data.frame, data.table or list",i+1);
        if (!LENGTH(li) || !length(VECTOR_ELT(li,0))) continue;
        if (first==-1) {
            first = i;   // First non-empty list/data.frame/data.table
            lf = li;
            ncol = length(lf);

            // initialize the max types - will possibly increment later
            maxtype = malloc(ncol * sizeof(SEXPTYPE));
            isColFactor = malloc(ncol * sizeof(Rboolean));
            for (j = 0; j < ncol; ++j) {
                maxtype[j] = TYPEOF(VECTOR_ELT(lf, j));
                isColFactor[j] = FALSE;
            }
        } else {
            if (length(li) != ncol) error("Item %d has %d columns, inconsistent with item %d which has %d columns",i+1,length(li),first+1,ncol);
        }
        nrow+=LENGTH(VECTOR_ELT(li,0));

        // we do this twice for the first list element - will add a tiny performance overhead
        for (j = 0; j < ncol; ++j) {
            thiscol = VECTOR_ELT(li, j);
            if (isFactor(thiscol)) {
                isColFactor[j] = TRUE;
                maxtype[j] = STRSXP;   // if any column is a factor everything will be converted to strings and factorized at the end
            } else {
                type = TYPEOF(thiscol);
                if (type > maxtype[j]) maxtype[j] = type;
            }
        }
    }
    PROTECT(ans = allocVector(VECSXP, ncol));
    setAttrib(ans, R_NamesSymbol, getAttrib(lf, R_NamesSymbol));
    for(j=0; j<ncol; j++) {
        thiscol = VECTOR_ELT(lf,j);
        target = allocVector(maxtype[j], nrow);
        if (!isFactor(thiscol))
          copyMostAttrib(thiscol, target);  // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.

        SET_VECTOR_ELT(ans, j, target);
        ansloc = 0;
        for (i=first; i<length(l); i++) {
            li = VECTOR_ELT(l,i);
            if (!length(li)) continue;  // majority of time though, each item of l is populated
            thislen = length(VECTOR_ELT(li,0));  // type of li was checked to be VECSXP already above
            if (!thislen) continue;
            thiscol = VECTOR_ELT(li,j);
            if (thislen != length(thiscol)) error("Column %d of item %d is length %d, inconsistent with first column of that item which is length %d. rbindlist doesn't recycle as it already expects each item to be a uniform list, data.frame or data.table", j+1, i+1, length(thiscol), thislen);
            
            if (TYPEOF(thiscol) != TYPEOF(target) && !isFactor(thiscol)) {
                thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));
                coerced = TRUE;
                // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
                // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
            }
            switch(TYPEOF(target)) {
            case STRSXP :
                if (isFactor(thiscol)) {
                    levels = getAttrib(thiscol, R_LevelsSymbol);
                    for (r=0; r<thislen; r++)
                        if (INTEGER(thiscol)[r]==NA_INTEGER)
                            SET_STRING_ELT(target, ansloc+r, NA_STRING);
                        else
                            SET_STRING_ELT(target, ansloc+r, STRING_ELT(levels,INTEGER(thiscol)[r]-1));
                } else {
                    if (TYPEOF(thiscol) != STRSXP) error("Internal logical error in rbindlist.c (not STRSXP), please report to datatable-help.");
                    for (r=0; r<thislen; r++) SET_STRING_ELT(target, ansloc+r, STRING_ELT(thiscol,r));
                }
                break;
            case VECSXP :
                if (TYPEOF(thiscol) != VECSXP) error("Internal logical error in rbindlist.c (not VECSXP), please report to datatable-help.");
                for (r=0; r<thislen; r++)
                    SET_VECTOR_ELT(target, ansloc+r, VECTOR_ELT(thiscol,r));
                break;
            case REALSXP:
            case INTSXP:
            case LGLSXP:
                if (TYPEOF(thiscol) != TYPEOF(target)) error("Internal logical error in rbindlist.c (thiscol's type should have been coerced to target), please report to datatable-help.");
                size = SIZEOF(thiscol);
                memcpy((char *)DATAPTR(target) + ansloc*size,
                       (char *)DATAPTR(thiscol),
                       thislen * size);
                break;
            default :
                error("Unsupported column type '%s'", type2char(TYPEOF(target))); 
            }
            ansloc += thislen;
            if (coerced) {
                UNPROTECT(1);
                coerced = FALSE;
            }
        }
        if (isColFactor[j]) {
            SETCAR(CDR(factorLangSxp), target);
            SET_VECTOR_ELT(ans, j, eval(factorLangSxp, R_GlobalEnv));
        }
    }
    UNPROTECT(2);  // ans and factorLangSxp

    free(maxtype);
    free(isColFactor);

    return(ans);
}


