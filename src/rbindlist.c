#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

SEXP keepattr();
int sizes[100];
#define SIZEOF(x) sizes[TYPEOF(x)]

SEXP rbindlist(SEXP l)
{
    R_len_t i,j,r, nrow=0, first=-1, ansloc, ncol=0, thislen;
    SEXP ans, li, lf=R_NilValue, thiscol, target, levels;
    int size;
    Rboolean coerced=FALSE, bindFactor=FALSE;
    
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
        } else {
            if (length(li) != ncol) error("Item %d has %d columns, inconsistent with item %d which has %d columns",i+1,length(li),first+1,ncol);
        }
        nrow+=LENGTH(VECTOR_ELT(li,0));
    }
    PROTECT(ans = allocVector(VECSXP, ncol));
    setAttrib(ans, R_NamesSymbol, getAttrib(lf, R_NamesSymbol));
    for(j=0; j<ncol; j++) {
        thiscol = VECTOR_ELT(lf,j);
        if (isFactor(thiscol)) {
            bindFactor = TRUE;
            target = allocVector(STRSXP, nrow);  // collate as string then factorize afterwards
        } else {
            bindFactor = FALSE;
            target = keepattr(allocVector(TYPEOF(thiscol), nrow), thiscol);
        }
        SET_VECTOR_ELT(ans, j, target);
        ansloc = 0;
        for (i=first; i<length(l); i++) {
            li = VECTOR_ELT(l,i);
            if (!length(li)) continue;  // majority of time though, each item of l is populated
            thislen = length(VECTOR_ELT(li,0));  // type of li was checked to be VECSXP already above
            if (!thislen) continue;
            thiscol = VECTOR_ELT(li,j);
            if (thislen != length(thiscol)) error("Column %d of item %d is length %d, inconsistent with first column of that item which is length %d. rbindlist doesn't recycle as it already expects each item to be a uniform list, data.frame or data.table", j+1, i+1, length(thiscol), thislen);
            //if (bindFactor && !isFactor(thiscol)) error("Column %d of item %d is not type factor, inconsistent with the first item where this column was factor", j+1, i+1);
            if (TYPEOF(thiscol) != TYPEOF(target) && !bindFactor && !isFactor(thiscol)) {
                thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));
                coerced = TRUE;
                // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
                // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
            }
            switch(TYPEOF(target)) {
            case STRSXP :
                if (isFactor(thiscol)) {
                    levels = getAttrib(thiscol, R_LevelsSymbol);
                    for (r=0; r<thislen; r++) SET_STRING_ELT(target, ansloc+r, STRING_ELT(levels,INTEGER(thiscol)[r]-1));
                } else {
                    for (r=0; r<thislen; r++) SET_STRING_ELT(target, ansloc+r, STRING_ELT(thiscol,r));
                }
                break;
            case VECSXP :
                for (r=0; r<thislen; r++)
                    SET_VECTOR_ELT(target, ansloc+r, VECTOR_ELT(thiscol,r));
                break;
            case REALSXP:
            case INTSXP:
            case LGLSXP:
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
        if (bindFactor) {
            SETCAR(CDR(factorLangSxp), target);
            SET_VECTOR_ELT(ans, j, eval(factorLangSxp, R_GlobalEnv));
        }
    }
    UNPROTECT(2);  // ans and factorLangSxp
    return(ans);
}


