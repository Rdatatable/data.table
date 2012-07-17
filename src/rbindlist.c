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
    SEXP ans, li, lf=R_NilValue, thiscol, target;
    int size;
    Rboolean coerced=FALSE;
    
    for (i=0;i<length(l);i++) {
        li = VECTOR_ELT(l,i);
        if (isNull(li)) continue;
        if (TYPEOF(li) != VECSXP) error("Item %d of list is not a list",i+1);
        if (first==-1) {
            first = i;   // First non-NULL
            lf = li;
            ncol = length(lf);
        } else {
            if (length(li) != ncol) error("Item %d has %d columns, inconsistent with item %d which has %d",i+1,length(li),first+1,ncol);
        }
        nrow+=length(VECTOR_ELT(li,0));
    }
    PROTECT(ans = allocVector(VECSXP, ncol));
    setAttrib(ans, R_NamesSymbol, getAttrib(lf, R_NamesSymbol));
    for(j=0; j<ncol; j++) {
        thiscol = VECTOR_ELT(lf,j);
        target = keepattr(allocVector(TYPEOF(thiscol), nrow), thiscol);
        SET_VECTOR_ELT(ans, j, target);
        ansloc = 0;
        for (i=first; i<length(l); i++) {
            li = VECTOR_ELT(l,i);
            if (isNull(li)) continue;  // majority of time though, each item of l is populated
            thislen = length(VECTOR_ELT(li,0));
            thiscol = VECTOR_ELT(li,j);
            if (thislen != length(thiscol)) error("Column %d of item %d is length %d, inconsistent with first column of that item which is length %d. rbindlist doesn't recycle as it already expects each item to be a uniform list, or data.table", j+1, i+1, length(thiscol), thislen);
            if (TYPEOF(thiscol) != TYPEOF(target)) {
                thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));
                coerced = TRUE;
                // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
                // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
            }
            switch(TYPEOF(target)) {
            case STRSXP :
                for (r=0; r<thislen; r++)
                    SET_STRING_ELT(target, ansloc+r, STRING_ELT(thiscol,r));
                    // see comments in dogroups.c about possible better way to do this
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
    }
    UNPROTECT(1);
    return(ans);
}


