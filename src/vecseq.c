#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

SEXP vecseq(SEXP x, SEXP len)
{
    // Name taken from bit::vecseq, but,
    //   * implemented in C rather than R
    //   * takes len rather than y endpoints
    //   * no concat=FALSE or eval arguments as not needed.
    // Specially for use by [.data.table after binary search, but could be expanded and exported.
    R_len_t reslen,i,j,k, thisx;
    SEXP ans;
    
    if (!isInteger(x)) error("x must be an integer vector");
    if (!isInteger(len)) error("len must be an integer vector");
    if (LENGTH(x) != LENGTH(len)) error("x and len must be the same length");
    
    reslen = 0;
    for (i=0; i<LENGTH(len); i++) reslen += INTEGER(len)[i];
    ans = PROTECT(allocVector(INTSXP, reslen));
    k = 0;
    for (i=0; i<LENGTH(len); i++) {
        thisx = INTEGER(x)[i];
        for (j=0; j<INTEGER(len)[i]; j++) {
            INTEGER(ans)[k++] = thisx++;
        }
    }
    UNPROTECT(1);
    return(ans);
}


