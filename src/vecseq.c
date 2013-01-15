#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

SEXP vecseq(SEXP x, SEXP len, SEXP clamp)
{
    // Name taken from bit::vecseq, but,
    //   * implemented in C rather than R
    //   * takes len rather than y endpoints
    //   * no concat=FALSE or eval arguments as not needed.
    //   * Adds clamp to prevent very large vectors being created (unless allow.cartesian=TRUE)
    // Specially for use by [.data.table after binary search. Now so specialized that for general use
    // bit::vecseq is recommended (Jens has coded it in C now).
    R_len_t reslen,i,j,k, thisx, limit;
    SEXP ans;
    
    if (!isInteger(x)) error("x must be an integer vector");
    if (!isInteger(len)) error("len must be an integer vector");
    if (LENGTH(x) != LENGTH(len)) error("x and len must be the same length");
    reslen = 0;
    for (i=0; i<LENGTH(len); i++) {
        if (INT_MAX-reslen < INTEGER(len)[i])
            error("Join results in more than 2^31 rows (internal vecseq reached physical limit). Very likely misspecified join. Check for duplicate key values in i, each of which join to the same group in x over and over again. If that's ok, try including `j` and dropping `by` (by-without-by) so that j runs for each group to avoid the large allocation. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and datatable-help for advice.");
        reslen += INTEGER(len)[i];
    }
    if (!isNull(clamp)) {
        if (!isInteger(clamp) || LENGTH(clamp)!=1) error("clamp must be an integer vector length 1");
        limit = INTEGER(clamp)[0];
        if (limit<0) error("clamp must be positive");
        if (reslen>limit) error("Join results in %d rows; more than %d = max(nrow(x),nrow(i)). Check for duplicate key values in i, each of which join to the same group in x over and over again. If that's ok, try including `j` and dropping `by` (by-without-by) so that j runs for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and datatable-help for advice.", reslen, limit);
    }
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


