#include "data.table.h"

static SEXP subsetVectorRaw(SEXP x, SEXP idx, int l, int tl)
// Only for use by subsetDT() or subsetVector() below, hence static
// l is the count of non-zero (including NAs) in idx i.e. the length of the result
// tl is the amount to be allocated,  tl>=l
// TO DO: if no 0 or NA detected up front in subsetDT() below, could switch to a faster subsetVectorRawNo0orNA()
{
    int i, this, ansi=0, max=length(x);
    if (tl<l) error("Internal error: tl<n passed to subsetVectorRaw");
    SEXP ans = PROTECT(allocVector(TYPEOF(x), tl));
    SETLENGTH(ans, l);
    SET_TRUELENGTH(ans, tl);
    
    union { double d; long long ll; } naval;
    if (INHERITS(x, char_integer64)) naval.ll = NAINT64;
    else naval.d = NA_REAL;
    
    if (l) switch(TYPEOF(x)) {
    case INTSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            INTEGER(ans)[ansi++] = (this<=0 || this>max) ? NA_INTEGER : INTEGER(x)[this-1];
            ansi -= (this==0);
            // skip over 0 without using branch.
            // negatives are checked before (in check_idx()) not to have reached here
            // this<=0 covers this==NA_INTEGER || this==0
        }
        break;
    case REALSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            REAL(ans)[ansi++] = (this<=0 || this>max) ? naval.d : REAL(x)[this-1];
            ansi -= (this==0);
        }
        break;
    case LGLSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            LOGICAL(ans)[ansi++] = (this<=0 || this>max) ? NA_LOGICAL : LOGICAL(x)[this-1];
            ansi -= (this==0);
        }
        break;
    case STRSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            SET_STRING_ELT(ans, ansi++, (this<=0 || this>max) ? NA_STRING : STRING_ELT(x, this-1));
            ansi -= (this==0);
        }
        break;
    case VECSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            SET_VECTOR_ELT(ans, ansi++, (this<=0 || this>max) ? R_NilValue : VECTOR_ELT(x, this-1));
            ansi -= (this==0);
        }
        break;
    case CPLXSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            if (this<=0 || this>max) {
                COMPLEX(ans)[ansi].r = NA_REAL;
                COMPLEX(ans)[ansi++].i = NA_REAL;
            } else COMPLEX(ans)[ansi++] = COMPLEX(x)[this-1];
            ansi -= (this==0);
        }
        break;
    case RAWSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            RAW(ans)[ansi++] = (this<=0 || this>max) ? (Rbyte) 0 : RAW(x)[this-1];
        }
        break;
    default :
        error("Unsupported column type '%s'", type2char(TYPEOF(x)));
    }
    if (ansi != l) error("Internal error: ansi [%d] != l [%d] at the end of subsetVectorRaw", ansi, l);
    copyMostAttrib(x, ans);
    UNPROTECT(1);
    return(ans);
}

static int check_idx(SEXP idx, int n)
// count non-0 in idx => the length of the subset result
// checks no negatives too
{
    int ans=0;
    Rboolean anyNeg = FALSE;
    if (!isInteger(idx)) error("Internal error. 'idx' is type '%s' not 'integer'", type2char(TYPEOF(idx)));
    for (int i=0; i<LENGTH(idx); i++) {
        int this = INTEGER(idx)[i];
        ans += (this!=0);
        // this>n is treated as NA for consistency with [.data.frame and operations like cbind(DT[w],DT[w+1])
        anyNeg |= this<0 && this!=NA_INTEGER;
    }
    if (anyNeg) error("Internal error: idx contains negatives. Should have been dealt with earlier.");
    return ans;
}

SEXP convertNegativeIdx(SEXP idx, SEXP maxArg)
{
    // + more precise and helpful error messages telling user exactly where the problem is (saving user debugging time)
    // + a little more efficient than negativeSubscript in src/main/subscript.c (it's private to R so we can't call it anyway)

    if (!isInteger(idx)) error("Internal error. 'idx' is type '%s' not 'integer'", type2char(TYPEOF(idx)));
    if (!isInteger(maxArg) || length(maxArg)!=1) error("Internal error. 'maxArg' is type '%s' and length %d, should be an integer singleton", type2char(TYPEOF(maxArg)), length(maxArg));
    int max = INTEGER(maxArg)[0];
    if (max<0) error("Internal error. max is %d, must be >= 0.", max);  // NA also an error which'll print as INT_MIN
    int firstNegative = 0, firstPositive = 0, firstNA = 0, num0 = 0;
    for (int i=0; i<LENGTH(idx); i++) {
        int this = INTEGER(idx)[i];
        if (this==NA_INTEGER) { if (firstNA==0) firstNA = i+1;  continue; }
        if (this==0)          { num0++;  continue; }
        if (this>0)           { if (firstPositive==0) firstPositive=i+1; continue; }
        if (firstNegative==0) firstNegative=i+1;
    }
    if (firstNegative==0) return(idx);  // 0's and NA can be mixed with positives, there are no negatives present, so we're done
    if (firstPositive) error("Item %d of i is %d and item %d is %d. Cannot mix positives and negatives.",
                 firstNegative, INTEGER(idx)[firstNegative-1], firstPositive, INTEGER(idx)[firstPositive-1]);
    if (firstNA)       error("Item %d of i is %d and item %d is NA. Cannot mix negatives and NA.",
                 firstNegative, INTEGER(idx)[firstNegative-1], firstNA);

    // idx is all negative without any NA but perhaps 0 present (num0) ...

    char *tmp = Calloc(max, char);    // 4 times less memory that INTSXP in src/main/subscript.c
    int firstDup = 0, numDup = 0, firstBeyond = 0, numBeyond = 0;
    for (int i=0; i<LENGTH(idx); i++) {
        int this = -INTEGER(idx)[i];
        if (this==0) continue;
        if (this>max) {
            numBeyond++;
            if (firstBeyond==0) firstBeyond=i+1;
            continue;
        }
        if (tmp[this-1]==1) {
            numDup++;
            if (firstDup==0) firstDup=i+1;
        } else tmp[this-1] = 1;
    }
    if (numBeyond)
        warning("Item %d of i is %d but there are only %d rows. Ignoring this and %d more like it out of %d.", firstBeyond, INTEGER(idx)[firstBeyond-1], max, numBeyond-1, LENGTH(idx));
    if (numDup)
        warning("Item %d of i is %d which has occurred before. Ignoring this and %d other duplicates out of %d.", firstDup, INTEGER(idx)[firstDup-1], numDup-1, LENGTH(idx));

    SEXP ans = PROTECT(allocVector(INTSXP, max-LENGTH(idx)+num0+numDup+numBeyond));
    int ansi = 0;
    for (int i=0; i<max; i++) if (tmp[i]==0) INTEGER(ans)[ansi++] = i+1;
    Free(tmp);
    UNPROTECT(1);
    if (ansi != max-LENGTH(idx)+num0+numDup+numBeyond) error("Internal error: ansi[%d] != max[%d]-LENGTH(idx)[%d]+num0[%d]+numDup[%d]+numBeyond[%d] in convertNegativeIdx",ansi,max,LENGTH(idx),num0,numDup,numBeyond);
    return(ans);
}

/*
* subsetDT - Subsets a data.table
* NOTE:
*   1) 'rows' and 'cols' are 1-based, passed from R level
*   2) Originally for subsetting vectors in fcast and now the beginnings of 
*       [.data.table ported to C
*   3) Immediate need is for R 3.1 as lglVec[1] now returns R's global TRUE 
*       and we don't want := to change that global [think 1 row data.tables]
*   4) Could do it other ways but may as well go to C now as we were going to 
*       do that anyway
*/
SEXP subsetDT(SEXP x, SEXP rows, SEXP cols) {
    if (!isNewList(x)) error("Internal error. Argument 'x' to CsubsetDT is type '%s' not 'list'", type2char(TYPEOF(rows)));
    if (!length(x)) return(x);  // return empty list
    R_len_t ansn = check_idx(rows, length(VECTOR_ELT(x,0)));  // check once up front before looping calls to subsetVectorRaw below
    if (!isInteger(cols)) error("Internal error. Argument 'cols' to Csubset is type '%s' not 'integer'", type2char(TYPEOF(cols)));
    for (int i=0; i<LENGTH(cols); i++) {
        int this = INTEGER(cols)[i];
        if (this<1 || this>LENGTH(x)) error("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]", i+1, this, LENGTH(x));
    }
    SEXP ans = PROTECT(allocVector(VECSXP, LENGTH(cols)+64));  // just do alloc.col directly, eventually alloc.col can be deprecated.
    copyMostAttrib(x, ans);  // other than R_NamesSymbol, R_DimSymbol and R_DimNamesSymbol  
                             // so includes row.names (oddly, given other dims aren't) and "sorted", dealt with below
    SET_TRUELENGTH(ans, LENGTH(ans));
    SETLENGTH(ans, LENGTH(cols));
    for (int i=0; i<LENGTH(cols); i++) {
        SET_VECTOR_ELT(ans, i, subsetVectorRaw(VECTOR_ELT(x, INTEGER(cols)[i]-1), rows, ansn, ansn));  // column vectors aren't over allocated yet
    }
    setAttrib(ans, R_NamesSymbol, subsetVectorRaw( getAttrib(x, R_NamesSymbol), cols, LENGTH(cols), LENGTH(cols)+64 ));
    SEXP tmp = PROTECT(allocVector(INTSXP, 2));
    INTEGER(tmp)[0] = NA_INTEGER;
    INTEGER(tmp)[1] = -ansn;
    setAttrib(ans, R_RowNamesSymbol, tmp);  // The contents of tmp must be set before being passed to setAttrib(). setAttrib looks at tmp value and copies it in the case of R_RowNamesSymbol. Caused hard to track bug around 28 Sep 2014.
    // maintain key if ordered subset ...
    SEXP key = getAttrib(x, install("sorted"));
    if (length(key)) {
        SEXP in = PROTECT(chmatch(key,getAttrib(ans,R_NamesSymbol), 0, TRUE)); // (nomatch ignored when in=TRUE)
        int i = 0;  while(i<LENGTH(key) && LOGICAL(in)[i]) i++;
        UNPROTECT(1);
        // i is now the keylen that can be kept. 2 lines above much easier in C than R
        if (i==0) {
            setAttrib(ans, install("sorted"), R_NilValue);
            // clear key that was copied over by copyMostAttrib() above
        } else if (isOrderedSubset(rows, ScalarInteger(length(VECTOR_ELT(x,0))))) {
            setAttrib(ans, install("sorted"), tmp=allocVector(STRSXP, i));
            for (int j=0; j<i; j++) SET_STRING_ELT(tmp, j, STRING_ELT(key, j));
        }
    }
    setAttrib(ans, install(".data.table.locked"), R_NilValue);
    setselfref(ans);
    UNPROTECT(2);
    return ans;
}

SEXP subsetVector(SEXP x, SEXP idx) { // idx is 1-based passed from R level
    int n = check_idx(idx, length(x));
    return subsetVectorRaw(x, idx, n, n);
}

