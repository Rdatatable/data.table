#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

extern size_t sizes[100];
#define SIZEOF(x) sizes[TYPEOF(x)]

extern SEXP chmatch(SEXP x, SEXP table, R_len_t nomatch, Rboolean in);
extern SEXP uniqlist(SEXP l, SEXP order);
extern SEXP allocNAVector(SEXPTYPE type, R_len_t n);
extern SEXP seq_int(int n, int start);
extern SEXP set_diff(SEXP x, int n);
extern SEXP which(SEXP x);
extern SEXP dogroups(SEXP dt, SEXP dtcols, SEXP groups, SEXP grpcols, SEXP jiscols, SEXP xjiscols, SEXP grporder, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP lhs, SEXP newnames, SEXP verbose);
extern SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose);
extern SEXP bmerge(SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP isorted, SEXP rollarg, SEXP rollends, SEXP nomatch, SEXP retFirst, SEXP retLength, SEXP allLen1);

// Note: all these functions below are internal functions and are designed specific to fcast.
SEXP zero_init(R_len_t n) {
    R_len_t i;
    SEXP ans;
    if (n < 0) error("Input argument 'n' to 'zero_init' must be >= 0");
    ans = PROTECT(allocVector(INTSXP, n));
    for (i=0; i<n; i++) INTEGER(ans)[i] = 0;
    UNPROTECT(1);
    return(ans);
}

SEXP vec_init(R_len_t n, SEXP val) {
    
    SEXP ans;
    R_len_t i;
    if (n < 0) error("Input argument 'n' to 'vec_init' must be >= 0");
    ans = PROTECT(allocVector(TYPEOF(val), n));
    switch(TYPEOF(val)) {
        case INTSXP :
        for (i=0; i<n; i++) INTEGER(ans)[i] = INTEGER(val)[0];
        break;
        case REALSXP :
        for (i=0; i<n; i++) REAL(ans)[i] = REAL(val)[0];
        break;
        case LGLSXP :
        for (i=0; i<n; i++) LOGICAL(ans)[i] = LOGICAL(val)[0];
        break;
        case STRSXP :
        for (i=0; i<n; i++) SET_STRING_ELT(ans, i, STRING_ELT(val, 0));
        break;
        case VECSXP :
        for (i=0; i<n; i++) SET_VECTOR_ELT(ans, i, VECTOR_ELT(val, 0));
        default : 
        error("Unknown input type '%s'", type2char(TYPEOF(val)));
    }
    UNPROTECT(1);
    return(ans);
}

SEXP cast_order(SEXP v, SEXP env) {
    R_len_t len;
    SEXP call, ans;
    if (TYPEOF(env) != ENVSXP) error("Argument 'env' to (data.table internals) 'cast_order' must be an environment");
    if (TYPEOF(v) == VECSXP) len = length(VECTOR_ELT(v, 0)); 
    else len = length(v);
    PROTECT(call = lang2(install("forder"), v)); // TODO: save the 'eval' by calling directly the C-function.
    ans = PROTECT(eval(call, env));
    if (length(ans) == 0) { // forder returns integer(0) if already sorted
        UNPROTECT(1); // ans
        ans = PROTECT(seq_int(len, 1));
    }
    UNPROTECT(2);
    return(ans);
}

SEXP cross_join(SEXP s, SEXP env) {
    // Calling CJ is faster and don't have to worry about sorting or setting key.
    SEXP call, r;
    if (!isNewList(s) || isNull(s)) error("Argument 's' to 'cross_join' must be a list of length > 0");
    PROTECT(call = lang3(install("do.call"), install("CJ"), s));
    r = eval(call, env);
    UNPROTECT(1);
    return(r);
}

static SEXP subsetVectorRaw(SEXP x, SEXP idx, int n)
// Only for use by subsetDT() or subsetVector() below, hence static
// n is the count of non-zero (including NAs) in idx i.e. the length of the result
// TO DO: if no 0 or NA detected up front in subsetDT() below, could switch to a faster subsetVectorRawNo0orNA()
{
    int i, this, ansi=0, max=length(x);
    SEXP ans = PROTECT(allocVector(TYPEOF(x), n));
    switch(TYPEOF(x)) {
    case INTSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            if (this==0) continue;
            INTEGER(ans)[ansi++] = (this==NA_INTEGER || this>max) ? NA_INTEGER : INTEGER(x)[this-1];
        }
        break;
    case REALSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            if (this==0) continue;
            REAL(ans)[ansi++] = (this==NA_INTEGER || this>max) ? NA_REAL : REAL(x)[this-1];
        }
        break;
    case LGLSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            if (this==0) continue;
            LOGICAL(ans)[ansi++] = (this==NA_INTEGER || this>max) ? NA_LOGICAL : LOGICAL(x)[this-1];
        }
        break;
    case STRSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            if (this==0) continue;
            SET_STRING_ELT(ans, ansi++, (this==NA_INTEGER || this>max) ? NA_STRING : STRING_ELT(x, this-1));
        }
        break;
    case VECSXP :
        for (i=0; i<LENGTH(idx); i++) {
            this = INTEGER(idx)[i];
            if (this==0) continue;
            SET_VECTOR_ELT(ans, ansi++, (this==NA_INTEGER || this>max) ? R_NilValue : VECTOR_ELT(x, this-1));
        }
        break;
    default :
        error("Unknown column type '%s'", type2char(TYPEOF(x)));
    }
    if (ansi != n) error("Internal error: ansi [%d] != n [%d] at the end of subsetVector", ansi, n);
    copyMostAttrib(x, ans);
    UNPROTECT(1);
    return(ans);
}

static int check_idx(SEXP idx, int n)
{
    int i, this, ans=0;
    if (!isInteger(idx)) error("Internal error. 'idx' is type '%s' not 'integer'", type2char(TYPEOF(idx)));
    for (i=0; i<LENGTH(idx); i++) {  // check idx once up front and count the non-0 so we know how long the answer will be
        this = INTEGER(idx)[i];
        if (this==0) continue;
        if (this!=NA_INTEGER && this<0) error("Internal error: item %d of idx is %d. Negatives should have been dealt with earlier.", i+1, this);
        // this>n is treated as NA for consistency with [.data.frame and things like cbind(DT[w],DT[w+1])
        ans++;
    }
    return ans;
}

SEXP convertNegativeIdx(SEXP idx, SEXP maxArg)
{
    int this;
    // + more precise and helpful error messages telling user exactly where the problem is (saving user debugging time)
    // + a little more efficient than negativeSubscript in src/main/subscript.c (it's private to R so we can't call it anyway)
    
    if (!isInteger(idx)) error("Internal error. 'idx' is type '%s' not 'integer'", type2char(TYPEOF(idx)));
    if (!isInteger(maxArg) || length(maxArg)!=1) error("Internal error. 'maxArg' is type '%s' and length %d, should be an integer singleton", type2char(TYPEOF(maxArg)), length(maxArg));
    int max = INTEGER(maxArg)[0];
    if (max<0) error("Internal error. max is %d, must be >= 0.", max);  // NA also an error which'll print as INT_MIN
    int firstNegative = 0, firstPositive = 0, firstNA = 0, num0 = 0;
    int i=0;
    for (i=0; i<LENGTH(idx); i++) {
        this = INTEGER(idx)[i];
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
    for (i=0; i<LENGTH(idx); i++) {
        this = -INTEGER(idx)[i];
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
    for (i=0; i<max; i++) {
        if (tmp[i]==0) INTEGER(ans)[ansi++] = i+1;
    }
    Free(tmp);
    UNPROTECT(1);
    if (ansi != max-LENGTH(idx)+num0+numDup+numBeyond) error("Internal error: ansi[%d] != max[%d]-LENGTH(idx)[%d]+num0[%d]+numDup[%d]+numBeyond[%d] in convertNegativeIdx",ansi,max,LENGTH(idx),num0,numDup,numBeyond);
    return(ans);
}

SEXP subsetDT(SEXP x, SEXP rows, SEXP cols) { // rows and cols are 1-based passed from R level

// Originally for subsetting vectors in fcast and now the beginnings of [.data.table ported to C
// Immediate need is for R 3.1 as lglVec[1] now returns R's global TRUE and we don't want := to change that global [think 1 row data.tables]
// Could do it other ways but may as well go to C now as we were going to do that anyway
    
    SEXP ans;
    R_len_t i, ansn=0;
    int this;
    if (!isNewList(x)) error("Internal error. Argument 'x' to CsubsetDT is type '%s' not 'list'", type2char(TYPEOF(rows)));
    if (!length(x)) error("x is an empty list() of 0 columns");
    ansn = check_idx(rows, length(VECTOR_ELT(x,0)));  // check once up front before looping calls to subsetVectorRaw below
    if (!isInteger(cols)) error("Internal error. Argument 'cols' to Csubset is type '%s' not 'integer'", type2char(TYPEOF(cols)));
    for (i=0; i<LENGTH(cols); i++) {
        this = INTEGER(cols)[i];
        if (this<1 || this>LENGTH(x)) error("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]", i+1, this, LENGTH(x));
    }
    ans = PROTECT(allocVector(VECSXP, LENGTH(cols)));
    for (i=0; i<LENGTH(cols); i++) {
        SET_VECTOR_ELT(ans, i, subsetVectorRaw(VECTOR_ELT(x, INTEGER(cols)[i]-1), rows, ansn));
    }
    setAttrib(ans, R_NamesSymbol, subsetVectorRaw( getAttrib(x, R_NamesSymbol), cols, LENGTH(cols) ));
    UNPROTECT(1);
    return ans;
}

SEXP subsetVector(SEXP x, SEXP idx) { // idx is 1-based passed from R level
    return subsetVectorRaw(x, idx, check_idx(idx, length(x)));
}


SEXP diff_int(SEXP x, R_len_t n) {
    
    R_len_t i;
    SEXP ans;
    if (TYPEOF(x) != INTSXP) error("Argument 'x' to 'diff_int' must be an integer vector");
    ans = PROTECT(allocVector(INTSXP, length(x)));
    for (i=1; i<length(x); i++)
        INTEGER(ans)[i-1] = INTEGER(x)[i] - INTEGER(x)[i-1];
    INTEGER(ans)[length(x)-1] = n - INTEGER(x)[length(x)-1] + 1;
    UNPROTECT(1);
    return(ans);
}

SEXP intrep(SEXP x, SEXP len) {
    
    R_len_t i,j,l=0, k=0;
    SEXP ans;
    if (TYPEOF(x) != INTSXP || TYPEOF(len) != INTSXP) error("Arguments 'x' and 'len' to 'intrep' should both be integer vectors");
    if (length(x) != length(len)) error("'x' and 'len' must be of same length");
    // assuming both are of length >= 1
    for (i=0; i<length(len); i++)
        l += INTEGER(len)[i]; // assuming positive values for len. internal use - can't bother to check.
    ans = PROTECT(allocVector(INTSXP, l));
    for (i=0; i<length(len); i++) {
        for (j=0; j<INTEGER(len)[i]; j++) {
            INTEGER(ans)[k++] = INTEGER(x)[i];
        }
    }
    UNPROTECT(1); // ans
    return(ans);
}

// taken match_transform() from base:::unique.c and modified
SEXP coerce_to_char(SEXP s, SEXP env)
{
    if(OBJECT(s)) {
    if(inherits(s, "factor")) return asCharacterFactor(s);
    else if(getAttrib(s, R_ClassSymbol) != R_NilValue) {
        SEXP call, r;
        PROTECT(call = lang2(install("as.character"), s));
        r = eval(call, env);
        UNPROTECT(1);
        return r;
    }
    }
    /* else */
    return coerceVector(s, STRSXP);
}

// to do: margins
// SEXP margins(...) {
// }

// TO DO: for sorted case - when forder is integer(0), just "duplicate" instead of using "subset" - will gain speed (as that'll use memcpy)
// TO DO: can we just move "subsetting" to R-side? Will simplify things a lot here and I'm not sure if there's any advantage of having it here
// TO DO: Remove "sorted" and implement that as well with "setkey" just on c(ff_, value.var) at R side?
SEXP fcast(SEXP DT, SEXP inames, SEXP mnames, SEXP vnames, SEXP fill, SEXP fill_d, SEXP is_agg, SEXP env, SEXP drop) {
    
    int protecti = 0;
    R_len_t i, j, nrows, ncols, ilen = length(inames), mlen = length(mnames), lanscols, ranscols;
    SEXP dtnames, lrnames, ldt, rdt, vdt, lrdt, lidx, ridx, vidx, lo, ro, lro, lrdup, ldup=R_NilValue, rdup=R_NilValue;
    SEXP tmp, cpy, thiscol, colnames, llen__, haskey, rollends, f__, len__, allLen1, roll;
    SEXP xx, cj, cjtmp, ans, outnamevec, ansnames, dtmp, lcj=R_NilValue, rcj=R_NilValue, ddup, dorder;
    SEXP lvls; // to get levels for drop=FALSE in case of factor column - #5379
    Rboolean isfill = TRUE;
    char buffer[128], uscore[] = "_";

    // check for arguments - some of these are not necessary as the intention is NOT to call directly, but thro' fcast.R (dcast), but alright
    if (TYPEOF(DT) != VECSXP) error("'data' should be a data.table or data.frame");
    if (TYPEOF(inames) != STRSXP) error("LHS of parsed formula did not result in character vector, possibly invalid formula");
    if (TYPEOF(mnames) != STRSXP) error("RHS of parsed formula did not result in character vector, possibly invalid formula");
    if (TYPEOF(vnames) != STRSXP || length(vnames) != 1) error("'value.var' must be a character vector of length 1");
    if (!isEnvironment(env)) error("'.CASTenv' must be an environment");
    if (!isLogical(drop) || length(drop) != 1) error("'drop' must be a logical vector of length 1");
    
    nrows = length(VECTOR_ELT(DT, 0));
    if (nrows < 1) error("nrow(data) is 0, cannot 'cast' an empty data.table"); // TODO: should this be a warning instead + return DT?
    ncols = length(DT);
    if (ncols < 1) error("ncol(data) is 0, cannot 'cast' an empty data.table"); // TODO: should this be a warning instead + return DT?

    dtnames = getAttrib(DT, R_NamesSymbol);
    if (isNull(dtnames)) error("'names(data)' is NULL. Possible internal error. Please report to data.table-help");
    
    // names passed from R-level - no checks
    lidx = PROTECT(chmatch(inames, dtnames, 0, FALSE)); protecti++;
    ridx = PROTECT(chmatch(mnames, dtnames, 0, FALSE)); protecti++;
    vidx = PROTECT(chmatch(vnames, dtnames, 0, FALSE)); protecti++;

    // get lrdt and value
    lrdt = PROTECT(allocVector(VECSXP, ilen+mlen)); protecti++;
    for (i=0; i<ilen; i++) SET_VECTOR_ELT(lrdt, i, VECTOR_ELT(DT, INTEGER(lidx)[i]-1));
    for (i=0; i<mlen; i++) SET_VECTOR_ELT(lrdt, i+ilen, VECTOR_ELT(DT, INTEGER(ridx)[i]-1));
    // value column
    vdt = isFactor(VECTOR_ELT(DT, INTEGER(vidx)[0]-1)) ? PROTECT(asCharacterFactor(VECTOR_ELT(DT, INTEGER(vidx)[0]-1))) : PROTECT(VECTOR_ELT(DT, INTEGER(vidx)[0]-1));
    protecti++;
    
    // get names
    lrnames = PROTECT(allocVector(STRSXP, ilen+mlen)); protecti++;
    for (i=0; i<ilen; i++) SET_STRING_ELT(lrnames, i, STRING_ELT(inames, i));
    for (i=0; i<mlen; i++) SET_STRING_ELT(lrnames, i+ilen, STRING_ELT(mnames, i));
    setAttrib(lrdt, R_NamesSymbol, lrnames);

    lro = PROTECT(seq_int(1, -1)); protecti++;
    lrdup = PROTECT(uniqlist(lrdt, lro)); protecti++;
    
    if (isNull(fill)) {
        isfill = FALSE;
        fill = LOGICAL(is_agg)[0] == TRUE ? PROTECT(allocNAVector(TYPEOF(vdt), 1)) : PROTECT(fill_d);
        protecti++;
    }
    if (isfill && ((isNull(fill_d) && TYPEOF(fill) != TYPEOF(vdt)) || (!isNull(fill_d) && TYPEOF(fill) != TYPEOF(fill_d)))) {
        Rprintf("class(fill) != class(%s), coercing 'fill' to %s\n", CHAR(STRING_ELT(vnames, 0)), type2char(TYPEOF(vdt)));
        fill = PROTECT(coerceVector(fill, TYPEOF(vdt))); protecti++;
    }

    // get ldt and rdt
    ldt = PROTECT(allocVector(VECSXP, ilen)); protecti++;
    rdt = PROTECT(allocVector(VECSXP, mlen)); protecti++;
    for (i=0; i<ilen; i++) SET_VECTOR_ELT(ldt, i, VECTOR_ELT(lrdt, i));
    for (i=0; i<mlen; i++) SET_VECTOR_ELT(rdt, i, VECTOR_ELT(lrdt, i+ilen));
    setAttrib(ldt, R_NamesSymbol, inames);
    setAttrib(rdt, R_NamesSymbol, mnames);
    
    if (!LOGICAL(drop)[0]) { // have to take care of drop=FALSE!
        lcj = PROTECT(allocVector(VECSXP, ilen)); protecti++;
        for (i=0; i<ilen; i++) {
            cpy = PROTECT(allocVector(VECSXP, 1));
            SET_VECTOR_ELT(cpy, 0, VECTOR_ELT(ldt, i));
            if (isFactor(VECTOR_ELT(ldt, i))) {
                lvls = PROTECT(getAttrib(VECTOR_ELT(ldt, i), R_LevelsSymbol));
                dtmp = PROTECT(seq_int(length(lvls), 1));
                setAttrib(dtmp, R_ClassSymbol, mkString("factor"));
                setAttrib(dtmp, R_LevelsSymbol, lvls);
                UNPROTECT(3);
            } else {
                dorder = PROTECT(cast_order(cpy, env));
                ddup = PROTECT(uniqlist(cpy, dorder));
                ddup = PROTECT(subsetVector(dorder, ddup));
                dtmp = PROTECT(subsetVector(VECTOR_ELT(cpy, 0), ddup));
                UNPROTECT(5); // dtmp, cpy, dorder, ddup
            }
            SET_VECTOR_ELT(lcj, i, dtmp);
        }
        lcj = PROTECT(cross_join(lcj, env)); protecti++;
        
        rcj = PROTECT(allocVector(VECSXP, mlen)); protecti++;
        for (i=0; i<mlen; i++) {
            cpy = PROTECT(allocVector(VECSXP, 1));
            SET_VECTOR_ELT(cpy, 0, VECTOR_ELT(rdt, i));
            dorder = PROTECT(cast_order(cpy, env));
            ddup = PROTECT(uniqlist(cpy, dorder));
            ddup = PROTECT(subsetVector(dorder, ddup));
            dtmp = PROTECT(subsetVector(VECTOR_ELT(cpy, 0), ddup));
            UNPROTECT(5);
            SET_VECTOR_ELT(rcj, i, dtmp);
        }
        rcj = PROTECT(cross_join(rcj, env)); protecti++;
        
        outnamevec = PROTECT(allocVector(VECSXP, length(rcj))); protecti++;
        for (i=0; i<length(outnamevec); i++)
            SET_VECTOR_ELT(outnamevec, i, coerce_to_char(VECTOR_ELT(rcj, i), R_GlobalEnv));

        xx = PROTECT(allocVector(VECSXP, 2)); protecti++;
        // lcj[ldt]
        f__ = PROTECT(zero_init(length(VECTOR_ELT(ldt, 0))));
        len__ = PROTECT(zero_init(length(f__)));
        haskey = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(haskey)[0] = 1;
        rollends = PROTECT(allocVector(LGLSXP, 2)); LOGICAL(rollends)[0] = 0; LOGICAL(rollends)[1] = 1;
        allLen1 = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(allLen1)[0] = 1;
        roll = PROTECT(allocVector(REALSXP, 1)); REAL(roll)[0] = 0.0;
        bmerge(ldt, lcj, PROTECT(seq_int(ilen, 1)), PROTECT(seq_int(ilen, 1)), haskey, roll, rollends, PROTECT(zero_init(1)), f__, len__, allLen1);
        UNPROTECT(9);
        SET_VECTOR_ELT(xx, 0, f__);
        
        // rcj[rdt]
        f__ = PROTECT(zero_init(length(VECTOR_ELT(rdt, 0))));
        len__ = PROTECT(zero_init(length(f__)));
        haskey = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(haskey)[0] = 0; // unsorted
        rollends = PROTECT(allocVector(LGLSXP, 2)); LOGICAL(rollends)[0] = 0; LOGICAL(rollends)[1] = 1;
        allLen1 = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(allLen1)[0] = 1;
        roll = PROTECT(allocVector(REALSXP, 1)); REAL(roll)[0] = 0.0;
        bmerge(rdt, rcj, PROTECT(seq_int(mlen, 1)), PROTECT(seq_int(mlen, 1)), haskey, roll, rollends, PROTECT(zero_init(1)), f__, len__, allLen1);
        UNPROTECT(9);
        SET_VECTOR_ELT(xx, 1, f__);
                
        cjtmp = PROTECT(allocVector(VECSXP, 2));
        SET_VECTOR_ELT(cjtmp, 0, PROTECT(seq_int(length(VECTOR_ELT(lcj, 0)), 1)));
        SET_VECTOR_ELT(cjtmp, 1, PROTECT(seq_int(length(VECTOR_ELT(rcj, 0)), 1)));
        UNPROTECT(3); // cjtmp
        cj = PROTECT(cross_join(cjtmp, env)); protecti++;
    } else {
        // we could do a bit faster
        lo = PROTECT(seq_int(nrows, 1)); protecti++; // no need for cast_order of "lo", already sorted
        ro = PROTECT(cast_order(rdt, env)); protecti++;
        ldup = PROTECT(uniqlist(ldt, lo)); protecti++;
        rdup = PROTECT(uniqlist(rdt, ro)); protecti++; 

        llen__ = PROTECT(diff_int(ldup, nrows)); protecti++;

        rdup = PROTECT(subsetVector(ro, rdup)); protecti++;
        tmp = PROTECT(allocVector(VECSXP, mlen+1)); protecti++;
        for (i=0; i<mlen; i++) {
            cpy = PROTECT(VECTOR_ELT(rdt, i));
            thiscol = PROTECT(subsetVector(cpy, rdup));
            UNPROTECT(2); // cpy, thiscol
            SET_VECTOR_ELT(tmp, i, thiscol);
        }
        // names for final output
        outnamevec = PROTECT(allocVector(VECSXP, length(tmp)-1)); protecti++;
        for (i=0; i<length(outnamevec); i++) {
            SET_VECTOR_ELT(outnamevec, i, coerce_to_char(VECTOR_ELT(tmp, i), R_GlobalEnv));
        }
        ro = PROTECT(cast_order(rdup, env));
        rdup = PROTECT(subsetVector(rdup, ro));
        UNPROTECT(2); // ro, rdup
        SET_VECTOR_ELT(tmp, mlen, rdup);

        colnames = PROTECT(allocVector(STRSXP, length(tmp)));
        for (i=0; i<length(tmp)-1; i++) 
            SET_STRING_ELT(colnames, i, STRING_ELT(mnames, i));
        SET_STRING_ELT(colnames, length(tmp)-1, mkChar("id"));
        UNPROTECT(1); //colnames
        setAttrib(tmp, R_NamesSymbol, colnames);

        // tmp[rdt] - binary search
        f__ = PROTECT(zero_init(length(lrdup))); protecti++;
        len__ = PROTECT(zero_init(length(f__)));
        haskey = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(haskey)[0] = 0;
        rollends = PROTECT(allocVector(LGLSXP, 2)); LOGICAL(rollends)[0] = 0; LOGICAL(rollends)[1] = 1;
        allLen1 = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(allLen1)[0] = 1;
        roll = PROTECT(allocVector(REALSXP, 1)); REAL(roll)[0] = 0.0;
        bmerge(rdt, tmp, PROTECT(seq_int(length(rdt), 1)), PROTECT(seq_int(length(rdt),1)), haskey, roll, rollends, PROTECT(zero_init(1)), f__, len__, allLen1);
        UNPROTECT(8); // len__, haskey, rollends, allLen1, roll (except f__)

        xx = PROTECT(allocVector(VECSXP, 2)); protecti++;
        SET_VECTOR_ELT(xx, 0, intrep(ldup, llen__));
        rdup = PROTECT(VECTOR_ELT(tmp, mlen));
        tmp = PROTECT(subsetVector(rdup, f__));
        UNPROTECT(2); // rdup, tmp
        SET_VECTOR_ELT(xx, 1, tmp);

        cjtmp = PROTECT(allocVector(VECSXP, 2));
        SET_VECTOR_ELT(cjtmp, 0, ldup);
        SET_VECTOR_ELT(cjtmp, 1, rdup);
        UNPROTECT(1); // cjtmp;
        cj = PROTECT(cross_join(cjtmp, env)); protecti++;
    }
    // to do: if we can check whether there are any missing combinations, we can skip this binary search and it should be faster (especially on bigger data).
    // xx[cj] binary search to get missing values
    f__ = PROTECT(zero_init(length(VECTOR_ELT(cj, 0)))); protecti++;
    len__ = PROTECT(zero_init(length(f__)));
    haskey = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(haskey)[0] = 1;
    rollends = PROTECT(allocVector(LGLSXP, 2)); LOGICAL(rollends)[0] = 0; LOGICAL(rollends)[1] = 1;
    allLen1 = PROTECT(allocVector(LGLSXP, 1)); LOGICAL(allLen1)[0] = 1;
    roll = PROTECT(allocVector(REALSXP, 1)); REAL(roll)[0] = 0.0;
    bmerge(cj, xx, PROTECT(seq_int(2, 1)), PROTECT(seq_int(2,1)), haskey, roll, rollends, PROTECT(zero_init(1)), f__, len__, allLen1);
    UNPROTECT(8); // len__, haskey, rollends, allLen1, roll (except f__)
    
    ranscols = !LOGICAL(drop)[0] ? length(VECTOR_ELT(rcj, 0)) : length(rdup);
    ans = PROTECT(allocVector(VECSXP, ranscols+ilen)); protecti++;
    if (!LOGICAL(drop)[0]) {
        for (i=0; i<ilen; i++)
            SET_VECTOR_ELT(ans, i, VECTOR_ELT(lcj, i));
        lanscols = length(VECTOR_ELT(lcj, 0));
    } else {
        for (i=0; i<ilen; i++) {
            cpy = PROTECT(VECTOR_ELT(lrdt, i));
            tmp = PROTECT(subsetVector(cpy, ldup));
            UNPROTECT(2); // tmp, cpy
            SET_VECTOR_ELT(ans, i, tmp);
        }
        lanscols = length(ldup);
    }
    for (i=0; i<ranscols; i++) {
        tmp = PROTECT(allocVector(TYPEOF(vdt), lanscols));
        switch(TYPEOF(tmp)) {
            case INTSXP :
            for (j=0; j<lanscols; j++) {
                if (INTEGER(f__)[i + j*ranscols] != 0)
                    INTEGER(tmp)[j] = INTEGER(vdt)[INTEGER(f__)[i + j*ranscols]-1];
                else 
                    INTEGER(tmp)[j] = INTEGER(fill)[0];
            }
            break;
            case REALSXP :
            for (j=0; j<lanscols; j++) {
                if (INTEGER(f__)[i + j*ranscols] != 0)
                    REAL(tmp)[j] = REAL(vdt)[INTEGER(f__)[i + j*ranscols]-1];
                else 
                    REAL(tmp)[j] = REAL(fill)[0];
            }
            break;
            case LGLSXP :
            for (j=0; j<lanscols; j++) {
                if (INTEGER(f__)[i + j*ranscols] != 0)
                    LOGICAL(tmp)[j] = LOGICAL(vdt)[INTEGER(f__)[i + j*ranscols]-1];
                else 
                    LOGICAL(tmp)[j] = LOGICAL(fill)[0];
            }
            break;
            case STRSXP :
            for (j=0; j<lanscols; j++) {
                if (INTEGER(f__)[i + j*ranscols] != 0)
                    SET_STRING_ELT(tmp, j, STRING_ELT(vdt, INTEGER(f__)[i + j*ranscols]-1));
                else 
                    SET_STRING_ELT(tmp, j, STRING_ELT(fill, 0));
            }
            break;
            case VECSXP :
            for (j=0; j<lanscols; j++) {
                if (INTEGER(f__)[i + j*ranscols] != 0)
                    SET_VECTOR_ELT(tmp, j, VECTOR_ELT(vdt, INTEGER(f__)[i + j*ranscols]-1));
                else 
                    SET_VECTOR_ELT(tmp, j, VECTOR_ELT(fill, 0));
            }
            break;
            default : error("Unknown 'value' column type '%s'", type2char(TYPEOF(tmp)));
        }
        UNPROTECT(1); // tmp
        SET_VECTOR_ELT(ans, i+ilen, tmp);
    }
    ansnames = PROTECT(allocVector(STRSXP, length(ans))); protecti++;
    for (i=0; i<ilen; i++)
        SET_STRING_ELT(ansnames, i, STRING_ELT(inames, i));
    for (i=0; i<length(VECTOR_ELT(outnamevec, 0)); i++) {
        sprintf(buffer, "%s", CHAR(STRING_ELT(VECTOR_ELT(outnamevec, 0), i)));
        for (j=1; j<length(outnamevec); j++) {
            strcat(buffer, uscore);
            strcat(buffer, translateChar(STRING_ELT(VECTOR_ELT(outnamevec, j), i))); // requires const char*
        }
        SET_STRING_ELT(ansnames, i+ilen, mkChar(buffer));
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(protecti);
    return(ans);
}

