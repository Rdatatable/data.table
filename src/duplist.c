#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#define cmpnum(a,b) ((R_IsNaN(a) && R_IsNaN(b)) || (a == R_PosInf && b == R_PosInf) || (a == R_NegInf && b == R_NegInf) || (R_IsNA(a) && R_IsNA(b) && !R_IsNaN(a) && !R_IsNaN(b)) || fabs(a-b)<REAL(tol)[0])

SEXP duplist(SEXP l, SEXP ans, SEXP anslen, SEXP order, SEXP tol)  //change name to uniqlist
{
    // Returns the positions of the non-repeated rows. This works like
    // UNIX uniq as referred to by ?base::unique; i.e., it drops immediately
    // repeated rows but doesn't drop duplicates of any previous row.
    // Unless, order is provided, then it also drops any previous row.
    // l must be a list of same length vectors
    // ans is allocated first (maximum length the number of rows) and the length returned in anslen.
    // TO DO: grow ans instead.
    R_len_t i,j,nrow,ncol,len,thisi,previ;
    Rboolean b,byorder;
    SEXP v;
    if (NA_INTEGER != NA_LOGICAL || sizeof(NA_INTEGER)!=sizeof(NA_LOGICAL)) error("Have assumed NA_INTEGER == NA_LOGICAL (currently R_NaInt). If R changes this in future (seems unlikely), an extra case is required; a simple change.");
    ncol = length(l);
    nrow = length(VECTOR_ELT(l,0));
    len=1;
    INTEGER(ans)[0] = 1;    // first row is always the first of the first group
    byorder = INTEGER(order)[0] != -1;
    // Using MISSING() does not seem stable under windows. Always having arguments passed in seems a good idea anyway.
    thisi = byorder ? INTEGER(order)[0]-1 : 0;
    for (i=1; i<nrow; i++) {
        previ = thisi;
        thisi = byorder ? INTEGER(order)[i]-1 : i;
        j = ncol;  // the last column varies the most frequently so check that first and work backwards
        b = TRUE;
        while (--j>=0 && b) {
            v=VECTOR_ELT(l,j);
            switch (TYPEOF(v)) {
            case INTSXP : case LGLSXP :
                b=INTEGER(v)[thisi]==INTEGER(v)[previ]; break;
            case STRSXP :
                b=STRING_ELT(v,thisi)==STRING_ELT(v,previ); break;
            case REALSXP :
                b=cmpnum(REAL(v)[thisi],REAL(v)[previ]); break;
            default :
                error("Type '%s' not supported", type2char(TYPEOF(v))); 
            }
        }
        if (!b) INTEGER(ans)[len++] = i+1;
    }
    INTEGER(anslen)[0] = len;
    return(R_NilValue);
}


// copied sort2_with_index in orderVector1 from /src/main/sort.c
// modified for :
// i) stability within tolerance
// ii) option of nalast removed as data.table is always NA first (for binary search)
// iii) rather than the loop through n in do_order to ++ the indx to return to R,
//     we point x and indx to 1 before the start, so we can use 1-based
//     indexing.  This allows the o vector up in fast order (the very same o vector
//     with no copies or loops through it doing ++ and --) to be passed on as the
//     columns are looped through in reverse order.
// iv) not using the rcmp function, saving the 5 if's and funct call
// v) the section of the "less" macro in main/src/sort.c: "|| (x[a] == x[b] && a > b)" seems to cause instability
//  when we don't pass in seq_along(x) as idx. Think it might be superfluous in base, but needs to be removed here
//  where we pass in different orders than seq_along(x).

//  It is still a comparison sort, so is not meant to be super fast, but should be slightly
//  more efficient than base for the reasons above. Intend to tackle fast radix sort
//  for floating point later (which does seem to be known), but then what about ties within tolerance (radix would
//  be stable within ties but at full accuracy by construction as it does no comparison. Could sweep after
//  within ties to get the original order back, perhaps.  If we do a recursive order through columns 1:n then it
//  becomes natural and stable to use shell/quick.

extern const int incs[];

//#define cmptol(a,b) (x[a] > x[b]+tol || (a>b && x[a]==x[b]))  [from src/main/sort.c]
// #define cmptol(a,b) ((ISNAN(x[a]) && ISNAN(x[b]) && a>b) || (!ISNAN(x[a]) && ( ISNAN(x[b]) || (x[a]-x[b]>tol) || (a>b && x[b]-x[a]<tol)) ))
#define cmptol(a,b) ((x[a] == R_PosInf && x[b] == R_PosInf && a>b) || (x[a] == R_NegInf && x[b] == R_NegInf && a>b) || (ISNAN(x[a]) && ISNAN(x[b]) && a>b) || (!ISNAN(x[a]) && ( ISNAN(x[b]) || (x[a]-x[b]>tol) || (a>b && x[b]-x[a]<tol)) ))


SEXP rorder_tol(SEXP xarg, SEXP indxarg, SEXP tolarg)
{
    R_len_t t, i, j, h, itmp;
    double *x=REAL(xarg)-1;
    R_len_t n=length(xarg);
    R_len_t lo = 1, hi = n;
    R_len_t *indx=INTEGER(indxarg)-1;
    double tol = REAL(tolarg)[0];
    
    //for (i=1; i<=n; i++) if (ISNAN(x[i])) error("NA and NaN are not allowed in numeric key columns. They have to be dealt with specially (slowing things down) but also NAs in the key can lead to ambiguities and confusion when it comes to joining to NA values. If you have a real-world example that really does need NAs in the key then consider choosing your own value to represent NA, such as -999.999. If you want to join to the -999.999 values then you can, and if you want to represent an NA row, then you can too. That will be much faster and clearer. We think that requirement is very rare, so data.table is setup to be optimized for the most common cases; i.e., no NAs in key columns. Also, binary search is faster without a check on NA_REAL (it seems we cannot rely on NA_REAL being REAL_MIN, unlike NA_INTEGER being MIN_INT).");

    //Rprintf("%d\n",(long)NA_REAL<0);  True. This would work well, were it not for tolerance. NA_REAL<0 is nan as IEEE returns nan at C level
    
    for (t = 0; incs[t] > hi-lo+1; t++);
    for (h = incs[t]; t < 16; h = incs[++t])
    for (i = lo + h; i <= hi; i++) {
        itmp = indx[i];
        j = i;
        while (j>=lo+h && cmptol(indx[j-h], itmp)) {
            indx[j] = indx[j - h]; j -= h;
        }
        indx[j] = itmp;
    }
    return(R_NilValue);
}


