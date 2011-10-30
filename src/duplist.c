#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP duplist();
#endif

SEXP duplist(SEXP l, SEXP ans, SEXP anslen, SEXP order, SEXP tol)  //change name to uniqlist
{
    // Returns the positions of the non-repeated rows. This works like
    // UNIX uniq as referred to by ?base::unique; i.e., it drops immediately
    // repeated rows but doesn't drop duplicates of any previous row.
    // Unless, order is provided, then it also drops any previous row.
    // l must be a list of same length integer vectors
    // ans is allocated first (maximum length the number of rows) and the length returned in anslen.
    int i,j,nrow,ncol,len,thisi,previ;
    ncol = length(l);
    nrow = length(VECTOR_ELT(l,0));
    len=1;
    INTEGER(ans)[0] = 1;    // first row is always the first of the first group
    if (INTEGER(order)[0] == -1) {      // i.e. order is missing, same order as row order. Using MISSING() does not seem stable under windows. Always having arguments passed in seems a good idea anyway.
        for (i=1; i<nrow; i++) {
            j = ncol;  // the last columns varies the most frequently so check that first and work backwards
            while (--j>=0 && ( isReal(VECTOR_ELT(l,j)) ? 
                               fabs(REAL(VECTOR_ELT(l,j))[i]-REAL(VECTOR_ELT(l,j))[i-1])<REAL(tol)[0] :
                               INTEGER(VECTOR_ELT(l,j))[i]==INTEGER(VECTOR_ELT(l,j))[i-1] ));
     // INTEGER() will be used here for LOGICAL too (logical now allowed in keys and 'by' from Aug 2010) [ok now, but not future proof] TO DO: revisit
            if (j>=0) INTEGER(ans)[len++] = i+1;
        }
    } else {
        for (i=1; i<nrow; i++) {
            j = ncol;
            thisi = INTEGER(order)[i]-1;
            previ = INTEGER(order)[i-1]-1;
            while (--j>=0 && ( isReal(VECTOR_ELT(l,j)) ? 
                               fabs(REAL(VECTOR_ELT(l,j))[thisi]-REAL(VECTOR_ELT(l,j))[previ])<REAL(tol)[0] :
                               INTEGER(VECTOR_ELT(l,j))[thisi]==INTEGER(VECTOR_ELT(l,j))[previ] ));
            if (j>=0) INTEGER(ans)[len++] = i+1;
        }
    }
    INTEGER(anslen)[0] = len;
    return(R_NilValue);
}


// copied sort2_with_index in orderVector1 from /src/main/sort.c
// modified for :
// i) stability within tolerance
// ii) option of nalast removed as data.table is always NA first (for binary search)
// iii) rather than the loop through n in do_order to ++ the indx to return to R,
//      we point x and indx to 1 pointer before the start, so we can use 1-based
//      indexing.  This allows the o vector up in fast order (the very same o vector
//      with no copies or loops through it doing ++ and --) to be passed on as the
//      columns are looped through in reverse order.

static int rcmp_tol(double x, double y, double tol, int a, int b)
{
    int nax = ISNAN(x), nay = ISNAN(y);
    if (nax && nay)	return 0;
    if (nax)		return -1;
    if (nay)		return 1;
    if (x < y-tol)	return -1;
    if (x > y+tol)	return 1;
    return a>b;
}

extern const size_t incs[];

#define cmptol(a,b) (x[a] > x[b]+tol || (a>b && x[a] > x[b]-tol))
// TO DO: Take account of NAN

void rorder_tol(SEXP xarg, SEXP indxarg, SEXP tolarg)
{
    //double v;
    int t, i, j, h;
    int itmp;
    double *x=REAL(xarg)-1;
    int n=length(xarg);
    int lo = 1, hi = n;  // ignore NAs for now (need to sort to the beginning in data.table (always)
    int *indx=INTEGER(indxarg)-1;
    double tol = REAL(tolarg)[0];

    for (t = 0; incs[t] > hi-lo+1; t++);
	for (h = incs[t]; t < 16; h = incs[++t])
    for (i = lo + h; i <= hi; i++) {
	    itmp = indx[i];
	    j = i;
	    // #define less(a, b) (x[a] > x[b] || (x[a] == x[b] && a > b))
	    //while (j>=lo+h && less(indx[j - h], itmp)) { // x[a] > x[b] || (x[a] == x[b] && a > b))
	    while (j>=lo+h && cmptol(indx[j-h], itmp)) {
	    //while (j >= lo + h && rcmp_tol(x[indx[j - h]], x[itmp], tol, indx[j - h], itmp)>0) {
	        indx[j] = indx[j - h]; j -= h;
		}
		indx[j] = itmp;
    }
}


