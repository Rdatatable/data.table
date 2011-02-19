
// Copied from do_radixsort in src/main/sort.c @ rev 51389
// Turns out not to be a radix sort, but a counting sort
// See http://r.789695.n4.nabble.com/method-radix-in-sort-list-isn-t-actually-a-radix-sort-tp3309470p3309470.html
// A counting sort does seem to be ideal for factors.
// Modified as follows :
// + To work with character, not integer
// + Requires sort(unique(x)) to be passed in as ordered
//   character vector e.g. stored as attribute of the column.
// + No need to scan for min and max. These are known in
//   advance to be 1 and length(charlevels)
// + No need for range offset, removed.
// + Always increasing so removed the decreasing option
// + NA always first, so removed that option
// + It's not really a sort, but an order that's returned so
//   name change from do_radixsort to countingcharacterorder
// + Changed to a direct .Call() signature.
// + 100,000 range restriction removed.
// + Replaced counts alloc (and =0 loop) with a single calloc

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP countingcharacterorder();
#endif

SEXP countingcharacterorder(SEXP x, SEXP charlevels)
{
    SEXP ans, tmp;
    R_len_t i, n, h, *counts;
    if (TYPEOF(x) != STRSXP) error("x is not character");
    if (TYPEOF(charlevels) != STRSXP) error("charlevels is not character");
    n = LENGTH(x);
    h = LENGTH(charlevels);
    PROTECT(ans = allocVector(INTSXP, n));
    counts = calloc(h+1,sizeof(R_len_t));
    for(i=0; i<h; i++) TRUELENGTH(STRING_ELT(charlevels,i)) = i+1;
    // Using truelength as spare storage. Code and runtime inspection revealed
    // it unused and uninitiated in the global CHARSXP cache. Attrib is already
    // used internally, and would be slower to fetch anyway.
    // If r-core object, or R changes in future, we could store and restore 
    // truelength at low cost.
    for(i = 0; i < n; i++) {
        tmp = STRING_ELT(x,i);
        counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]++;
    }
    for(i = 1; i <= h; i++) counts[i] += counts[i-1];
    for(i = n-1; i >= 0; i--) {
        tmp = STRING_ELT(x,i);
        INTEGER(ans)[--counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]] = i+1;
    }
    UNPROTECT(1);
    free(counts);
    return ans;
}


