
// Copied from do_radixsort in src/main/sort.c @ rev 51389
// Turns out not to be a radix sort, but a counting sort
// See http://r.789695.n4.nabble.com/method-radix-in-sort-list-isn-t-actually-a-radix-sort-tp3309470p3309470.html
// A counting sort does seem to be ideal for factors.
// Modified as follows :
// + To work with character, not integer
// + Requires sort(unique(x)) to be passed in as a hashed
//   environment e.g. stored as attribute of the column.
// + No need to scan for min and max. These are known in
//   advance to be 1 and length(hash)
// + No need for offset, removed.
// + For each character, lookup hash to get integer
// + Always increasing so removed the decreasing option
// + NA always first, so removed that option
// + It's not really a sort, but an order that's returned
// + Name change from do_radixsort to countingcharacterorder
// + Changed to a direct .Call() signature.
// + 100,000 range restriction removed.
// + replaced counts alloc (and =0 loop) with a single calloc

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP countingcharacterorder();
#endif

SEXP countingcharacterorder(SEXP x, SEXP hash)
{
    SEXP ans;
    unsigned int i, n, h, tmp, *counts;
    if (TYPEOF(x) != CHARSXP) error("x is not character");
    n = LENGTH(x);
    h = LENGTH(hash);
    PROTECT(ans = allocVector(INTSXP, n));
    counts = calloc(h+1,sizeof(unsigned int));
    for(i = 0; i < n; i++) {
        tmp = INTEGER(x)[i];  // TO DO: look up hash
        counts[(tmp==NA_INTEGER) ? 0 : tmp]++;
    }
    for(i = 1; i <= h; i++) counts[i] += counts[i-1];
    for(i = n-1; i >= 0; i--) {
        tmp = INTEGER(x)[i];  // TO DO: look up hash
	INTEGER(ans)[--counts[(tmp==NA_INTEGER) ? 0 : tmp]] = i+1;
    }
    UNPROTECT(1);
    free(counts);
    return ans;
}


