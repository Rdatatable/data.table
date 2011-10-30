
// Copied from do_radixsort in src/main/sort.c @ rev 51389
// Turns out not to be a radix sort, but a counting sort
// See http://r.789695.n4.nabble.com/method-radix-in-sort-list-isn-t-actually-a-radix-sort-tp3309470p3309470.html
// A counting sort does seem to be ideal for factors, and this makes it work for character
// Modified as follows :
// + To work with character, not integer
// + No levels needed. char is kept as char.
// + No need to scan for min and max. unique values are built up in a
//   single pass.
// + No need for range offset, removed.
// + Always increasing so removed the decreasing option
// + NA always first, so removed that option
// + It's not really a sort, but an order that's returned so
//   name change from do_radixsort to countingcharacter
// + Changed to a direct .Call() signature.
// + 100,000 range restriction removed.
// + Uses truelength on CHARSXP to increment and decrement the counts.
// + Sorting the groups is optional. sort=FALSE means keep groups in
//   first appearance order 

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP countingcharacter();
#endif

extern int Rf_Scollate();
void ssort2(SEXP *x, size_t n);
// See end of this file for comments and modifications.


SEXP countingcharacter(SEXP x, SEXP sort, SEXP GER2140)
{
    SEXP ans, tmp, *u;
    size_t i, n, h, k, cumsum, un=0, ualloc;
    if (!isString(x)) error("x is not character");
    if (!isLogical(sort)) error("sort is not logical");
    if (!isLogical(GER2140)) error("GER2140 is not logical");
    n = LENGTH(x);
    if (!LOGICAL(GER2140)[0]) // < R 2.14.0
        for(i=0; i<n; i++) TRUELENGTH(STRING_ELT(x,i)) = 0;    // TO DO: time the cost here
    else
        if (TRUELENGTH(STRING_ELT(x,0))!=0) error("R.version >= 2.14.0 but truelength on first CHARSXP ('%s') is %d (not 0 as expected). length is %d (should be nchar)",CHAR(STRING_ELT(x,0)),TRUELENGTH(STRING_ELT(x,0)),LENGTH(STRING_ELT(x,0)));
    PROTECT(ans = allocVector(INTSXP, n));
    ualloc = 1024;  // small initial guess, negligible time to alloc  
    u = Calloc(ualloc, SEXP);   
    
    // Using truelength as spare storage. Code and runtime inspection revealed
    // it unused and uninitiated in the global CHARSXP cache. Attrib is already
    // used by R in the cache (iiuc), and would be slower to fetch anyway.
    // NB: length is the nchar of each char *, not 1.
    for(i=0; i<n; i++) {
        tmp = STRING_ELT(x,i);
        if (!TRUELENGTH(tmp)) {
            if (un==ualloc) u = Realloc(u, ualloc=ualloc*2>n?n:ualloc*2, SEXP);
            u[un++] = tmp;
        }
        TRUELENGTH(tmp)++;
        // counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]++;
        // TO DO:  is NA_STRING a pointer to a CHARSXP with a truelength to cobble?
    }
    if (LOGICAL(sort)[0]) ssort2(u,un);
    cumsum = TRUELENGTH(u[0]);
    for(i=1; i<un; i++) {                                    // 0.000
        cumsum = (TRUELENGTH(u[i]) += cumsum);
    }
    for(i=n; i>0; i--) {                                     // 0.400 (page fetches on string cache)
        tmp = STRING_ELT(x,i-1);                             // + 0.800 (random access to ans)
        k = --TRUELENGTH(tmp);
        INTEGER(ans)[k] = i;
    } // NB: i is size_t so must be i>0 otherwise underflow to maxint.
    for(i=0; i<un; i++) TRUELENGTH(u[i]) = 0; // The cumsum means the counts are left non zero so reset for next time (0.00).
    // INTEGER(ans)[--counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]] = i+1;
    UNPROTECT(1);
    Free(u);
    return ans;
}


// When the data already happens to be grouped, the time is actually faster than
// base do_radix (0.318 vs 0.181). When data isn't already grouped, the random access to the
// string cache bites twice (0.4+0.4) and do_radix is faster (0.982 vs 1.556).
// The random access to ans (at the end) is about the same in both at 0.8 apx (nearly all of
// do_radix's time) But, do_radix has downsides: it needs extra time
// (and less convenience) to create the (required) factor input before hand (more than
// just creating sorted(unique(x))), and makes joining
// of two factor columns (different levels) more complex and time consuming at the top of [.data.table.
// The 0.4+0.4 when the groups are non-contiguous, and there are
// a lot of groups, may be fairly rare. We may be able to solve that by submitting patch to R
// to make R's string cache contiguous and more compact (likely difficult).
// Of course, a keyed by avoids this anyway, we're only talking ad hoc by here, and
// the differences are small.
//
// Timing tests now moved into test.data.table()/tests.R


// Copied as-is directly from src/main/sort.c :
const size_t incs[16] = {1073790977, 268460033, 67121153, 16783361, 4197377,
		       1050113, 262913, 65921, 16577, 4193, 1073, 281, 77,
		       23, 8, 1};

void ssort2(SEXP *x, size_t n)
// Copied from src/main/sort.c.
// ssort2 is declared static in base i.e. not exposed to packages unfortunately.
// We don't want to call the sortVector wrapper in base because we use Realloc in
// countingcharacter on an SEXP * directly; don't want to copy that into a SEXP vector
// just to call ssort2, for that to merely do a STRING_PTR on the SEXP (again).
// Some modifications needed anyway :
// 1. 'decreasing' removed to avoid the branch inside the 3rd for loop.
// 2. Assumes no NA are present (by construction of countingcharacter) so call to
//    scmp replaced to save 4 branches per item (and one fun call per item), leaving
//    it to the && to short circuit instead.
// 3. NA always go last in data.table, that option (of scmp) removed too, but there
//    won't be any NA anyway
// 4. size_t instead of int
{
    SEXP v;
    size_t i, j, h, t;
    for (t = 0; incs[t] > n; t++);
    for (h = incs[t]; t < 16; h = incs[++t])
	for (i = h; i < n; i++) {
	    v = x[i];
	    j = i;
		while (j>=h && x[j-h]!=v && Rf_Scollate(x[j-h],v) > 0)   // data.table
		// while (j >= h && scmp(x[j - h], v, TRUE) > 0)         // base
		{ x[j] = x[j-h]; j-=h; }
	    x[j] = v;
	}
}

/*
 * For ease of comparison, repeat original from src/main/sort.c
 *
static void ssort2(SEXP *x, int n, Rboolean decreasing)
{
    SEXP v;
    int i, j, h, t;

    for (t = 0; incs[t] > n; t++);
    for (h = incs[t]; t < 16; h = incs[++t])
	for (i = h; i < n; i++) {
	    v = x[i];
	    j = i;
	    if(decreasing)
		while (j >= h && scmp(x[j - h], v, TRUE) < 0)
		{ x[j] = x[j - h]; j -= h; }
	    else
		while (j >= h && scmp(x[j - h], v, TRUE) > 0)
		{ x[j] = x[j - h]; j -= h; }
	    x[j] = v;
	}
}
static int scmp(SEXP x, SEXP y, Rboolean nalast)
{
    if (x == NA_STRING && y == NA_STRING) return 0;
    if (x == NA_STRING) return nalast?1:-1;
    if (y == NA_STRING) return nalast?-1:1;
    if (x == y) return 0;  // same string in cache
    return Scollate(x, y);
}
*/

