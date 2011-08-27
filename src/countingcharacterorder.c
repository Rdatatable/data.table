
// Copied from do_radixsort in src/main/sort.c @ rev 51389
// Turns out not to be a radix sort, but a counting sort
// See http://r.789695.n4.nabble.com/method-radix-in-sort-list-isn-t-actually-a-radix-sort-tp3309470p3309470.html
// A counting sort does seem to be ideal for factors.
// Modified as follows :
// + To work with character, not integer
// + Requires sort(unique(x)) to be passed in as ordered
//   character vector e.g. stored as attribute of the column,
//   like factor (but keeping char as char, no integers).
// + No need to scan for min and max. These are known in
//   advance to be 1 and length(charlevels)
// + No need for range offset, removed.
// + Always increasing so removed the decreasing option
// + NA always first, so removed that option
// + It's not really a sort, but an order that's returned so
//   name change from do_radixsort to countingcharacterorder
// + Changed to a direct .Call() signature.
// + 100,000 range restriction removed.
// + Replaced counts alloc (and =0 loop) with a single calloc,
//   and then just removed counts altogether.


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
    int i, n, h, k, cumsum;
    if (TYPEOF(x) != STRSXP) error("x is not character");
    if (TYPEOF(charlevels) != STRSXP) error("charlevels is not character");
    n = LENGTH(x);
    h = LENGTH(charlevels);
    PROTECT(ans = allocVector(INTSXP, n));
    for(i=0; i<h; i++) TRUELENGTH(STRING_ELT(charlevels,i)) = 0;    // 0.000
    // Using truelength as spare storage. Code and runtime inspection revealed
    // it unused and uninitiated in the global CHARSXP cache. Attrib is already
    // used by R in the cache (iiuc), and would be slower to fetch anyway.
    // If r-core object, or R changes in future, we could store and restore 
    // truelength at low cost. (NB: length is the nchar of each char *, not 1).
    for(i=0; i<n; i++) {                                            // 0.400 (page fetches on string cache)
        tmp = STRING_ELT(x,i);
        TRUELENGTH(tmp)++;
        // counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]++;
        // TO DO:  is NA_STRING a pointer to a CHARSXP with a truelength to cobble?
    }
    cumsum = TRUELENGTH(STRING_ELT(charlevels,0));
    for(i=1; i<h; i++) {                                            // 0.000
        cumsum = (TRUELENGTH(STRING_ELT(charlevels,i)) += cumsum);
    }
    for(i = n-1; i >= 0; i--) {                                     // 0.400 (page fetches on string cache) 
        tmp = STRING_ELT(x,i);                                      // + 0.800 (random access to ans)
        k = --TRUELENGTH(tmp);
        INTEGER(ans)[k] = i+1;
    }
    // INTEGER(ans)[--counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]] = i+1;
    // --counts[TRUELENGTH(tmp)];
    // k[--counts[TRUELENGTH(tmp)]] = i+1;
    UNPROTECT(1);
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
// Above timings in this comment were on a 10 million vector of randomly
// scattered non-contiguous 10,000 (4 byte) strings, see below.  With fewer levels, the
// string cache hit issue reduces.
/*
n = 1e7
m = 10000
x = sample(as.character(as.hexmode(1:m)),n,replace=TRUE)
system.time(cl <- sort(unique(x)))                             # 4.492  
system.time(o1 <- .Call("countingcharacterorder",x,cl))        # 1.556   (4.49+1.55=6.04)
system.time(f <- factor(x))                                    # 7.611
system.time(o2 <- sort.list(f,method="radix"))                 # 0.982   (7.61+0.98=8.59)
identical(o1,o2)
fsorted = f[o2]
xsorted = x[o2]
system.time(o3 <- .Call("countingcharacterorder",xsorted,cl))  # 0.181
gc()                                                           # gcFirst=TRUE on next line wasn't always enough, it seems
system.time(o4 <- sort.list(fsorted,method="radix"),TRUE)      # 0.318 (even with TRUE sometimes gave 0.720)
identical(o3,o4)
*/


