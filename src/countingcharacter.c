
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

#define ENC_KNOWN(x) (LEVELS(x) & 76)
// LATIN1_MASK (1<<2) | UTF8_MASK (1<<3) | ASCII_MASK (1<<6)

void ssort2(SEXP *x, R_len_t n);
// See end of this file for comments and modifications.

extern SEXP *saveds;
extern R_len_t *savedtl, nalloc, nsaved;
extern void savetl_init(), savetl(SEXP s), savetl_end();

SEXP countingcharacter(SEXP x, SEXP sort)
{
    SEXP ans, tmp, *u, s;
    R_len_t i, n, k, cumsum, un=0, ualloc;
    if (!isString(x)) error("x is not character vector");
    if (!isLogical(sort)) error("sort is not logical vector");
    n = LENGTH(x);
    if (!n) return(allocVector(INTSXP,0));
    savetl_init();
    for(i=0; i<n; i++) {
        s = STRING_ELT(x,i);
        if (TRUELENGTH(s)!=0) {
            savetl(s);    // pre-2.14.0 this will save all the uninitialised truelength (i.e. random data)
                          // TO DO: post 2.14.0 can we assume hash>0 and use sign bit to avoid this pass?
                          // pre-v1.8.0 GER2140 was passed in here, but now removed. Make this optimisation if/when
                          // when make data.table depend on R 2.14.0.
            SET_TRUELENGTH(s,0);
        }
    }
    PROTECT(ans = allocVector(INTSXP, n));
    ualloc = 1024;  // small initial guess, negligible time to alloc  
    u = Calloc(ualloc, SEXP);   
    
    // Using truelength as spare storage, saving HASHPRI first if present. Attrib is already
    // used by R in the cache (iiuc), and would be slower to fetch anyway.
    // NB: length is the nchar of each char *, not 1.
    for(i=0; i<n; i++) {
        tmp = STRING_ELT(x,i);
        if (!TRUELENGTH(tmp)) {
            if (un==ualloc) u = Realloc(u, ualloc=ualloc*2>n?n:ualloc*2, SEXP);
            u[un++] = tmp;
        }
        SET_TRUELENGTH(tmp, TRUELENGTH(tmp)+1);
        // counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]++;
        // TO DO:  is NA_STRING a pointer to a CHARSXP with a truelength to cobble?
    }
    if (LOGICAL(sort)[0]) ssort2(u,un);
    cumsum = TRUELENGTH(u[0]);
    for(i=1; i<un; i++) {                                    // 0.000
        cumsum += TRUELENGTH(u[i]);
        SET_TRUELENGTH(u[i], cumsum);
    }
    for(i=n; i>0; i--) {                                     // 0.400 (page fetches on string cache)
        tmp = STRING_ELT(x,i-1);                             // + 0.800 (random access to ans)
        SET_TRUELENGTH(tmp, TRUELENGTH(tmp)-1);
        k = TRUELENGTH(tmp);
        INTEGER(ans)[k] = i;
    } // Aside: for loop bounds written with unsigned int (such as size_t) in mind (when i>=0 would result in underflow and infinite loop).
    for(i=0; i<un; i++) SET_TRUELENGTH(u[i],0); // The cumsum means the counts are left non zero so reset for next time (0.00).
    // INTEGER(ans)[--counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]] = i+1;  // TO DO: tests for NA in character vectors
    savetl_end();
    Free(u);
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
// Timing tests now moved into test.data.table()/tests.R


// Copied as-is directly from src/main/sort.c :
const int incs[16] = {1073790977, 268460033, 67121153, 16783361, 4197377,
               1050113, 262913, 65921, 16577, 4193, 1073, 281, 77,
               23, 8, 1};

int StrCmp(SEXP x, SEXP y)
{
    if (x == NA_STRING) return (y == NA_STRING ? 0 : -1);
    if (y == NA_STRING) return 1;
    if (x == y) return 0;  // same string in cache
    return strcmp(CHAR(x), CHAR(y));
}

void ssort2(SEXP *x, R_len_t n)
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
{
    SEXP v;
    R_len_t i, j, h, t;
    for (t = 0; incs[t] > n; t++);
    for (h = incs[t]; t < 16; h = incs[++t])
    for (i = h; i < n; i++) {
        v = x[i];
        j = i;
        while (j>=h && x[j-h]!=v && StrCmp(x[j-h],v) > 0)   // assumes ASCII.  TO DO: reinvestigate non-ascii.
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
*/
SEXP match_logical(SEXP table, SEXP x) {
    R_len_t i;
    SEXP ans, m;
    ans = PROTECT(allocVector(LGLSXP, length(x)));
    m = PROTECT(match(table, x, 0)); // nomatch=0
    for (i=0; i<length(x); i++)
        INTEGER(ans)[i] = INTEGER(m)[i] > 0;
    UNPROTECT(2);
    return(ans);
}

SEXP chmatch(SEXP x, SEXP table, R_len_t nomatch, Rboolean in) {
    R_len_t i, m;
    SEXP ans, s;
    if (!isString(x) && !isNull(x)) error("x is type '%s' (must be 'character' or NULL)", type2char(TYPEOF(x)));
    if (!isString(table) && !isNull(table)) error("table is type '%s' (must be 'character' or NULL)", type2char(TYPEOF(table)));    
    savetl_init();
    for (i=0; i<length(x); i++) {
        s = STRING_ELT(x,i);
        if (s != NA_STRING && !ENC_KNOWN(s)) {
            // symbols (i.e. as.name() or as.symbol()) containing non-ascii characters (>127) seem to be 'unknown' encoding in R.
            // The same string in different encodings (where unknown encoding is different to known, too) are different
            // CHARSXP pointers; this chmatch relies on pointer equality only. We tried mkChar(CHAR(s)) [ what install() does ]
            // but this impacted the fastest cases too much. Hence fall back to match() on the first unknown encoding detected
            // since match() considers the same string in different encodings as equal (but slower). See #2538 and #4818. 
            savetl_end();
            return (in ? match_logical(table, x) : match(table, x, nomatch));
        }
        if (TRUELENGTH(s)>0) savetl(s); 
        // as from v1.8.0 we assume R's internal hash is positive. So in R < 2.14.0 we
        // don't save the uninitialised truelengths that by chance are negative, but
        // will save if positive. Hence R >= 2.14.0 may be faster and preferred now that R
        // initializes truelength to 0 from R 2.14.0.
        SET_TRUELENGTH(s,0);
    }
    for (i=length(table)-1; i>=0; i--) {
        s = STRING_ELT(table,i);
        if (s != NA_STRING && !ENC_KNOWN(s)) {
            for (int j=i+1; j<LENGTH(table); j++) SET_TRUELENGTH(STRING_ELT(table,j),0);  // reinstate 0 rather than leave the -i-1
            savetl_end();  // and then reinstate HASHPRI (if any) over the 0
            return (in ? match_logical(table, x) : match(table, x, nomatch));
        }
        if (TRUELENGTH(s)>0) savetl(s);
        SET_TRUELENGTH(s, -i-1);
    }
    if (in) {
        PROTECT(ans = allocVector(LGLSXP,length(x)));
        for (i=0; i<length(x); i++) {
            LOGICAL(ans)[i] = TRUELENGTH(STRING_ELT(x,i))<0;
            // nomatch ignored for logical as base does I think
        }
    } else {
        PROTECT(ans = allocVector(INTSXP,length(x)));
        for (i=0; i<length(x); i++) {
            m = TRUELENGTH(STRING_ELT(x,i));
            INTEGER(ans)[i] = (m<0) ? -m : nomatch;
        }
    }
    for (i=0; i<length(table); i++)
        SET_TRUELENGTH(STRING_ELT(table,i),0);  // reinstate 0 rather than leave the -i-1
    savetl_end();   // and then reinstate HASHPRI (if any) over the 0
    UNPROTECT(1);
    return(ans);
}

SEXP chmatchwrapper(SEXP x, SEXP table, SEXP nomatch, SEXP in) {
    return(chmatch(x,table,INTEGER(nomatch)[0],LOGICAL(in)[0]));
}

