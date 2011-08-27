
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
//   name change from do_radixsort to countcharorder
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
# include <sys/time.h>

double now()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return((double) tv.tv_sec + 1e-6 * (double) tv.tv_usec);
}

SEXP countingcharacterorder(SEXP x, SEXP charlevels)
{
    SEXP ans, tmp; 
    //double startedat;
    int i, n, h, k, cumsum;
    // startedat=now();
    if (TYPEOF(x) != STRSXP) error("x is not character");
    if (TYPEOF(charlevels) != STRSXP) error("charlevels is not character");
    n = LENGTH(x);
    h = LENGTH(charlevels);
    // Rprintf("1: %3.3f\n",now()-startedat);
    PROTECT(ans = allocVector(INTSXP, n));
    for(i=0; i<h; i++) TRUELENGTH(STRING_ELT(charlevels,i)) = 0;    // 0.000
    // Using truelength as spare storage. Code and runtime inspection revealed
    // it unused and uninitiated in the global CHARSXP cache. Attrib is already
    // used by R in the cache (iiuc), and would be slower to fetch anyway.
    // If r-core object, or R changes in future, we could store and restore 
    // truelength at low cost. (NB: length is the nchar of each char *, not 1).
    // Rprintf("2: %3.3f\n",now()-startedat);
    for(i=0; i<n; i++) {                                            // 0.400 (page fetches on string cache)
        tmp = STRING_ELT(x,i);
        TRUELENGTH(tmp)++;
        // counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]++;
        // TO DO:  is NA_STRING a pointer to a CHARSXP with a truelength to cobble?
    }
    // Rprintf("3: %3.3f\n",now()-startedat);   //
    cumsum = TRUELENGTH(STRING_ELT(charlevels,0));
    for(i=1; i<h; i++) {                                            // 0.000
        cumsum = (TRUELENGTH(STRING_ELT(charlevels,i)) += cumsum);
    }
    // Rprintf("4: %3.3f\n",now()-startedat);                          
    for(i = n-1; i >= 0; i--) {                                     // 0.400 (page fetches on string cache) 
        tmp = STRING_ELT(x,i);                                      // + 0.800 (random access to ans)
        k = --TRUELENGTH(tmp);
        INTEGER(ans)[k] = i+1;
    }
    // INTEGER(ans)[--counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]] = i+1;
    // --counts[TRUELENGTH(tmp)];
    // k[--counts[TRUELENGTH(tmp)]] = i+1;
    // Rprintf("5: %3.3f\n",now()-startedat);   //
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
system.time(o1 <- .Call("countingcharacterorder",x,cl))        # 1.556
system.time(f <- factor(x))                                    # 7.611
system.time(o2 <- sort.list(f,method="radix"))                 # 0.982
identical(o1,o2)
fsorted = f[o2]
xsorted = x[o2]
system.time(o3 <- .Call("countingcharacterorder",xsorted,cl))  # 0.181
gc()                                                           # gcFirst=TRUE on next line wasn't always enough, it seems
system.time(o4 <- sort.list(fsorted,method="radix"),TRUE)      # 0.318 (even with TRUE sometimes gave 0.720)
identical(o3,o4)
*/


//  Various (failed) efforts at hashing to avoid page fetching on the global string cache ...
//  Committing (temporarily) in case come back to it in future.  Will delete on next commit.


SEXP baseradix(SEXP x)  //call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    double startedat;
    Rboolean nalast, decreasing;
    int i, n, tmp, xmax = NA_INTEGER, xmin = NA_INTEGER, off, napos;
    startedat=now();
    nalast = 0; //asLogical(0); //CADR(args));
    decreasing = 0; //asLogical(0); //asLogical(CADDR(args));
    off = nalast^decreasing ? 0 : 1;
    n = LENGTH(x);
    PROTECT(ans = allocVector(INTSXP, n));
    Rprintf("1: %3.3f\n",now()-startedat);
    for(i = 0; i < n; i++) {
	tmp = INTEGER(x)[i];
	if(tmp == NA_INTEGER) continue;
	if(xmax == NA_INTEGER || tmp > xmax) xmax = tmp;
	if(xmin == NA_INTEGER || tmp < xmin) xmin = tmp;
    }
    Rprintf("2: %3.3f\n",now()-startedat);
    if(xmin == NA_INTEGER) {  /* all NAs, so nothing to do */
	for(i = 0; i < n; i++) INTEGER(ans)[i] = i+1;
	UNPROTECT(1);
	return ans;
    }
    Rprintf("3: %3.3f\n",now()-startedat);
    xmax -= xmin;
    //if(xmax > 100000) error(_("too large a range of values in 'x'"));
    napos = off ? 0 : xmax + 1;
    off -= xmin;
    /* automatic allocation is fine here: we know this is small */
    unsigned int cnts[xmax+1];
    Rprintf("4: %3.3f\n",now()-startedat);
    for(i = 0; i <= xmax+1; i++) cnts[i] = 0;
    Rprintf("5: %3.3f\n",now()-startedat);
    for(i = 0; i < n; i++) {
	if(INTEGER(x)[i] == NA_INTEGER) cnts[napos]++;
	else cnts[off+INTEGER(x)[i]]++;
    }
    Rprintf("6: %3.3f\n",now()-startedat);
    for(i = 1; i <= xmax+1; i++) cnts[i] += cnts[i-1];
    Rprintf("7: %3.3f\n",now()-startedat);
    if(decreasing)
	for(i = 0; i < n; i++){
	    tmp = INTEGER(x)[i];
	    INTEGER(ans)[n-(cnts[(tmp==NA_INTEGER) ? napos : off+tmp]--)] = i+1;
	}
    else
	for(i = n-1; i >= 0; i--) {
	    tmp = INTEGER(x)[i];
	    INTEGER(ans)[--cnts[(tmp==NA_INTEGER) ? napos : off+tmp]] = i+1;
            // 1s of the 1.187 is the line above, too!  It's all memory in random access!
            // It's all page fetch.
            // Looping through by n shows up as closed to 0! Even with all the if's.
	}
    Rprintf("8: %3.3f\n",now()-startedat);
    UNPROTECT(1);
    return ans;
}


SEXP ccoqing(SEXP x, SEXP charlevels)
{
    SEXP ans, tmp; 
    double startedat;
    int i, n, h, k;
    //unsigned int *counts;
    //usigned int **p; 
    startedat=now();
    if (TYPEOF(x) != STRSXP) error("x is not character");
    if (TYPEOF(charlevels) != STRSXP) error("charlevels is not character");
    n = LENGTH(x);
    h = LENGTH(charlevels);
    //k = calloc(n+1,sizeof(int));
    PROTECT(ans = allocVector(INTSXP, n));

    //counts = calloc(h+1,sizeof(R_len_t));
    // do_radixsort in base (sort.c) uses a VLA allocation in C99 with a 100,000 restriction
    // The crucial thing is that counts is unsigned int. If it is
    
    unsigned int counts[h+1], *p[h+1];
    for(i = 0; i <= h; i++) counts[i] = 0;

    //counts[0]=0;
    Rprintf("1: %3.3f\n",now()-startedat);  //0.000
    for(i=0; i<h; i++) TRUELENGTH(STRING_ELT(charlevels,i)) = i+1;
    Rprintf("2: %3.3f\n",now()-startedat);  //0.000
        //counts[i+1]=0;
    // Using truelength as spare storage. Code and runtime inspection revealed
    // it unused and uninitiated in the global CHARSXP cache. Attrib is already
    // used internally, and would be slower to fetch anyway.
    // If r-core object, or R changes in future, we could store and restore 
    // truelength at low cost.

    //  CAN WE SET TRUELENGTH OF NA_STRING TO 0, and save the if,  twice ?
    //  ALSO CAN WE USE STATIC STORAGE LIKE THE BASE ONE DOES ? (just re-alloc on each one but would still need to loop through to set to 0?) - probably isn't that, then
    //  might the truelength be calling as function?
    // don't understand this really, it has less loops through n (no min/max with if's) so why
    // slower than do_radix ?
    for(i = 0; i < n; i++) {
        tmp = STRING_ELT(x,i);
        //counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]++;
        counts[TRUELENGTH(tmp)]++;   // shouldn't this be adding i to the queue ??
    }
    Rprintf("3: %3.3f\n",now()-startedat);   // 0.540 - can be done in advance

    p[1] = INTEGER(ans);
    for(i = 1; i < h; i++) p[i+1] = p[i]+counts[i];

    Rprintf("4: %3.3f\n",now()-startedat);   // 0.000
    for(i=0; i<n; i++) {
        tmp = STRING_ELT(x,i);
        *(p[TRUELENGTH(tmp)]++) = i+1;
        //INTEGER(ans)[k] = i+1;
    }
//INTEGER(ans)[--counts[(tmp==NA_STRING) ? 0 : TRUELENGTH(tmp)]] = i+1;
        //--counts[TRUELENGTH(tmp)];
        //k[--counts[TRUELENGTH(tmp)]] = i+1;
    Rprintf("5: %3.3f\n",now()-startedat);   // 1.900  (0.5 of which is the counts random access, 1 is the ans random access
    UNPROTECT(1);
    //free(counts);
    Rprintf("6: %3.3f\n",now()-startedat);
    return ans;
}


// It's random access assignment to ans that dominates both
// Even if queing, the queues are still random access!
// If we squash the queues into chunks, we'll then do more random access to assign afterwards. Grr.

// Best, then is a perfect hash on the pointers ?   min to max / mod (size(CHARSXP))


SEXP countingcharmk2(SEXP x, SEXP charlevels)
{
    SEXP ans;
    double startedat;
    Rboolean nalast, decreasing;
    int n, h;
    unsigned int i, tmp, xmax, xmin, off, napos, s, ip;
    SEXP p;
    startedat=now();
    nalast = 0; //asLogical(0); //CADR(args));
    decreasing = 0; //asLogical(0); //asLogical(CADDR(args));
    off = nalast^decreasing ? 0 : 1;
    n = LENGTH(x);
    h = LENGTH(charlevels);
    PROTECT(ans = allocVector(INTSXP, n));
    Rprintf("1: %3.3f\n",now()-startedat);

    //unsigned int bitcount[32];
    //int b;
    //for (b=0;b<32;b++)bitcount[b]=0;

    // TO DO: remove this bit !
    xmax=xmin=(unsigned int)STRING_ELT(charlevels,0);
    for(i = 1; i < h; i++) {   // there is no NA in the charlevels !!
        tmp = (unsigned int)STRING_ELT(charlevels,i);
        if (tmp > xmax) xmax = tmp;
        if (tmp < xmin) xmin = tmp;
    }
    // To test is perfect hash is ok,  apply it and if it doesn't equal i then we know to re-create!! :)

    // TO DO - drop this and just use standard hash without -xmin/s
    // first just make it a hash that doesn't preserve order, and still use order vector afterwards.
    // then make it order preserving later :-)

    Rprintf("2: %3.3f\n",now()-startedat);
    Rprintf("%d %d\n", xmin, xmax);
    //if(xmin == NA_INTEGER) {  /* all NAs, so nothing to do */
    //	for(i = 0; i < n; i++) INTEGER(ans)[i] = i+1;
    //	UNPROTECT(1);
    //	return ans;
    //}
    Rprintf("3: %3.3f\n",now()-startedat);
    xmax -= xmin;
    s = sizeof(VECTOR_SEXPREC)+1;  // even the "" string has the /0 terminator. Could try adding 2 or 4 instead, depending on minimum size for the vector part (might be 4 bytes anyway).
    xmax = xmax / s;
    // The +1 is for strings at least 1 byte long, if longer then fragmentation is our index (maybe twice or more!)
    Rprintf("xmax is %d\n",xmax);

    //for(i = 0; i < h; i++) {
    //    tmp = ((unsigned int)STRING_ELT(charlevels,i)-xmin)/s;
    //    for (b=0;b<32;b++) bitcount[b]+=(tmp&(1<<b))>0;   // doesn't have to go to 32 since we know xmax
    //}
    //for (b=0;b<32;b++) Rprintf("Bit count %d = %u\n", b, bitcount[b]);

    unsigned int size, bits, maxhash;
    bits = 31-(unsigned int)log2(h);
    size = 2<<(int)log2(h);
    Rprintf("size = %d\n",size);
    unsigned int collide[size];
    for(i=0;i<size;i++) collide[i]=0;
    maxhash=0;
    for(i = 0; i < h; i++) {
        tmp = (3141592653U * ((unsigned int)STRING_ELT(charlevels,i)>>2)) >> bits;
        //tmp = ((((unsigned int)STRING_ELT(charlevels,i))-xmin)/s) % size;
        collide[tmp]++;
        //INTEGER(ans)[tmp]++;
        if (tmp>maxhash) maxhash=tmp;
    }
    tmp=0;
    for(i=0;i<size;i++)tmp+=collide[i];
    Rprintf("Sum of collide = %d\n",tmp);
    Rprintf("Max hash value = %d\n",maxhash);
    if (tmp!=h) error("sum of collide is not h");
    tmp=0;
    for(i=0;i<size;i++)if (collide[i]>tmp) tmp=collide[i];
    int maxcoll = tmp;
    Rprintf("Worst number of collisions = %d\n",maxcoll);
    int collcounts[maxcoll+1];
    for(i=0;i<=maxcoll;i++)collcounts[i]=0; 
    for(i=0;i<size;i++) collcounts[collide[i]]++;
    Rprintf("%d of %d unused hash values\n", collcounts[0], size);
    tmp = 0;
    for(i=1;i<=maxcoll;i++)Rprintf("Number of %d collisions = %d\n", i-1, i*collcounts[i]);
    //UNPROTECT(1);
    //return(ans);
    //    [ 0  4  0  2  1  1  0 ]   collide
    //    [ 0  0  0  4  6  7  0 ]   hashv
    //    [ 4.1 4.2 4.3 4.4 2.1 2.2 1.1 1.1 0 ]  where x.y are the CHARSXP pointers,  probe
    //    [ 12 3 19 45  2  9  3 ]   the order

    unsigned int hashv[size], probe[h], order[h];  // very small, easily in L2 cache
    tmp = 0;
    for (i=0;i<size;i++) {
        if (collide[i]>0) {
            hashv[i]=tmp;
            tmp+=collide[i];
        } else hashv[i]=0;  // no need to set really, but for safety
    }
    Rprintf("4: %3.3f\n",now()-startedat);
    for (i=0;i<h;i++) probe[i]=0;
    for(i = 0; i < h; i++) {
        ip = (unsigned int)STRING_ELT(charlevels,i);
        tmp = (3141592653U * (ip>>2)) >> bits;
        //tmp = ((ip-xmin)/s) % size;
        tmp = hashv[tmp];
        while (probe[tmp]) tmp++;
        probe[tmp]=ip;
        order[tmp]=i+1;
    }
    Rprintf("5: %3.3f\n",now()-startedat);
    //for (i=0;i<100;i++)Rprintf("%10d %10d %10d %10d\n",(unsigned int)STRING_ELT(charlevels,i),hashv[i],probe[i],order[i]);
    if(xmax > 1000000) error("too large a range of values in 'x'");
    //napos = off ? 0 : xmax + 1;
    //off -= xmin;
    /* automatic allocation is fine here: we know this is small */
    unsigned int counts[h+1];  //, order[xmax+s];
    //counts[0] = 0;
    for(i=0; i<=h; i++) counts[i] = 0;
    //    order[1+(((unsigned int)STRING_ELT(charlevels,i))-xmin)/s] = i+1;
    //}
    Rprintf("6: %3.3f\n",now()-startedat);
    for(i = 0; i < n; i++) {
        p = STRING_ELT(x,i);
        //if(p == NA_STRING) counts[0]++;
        //else {
            ip = (unsigned int)p;
            tmp = (3141592653U * (ip>>2)) >> bits;
            tmp = hashv[tmp];
            while (probe[tmp]!=ip) tmp++;   // a little ~ 0.2s
            counts[order[tmp]]++;    // lots time ~ 0.7s
        //}
    }
    Rprintf("7: %3.3f\n",now()-startedat);
    for(i = 1; i <= h; i++) counts[i] += counts[i-1];
    Rprintf("8: %3.3f\n",now()-startedat);
    i = n;
    while(i>0) {
        i--;
        p = STRING_ELT(x,i);
        //if (p==NA_STRING) INTEGER(ans)[--counts[0]]=i+1;
        //else {
            ip = (unsigned int)p;
            tmp = (3141592653U * (ip>>2)) >> bits; //hashv[((ip-xmin)/s)%size];
            tmp = hashv[tmp];
            while (probe[tmp]!=ip) tmp++;
            INTEGER(ans)[--counts[order[tmp]]]=i+1;
        //}
        // 1s of the 1.187 is the line above, too!  It's all memory in random access!
        // It's all page fetch.
        // Looping through by n shows up as closed to 0! Even with all the if's.
    }
    Rprintf("9: %3.3f\n",now()-startedat);
    UNPROTECT(1);
    return ans;
}

// Some kind of shrinkage of xmax, in case of gaps ?

/*
remove 9 bits (no distinguishing)
pick bit 6 (closest to 5000)  -  likely but may not be the one!
which bit divides the rest into 4 the best? i.e. avg = 2500.  Skipping 9bit and 6,  count where 6 is 1 and count where 6 is 0.  Choose where average is best, say bit 8.
skipping 9bit, 6 and 8,  count where 6 and 8 are (0,0),(0,1),(1,0),(1,1)


then for the 23 remaining, for each one look at pairwise counts
when on and when off




Just goes through h, not n.
*/
   




// throw away where all 0 or 10000   (9 bits gone)
// of remaining, are any included in others i.e. if bit 3 is on is bit 4 always on too ?


// we know we should have clusters of sequences length 1 in memory (unless they are very long strings, meaning many gaps.
// lets just count the colisions ...



