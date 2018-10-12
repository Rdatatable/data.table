#include "data.table.h"
// #define TIMING_ON

/*
  - Only forder() and *twiddle() functions are meant for use by other C code in data.table, hence all other functions here except forder and *twiddle are static.
  - The static functions here make use of global variables, whose scope is limited to the same translation unit; therefore access from outside this file must be via forder and/or *twiddle only.
  - The coding techniques deployed here are for efficiency; e.g. i) the static functions are recursive or called repetitively and we wish to minimise stack overhead, or ii) reach outside themselves to place results in the end result directly rather than returning small bits of memory.
*/

static int *gs[2] = {NULL};                                         // gs = groupsizes e.g. 23,12,87,2,1,34,...
static int flip = 0;                                                // two vectors flip flopped: flip and 1-flip
static int gsalloc[2] = {0};                                        // allocated stack size
static int gsngrp[2] = {0};                                         // used
static int gsmax[2] = {0};                                          // max grpn so far
static int gsmaxalloc = 0;                                          // max size of stack, set by forder to nrows
static void *xsub = NULL;                                           // allocated by forder; local function arguments named xsub point into it
static int *newo = NULL;                                            // used by forder and [i|d|c]sort to reorder order. not needed if length(by)==1
static int *otmp = NULL; static int otmp_alloc = 0;
static void *xtmp = NULL; static int xtmp_alloc = 0;                // TO DO: save xtmp if possible, see allocs in forder
static void *radix_xsub = NULL; static size_t radix_xsub_alloc = 0;
static int *cradix_counts = NULL; static int cradix_counts_alloc = 0;
static int maxlen = 1;
static SEXP *cradix_xtmp = NULL; static int cradix_xtmp_alloc = 0;
static int *csort_otmp=NULL; static int csort_otmp_alloc=0;
static SEXP *ustr = NULL; static int ustr_alloc = 0; static int ustr_n = 0;

#define Error(...) do {cleanup(); error(__VA_ARGS__);} while(0)      // http://gcc.gnu.org/onlinedocs/cpp/Swallowing-the-Semicolon.html#Swallowing-the-Semicolon
#undef warning
#define warning(...) Do not use warning in this file                // since it can be turned to error via warn=2
/* Using OS realloc() in this file to benefit from (often) in-place realloc() to save copy
 * We have to trap on exit anyway to call savetl_end().
 * NB: R_alloc() would be more convenient (fails within) and robust (auto free) but there is no R_realloc(). Implementing R_realloc() would be an alloc and copy, iiuc.
 *     Calloc/Realloc needs to be Free'd, even before error() [R-exts$6.1.2]. An oom within Calloc causes a previous Calloc to leak so Calloc would still needs to be trapped anyway.
 * Therefore, using <<if (!malloc()) Error("helpful context msg")>> approach to cleanup() on error.
 */

static void cleanup() {
  free(gs[0]); gs[0] = NULL;
  free(gs[1]); gs[1] = NULL;
  flip = 0;
  gsalloc[0] = gsalloc[1] = 0;
  gsngrp[0] = gsngrp[1] = 0;
  gsmax[0] = gsmax[1] = 0;
  gsmaxalloc = 0;

  free(xsub);             xsub=NULL;
  free(newo);             newo=NULL;
  free(otmp);             otmp=NULL;             otmp_alloc=0;
  free(xtmp);             xtmp=NULL;             xtmp_alloc=0;
  free(radix_xsub);       radix_xsub=NULL;       radix_xsub_alloc=0;
  free(cradix_counts);    cradix_counts=NULL;    cradix_counts_alloc=0;
  maxlen = 1;
  free(cradix_xtmp);      cradix_xtmp=NULL;      cradix_xtmp_alloc=0;   // TO DO: reuse xtmp
  free(csort_otmp);       csort_otmp=NULL;       csort_otmp_alloc=0;

  for(int i=0; i<ustr_n; i++)
    SET_TRUELENGTH(ustr[i],0);
  free(ustr);             ustr=NULL;             ustr_alloc=0; ustr_n=0;
  savetl_end();  // Restore R's own usage of tl. Must run after the for loop above since only CHARSXP which had tl>0 (R's usage) are stored there.
}

static int nalast = -1;                                             // =1, 0, -1 for TRUE, NA, FALSE respectively. Value rewritten inside forder().
                                                                    // note that na.last=NA (0) removes NAs, not retains them.
static int order = 1;                                               // =1, -1 for ascending and descending order respectively
static Rboolean stackgrps = TRUE;                                   // switched off for last column when not needed by setkey
static Rboolean sortStr = TRUE;                                     // TRUE for setkey, FALSE for by=

#define N_SMALL 200                                                 // replaced n < 200 with n < N_SMALL. Easier to change later
#define N_RANGE 100000                                              // range limit for counting sort. UPDATE: should be less than INT_MAX (see setRange for details)

static void growstack(uint64_t newlen) {
  if (newlen==0) newlen=4096;                                       // thus initially 4 pages since sizeof(int)==4
  if (newlen>gsmaxalloc) newlen=gsmaxalloc;                         // gsmaxalloc is type int so newlen can now be cast to int ok
  gs[flip] = realloc(gs[flip], newlen*sizeof(int));
  if (gs[flip] == NULL) Error("Failed to realloc working memory stack to %d*4bytes (flip=%d)", (int)newlen, flip);
  gsalloc[flip] = newlen;
}

static void newpush(int *x, int n) {
  if (!stackgrps || x==0) return;
  if (gsalloc[flip] < gsngrp[flip]+n) growstack(((uint64_t)(gsngrp[flip])+n)*2);
  memcpy(gs[flip]+gsngrp[flip], x, n*sizeof(int));
  gsngrp[flip]+=n;
}

static void push(int x) {
  if (!stackgrps || x==0) return;
  if (gsalloc[flip] == gsngrp[flip]) growstack((uint64_t)(gsngrp[flip])*2);
  gs[flip][gsngrp[flip]++] = x;
  if (x > gsmax[flip]) gsmax[flip] = x;
}

static void mpush(int x, int n) {
  if (!stackgrps || x==0) return;
  if (gsalloc[flip] < gsngrp[flip]+n) growstack(((uint64_t)(gsngrp[flip])+n)*2);
  for (int i=0; i<n; i++) gs[flip][gsngrp[flip]++] = x;
  if (x > gsmax[flip]) gsmax[flip] = x;
}


static void flipflop() {
  flip = 1-flip;
  gsngrp[flip] = 0;
  gsmax[flip] = 0;
  if (gsalloc[flip] < gsalloc[1-flip]) growstack((uint64_t)(gsalloc[1-flip])*2);
}

#ifdef TIMING_ON
  // many calls to clock() can be expensive, hence compiled out rather than switch(verbose)
  #include <time.h>
  #define NBLOCK 20
  static clock_t tblock[NBLOCK], tstart;
  static int nblock[NBLOCK];
  #define TBEG() tstart = clock();
  #define TEND(i) tblock[i] += clock()-tstart; nblock[i]++; tstart = clock();
#else
  #define TBEG()
  #define TEND(i)
#endif


/*
   icount originally copied from do_radixsort in src/main/sort.c @ rev 51389. Then reworked here again in forder.c in v1.8.11
   base::sort.list(method="radix") turns out not to be a radix sort, but a counting sort, and we like it.
   See r-devel post: http://r.789695.n4.nabble.com/method-radix-in-sort-list-isn-t-actually-a-radix-sort-tp3309470p3309470.html
   Main changes :
   1. Negatives are fine. Btw, wish raised for simple change to base R : https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=15644
   2. Doesn't cummulate through 0's for speed in repeated calls of sparse counts by saving memset back to 0 of many 0
   3. Separated setRange so forder can redirect to iradix
*/

static void range_i32(int32_t *x, int n, int64_t *out_min, int64_t *out_max, bool *out_anyna)  // out_* in common type
{
  int32_t min = NA_INTEGER;
  int32_t max = NA_INTEGER;
  bool anyna=false;
  int i=0;
  while(i<n && x[i]==NA_INTEGER) i++;
  if (i>0) anyna = true;
  if (i<n) max = min = x[i++];
  for(; i<n; i++) {
    int tmp = x[i];
    if (tmp>max) max=tmp;
    else if (tmp<min) {
      if (tmp==NA_INTEGER) anyna=true;
      else min=tmp;
    }
  }
  *out_anyna = anyna;
  if(min==NA_INTEGER) {
    *out_min = INT64_MIN;
    *out_max = INT64_MIN;
  } else {
    *out_min = min;
    *out_max = max;
  }
}

static void range_i64(int64_t *x, int n, int64_t *out_min, int64_t *out_max, bool *out_anyna)
{
  int64_t min = INT64_MIN;
  int64_t max = INT64_MIN;
  bool anyna=false;
  int i=0;
  while(i<n && x[i]==INT64_MIN) i++;
  if (i>0) anyna=true;
  if (i<n) max = min = x[i++];
  for(; i<n; i++) {
    int64_t tmp = x[i];
    if (tmp>max) max=tmp;
    else if (tmp<min) {
      if (tmp==INT64_MIN) anyna=true;
      else min=tmp;
    }
  }
  *out_anyna = anyna;
  *out_min = min;
  *out_max = max;
}

static void range_str(SEXP *x, int n, int64_t *out_min, int64_t *out_max, bool *out_anyna)
// just for 1st column as wouldn't retain the first-appearance order if column 2+.  Yes it will!, since all numbers will retain first appearance order.
// for by= not keyby=
// group numbers are left in truelength to be reused by part II
{
  bool anyna=false;
  int ngroup=0;
  if (ustr_n!=0) Error("Internal error: ustr isn't empty when starting range_str: ustr_n=%d, ustr_alloc=%d", ustr_n, ustr_alloc);
  // savetl_init() is called once at the start of forder
  for(int i=0; i<n; i++) {
    SEXP s = x[i];
    if (s==NA_STRING) {anyna=true; continue;}
    if (TRUELENGTH(s)<0) {
      // group number is in TRUELENGTH; seen this group before; this case most frequent so do first
      continue;
    }
    if (TRUELENGTH(s)>0)   // save any of R's own usage of tl (assumed positive, so we can both count and save in one scan), to restore
      savetl(s);           // afterwards. From R 2.14.0, tl is initialized to 0, prior to that it was random so this step saved too much.
    // now save unique SEXP separately, so we can loop through them afterwards and reset TRUELENGTH to 0.
    if (ustr_alloc<=ustr_n) {
      ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;  // 10000 = 78k of 8byte pointers. Small initial guess, negligible time to alloc.
      if (ustr_alloc>n) ustr_alloc = n;  // clamps at n (fully unique, no dups); n passed in
      ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
      if (ustr == NULL) Error("Unable to realloc %d * %d bytes in cgroup", ustr_alloc, sizeof(SEXP));
    }
    SET_TRUELENGTH(s, -(++ngroup));
    ustr[ustr_n++] = s;
  }
  *out_anyna = anyna;
  if (ngroup==0) {
    *out_min = INT64_MIN;
    *out_max = INT64_MIN;
  } else {
    *out_min = 1;
    *out_max = ngroup;
  }
}


static void count_group(const uint8_t *x, const int n, bool *out_grouped, int *out_order, int *out_ngrp, int *out_grpsize)
// orders x by reference (that's ok destructively as we don't use this radix again) and writes the order into o; the o ordering is used several times by the caller.
// the uint8_t *o limits the capability to max n=256 items (4 cache lines * 2 = 8 cache lines)
// caller then uses x to detect groups and push those in the ordered clause.
// don't be tempted to binsearch backwards here, have to shift anyway; many memmove would have overhead and do the same thing.

// The only output need is the ordering, which is only used if not grouped.
// with no actual histogram (or needing to zero it), this might work always!, not just for n<200
// theres no need for all the writes of moving xtmp and otmp that insert sort does

{
  //uint8_t wgrp[256] = {0};  // which group
  uint8_t ugrp[256];  // uninitialized is fine.  unique groups; avoids sweeping all 256 counts when very often very few of them are used (avoids degrade of counting sort), as well as maintaining first-appearance order
  int counts[256] = {0};  // ** We can try and see if this is fast, because it's on stack it might be.  If not, then pass in my_counts as we need thread local static.
                          // TODO TODO: these should not be on stack since they need to be initialized, and we don't need that initialization

  // first item is always first of first group
  ugrp[0] = x[0];
  counts[x[0]] = 1;
  int ngrp = 1;             // number of groups
  bool grouped = true;

  for (int i=1; i<n; i++) {
    uint8_t this = x[i];
    if (counts[this]==0) {
      // have not seen this value before
      ugrp[ngrp++]=this;
    } else if (grouped && this!=x[i-1]) {
      // seen this value before and it isn't the previous value, so data is not grouped
      grouped=false;
    }
    counts[this]++;
  }

  // TODO: if we need to sort the groups,  then just insert sort the uniques, using in-place insert sort.  But then out_grouped would need to be out_need_to_reorder

  *out_grouped = grouped;
  *out_ngrp = ngrp;
  for (int i=0; i<ngrp; i++) { uint8_t w=ugrp[i]; out_grpsize[i]=counts[w]; }  // no need to sweep == in caller to find groups; we already have sizes here
  if (!grouped) {
    // this is where the potentially random access comes in but only to at-most 256 points, each contigously
    for (int i=0, sum=0; i<ngrp; i++) { uint8_t w=ugrp[i]; int tmp=counts[w]; counts[w]=sum; sum+=tmp; }  // prepare for forwards-assign to help cpu prefetch on next line
    for (int i=0; i<n; i++) { uint8_t this=x[i]; out_order[counts[this]++] = i; }  // it's this second sweep through x[] that benefits from x[] being in cache from last time above, so keep it small.
  }
  for (int i=0; i<ngrp; i++) counts[ugrp[i]] = 0;  // ready for next time to save initializing the 256 vector
}



// x*order results in integer overflow when -1*NA, so careful to avoid that here :
static inline int icheck(int x) {
  return ((nalast != 1) ? ((x != NA_INTEGER) ? x*order : x) : ((x != NA_INTEGER) ? (x*order)-1 : INT_MAX)); // if nalast==1, NAs must go last.
}


//static void icount(int *x, int *o, int n)
/* Counting sort:
  1. Places the ordering into o directly, overwriting whatever was there
  2. Doesn't change x
  3. Pushes group sizes onto stack
*/
/*{
  int i=0, tmp;
  int napos = range;  // always count NA in last bucket and we'll account for nalast option in due course
  static unsigned int counts[N_RANGE+1] = {0};                            // static is IMPORTANT, counting sort is called repetitively.
  // counts are set back to 0 at the end efficiently. 1e5 = 0.4MB i.e
  // tiny. We'll only use the front part of it, as large as range. So it's
  // just reserving space, not using it. Have defined N_RANGE to be 100000.
  if (range > N_RANGE) Error("Internal error: range = %d; isorted can't handle range > %d", range, N_RANGE);
  for(i=0; i<n; i++) {
    if (x[i] == NA_INTEGER) counts[napos]++;             // For nalast=NA case, we won't remove/skip NAs, rather set 'o' indices
    else counts[x[i]-xmin]++;                            // to 0. subset will skip them. We can't know how many NAs to skip
  }                                                        // beforehand - i.e. while allocating "ans" vector
  // TO DO: at this point if the last count==n then it's all the same number and we can stop now.
  // Idea from Terdiman, then improved on that by not needing to loop through counts.

  tmp = 0;
  if (nalast!=1 && counts[napos]) {
    push(counts[napos]);
    tmp += counts[napos];
  }
  int w = (order==1) ? 0 : range-1;                                   // *** BLOCK 4 ***
  for (i=0; i<range; i++)
  // no point in adding tmp<n && i<=range, since range includes max,
  //   need to go to max, unlike 256 loops elsewhere in forder.c
  {
    if (counts[w]) {                                                    // cumulate but not through 0's. Helps resetting zeros when n<range, below.
      push(counts[w]);
      counts[w] = (tmp += counts[w]);
    }
    w += order; // order is +1 or -1
  }
  if (nalast==1 && counts[napos]) {
    push(counts[napos]);
    counts[napos] = (tmp += counts[napos]);
  }
  for(i=n-1; i>=0; i--) {
    o[--counts[(x[i] == NA_INTEGER) ? napos : x[i]-xmin]] = (int)(i+1);    // This way na.last=TRUE/FALSE cases will have just a single if-check overhead.
  }
  if (nalast == 0)                                                        // nalast = 1, -1 are both taken care already.
    for (i=0; i<n; i++) o[i] = (x[o[i]-1] == NA_INTEGER) ? 0 : o[i];    // nalast = 0 is dealt with separately as it just sets o to 0
                                      // at those indices where x is NA. x[o[i]-1] because x is not modifed here.

  // counts were cumulated above so leaves non zero.
  // Faster to clear up now ready for next time.
  if (n < range) {
    // Many zeros in counts already. Loop through n instead,
    // doesn't matter if we set to 0 several times on any repeats
    counts[napos]=0;
    for (i=0; i<n; i++) {
      if (x[i]!=NA_INTEGER) counts[x[i]-xmin]=0;
    }
  } else {
    memset(counts, 0, (range+1)*sizeof(int));                           // *** BLOCK 6 ***
  }
  return;
}
*/






static void insert(uint8_t *x, int n, uint8_t *o, bool *ordered)
// orders x by reference (that's ok destructively as we don't use this radix again) and writes the order into o; the o ordering is used several times by the caller.
// the uint8_t *o limits the capability to max n=256 items (4 cache lines * 2 = 8 cache lines)
// caller then uses x to detect groups and push those in the ordered clause.
// don't be tempted to binsearch backwards here, have to shift anyway; many memmove would have overhead and do the same thing.
{
  bool anymoved = false;
  o[0]=0;
  for (int i=1; i<n; i++) {
    o[i]=i;  // initialize with 0:(n-1)
    uint8_t xtmp = x[i];
    if (xtmp < x[i-1]) {
      anymoved = true;
      int j = i-1;
      uint8_t otmp = i;
      while (j>=0 && xtmp < x[j]) {
        x[j+1] = x[j];
        o[j+1] = o[j];
        j--;
      }
      x[j+1] = xtmp;
      o[j+1] = otmp;
    }
  }
  *ordered = !anymoved;
}

/*
  iradix is a counting sort performed forwards from MSB to LSB, with some tricks
  and short circuits building on Terdiman and Herf.
  http://codercorner.com/RadixSortRevisited.htm
  http://stereopsis.com/radix.html

  ~ Note they are LSD, but we do MSD here which is more complicated, for efficiency.
  ~ NAs need no special treatment as NA is the most negative integer in R (checked in init.c once,
    for efficiency) so NA naturally sort to the front.
  ~ Using 4-pass 1-byte radix for the following reasons :
    *  11-bit (Herf) reduces to 3-passes (3*11=33) yes, and LSD need random access
       to o vector in each pass 1:n so reduction in passes
    *  is good, but ...
    *  ... Terdiman's idea to skip a radix if all values are equal occurs less the wider the
         radix. A narrower radix benefits more from that.
    *      That's detected here using a single 'if', an improvement on Terdiman's exposition
         of a single loop to find if any count==n
    *  The pass through counts bites when radix is wider, because we repetitively call this
       iradix from fastorder forwards.
    *  Herf's parallel histogramming is neat. In 4-pass 1-byte it needs 4*256 storage, that's
       tiny, and can be static. 4*256 << 3*2048
    *  4-pass 1-byte is simpler and tighter code than 3-pass 11-bit, giving modern optimizers
       and modern CPUs a better chance. We may get
    *  lucky anyway, if one or two of the 4-passes are skipped.

   Recall: there are no comparisons at all in counting and radix, there is wide random access
   in each LSD radix pass, though.
*/

static unsigned int radixcounts[8][257] = {{0}};                            // 4 are used for iradix, 8 for dradix and i64radix
static int skip[8];
/* global because iradix and iradix_r interact and are called repetitively.
   counts are set back to 0 after each use, to benefit from skipped radix. */

static void alloc_otmp(int n) {
  if (otmp_alloc >= n) return;
  otmp = (int *)realloc(otmp, n * sizeof(int));
  if (otmp == NULL) Error("Failed to allocate working memory for otmp. Requested %d * %d bytes", n, sizeof(int));
  otmp_alloc = n;
}

static void alloc_xtmp(int n) {                                             // TO DO: currently always the largest type (double) but
                                      // could be int if that's all that's needed
  if (xtmp_alloc >= n) return;
  xtmp = (double *)realloc(xtmp, n * sizeof(double));
  if (xtmp == NULL) Error("Failed to allocate working memory for xtmp. Requested %d * %d bytes", n, sizeof(double));
  xtmp_alloc = n;
}

static void iradix_r(int *xsub, int *osub, int n, int radix);

static void iradix(int *x, int *o, int n)
   /* As icount :
     Places the ordering into o directly, overwriting whatever was there
     Doesn't change x
     Pushes group sizes onto stack */
{
  int i, j, radix, nextradix, itmp, thisgrpn, maxgrpn;
  unsigned int thisx=0, shift, *thiscounts;

  for (i=0;i<n;i++) {
    /* parallel histogramming pass; i.e. count occurrences of
    0:255 in each byte.  Sequential so almost negligible. */
    thisx = (unsigned int)(icheck(x[i])) - INT_MIN;                     // relies on overflow behaviour. And shouldn't -INT_MIN be up in iradix?
    radixcounts[0][thisx & 0xFF]++;                                     // unrolled since inside n-loop
    radixcounts[1][thisx >> 8 & 0xFF]++;
    radixcounts[2][thisx >> 16 & 0xFF]++;
    radixcounts[3][thisx >> 24 & 0xFF]++;
  }
  for (radix=0; radix<4; radix++) {
    /* any(count == n) => all radix must have been that value =>
    last x (still thisx) was that value */
    i = thisx >> (radix*8) & 0xFF;
    skip[radix] = radixcounts[radix][i] == n;
    if (skip[radix]) radixcounts[radix][i] = 0;                         // clear it now, the other counts must be 0 already
  }

  radix = 3;  // MSD
  while (radix>=0 && skip[radix]) radix--;
  if (radix==-1) {                                                        // All radix are skipped; i.e. one number repeated n times.
    if (nalast == 0 && x[0] == NA_INTEGER)                              // all values are identical. return 0 if nalast=0 & all NA
      for (i=0; i<n; i++) o[i] = 0;                                   // because of 'return', have to take care of it here.
    else for (i=0; i<n; i++) o[i] = (i+1);
    push(n);
    return;
  }
  for (i=radix-1; i>=0; i--) {
    if (!skip[i]) memset(radixcounts[i], 0, 257*sizeof(unsigned int));
    /* clear the counts as we only needed the parallel pass for skip[]
       and we're going to use radixcounts again below. Can't use parallel
       lower counts in MSD radix, unlike LSD. */
  }
  thiscounts = radixcounts[radix];
  shift = radix * 8;

  itmp = thiscounts[0];
  maxgrpn = itmp;
  for (i=1; itmp<n && i<256; i++) {
    thisgrpn = thiscounts[i];
    if (thisgrpn) {                                                     // don't cummulate through 0s, important below.
      if (thisgrpn>maxgrpn) maxgrpn = thisgrpn;
      thiscounts[i] = (itmp += thisgrpn);
    }
  }
  for (i=n-1; i>=0; i--) {
    thisx = ((unsigned int)(icheck(x[i])) - INT_MIN) >> shift & 0xFF;
    o[--thiscounts[thisx]] = i+1;
  }

  if (radix_xsub_alloc < maxgrpn) {
    // The largest group according to the first non-skipped radix, so could be big (if radix is needed on first column)
    // TO DO: could include extra bits to divide the first radix up more. Often the MSD has groups in just 0-4 out of 256.
    // free'd at the end of forder once we're done calling iradix repetitively
    radix_xsub = (int *)realloc(radix_xsub, maxgrpn*sizeof(double));    // realloc(NULL) == malloc
    if (!radix_xsub) Error("Failed to realloc working memory %d*8bytes (xsub in iradix), radix=%d", maxgrpn, radix);
    radix_xsub_alloc = maxgrpn;
  }

  alloc_otmp(maxgrpn);   // TO DO: can we leave this to forder and remove these calls??
  alloc_xtmp(maxgrpn);   // TO DO: doesn't need to be sizeof(double) always, see inside

  nextradix = radix-1;
  while (nextradix>=0 && skip[nextradix]) nextradix--;
  if (thiscounts[0] != 0) Error("Internal error. thiscounts[0]=%d but should have been decremented to 0. dradix=%d", thiscounts[0], radix);
  thiscounts[256] = n;
  itmp = 0;
  for (i=1; itmp<n && i<=256; i++) {
    if (thiscounts[i] == 0) continue;
    thisgrpn = thiscounts[i] - itmp;                                    // undo cumulate; i.e. diff
    if (thisgrpn == 1 || nextradix==-1) {
      push(thisgrpn);
    } else {
      for (j=0; j<thisgrpn; j++)
        ((int *)radix_xsub)[j] = icheck(x[o[itmp+j]-1]);            // this is why this xsub here can't be the same memory as xsub in forder.
      iradix_r(radix_xsub, o+itmp, thisgrpn, nextradix);              // changes xsub and o by reference recursively.
    }
    itmp = thiscounts[i];
    thiscounts[i] = 0;
  }
  if (nalast == 0)                                                        // nalast = 1, -1 are both taken care already.
    for (i=0; i<n; i++) o[i] = (x[o[i]-1] == NA_INTEGER) ? 0 : o[i];    // nalast = 0 is dealt with separately as it just sets o to 0
                                      // at those indices where x is NA. x[o[i]-1] because x is not
                                      // modified by reference unlike iinsert or iradix_r
}

static void iradix_r(int *xsub, int *osub, int n, int radix)
  // xsub is a recursive offset into xsub working memory above in iradix, reordered by reference.
  // osub is a an offset into the main answer o, reordered by reference.
  // radix iterates 3,2,1,0
{
  int i, j, itmp, thisx, thisgrpn, nextradix, shift;
  unsigned int *thiscounts;

  if (n < N_SMALL) {          // N_SMALL=200 is guess based on limited testing. Needs calibrate().
                              // Was 50 based on sum(1:50)=1275 worst -vs- 256 cummulate + 256 memset + allowance since reverse order is unlikely.
 //   iinsert(xsub, osub, n);   // when nalast==0, iinsert will be called only from within iradix.
    return;
  }

  shift = radix*8;
  thiscounts = radixcounts[radix];

  for (i=0; i<n; i++) {
    thisx = (unsigned int)xsub[i] - INT_MIN;                                // sequential in xsub
    thiscounts[thisx >> shift & 0xFF]++;
  }
  itmp = thiscounts[0];
  for (i=1; itmp<n && i<256; i++)
    if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);             // don't cummulate through 0s, important below
  for (i=n-1; i>=0; i--) {
    thisx = ((unsigned int)xsub[i] - INT_MIN) >> shift & 0xFF;
    j = --thiscounts[thisx];
    otmp[j] = osub[i];
    ((int *)xtmp)[j] = xsub[i];
  }
  memcpy(osub, otmp, n*sizeof(int));
  memcpy(xsub, xtmp, n*sizeof(int));

  nextradix = radix-1;
  while (nextradix>=0 && skip[nextradix]) nextradix--;
  // TO DO:  If nextradix==-1 AND no further columns from forder AND !retGrp, we're
  //     done. We have o. Remember to memset thiscounts before returning.

  if (thiscounts[0] != 0) Error("Logical error. thiscounts[0]=%d but should have been decremented to 0. radix=%d", thiscounts[0], radix);
  thiscounts[256] = n;
  itmp = 0;
  for (i=1; itmp<n && i<=256; i++) {
    if (thiscounts[i] == 0) continue;
    thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
    if (thisgrpn == 1 || nextradix==-1) {
      push(thisgrpn);
    } else {
      iradix_r(xsub+itmp, osub+itmp, thisgrpn, nextradix);
    }
    itmp = thiscounts[i];
    thiscounts[i] = 0;
  }
}


// dradix from Arun's fastradixdouble.c
// + changed to MSD and hooked into forder framework here.
// + replaced tolerance with rounding s.f.

// No rounding by default, for now. Handles #1642, #1728, #1463, #485
static int dround = 0;
static unsigned long long dmask1;
static unsigned long long dmask2;

SEXP setNumericRounding(SEXP droundArg)
// init.c has initial call with default of 2
{
  if (!isInteger(droundArg) || LENGTH(droundArg)!=1) error("Must an integer or numeric vector length 1");
  if (INTEGER(droundArg)[0] < 0 || INTEGER(droundArg)[0] > 2) error("Must be 2, 1 or 0");
  dround = INTEGER(droundArg)[0];
  dmask1 = dround ? 1 << (8*dround-1) : 0;
  dmask2 = 0xffffffffffffffff << dround*8;
  return R_NilValue;
}

SEXP getNumericRounding()
{
  return ScalarInteger(dround);
}

int getNumericRounding_C()
// for use in uniqlist.c
{
  return dround;
}

static union {
  double d;
  unsigned long long ull;
  //  int i;
  //  unsigned int ui;
} u;

unsigned long long dtwiddle(void *p, int i, int order)
{
  u.d = order*((double *)p)[i];                               // take care of 'order' right at the beginning
  if (R_FINITE(u.d)) {
    u.ull = (u.d) ? u.ull + ((u.ull & dmask1) << 1) : 0;    // handle 0, -0 case. Fix for issues/743.
                                                            // tested on vector length 100e6. was the fastest fix (see results at the bottom of page)
  } else if (ISNAN(u.d)) {
   /* 1. NA twiddled to all bits 0, sorts first.  R's value 1954 cleared.
      2. NaN twiddled to set just bit 13, sorts immediately after NA. 13th bit to be
         consistent with "quiet" na bit but any bit outside last 2 bytes would do.
         (ref r-devel post: http://r.789695.n4.nabble.com/Question-re-NA-NaNs-in-R-td4685014.html)
      3. This also normalises a difference between NA on 32bit R (bit 13 set) and 64bit R (bit 13 not set)
      4. -Inf twiddled to : 0 sign, exponent all 0, mantissa all 1, sorts after NaN
      5. +Inf twiddled to : 1 sign, exponent all 1, mantissa all 0, sorts last since finite
         numbers are defined by not-all-1 in exponent */
      u.ull = (ISNA(u.d) ? 0 : (1ULL << 51));
      return (nalast == 1 ? ~u.ull : u.ull);
  }
  unsigned long long mask = (u.ull & 0x8000000000000000) ?
           0xffffffffffffffff : 0x8000000000000000;       // always flip sign bit and if negative (sign bit was set) flip other bits too
  return( (u.ull ^ mask) & dmask2 );
}

unsigned long long i64twiddle(void *p, int i, int order)
// 'order' is in effect now - ascending and descending order implemented. Default
// case (setkey) will not be affected much because nalast != 1 and order == 1 are
// defaults.
{
  u.d = ((double *)p)[i];
  u.ull ^= 0x8000000000000000;
  if (nalast != 1) {
    if (order != 1 && u.ull != 0) u.ull ^= 0xffffffffffffffff;
  } else {
    if ( (order == 1 && u.ull==0) || (order != 1) )
      u.ull ^= 0xffffffffffffffff;
  }
  // another way - equivalent as above.
  // if (order == 1) {// u.ull = 0 now means NA
  //     if (nalast == 1 && u.ull == 0) u.ull ^= 0xffffffffffffffff;
  // } else {
  //     if (!(nalast != 1 && u.ull == 0)) u.ull ^= 0xffffffffffffffff;
  // }
  return u.ull;
}

Rboolean dnan(void *p, int i) {
  u.d = ((double *)p)[i];
  return (ISNAN(u.d));
}

Rboolean i64nan(void *p, int i) {
  u.d = ((double *)p)[i];
  return ((u.ull ^ 0x8000000000000000) == 0);
}

/*
unsigned long long i32twiddle(void *p, int i)
{
  u.i = ((int *)p)[i];  // a cast of int to long would introduce 31 redundant bits after the sign bit (better to be packed together in 32)
  unsigned int mask = -(int)(u.ui >> 31) | 0x80000000;
  u.ui ^= mask;
  return( u.ull );
}
*/

unsigned long long (*twiddle)(void *, int, int);
// integer64 has NA = 0x8000000000000000. And it gives TRUE for all ISNAN(.) when '.' is -ve number.
// So, ISNAN(.) would just provide wrong results. This was particularly an issue while implementing
// DT[order(., na.last=NA)] where '.' is an integer64 column. Therefore, 'is_nan'. This is basically
// ISNAN(.) for double and (u.ull ^ 0x8000000000000000 == 0) for integer64.
Rboolean (*is_nan)(void *, int);
size_t colSize=8;  // the size of the column type (4 or 8). Just 8 currently until iradix is merged in.

static void dradix_r(uint8_t *xsub, int *osub, int n, int radix);

#ifdef WORDS_BIGENDIAN
#define RADIX_BYTE colSize-radix-1
#else
#define RADIX_BYTE radix
#endif

static void dradix(uint8_t *x, int *o, int n)
{
  int i, j, radix, nextradix, itmp, thisgrpn, maxgrpn;
  unsigned int *thiscounts;
  unsigned long long thisx=0;
  // see comments in iradix for structure.  This follows the same. TO DO: merge iradix in here (almost ready)
  for (i=0;i<n;i++) {
    thisx = twiddle(x,i,order);
    for (radix=0; radix<colSize; radix++)
      radixcounts[radix][((unsigned char *)&thisx)[RADIX_BYTE]]++;
    // if dround==2 then radix 0 and 1 will be all 0 here and skipped.
    /* on little endian, 0 is the least significant bits (the right)
    / and 7 is the most including sign (the left); i.e. reversed. */
  }
  for (radix=0; radix<colSize; radix++) {
    i = ((unsigned char *)&thisx)[RADIX_BYTE];        // thisx is the last x after loop above
    skip[radix] = radixcounts[radix][i] == n;
    if (skip[radix]) radixcounts[radix][i] = 0;       // clear it now, the other counts must be 0 already
  }
  radix = colSize-1;  // MSD
  while (radix>=0 && skip[radix]) radix--;
  if (radix==-1) {                                    // All radix are skipped; i.e. one number repeated n times.
    if (nalast == 0 && is_nan(x, 0))                  // all values are identical. return 0 if nalast=0 & all NA
      for (i=0; i<n; i++) o[i] = 0;                   // because of 'return', have to take care of it here.
    else for (i=0; i<n; i++) o[i] = (i+1);
    push(n);
    return;
  }
  for (i=radix-1; i>=0; i--) {  // clear the lower radix counts, we only did them to know skip. will be reused within each group
    if (!skip[i]) memset(radixcounts[i], 0, 257*sizeof(unsigned int));
  }
  thiscounts = radixcounts[radix];
  itmp = thiscounts[0];
  maxgrpn = itmp;
  for (i=1; itmp<n && i<256; i++) {
    thisgrpn = thiscounts[i];
    if (thisgrpn) {  // don't cummulate through 0s, important below
      if (thisgrpn>maxgrpn) maxgrpn = thisgrpn;
      thiscounts[i] = (itmp += thisgrpn);
    }
  }
  for (i=n-1; i>=0; i--) {
    thisx = twiddle(x,i,order);
    o[ --thiscounts[((unsigned char *)&thisx)[RADIX_BYTE]] ] = i+1;
  }

  if (radix_xsub_alloc < maxgrpn) {                                            // TO DO: centralize this alloc
    // The largest group according to the first non-skipped radix, so could be big (if radix is needed on first column)
    // TO DO: could include extra bits to divide the first radix up more. Often the MSD has groups in just 0-4 out of 256.
    // free'd at the end of forder once we're done calling iradix repetitively
    radix_xsub = (double *)realloc(radix_xsub, maxgrpn*sizeof(double));  // realloc(NULL) == malloc
    if (!radix_xsub) Error("Failed to realloc working memory %d*8bytes (xsub in dradix), radix=%d", maxgrpn, radix);
    radix_xsub_alloc = maxgrpn;
  }

  alloc_otmp(maxgrpn);   // TO DO: leave to forder and remove these calls?
  alloc_xtmp(maxgrpn);

  nextradix = radix-1;
  while (nextradix>=0 && skip[nextradix]) nextradix--;
  if (thiscounts[0] != 0) Error("Logical error. thiscounts[0]=%d but should have been decremented to 0. dradix=%d", thiscounts[0], radix);
  thiscounts[256] = n;
  itmp = 0;
  for (i=1; itmp<n && i<=256; i++) {
    if (thiscounts[i] == 0) continue;
    thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
    if (thisgrpn == 1 || nextradix==-1) {
      push(thisgrpn);
    } else {
      if (colSize==4) { // ready for merging in iradix ...
        error("Not yet used, still using iradix instead");
        for (j=0; j<thisgrpn; j++) ((int *)radix_xsub)[j] = twiddle(x, o[itmp+j]-1, order);
      } else for (j=0; j<thisgrpn; j++) ((unsigned long long *)radix_xsub)[j] = twiddle(x, o[itmp+j]-1, order); // this is why this xsub here can't be the same memory as xsub in forder
      dradix_r(radix_xsub, o+itmp, thisgrpn, nextradix); // changes xsub and o by reference recursively.
    }
    itmp = thiscounts[i];
    thiscounts[i] = 0;
  }
  if (nalast == 0)                                             // nalast = 1, -1 are both taken care already.
    for (i=0; i<n; i++) o[i] = is_nan(x, o[i]-1) ? 0 : o[i];   // nalast = 0 is dealt with separately as it just sets o to 0
                                                               // at those indices where x is NA. x[o[i]-1] because x is not
                                                               // modified by reference unlike iinsert or iradix_r

}

static void dinsert(unsigned long long *x, int *o, int n)
// orders both x and o by reference in-place. Fast for small vectors, low overhead.
// don't be tempted to binsearch backwards here, have to shift anyway; many memmove would have overhead and do the same thing
// 'dinsert' will not be called when nalast = 0 and o[0] = -1.
{
  int i, j, otmp, tt;
  unsigned long long xtmp;
  for (i=1; i<n; i++) {
    xtmp = x[i];
    if (xtmp < x[i-1]) {
      j = i-1;
      otmp = o[i];
      while (j>=0 && xtmp < x[j]) { x[j+1] = x[j]; o[j+1] = o[j]; j--; }
      x[j+1] = xtmp;
      o[j+1] = otmp;
    }
  }
  tt = 0;
  for (i=1; i<n; i++) if (x[i]==x[i-1]) tt++; else { push(tt+1); tt=0; }
  push(tt+1);
}

static void dradix_r(unsigned char *xsub, int *osub, int n, int radix)
  /* xsub is a recursive offset into xsub working memory above in dradix, reordered by reference.
     osub is a an offset into the main answer o, reordered by reference.
     dradix iterates 7,6,5,4,3,2,1,0 */
{
  int i, j, itmp, thisgrpn, nextradix;
  unsigned int *thiscounts;
  unsigned char *p;
  if (n < 200) {
    /* 200 is guess based on limited testing. Needs calibrate(). Was 50
    based on sum(1:50)=1275 worst -vs- 256 cummulate + 256 memset +
    allowance since reverse order is unlikely */
    dinsert((void *)xsub, osub, n);                                         // order=1 here because it's already taken care of in iradix
    return;
  }
  thiscounts = radixcounts[radix];
  p = xsub + RADIX_BYTE;
  for (i=0; i<n; i++) {
    thiscounts[*p]++;
    p += colSize;
  }
  itmp = thiscounts[0];
  for (i=1; itmp<n && i<256; i++)
    if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);             // don't cummulate through 0s, important below
  p = xsub + (n-1)*colSize;
  if (colSize == 4) {
    error("Not yet used, still using iradix instead");
    for (i=n-1; i>=0; i--) {
      j = --thiscounts[*(p+RADIX_BYTE)];
      otmp[j] = osub[i];
      ((int *)xtmp)[j] = *(int *)p;
      p -= colSize;
    }
  } else {
    for (i=n-1; i>=0; i--) {
      j = --thiscounts[*(p+RADIX_BYTE)];
      otmp[j] = osub[i];
      ((unsigned long long *)xtmp)[j] = *(unsigned long long *)p;
      p -= colSize;
    }
  }
  memcpy(osub, otmp, n*sizeof(int));
  memcpy(xsub, xtmp, n*colSize);

  nextradix = radix-1;
  while (nextradix>=0 && skip[nextradix]) nextradix--;
  // TO DO:  If nextradix==-1 and no further columns from forder,  we're done. We have o. Remember to memset thiscounts before returning.

  if (thiscounts[0] != 0) Error("Logical error. thiscounts[0]=%d but should have been decremented to 0. radix=%d", thiscounts[0], radix);
  thiscounts[256] = n;
  itmp = 0;
  for (i=1; itmp<n && i<=256; i++) {
    if (thiscounts[i] == 0) continue;
    thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
    if (thisgrpn == 1 || nextradix==-1) {
      push(thisgrpn);
    } else {
      dradix_r(xsub + itmp*colSize, osub+itmp, thisgrpn, nextradix);
    }
    itmp = thiscounts[i];
    thiscounts[i] = 0;
  }
}

// TO DO?: dcount. Find step size, then range = (max-min)/step and proceed as icount. Many fixed precision floats (such as prices)
// may be suitable. Fixed precision such as 1.10, 1.15, 1.20, 1.25, 1.30 ... do use all bits so dradix skipping may not help.

int StrCmp2(SEXP x, SEXP y) {    // same as StrCmp but also takes into account 'na.last' argument.
  if (x == y) return 0;                   // same cached pointer (including NA_STRING==NA_STRING)
  if (x == NA_STRING) return nalast;      // if x=NA, nalast=1 ? then x > y else x < y (Note: nalast == 0 is already taken care of in 'csorted', won't be 0 here)
  if (y == NA_STRING) return -nalast;     // if y=NA, nalast=1 ? then y > x
  return order*strcmp(CHAR(ENC2UTF8(x)), CHAR(ENC2UTF8(y)));  // same as explanation in StrCmp
}

int StrCmp(SEXP x, SEXP y)            // also used by bmerge and chmatch
{
  if (x == y) return 0;             // same cached pointer (including NA_STRING==NA_STRING)
  if (x == NA_STRING) return -1;    // x<y
  if (y == NA_STRING) return 1;     // x>y
  return strcmp(CHAR(ENC2UTF8(x)), CHAR(ENC2UTF8(y))); // ENC2UTF8 handles encoding issues by converting all marked non-utf8 encodings alone to utf8 first. The function could be wrapped in the first if-statement already instead of at the last stage, but this is to ensure that all-ascii cases are handled with maximum efficiency.
  // This seems to fix the issues as far as I've checked. Will revisit if necessary.

  // OLD COMMENT: can return 0 here for the same string in known and unknown encodings, good if the unknown string is in that encoding but not if not ordering is ascii only (C locale). TO DO: revisit and allow user to change to strcoll, and take account of Encoding. see comments in bmerge().  10k calls of strcmp = 0.37s, 10k calls of strcoll = 4.7s. See ?Comparison, ?Encoding, Scollate in R internals.

}

// TO DO: check that all unknown encodings are ascii; i.e. no non-ascii unknowns are present, and that either Latin1
//        or UTF-8 is used by user, not both. Then error if not. If ok, then can proceed with byte level. ascii is never marked known by R, but non-ascii (i.e. knowable encoding) could be marked unknown.
//        does R internals have is_ascii function exported?  If not, simple enough.

static void cradix_r(SEXP *xsub, int n, int radix)
// xsub is a unique set of CHARSXP, to be ordered by reference
// First time, radix==0, and xsub==x. Then recursively moves SEXP together for L1 cache efficiency.
// Quite different to iradix because
//   1) x is known to be unique so fits in cache (wide random access not an issue)
//   2) they're variable length character strings
//   3) no need to maintain o.  Just simply reorder x. No grps or push.
// Fortunately, UTF sorts in the same order if treated as ASCII, so we can simplify by doing it by bytes.  TO DO: confirm
// a forwards (MSD) radix for efficiency, although more complicated
// This part has nothing to do with truelength. The truelength stuff is to do with finding the unique strings.
// We may be able to improve CHARSXP derefencing by submitting patch to R to make R's string cache contiguous
// but would likely be difficult. If we strxfrm, then it'll then be contiguous and compact then anyway.
{
  int i, j, itmp, *thiscounts, thisgrpn=0, thisx=0;
  SEXP stmp;

  // TO DO?: chmatch to existing sorted vector, then grow it.
  // TO DO?: if (n<N_SMALL=200) insert sort, then loop through groups via ==
  if (n<=1) return;
  if (n==2) {
    if (StrCmp(xsub[1],xsub[0])<0) { stmp=xsub[0]; xsub[0]=xsub[1]; xsub[1]=stmp; }
    return;
  }
  // TO DO: if (n<50) cinsert (continuing from radix offset into CHAR) or using StrCmp. But 256 is narrow, so quick and not too much an issue.

  thiscounts = cradix_counts + radix*256;
  for (i=0; i<n; i++) {
    thisx = xsub[i]==NA_STRING ? 0 : (radix<LENGTH(xsub[i]) ? (unsigned char)(CHAR(xsub[i])[radix]) : 1);
    thiscounts[ thisx ]++;   // 0 for NA,  1 for ""
  }
  if (thiscounts[thisx] == n && radix < maxlen-1) {   // this also catches when subx has shorter strings than the rest, thiscounts[0]==n and we'll recurse very quickly through to the overall maxlen with no 256 overhead each time
    cradix_r(xsub, n, radix+1);
    thiscounts[thisx] = 0;  // the rest must be 0 already, save the memset
    return;
  }
  itmp = thiscounts[0];
  for (i=1; i<256; i++)
    if (thiscounts[i]) thiscounts[i] = (itmp += thiscounts[i]);  // don't cummulate through 0s, important below
  for (i=n-1; i>=0; i--) {
    thisx = xsub[i]==NA_STRING ? 0 : (radix<LENGTH(xsub[i]) ? (unsigned char)(CHAR(xsub[i])[radix]) : 1);
    j = --thiscounts[thisx];
    cradix_xtmp[j] = xsub[i];
  }
  memcpy(xsub, cradix_xtmp, n*sizeof(SEXP));
  if (radix == maxlen-1) {
    memset(thiscounts, 0, 256*sizeof(int));
    return;
  }
  if (thiscounts[0] != 0) Error("Logical error. counts[0]=%d in cradix but should have been decremented to 0. radix=%d", thiscounts[0], radix);
  itmp = 0;
  for (i=1;i<256;i++) {
    if (thiscounts[i] == 0) continue;
    thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
    cradix_r(xsub+itmp, thisgrpn, radix+1);
    itmp = thiscounts[i];
    thiscounts[i] = 0;  // set to 0 now since we're here, saves memset afterwards. Important to clear! Also more portable for machines where 0 isn't all bits 0 (?!)
  }
  if (itmp<n-1) cradix_r(xsub+itmp, n-itmp, radix+1);  // final group
}





static void cgroup(SEXP *x, int *o, int n)
// As icount :
//   Places the ordering into o directly, overwriting whatever was there
//   Doesn't change x
//   Pushes group sizes onto stack
// Only run when sortStr==FALSE. Basically a counting sort, in first appearance order, directly.
// Since it doesn't sort the strings, the name is cgroup.
// there is no _pre for this.  ustr created and cleared each time.
{
  SEXP s;
  int i, k, cumsum;
  // savetl_init() is called once at the start of forder
  if (ustr_n != 0) Error("Internal error. ustr isn't empty when starting cgroup: ustr_n=%d, ustr_alloc=%d", ustr_n, ustr_alloc);
  for(i=0; i<n; i++) {
    s = x[i];
    if (TRUELENGTH(s)<0) {                   // this case first as it's the most frequent
      SET_TRUELENGTH(s, TRUELENGTH(s)-1);  // use negative counts so as to detect R's own (positive) usage of tl on CHARSXP
      continue;
    }
    if (TRUELENGTH(s)>0) {  // Save any of R's own usage of tl (assumed positive, so we can both count and save in one scan), to restore
      savetl(s);          // afterwards. From R 2.14.0, tl is initialized to 0, prior to that it was random so this step saved too much.
      SET_TRUELENGTH(s,0);
    }
    if (ustr_alloc<=ustr_n) {
      ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;  // 10000 = 78k of 8byte pointers. Small initial guess, negligible time to alloc.
      if (ustr_alloc>n) ustr_alloc = n;
      ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
      if (ustr == NULL) Error("Unable to realloc %d * %d bytes in cgroup", ustr_alloc, sizeof(SEXP));
    }
    SET_TRUELENGTH(s, -1);
    ustr[ustr_n++] = s;
  }
  // TO DO: the same string in different encodings will be considered different here. Sweep through ustr and merge counts where equal (sort needed therefore, unfortunately?, only if there are any marked encodings present)
  cumsum = 0;
  for(i=0; i<ustr_n; i++) {                                    // 0.000
    push(-TRUELENGTH(ustr[i]));
    SET_TRUELENGTH(ustr[i], cumsum += -TRUELENGTH(ustr[i]));
  }
  int *target = (o[0] != -1) ? newo : o;
  for(i=n-1; i>=0; i--) {
    s = x[i];                                            // 0.400 (page fetches on string cache)
    SET_TRUELENGTH(s, k = TRUELENGTH(s)-1);
    target[k] = i+1;                                     // 0.800 (random access to o)
  }
  for(i=0; i<ustr_n; i++) SET_TRUELENGTH(ustr[i],0);     // The cummulate meant counts are left non zero, so reset for next time (0.00s).
  ustr_n = 0;
}

static void alloc_csort_otmp(int n) {
  if (csort_otmp_alloc >= n) return;
  csort_otmp = (int *)realloc(csort_otmp, n * sizeof(int));
  if (csort_otmp == NULL) Error("Failed to allocate working memory for csort_otmp. Requested %d * %d bytes", n, sizeof(int));
  csort_otmp_alloc = n;
}

/*static void csort(SEXP *x, int *o, int n)
//   As icount :
//    Places the ordering into o directly, overwriting whatever was there
//    Doesn't change x
//    Pushes group sizes onto stack
//   Requires csort_pre() to have created and sorted ustr already
{
  int i;
  // can't use otmp, since iradix might be called here and that uses otmp (and xtmp).
  //   alloc_csort_otmp(n) is called from forder for either n=nrow if 1st column,
  // or n=maxgrpn if onwards columns
  for(i=0; i<n; i++) csort_otmp[i] = (x[i] == NA_STRING) ? NA_INTEGER : -TRUELENGTH(x[i]);
  if (nalast == 0 && n == 2) {                        // special case for nalast==0. n==1 is handled inside forder. at least 1 will be NA here
    if (o[0] == -1) for (i=0; i<n; i++) o[i] = i+1;    // else use o from caller directly (not 1st column)
    for (int i=0; i<n; i++) if (csort_otmp[i] == NA_INTEGER) o[i] = 0;
    push(1); push(1);
    return;
  }
  if (n < N_SMALL && nalast != 0) {                                    // TO DO: calibrate() N_SMALL=200
    if (o[0] == -1) for (i=0; i<n; i++) o[i] = i+1;    // else use o from caller directly (not 1st column)
    for (int i=0; i<n; i++) csort_otmp[i] = icheck(csort_otmp[i]);
    iinsert(csort_otmp, o, n);
  } else {
    setRange(csort_otmp, n);
    if (range == NA_INTEGER) Error("Internal error. csort's otmp contains all-NA");
    int *target = (o[0] != -1) ? newo : o;
    if (range <= N_RANGE) // && range<n)            // TO DO: calibrate(). radix was faster (9.2s "range<=10000" instead of 11.6s
      icount(csort_otmp, target, n);       // "range<=N_RANGE && range<n") for run(7) where range=N_RANGE n=10000000
    else iradix(csort_otmp, target, n);
  }
  // all i* push onto stack. Using their counts may be faster here than thrashing SEXP fetches over several passes as cgroup does
  // (but cgroup needs that to keep orginal order, and cgroup saves the sort in csort_pre).
}
*/
static void csort_pre(SEXP *x, int n)
// Finds ustr and sorts it.
// Runs once for each column (if sortStr==TRUE), then ustr is used by csort within each group
// ustr is grown on each character column, to save sorting the same strings again if several columns contain the same strings
{
  SEXP s;
  int i, old_un, new_un;
  // savetl_init() is called once at the start of forder
  old_un = ustr_n;
  for(i=0; i<n; i++) {
    s = x[i];
    if (TRUELENGTH(s)<0) continue;   // this case first as it's the most frequent. Already in ustr, this negative is its ordering.
    if (TRUELENGTH(s)>0) {  // Save any of R's own usage of tl (assumed positive, so we can both count and save in one scan), to restore
      savetl(s);          // afterwards. From R 2.14.0, tl is initialized to 0, prior to that it was random so this step saved too much.
      SET_TRUELENGTH(s,0);
    }
    if (ustr_alloc<=ustr_n) {
      ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;  // 10000 = 78k of 8byte pointers. Small initial guess, negligible time to alloc.
      if (ustr_alloc > old_un+n) ustr_alloc = old_un + n;
      ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
      if (ustr==NULL) Error("Failed to realloc ustr. Requested %d * %d bytes", ustr_alloc, sizeof(SEXP));
    }
    SET_TRUELENGTH(s, -1);  // this -1 will become its ordering later below
    ustr[ustr_n++] = s;
    if (s!=NA_STRING && LENGTH(s)>maxlen) maxlen=LENGTH(s);  // length on CHARSXP is the nchar of char * (excluding \0), and treats marked encodings as if ascii.
  }
  new_un = ustr_n;
  if (new_un == old_un) return;  // No new strings observed, seen them all before in previous column. ustr already sufficient.
  // If we ever make ustr permanently held by data.table, we'll just need to make the final loop to set -i-1 before returning here.
  // sort ustr.  TO DO: just sort new ones and merge them in.
  // These allocs are here, to save them being in the recursive cradix_r()
  if (cradix_counts_alloc < maxlen) {
    cradix_counts_alloc = maxlen + 10;   // +10 to save too many reallocs
    cradix_counts = (int *)realloc(cradix_counts, cradix_counts_alloc * 256 * sizeof(int) );  // stack of counts
    if (!cradix_counts) Error("Failed to alloc cradix_counts");
    memset(cradix_counts, 0, cradix_counts_alloc * 256 * sizeof(int));
  }
  if (cradix_xtmp_alloc < ustr_n) {
    cradix_xtmp = (SEXP *)realloc( cradix_xtmp,  ustr_n * sizeof(SEXP) );  // TO DO: Reuse the one we have in forder. Does it need to be n length?
    if (!cradix_xtmp) Error("Failed to alloc cradix_tmp");
    cradix_xtmp_alloc = ustr_n;
  }
  cradix_r(ustr, ustr_n, 0);  // sorts ustr in-place by reference
  for(i=0; i<ustr_n; i++)     // save ordering in the CHARSXP. negative so as to distinguish with R's own usage.
    SET_TRUELENGTH(ustr[i], -i-1);
}

// functions to test vectors for sortedness: isorted, dsorted and csorted
// base:is.unsorted uses any(is.na(x)) at R level (inefficient), and returns NA in the presence of any NA.
// Hence, here we deal with NAs in C and return true if NAs are all at the beginning (what we need in data.table).
// We also return -1 if x is sorted in _strictly_ reverse order; a common case we optimize in forder.
// If a vector is in decreasing order *with ties*, then an in-place reverse (no sort) would result in instability of ties (TO DO).
// For use by forder only, which now returns NULL if already sorted (hence no need for separate is.sorted).
// TO DO: test in big steps first to return faster if unsortedness is at the end (a common case of rbind'ing data to end)
// These are all sequential access to x, so very quick and cache efficient.

static int isorted(int *x, int n)                           // order = 1 is ascending and order=-1 is descending
{                                                           // also takes care of na.last argument with check through 'icheck'
                                                            // Relies on NA_INTEGER==INT_MIN, checked in init.c
  int i=1,j=0;
  if (nalast == 0) {                                        // when nalast = NA,
    for (int k=0; k<n; k++) if (x[k] != NA_INTEGER) j++;
    if (j == 0) { push(n); return(-2); }                    // all NAs ? return special value to replace all o's values with '0'
    if (j != n) return(0);                                  // any NAs ? return 0 = unsorted and leave it to sort routines to replace o's with 0's
  }                                                         // no NAs  ? continue to check the rest of isorted - the same routine as usual
  if (n<=1) { push(n); return(1); }
  if (icheck(x[1]) < icheck(x[0])) {
    i = 2;
    while (i<n && icheck(x[i]) < icheck(x[i-1])) i++;
    if (i==n) { mpush(1,n); return(-1);}                    // strictly opposite to expected 'order', no ties;
                                                            // e.g. no more than one NA at the beginning/end (for order=-1/1)
    else return(0);
  }
  int old = gsngrp[flip];
  int tt = 1;
  for (i=1; i<n; i++) {
    if (icheck(x[i]) < icheck(x[i-1])) { gsngrp[flip] = old; return(0); }
    if (x[i]==x[i-1]) tt++; else { push(tt); tt=1; }
  }
  push(tt);
  return(1);                                                // same as 'order', NAs at the beginning for order=1, at end for order=-1, possibly with ties
}

static int dsorted(double *x, int n)                        // order=1 is ascending and -1 is descending
{                                                           // also accounts for nalast=0 (=NA), =1 (TRUE), -1 (FALSE) (in twiddle)
  int i=1,j=0;
  unsigned long long prev, this;
  if (nalast == 0) {                                        // when nalast = NA,
    for (int k=0; k<n; k++) if (!is_nan(x, k)) j++;
    if (j == 0) { push(n); return(-2); }                    // all NAs ? return special value to replace all o's values with '0'
    if (j != n) return(0);                                  // any NAs ? return 0 = unsorted and leave it to sort routines to replace o's with 0's
  }                                                         // no NAs  ? continue to check the rest of isorted - the same routine as usual
  if (n<=1) { push(n); return(1); }
  prev = twiddle(x,0,order);
  this = twiddle(x,1,order);
  if (this < prev) {
    i = 2;
    prev=this;
    while (i<n && (this=twiddle(x,i,order)) < prev) {i++; prev=this; }
    if (i==n) { mpush(1,n); return(-1);}                    // strictly opposite of expected 'order', no ties;
                                                            // e.g. no more than one NA at the beginning/end (for order=-1/1)
    else return(0);                                         // TO DO: improve to be stable for ties in reverse
  }
  int old = gsngrp[flip];
  int tt = 1;
  for (i=1; i<n; i++) {
    this = twiddle(x,i,order);                              // TO DO: once we get past -Inf, NA and NaN at the bottom,  and +Inf at the top,
                                                            //        the middle only need be twiddled for tolerance (worth it?)
    if (this < prev) { gsngrp[flip] = old; return(0); }
    if (this==prev) tt++; else { push(tt); tt=1; }
    prev = this;
  }
  push(tt);
  return(1);                                                // exactly as expected in 'order' (1=increasing, -1=decreasing), possibly with ties
}

static int csorted(SEXP *x, int n)                          // order=1 is ascending and -1 is descending
{                                                           // also accounts for nalast=0 (=NA), =1 (TRUE), -1 (FALSE)
  int i=1, j=0, tmp;
  if (nalast == 0) {                                        // when nalast = NA,
    for (int k=0; k<n; k++) if (x[k] != NA_STRING) j++;
    if (j == 0) { push(n); return(-2); }                    // all NAs ? return special value to replace all o's values with '0'
    if (j != n) return(0);                                  // any NAs ? return 0 = unsorted and leave it to sort routines to replace o's with 0's
  }                                                         // no NAs  ? continue to check the rest of isorted - the same routine as usual
  if (n<=1) { push(n); return(1); }
  if (StrCmp2(x[1],x[0])<0) {
    i = 2;
    while (i<n && StrCmp2(x[i],x[i-1])<0) i++;
    if (i==n) { mpush(1,n); return(-1);}                    // strictly opposite of expected 'order', no ties;
                                                            // e.g. no more than one NA at the beginning/end (for order=-1/1)
    else return(0);
  }
  int old = gsngrp[flip];
  int tt = 1;
  for (i=1; i<n; i++) {
    tmp = StrCmp2(x[i],x[i-1]);
    if (tmp < 0) { gsngrp[flip] = old; return(0); }
    if (tmp == 0) tt++; else { push(tt); tt=1; }
  }
  push(tt);
  return(1);                                                // exactly as expected in 'order', possibly with ties
}
/*
static void isort(int *x, int *o, int n)
{
  if (n<=2) {
    if (nalast == 0 && n == 2) {                            // nalast = 0 and n == 2 (check bottom of this file for explanation)
      if (o[0]==-1) { o[0]=1; o[1]=2; }
      for (int i=0; i<n; i++) if (x[i] == NA_INTEGER) o[i] = 0;
      push(1); push(1);
      return;
    } else Error("Internal error: isort received n=%d. isorted should have dealt with this (e.g. as a reverse sorted vector) already",n);
  }
  if (n<N_SMALL && o[0] != -1 && nalast != 0) {                 // see comment above in iradix_r on N_SMALL=200.
    //if not o[0] then can't just populate with 1:n here, since x is changed by ref too (so would need to be copied).
    // pushes inside too. Changes x and o by reference, so not suitable  in first column when o hasn't been populated yet
    //   and x is the actual column in DT (hence check on o[0]).
    if (order != 1 || nalast != -1)                     // so that default case, i.e., order=1, nalast=FALSE will not be affected (ex: `setkey`)
      for (int i=0; i<n; i++) x[i] = icheck(x[i]);
    iinsert(x, o, n);
  } else {
    // Tighter range (e.g. copes better with a few abormally large values in some groups), but also, when setRange was once at
    // colum level that caused an extra scan of (long) x first. 10,000 calls to setRange takes just 0.04s i.e. negligible.
    setRange(x, n);
    if (range==NA_INTEGER) Error("Internal error: isort passed all-NA. isorted should have caught this before this point");
    int *target = (o[0] != -1) ? newo : o;
    if (range<=N_RANGE && range<=n) {                   // was range<10000 for subgroups, but 1e5 for the first column,
      icount(x, target, n);                             // tried to generalise here.  1e4 rather than 1e5 here because iterated
    } else {                                            // was (thisgrpn < 200 || range > 20000) then radix
      iradix(x, target, n);                             // a short vector with large range can bite icount when iterated (BLOCK 4 and 6)
    }
  }
  // TO DO: add calibrate() to init.c
}
*/

static void dsort(double *x, int *o, int n)
{
  if (n <= 2) {                                         // nalast = 0 and n == 2 (check bottom of this file for explanation)
    if (nalast == 0 && n == 2) {                        // don't have to twiddle here.. at least one will be NA and 'n' WILL BE 2.
      if (o[0]==-1) { o[0]=1; o[1]=2; }
      for (int i=0; i<n; i++) if (is_nan(x, i)) o[i] = 0;
      push(1); push(1);
      return;
    } Error("Internal error: dsort received n=%d. dsorted should have dealt with this (e.g. as a reverse sorted vector) already",n);
  }
  if (n<N_SMALL && o[0] != -1 && nalast != 0) {                                  // see comment above in iradix_r re N_SMALL=200,  and isort for o[0]
    for (int i=0; i<n; i++) ((unsigned long long *)x)[i] = twiddle(x,i,order);   // have to twiddle here anyways, can't speed up default case like in isort
    dinsert((unsigned long long *)x, o, n);
  } else {
    dradix((unsigned char *)x, (o[0] != -1) ? newo : o, n);
  }
}

bool need2utf8(SEXP x, int n)
{
  for (int i=0; i<n; i++) if (NEED2UTF8(STRING_ELT(x, i))) return(true);
  return(false);
}

SEXP forder(SEXP DT, SEXP by, SEXP retGrp, SEXP sortStrArg, SEXP orderArg, SEXP naArg)
// sortStr TRUE from setkey, FALSE from by=
{
  //int i, j, k, grp, ngrp, tmp, *osub, thisgrpn, n, col;
  //Rboolean isSorted = TRUE;
  //SEXP x, class;
  //void *xd;
  /*
#ifdef TIMING_ON
  memset(tblock, 0, NBLOCK*sizeof(clock_t));
  memset(nblock, 0, NBLOCK*sizeof(int));
  clock_t tstart;  // local variable to mask the global tstart for ease when timing sub funs
#endif
  TBEG()
  */
  int n_protect = 0;

  if (!isNewList(DT)) {
    if (!isVectorAtomic(DT)) error("Input is not either a list of columns, or an atomic vector.");
    if (!isNull(by)) error("Input is an atomic vector (not a list of columns) but by= is not NULL");
    if (!isInteger(orderArg) || LENGTH(orderArg)!=1) error("Input is an atomic vector (not a list of columns) but order= is not a length 1 integer");
    SEXP tt = PROTECT(allocVector(VECSXP, 1)); n_protect++;
    SET_VECTOR_ELT(tt, 0, DT);
    DT = tt;
    by = PROTECT(allocVector(INTSXP, 1)); n_protect++;
    INTEGER(by)[0] = 1;
  }

  if (!length(DT)) error("DT is an empty list() of 0 columns");
  if (!isInteger(by) || !LENGTH(by)) error("DT has %d columns but 'by' is either not integer or is length 0", length(DT));  // seq_along(x) at R level
  if (!isInteger(orderArg) || LENGTH(orderArg)!=LENGTH(by)) error("Either 'order' is not integer or its length (%d) is different to 'by's length (%d)", LENGTH(orderArg), LENGTH(by));
  int n = length(VECTOR_ELT(DT,0));
  for (int i=0; i<LENGTH(by); i++) {
    if (INTEGER(by)[i] < 1 || INTEGER(by)[i] > length(DT))
      error("'by' value %d out of range [1,%d]", INTEGER(by)[i], length(DT));
    if ( n != length(VECTOR_ELT(DT, INTEGER(by)[i]-1)) )
      error("Column %d is length %d which differs from length of column 1 (%d)\n", INTEGER(by)[i], length(VECTOR_ELT(DT, INTEGER(by)[i]-1)), n);
  }

  if (!isLogical(retGrp) || LENGTH(retGrp)!=1 || INTEGER(retGrp)[0]==NA_LOGICAL) error("retGrp must be TRUE or FALSE");
  if (!isLogical(sortStrArg) || LENGTH(sortStrArg)!=1 || INTEGER(sortStrArg)[0]==NA_LOGICAL ) error("sortStr must be TRUE or FALSE");
  if (!isLogical(naArg) || LENGTH(naArg) != 1) error("na.last must be logical TRUE, FALSE or NA of length 1");
  sortStr = LOGICAL(sortStrArg)[0];
  // static global set on top...
  nalast = (LOGICAL(naArg)[0] == NA_LOGICAL) ? 0 : (LOGICAL(naArg)[0] == TRUE) ? 1 : -1; // 1=TRUE, -1=FALSE, 0=NA
  gsmaxalloc = n;  // upper limit for stack size (all size 1 groups). We'll detect and avoid that limit, but if just one non-1 group (say 2), that can't be avoided.

  if (n==0) {
    // empty vector or 0-row DT is always sorted
    SEXP ans = PROTECT(allocVector(INTSXP, 0));
    if (LOGICAL(retGrp)[0]) {
      setAttrib(ans, sym_starts, allocVector(INTSXP, 0));
      setAttrib(ans, sym_maxgrpn, ScalarInteger(0));
    }
    UNPROTECT(1);
    return ans;
  }
  // if n==1, the code is left to proceed below in case one or more of the 1-row by= columns are NA and na.last=NA. Otherwise it would be easy to return now.



  savetl_init();   // from now on use Error not error.

  order = INTEGER(orderArg)[0];

  // always stack groups since we now go by byte and bytes_in_key is always > 1 ...stackgrps = length(by)>1 || LOGICAL(retGrp)[0];



//  int (*f)(); void (*g)();

/*
  if (length(by)>1 && gsngrp[flip]<n) {
    xsub = (void *)malloc(maxgrpn * sizeof(double));    // double is the largest type, 8
    if (xsub==NULL) Error("Couldn't allocate xsub in forder, requested %d * %d bytes.", maxgrpn, sizeof(double));
    newo = (int *)malloc(maxgrpn * sizeof(int));        // used by isort, dsort, sort and cgroup
    if (newo==NULL) Error("Couldn't allocate newo in forder, requested %d * %d bytes.", maxgrpn, sizeof(int));
  }
  TEND(1)  // should be negligible time to malloc even large blocks, but time it anyway to be sure
*/

  int ncol=length(by);
  uint8_t *key[ncol*8];  // needs to be before loop because part II relies on part I, column-by-column. ncol*8 is maximum
  for (int i=0; i<ncol*8; i++) key[i]=NULL;
  int nradix=0; // the current byte we're writing this column to; might be squashing into it (spare>0)
  int spare=0;  // the amount of bits remaining on the right of the current nradix byte
  for (int col=0; col<ncol; col++) {
    SEXP x = VECTOR_ELT(DT,INTEGER(by)[col]-1);
    int64_t min, max;     // min and max of non-NA values
    bool anyna;
    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP :  // TODO trivial LGL count
      range_i32(INTEGER(x), n, &min, &max, &anyna);
      break;
    case REALSXP :
      if (inherits(x, "integer64")) {
        range_i64((int64_t *)REAL(x), n, &min, &max, &anyna);
      } else {
        Error("double not yet implemented because it needs to twiddle");
        //range_d(  REAL(x), n, &min, &max, &anyna);
      }
      break;
    case STRSXP :
      if (need2utf8(x, n)) {
        SEXP tt = PROTECT(allocVector(STRSXP, n)); n_protect++;
        for (int i=0; i<n; i++) SET_STRING_ELT(tt, i, ENC2UTF8(STRING_ELT(x, i)));
        x = tt;
      }
      range_str(STRING_PTR(x), n, &min, &max, &anyna);
      break;
    default:
       Error("Column %d of 'by' (%d) is type '%s', not yet supported", col, INTEGER(by)[col-1], type2char(TYPEOF(x)));
    }

    if (min==INT64_MIN/*all na*/ || (min==max && !anyna)) continue;  // all same value; skip column as nothing to do
    uint64_t range = (max>=0 && min<0) ? (uint64_t)max+(uint64_t)(-min) : max-min;
    range++; // always provide for the NA spot (0 offset), even if NAs aren't present (for coding brevity and robustness).
             // TODO: could compress away NA
             // TODO: the NA value could be placed as max if na.last=TRUE.  Deal with na.last up front in the key and then forget (TODO)
    // range 5 means 0(NA),1,2,3,4,5  (6 values)
    int maxBit=0;
    while (range) { maxBit++; range>>=1; }
    int nbyte = 1+(maxBit-1)/8; // the number of bytes spanned by the value
    if (spare==0) {
      spare = (8-maxBit%8)%8;   // left align to byte boundary
    } else {
      spare -= maxBit%8;        // new spare is also how many bits to left shift
      if (spare<0) {
        nbyte++;                // after shift, will need an extra byte
        spare+=8;
      }
    }
    min--;  // so that x-min results in 1 for the minimum; 0 is for NA
    for (int b=0; b<nbyte; b++) {
      if (key[nradix+b]==NULL)
        key[nradix+b] = calloc(n, sizeof(uint8_t));  // 0 initialize so that NA's can just skip (NA is always the 0 offset)
    }
    // several columns could squash into 1 byte, too!  3 logical colums, for example.  TODO: add test.
    #define WRITE_KEY                                   \
    this-=min;                                          \
    this <<= spare;                                     \
    for (int b=nbyte-1; b>0; b--) {                     \
      key[nradix+b][i] = (uint8_t)(this & 0xff);    \
      this >>= 8;                                       \
    }                                                   \
    key[nradix][i] |= (uint8_t)(this & 0xff);
    // don't leave unused bits between columns; squash up the bits
    // if 1st column's range needs just 4 bits (16 values in column but we have 96 cores), left align too and take bits from 2nd column to get a better first split
    // aside: when align==0 (e.g. maxBit=8,16,24,32), the last key[nradix][i] |= will go against 0 so could have been =, but no worries
    // NB: retaining group order does not retain the appearance-order in the sense that DT[,,by=] does. Because we go column-by-column, the first column values
    //     are grouped together.   Whether we do byte-column-within-column doesn't alter this and neither does bit packing across column boundaries.
    //     if input data is grouped-by-column (not grouped-by-row) then this grouping strategy will help a lot.
    //     if input data is random ordering, then a consequence of grouping-by-column (whether sorted or not) is that the output ordering will not be by
    //       row-group-appearance-order built it; the result will always have to be resorted by row number of first group item.
    //     this was the case with the previous forder as well, which went by whole-column
    //     So, we may as well bit-pack as done above.
    //     Retaining apperance-order within key-byte-column is still advatangeous for when we do encounter column-grouped data (to avoid data movement); and
    //       the ordering will be appearance-order preserving in that case.

    switch(TYPEOF(x)) {
    case INTSXP : case LGLSXP : {  // TODO trivial LGL count
      int *xd = INTEGER(x);
      for (int i=0; i<n; i++) {
        int this = xd[i];
        if (this==NA_INTEGER) continue;   // TODO:  separate branchless when !anyna
        WRITE_KEY
      }}
      break;
    case REALSXP :
      if (inherits(x, "integer64")) {
        int64_t *xd = (int64_t *)REAL(x);
        for (int i=0; i<n; i++) {
          int64_t this = xd[i];
          if (this==INT64_MIN) continue;
          WRITE_KEY
        }
      } else {
        error("double not yet implemented");
        /*double *xd = REAL(x);
        for (int i=0; i<n; i++) {
          if (ISNA(xd[i])) continue;          // don't need if no-NA are known
          int64_t this = dtwiddle(xd[i]);  // TODO: don't need twiddle if no negatives or NA, known from range above
          WRITE_KEY
        }*/
      }
      break;
    case STRSXP : {
      // use tt from above if need2utf8.  TODO optional ascii mode for data.table
      SEXP *xd = STRING_PTR(x);
      for (int i=0; i<n; i++) {
        if (xd[i]==NA_STRING) continue;
        int this = -TRUELENGTH(xd[i]);
        WRITE_KEY
      }}
      for(int i=0; i<ustr_n; i++) SET_TRUELENGTH(ustr[i],0);
      free(ustr); ustr=NULL; ustr_n=0; ustr_alloc=0;   // ustr could be left allocated and reused, but free now in case large and we're tight on ram
      break;
    default:
       Error("Internal error: column not supported not caught earlier");
    }
    nradix += nbyte-1+(spare==0);
  }
  if (key[nradix]!=NULL) nradix++;  // nradix now number of bytes in key

  //for (int i=0; i<n; i++) {
  //  for (int b=0; b<nradix; b++) Rprintf("%03d  ", key[b][i]);
  //  Rprintf("\n");
  //}

  SEXP ans = PROTECT(allocVector(INTSXP, n)); n_protect++;
  int *o = INTEGER(ans);
  for (int i=0; i<n; i++) o[i]=i+1;  // initialize for smaller n or range where the parallel-batch doesn't happen. TODO: parallelize but low priority
                                     // the first gather is the worst; for the first radix==0 case, the parallel-batch can just write o directly to save wasteful reordering of 1:n

  //int maxgrpn = gsmax[flip];   // biggest group in the first column
  /*
  ngrp = gsngrp[flip];
  if (ngrp == n && nalast != 0) break;
  flipflop();

  order = INTEGER(orderArg)[col-1];
  */

  stackgrps = TRUE;
  push(n);
  omp_set_nested(1);
  for (int radix=0; radix<nradix; radix++) {
    flipflop();
    //int *grps = gs[flip];
    stackgrps = radix<(nradix-1) || LOGICAL(retGrp)[0];  // if just ordering, we can save allocating and writing the last (and biggest) group size vector
    int tmp=gs[1-flip][0]; for (int i=1; i<gsngrp[1-flip]; i++) tmp=(gs[1-flip][i]+=tmp);   // cummulate, so we can parallelise

    #pragma omp parallel num_threads(getDTthreads())
    {
      #define STL 131072  // Single Thread Load 128KB (4096*32   bytes=128KB, int=512KB).  TODO: might be too large
      int my_order[STL];
      int my_gs[STL];     // group sizes, if all size 1 then STL will be full
      int my_ng=0;        // number of groups; number of items used in my_gs
      int my_tmp[STL];    // used to reorder ans and each key[index]

      #pragma omp for ordered schedule(dynamic)
      for (int g=0; g<gsngrp[1-flip]; g++) {     // first time this will be just 1, which is fine
        int from = g==0 ? 0 : gs[1-flip][g-1];    // need grps cummulated
        int to = gs[1-flip][g];  // non-inclusive
        const int my_n = to-from;
        bool nested = false;
        const uint8_t *ksub = key[radix]+from;
        if (my_n>=100000) {
          // 256*nth*100 (so batch approach worth it)
          nested = true;
        } else {
          nested = false;
          bool grouped;
          count_group(ksub, my_n, &grouped, my_order, &my_ng, my_gs);
          if (!grouped) {
            // reorder o
            int *osub = o+from;
            if (radix==0) {
              // write the ordering directly (without going via my_tmp to reorder contents of o) because o just contains identity 1:n when radix==0
              for (int i=0; i<my_n; i++) osub[i] = from+my_order[i]+1;
            } else {
              for (int i=0; i<my_n; i++) my_tmp[i] = osub[my_order[i]];
              memcpy(osub, my_tmp, my_n*sizeof(int));
            }
            // reorder remaining key columns (radix+1 onwards)
            for (int i=radix+1; i<nradix; i++) {
              uint8_t *b = (uint8_t *)my_tmp;
              uint8_t *ksub = key[i]+from;
              for (int j=0; j<my_n; j++) *b++ = ksub[my_order[j]];
              memcpy(ksub, my_tmp, my_n*1);
            }
          }
        }

        #pragma omp ordered
        {
          if (nested) {
            // nested parallel and batch it in chunks

            // this may be the best place to write initial ordering into o[], since it's the widest random access.
          }
          newpush(my_gs, my_ng);
        }
      }
    }
  }

/*
#ifdef TIMING_ON
  for (i=0; i<NBLOCK; i++) {
    Rprintf("Timing block %d = %8.3f   %8d\n", i, 1.0*tblock[i]/CLOCKS_PER_SEC, nblock[i]);
    if (i==12) Rprintf("\n");
  }
  Rprintf("Found %d groups and maxgrpn=%d\n", gsngrp[flip], gsmax[flip]);
#endif

  if (!sortStr && ustr_n!=0) Error("Internal error: at the end of forder sortStr==FALSE but ustr_n!=0 [%d]", ustr_n);

  if (isSorted) {
    // the o vector created earlier could be avoided in this case if we only create it when isSorted becomes FALSE
    ans = PROTECT(allocVector(INTSXP, 0));  // Can't attach attributes to NULL
    n_protect++;
  }
  */
  if (LOGICAL(retGrp)[0]) {
    SEXP tt;
    setAttrib(ans, sym_starts, tt = allocVector(INTSXP, gsngrp[flip]));
    int *ss = INTEGER(tt);
    for (int i=0, tmp=1; i<gsngrp[flip]; i++) {ss[i]=tmp; tmp+=gs[flip][i]; };
    //if (isSorted || LOGICAL(sort)[0])
    //  for (INTEGER(x)[0]=1, i=1; i<ngrp; i++) INTEGER(x)[i] = INTEGER(x)[i-1] + gs[flip][i-1];
    //else {
      // it's not sorted already and we want to keep original group order
    //    cumsum = 0;
    //    for (i=0; i<ngrp; i++) { INTEGER(x)[i] = o[i+cumsum]; cumsum+=gs[flip][i]; }
    //    isort(INTEGER(x), ngrp);
    //}
    //setAttrib(ans, sym_maxgrpn, ScalarInteger(gsmax[flip]));
  }

  cleanup();
  UNPROTECT(n_protect);
  return ans;
}

// TODO: implement 'order' argument to 'fsorted'
// Not touching 'fsorted' for now for "decreasing order". Passing '1' as the value of 'order' argument (checks only ascending order as before).
SEXP fsorted(SEXP x)
{
  // Just checks if ordered and returns FALSE early if not (and don't return ordering if so, unlike forder).
  int tmp,n;
  void *xd;
  n = length(x);
  if (n <= 1) return(ScalarLogical(TRUE));
  if (!isVectorAtomic(x)) Error("is.sorted (R level) and fsorted (C level) only to be used on vectors. If needed on a list/data.table, you'll need the order anyway if not sorted, so use if (length(o<-forder(...))) for efficiency in one step, or equivalent at C level");
  xd = DATAPTR(x);
  stackgrps = FALSE;
  order = 1;
  switch(TYPEOF(x)) {
  case INTSXP : case LGLSXP :
    tmp = isorted(xd, n); break;
  case REALSXP :
    tmp = dsorted(xd, n); break;
  case STRSXP :
    tmp = csorted(xd, n); break;
  default :
    Error("type '%s' is not yet supported", type2char(TYPEOF(x)));
  }
  return(ScalarLogical( tmp==1 ? TRUE : FALSE ));
}

SEXP isOrderedSubset(SEXP x, SEXP nrow)
// specialized for use in [.data.table only
// Ignores 0s but heeds NAs and any out-of-range (which result in NA)
{
  int i=0, last, this;
  if (!length(x)) return(ScalarLogical(TRUE));
  if (!isInteger(x)) error("x has non-0 length but isn't an integer vector");
  if (!isInteger(nrow) || LENGTH(nrow)!=1 || INTEGER(nrow)[0]<0) error("nrow must be integer vector length 1 and >=0");
  if (LENGTH(x)<=1) return(ScalarLogical(TRUE));
  while (i<LENGTH(x) && INTEGER(x)[i]==0) i++;
  if (i==LENGTH(x)) return(ScalarLogical(TRUE));
  last = INTEGER(x)[i];  // the first non-0
  i++;
  for (; i<LENGTH(x); i++) {
    this = INTEGER(x)[i];
    if (this == 0) continue;
    if (this < last || this < 0 || this > INTEGER(nrow)[0])
      return(ScalarLogical(FALSE));
    last = this;
  }
  return(ScalarLogical(TRUE));
}


/*
  // LSD approach ...

  Rboolean firstTime = TRUE;
  for (radix=0; radix<4; radix++) {
    if (skip[radix]) continue;
    tmp = ord; ord = ans; ans = tmp;     // flip-flop
    shift = radix * 8;
    thiscounts = counts[radix];
    for (i=1;i<256;i++) thiscounts[i] += thiscounts[i-1];
    tt = clock();
    if (firstTime) {
      for(i = n-1; i >= 0; i--) {
        thisx = x[i] >> shift & 0xFF;
        ans[--thiscounts[thisx]] = i+1;
      }
    } else {
      for(i = n-1; i >= 0; i--) {
        thisx = x[ord[i]-1] >> shift & 0xFF;
        ans[--thiscounts[thisx]] = ord[i];
      }
    }
    memset(thiscounts, 0, 256*sizeof(unsigned int));
    t[6+radix] += clock()-tt;
    firstTime = FALSE;
  }
  if (firstTime) Error("Internal error: x is one repeated number so isorted should have skipped iradix, but iradix has been called and skipped all 4 radix");
}
*/

SEXP isReallyReal(SEXP x) {
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = 0;
  // return 0 (FALSE) when not type double, or is type double but contains integers
  // used to error if not passed type double but this needed extra is.double() calls in calling R code
  // which needed a repeat of the argument. Hence simpler and more robust to return 0 when not type double.
  if (isReal(x)) {
    int n=length(x), i=0;
    while (i<n &&
        ( ISNA(REAL(x)[i]) ||
        ( R_FINITE(REAL(x)[i]) && REAL(x)[i] == (int)(REAL(x)[i])))) {
      i++;
    }
    if (i<n) INTEGER(ans)[0] = i+1;  // return the location of first element which is really real; i.e. not an integer
  }
  UNPROTECT(1);
  return(ans);
}


SEXP binary(SEXP x)
// base::intToBits is close, but why does that print the result as "00 00 00 00" (raw) rather than ("0000") bits? Seems odd.
{
  char buffer[69];
  int j;
  if (!isReal(x)) error("x must be type 'double'");
  SEXP ans = PROTECT(allocVector(STRSXP, LENGTH(x)));
  for (int i=0; i<LENGTH(x); i++) {
    u.d = REAL(x)[i];
    j = 0;
    for(int bit=64; bit>=1; bit--)
    {
       buffer[j++] = '0' + (u.ull >> (bit-1) & 1);
       if (bit==64 || bit==53 || bit==17 || bit==9) buffer[j++]=' ';
       //       ^sign      ^exponent  ^last 2 byte rounding
    }
    SET_STRING_ELT(ans, i, mkCharLen(buffer,68));  // 64 bits + 4 spaces
  }
  UNPROTECT(1);
  return ans;
}


/*
Note on challenges implementing `nalast=NA` so that things are under the same perspective when looked at later. To go to the point, there are four challenges in implementing `nalast=NA` with the current form of forder.c (which is excellent!):

  1) Handling the i|d|csorted routines for nalast=NA:
     The current implementation of nalast=NA in *sorted routine is that if all values are NAs, then it returns -2, a different value, so that all values of o/osub can be replacedw with 0. If any values are NA, then we assume that this vector is unsorted and return 0 to be taken care of by the corresponding sort routine. Now, this may very well be questioned/improved. For ex: we could detect if the vector is sorted and also if the first value is NA, and if so, get the group size of these NAs and replace that many indices with 0. This is not yet done, but could very well be implemented later. If there are no NAs in the vector, then things are the same.

  2) Handling cases where n==1 and n==2:
     The current sorting routines don't take care of n<=2, and rightly so, as they can be dealt with directly. But it's not the case for nalast=NA because, when n==2 and all are NAs, point 1 will handle it. But when n==1 and it is NA and the column is not the first column to order upon, with the current implementation, "thisgrpn" is checked for "== 1" and if so, just pushes that group size and continues.. But we've to replace it with 0 in our case. So, nalast==0 is checked for inside that if statement and if so, if the value is NA is checked for all types and if so, replaced with 0. For n==2 and "any" is NA, we've to introduce a special case for this in each sort routine to take care of and not result in an error (which tells this should be already taken care of in *sorted).

  3) o[0] and newo[0] = -1:
     Since nalast already populates NAs with 0 values in o, we can't use the old value of 0 to check if it's already populated or not. Therefore this has been changed to -1.

  4) default cases are untouched as much as possible:
     In order to not compromise in speed for default case (`setkey`), iinsert and dinsert are not touched at all, which means, we've to make sure that they are not run when n<200 and nalast==0, as they don't replace o[x=NA] with 0. And, 'icount', 'iradix', 'dradix' are modified to take care of nalast=NA, to my knowledge.

Ends here.
------------------------------------------
This is just for my reference regarding the cases to take care of when checking for nalast=NA.
n=1     is NA   done.
n=1    not NA   done.
n=2  both NAs   done.
n=2    any NA   done.
n=2     no NA   done.
n>2   all NAs   done.
n>2    any NA   done.
n>2     no NA   done.

*/

/*
Added 2nd August 2014 - Testing different fixes for #743
When u.d = 0 or -0, we've to set it 0, so that the sign bit in -0 doesn't bite us later.

# Vector to benchmark on:
set.seed(45L)
x = 1*sample(-1e5:1e5, 1e8, TRUE)
require(data.table)

# code to run the benchmark:
system.time(data.table:::forderv(x))
system.time(data.table:::forderv(x))
system.time(data.table:::forderv(x))

# A. Timing on code before fix:
   user  system elapsed
 11.992   1.063  13.113
 11.968   1.095  13.154
 11.943   1.106  13.141

# B. Timing on fix using: u.ull <<= (!u.d); after the first line where we get 'u.d = order*...'
system.time(data.table:::forderv(x))
 12.640   1.095  13.814
 12.629   1.129  13.836
 12.634   1.135  13.836

# C. Timing on current solution:
system.time(data.table:::forderv(x))
 12.208   1.134  13.426
 12.233   1.148  13.445
 12.223   1.154  13.548

B is ~.7 sec slower whereas C is ~.3-.4 seconds slower. This should be okay on 100 million rows, I think!
*/
