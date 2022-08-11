#include "data.table.h"
//#include <time.h>

static int ngrp = 0;         // number of groups
static int *grpsize = NULL;  // size of each group, used by gmean (and gmedian) not gsum
static int nrow = 0;         // length of underlying x; same as length(ghigh) and length(glow)
static int *irows;           // GForce support for subsets in 'i' (TODO: joins in 'i')
static int irowslen = -1;    // -1 is for irows = NULL
static uint16_t *high=NULL, *low=NULL;  // the group of each x item; a.k.a. which-group-am-I
static int *restrict grp;    // TODO: eventually this can be made local for gforce as won't be needed globally when all functions here use gather
static size_t highSize;
static int shift, mask;
static char *gx=NULL;

static size_t nBatch, batchSize, lastBatchSize;
static int *counts, *tmpcounts;

// for gmedian
static int maxgrpn = 0;
static int *oo = NULL;
static int *ff = NULL;
static int isunsorted = 0;

// for first/last with n>1 to error when used with :=, until implemented
static bool assignByRef = false;

// from R's src/cov.c (for variance / sd)
#ifdef HAVE_LONG_DOUBLE
# define SQRTL sqrtl
#else
# define SQRTL sqrt
#endif

static int nbit(int n)
{
  // returns position of biggest bit; i.e. floor(log2(n))+1 without using fpa
  // not needed to be fast. Just a helper function.
  int nb=0;
  while (n) { nb++; n>>=1; }
  return nb;
}

SEXP gforce(SEXP env, SEXP jsub, SEXP o, SEXP f, SEXP l, SEXP irowsArg, SEXP grpcols, SEXP lhs) {
  int nprotect=0;
  double started = wallclock();
  const bool verbose = GetVerbose();
  assignByRef = !isNull(lhs);
  if (TYPEOF(env) != ENVSXP) error(_("env is not an environment"));
  // The type of jsub is pretty flexible in R, so leave checking to eval() below.
  if (!isInteger(o)) error(_("%s is not an integer vector"), "o");
  if (!isInteger(f)) error(_("%s is not an integer vector"), "f");
  if (!isInteger(l)) error(_("%s is not an integer vector"), "l");
  if (isNull(irowsArg)) {
    irows = NULL;
    irowslen = -1;
  }
  else if (isInteger(irowsArg)) {
    irows = INTEGER(irowsArg);
    irowslen = LENGTH(irowsArg);
  }
  else error(_("irowsArg is neither an integer vector nor NULL"));  // # nocov
  ngrp = LENGTH(l);
  if (LENGTH(f) != ngrp) error(_("length(f)=%d != length(l)=%d"), LENGTH(f), ngrp);
  nrow=0;
  grpsize = INTEGER(l);
  maxgrpn = 0;
  for (int i=0; i<ngrp; i++) {
    nrow+=grpsize[i];
    if (grpsize[i]>maxgrpn) maxgrpn = grpsize[i];  // old comment to be checked: 'needed for #2046 and #2111 when maxgrpn attribute is not attached to empty o'
  }
  if (LENGTH(o) && LENGTH(o)!=nrow) error(_("o has length %d but sum(l)=%d"), LENGTH(o), nrow);
  {
    SEXP tt = getAttrib(o, install("maxgrpn"));
    if (length(tt)==1 && INTEGER(tt)[0]!=maxgrpn) error(_("Internal error: o's maxgrpn attribute mismatches recalculated maxgrpn")); // # nocov
  }

  int nb = nbit(ngrp-1);
  shift = nb/2;    // /2 so that high and low can be uint16_t, and no limit (even for nb=4) to stress-test.
  // shift=MAX(nb-8,0); if (shift>16) shift=nb/2;     // TODO: when we have stress-test off mode, do this
  mask = (1<<shift)-1;
  highSize = ((ngrp-1)>>shift) + 1;

  grp = (int *)R_alloc(nrow, sizeof(int));   // TODO: use malloc and made this local as not needed globally when all functions here use gather
                                             // maybe better to malloc to avoid R's heap. This grp isn't global, so it doesn't need to be R_alloc
  const int *restrict fp = INTEGER(f);

  nBatch = MIN((nrow+1)/2, getDTthreads(nrow, true)*2);  // *2 to reduce last-thread-home. TODO: experiment. The higher this is though, the bigger is counts[]
  batchSize = MAX(1, (nrow-1)/nBatch);
  lastBatchSize = nrow - (nBatch-1)*batchSize;
  // We deliberate use, for example, 40 batches of just 14 rows, to stress-test tests. This strategy proved to be a good one as #3204 immediately came to light.
  // TODO: enable stress-test mode in tests only (#3205) which can be turned off by default in release to decrease overhead on small data
  //       if that is established to be biting (it may be fine).
  if (nBatch<1 || batchSize<1 || lastBatchSize<1) {
    error(_("Internal error: nrow=%d  ngrp=%d  nbit=%d  shift=%d  highSize=%d  nBatch=%d  batchSize=%d  lastBatchSize=%d\n"),  // # nocov
           nrow, ngrp, nb, shift, highSize, nBatch, batchSize, lastBatchSize);                                              // # nocov
  }
  // initial population of g:
  #pragma omp parallel for num_threads(getDTthreads(ngrp, false))
  for (int g=0; g<ngrp; g++) {
    int *elem = grp + fp[g]-1;
    for (int j=0; j<grpsize[g]; j++)  elem[j] = g;
  }
  if (verbose) { Rprintf(_("gforce initial population of grp took %.3f\n"), wallclock()-started); started=wallclock(); }
  isunsorted = 0;
  if (LENGTH(o)) {
    isunsorted = 1; // for gmedian

    // What follows is more cache-efficient version of this scattered assign :
    // for (int g=0; g<ngrp; g++) {
    //  const int *elem = op + fp[g]-1;
    //  for (int j=0; j<grpsize[g]; j++)  grp[ elem[j]-1 ] = g;
    //}

    const int *restrict op = INTEGER(o);  // o is a permutation of 1:nrow
    int nb = nbit(nrow-1);
    int shift = MAX(nb-8, 0);  // TODO: experiment nb/2.  Here it doesn't have to be /2 currently.
    int highSize = ((nrow-1)>>shift) + 1;
    //Rprintf(_("When assigning grp[o] = g, highSize=%d  nb=%d  shift=%d  nBatch=%d\n"), highSize, nb, shift, nBatch);
    int *counts = calloc(nBatch*highSize, sizeof(int));  // TODO: cache-line align and make highSize a multiple of 64
    int *TMP   = malloc(nrow*2l*sizeof(int)); // must multiple the long int otherwise overflow may happen, #4295
    if (!counts || !TMP ) error(_("Internal error: Failed to allocate counts or TMP when assigning g in gforce"));
    #pragma omp parallel for num_threads(getDTthreads(nBatch, false))   // schedule(dynamic,1)
    for (int b=0; b<nBatch; b++) {
      const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
      const int *my_o = op + b*batchSize;
      int *restrict my_counts = counts + b*highSize;
      for (int i=0; i<howMany; i++) {
        const int w = (my_o[i]-1) >> shift;
        my_counts[w]++;
      }
      for (int i=0, cum=0; i<highSize; i++) {
        int tmp = my_counts[i];
        my_counts[i] = cum;
        cum += tmp;
      }
      const int *restrict my_g = grp + b*batchSize;
      int *restrict my_tmp = TMP + b*2*batchSize;
      for (int i=0; i<howMany; i++) {
        const int w = (my_o[i]-1) >> shift;   // could use my_high but may as well use my_pg since we need my_pg anyway for the lower bits next too
        int *p = my_tmp + 2*my_counts[w]++;
        *p++ = my_o[i]-1;
        *p   = my_g[i];
      }
    }
    //Rprintf(_("gforce assign TMP (o,g) pairs took %.3f\n"), wallclock()-started); started=wallclock();
    #pragma omp parallel for num_threads(getDTthreads(highSize, false))
    for (int h=0; h<highSize; h++) {  // very important that high is first loop here
      for (int b=0; b<nBatch; b++) {
        const int start = h==0 ? 0 : counts[ b*highSize + h - 1 ];
        const int end   = counts[ b*highSize + h ];
        const int *restrict p = TMP + b*2*batchSize + start*2;
        for (int k=start; k<end; k++, p+=2) {
          grp[p[0]] = p[1];  // TODO: could write high here, and initial low.   ** If so, same in initial population when o is missing **
        }
      }
    }
    free(counts);
    free(TMP);
    //Rprintf(_("gforce assign TMP [ (o,g) pairs ] back to grp took %.3f\n"), wallclock()-started); started=wallclock();
  }

  high = (uint16_t *)R_alloc(nrow, sizeof(uint16_t));  // maybe better to malloc to avoid R's heap, but safer to R_alloc since it's done via eval()
  low  = (uint16_t *)R_alloc(nrow, sizeof(uint16_t));
  // global ghigh and glow because the g* functions (inside jsub) share this common memory

  gx = (char *)R_alloc(nrow, sizeof(Rcomplex));  // enough for a copy of one column (or length(irows) if supplied)
  // TODO: reduce to the largest type present; won't be faster (untouched RAM won't be fetched) but it will increase the largest size that works.

  counts = (int *)S_alloc(nBatch*highSize, sizeof(int));  // (S_ zeros) TODO: cache-line align and make highSize a multiple of 64
  tmpcounts = (int *)R_alloc(getDTthreads(nBatch, false)*highSize, sizeof(int));

  const int *restrict gp = grp;
  #pragma omp parallel for num_threads(getDTthreads(nBatch, false))   // schedule(dynamic,1)
  for (int b=0; b<nBatch; b++) {
    int *restrict my_counts = counts + b*highSize;
    uint16_t *restrict my_high = high + b*batchSize;
    const int *my_pg = gp + b*batchSize;
    const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
    for (int i=0; i<howMany; i++) {
      const int w = my_pg[i] >> shift;
      my_counts[w]++;
      my_high[i] = (uint16_t)w;  // reduce 4 bytes to 2
    }
    for (int i=0, cum=0; i<highSize; i++) {
      int tmp = my_counts[i];
      my_counts[i] = cum;
      cum += tmp;
    }
    uint16_t *restrict my_low = low + b*batchSize;
    int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
    memcpy(my_tmpcounts, my_counts, highSize*sizeof(int));
    for (int i=0; i<howMany; i++) {
      const int w = my_pg[i] >> shift;   // could use my_high but may as well use my_pg since we need my_pg anyway for the lower bits next too
      my_low[my_tmpcounts[w]++] = (uint16_t)(my_pg[i] & mask);
    }
    // counts is now cumulated within batch (with ending values) and we leave it that way
    // memcpy(counts + b*256, myCounts, 256*sizeof(int));  // save cumulate for later, first bucket contains position of next. For ease later in the very last batch.
  }
  if (verbose) { Rprintf(_("gforce assign high and low took %.3f\n"), wallclock()-started); started=wallclock(); }

  oo = INTEGER(o);
  ff = INTEGER(f);

  SEXP gans = PROTECT( eval(jsub, env) );  nprotect++; // just the gforce result columns; the group columns are added below when we know the shape
  if (verbose) { Rprintf(_("gforce eval took %.3f\n"), wallclock()-started); started=wallclock(); }
  // if this eval() fails with R error, R will release grp for us. Which is why we use R_alloc above.
  if (isVectorAtomic(gans)) {
    SEXP tt = PROTECT(allocVector(VECSXP, 1)); nprotect++;
    SET_VECTOR_ELT(tt, 0, gans);
    gans = tt;
  }
  
  // TODO: refine these comments
  //
  // Now replicate group values to match the number of rows in each group.
  // In most cases (e.g. mean, sum) gfuns return one value per group and there is nothing left to do because
  // each column in ans is ngrp long.
  // gforce_fixed_over_1 and gforce_dynamic
  // However, first/last with n>1 and na.rm=false results in MIN(grpsize[g], n) items per group. This is
  // still referred to as gforce_dynamic (because it's not a fixed 1 per group) but the gfun doesn't need
  // to return how many each group has (the lens att is null); all it needs return is the n it was passed.
  // shift is the same as first/last with n>1 and na.rm=false; i.e., just returns n and lens can be empty.
  // Currently just first/last with na.rm=true (regardless of n) returns the lens attribute because the
  // number of items for each group does depend on the data in that case; e.g. it can be 0 when all NA. 
  // Further, if the query consists of several calls to first/last, each call could have a
  // different value of n and/or na.rm=TRUE which would result in the group results not being aligned
  // across the columns of ans. Any gfun call which can result in not-1-value-per-group should attach
  // sym_gforce_dynamic to tell us here how to align the groups across the columns in ans.
  // For example, first/last with the default n=1 and na.rm=FALSE does not attach sym_gforce_dynamic
  // because it it returns a length-ngrp result.
  // If one of the ans columns contains a gforce_dynamic result, then the non-dynamic other columns
  // are replicated to match.
  // If there is only one column (very common) and it is gforce_dynamic then the gaps will now be removed
  // since each group was allocated to accomodate the largest result per group: max(grpsize[], n).
  
  // If there are many columns (e.g. 10,000) and many small groups (e.g. size 1-5 rows) then we wish to
  // avoid each dynamic gfun from allocating a new lens (ngrp long) if that can be avoided; e.g. shift(.SD)
  // always returns .N rows for each group; that shape is known before calling shift. If lens was allocated
  // and populated it would be the same as grpsize[], every time for each column.
  
  // 3 states :    i) no gforce_dynamic at all (including n=1 na.rm=false)
  //              ii) 1 < n < maxgrpn meaning min(n, grp_size[g]) must be summed.  We might have less than ngrp when na.rm=TRUE due to all-NA groups
  //             iii) n >= maxgrpn and na.rm=false meaning all groups are size grpsize[]; e.g. shift sets n=INT_MAX 
  // If we have reached maxgrpn, then we can stop there; e.g. shifting a lot of small groups which is within scope to do a good job on
  
  // We can point lens to be the grpsize[] and find maximum n that clamps the grpsize
  
  // Having one final lens, though, with max_dynamic_n clamped, is desirable, to pass to the eval of rep.int for example. Just one allocation is dramatically better than on each of the 10,000 gans columns.
  
  const int ngans=length(gans);
  SEXP lens=NULL;
  int max_w=0;
  bool lensCopied=false;
  for (int i=0; i<ngans; ++i) {
    SEXP tt;
    if (!isNull(tt=getAttrib(VECTOR_ELT(gans, i), sym_gforce_dynamic))) {
      if (isNull(VECTOR_ELT(tt, 0))) {
        int this_w=INTEGER(VECTOR_ELT(tt, 2))[0];
        if (this_w>max_w) max_w=this_w;
      } else {
        if (!lens) {
          lens=VECTOR_ELT(tt, 0);  // find the first dynamic column's lens and use it directly without copying it if there are no other dynamic columns
        } else {
          if (!lensCopied) {
            // upon the 2nd lens, we need to allocate a new lens to hold the max of the 2-or-more dynamic result with lens
            // allocate a new lens to calc the max size of each group across the dynamic columns
            // original gforce_dynamic attributes need to be retained so we can navigate those columns after we find the max
            //int *newlens = (int *)R_alloc(ngrp, sizeof(int));
            lens=PROTECT(duplicate(lens)); nprotect++;
            lensCopied=true;
          }
          int *lensp=INTEGER(lens);
          const int *ss=INTEGER(VECTOR_ELT(tt, 0));
          for (int g=0; g<ngrp; ++g)
            if (ss[g]>lensp[g]) lensp[g]=ss[g];
        }
      }
    }
  }
  if (max_w) {
    if (!lens) {
      // construct a lens because we currently need it to pass to rep.int below
      lens = PROTECT(allocVector(INTSXP, ngrp)); nprotect++;
      int *lensp = INTEGER(lens);
      for (int g=0; g<ngrp; ++g)
        lensp[g]=MIN(grpsize[g], max_w);
    } else {    
      // there is a mixture of non-lens and lens; e.g. .(first(colA,n=3), first(colB,n=3,na.rm=TRUE))
      if (!lensCopied) {
        // there is a single dynamic column so it wasn't copied yet
        lens=PROTECT(duplicate(lens)); nprotect++;
        lensCopied=true;
      }
      int *lensp = INTEGER(lens);
      for (int g=0; g<ngrp; ++g) {
        int this_w=MIN(grpsize[g], max_w);
        if (this_w>lensp[g]) lensp[g]=this_w;
      }
    }
  }
  // we have now maximized over any combination of over_1 and dynamic which determines the final shape in lens
  // TODO if max_w>=grpsize_max then further savings can be made

  // TODO: First we replicate group values to match the number of rows in each group if necessary
  // TODO: DT[, .(first(A,n=2), last(B,n=3)), by=group] -- missing test
  
  // We could require that vector output functions have to be returned in a list column but that could be expensive and likely would be flattened afterwards by user anyway.
  // If user really doesn't want flattening, then they can wrap with list().
  
  const int ngrpcol=length(grpcols);
  SEXP ans = PROTECT(allocVector(VECSXP, ngrpcol+ngans)); nprotect++;
  SEXP first_each_group = PROTECT( length(o) ? subsetVector(o, f) : f ); nprotect++;
  
  for (int i=0; i<ngrpcol; ++i) {
    const SEXP in = VECTOR_ELT(grpcols, i);
    SEXP out;
    SET_VECTOR_ELT(ans, i, out=allocVector(TYPEOF(in), ngrp));
    copyMostAttrib(in, out);
    subsetVectorRaw(out, in, first_each_group, /*anyNA=*/false);
    // avoiding subsetVector()'s check that idx is in bounds in case both ngrp and ngrpcol are large 
    // this is rep.int'd below when group results have more than 1-row.
    // TODO: for those cases avoid this subset first by enabling the rep.int to accept first_each_group
  }
  
  if (!lens) {
    // every group is length-1 (no dynamic or fixed_over_1 results) so can just use the gans as-is and we're done
    for (int i=0; i<ngans; ++i) {
      SET_VECTOR_ELT(ans, ngrpcol+i, VECTOR_ELT(gans, i));
    }
  } else {
    // There is one or more gforce_dynamic columns, gap removal and/or padding within groups may be necessary
    // and the non-gforce-dynamic columns (including group values) need to be replicated to align with the dynamic columns
    // Either we create another column, or we budge up/down the padding within the same allocated column
    // budging up/down many small groups will take a lot of reads and writes, albeit saving total memory. The
    // extra memory is only one-at-a-time per column though, so choose speed over memory here.
    // gforce_dynamic includes any result where the number of values per group may not be 1; e.g. shift() simply sets
    // its lens to equal the grp lens to save an allocation
    
    int anslen=0;  // to be populated when no dynamic is just the length of the first ans column
    const int *lensp = INTEGER(lens);
    // if (lens==grpsize) {
    //  if (max_dynamic_n <= 1) error("Internal error: max_dynamic_n<=1"); // is not considered dynamic and should not be marked as such
    //  if (max_dynamic_n >= maxgrpn) anslen=nrow;                               // e.g. last(.SD, 2) where biggest group is 2 rows
    //  else for (int g=0; g<ngrp; ++g) anslen+=MIN(grpsize[g], max_dynamic_n);  // e.g. last(.SD, 2) where biggest group is >2 rows, and there might be some 1-row groups
    //} else {
    
    for (int g=0; g<ngrp; ++g) anslen+=lensp[g];  // contiguous sweep insignificant time not worth trying to save; e.g. by including the sum in the gforce_dynamic attribute returned for when there's just one column and it's dynamic
    
    for (int i=0; i<ngrpcol; ++i) {
      SET_VECTOR_ELT(ans, i, eval(PROTECT(lang3(install("rep.int"), VECTOR_ELT(ans, i), lens)), R_GlobalEnv));
      UNPROTECT(1);
      // use R's rep.int for now, no C API for it afaik. R's rep.int will be summing lens here on each call which we can avoid (TODO).
      // TODO: expand subsetVector to accept 'rep=' and 'len=' so that subset and rep.int can be done in one step avoiding intermediate subset of first_each_group, and pass len=anslen which we know already
    }

    for (int i=0; i<ngans; ++i) {
      SEXP tt = VECTOR_ELT(gans, i);
      SEXP att = getAttrib(tt, sym_gforce_dynamic);  // how long the items are, we won't parallelize within column so we can sweep forwards
      if (isNull(att)) {
        // e.g. the mean in .(mean(colA), first(colB, n=2));    TODO test
        SET_VECTOR_ELT(ans, ngrpcol+i, eval(PROTECT(lang3(install("rep.int"), tt, lens)), R_GlobalEnv));
        UNPROTECT(1);
      } else {
        // att can be i) empty lens vec in which case this ans col's shape is min(grpsize,w). If this matches the end result shape (i.e. all ans columns are the same, then no need to allocate copy)
        //           ii) presence of lens vec always needs a copy and pad? (or could loop it and test if the same since looping through ngrp is much faster than nrow vector) 
        const int *ss = isNull(VECTOR_ELT(att,0)) ? NULL : INTEGER(VECTOR_ELT(att,0));
        const bool first = LOGICAL(VECTOR_ELT(att, 1))[0];
        const int w = INTEGER(VECTOR_ELT(att,2))[0];
        SEXP newcol = PROTECT(allocVector(TYPEOF(tt), anslen));
        copyMostAttrib(tt, newcol);
        setAttrib(newcol, sym_gforce_dynamic, R_NilValue);
        int ansi=0, k=0;

        #define DO(CTYPE, RTYPE, RNA, ASSIGN) {                                                        \
        const CTYPE *xd = (const CTYPE *)RTYPE(tt);                                                    \
        CTYPE *ansd = (CTYPE *)RTYPE(newcol);                                                          \
        CTYPE val = RNA;                                                                               \
        for (int g=0; g<ngrp; ++g) {                                                                   \
          const int grp_allocated = MIN(grpsize[g], w);                                                \
          const int thislen = ss ? ss[g] : grp_allocated;                                              \
          const int targetlen = lensp[g];                                                              \
          const int napad = targetlen-thislen;                                                         \
          k += (!first)*(grp_allocated-thislen);                                                       \
          for (int i=0; i<thislen; ++i) { val=xd[k++]; ASSIGN; }                                       \
          val=RNA; for (int i=0; i<napad; ++i) ASSIGN;                                                 \
          k += (first)*(grp_allocated-thislen);                                                        \
        }                                                                                              \
        ansd++; /* just to suppress unused-variable warning in STRSXP and VECSXP cases */              \
        } break;

        switch(TYPEOF(tt)) {
        case RAWSXP:  DO(Rbyte,    RAW,     0,            ansd[ansi++]=val)
        case LGLSXP:  DO(int,      LOGICAL, NA_LOGICAL,   ansd[ansi++]=val)
        case INTSXP:  DO(int,      INTEGER, NA_INTEGER,   ansd[ansi++]=val)
        case REALSXP: if (INHERITS(tt, char_integer64)) {
                      DO(int64_t,  REAL,    NA_INTEGER64, ansd[ansi++]=val)
             } else { DO(double,   REAL,    NA_REAL,      ansd[ansi++]=val) }
        case CPLXSXP: DO(Rcomplex, COMPLEX, NA_CPLX,      ansd[ansi++]=val)
        case STRSXP:  DO(SEXP,  STRING_PTR, NA_STRING,    SET_STRING_ELT(newcol,ansi++,val))
        case VECSXP:  DO(SEXP,  SEXPPTR_RO, ScalarLogical(NA_LOGICAL), SET_VECTOR_ELT(newcol,ansi++,val))       /* TODO: global replace ScalarLogical() with fixed constant R_NAValue, depending on R dependency */
        default:
          error(_("Type '%s' is not supported by gforce padding."), type2char(TYPEOF(tt)));
        }
        SET_VECTOR_ELT(ans, ngrpcol+i, newcol);
        UNPROTECT(1); // newcol
      }
    }
  }
  UNPROTECT(nprotect);
  return ans;
}

void *gather(SEXP x, bool *anyNA)
{
  double started=wallclock();
  const bool verbose = GetVerbose();
  if (verbose) Rprintf(_("gather took ... "));
  switch (TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *restrict thisx = INTEGER(x);
    #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
    for (int b=0; b<nBatch; b++) {
      int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
      memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(int));   // original cumulated   // already cumulated for this batch
      int *restrict my_gx = (int *)gx + b*batchSize;
      const uint16_t *my_high = high + b*batchSize;
      const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
      bool my_anyNA = false;
      if (irowslen==-1) {
        const int *my_x = thisx + b*batchSize;
        for (int i=0; i<howMany; i++) {
          const int elem = my_x[i];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          if (elem==NA_INTEGER) my_anyNA = true;
        }
      } else {
        const int *my_x = irows + b*batchSize;
        for (int i=0; i<howMany; i++) {
          int elem = my_x[i]==NA_INTEGER ? NA_INTEGER : thisx[ my_x[i]-1 ];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          if (elem==NA_INTEGER) my_anyNA = true;
        }
      }
      if (my_anyNA) *anyNA = true;  // naked write ok since just bool and always writing true; and no performance issue as maximum nBatch writes
    }
  } break;
  case REALSXP: {
    if (!INHERITS(x, char_integer64)) {
      const double *restrict thisx = REAL(x);
      #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
      for (int b=0; b<nBatch; b++) {
        int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
        memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(int));
        double *restrict my_gx = (double *)gx + b*batchSize;
        const uint16_t *my_high = high + b*batchSize;
        const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
        bool my_anyNA = false;
        if (irowslen==-1) {
          const double *my_x = thisx + b*batchSize;
          for (int i=0; i<howMany; i++) {
            const double elem = my_x[i];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (ISNAN(elem)) my_anyNA = true;   // R's ISNAN includes NA; i.e. defined as C isnan with some platform specific differences (perhaps historic)
          }
        } else {
          const int *my_x = irows + b*batchSize;
          for (int i=0; i<howMany; i++) {
            double elem = my_x[i]==NA_INTEGER ? NA_REAL : thisx[ my_x[i]-1 ];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (ISNAN(elem)) my_anyNA = true;
          }
        }
        if (my_anyNA) *anyNA = true;
      }
    } else {
      const int64_t *restrict thisx = (int64_t *)REAL(x);
      #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
      for (int b=0; b<nBatch; b++) {
        int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
        memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(int));
        int64_t *restrict my_gx = (int64_t *)gx + b*batchSize;
        const uint16_t *my_high = high + b*batchSize;
        const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
        bool my_anyNA = false;
        if (irowslen==-1) {
          const int64_t *my_x = thisx + b*batchSize;
          for (int i=0; i<howMany; i++) {
            const int64_t elem = my_x[i];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (elem==INT64_MIN) my_anyNA = true;
          }
        } else {
          const int *my_x = irows + b*batchSize;
          for (int i=0; i<howMany; i++) {
            int64_t elem = my_x[i]==NA_INTEGER ? NA_INTEGER64 : thisx[ my_x[i]-1 ];
            my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
            if (elem==INT64_MIN) my_anyNA = true;
          }
        }
        if (my_anyNA) *anyNA = true;
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict thisx = COMPLEX(x);
    #pragma omp parallel for num_threads(getDTthreads(nBatch, false))
    for (int b=0; b<nBatch; b++) {
      int *restrict my_tmpcounts = tmpcounts + omp_get_thread_num()*highSize;
      memcpy(my_tmpcounts, counts + b*highSize, highSize*sizeof(int));
      Rcomplex *restrict my_gx = (Rcomplex *)gx + b*batchSize;
      const uint16_t *my_high = high + b*batchSize;
      const int howMany = b==nBatch-1 ? lastBatchSize : batchSize;
      bool my_anyNA = false;
      if (irowslen==-1) {
        const Rcomplex *my_x = thisx + b*batchSize;
        for (int i=0; i<howMany; i++) {
          const Rcomplex elem = my_x[i];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          // typically just checking one component would be enough,
          //   but ?complex suggests there may be some edge cases; better to be safe
          if (ISNAN(elem.r) && ISNAN(elem.i)) my_anyNA = true;
        }
      } else {
        const int *my_x = irows + b*batchSize;
        for (int i=0; i<howMany; i++) {
          Rcomplex elem = my_x[i]==NA_INTEGER ? NA_CPLX : thisx[ my_x[i]-1 ];
          my_gx[ my_tmpcounts[my_high[i]]++ ] = elem;
          if (ISNAN(elem.r) && ISNAN(elem.i)) my_anyNA = true;
        }
      }
      if (my_anyNA) *anyNA = true;  // naked write ok since just bool and always writing true; and no performance issue as maximum nBatch writes
    }
  } break;
  default :
    error(_("gather implemented for INTSXP, REALSXP, and CPLXSXP but not '%s'"), type2char(TYPEOF(x)));   // # nocov
  }
  if (verbose) { Rprintf(_("%.3fs\n"), wallclock()-started); }
  return gx;
}

SEXP gsum(SEXP x, SEXP narmArg)
{
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  const bool narm = LOGICAL(narmArg)[0];
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "sum");
  const int n = (irowslen == -1) ? length(x) : irowslen;
  double started = wallclock();
  const bool verbose=GetVerbose();
  if (verbose) Rprintf(_("This gsum (narm=%s) took ... "), narm?"TRUE":"FALSE");
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gsum");
  bool anyNA=false;
  SEXP ans;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(INTSXP, ngrp));
    int *restrict ansp = INTEGER(ans);
    memset(ansp, 0, ngrp*sizeof(int));
    bool overflow=false;
    //double started = wallclock();
    if (!anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false)) //schedule(dynamic,1)
      for (int h=0; h<highSize; h++) {   // very important that high is first loop here
        int *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const int *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const int a = _ans[my_low[i]];
            const int b = my_gx[i];
            if ((a>0 && b>INT_MAX-a) || (a<0 && b<NA_INTEGER+1-a)) overflow=true;
            else _ans[my_low[i]] += b;  // naked by design; each thread does all of each h for all batches
          }
        }
      }
    } else {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        int *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const int *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const int a = _ans[my_low[i]];
            if (a==NA_INTEGER) continue;
            const int b = my_gx[i];
            if (b==NA_INTEGER) {
              if (!narm) _ans[my_low[i]]=NA_INTEGER;
              continue;
            }
            if ((a>0 && b>INT_MAX-a) || (a<0 && b<NA_INTEGER+1-a)) overflow=true;
            else _ans[my_low[i]] += b;
          }
        }
      }
    }
    //Rprintf(_("gsum int took %.3f\n"), wallclock()-started);
    if (overflow) {
      UNPROTECT(1); // discard the result with overflow
      warning(_("The sum of an integer column for a group was more than type 'integer' can hold so the result has been coerced to 'numeric' automatically for convenience."));
      ans = PROTECT(allocVector(REALSXP, ngrp));
      double *restrict ansp = REAL(ans);
      memset(ansp, 0, ngrp*sizeof(double));
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        double *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const int *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          // rare and slower so no need to switch on anyNA
          for (int i=0; i<howMany; i++) {
            const int elem = my_gx[i];
            if (elem==NA_INTEGER) {
              if (!narm) _ans[my_low[i]]=NA_REAL;
              continue;
            }
            _ans[my_low[i]] += elem;  // let NA_REAL propagate
          }
        }
      }
    }
  } break;
  case REALSXP: {
    if (!INHERITS(x, char_integer64)) {
      const double *restrict gx = gather(x, &anyNA);
      ans = PROTECT(allocVector(REALSXP, ngrp));
      double *restrict ansp = REAL(ans);
      memset(ansp, 0, ngrp*sizeof(double));
      if (!narm || !anyNA) {
        #pragma omp parallel for num_threads(getDTthreads(highSize, false))
        for (int h=0; h<highSize; h++) {
          double *restrict _ans = ansp + (h<<shift);
          for (int b=0; b<nBatch; b++) {
            const int pos = counts[ b*highSize + h ];
            const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
            const double *my_gx = gx + b*batchSize + pos;
            const uint16_t *my_low = low + b*batchSize + pos;
            for (int i=0; i<howMany; i++) {
              _ans[my_low[i]] += my_gx[i];  // let NA propagate when !narm
            }
          }
        }
      } else {
        // narm==true and anyNA==true
        #pragma omp parallel for num_threads(getDTthreads(highSize, false))
        for (int h=0; h<highSize; h++) {
          double *restrict _ans = ansp + (h<<shift);
          for (int b=0; b<nBatch; b++) {
            const int pos = counts[ b*highSize + h ];
            const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
            const double *my_gx = gx + b*batchSize + pos;
            const uint16_t *my_low = low + b*batchSize + pos;
            for (int i=0; i<howMany; i++) {
              const double elem = my_gx[i];
              if (!ISNAN(elem)) _ans[my_low[i]] += elem;
            }
          }
        }
      }
    } else { // int64
      const int64_t *restrict gx = gather(x, &anyNA);
      ans = PROTECT(allocVector(REALSXP, ngrp));
      int64_t *restrict ansp = (int64_t *)REAL(ans);
      memset(ansp, 0, ngrp*sizeof(int64_t));
      if (!anyNA) {
        #pragma omp parallel for num_threads(getDTthreads(highSize, false))
        for (int h=0; h<highSize; h++) {
          int64_t *restrict _ans = ansp + (h<<shift);
          for (int b=0; b<nBatch; b++) {
            const int pos = counts[ b*highSize + h ];
            const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
            const int64_t *my_gx = gx + b*batchSize + pos;
            const uint16_t *my_low = low + b*batchSize + pos;
            for (int i=0; i<howMany; i++) {
              _ans[my_low[i]] += my_gx[i]; // does not propagate INT64 for !narm
            }
          }
        }
      } else { // narm==true/false and anyNA==true
        if (!narm) {
          #pragma omp parallel for num_threads(getDTthreads(highSize, false))
          for (int h=0; h<highSize; h++) {
            int64_t *restrict _ans = ansp + (h<<shift);
            for (int b=0; b<nBatch; b++) {
              const int pos = counts[ b*highSize + h ];
              const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
              const int64_t *my_gx = gx + b*batchSize + pos;
              const uint16_t *my_low = low + b*batchSize + pos;
              for (int i=0; i<howMany; i++) {
                const int64_t elem = my_gx[i];
                if (elem!=INT64_MIN) {
                  _ans[my_low[i]] += elem;
                } else {
                  _ans[my_low[i]] = INT64_MIN;
                  break;
                }
              }
            }
          }
        } else {
          #pragma omp parallel for num_threads(getDTthreads(highSize, false))
          for (int h=0; h<highSize; h++) {
            int64_t *restrict _ans = ansp + (h<<shift);
            for (int b=0; b<nBatch; b++) {
              const int pos = counts[ b*highSize + h ];
              const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
              const int64_t *my_gx = gx + b*batchSize + pos;
              const uint16_t *my_low = low + b*batchSize + pos;
              for (int i=0; i<howMany; i++) {
                const int64_t elem = my_gx[i];
                if (elem!=INT64_MIN) _ans[my_low[i]] += elem;
              }
            }
          }
        }
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(CPLXSXP, ngrp));
    Rcomplex *restrict ansp = COMPLEX(ans);
    memset(ansp, 0, ngrp*sizeof(Rcomplex));
    if (!narm || !anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            _ans[my_low[i]].r += my_gx[i].r;  // let NA propagate when !narm
            _ans[my_low[i]].i += my_gx[i].i;
          }
        }
      }
    } else {
      // narm==true and anyNA==true
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const Rcomplex elem = my_gx[i];
            if (!ISNAN(elem.r)) _ans[my_low[i]].r += elem.r;
            if (!ISNAN(elem.i)) _ans[my_low[i]].i += elem.i;
          }
        }
      }
    }
  } break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)), "sum (gsum)", "base::sum(.)");
  }
  copyMostAttrib(x, ans);
  if (verbose) { Rprintf(_("%.3fs\n"), wallclock()-started); }
  UNPROTECT(1);
  return(ans);
}

SEXP gmean(SEXP x, SEXP narmArg)
{
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "mean");
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  const bool narm = LOGICAL(narmArg)[0];
  const int n = (irowslen == -1) ? length(x) : irowslen;
  double started = wallclock();
  const bool verbose=GetVerbose();
  if (verbose) Rprintf(_("This gmean took (narm=%s) ... "), narm?"TRUE":"FALSE"); // narm=TRUE only at this point
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gmean");
  bool anyNA=false;
  SEXP ans=R_NilValue;
  int protecti=0;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP:
    x = PROTECT(coerceVector(x, REALSXP)); protecti++;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) {
      x = PROTECT(coerceAs(x, /*as=*/ScalarReal(1), /*copyArg=*/ScalarLogical(TRUE))); protecti++;
    }
    const double *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(REALSXP, ngrp)); protecti++;
    double *restrict ansp = REAL(ans);
    memset(ansp, 0, ngrp*sizeof(double));
    if (!narm || !anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        double *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const double *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            _ans[my_low[i]] += my_gx[i];  // let NA propagate when !narm
          }
        }
      }
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) ansp[i] /= grpsize[i];
    } else {
      // narm==true and anyNA==true
      int *restrict nna_counts = calloc(ngrp, sizeof(int));
      if (!nna_counts) error(_("Unable to allocate %d * %d bytes for non-NA counts in gmean na.rm=TRUE"), ngrp, sizeof(int));
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
          double *restrict _ans = ansp + (h<<shift);
          int *restrict _nna = nna_counts + (h<<shift);
          for (int b=0; b<nBatch; b++) {
            const int pos = counts[ b*highSize + h ];
            const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
            const double *my_gx = gx + b*batchSize + pos;
            const uint16_t *my_low = low + b*batchSize + pos;
            for (int i=0; i<howMany; i++) {
              const double elem = my_gx[i];
              if (!ISNAN(elem)) {
                _ans[my_low[i]] += elem;
                _nna[my_low[i]]++;
              }
            }
          }
        }
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) ansp[i] /= nna_counts[i];
      free(nna_counts);
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *restrict gx = gather(x, &anyNA);
    ans = PROTECT(allocVector(CPLXSXP, ngrp)); protecti++;
    Rcomplex *restrict ansp = COMPLEX(ans);
    memset(ansp, 0, ngrp*sizeof(Rcomplex));
    if (!narm || !anyNA) {
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            _ans[my_low[i]].r += my_gx[i].r;  // let NA propagate when !narm
            _ans[my_low[i]].i += my_gx[i].i;
          }
        }
      }
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) {
        ansp[i].i /= grpsize[i];
        ansp[i].r /= grpsize[i];
      }
    } else {
      // narm==true and anyNA==true
      int *restrict nna_counts_r = calloc(ngrp, sizeof(int));
      int *restrict nna_counts_i = calloc(ngrp, sizeof(int));
      if (!nna_counts_r || !nna_counts_i) {
        // # nocov start
        free(nna_counts_r);  // free(NULL) is allowed and does nothing. Avoids repeating the error() call here.
        free(nna_counts_i);
        error(_("Unable to allocate %d * %d bytes for non-NA counts in gmean na.rm=TRUE"), ngrp, sizeof(int));
        // # nocov end
      }
      #pragma omp parallel for num_threads(getDTthreads(highSize, false))
      for (int h=0; h<highSize; h++) {
        Rcomplex *restrict _ans = ansp + (h<<shift);
        int *restrict _nna_r = nna_counts_r + (h<<shift);
        int *restrict _nna_i = nna_counts_i + (h<<shift);
        for (int b=0; b<nBatch; b++) {
          const int pos = counts[ b*highSize + h ];
          const int howMany = ((h==highSize-1) ? (b==nBatch-1?lastBatchSize:batchSize) : counts[ b*highSize + h + 1 ]) - pos;
          const Rcomplex *my_gx = gx + b*batchSize + pos;
          const uint16_t *my_low = low + b*batchSize + pos;
          for (int i=0; i<howMany; i++) {
            const Rcomplex elem = my_gx[i];
            if (!ISNAN(elem.r)) {
              _ans[my_low[i]].r += elem.r;
              _nna_r[my_low[i]]++;
            }
            if (!ISNAN(elem.i)) {
              _ans[my_low[i]].i += elem.i;
              _nna_i[my_low[i]]++;
            }
          }
        }
      }
      #pragma omp parallel for num_threads(getDTthreads(ngrp, true))
      for (int i=0; i<ngrp; i++) {
        ansp[i].r /= nna_counts_r[i];
        ansp[i].i /= nna_counts_i[i];
      }
      free(nna_counts_r);
      free(nna_counts_i);
    }
  } break;
  default:
    error(_("Type '%s' not supported by GForce mean (gmean). Either add the prefix base::mean(.) or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  if (verbose) { Rprintf(_("%.3fs\n"), wallclock()-started); }
  UNPROTECT(protecti);
  return(ans);
}

static SEXP gminmax(SEXP x, SEXP narm, const bool min)
{
  if (!IS_TRUE_OR_FALSE(narm))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  if (!isVectorAtomic(x)) error(_("GForce min/max can only be applied to columns, not .SD or similar. To find min/max of all items in a list such as .SD, either add the prefix base::min(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,min),by=,.SDcols=]'"));
  if (inherits(x, "factor") && !inherits(x, "ordered"))
    error(_("%s is not meaningful for factors."), min?"min":"max");
  const bool nosubset = irowslen==-1;
  const int n = nosubset ? length(x) : irowslen;
  //clock_t start = clock();
  SEXP ans;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gminmax");
  // GForce guarantees each group has at least one value; i.e. we don't need to consider length-0 per group here
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    ans = PROTECT(allocVector(INTSXP, ngrp));
    int *ansd = INTEGER(ans);
    const int *xd = INTEGER(x);
    if (!LOGICAL(narm)[0]) {
      const int init = min ? INT_MAX : INT_MIN+1;  // NA_INTEGER==INT_MIN checked in init.c
      for (int i=0; i<ngrp; ++i) ansd[i] = init;
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_INTEGER) continue;  // once an NA has been observed in this group, it doesn't matter what the remaining values in this group are
        const int elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER : xd[irows[i]-1]);
        if (elem==NA_INTEGER || (elem<ansd[thisgrp])==min)
          ansd[thisgrp] = elem;  // always true on the first value in the group (other than if the first value is INT_MAX or INT_MIN-1 which is fine too)
      }
    } else {
      for (int i=0; i<ngrp; ++i) ansd[i] = NA_INTEGER;  // in the all-NA case we now return NA for type consistency
      for (int i=0; i<n; ++i) {
        const int elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER : xd[irows[i]-1]);
        if (elem==NA_INTEGER) continue;
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_INTEGER || (elem<ansd[thisgrp])==min)
          ansd[thisgrp] = elem;
      }
    }}
    break;
  case STRSXP: {
    ans = PROTECT(allocVector(STRSXP, ngrp));
    const SEXP *ansd = STRING_PTR(ans);
    const SEXP *xd = STRING_PTR(x);
    if (!LOGICAL(narm)[0]) {
      const SEXP init = min ? char_maxString : R_BlankString;  // char_maxString == "\xFF\xFF..." in init.c
      for (int i=0; i<ngrp; ++i) SET_STRING_ELT(ans, i, init);
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_STRING) continue;
        const SEXP elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_STRING : xd[irows[i]-1]);
        if (elem==NA_STRING || (strcmp(CHAR(elem), CHAR(ansd[thisgrp]))<0)==min)
          SET_STRING_ELT(ans, thisgrp, elem);
      }
    } else {
      for (int i=0; i<ngrp; ++i) SET_STRING_ELT(ans, i, NA_STRING); // all missing returns NA consistent with base
      for (int i=0; i<n; ++i) {
        const SEXP elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_STRING : xd[irows[i]-1]);
        if (elem==NA_STRING) continue;
        const int thisgrp = grp[i];
        if (ansd[thisgrp]==NA_STRING || (strcmp(CHAR(elem), CHAR(ansd[thisgrp]))<0)==min)
          SET_STRING_ELT(ans, thisgrp, elem);
      }
    }}
    break;
  case REALSXP: {
    ans = PROTECT(allocVector(REALSXP, ngrp));
    if (INHERITS(x, char_integer64)) {
      int64_t *ansd = (int64_t *)REAL(ans);
      const int64_t *xd = (const int64_t *)REAL(x);
      if (!LOGICAL(narm)[0]) {
        const int64_t init = min ? INT64_MAX : INT64_MIN+1;
        for (int i=0; i<ngrp; ++i) ansd[i] = init;
        for (int i=0; i<n; ++i) {
          const int thisgrp = grp[i];
          if (ansd[thisgrp]==NA_INTEGER64) continue;
          const int64_t elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER64 : xd[irows[i]-1]);
          if (elem==NA_INTEGER64 || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      } else {
        for (int i=0; i<ngrp; ++i) ansd[i] = NA_INTEGER64;
        for (int i=0; i<n; ++i) {
          const int64_t elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER64 : xd[irows[i]-1]);
          if (elem==NA_INTEGER64) continue;
          const int thisgrp = grp[i];
          if (ansd[thisgrp]==NA_INTEGER64 || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      }
    } else {
      double *ansd = REAL(ans);
      const double *xd = REAL(x);
      if (!LOGICAL(narm)[0]) {
        const double init = min ? R_PosInf : R_NegInf;
        for (int i=0; i<ngrp; ++i) ansd[i] = init;
        for (int i=0; i<n; ++i) {
          const int thisgrp = grp[i];
          if (ISNAN(ansd[thisgrp])) continue;
          const double elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_REAL : xd[irows[i]-1]);
          if (ISNAN(elem) || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      } else {
        for (int i=0; i<ngrp; ++i) ansd[i] = NA_REAL;
        for (int i=0; i<n; ++i) {
          const double elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_REAL : xd[irows[i]-1]);
          if (ISNAN(elem)) continue;
          const int thisgrp = grp[i];
          if (ISNAN(ansd[thisgrp]) || (elem<ansd[thisgrp])==min)
            ansd[thisgrp] = elem;
        }
      }
    }}
    break;
  case CPLXSXP:
    error(_("Type 'complex' has no well-defined min/max"));
    break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"),
          type2char(TYPEOF(x)), min?"min (gmin)":"max (gmax)", min?"base::min(.)":"base::max(.)");
  }
  copyMostAttrib(x, ans); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
  UNPROTECT(1);  // ans
  // Rprintf(_("this gminmax took %8.3f\n"), 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return(ans);
}

SEXP gmin(SEXP x, SEXP narm)
{
  return gminmax(x, narm, true);
}

SEXP gmax(SEXP x, SEXP narm)
{
  return gminmax(x, narm, false);
}

// gmedian, always returns numeric type (to avoid as.numeric() wrap..)
SEXP gmedian(SEXP x, SEXP narmArg) {
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  if (!isVectorAtomic(x)) error(_("GForce median can only be applied to columns, not .SD or similar. To find median of all items in a list such as .SD, either add the prefix stats::median(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,median),by=,.SDcols=]'"));
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "median");
  const bool isInt64 = INHERITS(x, char_integer64), narm = LOGICAL(narmArg)[0];
  const int n = (irowslen == -1) ? length(x) : irowslen;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gmedian");
  SEXP ans = PROTECT(allocVector(REALSXP, ngrp));
  double *ansd = REAL(ans);
  const bool nosubset = irowslen==-1;
  switch(TYPEOF(x)) {
  case REALSXP: {
    double *subd = REAL(PROTECT(allocVector(REALSXP, maxgrpn))); // allocate once upfront and reuse
    int64_t *xi64 = (int64_t *)REAL(x);
    double  *xd = REAL(x);
    for (int i=0; i<ngrp; ++i) {
      int thisgrpsize = grpsize[i], nacount=0;
      for (int j=0; j<thisgrpsize; ++j) {
        int k = ff[i]+j-1;
        if (isunsorted) k = oo[k]-1;
        k = nosubset ? k : (irows[k]==NA_INTEGER ? NA_INTEGER : irows[k]-1);
        if (k==NA_INTEGER || (isInt64 ? xi64[k]==NA_INTEGER64 : ISNAN(xd[k]))) nacount++;
        else subd[j-nacount] = xd[k];
      }
      thisgrpsize -= nacount;  // all-NA is returned as NA_REAL via n==0 case inside *quickselect
      ansd[i] = (nacount && !narm) ? NA_REAL : (isInt64 ? i64quickselect((void *)subd, thisgrpsize) : dquickselect(subd, thisgrpsize));
    }}
    break;
  case LGLSXP: case INTSXP: {
    int *subi = INTEGER(PROTECT(allocVector(INTSXP, maxgrpn)));
    int *xi = INTEGER(x);
    for (int i=0; i<ngrp; i++) {
      const int thisgrpsize = grpsize[i];
      int nacount=0;
      for (int j=0; j<thisgrpsize; ++j) {
        int k = ff[i]+j-1;
        if (isunsorted) k = oo[k]-1;
        if (nosubset ? xi[k]==NA_INTEGER : (irows[k]==NA_INTEGER || (k=irows[k]-1,xi[k]==NA_INTEGER))) nacount++;
        else subi[j-nacount] = xi[k];
      }
      ansd[i] = (nacount && !narm) ? NA_REAL : iquickselect(subi, thisgrpsize-nacount);
    }}
    break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)), "median (gmedian)", "stats::median(.)");
  }
  if (!isInt64) copyMostAttrib(x, ans);
  // else the integer64 class needs to be dropped since double is always returned by gmedian
  UNPROTECT(2);  // ans, subd|subi
  return ans;
}

static SEXP gfirstlast(const SEXP x, const bool first, const SEXP nArg, const bool nthvalue, const SEXP narmArg) {
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");  // # nocov
  const bool narm = LOGICAL(narmArg)[0];
  if (!isInteger(nArg) || LENGTH(nArg)!=1 || INTEGER(nArg)[0]<0)
    error(_("Internal error, gfirstlast is not implemented for n<0. This should have been caught before. Please report to data.table issue tracker.")); // # nocov
  const int w = INTEGER(nArg)[0];
  if (w>1 && assignByRef)
    error(_("Is first/last/head/tail with n>1 and := by group intentional? Please provide a use case to the GitHub issue tracker. It could be implemented."));
  // select 1:w when first=TRUE, and (n-w+1):n when first=FALSE
  // or select w'th item when nthvalue=TRUE; e.g. the n=0 case in test 280
  const bool nosubset = irowslen == -1;
  const bool issorted = !isunsorted; // make a const-bool for use inside loops
  const int n = nosubset ? length(x) : irowslen;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, first?"gfirst":"glast");
  int anslen = ngrp;
  if (!nthvalue && w>1) {
    anslen = 0;
    for (int i=0; i<ngrp; ++i) {
      anslen += MIN(w, grpsize[i]);
    }
  }
  SEXP ans = PROTECT(allocVector(TYPEOF(x), anslen));
  int *anslens = NULL;
  if (narm || (w>1 && !nthvalue)) {  // w>1 because some groups may be smaller than w so we need to save w
    // narm=true needing gforce_dynamic is clear
    // when w>1 and narm=false, we could avoid gforce_dynamic since each group result in MIN(w,grpsize[g]) 
    // how many non-NA were found for each group im
    SEXP att, v;
    setAttrib(ans, sym_gforce_dynamic, att=allocVector(VECSXP, 3));
    SET_VECTOR_ELT(att, 0, v = narm ? allocVector(INTSXP, ngrp) : R_NilValue);
    SET_VECTOR_ELT(att, 1, ScalarLogical(first)); // so gforce knows which end the data is (last writes from the end of the alloc)
    SET_VECTOR_ELT(att, 2, ScalarInteger(w));     // to know how many were allocated for each group; i.e. MIN(w,grpsize[i])
    if (narm) anslens = INTEGER(v);
  }
  int ansi = 0;
  #undef DO
  #define DO(CTYPE, RTYPE, RNA, ASSIGN) {                                                          \
    const CTYPE *xd = (const CTYPE *)RTYPE(x);                                                     \
    CTYPE *ansd = (CTYPE *)RTYPE(ans);                                                             \
    if (w==1 || !nthvalue) {                                                                       \
      const int inc = first ? +1 : -1;                                                             \
      for (int g=0; g<ngrp; ++g) {                                                                 \
        const int grpn = grpsize[g];                                                               \
        const int thisn = MIN(w, grpn);                                                            \
        const int jstart = ff[g]-1 + !first*(grpn-1);                                              \
        if (!first) ansi+=thisn-1;                                                                 \
        int read=0, write=0;                                                                       \
        while (write<thisn && read<grpn) {                                                         \
          const int k = issorted ? jstart+inc*read : oo[jstart+inc*read]-1;                        \
          const CTYPE val = nosubset ? xd[k] : (irows[k]==NA_INTEGER ? RNA : xd[irows[k]-1]);      \
          /* ternaries on const-bools isorted and nosubset above assumed branch-predicted and */   \
          /* insignificant inside loop  */                                                         \
          read++;                                                                                  \
          if (narm && ISNAT(val)) continue;                                                        \
          /* const-bool narm and short-circuit-&& means above if() insignificant */                \
          ASSIGN; write++;                                                                         \
        }                                                                                          \
        if (anslens) anslens[g]=write;                                                              \
        const CTYPE val = RNA;                                                                     \
        while (write<thisn) { ASSIGN; write++; }  /* when not enough non-NA pad with NA */         \
        /* if NAs can be removed when narm=TRUE, that's done up in gforce() afterwards when     */ \
        /* we know what the results are from the other gforce columns are to align with. When   */ \
        /* there's just one column, and narm=TRUE too, and there are NAs removed, it would      */ \
        /* save removing the NAs in gforce() by not doing the padding now and just continuing   */ \
        /* to write the next group's results straight after this group. A special case and      */ \
        /* likely only measureable speedup for string columns but relatively easy to do.        */ \
        if (!first) ansi+=write+1; /* we wrote backwards so pass over what we wrote */             \
      }                                                                                            \
    } else if (first) {                                                                            \
      /* gnthvalue */                                                                              \
      const int inc=1;                                                                             \
      for (int g=0; g<ngrp; ++g) {                                                                 \
        const int grpn = grpsize[g];                                                               \
        if (w>grpn || w==0) { const CTYPE val=RNA; ASSIGN; continue; }                             \
        const int j = ff[g]-1+w-1;                                                                 \
        const int k = issorted ? j : oo[j]-1;                                                      \
        const CTYPE val = nosubset ? xd[k] : (irows[k]==NA_INTEGER ? RNA : xd[irows[k]-1]);        \
        ASSIGN;                                                                                    \
      }                                                                                            \
    } else {                                                                                       \
      /* w>1 && !first not supported because -i in R means everything-but-i and gnthvalue */       \
      /* currently takes n>0 only. However, we could still support n'th from the end, somehow */   \
      error(_("Internal error: unanticipated case in gfirstlast first=%d w=%d nthvalue=%d"),       \
              first, w, nthvalue);                                                                 \
    }                                                                                              \
    ansd++; /* just to suppress unused-variable warning in STRSXP and VECSXP cases */              \
  }
  switch(TYPEOF(x)) {
  case LGLSXP:  {
    #undef ISNAT
    #define ISNAT(x) ((x)==NA_INTEGER)
    DO(int,      LOGICAL, NA_LOGICAL,   ansd[ansi]=val; ansi+=inc)
  } break;
  case INTSXP:  {
    #undef ISNAT
    #define ISNAT(x) ((x)==NA_INTEGER)
    DO(int,      INTEGER, NA_INTEGER,   ansd[ansi]=val; ansi+=inc)
  } break;
  case REALSXP: if (INHERITS(x, char_integer64)) {
    #undef ISNAT
    #define ISNAT(x) ((x)==NA_INTEGER64)
    DO(int64_t,  REAL,    NA_INTEGER64, ansd[ansi]=val; ansi+=inc)
    } else {
    #undef ISNAT
    #define ISNAT(x) (ISNAN(x))
    DO(double,   REAL,    NA_REAL,      ansd[ansi]=val; ansi+=inc)
  } break;
  case CPLXSXP: {
    #undef ISNAT
    #define ISNAT(x) (ISNAN_COMPLEX(x))
    DO(Rcomplex, COMPLEX, NA_CPLX,      ansd[ansi]=val; ansi+=inc)
  } break;
  case STRSXP: {
    #undef ISNAT
    #define ISNAT(x) ((x)==NA_STRING)
    DO(SEXP,  STRING_PTR, NA_STRING,    SET_STRING_ELT(ans,ansi,val); ansi+=inc)
  } break;
  case VECSXP: {
    #undef ISNAT
    #define ISNAT(x) (isNull(x) || (isLogical(x) && LENGTH(x)==1 && LOGICAL(x)[0]==NA_LOGICAL))
    DO(SEXP, SEXPPTR_RO, ScalarLogical(NA_LOGICAL), SET_VECTOR_ELT(ans,ansi,val); ansi+=inc)       /* global replace ScalarLogical() with fixed constant R_FalseValue somehow */
  } break;
  default:
    error(_("Type '%s' is not supported by GForce head/tail/first/last/`[`. Either add the namespace prefix (e.g. utils::head(.)) or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)));
  }
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  return(ans);
}

SEXP glast(const SEXP x, const SEXP nArg, const SEXP narmArg) {
  return gfirstlast(x, /*first=*/false, nArg, /*nthvalue=*/false, narmArg);
}

SEXP gfirst(const SEXP x, const SEXP nArg, const SEXP narmArg) {
  return gfirstlast(x, true, nArg, false, narmArg);
}

SEXP gtail(const SEXP x, const SEXP nArg) {
  return gfirstlast(x, false, nArg, false, ScalarLogical(0));
}

SEXP ghead(const SEXP x, const SEXP nArg) {
  return gfirstlast(x, true, nArg, false, ScalarLogical(0));
}

SEXP gnthvalue(const SEXP x, const SEXP nArg) {
  return gfirstlast(x, /*first=*/true, nArg, /*nthvalue=*/true, ScalarLogical(0));
}

// TODO: gwhich.min, gwhich.max
// implemented this similar to gmedian to balance well between speed and memory usage. There's one extra allocation on maximum groups and that's it.. and that helps speed things up extremely since we don't have to collect x's values for each group for each step (mean, residuals, mean again and then variance).
static SEXP gvarsd1(SEXP x, SEXP narmArg, bool isSD)
{
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  if (!isVectorAtomic(x)) error(_("GForce var/sd can only be applied to columns, not .SD or similar. For the full covariance matrix of all items in a list such as .SD, either add the prefix stats::var(.SD) (or stats::sd(.SD)) or turn off GForce optimization using options(datatable.optimize=1). Alternatively, if you only need the diagonal elements, 'DT[,lapply(.SD,var),by=,.SDcols=]' is the optimized way to do this."));
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), isSD ? "sd" : "var");
  const int n = (irowslen == -1) ? length(x) : irowslen;
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gvar");
  SEXP sub, ans = PROTECT(allocVector(REALSXP, ngrp));
  double *ansd = REAL(ans);
  const bool nosubset = irowslen==-1;
  const bool narm = LOGICAL(narmArg)[0];
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    sub = PROTECT(allocVector(INTSXP, maxgrpn)); // allocate once upfront
    int *subd = INTEGER(sub);
    const int *xd = INTEGER(x);
    for (int i=0; i<ngrp; ++i) {
      const int thisgrpsize = grpsize[i];
      if (thisgrpsize==1) {
        ansd[i] = NA_REAL;
      } else {
        int nna = 0;  // how many not-NA
        long double m=0., s=0., v=0.;
        for (int j=0; j<thisgrpsize; ++j) {
          int ix = ff[i]+j-1;
          if (isunsorted) ix = oo[ix]-1;
          if (nosubset ? xd[ix]==NA_INTEGER : (irows[ix]==NA_INTEGER || (ix=irows[ix]-1,xd[ix]==NA_INTEGER))) {
            if (narm) continue; else break;
          }
          m += (subd[nna++]=xd[ix]); // sum
        }
        if (nna!=thisgrpsize && (!narm || nna<=1)) { ansd[i]=NA_REAL; continue; }
        m = m/nna; // mean, first pass
        for (int j=0; j<nna; ++j) s += (subd[j]-m); // residuals
        m += (s/nna); // mean, second pass
        for (int j=0; j<nna; ++j) { // variance
          v += (subd[j]-(double)m) * (subd[j]-(double)m);
        }
        ansd[i] = (double)v/(nna-1);
        if (isSD) ansd[i] = SQRTL(ansd[i]);
      }
    }}
    break;
  case REALSXP: {
    sub = PROTECT(allocVector(REALSXP, maxgrpn)); // allocate once upfront
    double *subd = REAL(sub);
    const double *xd = REAL(x);
    for (int i=0; i<ngrp; ++i) {
      const int thisgrpsize = grpsize[i];
      if (thisgrpsize==1) {
        ansd[i] = NA_REAL;
      } else {
        int nna = 0;  // how many not-NA
        long double m=0., s=0., v=0.;
        for (int j=0; j<thisgrpsize; ++j) {
          int ix = ff[i]+j-1;
          if (isunsorted) ix = oo[ix]-1;
          if (nosubset ? ISNAN(xd[ix]) : (irows[ix]==NA_INTEGER || (ix=irows[ix]-1,ISNAN(xd[ix])))) {
            if (narm) continue; else break;
          }
          m += (subd[nna++]=xd[ix]); // sum
        }
        if (nna!=thisgrpsize && (!narm || nna<=1)) { ansd[i]=NA_REAL; continue; }
        m = m/nna; // mean, first pass
        for (int j=0; j<nna; ++j) s += (subd[j]-m); // residuals
        m += (s/nna); // mean, second pass
        for (int j=0; j<nna; ++j) { // variance
          v += (subd[j]-(double)m) * (subd[j]-(double)m);
        }
        ansd[i] = (double)v/(nna-1);
        if (isSD) ansd[i] = SQRTL(ansd[i]);
      }
    }}
    break;
  default:
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"),
            type2char(TYPEOF(x)), isSD?"sd (gsd)":"var (gvar)", isSD?"stats::sd(.)":"stats::var(.)");
  }
  // no copyMostAttrib(x, ans) since class (e.g. Date) unlikely applicable to sd/var
  UNPROTECT(2); // ans,sub
  return ans;
}

SEXP gvar(SEXP x, SEXP narm) {
  return (gvarsd1(x, narm, FALSE));
}

SEXP gsd(SEXP x, SEXP narm) {
  return (gvarsd1(x, narm, TRUE));
}

SEXP gprod(SEXP x, SEXP narmArg) {
  if (!IS_TRUE_OR_FALSE(narmArg))
    error(_("%s must be TRUE or FALSE"), "na.rm");
  const bool narm=LOGICAL(narmArg)[0];
  if (!isVectorAtomic(x))
    error(_("GForce prod can only be applied to columns, not .SD or similar. To multiply all items in a list such as .SD, either add the prefix base::prod(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,prod),by=,.SDcols=]'"));
  if (inherits(x, "factor"))
    error(_("%s is not meaningful for factors."), "prod");
  const bool nosubset = irowslen==-1;
  const int n = nosubset ? length(x) : irowslen;
  //clock_t start = clock();
  if (nrow != n) error(_("nrow [%d] != length(x) [%d] in %s"), nrow, n, "gprod");
  long double *s = malloc(ngrp * sizeof(long double));
  if (!s) error(_("Unable to allocate %d * %d bytes for gprod"), ngrp, sizeof(long double));
  for (int i=0; i<ngrp; ++i) s[i] = 1.0;
  switch(TYPEOF(x)) {
  case LGLSXP: case INTSXP: {
    const int *xd = INTEGER(x);
    for (int i=0; i<n; ++i) {
      const int thisgrp = grp[i];
      const int elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER : xd[irows[i]-1]);
      if (elem==NA_INTEGER) {
        if (!narm) s[thisgrp] = NA_REAL;  // Let NA_REAL propogate from here. R_NaReal is IEEE.
        continue;
      }
      s[thisgrp] *= elem; // no under/overflow here, s is long double (like base)
    }}
    break;
  case REALSXP: {
    if (INHERITS(x, char_integer64)) {
      const int64_t *xd = (const int64_t *)REAL(x);
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        const int64_t elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_INTEGER64 : xd[irows[i]-1]);
        if (elem==NA_INTEGER64) {
          if (!narm) s[thisgrp] = NA_REAL;
          continue;
        }
        s[thisgrp] *= elem;
      }
    } else {
      const double *xd = REAL(x);
      for (int i=0; i<n; ++i) {
        const int thisgrp = grp[i];
        const double elem = nosubset ? xd[i] : (irows[i]==NA_INTEGER ? NA_REAL : xd[irows[i]-1]);
        if (ISNAN(elem)) {
          if (!narm) s[thisgrp] = NA_REAL;
          continue;
        }
        s[thisgrp] *= elem;
      }
    }
  } break;
  default:
    free(s);
    error(_("Type '%s' is not supported by GForce %s. Either add the prefix %s or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)), "prod (gprod)", "base::prod(.)");
  }
  SEXP ans = PROTECT(allocVector(REALSXP, ngrp));
  if (INHERITS(x, char_integer64)) {
    int64_t *ansd = (int64_t *)REAL(ans);
    for (int i=0; i<ngrp; ++i) {
      ansd[i] = (s[i]>INT64_MAX || s[i]<=INT64_MIN) ? NA_INTEGER64 : (int64_t)s[i];
    }
  } else {
    double *ansd = REAL(ans);
    for (int i=0; i<ngrp; ++i) {
      if (s[i] > DBL_MAX) ansd[i] = R_PosInf;
      else if (s[i] < -DBL_MAX) ansd[i] = R_NegInf;
      else ansd[i] = (double)s[i];
    }
  }
  free(s);
  copyMostAttrib(x, ans);
  UNPROTECT(1);
  // Rprintf(_("this gprod took %8.3f\n"), 1.0*(clock()-start)/CLOCKS_PER_SEC);
  return ans;
}

SEXP gshift(SEXP x, SEXP nArg, SEXP fillArg, SEXP typeArg) {
  const bool nosubset = irowslen == -1;
  const bool issorted = !isunsorted;
  const int n = nosubset ? length(x) : irowslen;
  if (nrow != n) error(_("Internal error: nrow [%d] != length(x) [%d] in %s"), nrow, n, "gshift");

  int nprotect=0;
  enum {LAG, LEAD/*, SHIFT*/,CYCLIC} stype = LAG;
  if (!(length(fillArg) == 1))
    error(_("fill must be a vector of length 1"));

  if (!isString(typeArg) || length(typeArg) != 1)
    error(_("Internal error: invalid type for gshift(), should have been caught before. please report to data.table issue tracker")); // # nocov
  if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "lag")) stype = LAG;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "lead")) stype = LEAD;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "shift")) stype = LAG;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "cyclic")) stype = CYCLIC;
  else error(_("Internal error: invalid type for gshift(), should have been caught before. please report to data.table issue tracker")); // # nocov

  bool lag;
  const bool cycle = stype == CYCLIC;

  R_xlen_t nx = xlength(x), nk = length(nArg);
  if (!isInteger(nArg)) error(_("Internal error: n must be integer")); // # nocov
  const int *kd = INTEGER(nArg);
  for (int i=0; i<nk; i++) if (kd[i]==NA_INTEGER) error(_("Item %d of n is NA"), i+1);

  SEXP ans = PROTECT(allocVector(VECSXP, nk)); nprotect++;
  SEXP att = PROTECT(allocVector(VECSXP, 3)); nprotect++;
  SET_VECTOR_ELT(att, 0, R_NilValue);
  SET_VECTOR_ELT(att, 1, ScalarLogical(true));    // first/last doesn't matter for gshift
  SET_VECTOR_ELT(att, 2, ScalarInteger(INT_MAX)); // i.e. grpsize; TODO: perhaps point lens directly to grpsize instead
  SEXP thisfill = PROTECT(coerceAs(fillArg, x, ScalarLogical(0))); nprotect++;
  for (int g=0; g<nk; g++) {
    lag = stype == LAG || stype == CYCLIC;
    int m = kd[g];
    // switch
    if (m < 0) {
      m = m * (-1);
      lag = !lag;
    }
    R_xlen_t ansi = 0;
    SEXP tmp;
    SET_VECTOR_ELT(ans, g, tmp=allocVector(TYPEOF(x), nx));
    setAttrib(tmp, sym_gforce_dynamic, att);
    #define SHIFT(CTYPE, RTYPE, ASSIGN) {                                                                         \
      const CTYPE *xd = (const CTYPE *)RTYPE(x);                                                                  \
      const CTYPE fill = RTYPE(thisfill)[0];                                                                      \
      for (int i=0; i<ngrp; ++i) {                                                                                \
        const int grpn = grpsize[i];                                                                              \
        const int mg = cycle ? (((m-1) % grpn) + 1) : m;                                                          \
        const int thisn = MIN(mg, grpn);                                                                          \
        const int jstart = ff[i]-1+ (!lag)*(thisn);                                                               \
        const int jend = jstart+ MAX(0, grpn-mg); /*if m > grpn -> jend = jstart */                               \
        if (lag) {                                                                                                \
          const int o = ff[i]-1+(grpn-thisn);                                                                     \
          for (int j=0; j<thisn; ++j) {                                                                           \
          const int k = issorted ? (o+j) : oo[o+j]-1;                                                             \
            const CTYPE val = cycle ? (nosubset ? xd[k] : (irows[k]==NA_INTEGER ? fill : xd[irows[k]-1])) : fill; \
            ASSIGN;                                                                                               \
          }                                                                                                       \
        }                                                                                                         \
        for (int j=jstart; j<jend; ++j) {                                                                         \
          const int k = issorted ? j : oo[j]-1;                                                                   \
          const CTYPE val = nosubset ? xd[k] : (irows[k]==NA_INTEGER ? fill : xd[irows[k]-1]);                    \
          ASSIGN;                                                                                                 \
        }                                                                                                         \
        if (!lag) {                                                                                               \
          const int o = ff[i]-1;                                                                                  \
          for (int j=0; j<thisn; ++j) {                                                                           \
            const int k = issorted ? (o+j) : oo[o+j]-1;                                                           \
            const CTYPE val = cycle ? (nosubset ? xd[k] : (irows[k]==NA_INTEGER ? fill : xd[irows[k]-1])) : fill; \
            ASSIGN;                                                                                               \
          }                                                                                                       \
        }                                                                                                         \
      }                                                                                                           \
    }
    switch(TYPEOF(x)) {
      case RAWSXP:  { Rbyte *ansd=RAW(tmp);               SHIFT(Rbyte,   RAW,       ansd[ansi++]=val); } break;
      case LGLSXP:  { int *ansd=LOGICAL(tmp);             SHIFT(int,     LOGICAL,   ansd[ansi++]=val); } break;
      case INTSXP:  { int *ansd=INTEGER(tmp);             SHIFT(int,     INTEGER,   ansd[ansi++]=val); } break;
      case REALSXP: { double *ansd=REAL(tmp);             SHIFT(double,  REAL,      ansd[ansi++]=val); } break;
                    // integer64 is shifted as if it's REAL; and assigning fill=NA_INTEGER64 is ok as REAL
      case CPLXSXP: { Rcomplex *ansd=COMPLEX(tmp);        SHIFT(Rcomplex, COMPLEX,  ansd[ansi++]=val); } break;
      case STRSXP: { SHIFT(SEXP, STRING_PTR,                          SET_STRING_ELT(tmp,ansi++,val)); } break;
      //case VECSXP: { SHIFT(SEXP, SEXPPTR_RO,                          SET_VECTOR_ELT(tmp,ansi++,val)); } break;
      default:
        error(_("Type '%s' is not supported by GForce gshift. Either add the namespace prefix (e.g. data.table::shift(.)) or turn off GForce optimization using options(datatable.optimize=1)"), type2char(TYPEOF(x)));
    }
    copyMostAttrib(x, tmp); // needed for integer64 because without not the correct class of int64 is assigned
  }
  UNPROTECT(nprotect);
  return(ans);
}

