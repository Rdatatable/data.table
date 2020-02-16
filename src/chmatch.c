#include "data.table.h"

static SEXP chmatchMain(SEXP x, SEXP table, int nomatch, bool chin, bool chmatchdup) {
  if (!isString(table) && !isNull(table))
    error(_("table is type '%s' (must be 'character' or NULL)"), type2char(TYPEOF(table)));
  if (chin && chmatchdup)
    error(_("Internal error: either chin or chmatchdup should be true not both"));  // # nocov
  SEXP sym = NULL;
  const int xlen = length(x);
  if (TYPEOF(x) == SYMSXP) {
    if (xlen!=1)
      error(_("Internal error: length of SYMSXP is %d not 1"), xlen); // # nocov
    sym = PRINTNAME(x);  // so we can do &sym to get a length 1 (const SEXP *)STRING_PTR(x) and save an alloc for coerce to STRSXP
  } else if (!isString(x) && !isSymbol(x) && !isNull(x)) {
    if (chin && !isVectorAtomic(x)) {
      return ScalarLogical(FALSE);
      // commonly type 'language' returns FALSE here, to make %iscall% simpler; e.g. #1369 results in (function(x) sum(x)) as jsub[[.]] from dcast.data.table
    } else {
      error(_("x is type '%s' (must be 'character' or NULL)"), type2char(TYPEOF(x)));
    }
  }
  // allocations up front before savetl starts in case allocs fail
  SEXP ans = PROTECT(allocVector(chin?LGLSXP:INTSXP, xlen));
  if (xlen==0) { // no need to look at table when x is empty (including null)
    UNPROTECT(1);
    return ans;
  }
  int *ansd = INTEGER(ans);
  const int tablelen = length(table);
  if (tablelen==0) {
    const int val=(chin?0:nomatch), n=xlen;
    for (int i=0; i<n; ++i) ansd[i]=val;
    UNPROTECT(1);
    return ans;
  }
  // Since non-ASCII strings may be marked with different encodings, it only make sense to compare
  // the bytes under a same encoding (UTF-8) #3844 #3850
  const SEXP *xd = isSymbol(x) ? &sym : STRING_PTR(PROTECT(coerceUtf8IfNeeded(x)));
  const SEXP *td = STRING_PTR(PROTECT(coerceUtf8IfNeeded(table)));
  const int nprotect = 2 + !isSymbol(x); // ans, xd, td
  if (xlen==1) {
    ansd[0] = nomatch;
    for (int i=0; i<tablelen; ++i) {
      if (td[i]==xd[0]) {
        ansd[0] = chin ? 1 : i+1;
        break; // short-circuit early; if there are dups in table the first is returned
      }
    }
    UNPROTECT(nprotect);
    return ans;
  }
  // else xlen>1; nprotect is const above since no more R allocations should occur after this point
  savetl_init();
  for (int i=0; i<xlen; i++) {
    SEXP s = xd[i];
    const int tl = TRUELENGTH(s);
    if (tl>0) {
      savetl(s);  // R's internal hash (which is positive); save it
      SET_TRUELENGTH(s,0);
    } else if (tl<0) {
      // R 2.14.0+ initializes truelength to 0 (before that it was uninitialized/random).
      // Now that data.table depends on R 3.1.0+, that is after 2.14.0 too.
      // We rely on that 0-initialization, and that R's internal hash is positive.
      // # nocov start
      savetl_end();
      error(_("Internal error: CHARSXP '%s' has a negative truelength (%d). Please file an issue on the data.table tracker."), CHAR(s), tl);
      // # nocov end
    }
  }
  int nuniq=0;
  for (int i=0; i<tablelen; ++i) {
    SEXP s = td[i];
    int tl = TRUELENGTH(s);
    if (tl>0) { savetl(s); tl=0; }
    if (tl==0) SET_TRUELENGTH(s, chmatchdup ? -(++nuniq) : -i-1); // first time seen this string in table
  }
  if (chmatchdup) {
    // chmatchdup() is basically base::pmatch() but without the partial matching part. For example :
    //   chmatchdup(c("a", "a"), c("a", "a"))   # 1,2  - the second 'a' in 'x' has a 2nd match in 'table'
    //   chmatchdup(c("a", "a"), c("a", "b"))   # 1,NA - the second one doesn't 'see' the first 'a'
    //   chmatchdup(c("a", "a"), c("a", "a.1")) # 1,NA - differs from 'pmatch' output = 1,2
    // Used to be called chmatch2 before v1.12.2 and was in rbindlist.c. New implementation from 1.12.2 here in chmatch.c
    // If nuniq==tablelen then there are no dups in table but we still need this branch because there might be dups
    //   in x and the 2nd onwards of any dup needs to return NA
    // See end of file for benchmark
    //                                                                                        uniq         dups
    // For example: A,B,C,B,D,E,A,A   =>   A(TL=1),B(2),C(3),D(4),E(5)   =>   dupMap    1  2  3  5  6 | 8  7  4
    //                                                                        dupLink   7  8          |    6     (blank=0)
    int *counts = (int *)calloc(nuniq, sizeof(int));
    int *map =    (int *)calloc(tablelen+nuniq, sizeof(int));  // +nuniq to store a 0 at the end of each group
    if (!counts || !map) {
      // # nocov start
      for (int i=0; i<tablelen; i++) SET_TRUELENGTH(td[i], 0);
      savetl_end();
      error(_("Failed to allocate %"PRIu64" bytes working memory in chmatchdup: length(table)=%d length(unique(table))=%d"), ((uint64_t)tablelen*2+nuniq)*sizeof(int), tablelen, nuniq);
      // # nocov end
    }
    for (int i=0; i<tablelen; ++i) counts[-TRUELENGTH(td[i])-1]++;
    for (int i=0, sum=0; i<nuniq; ++i) { int tt=counts[i]; counts[i]=sum; sum+=tt+1; }
    for (int i=0; i<tablelen; ++i) map[counts[-TRUELENGTH(td[i])-1]++] = i+1;           // 0 is left ending each group thanks to the calloc
    for (int i=0, last=0; i<nuniq; ++i) {int tt=counts[i]+1; counts[i]=last; last=tt;}  // rewind counts to the beginning of each group
    for (int i=0; i<xlen; ++i) {
      int u = TRUELENGTH(xd[i]);
      if (u<0) {
        int w = counts[-u-1]++;
        if (map[w]) { ansd[i]=map[w]; continue; }
        SET_TRUELENGTH(xd[i],0); // w falls on ending 0 marker: dups used up; any more dups should return nomatch
        // we still need the 0-setting loop at the end of this function because often there will be some values in table that are not matched to at all.
      }
      ansd[i] = nomatch;
    }
    free(counts);
    free(map);
  } else if (chin) {
    for (int i=0; i<xlen; i++) {
      ansd[i] = TRUELENGTH(xd[i])<0;
    }
  } else {
    for (int i=0; i<xlen; i++) {
      int m = TRUELENGTH(xd[i]);
      ansd[i] = (m<0) ? -m : nomatch;
    }
  }
  for (int i=0; i<tablelen; i++)
    SET_TRUELENGTH(td[i], 0);  // reinstate 0 rather than leave the -i-1
  savetl_end();
  UNPROTECT(nprotect);  // ans, xd, td
  return ans;
}

// for internal use from C :
SEXP chmatch(SEXP x, SEXP table, int nomatch) {  // chin=  chmatchdup=
  return chmatchMain(x, table, nomatch,             false, false);
}
SEXP chin(SEXP x, SEXP table) {
  return chmatchMain(x, table, 0,                   true,  false);
}

// for use from internals at R level; chmatch and chin are exported too but not chmatchdup yet
SEXP chmatch_R(SEXP x, SEXP table, SEXP nomatch) {
  return chmatchMain(x, table, INTEGER(nomatch)[0], false, false);
}
SEXP chin_R(SEXP x, SEXP table) {
  return chmatchMain(x, table, 0,                   true,  false);
}
SEXP chmatchdup_R(SEXP x, SEXP table, SEXP nomatch) {
  return chmatchMain(x, table, INTEGER(nomatch)[0], false, true);
}

/*
## Benchmark moved here in v1.12.2 from rbindlist.c
set.seed(45L)
x <- sample(letters, 1e6, TRUE)
y <- sample(letters, 1e7, TRUE)
system.time(ans0 <- base::pmatch(x,y,0L))           # over 5 minutes as of R 3.5.3 (March 2019)
system.time(ans1 <- .Call("Cchmatch2_old", x,y,0L)) # 2.40sec  many years old
system.time(ans2 <- .Call("Cchmatch2", x,y,0L))     # 0.17sec  as of 1.12.0 and in place for several years before that
system.time(ans3 <- chmatchdup(x,y,0L))             # 0.09sec  from 1.12.2; but goal wasn't speed rather simplified code; e.g. rbindlist.c down from 960 to 360 lines
identical(ans2,ans3)  # test 2000
*/

