#include "data.table.h"

#define ENC_KNOWN(x) (LEVELS(x) & 76)
// LATIN1_MASK (1<<2) | UTF8_MASK (1<<3) | ASCII_MASK (1<<6)

static SEXP match_logical(SEXP table, SEXP x) {
  R_len_t i;
  SEXP ans, m;
  ans = PROTECT(allocVector(LGLSXP, length(x)));
  m = PROTECT(match(table, x, 0)); // nomatch=0
  for (i=0; i<length(x); i++)
    INTEGER(ans)[i] = INTEGER(m)[i] > 0;
  UNPROTECT(2);
  return(ans);
}

static SEXP chmatchMain(SEXP x, SEXP table, int nomatch, bool chin, bool chmatchdup) {
  if (!isString(x) && !isNull(x)) error("x is type '%s' (must be 'character' or NULL)", type2char(TYPEOF(x)));
  if (!isString(table) && !isNull(table)) error("table is type '%s' (must be 'character' or NULL)", type2char(TYPEOF(table)));
  if (chin && chmatchdup) error("Internal error: either chin or chmatchdup should be true not both");  // # nocov
  // allocations up front before savetl starts
  SEXP ans = PROTECT(allocVector(chin?LGLSXP:INTSXP, length(x)));
  if (!length(x)) { UNPROTECT(1); return ans; }  // no need to look at table when x is empty
  int *ansd = INTEGER(ans);
  if (!length(table)) { const int val=(chin?0:nomatch), n=LENGTH(x); for (int i=0; i<n; ++i) ansd[i]=val; UNPROTECT(1); return ans; }
  savetl_init();
  const SEXP *xd = STRING_PTR(x);
  const int xlen = length(x);
  for (int i=0; i<xlen; i++) {
    SEXP s = xd[i];
    if (s != NA_STRING && ENC_KNOWN(s) != 64) { // PREV: s != NA_STRING && !ENC_KNOWN(s) - changed to fix for bug #5159. The previous fix
                       // dealt with UNKNOWN encodings. But we could have the same string, where both are in different
                       // encodings than ASCII (ex: UTF8 and Latin1). To fix this, we'll to resort to 'match' if not ASCII.
                       // This takes care of all anomalies. It's unfortunate the 'chmatch' can't be used in these cases...
                       // // fix for the 2nd part of bug #5159

      // explanation for PREV case: (still useful to keep it here)
      // symbols (i.e. as.name() or as.symbol()) containing non-ascii characters (>127) seem to be 'unknown' encoding in R.
      // The same string in different encodings (where unknown encoding is different to known, too) are different
      // CHARSXP pointers; this chmatch relies on pointer equality only. We tried mkChar(CHAR(s)) [ what install() does ]
      // but this impacted the fastest cases too much. Hence fall back to match() on the first unknown encoding detected
      // since match() considers the same string in different encodings as equal (but slower). See #2538 and #4818.
      savetl_end();
      UNPROTECT(1);
      return (chin ? match_logical(table, x) : match(table, x, nomatch));
    }
    if (TRUELENGTH(s)>0) savetl(s);
    // as from v1.8.0 we assume R's internal hash is positive. So in R < 2.14.0 we
    // don't save the uninitialised truelengths that by chance are negative, but
    // will save if positive. Hence R >= 2.14.0 may be faster and preferred now that R
    // initializes truelength to 0 from R 2.14.0.
    SET_TRUELENGTH(s,0);   // TODO: do we need to set to zero first (we can rely on R 3.1.0 now)?
  }
  const int tablelen = length(table);
  const SEXP *td = STRING_PTR(table);
  int nuniq=0;
  for (int i=0; i<tablelen; ++i) {
    SEXP s = td[i];
    if (s != NA_STRING && ENC_KNOWN(s) != 64) { // changed !ENC_KNOWN(s) to !ASCII(s) - check above for explanation
      // This branch is now covered by tests 2004.*. However, is this branch redundant? It means there were no non-ascii encodings
      // in x since the fallback above to match() didn't happen when x was checked. If that was the case in x, then it means none of the
      // x values can match to table anyway. Can't we just drop this branch then? (TODO)
      for (int j=0; j<i; ++j) SET_TRUELENGTH(td[j],0);  // reinstate 0 rather than leave the -i-1
      savetl_end();
      UNPROTECT(1);
      return (chin ? match_logical(table, x) : match(table, x, nomatch));
    }
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
      error("Failed to allocate %lld bytes working memory in chmatchdup: length(table)=%d length(unique(table))=%d", (tablelen*2+nuniq)*sizeof(int), tablelen, nuniq);
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
  UNPROTECT(1);
  return(ans);
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

