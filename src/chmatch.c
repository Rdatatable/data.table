#include "data.table.h"

static SEXP chmatchMain(SEXP x, SEXP table, int nomatch, bool chin, bool chmatchdup) {
  if (!isString(table) && !isNull(table))
    error(_("table is type '%s' (must be 'character' or NULL)"), type2char(TYPEOF(table)));
  if (chin && chmatchdup)
    internal_error(__func__, "either chin or chmatchdup should be true not both");  // # nocov
  SEXP sym = NULL;
  const int xlen = length(x);
  if (TYPEOF(x) == SYMSXP) {
    if (xlen!=1)
      internal_error(__func__, "length of SYMSXP is %d not 1", xlen); // # nocov
    sym = PRINTNAME(x);  // so we can do &sym to get a length 1 (const SEXP *)STRING_PTR_RO(x) and save an alloc for coerce to STRSXP
  } else if (!isString(x) && !isSymbol(x) && !isNull(x)) {
    if (chin && !isVectorAtomic(x)) {
      return ScalarLogical(FALSE);
      // commonly type 'language' returns FALSE here, to make %iscall% simpler; e.g. #1369 results in (function(x) sum(x)) as jsub[[.]] from dcast.data.table
    } else {
      error(_("x is type '%s' (must be 'character' or NULL)"), type2char(TYPEOF(x)));
    }
  }
  // allocations up front before savetl starts in case allocs fail
  int nprotect=0;
  SEXP ans = PROTECT(allocVector(chin?LGLSXP:INTSXP, xlen)); nprotect++;
  if (xlen==0) { // no need to look at table when x is empty (including null)
    UNPROTECT(nprotect);
    return ans;
  }
  int *ansd = INTEGER(ans);
  const int tablelen = length(table);
  if (tablelen==0) {
    const int val=(chin?0:nomatch), n=xlen;
    for (int i=0; i<n; ++i) ansd[i]=val;
    UNPROTECT(nprotect);
    return ans;
  }
  // Since non-ASCII strings may be marked with different encodings, it only make sense to compare
  // the bytes under a same encoding (UTF-8) #3844 #3850.
  const SEXP *xd;
  if (isSymbol(x)) {
    xd = &sym;
  } else {
    xd = (SEXP *)STRING_PTR_RO(PROTECT(coerceUtf8IfNeeded(x))); nprotect++;
  }
  const SEXP *td = STRING_PTR_RO(PROTECT(coerceUtf8IfNeeded(table))); nprotect++;
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
  // Else xlen > 1.
  // When table >> x, hash x and scan table // ToDo tune the kick-in factor
  if (!chmatchdup && tablelen > 2 * xlen) {
    hashtab *marks = hash_create(xlen);
    PROTECT(marks->prot); nprotect++;
    int nuniq = 0;
    for (int i = 0; i < xlen; ++i) {
      // todo use lookup_insert?
      int tl = hash_lookup(marks, xd[i], 0);
      if (tl == 0) {
        hash_set(marks, xd[i], -1);
        nuniq++;
      }
    }

    for (int i = 0; i < tablelen; ++i) {
      int tl = hash_lookup(marks, td[i], 0);
      if (tl == -1) {
        hash_set(marks, td[i], i + 1);
        nuniq--;
        if (nuniq == 0) break; // all found, stop scanning
      }
    }

    if (chin) {
      #pragma omp parallel for num_threads(getDTthreads(xlen, true))
      for (int i = 0; i < xlen; ++i) {
        ansd[i] = hash_lookup(marks, xd[i], 0) > 0;
      }
    } else {
      #pragma omp parallel for num_threads(getDTthreads(xlen, true))
      for (int i = 0; i < xlen; ++i) {
        const int m = hash_lookup(marks, xd[i], 0);
        ansd[i] = (m < 0) ? nomatch : m;
      }
    }
    UNPROTECT(nprotect);
    return ans;
  }
  hashtab * marks = hash_create(tablelen);
  PROTECT(marks->prot); nprotect++;
  int nuniq=0;
  for (int i=0; i<tablelen; ++i) {
    const SEXP s = td[i];
    int tl = hash_lookup(marks, s, 0);
    if (tl==0) hash_set(marks, s, chmatchdup ? -(++nuniq) : -i-1); // first time seen this string in table
  }
  // in future if we need NAs in x not to be matched to NAs in table ...
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
    unsigned int mapsize = tablelen+nuniq; // lto compilation warning #5760 // +nuniq to store a 0 at the end of each group
    void *vmax = vmaxget();
    int *counts = (int *)R_alloc(nuniq, sizeof(*counts));
    if (nuniq)   memset(counts, 0, sizeof(*counts) * nuniq);
    int *map    = (int *)R_alloc(mapsize, sizeof(*map));
    if (mapsize) memset(map,    0, sizeof(*map)    * mapsize);
    for (int i=0; i<tablelen; ++i) counts[-hash_lookup(marks, td[i], 0)-1]++;
    for (int i=0, sum=0; i<nuniq; ++i) { int tt=counts[i]; counts[i]=sum; sum+=tt+1; }
    for (int i=0; i<tablelen; ++i) map[counts[-hash_lookup(marks, td[i], 0)-1]++] = i+1;           // 0 is left ending each group thanks to the memset
    for (int i=0, last=0; i<nuniq; ++i) {int tt=counts[i]+1; counts[i]=last; last=tt;}  // rewind counts to the beginning of each group
    for (int i=0; i<xlen; ++i) {
      int u = hash_lookup(marks, xd[i], 0);
      if (u<0) {
        const int w = counts[-u-1]++;
        if (map[w]) { ansd[i]=map[w]; continue; }
        hash_set(marks,xd[i],0); // w falls on ending 0 marker: dups used up; any more dups should return nomatch
      }
      ansd[i] = nomatch;
    }
    vmaxset(vmax);
  } else if (chin) {
    #pragma omp parallel for num_threads(getDTthreads(xlen, true))
    for (int i=0; i<xlen; i++) {
      ansd[i] = hash_lookup(marks,xd[i],0)<0;
    }
  } else {
    #pragma omp parallel for num_threads(getDTthreads(xlen, true))
    for (int i=0; i<xlen; i++) {
      const int m = hash_lookup(marks,xd[i],0);
      ansd[i] = (m<0) ? -m : nomatch;
    }
  }
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
