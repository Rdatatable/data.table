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
  if (chin && chmatchdup) error("Internal error: either chin or chmatchdup should be true not both");
  // allocations up front before savetl starts
  SEXP ans = PROTECT(allocVector(chin?LGLSXP:INTSXP, length(x)));
  int *ansd = INTEGER(ans);
  int *dupMap=NULL, *dupLink=NULL;
  if (chmatchdup) {
    dupMap  = (int *)R_alloc(length(table), sizeof(int));
    dupLink = (int *)R_alloc(length(table), sizeof(int));
  }
  // end allocations
  savetl_init();
  const SEXP *xd = STRING_PTR(x);
  const int xlen = length(x);
  for (int i=0; i<xlen; i++) {
    SEXP s = STRING_ELT(x,i);
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
    SET_TRUELENGTH(s,0);
  }
  const int tablelen = length(table);
  const SEXP *td = STRING_PTR(table);
  int nuniq=0;
  for (int i=0; i<tablelen; ++i) {
    SEXP s = STRING_ELT(table,i);
    if (s != NA_STRING && ENC_KNOWN(s) != 64) { // changed !ENC_KNOWN(s) to !ASCII(s) - check above for explanation
      for (int j=0; j<i; ++j) SET_TRUELENGTH(STRING_ELT(table,j),0);  // reinstate 0 rather than leave the -i-1
      savetl_end();
      UNPROTECT(1);
      return (chin ? match_logical(table, x) : match(table, x, nomatch));
    }
    int tl = TRUELENGTH(s);
    if (tl>0) { savetl(s); tl=0; }
    if (tl==0) SET_TRUELENGTH(s, chmatchdup ? -(++nuniq) : -i-1); // first time seen this string in table
  }
  if (chmatchdup && nuniq<tablelen) {
    // used to be called chmatch2 before v1.12.2. New implementation from 1.12.2 too        uniq          dups
    // For example: A,B,C,B,D,E,A,A   =>   A(TL=1),B(2),C(3),D(4),E(5)   =>   dupMap    1  2  3  5  6 | 8  7  4
    //                                                                        dupLink   7  8          |    6     (blank=0)
    memset(dupMap,  0, tablelen*sizeof(int));
    memset(dupLink, 0, tablelen*sizeof(int));
    int dupAtEnd=tablelen;
    for (int i=0; i<tablelen; ++i) {
      int u = -TRUELENGTH(td[i]);  // 1-based
      while (dupLink[u-1]) u = dupLink[u-1];
      if (dupMap[u-1]==0) dupMap[u-1]=i+1;  // first time seen this uniq
      else {
        dupMap[dupAtEnd-1] = i+1;
        dupLink[u-1] = dupAtEnd--;
      }
    }
    for (int i=0; i<xlen; ++i) {
      int u = TRUELENGTH(xd[i]);
      if (u<0) {
        ansd[i] = dupMap[-u-1];
        SET_TRUELENGTH(xd[i], -dupLink[-u-1]);
        // sets to 0 after last dup matched so more dups of "a" in x than are in table will only match as many dups as there are in table
        // we still need the 0-setting loop below because often there will be some values in table that are not matched to at all.
      } else {
        ansd[i] = nomatch;
      }
    }
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

