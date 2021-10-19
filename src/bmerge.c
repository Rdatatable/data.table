#include "data.table.h"

/*
Implements binary search (a.k.a. divide and conquer).
http://en.wikipedia.org/wiki/Binary_search
http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary
http://googleresearch.blogspot.com/2006/06/extra-extra-read-all-about-it-nearly.html
Differences over standard binary search (e.g. bsearch in stdlib.h) :
  o list of vectors (key of many columns) of different types
  o ties (groups)
  o NA,NAN,-Inf,+Inf are distinct values and can be joined to
  o type double is joined within tolerance (apx 11 s.f.) according to setNumericRounding (default off)
  o join to prevailing value (roll join a.k.a locf), forwards or backwards
  o join to nearest
  o roll the beginning and end optionally
  o limit the roll distance to a user provided value
  o non equi joins (no != yet) since 1.9.8
*/

#define ENC_KNOWN(x) (LEVELS(x) & 12)
// 12 = LATIN1_MASK (1<<2) | UTF8_MASK (1<<3)  // Would use these definitions from Defn.h, but that appears to be private to R. Hence 12.

#define EQ 1
#define LE 2
#define LT 3
#define GE 4
#define GT 5

static const SEXP *idtVec, *xdtVec;
static const int *icols, *xcols;
static bool *isI64;
static SEXP nqgrp;
static int ncol, *o, *xo, *retFirst, *retLength, *retIndex, *allLen1, *allGrp1, *rollends, ilen, anslen;
static int *op, nqmaxgrp;
static int ctr, nomatch; // populating matches for non-equi joins
enum {ALL, FIRST, LAST} mult = ALL;
static double roll, rollabs;
static Rboolean rollToNearest=FALSE;
#define XIND(i) (xo ? xo[(i)]-1 : i)

void bmerge_r(int xlowIn, int xuppIn, int ilowIn, int iuppIn, int col, int thisgrp, const bool lowmax, const bool uppmax);

SEXP bmerge(SEXP idt, SEXP xdt, SEXP icolsArg, SEXP xcolsArg, SEXP isorted, SEXP xoArg, SEXP rollarg, SEXP rollendsArg, SEXP nomatchArg, SEXP multArg, SEXP opArg, SEXP nqgrpArg, SEXP nqmaxgrpArg) {
  int xN, iN, protecti=0;
  ctr=0; // needed for non-equi join case
  SEXP retFirstArg, retLengthArg, retIndexArg, allLen1Arg, allGrp1Arg;
  retFirstArg = retLengthArg = retIndexArg = R_NilValue; // suppress gcc msg

  // iArg, xArg, icolsArg and xcolsArg
  idtVec = SEXPPTR_RO(idt);  // set globals so bmerge_r can see them.
  xdtVec = SEXPPTR_RO(xdt);
  if (!isInteger(icolsArg)) error(_("Internal error: icols is not integer vector")); // # nocov
  if (!isInteger(xcolsArg)) error(_("Internal error: xcols is not integer vector")); // # nocov
  if ((LENGTH(icolsArg)==0 || LENGTH(xcolsArg)==0) && LENGTH(idt)>0) // We let through LENGTH(i) == 0 for tests 2126.*
    error(_("Internal error: icols and xcols must be non-empty integer vectors."));
  if (LENGTH(icolsArg) > LENGTH(xcolsArg)) error(_("Internal error: length(icols) [%d] > length(xcols) [%d]"), LENGTH(icolsArg), LENGTH(xcolsArg)); // # nocov
  icols = INTEGER(icolsArg);
  xcols = INTEGER(xcolsArg);
  xN = LENGTH(xdt) ? LENGTH(VECTOR_ELT(xdt,0)) : 0;
  iN = ilen = anslen = LENGTH(idt) ? LENGTH(VECTOR_ELT(idt,0)) : 0;
  ncol = LENGTH(icolsArg);    // there may be more sorted columns in x than involved in the join
  isI64 = (bool *)R_alloc(ncol, sizeof(bool));
  for(int col=0; col<ncol; col++) {
    if (icols[col]==NA_INTEGER) error(_("Internal error. icols[%d] is NA"), col); // # nocov
    if (xcols[col]==NA_INTEGER) error(_("Internal error. xcols[%d] is NA"), col); // # nocov
    if (icols[col]>LENGTH(idt) || icols[col]<1) error(_("icols[%d]=%d outside range [1,length(i)=%d]"), col, icols[col], LENGTH(idt));
    if (xcols[col]>LENGTH(xdt) || xcols[col]<1) error(_("xcols[%d]=%d outside range [1,length(x)=%d]"), col, xcols[col], LENGTH(xdt));
    SEXP ic = VECTOR_ELT(idt, icols[col]-1);
    SEXP xc = VECTOR_ELT(xdt, xcols[col]-1);
    int it = TYPEOF(ic);
    int xt = TYPEOF(xc);
    if (iN && it!=xt) error(_("typeof x.%s (%s) != typeof i.%s (%s)"), CHAR(STRING_ELT(getAttrib(xdt,R_NamesSymbol),xcols[col]-1)), type2char(xt), CHAR(STRING_ELT(getAttrib(idt,R_NamesSymbol),icols[col]-1)), type2char(it));
    if (iN && it!=LGLSXP && it!=INTSXP && it!=REALSXP && it!=STRSXP)
      error(_("Type '%s' is not supported for joining/merging"), type2char(it));
    isI64[col] = INHERITS(xc, char_integer64);
    if (iN && isI64[col]!=INHERITS(ic, char_integer64))
      error(_("typeof x.%s (%s) != typeof i.%s (%s)"), CHAR(STRING_ELT(getAttrib(xdt,R_NamesSymbol),xcols[col]-1)), isI64[col]?"integer64":"double",
                                                       CHAR(STRING_ELT(getAttrib(idt,R_NamesSymbol),icols[col]-1)), isI64[col]?"double":"integer64");
  }

  // rollArg, rollendsArg
  roll = 0.0; rollToNearest = FALSE;
  if (isString(rollarg)) {
    if (strcmp(CHAR(STRING_ELT(rollarg,0)),"nearest") != 0) error(_("roll is character but not 'nearest'"));
    if (ncol>0 && TYPEOF(VECTOR_ELT(idt, icols[ncol-1]-1))==STRSXP) error(_("roll='nearest' can't be applied to a character column, yet."));
    roll=1.0; rollToNearest=TRUE;       // the 1.0 here is just any non-0.0, so roll!=0.0 can be used later
  } else {
    if (!isReal(rollarg)) error(_("Internal error: roll is not character or double")); // # nocov
    roll = REAL(rollarg)[0];   // more common case (rolling forwards or backwards) or no roll when 0.0
  }
  rollabs = fabs(roll);
  if (!isLogical(rollendsArg) || LENGTH(rollendsArg) != 2)
    error(_("rollends must be a length 2 logical vector"));
  rollends = LOGICAL(rollendsArg);

  if (isNull(nomatchArg)) {
    nomatch=0;
  } else {
    if (length(nomatchArg)!=1 || (!isLogical(nomatchArg) && !isInteger(nomatchArg)))
      error(_("Internal error: nomatchArg must be NULL or length-1 logical/integer")); // # nocov
    nomatch = INTEGER(nomatchArg)[0];
    if (nomatch!=NA_INTEGER && nomatch!=0)
      error(_("Internal error: nomatchArg must be NULL, NA, NA_integer_ or 0L")); // # nocov
  }

  // mult arg
  if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "all")) mult = ALL;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "first")) mult = FIRST;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "last")) mult = LAST;
  else error(_("Internal error: invalid value for 'mult'. please report to data.table issue tracker")); // # nocov

  // opArg
  if (!isInteger(opArg) || length(opArg)!=ncol)
    error(_("Internal error: opArg is not an integer vector of length equal to length(on)")); // # nocov
  op = INTEGER(opArg);
  for (int i=0; i<ncol; ++i) {
    // check up front to avoid default: cases in non-equi switches which may be in parallel regions which could not call error()
    if (op[i]<EQ/*1*/ || op[i]>GT/*5*/)
      error(_("Internal error in bmerge_r for x.'%s'. Unrecognized value op[col]=%d"), // # nocov
              CHAR(STRING_ELT(getAttrib(xdt,R_NamesSymbol),xcols[i]-1)), op[i]);       // # nocov
    if (op[i]!=EQ && TYPEOF(xdtVec[xcols[i]-1])==STRSXP)
      error(_("Only '==' operator is supported for columns of type character."));      // # nocov
  }

  if (!isInteger(nqgrpArg))
    error(_("Internal error: nqgrpArg must be an integer vector")); // # nocov
  nqgrp = nqgrpArg; // set global for bmerge_r
  const int scols = (!length(nqgrpArg)) ? 0 : -1; // starting col index, -1 is external group column for non-equi join case

  // nqmaxgrpArg
  if (!isInteger(nqmaxgrpArg) || length(nqmaxgrpArg) != 1 || INTEGER(nqmaxgrpArg)[0] <= 0)
    error(_("Intrnal error: nqmaxgrpArg is not a positive length-1 integer vector")); // # nocov
  nqmaxgrp = INTEGER(nqmaxgrpArg)[0];
  if (nqmaxgrp>1 && mult == ALL) {
    // non-equi case with mult=ALL, may need reallocation
    anslen = 1.1 * ((iN > 1000) ? iN : 1000);
    retFirst = Calloc(anslen, int); // anslen is set above
    retLength = Calloc(anslen, int);
    retIndex = Calloc(anslen, int);
    if (retFirst==NULL || retLength==NULL || retIndex==NULL)
      error(_("Internal error in allocating memory for non-equi join")); // # nocov
    // initialise retIndex here directly, as next loop is meant for both equi and non-equi joins
    for (int j=0; j<anslen; j++) retIndex[j] = j+1;
  } else { // equi joins (or) non-equi join but no multiple matches
    retFirstArg = PROTECT(allocVector(INTSXP, anslen));
    retFirst = INTEGER(retFirstArg);
    retLengthArg = PROTECT(allocVector(INTSXP, anslen)); // TODO: no need to allocate length at all when
    retLength = INTEGER(retLengthArg);                   // mult = "first" / "last"
    retIndexArg = PROTECT(allocVector(INTSXP, 0));
    retIndex = INTEGER(retIndexArg);
    protecti += 3;
  }
  for (int j=0; j<anslen; j++) {
    // defaults need to populated here as bmerge_r may well not touch many locations, say if the last row of i is before the first row of x.
    retFirst[j] = nomatch;   // default to no match for NA goto below
    // retLength[j] = 0;   // TO DO: do this to save the branch below and later branches at R level to set .N to 0
    retLength[j] = nomatch==0 ? 0 : 1;
  }

  // allLen1Arg
  allLen1Arg = PROTECT(allocVector(LGLSXP, 1));
  allLen1 = LOGICAL(allLen1Arg);
  allLen1[0] = TRUE;  // All-0 and All-NA are considered all length 1 according to R code currently. Really, it means any(length>1).

  // allGrp1Arg, if TRUE, out of all nested group ids, only one of them matches 'x'. Might be rare, but helps to be more efficient in that case.
  allGrp1Arg = PROTECT(allocVector(LGLSXP, 1));
  allGrp1 = LOGICAL(allGrp1Arg);
  allGrp1[0] = TRUE;
  protecti += 2;

  // isorted arg
  o = NULL;
  if (!LOGICAL(isorted)[0]) {
    SEXP order = PROTECT(allocVector(INTSXP, length(icolsArg)));
    protecti++;
    for (int j=0; j<LENGTH(order); j++) INTEGER(order)[j]=1;   // rep(1L, length(icolsArg))
    SEXP oSxp = PROTECT(forder(idt, icolsArg, ScalarLogical(FALSE), ScalarLogical(TRUE), order, ScalarLogical(FALSE)));
    protecti++;
    // TODO - split head of forder into C-level callable
    if (!LENGTH(oSxp)) o = NULL; else o = INTEGER(oSxp);
  }

  // xo arg
  xo = NULL;
  if (length(xoArg)) {
    if (!isInteger(xoArg)) error(_("Internal error: xoArg is not an integer vector")); // # nocov
    xo = INTEGER(xoArg);
  }

  // start bmerge
  if (iN) {
    // embarassingly parallel if we've storage space for nqmaxgrp*iN
    for (int kk=0; kk<nqmaxgrp; kk++) {
      bmerge_r(-1,xN,-1,iN,scols,kk+1,true,true);
    }
  }
  ctr += iN;
  if (nqmaxgrp > 1 && mult == ALL) {
    // memcpy ret* to SEXP
    retFirstArg = PROTECT(allocVector(INTSXP, ctr));
    retLengthArg = PROTECT(allocVector(INTSXP, ctr));
    retIndexArg = PROTECT(allocVector(INTSXP, ctr));
    protecti += 3;
    memcpy(INTEGER(retFirstArg), retFirst, sizeof(int)*ctr);
    memcpy(INTEGER(retLengthArg), retLength, sizeof(int)*ctr);
    memcpy(INTEGER(retIndexArg), retIndex, sizeof(int)*ctr);
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 5)); protecti++;
  SEXP ansnames = PROTECT(allocVector(STRSXP, 5)); protecti++;
  SET_VECTOR_ELT(ans, 0, retFirstArg);
  SET_VECTOR_ELT(ans, 1, retLengthArg);
  SET_VECTOR_ELT(ans, 2, retIndexArg);
  SET_VECTOR_ELT(ans, 3, allLen1Arg);
  SET_VECTOR_ELT(ans, 4, allGrp1Arg);
  SET_STRING_ELT(ansnames, 0, char_starts);  // changed from mkChar to char_ to pass the grep in CRAN_Release.cmd
  SET_STRING_ELT(ansnames, 1, char_lens);
  SET_STRING_ELT(ansnames, 2, char_indices);
  SET_STRING_ELT(ansnames, 3, char_allLen1);
  SET_STRING_ELT(ansnames, 4, char_allGrp1);
  setAttrib(ans, R_NamesSymbol, ansnames);
  if (nqmaxgrp > 1 && mult == ALL) {
    Free(retFirst);
    Free(retLength);
    Free(retIndex);
  }
  UNPROTECT(protecti);
  return (ans);
}

void bmerge_r(int xlowIn, int xuppIn, int ilowIn, int iuppIn, int col, int thisgrp, const bool lowmax, const bool uppmax)
// col is >0 and <=ncol-1 if this range of [xlow,xupp] and [ilow,iupp] match up to but not including that column
// lowmax/uppmax is true if xlowIn/xuppIn is the bound of this group. Needed for roll because it recurses into the series
// and needs to know if the current extents are at the boundaries for rollends.
// col starts with -1 for non-equi joins, which gathers rows from nested id group counter 'thisgrp'
{
  int xlow=xlowIn, xupp=xuppIn, ilow=ilowIn, iupp=iuppIn;
  const bool isDataCol = col>-1; // check once for non nq join grp id internal technical, non-data, field
  const bool isRollCol = roll!=0.0 && col==ncol-1;  // col==ncol-1 implies col>-1
  SEXP ic, xc;
  if (isDataCol) {
    ic = idtVec[icols[col]-1];  // ic = i column
    xc = xdtVec[xcols[col]-1];  // xc = x column
    // it was checked in bmerge() above that TYPEOF(ic)==TYPEOF(xc)
  } else {
    ic = R_NilValue;
    xc = nqgrp;
  }

  int lir = ilow + (iupp-ilow)/2;           // lir = logical i row.
  int ir = o ? o[lir]-1 : lir;              // ir = the actual i row if i were ordered

  #define BINX(XVAL, CMP1, CMP2)                                                                  \
    while (xlow < xupp-1) {                                                                       \
      int mid = xlow + (xupp-xlow)/2;                                                             \
      XVAL;                                                                                       \
      if (CMP1) {   /* relies on NA_INTEGER==INT_MIN, tested in init.c */                         \
        xlow=mid;                                                                                 \
      } else if (CMP2)  {   /* TO DO: switch(sign(xval-ival)) ? */                                \
        xupp=mid;                                                                                 \
      } else {                                                                                    \
        /* xval == ival  including NA_INTEGER==NA_INTEGER */                                      \
        /* branch mid to find start and end of this group in this column */                       \
        /*   TO DO?: not if mult=first|last and col<ncol-1 */                                     \
        int tmplow = mid;                                                                         \
        while (tmplow<xupp-1) {                                                                   \
          int mid = tmplow + (xupp-tmplow)/2;                                                     \
          if (WRAP(xcv[XIND(mid)]) == ival) tmplow=mid; else xupp=mid;                            \
        }                                                                                         \
        int tmpupp = mid;                                                                         \
        while (xlow<tmpupp-1) {                                                                   \
          int mid = xlow + (tmpupp-xlow)/2;                                                       \
          if (WRAP(xcv[XIND(mid)]) == ival) tmpupp=mid; else xlow=mid;                            \
        }                                                                                         \
        /* xlow and xupp now surround the group in xc */                                          \
        break;                                                                                    \
      }                                                                                           \
    }

  #define DO(TYPE, LOWDIST, UPPDIST, SEARCH_EQUAL_IROW)                                           \
    if (!isDataCol)                                                                               \
      break;                                                                                      \
    if (isRollCol && xlow==xupp-1) {  /* no match found in last column */                         \
      if (rollToNearest) {                                                                        \
        /* when rollToNearest you can't limit the distance currently */                           \
        /* rollToNearest not yet supported for STRSXP (checked up front) */                       \
        if ( xlow>xlowIn && xupp<xuppIn ) {                                                       \
          if ( LOWDIST <= UPPDIST )                                                               \
            xlow--;                                                                               \
          else                                                                                    \
            xupp++;                                                                               \
        }                                                                                         \
        else if (xupp==xuppIn && rollends[1])                                                     \
          xlow--;                                                                                 \
        else if (xlow==xlowIn && rollends[0])                                                     \
          xupp++;                                                                                 \
      } else {                                                                                    \
        /* Regular roll=TRUE|+ve|-ve */                                                           \
        /* Rprintf("xlow=%d xlowIn=%d xupp=%d xuppIn=%d lowmax=%d uppmax=%d ilow=%d iupp=%d\n", xlow, xlowIn, xupp, xuppIn, lowmax, uppmax, ilow, iupp); */ \
        if ((( roll>0.0 && xlow>xlowIn && (xupp<xuppIn || !uppmax || rollends[1]))                \
          || ( roll<0.0 && xupp==xuppIn && uppmax && rollends[1]) )  /* test 933 */               \
          && ( isinf(rollabs) || ((LOWDIST)-(TYPE)rollabs <= (TYPE)1e-6) ))                       \
          /*   ^^^^^^^^^^^^^^ always true for STRSXP where LOWDIST is a dummy,  TODO pre-save isinf() into const bool */ \
          xlow--;                                                                                 \
          /* ** AND NOW EXTEND iupp too for all irows that join to this same row */               \
        else                                                                                      \
        if ((( roll<0.0 && xupp<xuppIn && (xlow>xlowIn || !lowmax || rollends[0]))                \
          || ( roll>0.0 && xlow==xlowIn && lowmax && rollends[0]) )                               \
          && ( isinf(rollabs) || ((UPPDIST)-(TYPE)rollabs <= (TYPE)1e-6) ))                       \
          xupp++;                                                                                 \
      }                                                                                           \
    }                                                                                             \
    if (op[col] != EQ) {                                                                          \
      /* never true for STRSXP checked up front */                                                \
      switch (op[col]) {                                                                          \
      case LE : if (!ISNAT(ival)) xlow = xlowIn; break;                                           \
      case LT : xupp = xlow + 1; if (!ISNAT(ival)) xlow = xlowIn; break;                          \
      case GE : if (!ISNAT(ival)) xupp = xuppIn; break;                                           \
      case GT : xlow = xupp - 1; if (!ISNAT(ival)) xupp = xuppIn; break;                          \
      /* no other cases; checked up front to avoid handling error in parallel region */           \
      }                                                                                           \
      /* for LE/LT cases, ensure xlow excludes NA indices, != EQ is checked above already */      \
      if (op[col]<=3 && xlow<xupp-1 && (!ISNAT(ival)) && (ISNAT(xcv[XIND(xlow+1)]))) {            \
        int tmplow = xlow, tmpupp = xupp;                                                         \
        while (tmplow < tmpupp-1) {                                                               \
          int mid = tmplow + (tmpupp-tmplow)/2;                                                   \
          if (ISNAT(xcv[XIND(mid)])) tmplow = mid; else tmpupp = mid;                             \
        }                                                                                         \
        xlow = tmplow; /* tmplow is the index of last NA value */                                 \
      }                                                                                           \
    }                                                                                             \
    if (SEARCH_EQUAL_IROW) {                                                                      \
      /* false when NA_REAL was dealt with up front and matches were found */                     \
      int tmplow = lir;                                                                           \
      while (tmplow<iupp-1) { /* TO DO: could double up from lir rather than halving from iupp */ \
        int mid = tmplow + (iupp-tmplow)/2;                                                       \
        if (WRAP(icv[ o ? o[mid]-1 : mid ]) == ival) tmplow=mid; else iupp=mid;                   \
        /* if we could guarantee ivals to be *always* sorted for all columns independently */     \
        /* (= max(nestedid) = 1), could speed this up 2x by checking GE,GT,LE,LT separately */    \
      }                                                                                           \
      int tmpupp = lir;                                                                           \
      while (ilow<tmpupp-1) {                                                                     \
        int mid = ilow + (tmpupp-ilow)/2;                                                         \
        if (WRAP(icv[ o ? o[mid]-1 : mid ]) == ival) tmpupp=mid; else ilow=mid;                   \
      }                                                                                           \
      /* ilow and iupp now surround the group in ic, too */                                       \
    }

  switch (TYPEOF(xc)) {
  case LGLSXP : case INTSXP : {  // including factors
    const int *icv = isDataCol ? INTEGER(ic) : NULL;
    const int *xcv = INTEGER(xc);
    const int ival = isDataCol ? icv[ir] : thisgrp;
    #define ISNAT(x) ((x)==NA_INTEGER)
    #define WRAP(x) (x)  // wrap not needed for int
    BINX(const int xval = xcv[XIND(mid)], xval<ival, xval>ival)
    DO(int, ival-xcv[XIND(xlow)], xcv[XIND(xupp)]-ival, true)
  } break;
  case STRSXP : {
    // op[col]==EQ checked up front to avoid an if() here and non-thread-safe error()
    // not sure why non-EQ is not supported for STRSXP though as it seems straightforward (StrCmp returns sign to indicate GT or LT)
    const SEXP *icv = STRING_PTR(ic);
    const SEXP *xcv = STRING_PTR(xc);
    const SEXP ival = ENC2UTF8(icv[ir]);
    #undef ISNAT
    #undef WRAP
    #define ISNAT(x) (x) // ISNAT only used for non-equi which doesn't occur for STRSXP
    #define WRAP(x) (ENC2UTF8(x))
    BINX(int tmp=StrCmp(ENC2UTF8(xcv[XIND(mid)]), ival), tmp<0, tmp>0)
    DO(int, 0, 0, true)
    // NA_STRING are allowed and joined to; does not do ENC2UTF8 again inside StrCmp
    // TO DO: deal with mixed encodings and locale optionally; could StrCmp non-ascii in a thread-safe non-alloc manner
  } break;
  case REALSXP :
    if (isI64[col]) {  // use pre-stored result of INHERITS, PR#5187
      const int64_t *icv = (const int64_t *)REAL(ic);
      const int64_t *xcv = (const int64_t *)REAL(xc);
      const int64_t ival = icv[ir];
      #undef ISNAT
      #undef WRAP
      #define ISNAT(x) ((x)==NA_INTEGER64)
      #define WRAP(x) (x)
      BINX(const int64_t xval=xcv[XIND(mid)], xval<ival, xval>ival)
      DO(int64_t, ival-xcv[XIND(xlow)], xcv[XIND(xupp)]-ival, true)
    } else {
      const double *icv = REAL(ic);
      const double *xcv = REAL(xc);
      double ival = icv[ir];
      #undef ISNAT
      #undef WRAP
      #define ISNAT(x) (ISNAN(x))
      #define WRAP(x) (x)

      // get NA and NaN out of the way; if any they are at the start so use that fact
      // -Inf and +Inf don't need to be dealt with because we now use cmp ops on double (no longer dtwiddle)
      // under non-equiy <= and >=, NA match to NA and NaN match to NaN (the = part)
      bool someNAmatch=false;
      int xtmp=xlow, itmp=ilow;
      while (xtmp<xupp-1 && ISNAN(xcv[XIND(xtmp+1)])) xtmp++;
      while (itmp<iupp-1 && ISNAN(icv[o ? o[itmp+1]-1 : itmp+1])) itmp++;
      if (xtmp>xlowIn && itmp>ilowIn) {
        // both x and i have some NA or NaN at the beginning; NA<NaN guaranteed
        int xtmp2=xtmp, itmp2=itmp;
        while (xtmp2>xlowIn && !ISNA(xcv[XIND(xtmp2)])) xtmp2--;  // reverse over NaNs
        while (itmp2>ilowIn && !ISNA(icv[o ? o[itmp2]-1 : itmp2])) itmp2--;
        if (xtmp2>xlowIn && itmp2>ilowIn) {
          // some NA match to NA
          xupp = xtmp2+1;
          iupp = itmp2+1;
          someNAmatch=true;
          ival = NA_REAL;
        }
        else if (xtmp2<xtmp && itmp2<itmp) {
          // some NaN match to NaN
          xlow = xtmp2;
          xupp = xtmp+1;
          ilow = itmp2;
          iupp = itmp+1;
          someNAmatch=true;
          ival = R_NaN;
        }
      }
      if (!someNAmatch) {
        if (xtmp==xupp-1 || itmp==iupp-1) {
          // all NA/NaN on one or both sides and non of them match, so we're done; otherwise following code
          // requires low and upp to straddle at least one row of x and i
          return;
        }
        xlow = xtmp;  // skip NA/NaN at the start of this subgroup of x because they don't match to any in i
        ilow = itmp;  // skip NA/NaN at the start of this subgroup of i because they don't match to any in x
        lir = ilow + (iupp-ilow)/2;  // recompute lir and ir in case NA/NaN were skipped and ilow increased
        ir = o ? o[lir]-1 : lir;
        ival = icv[ir];
        if (xo) {
          BINX(const double xval=xcv[xo[mid]-1], xval<ival, xval>ival)
        } else {
          BINX(const double xval=xcv[mid],       xval<ival, xval>ival)
        }
      }
      if (xo) {
        DO(double, ival-xcv[xo[xlow]-1], xcv[xo[xupp]-1]-ival, !someNAmatch)
      } else {
        DO(double, ival-xcv[xlow],       xcv[xupp]-ival, !someNAmatch)
      }
    }
    break;
  // supported types were checked up front to avoid handling an error here in (future) parallel region
  default:
    error(_("Type '%s' is not supported for joining/merging"), type2char(TYPEOF(xc)));
  }

  if (xlow<xupp-1) { // if value found, xlow and xupp surround it, unlike standard binary search where low falls on it
    if (col<ncol-1) {  // could include col==-1 here (a non-equi non-data column)
      bmerge_r(xlow, xupp, ilow, iupp, col+1, thisgrp, true, true);
    } else {
      int len = xupp-xlow-1;
      if (mult==ALL && len>1) allLen1[0] = FALSE;
      if (nqmaxgrp == 1) {
        const int rf = (mult!=LAST) ? xlow+2 : xupp; // extra +1 for 1-based indexing at R level
        const int rl = (mult==ALL) ? len : 1;
        for (int j=ilow+1; j<iupp; j++) {   // usually iterates once only for j=ir
          const int k = o ? o[j]-1 : j;
          retFirst[k] = rf;
          retLength[k]= rl;
          // retIndex initialisation is taken care of in bmerge and doesn't change for thisgrp=1
        }
        // Rprintf("bmerge_r0 %d %d %d %d\n", xlow, xupp, ilow, iupp);
      } else {
        // non-equi join
        for (int j=ilow+1; j<iupp; j++) {
          const int k = o ? o[j]-1 : j;
          if (retFirst[k] != nomatch) {
            if (mult == ALL) {
              // for this irow, we've matches on more than one group
              allGrp1[0] = FALSE;
              retFirst[ctr+ilen] = xlow+2;
              retLength[ctr+ilen] = len;
              retIndex[ctr+ilen] = k+1;
              ++ctr;
              if (ctr+ilen >= anslen) {
                anslen = 1.1*anslen;
                retFirst = Realloc(retFirst, anslen, int);   // if fails, it fails inside Realloc with R error
                retLength = Realloc(retLength, anslen, int);
                retIndex = Realloc(retIndex, anslen, int);
              }
            } else if (mult == FIRST) {
              retFirst[k] = (XIND(retFirst[k]-1) > XIND(xlow+1)) ? xlow+2 : retFirst[k];
              retLength[k] = 1;
            } else {
              retFirst[k] = (XIND(retFirst[k]-1) < XIND(xupp-1)) ? xupp : retFirst[k];
              retLength[k] = 1;
            }
          } else {
            // none of the groups so far have filled in for this index. So use it!
            if (mult == ALL) {
              retFirst[k] = xlow+2;
              retLength[k] = len;
              retIndex[k] = k+1;
              // no need to increment ctr of course
            } else {
              retFirst[k] = (mult == FIRST) ? xlow+2 : xupp;
              retLength[k] = 1;
            }
          }
        }
      }
    }
  }
  switch (op[col]) {
  case EQ:
    if (ilow>ilowIn && (xlow>xlowIn || isRollCol)) {
      // Rprintf("bmerge_r1 %d %d %d %d\n", xlowIn, xlow+1+isRollCol, ilowIn, ilow+1);
      bmerge_r(xlowIn, xlow+1+isRollCol, ilowIn, ilow+1, col, 1, lowmax, uppmax && xlow+1+isRollCol==xuppIn);
    }
    if (iupp<iuppIn && (xupp<xuppIn || isRollCol)) {
      // Rprintf("bmerge_r2 %d %d %d %d\n", xupp-1-isRollCol, xuppIn, iupp-1, iuppIn);
      bmerge_r(MAX(xupp-1-isRollCol, xlowIn), xuppIn, iupp-1, iuppIn, col, 1, lowmax && MAX(xupp-1-isRollCol, xlowIn)==xlowIn, uppmax);
      // both MAX's needed for test 936. Just the first MAX needed for test 937. TODO: remove MAX's when too much recursion is avoided. They aren't needed for the case just above.
    }
    break;
  case LE: case LT:
    // roll is not yet implemented
    if (ilow>ilowIn)
      bmerge_r(xlowIn, xuppIn, ilowIn, ilow+1, col, 1, false, false);
    if (iupp<iuppIn)
      bmerge_r(xlowIn, xuppIn, iupp-1, iuppIn, col, 1, false, false);
    break;
  case GE: case GT:
    // roll is not yet implemented
    if (ilow>ilowIn)
      bmerge_r(xlowIn, xuppIn, ilowIn, ilow+1, col, 1, false, false);
    if (iupp<iuppIn)
      bmerge_r(xlowIn, xuppIn, iupp-1, iuppIn, col, 1, false, false);
    break;
  default : break;  // one of 5 valid cases checked up front
  }
}
