#include "data.table.h"
#include <signal.h> // the debugging machinery + breakpoint aidee

/*
Implements binary search (a.k.a. divide and conquer).
http://en.wikipedia.org/wiki/Binary_search
http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary
http://googleresearch.blogspot.com/2006/06/extra-extra-read-all-about-it-nearly.html
Differences over standard binary search (e.g. bsearch in stdlib.h) :
  o list of vectors (key of many columns) of different types
  o ties (groups)
  o NA,NAN,-Inf,+Inf are distinct values and can be joined to
  o type double is joined within tolerance (apx 11 s.f.)
  o join to prevailing value (roll join a.k.a locf), forwards or backwards
  o join to nearest
  o roll the beginning and end optionally
  o limit the roll distance to a user provided value
*/

#define ENC_KNOWN(x) (LEVELS(x) & 12)
// 12 = LATIN1_MASK (1<<2) | UTF8_MASK (1<<3)  // Would use these definitions from Defn.h, but that appears to be private to R. Hence 12.

#define EQ 1
#define LE 2
#define LT 3
#define GE 4
#define GT 5

static SEXP i, x, nqgrp;
static int ncol, *icols, *xcols, *o, *xo, *retFirst, *retLength, *retIndex, *allLen1, *allGrp1, *rollends, ilen, anslen;
static int *op, nqmaxgrp, *tmpptr, scols;
static int ctr, nomatch; // populating matches for non-equi joins
enum {ALL, FIRST, LAST} mult = ALL;
static double roll, rollabs;
static Rboolean rollToNearest=FALSE;
#define XIND(i) (xo ? xo[(i)]-1 : i)

void bmerge_r(int xlow, int xupp, int ilow, int iupp, int col, int thisgrp, int lowmax, int uppmax);

SEXP bmerge(SEXP iArg, SEXP xArg, SEXP icolsArg, SEXP xcolsArg, SEXP isorted, SEXP xoArg, SEXP rollarg, SEXP rollendsArg, SEXP nomatchArg, SEXP multArg, SEXP opArg, SEXP nqgrpArg, SEXP nqmaxgrpArg) {
    int xN, iN, protecti=0;
    ctr=0; // needed for non-equi join case
    SEXP retFirstArg, retLengthArg, retIndexArg, allLen1Arg, allGrp1Arg;
    retFirstArg = retLengthArg = retIndexArg = R_NilValue; // suppress gcc msg

    // iArg, xArg, icolsArg and xcolsArg
    i = iArg; x = xArg;  // set globals so bmerge_r can see them.
    if (!isInteger(icolsArg)) error("Internal error: icols is not integer vector");
    if (!isInteger(xcolsArg)) error("Internal error: xcols is not integer vector");
    if (LENGTH(icolsArg) > LENGTH(xcolsArg)) error("Internal error: length(icols) [%d] > length(xcols) [%d]", LENGTH(icolsArg), LENGTH(xcolsArg)); 
    icols = INTEGER(icolsArg);
    xcols = INTEGER(xcolsArg);
    xN = LENGTH(VECTOR_ELT(x,0));
    iN = ilen = anslen = LENGTH(VECTOR_ELT(i,0));
    ncol = LENGTH(icolsArg);    // there may be more sorted columns in x than involved in the join
    for(int col=0; col<ncol; col++) {
        if (icols[col]==NA_INTEGER) error("Internal error. icols[%d] is NA", col);
        if (xcols[col]==NA_INTEGER) error("Internal error. xcols[%d] is NA", col);
        if (icols[col]>LENGTH(i) || icols[col]<1) error("icols[%d]=%d outside range [1,length(i)=%d]", col, icols[col], LENGTH(i));
        if (xcols[col]>LENGTH(x) || xcols[col]<1) error("xcols[%d]=%d outside range [1,length(x)=%d]", col, xcols[col], LENGTH(x));
        int it = TYPEOF(VECTOR_ELT(i, icols[col]-1));
        int xt = TYPEOF(VECTOR_ELT(x, xcols[col]-1));
        if (it != xt) error("typeof x.%s (%s) != typeof i.%s (%s)", CHAR(STRING_ELT(getAttrib(x,R_NamesSymbol),xcols[col]-1)), type2char(xt), CHAR(STRING_ELT(getAttrib(i,R_NamesSymbol),icols[col]-1)), type2char(it));
    }
    // raise(SIGINT);

    // rollArg, rollendsArg
    roll = 0.0; rollToNearest = FALSE;
    if (isString(rollarg)) {
        if (strcmp(CHAR(STRING_ELT(rollarg,0)),"nearest") != 0) error("roll is character but not 'nearest'");
        roll=1.0; rollToNearest=TRUE;       // the 1.0 here is just any non-0.0, so roll!=0.0 can be used later
    } else {
        if (!isReal(rollarg)) error("Internal error: roll is not character or double");
        roll = REAL(rollarg)[0];   // more common case (rolling forwards or backwards) or no roll when 0.0
    }
    rollabs = fabs(roll);
    if (!isLogical(rollendsArg) || LENGTH(rollendsArg) != 2)
        error("rollends must be a length 2 logical vector");
    rollends = LOGICAL(rollendsArg);
    if (rollToNearest && TYPEOF(VECTOR_ELT(i, icols[ncol-1]-1))==STRSXP)
        error("roll='nearest' can't be applied to a character column, yet.");

    // nomatch arg
    nomatch = INTEGER(nomatchArg)[0];

    // mult arg
    if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "all")) mult = ALL;
    else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "first")) mult = FIRST;
    else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "last")) mult = LAST;
    else error("Internal error: invalid value for 'mult'. Please report to datatable-help");

    // opArg
    if (!isInteger(opArg) || length(opArg) != ncol)
        error("Internal error: opArg is not an integer vector of length equal to length(on)");
    op = INTEGER(opArg);
    if (!isInteger(nqgrpArg))
        error("Internal error: nqgrpArg must be an integer vector");
    nqgrp = nqgrpArg; // set global for bmerge_r
    scols = (!length(nqgrpArg)) ? 0 : -1; // starting col index, -1 is external group column for non-equi join case

    // nqmaxgrpArg
    if (!isInteger(nqmaxgrpArg) || length(nqmaxgrpArg) != 1 || INTEGER(nqmaxgrpArg)[0] <= 0)
        error("Intrnal error: nqmaxgrpArg is not a positive length-1 integer vector");
    nqmaxgrp = INTEGER(nqmaxgrpArg)[0];
    if (nqmaxgrp>1 && mult == ALL) {
        // non-equi case with mult=ALL, may need reallocation
        anslen = 1.1 * ((iN > 1000) ? iN : 1000);
        retFirst = Calloc(anslen, int); // anslen is set above
        retLength = Calloc(anslen, int);
        retIndex = Calloc(anslen, int);
        if (retFirst==NULL || retLength==NULL || retIndex==NULL)
            error("Internal error in allocating memory for non-equi join");
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
        SEXP order = PROTECT(int_vec_init(length(icolsArg), 1)); // rep(1L, length(icolsArg))
        SEXP oSxp = PROTECT(forder(i, icolsArg, PROTECT(ScalarLogical(FALSE)),
                            PROTECT(ScalarLogical(TRUE)), order, PROTECT(ScalarLogical(FALSE))));
        UNPROTECT(3); // The 3 logicals in line above. TODO - split head of forder into C-level callable
        protecti += 2;   // order and oSxp
        if (!LENGTH(oSxp)) o = NULL; else o = INTEGER(oSxp);
    }

    // xo arg
    xo = NULL;
    if (length(xoArg)) {
        if (!isInteger(xoArg)) error("Internal error: xoArg is not an integer vector");
        xo = INTEGER(xoArg);
    }

    // start bmerge
    if (iN) {
        // embarassingly parallel if we've storage space for nqmaxgrp*iN
        for (int kk=0; kk<nqmaxgrp; kk++) {
            bmerge_r(-1,xN,-1,iN,scols,kk+1,1,1);
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
    SET_STRING_ELT(ansnames, 1, mkChar("lens"));
    SET_STRING_ELT(ansnames, 2, mkChar("indices"));
    SET_STRING_ELT(ansnames, 3, mkChar("allLen1"));
    SET_STRING_ELT(ansnames, 4, mkChar("allGrp1"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    if (nqmaxgrp > 1 && mult == ALL) {
        Free(retFirst);
        Free(retLength);
        Free(retIndex);
    }
    UNPROTECT(protecti);
    return (ans);
}

static union {
  int i;
  double d;
  unsigned long long ull;
  long long ll;
  SEXP s;
} ival, xval;

static int mid, tmplow, tmpupp;  // global to save them being added to recursive stack. Maybe optimizer would do this anyway.
static SEXP ic, xc;

// If we find a non-ASCII, non-NA, non-UTF8 encoding, we try to convert it to UTF8. That is, marked non-ascii/non-UTF8 encodings will always be checked in UTF8 locale. This seems to be the best fix I could think of to put the encoding issues to rest..
// Since the if-statement will fail with the first condition check in "normal" ASCII cases, there shouldn't be huge penalty issues for default setup.
// Fix for #66, #69, #469 and #1293
// TODO: compare 1.9.6 performance with 1.9.7 with huge number of ASCII strings.
SEXP ENC2UTF8(SEXP s) {
    if (!IS_ASCII(s) && s != NA_STRING && !IS_UTF8(s))
        s = mkCharCE(translateCharUTF8(s), CE_UTF8);
    return (s);
}

void bmerge_r(int xlowIn, int xuppIn, int ilowIn, int iuppIn, int col, int thisgrp, int lowmax, int uppmax)
// col is >0 and <=ncol-1 if this range of [xlow,xupp] and [ilow,iupp] match up to but not including that column
// lowmax=1 if xlowIn is the lower bound of this group (needed for roll)
// uppmax=1 if xuppIn is the upper bound of this group (needed for roll)
// new: col starts with -1 for non-equi joins, which gathers rows from nested id group counter 'thisgrp'
{
    int xlow=xlowIn, xupp=xuppIn, ilow=ilowIn, iupp=iuppIn, j, k, ir, lir, tmp;
    Rboolean isInt64=FALSE;
    ir = lir = ilow + (iupp-ilow)/2;           // lir = logical i row.
    if (o) ir = o[lir]-1;                      // ir = the actual i row if i were ordered
    if (col>-1) {
        ic = VECTOR_ELT(i,icols[col]-1);  // ic = i column
        xc = VECTOR_ELT(x,xcols[col]-1);  // xc = x column
    // it was checked in bmerge() that the types are equal
    } else xc = nqgrp;
    switch (TYPEOF(xc)) {
    case LGLSXP : case INTSXP :   // including factors
        ival.i = (col>-1) ? INTEGER(ic)[ir] : thisgrp;
        while(xlow < xupp-1) {
            mid = xlow + (xupp-xlow)/2;   // Same as (xlow+xupp)/2 but without risk of overflow
            xval.i = INTEGER(xc)[XIND(mid)];
            if (xval.i<ival.i) {          // relies on NA_INTEGER == INT_MIN, tested in init.c
                xlow=mid;
            } else if (xval.i>ival.i) {   // TO DO: is *(&xlow, &xupp)[0|1]=mid more efficient than branch?
                xupp=mid;
            } else {
                // xval.i == ival.i  including NA_INTEGER==NA_INTEGER
                // branch mid to find start and end of this group in this column
                // TO DO?: not if mult=first|last and col<ncol-1
                tmplow = mid;
                tmpupp = mid;
                while(tmplow<xupp-1) {
                    mid = tmplow + (xupp-tmplow)/2;
                    xval.i = INTEGER(xc)[XIND(mid)];
                    if (xval.i == ival.i) tmplow=mid; else xupp=mid;
                }
                while(xlow<tmpupp-1) {
                    mid = xlow + (tmpupp-xlow)/2;
                    xval.i = INTEGER(xc)[XIND(mid)];
                    if (xval.i == ival.i) tmpupp=mid; else xlow=mid;
                }
                // xlow and xupp now surround the group in xc, we only need this range for the next column
                break;
            }
        }
        if (col>-1 && op[col] != EQ) {
            switch (op[col]) {
                case LE : xlow = xlowIn; break;
                case LT : xupp = xlow + 1; xlow = xlowIn; break;
                case GE : if (ival.i != NA_INTEGER) xupp = xuppIn; break;
                case GT : xlow = xupp - 1; if (ival.i != NA_INTEGER) xupp = xuppIn; break;
            }
            // for LE/LT cases, we need to ensure xlow excludes NA indices, != EQ is checked above already
            if (op[col] <= 3 && xlow<xupp-1 && ival.i != NA_INTEGER && INTEGER(xc)[XIND(xlow+1)] == NA_INTEGER) {
                tmplow = xlow; tmpupp = xupp;
                while (tmplow < tmpupp-1) {
                    mid = tmplow + (tmpupp-tmplow)/2;
                    xval.i = INTEGER(xc)[XIND(mid)];
                    if (xval.i == NA_INTEGER) tmplow = mid; else tmpupp = mid;
                }
                xlow = tmplow; // tmplow is the index of last NA value
            }
        }
        tmplow = lir;
        tmpupp = lir;
        if (col>-1) {
            while(tmplow<iupp-1) {   // TO DO: could double up from lir rather than halving from iupp
                mid = tmplow + (iupp-tmplow)/2;
                xval.i = INTEGER(ic)[ o ? o[mid]-1 : mid ];   // reuse xval to search in i
                if (xval.i == ival.i) tmplow=mid; else iupp=mid;
                // if we could guarantee ivals to be *always* sorted for all columns independently (= max(nestedid) = 1), then we can speed this up by 2x by adding checks for GE,GT,LE,LT separately.
            }
            while(ilow<tmpupp-1) {
                mid = ilow + (tmpupp-ilow)/2;
                xval.i = INTEGER(ic)[ o ? o[mid]-1 : mid ];
                if (xval.i == ival.i) tmpupp=mid; else ilow=mid;
            }
        }
        // ilow and iupp now surround the group in ic, too
        break;
    case STRSXP :
        if (op[col] != EQ) error("Only '==' operator is supported for columns of type %s.", type2char(TYPEOF(xc)));
        ival.s = ENC2UTF8(STRING_ELT(ic,ir));
        while(xlow < xupp-1) {
            mid = xlow + (xupp-xlow)/2;
            xval.s = ENC2UTF8(STRING_ELT(xc, XIND(mid)));
            tmp = StrCmp(xval.s, ival.s);  // uses pointer equality first, NA_STRING are allowed and joined to, then uses strcmp on CHAR().
            if (tmp == 0) {                // TO DO: deal with mixed encodings and locale optionally
                tmplow = mid;
                tmpupp = mid;
                while(tmplow<xupp-1) {
                    mid = tmplow + (xupp-tmplow)/2;
                    xval.s = ENC2UTF8(STRING_ELT(xc, XIND(mid)));
                    if (ival.s == xval.s) tmplow=mid; else xupp=mid;  // the == here handles encodings as well. Marked non-utf8 encodings are converted to utf-8 using ENC2UTF8.
                }
                while(xlow<tmpupp-1) {
                    mid = xlow + (tmpupp-xlow)/2;
                    xval.s = ENC2UTF8(STRING_ELT(xc, XIND(mid)));
                    if (ival.s == xval.s) tmpupp=mid; else xlow=mid;  // see above re ==
                }
                break;
            } else if (tmp < 0) {
                xlow=mid;
            } else {
                xupp=mid;
            }
        }
        tmplow = lir;
        tmpupp = lir;
        while(tmplow<iupp-1) {
            mid = tmplow + (iupp-tmplow)/2;
            xval.s = ENC2UTF8(STRING_ELT(ic, o ? o[mid]-1 : mid));
            if (xval.s == ival.s) tmplow=mid; else iupp=mid;   // see above re ==
        }
        while(ilow<tmpupp-1) {
            mid = ilow + (tmpupp-ilow)/2;
            xval.s = ENC2UTF8(STRING_ELT(ic, o ? o[mid]-1 : mid));
            if (xval.s == ival.s) tmpupp=mid; else ilow=mid;   // see above re == 
        }
        break;
    case REALSXP :
        isInt64 = INHERITS(xc, char_integer64);
        twiddle = isInt64 ? &i64twiddle : &dtwiddle;
        ival.ull = twiddle(DATAPTR(ic), ir, 1);
        while(xlow < xupp-1) {
            mid = xlow + (xupp-xlow)/2;
            xval.ull = twiddle(DATAPTR(xc), XIND(mid), 1);
            if (xval.ull<ival.ull) {
                xlow=mid;
            } else if (xval.ull>ival.ull) {
                xupp=mid;
            } else { // xval.ull == ival.ull) 
                tmplow = mid;
                tmpupp = mid;
                while(tmplow<xupp-1) {
                    mid = tmplow + (xupp-tmplow)/2;
                    xval.ull = twiddle(DATAPTR(xc), XIND(mid), 1);
                    if (xval.ull == ival.ull) tmplow=mid; else xupp=mid;
                }
                while(xlow<tmpupp-1) {
                    mid = xlow + (tmpupp-xlow)/2;
                    xval.ull = twiddle(DATAPTR(xc), XIND(mid), 1);
                    if (xval.ull == ival.ull) tmpupp=mid; else xlow=mid;
                }
                break;
            }
        }
        if (col>-1 && op[col] != EQ) {
            Rboolean isivalNA = !isInt64 ? ISNAN(REAL(ic)[ir]) : (DtoLL(REAL(ic)[ir]) == NA_INT64_LL);
            switch (op[col]) {
            case LE : if (!isivalNA) xlow = xlowIn; break;
            case LT : xupp = xlow + 1; if (!isivalNA) xlow = xlowIn; break;
            case GE : if (!isivalNA) xupp = xuppIn; break;
            case GT : xlow = xupp - 1; if (!isivalNA) xupp = xuppIn; break;
            }
            // for LE/LT cases, we need to ensure xlow excludes NA indices, != EQ is checked above already
            if (op[col] <= 3 && xlow<xupp-1 && !isivalNA && (!isInt64 ? ISNAN(REAL(xc)[XIND(xlow+1)]) : (DtoLL(REAL(xc)[XIND(xlow+1)]) == NA_INT64_LL))) {
                tmplow = xlow; tmpupp = xupp;
                while (tmplow < tmpupp-1) {
                    mid = tmplow + (tmpupp-tmplow)/2;
                    xval.d = REAL(xc)[XIND(mid)];
                    if (!isInt64 ? ISNAN(xval.d) : xval.ll == NA_INT64_LL) tmplow = mid; else tmpupp = mid;
                }
                xlow = tmplow; // tmplow is the index of last NA value
            }
        }
        tmplow = lir;
        tmpupp = lir;
        if (col>-1) {
            while(tmplow<iupp-1) {
                mid = tmplow + (iupp-tmplow)/2;
                xval.ull = twiddle(DATAPTR(ic), o ? o[mid]-1 : mid, 1 );
                if (xval.ull == ival.ull) tmplow=mid; else iupp=mid;
            }
            while(ilow<tmpupp-1) {
                mid = ilow + (tmpupp-ilow)/2;
                xval.ull = twiddle(DATAPTR(ic), o ? o[mid]-1 : mid, 1 );
                if (xval.ull == ival.ull) tmpupp=mid; else ilow=mid;
            }
        }
        // ilow and iupp now surround the group in ic, too
        break;
    default:
        error("Type '%s' not supported as key column", type2char(TYPEOF(xc)));
    }
    if (xlow<xupp-1) { // if value found, low and upp surround it, unlike standard binary search where low falls on it
        if (col<ncol-1) {
            bmerge_r(xlow, xupp, ilow, iupp, col+1, thisgrp, 1, 1);
            // final two 1's are lowmax and uppmax
        } else {
            int len = xupp-xlow-1;
            if (mult==ALL && len>1) allLen1[0] = FALSE;
            if (nqmaxgrp == 1) {
                for (j=ilow+1; j<iupp; j++) {   // usually iterates once only for j=ir
                    k = o ? o[j]-1 : j;
                    retFirst[k] = (mult != LAST) ? xlow+2 : xupp; // extra +1 for 1-based indexing at R level
                    retLength[k]= (mult == ALL) ? len : 1;
                    // retIndex initialisation is taken care of in bmerge and doesn't change for thisgrp=1
                }
            } else {
                // non-equi join
                for (j=ilow+1; j<iupp; j++) {
                    k = o ? o[j]-1 : j;
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
                                tmpptr = Realloc(retFirst, anslen, int);
                                if (tmpptr != NULL) retFirst = tmpptr; 
                                else error("Error in reallocating memory in non-equi joins.\n");
                                tmpptr = Realloc(retLength, anslen, int);
                                if (tmpptr != NULL) retLength = tmpptr; 
                                else error("Error in reallocating memory in non-equi joins.\n");
                                tmpptr = Realloc(retIndex, anslen, int);
                                if (tmpptr != NULL) retIndex = tmpptr; 
                                else error("Error in reallocating memory in non-equi joins.\n");
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
    } else if (roll!=0.0 && col==ncol-1) {
        // runs once per i row (not each search test), so not hugely time critical
        if (xlow != xupp-1 || xlow<xlowIn || xupp>xuppIn) error("Internal error: xlow!=xupp-1 || xlow<xlowIn || xupp>xuppIn");
        if (rollToNearest) {   // value of roll ignored currently when nearest
            if ( (!lowmax || xlow>xlowIn) && (!uppmax || xupp<xuppIn) ) {
                if (  ( TYPEOF(ic)==REALSXP && REAL(ic)[ir]-REAL(xc)[XIND(xlow)] <= REAL(xc)[XIND(xupp)]-REAL(ic)[ir] )
                   || ( TYPEOF(ic)<=INTSXP && INTEGER(ic)[ir]-INTEGER(xc)[XIND(xlow)] <= INTEGER(xc)[XIND(xupp)]-INTEGER(ic)[ir] )) {
                    retFirst[ir] = xlow+1;
                    retLength[ir] = 1;
                } else {
                    retFirst[ir] = xupp+1;
                    retLength[ir] = 1;
                }
            } else if (uppmax && xupp==xuppIn && rollends[1]) {
                retFirst[ir] = xlow+1;
                retLength[ir] = 1;
            } else if (lowmax && xlow==xlowIn && rollends[0]) {
                retFirst[ir] = xupp+1;
                retLength[ir] = 1;
            }
        } else {
            // Regular roll=TRUE|+ve|-ve
            // Fixed issues: #1405, #1650, #1007
            // TODO: incorporate the twiddle logic for roll as well instead of tolerance?  
            if ( (   (roll>0.0 && (!lowmax || xlow>xlowIn) && (xupp<xuppIn || !uppmax || rollends[1]))
                  || (roll<0.0 && xupp==xuppIn && uppmax && rollends[1]) )
              && (   (TYPEOF(ic)==REALSXP &&
                      (ival.d = REAL(ic)[ir], xval.d = REAL(xc)[XIND(xlow)], 1) &&
                     (( !isInt64 &&
                        (ival.d-xval.d-rollabs < 1e-6 || 
                         ival.d-xval.d == rollabs /*#1007*/))
                   || ( isInt64 &&
                        (double)(ival.ll-xval.ll)-rollabs < 1e-6 ) ))  // cast to double for when rollabs==Inf
                  || (TYPEOF(ic)<=INTSXP && (double)(INTEGER(ic)[ir]-INTEGER(xc)[XIND(xlow)])-rollabs < 1e-6 )
                  || (TYPEOF(ic)==STRSXP)   )) {
                retFirst[ir] = xlow+1;
                retLength[ir] = 1;
            } else if
               (  (  (roll<0.0 && (!uppmax || xupp<xuppIn) && (xlow>xlowIn || !lowmax || rollends[0]))
                  || (roll>0.0 && xlow==xlowIn && lowmax && rollends[0]) )
              && (   (TYPEOF(ic)==REALSXP &&
                      (ival.d = REAL(ic)[ir], xval.d = REAL(xc)[XIND(xupp)], 1) &&
                     (( !isInt64 &&
                        (xval.d-ival.d-rollabs < 1e-6 || 
                         xval.d-ival.d == rollabs /*#1007*/))
                   || ( isInt64 &&
                        (double)(xval.ll-ival.ll)-rollabs < 1e-6 ) ))
                  || (TYPEOF(ic)<=INTSXP && (double)(INTEGER(xc)[XIND(xupp)]-INTEGER(ic)[ir])-rollabs < 1e-6 )
                  || (TYPEOF(ic)==STRSXP)   )) {
                retFirst[ir] = xupp+1;   // == xlow+2
                retLength[ir] = 1;
            }
        }
        if (iupp-ilow > 2 && retFirst[ir]!=NA_INTEGER) {
            // >=2 equal values in the last column being rolling to the same point.  
            for (j=ilow+1; j<iupp; j++) {
                // will rewrite retFirst[ir] to itself, but that's ok
                if (o) k=o[j]-1; else k=j;
                retFirst[k] = retFirst[ir];
                retLength[k]= retLength[ir]; 
            }
        }
    }
    switch (op[col]) {
    case EQ:
        if (ilow>ilowIn && (xlow>xlowIn || ((roll!=0.0 || op[col] != EQ) && col==ncol-1)))
            bmerge_r(xlowIn, xlow+1, ilowIn, ilow+1, col, 1, lowmax, uppmax && xlow+1==xuppIn);
        if (iupp<iuppIn && (xupp<xuppIn || ((roll!=0.0 || op[col] != EQ) && col==ncol-1)))
            bmerge_r(xupp-1, xuppIn, iupp-1, iuppIn, col, 1, lowmax && xupp-1==xlowIn, uppmax);
    break;
    case LE: case LT:
        // roll is not yet implemented
        if (ilow>ilowIn)
            bmerge_r(xlowIn, xuppIn, ilowIn, ilow+1, col, 1, lowmax, uppmax && xlow+1==xuppIn);
        if (iupp<iuppIn)
            bmerge_r(xlowIn, xuppIn, iupp-1, iuppIn, col, 1, lowmax && xupp-1==xlowIn, uppmax);
    break;
    case GE: case GT:
        // roll is not yet implemented
        if (ilow>ilowIn)
            bmerge_r(xlowIn, xuppIn, ilowIn, ilow+1, col, 1, lowmax, uppmax && xlow+1==xuppIn);
        if (iupp<iuppIn)
            bmerge_r(xlowIn, xuppIn, iupp-1, iuppIn, col, 1, lowmax && xupp-1==xlowIn, uppmax);
    break;
    }
}
