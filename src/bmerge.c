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
  o type double is joined within tolerance (apx 11 s.f.)
  o join to prevailing value (roll join a.k.a locf), forwards or backwards
  o join to nearest
  o roll the beginning and end optionally
  o limit the roll distance to a user provided value
*/

#define ENC_KNOWN(x) (LEVELS(x) & 12)
// 12 = LATIN1_MASK (1<<2) | UTF8_MASK (1<<3)  // Would use these definitions from Defn.h, but that appears to be private to R. Hence 12.

static SEXP i, x;
static int ncol, *icols, *xcols, *o, *xo, *retFirst, *retLength, *allLen1, *rollends;
static double roll, rollabs;
static Rboolean rollToNearest=FALSE; 
#define XIND(i) (xo ? xo[(i)]-1 : i)

void bmerge_r(int xlow, int xupp, int ilow, int iupp, int col, int lowmax, int uppmax);

SEXP bmerge(SEXP iArg, SEXP xArg, SEXP icolsArg, SEXP xcolsArg, SEXP isorted, SEXP xoArg, SEXP rollarg, SEXP rollendsArg, SEXP nomatch, SEXP retFirstArg, SEXP retLengthArg, SEXP allLen1Arg)
{
    int xN, iN, protecti=0;
    roll = 0.0;
    rollToNearest = FALSE;
    if (isString(rollarg)) {
        if (strcmp(CHAR(STRING_ELT(rollarg,0)),"nearest") != 0) error("roll is character but not 'nearest'");
        roll=1.0; rollToNearest=TRUE;       // the 1.0 here is just any non-0.0, so roll!=0.0 can be used later
    } else {
        if (!isReal(rollarg)) error("Internal error: roll is not character or double");
        roll = REAL(rollarg)[0];   // more common case (rolling forwards or backwards) or no roll when 0.0
    }
    rollabs = fabs(roll);
    
    i = iArg; x = xArg;  // set globals so bmerge_r can see them.
    if (!isInteger(icolsArg)) error("Internal error: icols is not integer vector");
    if (!isInteger(xcolsArg)) error("Internal error: xcols is not integer vector");
    if (LENGTH(icolsArg) > LENGTH(xcolsArg)) error("Internal error: length(icols) [%d] > length(xcols) [%d]", LENGTH(icolsArg), LENGTH(xcolsArg)); 
    icols = INTEGER(icolsArg);
    xcols = INTEGER(xcolsArg);
    xN = LENGTH(VECTOR_ELT(x,0));
    iN = LENGTH(VECTOR_ELT(i,0));
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
    if (!isInteger(retFirstArg) || LENGTH(retFirstArg)!=iN) error("retFirst must be integer vector the same length as nrow(i)");
    retFirst = INTEGER(retFirstArg);
    if (!isInteger(retLengthArg) || LENGTH(retLengthArg)!=iN) error("retLength must be integer vector the same length as nrow(i)");
    retLength = INTEGER(retLengthArg);
    if (!isLogical(allLen1Arg) || LENGTH(allLen1Arg) != 1) error("allLen1 must be a length 1 logical vector");
    allLen1 = LOGICAL(allLen1Arg);
    if (!isLogical(rollendsArg) || LENGTH(rollendsArg) != 2) error("rollends must be a length 2 logical vector");
    rollends = LOGICAL(rollendsArg);
    
    if (rollToNearest && TYPEOF(VECTOR_ELT(i, icols[ncol-1]-1))==STRSXP) error("roll='nearest' can't be applied to a character column, yet.");
         
    for (int j=0; j<iN; j++) {
        // defaults need to populated here as bmerge_r may well not touch many locations, say if the last row of i is before the first row of x.
        retFirst[j] = INTEGER(nomatch)[0];   // default to no match for NA goto below
        // retLength[j] = 0;   // TO DO: do this to save the branch below and later branches at R level to set .N to 0
        retLength[j] = INTEGER(nomatch)[0]==0 ? 0 : 1;
    }
    allLen1[0] = TRUE;  // All-0 and All-NA are considered all length 1 according to R code currently. Really, it means any(length>1).
    
    o = NULL;
    if (!LOGICAL(isorted)[0]) {
        SEXP order = PROTECT(vec_init(length(icolsArg), ScalarInteger(1))); // rep(1, length(icolsArg))
        SEXP oSxp = PROTECT(forder(i, icolsArg, ScalarLogical(FALSE), ScalarLogical(TRUE), order, ScalarLogical(FALSE)));
        protecti += 2;
        if (!LENGTH(oSxp)) o = NULL; else o = INTEGER(oSxp);
    }
    xo = NULL;
    if (length(xoArg)) {
        if (!isInteger(xoArg)) error("Internal error: xoArg is not an integer vector");
        xo = INTEGER(xoArg);
    }
    
    if (iN) bmerge_r(-1,xN,-1,iN,0,1,1);
    
    UNPROTECT(protecti);
    return(R_NilValue);
}

static union {
  int i;
  double d;
  unsigned long long ll;
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

void bmerge_r(int xlowIn, int xuppIn, int ilowIn, int iuppIn, int col, int lowmax, int uppmax)
// col is >0 and <=ncol-1 if this range of [xlow,xupp] and [ilow,iupp] match up to but not including that column
// lowmax=1 if xlowIn is the lower bound of this group (needed for roll)
// uppmax=1 if xuppIn is the upper bound of this group (needed for roll)
{
    int xlow=xlowIn, xupp=xuppIn, ilow=ilowIn, iupp=iuppIn, j, k, ir, lir, tmp;
    SEXP class;
    ir = lir = ilow + (iupp-ilow)/2;           // lir = logical i row.
    if (o) ir = o[lir]-1;                      // ir = the actual i row if i were ordered

    ic = VECTOR_ELT(i,icols[col]-1);  // ic = i column
    xc = VECTOR_ELT(x,xcols[col]-1);  // xc = x column
    // it was checked in bmerge() that the types are equal
    
    switch (TYPEOF(xc)) {
    case LGLSXP : case INTSXP :   // including factors
        ival.i = INTEGER(ic)[ir];
        while(xlow < xupp-1) {
            mid = xlow + (xupp-xlow)/2;   // Same as (xlow+xupp)/2 but without risk of overflow
            xval.i = INTEGER(xc)[XIND(mid)];
            if (xval.i<ival.i) {          // relies on NA_INTEGER == INT_MIN, tested in init.c
                xlow=mid;
            } else if (xval.i>ival.i) {   // TO DO: is *(&xlow, &xupp)[0|1]=mid more efficient than branch?
                xupp=mid;
            } else { // xval.i == ival.i  including NA_INTEGER==NA_INTEGER
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
        tmplow = lir;
        tmpupp = lir;
        while(tmplow<iupp-1) {   // TO DO: could double up from lir rather than halving from iupp
            mid = tmplow + (iupp-tmplow)/2;
            xval.i = INTEGER(ic)[ o ? o[mid]-1 : mid ];   // reuse xval to search in i
            if (xval.i == ival.i) tmplow=mid; else iupp=mid;
        }
        while(ilow<tmpupp-1) {
            mid = ilow + (tmpupp-ilow)/2;
            xval.i = INTEGER(ic)[ o ? o[mid]-1 : mid ];
            if (xval.i == ival.i) tmpupp=mid; else ilow=mid;
        }
        // ilow and iupp now surround the group in ic, too
        break;
    case STRSXP :
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
        class = getAttrib(xc, R_ClassSymbol);
        twiddle = (isString(class) && STRING_ELT(class, 0)==char_integer64) ? &i64twiddle : &dtwiddle;
        ival.ll = twiddle(DATAPTR(ic), ir, 1);
        while(xlow < xupp-1) {
            mid = xlow + (xupp-xlow)/2;
            xval.ll = twiddle(DATAPTR(xc), XIND(mid), 1);
            if (xval.ll<ival.ll) {
                xlow=mid;
            } else if (xval.ll>ival.ll) {
                xupp=mid;
            } else { // xval.ll == ival.ll) 
                tmplow = mid;
                tmpupp = mid;
                while(tmplow<xupp-1) {
                    mid = tmplow + (xupp-tmplow)/2;
                    xval.ll = twiddle(DATAPTR(xc), XIND(mid), 1);
                    if (xval.ll == ival.ll) tmplow=mid; else xupp=mid;
                }
                while(xlow<tmpupp-1) {
                    mid = xlow + (tmpupp-xlow)/2;
                    xval.ll = twiddle(DATAPTR(xc), XIND(mid), 1);
                    if (xval.ll == ival.ll) tmpupp=mid; else xlow=mid;
                }
                break;
            }
        }
        tmplow = lir;
        tmpupp = lir;
        while(tmplow<iupp-1) {
            mid = tmplow + (iupp-tmplow)/2;
            xval.ll = twiddle(DATAPTR(ic), o ? o[mid]-1 : mid, 1 );
            if (xval.ll == ival.ll) tmplow=mid; else iupp=mid;
        }
        while(ilow<tmpupp-1) {
            mid = ilow + (tmpupp-ilow)/2;
            xval.ll = twiddle(DATAPTR(ic), o ? o[mid]-1 : mid, 1 );
            if (xval.ll == ival.ll) tmpupp=mid; else ilow=mid;
        }
        break;
    default:
        error("Type '%s' not supported as key column", type2char(TYPEOF(xc)));
    }
    
    if (xlow<xupp-1) {      // if value found, low and upp surround it, unlike standard binary search where low falls on it
        if (col<ncol-1) bmerge_r(xlow, xupp, ilow, iupp, col+1, 1, 1);  // final two 1's are lowmax and uppmax
        else {
            int len = xupp-xlow-1;
            if (len>1) allLen1[0] = FALSE;
            for (j=ilow+1; j<iupp; j++) {   // usually iterates once only for j=ir
                k = o ? o[j]-1 : j;
                retFirst[k] = xlow+2;       // extra +1 for 1-based indexing at R level
                retLength[k]= len; 
            }
        }
    }
    else if (roll!=0.0 && col==ncol-1) {
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
            if ( (   (roll>0.0 && (!lowmax || xlow>xlowIn) && (xupp<xuppIn || !uppmax || rollends[1]))
                  || (roll<0.0 && xupp==xuppIn && uppmax && rollends[1]) )
              && (   (TYPEOF(ic)==REALSXP && (REAL(ic)[ir]-REAL(xc)[XIND(xlow)]-rollabs<1e-6 || 
                                              REAL(ic)[ir]-REAL(xc)[XIND(xlow)] == rollabs)) // #1007 fix
                  || (TYPEOF(ic)<=INTSXP && (double)(INTEGER(ic)[ir]-INTEGER(xc)[XIND(xlow)])-rollabs<1e-6 ) 
                  || (TYPEOF(ic)==STRSXP)   )) {
                retFirst[ir] = xlow+1;
                retLength[ir] = 1;
            } else if
               (  (  (roll<0.0 && (!uppmax || xupp<xuppIn) && (xlow>xlowIn || !lowmax || rollends[0]))
                  || (roll>0.0 && xlow==xlowIn && lowmax && rollends[0]) )
              && (   (TYPEOF(ic)==REALSXP && (REAL(xc)[XIND(xupp)]-REAL(ic)[ir]-rollabs<1e-6 || 
                                              REAL(xc)[XIND(xupp)]-REAL(ic)[ir] == rollabs)) // 1007 fix
                  || (TYPEOF(ic)<=INTSXP && (double)(INTEGER(xc)[XIND(xupp)]-INTEGER(ic)[ir])-rollabs<1e-6 )
                  || (TYPEOF(ic)==STRSXP)   )) {
                retFirst[ir] = xupp+1;   // == xlow+2
                retLength[ir] = 1;
            }
        }
        if (iupp-ilow > 2 && retFirst[ir]!=NA_INTEGER) {  // >=2 equal values in the last column being rolling to the same point.  
            for (j=ilow+1; j<iupp; j++) {                 // will rewrite retFirst[ir] to itself, but that's ok
                if (o) k=o[j]-1; else k=j;
                retFirst[k] = retFirst[ir];
                retLength[k]= retLength[ir]; 
            }
        }
    }
    if (ilow>ilowIn && (xlow>xlowIn || (roll!=0.0 && col==ncol-1)))
        bmerge_r(xlowIn, xlow+1, ilowIn, ilow+1, col, lowmax, uppmax && xlow+1==xuppIn);
    if (iupp<iuppIn && (xupp<xuppIn || (roll!=0.0 && col==ncol-1)))
        bmerge_r(xupp-1, xuppIn, iupp-1, iuppIn, col, lowmax && xupp-1==xlowIn, uppmax);
}

// HOPEFULLY NOT NEEDED ANYMORE BUT JUST IN CASE...
// // adapted from util.c::do_enc2, here so that we can directly update columns
// SEXP setencoding(SEXP s, SEXP enc_utf8, SEXP enc_latin1) {
//     R_xlen_t i;
//     SEXP el;
//     for (i=0; i<XLENGTH(s); i++) {
//         el = STRING_ELT(s, i);
//         if (el == NA_STRING) continue;
//         if (LOGICAL(enc_utf8)[0]) {
//             if (IS_ASCII(el) || IS_UTF8(el)) continue;
//             SET_STRING_ELT(s, i, mkCharCE(translateCharUTF8(el), CE_UTF8));
//         } else if (ENC_KNOWN(el)) {
//             if (IS_ASCII(el) || (LOGICAL(enc_latin1)[0] && IS_LATIN1(el))) continue;
//             if (LOGICAL(enc_latin1)[0])
//                 SET_STRING_ELT(s, i, mkCharCE(translateChar(el), CE_LATIN1));
//             else 
//                 SET_STRING_ELT(s, i, mkChar(translateChar(el)));
//         }
//     }
//     return (R_NilValue);
// }

// SEXP is_encoding_marked(SEXP x) {

//     int i, n=XLENGTH(x);
//     SEXP s;
//     for (i=0; i<n; i++) {
//         s = STRING_ELT(x, i);
//         if (s != NA_STRING && !IS_ASCII(s))
//             break;
//     }
//     return (ScalarLogical(i!=n));
// }
