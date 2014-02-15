#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#define ENCODING(x) (LEVELS(x) & 76) // get encoding
// LATIN1_MASK (1<<2) | UTF8_MASK (1<<3) | ASCII_MASK (1<<6)

/* for isSortedList */
#define FLIP_SIGN_BIT   0x8000000000000000
#define RESET_NA_NAN    0x0000ffffffffffff
#define SET_NA_NAN_COMP 0xfff8000000000000 // we can directly set complement in hack_na_nan

// change NA/NaN so that their values are consistent across 32/64 bit
// see fastradixdouble for the issues with NA/NaN for why we do it this way
// for comparison - flip'em by sign bit - then compare in unsigned form...
unsigned long long flip_cmp_double(unsigned long long f, Rboolean isnan) {
    if (isnan) {                  // TO DO: ATM, for both i and i-1 we make checks here. if done by ref. we could try to eliminate check at i-1.
        f &= RESET_NA_NAN;
        f |= SET_NA_NAN_COMP;
    }
    unsigned long long mask = -(long long)(f >> 63) | FLIP_SIGN_BIT;
    return f ^ mask;
}

/* for binary search */
// following Kevin's suggestion
// To avoid 'dereferencing type-punned pointer will break strict-aliasing rules' 
// in recent versions of gcc, casting first and then returning
static unsigned long long R_NA_unsigned_long() {
    unsigned long long *ans = (unsigned long long*)(&NA_REAL);
    return (*ans);
}

static unsigned long long R_NaN_unsigned_long() {
    unsigned long long *ans = (unsigned long long*)(&R_NaN);
    return (*ans);
}

int StrCmp(SEXP x, SEXP y);   // in countingcharacter.c

/*
Implements binary search (a.k.a. divide and conquer).
http://en.wikipedia.org/wiki/Binary_search
http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary
http://googleresearch.blogspot.com/2006/06/extra-extra-read-all-about-it-nearly.html.
Potential integer overflow in (low+upp)/2 is avoided using (low+(upp-low)/2).
Differences over standard binary search (e.g. bsearch in stdlib.h) :
  o list of vectors (key of many columns) of different types
  o ties (groups)
  o options to join to prevailing value (roll join a.k.a locf)
*/

static double tol;  // TO DO: remove and use twiddle
static SEXP i, x;
static int ncol, *icols, *xcols, *o, *retFirst, *retLength, *allLen1, *rollends;
static unsigned long long R_UNSIGNED_LONG_NA_REAL;
static unsigned long long R_UNSIGNED_LONG_NAN_REAL;
static double roll, rollabs;
static Rboolean nearest=FALSE, enc_warn=TRUE;

extern SEXP forder();

void bmerge_r(int xlow, int xupp, int ilow, int iupp, int col, int lowmax, int uppmax);

SEXP binarysearch(SEXP iArg, SEXP xArg, SEXP icolsArg, SEXP xcolsArg, SEXP isorted, SEXP rollarg, SEXP rollendsArg, SEXP nomatch, SEXP tolerance, SEXP retFirstArg, SEXP retLengthArg, SEXP allLen1Arg)
{
    int xN, iN, protecti=0;

    // get value of NA and NaN in unsigned long to check for TYPE=REAL case below.  TO DO: replace with twiddle()
    R_UNSIGNED_LONG_NA_REAL  = R_NA_unsigned_long();
    R_UNSIGNED_LONG_NAN_REAL = R_NaN_unsigned_long();
    tol = REAL(tolerance)[0];   // TO DO: remove
    roll = 0.0;
    nearest = FALSE;
    enc_warn = TRUE;
    if (isString(rollarg)) {
        if (strcmp(CHAR(STRING_ELT(rollarg,0)),"nearest") != 0) error("roll is character but not 'nearest'");
        roll=1.0; nearest=TRUE;       // the 1.0 here is just any non-0.0, so roll!=0.0 can be used later
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
    
    if (nearest && TYPEOF(VECTOR_ELT(i, icols[ncol-1]-1))==STRSXP) error("roll='nearest' can't be applied to a character column, yet.");
         
    for (int j=0; j<iN; j++) {
        // defaults need to populated here as bmerge_r may well not touch many locations, say if the last row of i is before the first row of x.
        retFirst[j] = INTEGER(nomatch)[0];   // default to no match for NA goto below
        // retLength[j] = 0;   // TO DO: do this to save the branch below and later branches at R level to set .N to 0
        retLength[j] = INTEGER(nomatch)[0]==0 ? 0 : 1;
    }
    allLen1[0] = TRUE;  // All-0 and All-NA are considered all length 1 according to R code currently. Really, it means any(length>1).
    
    o = NULL;
    if (!LOGICAL(isorted)[0]) {
        SEXP oSxp = PROTECT(forder(i, icolsArg, ScalarLogical(FALSE), ScalarLogical(TRUE)));
        protecti++;
        if (!LENGTH(oSxp)) o = NULL; else o = INTEGER(oSxp);
    }
    
    bmerge_r(-1,xN,-1,iN,0,1,1);
    
    UNPROTECT(protecti);
    return(R_NilValue);
}

static union {
  int i;
  double d;
  SEXP s;
} ival, xval;

static int mid, tmplow, tmpupp;  // global to save them being added to recursive stack. Maybe optimizer would do this anyway.
static SEXP ic, xc;
static unsigned long long *ic_ul, *xc_ul;  // for NA/NaN numerics.   TO DO: remove.

void bmerge_r(int xlowIn, int xuppIn, int ilowIn, int iuppIn, int col, int lowmax, int uppmax)
// col is >0 and <=ncol-1 if this range of [xlow,xupp] and [ilow,iupp] match up to but not including that column
// lowmax=1 if xlowIn is the lower bound of this group (needed for roll)
// uppmax=1 if xuppIn is the upper bound of this group (needed for roll)
{
    int xlow=xlowIn, xupp=xuppIn, ilow=ilowIn, iupp=iuppIn, j, k, ir, lir; 
    ir = lir = ilow + (iupp-ilow)/2;           // lir = logical i row.
    if (o) ir = o[lir]-1;                      // ir = the actual i row if i were ordered

    ic = VECTOR_ELT(i,icols[col]-1);  // ic = i column
    xc = VECTOR_ELT(x,xcols[col]-1);  // xc = x column
    // it was checked in bmerge that the types are equal
    
    switch (TYPEOF(xc)) {
    case LGLSXP : case INTSXP :   // including factors
        ival.i = INTEGER(ic)[ir];
        // if (lval.i==NA_INTEGER) goto nextlr; // TO DO: remove 'if' if NA are allowed in key (could do)
        //                                      // break breaks out of this switch, but we want to break this loop
        while(xlow < xupp-1) {
            mid = xlow+((xupp-xlow)/2);   // (upp+low)/2 may overflow
            xval.i = INTEGER(xc)[mid];
            if (xval.i<ival.i) {          // relies on NA_INTEGER == INT_MIN, tested in init.c
                xlow=mid;
            } else if (xval.i>ival.i) {   // TO DO: is *(&xlow, &xupp)[0|1]=mid more efficient than branch?
                xupp=mid;
            } else { // xval.i == ival.i)
                // branch mid to find start and end of this group in this column
                // TO DO?: not if mult=first|last and col<ncol-1
                tmplow = mid;
                tmpupp = mid;
                while(tmplow<xupp-1) {
                    mid = tmplow+((xupp-tmplow)/2);
                    xval.i = INTEGER(xc)[mid];
                    if (xval.i == ival.i) tmplow=mid; else xupp=mid;
                }
                while(xlow<tmpupp-1) {
                    mid = xlow+((tmpupp-xlow)/2);
                    xval.i = INTEGER(xc)[mid];
                    if (xval.i == ival.i) tmpupp=mid; else xlow=mid;
                }
                // xlow and xupp now surround the group in xc, we only need this range for the next column
                break;
            }
        }
        tmplow = lir;
        tmpupp = lir;
        while(tmplow<iupp-1) {   // TO DO: could double up from lir rather than halving from iupp
            mid = tmplow+((iupp-tmplow)/2);
            xval.i = INTEGER(ic)[ o ? o[mid]-1 : mid ];   // reuse xval to search in i
            if (xval.i == ival.i) tmplow=mid; else iupp=mid;
        }
        while(ilow<tmpupp-1) {
            mid = ilow+((tmpupp-ilow)/2);
            xval.i = INTEGER(ic)[ o ? o[mid]-1 : mid ];
            if (xval.i == ival.i) tmpupp=mid; else ilow=mid;
        }
        // ilow and iupp now surround the group in ic, too
        break;
    case STRSXP :
        // lval_c and rval_c are added to address bugs reg. character encoding - bugs #5159 and #5266
        ival.s = STRING_ELT(ic,ir);
        // if (lval.s==NA_STRING) goto nextlr;
        while(xlow < xupp-1) {
            mid = xlow+((xupp-xlow)/2);
            xval.s = STRING_ELT(xc,mid);
            // if (xval.s == NA_STRING) error("NA not allowed in keys"); should be in setkey as some NA may be missed by the binary search here.
            if (ival.s == xval.s) {   // TO DO: StrCmp does this inside it, so why not call StrCmp on line above to allow the same string in different encodings to match
                tmplow = mid;
                tmpupp = mid;
                while(tmplow<xupp-1) {
                    mid = tmplow+((xupp-tmplow)/2);
                    xval.s = STRING_ELT(xc,mid);
                    if (ival.s == xval.s) tmplow=mid; else xupp=mid;
                }
                while(xlow<tmpupp-1) {
                    mid = xlow+((tmpupp-xlow)/2);
                    xval.s = STRING_ELT(xc,mid);
                    if (ival.s == xval.s) tmpupp=mid; else xlow=mid;
                }
                break;
            } else if (StrCmp(xval.s, ival.s) < 0) {
            // TO DO: Reinvestigate non-ASCII. Switch can be a column level check that all is ascii
            // (setkey can check and mark). Used to use Rf_Scollate but was removed from r-devel API.
            // We're using the last line of scmp in sort.c since we already dealt with NA and == above
                if (enc_warn && ENCODING(ival.s) != ENCODING(xval.s) && ival.s != NA_STRING && xval.s != NA_STRING) {
                    warning("Encoding of '%s' in X is different from '%s' in Y in join X[Y]. Joins are not implemented yet for non-identical character encodings and therefore likely to contain unexpected results for those entries. Please ensure that character columns have identical encodings for joins.", CHAR(xval.s), CHAR(ival.s));
                    enc_warn=FALSE; // just warn once
                }
                xlow=mid;
            } else {
                if (enc_warn && ENCODING(ival.s) != ENCODING(xval.s) && ival.s != NA_STRING && xval.s != NA_STRING) {
                    warning("Encoding of '%s' in X is different from '%s' in Y in join X[Y]. Joins are not implemented yet for non-identical character encodings and therefore likely to contain unexpected results for those entries. Please ensure that character columns have identical encodings for joins.", CHAR(xval.s), CHAR(ival.s));
                    enc_warn=FALSE; // just warn once
                }
                xupp=mid;
            }
        }
        tmplow = lir;
        tmpupp = lir;
        while(tmplow<iupp-1) {
            mid = tmplow+((iupp-tmplow)/2);
            xval.s = STRING_ELT(ic, o ? o[mid]-1 : mid);
            if (xval.s == ival.s) tmplow=mid; else iupp=mid;
        }
        while(ilow<tmpupp-1) {
            mid = ilow+((tmpupp-ilow)/2);
            xval.s = STRING_ELT(ic, o ? o[mid]-1 : mid);
            if (xval.s == ival.s) tmpupp=mid; else ilow=mid;
        }
        break;
    case REALSXP :
        // hack to pull out the binary search results for NA and NaN.  TO DO: revisit
        ic_ul = (unsigned long long*)REAL(ic);
        xc_ul = (unsigned long long*)REAL(xc);

        ival.d = REAL(ic)[ir];
        unsigned long long ival_ud, xval_ud;
        ival_ud = ic_ul[ir];
        // if (ISNAN(ival.d)) goto nextlr;
        while(xlow < xupp-1) {
            mid = xlow+((xupp-xlow)/2);
            xval.d = REAL(xc)[mid];
            xval_ud = xc_ul[mid];
             // if ival is NA and xval is NaN, upp=mid *must* execute - not sure how to incorporate this into existing if-statements
            if (xval_ud == R_UNSIGNED_LONG_NAN_REAL && ival_ud == R_UNSIGNED_LONG_NA_REAL) {
                xupp = mid;
            } else if (xval.d<ival.d-tol || (ISNAN(xval.d) && xval_ud != ival_ud)) {
                xlow=mid;
            } else if (xval.d>ival.d+tol || (ISNAN(ival.d) && ival_ud != xval_ud)) {
                xupp=mid;
            } else { // xval.d == ival.d) 
                tmplow = mid;
                tmpupp = mid;
                while(tmplow<xupp-1) {
                    mid = tmplow+((xupp-tmplow)/2);
                    xval.d = REAL(xc)[mid];
                    xval_ud = xc_ul[mid];
                    if (fabs(xval.d-ival.d)<tol || ival_ud == xval_ud) tmplow=mid; else xupp=mid;
                }
                while(xlow<tmpupp-1) {
                    mid = xlow+((tmpupp-xlow)/2);
                    xval.d = REAL(xc)[mid];
                    xval_ud = xc_ul[mid];
                    if (fabs(xval.d-ival.d)<tol || ival_ud == xval_ud) tmpupp=mid; else xlow=mid;
                }
                break;
            }
        }
        tmplow = lir;
        tmpupp = lir;
        while(tmplow<iupp-1) {
            mid = tmplow+((iupp-tmplow)/2);
            xval.d = REAL(ic)[ o ? o[mid]-1 : mid ];
            if (xval.d == ival.d) tmplow=mid; else iupp=mid;
        }
        while(ilow<tmpupp-1) {
            mid = ilow+((tmpupp-ilow)/2);
            xval.d = REAL(ic)[ o ? o[mid]-1 : mid ];
            if (xval.d == ival.d) tmpupp=mid; else ilow=mid;
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
                if (o) k=o[j]-1; else k=j;
                retFirst[k] = xlow+2;       // extra +1 for 1-based indexing at R level
                retLength[k]= len; 
            }
        }
    }
    else if (roll!=0.0 && col==ncol-1) {
        // runs once per i row (not each search test), so not hugely time critical
        if (xlow != xupp-1 || xlow<xlowIn || xupp>xuppIn) error("Internal error: xlow!=xupp-1 || xlow<xlowIn || xupp>xuppIn");
        if (nearest) {   // value of roll ignored currently when nearest
            if ( (!lowmax || xlow>xlowIn) && (!uppmax || xupp<xuppIn) ) {
                if (  ( TYPEOF(ic)==REALSXP && REAL(ic)[ir]-REAL(xc)[xlow] <= REAL(xc)[xupp]-REAL(ic)[ir] )
                   || ( TYPEOF(ic)<=INTSXP && INTEGER(ic)[ir]-INTEGER(xc)[xlow] <= INTEGER(xc)[xupp]-INTEGER(ic)[ir] )) {
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
              && (   (TYPEOF(ic)==REALSXP && REAL(ic)[ir]-REAL(xc)[xlow]-rollabs<tol)
                  || (TYPEOF(ic)<=INTSXP && (double)(INTEGER(ic)[ir]-INTEGER(xc)[xlow])-rollabs<tol ) 
                  || (TYPEOF(ic)==STRSXP)   )) {
                retFirst[ir] = xlow+1;
                retLength[ir] = 1;
            } else if
               (  (  (roll<0.0 && (!uppmax || xupp<xuppIn) && (xlow>xlowIn || !lowmax || rollends[0]))
                  || (roll>0.0 && xlow==xlowIn && lowmax && rollends[0]) )
              && (   (TYPEOF(ic)==REALSXP && REAL(xc)[xupp]-REAL(ic)[ir]-rollabs<tol)
                  || (TYPEOF(ic)<=INTSXP && (double)(INTEGER(xc)[xupp]-INTEGER(ic)[ir])-rollabs<tol )
                  || (TYPEOF(ic)==STRSXP)   )) {
                retFirst[ir] = xupp+1;   // == xlow+2
                retLength[ir] = 1;
            }
        }
        if (iupp-ilow > 2 && retFirst[ir]!=NA_INTEGER) {  // >=2 equal values in the last column being rolling to the same point.  
            for (j=ilow+1; j<iupp; j++) {                 // will rewrite retFirst[ir] to itself, but that's ok
                if (o) k=o[j]-1; else k=j;
                retFirst[k] = retFirst[ir];
                retLength[k]= 1; 
            }
        }
    }
    if (ilow>ilowIn && (xlow>xlowIn || (roll!=0.0 && col==ncol-1)))
        bmerge_r(xlowIn, xlow+1, ilowIn, ilow+1, col, lowmax, uppmax && xlow+1==xuppIn);
    if (iupp<iuppIn && (xupp<xuppIn || (roll!=0.0 && col==ncol-1)))
        bmerge_r(xupp-1, xuppIn, iupp-1, iuppIn, col, lowmax && xupp-1==xlowIn, uppmax);
}


SEXP isSortedList(SEXP l, SEXP w, SEXP tolerance)
{
    R_len_t i,j,nrow,ncol;
    Rboolean b;
    SEXP v;
    unsigned long long *vu;
    double tol = REAL(tolerance)[0];
    ncol = length(l);
    nrow = length(VECTOR_ELT(l,0));
    if (NA_INTEGER != INT_MIN) error("Internal error: NA_INTEGER (%d) != INT_MIN (%d).", NA_INTEGER, INT_MIN);
    if (NA_INTEGER != NA_LOGICAL) error("Have assumed NA_INTEGER == NA_LOGICAL (currently R_NaInt). If R changes this in future (seems unlikely), an extra case is required; a simple change.");
    for (j=1; j<ncol; j++) if (length(VECTOR_ELT(l,j)) != nrow) error("Column %d is length %d which differs from length of column 1 (%d).", j+1, length(VECTOR_ELT(l,j)), nrow);
    if (!isInteger(w)) error("vector w of columns to test isn't an integer vector");
    for (i=1; i<nrow; i++) {
        b = TRUE;
        j = -1;
        while (b && ++j<LENGTH(w)-1) {
            v=VECTOR_ELT(l,INTEGER(w)[j]-1);
            switch (TYPEOF(v)) {      // TO DO: change switch to *(EQ[type])(v,i,i-1)
            case INTSXP : case LGLSXP :
                b = INTEGER(v)[i]==INTEGER(v)[i-1]; break;      // DONE: NA is INT_MIN (checked above) here, so it just works normally
            case STRSXP :
                b = STRING_ELT(v,i)==STRING_ELT(v,i-1); break;  // NA seems to work here as well
            case REALSXP :
                vu = (unsigned long long*)REAL(v); // allows equality checking very straightforward (incl. NA, NaN, -Inf and Inf)
                b = (vu[i] == vu[i-1] || fabs(REAL(v)[i] - REAL(v)[i-1])<tol); break; // DONE: NA, NaN, -Inf and Inf
            default :
                error("Type '%s' not supported", type2char(TYPEOF(v))); 
            }
        }
        v = VECTOR_ELT(l,INTEGER(w)[j]-1);
        switch (TYPEOF(v)) {
        case INTSXP : case LGLSXP :
            b = INTEGER(v)[i]<INTEGER(v)[i-1]; break;    // DONE: Check assumption NA largest negative
        case STRSXP :
            b = StrCmp(STRING_ELT(v,i), STRING_ELT(v,i-1))<0; break;
        case REALSXP :
            vu = (unsigned long long*)REAL(v);
            b = flip_cmp_double(vu[i], ISNAN(REAL(v)[i])) < flip_cmp_double(vu[i-1], ISNAN(REAL(v)[i-1])); break; // DONE: NA, NaN, -Inf and Inf
        default :
            error("Type '%s' not supported", type2char(TYPEOF(v))); 
        }
        if (b) return(ScalarLogical(FALSE));
    }
    return(ScalarLogical(TRUE));
}

// require(data.table)
// set.seed(1L)
// DT <- setDT(lapply(1:2, function(x) sample(c(-1e4:1e4), 1e8, TRUE)))
// system.time(setkey(DT))

 // no NA/NaN implementation (Matthew's way using REAL(.) < REAL(.)')
// system.time(data.table:::is.sorted(as.list(DT))[1L])
//  user  system elapsed 
// 0.830   0.001   0.850 
// system.time(data.table:::is.sorted(as.list(DT)))
//  user  system elapsed 
// 0.826   0.002   0.828 

// with unsigned long long implementation:
// system.time(data.table:::is.sorted(as.list(DT))[1L])
//  user  system elapsed 
// 1.168   0.002   1.172 
// system.time(data.table:::is.sorted(as.list(DT)))
//  user  system elapsed 
// 1.173   0.002   1.187 

// conclusion: about .33 sec checking NA/NaN/-Inf/Inf! Not bad, I suppose? Can we make it quicker?
// Note that this is on 1e8. On 1e7 size, this doesn't matter at all as the time to check is 0.07 sec.


/*SEXP sortedintegermatch (SEXP ans, SEXP left, SEXP right, SEXP nomatch)
{
    // As sortedstringmatch, see comments above, but for integers.
    int lr,nr,low,mid,upp;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);
    nr = length(right); 
    for (lr=0; lr < length(left); lr++) {
        low = -1;
        upp = nr;
        while(low < upp-1) {
            mid = low + (upp-low)/2;
            if (INTEGER(left)[lr]>INTEGER(right)[mid]) {
                low=mid;
            } else {
                upp=mid;
            }
        }
        INTEGER(ans)[lr] = (upp<nr && INTEGER(left)[lr] == INTEGER(right)[upp]) ? upp+1 : INTEGER(nomatch)[0];
    }
    return(R_NilValue);
}*/


