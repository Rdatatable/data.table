#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>
extern int Rf_Scollate();   // #include <Defn.h> failed to find Defn.h in development, so this extern stops the warning

#ifdef BUILD_DLL
// For Windows only
#define EXPORT __declspec(dllexport)
EXPORT SEXP binarysearch();
EXPORT SEXP sortedstringmatch ();
EXPORT SEXP sortedintegermatch ();
#endif

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

SEXP binarysearch(SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP isorted, SEXP roll, SEXP rolltolast, SEXP retFirst, SEXP retLast)
{
    // If the left table is large and the right table is large, then sorting the left table first may be
    // quicker depending on how long to sort the left table. This is up to user via use of J() or SJ()
    
    int lr,nr,low,mid,upp,coln,col,lci,rci;
    int prevlow, prevupp, type, newlow, newupp, size;
    union {
        int i;
        SEXP str;
    } lval, rval;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);
    nr = LENGTH(VECTOR_ELT(right,0));   // num rows in right hand table
    coln = LENGTH(leftcols);    // there may be more sorted columns in the right table, but we just need up to the number in the left.
    low=-1;
    for (lr=0; lr < LENGTH(VECTOR_ELT(left,0)); lr++) {  // left row
        upp = prevupp = nr;
        low = prevlow = (LOGICAL(isorted)[0]) ? low : -1;
        INTEGER(retFirst)[lr] = INTEGER(retLast)[lr] = 0;   // default to no match for 2 goto's below
        for(col=0; col<coln && low<upp-1; col++) {
            lci = INTEGER(leftcols)[col];
            rci = INTEGER(rightcols)[col];
            type = TYPEOF(VECTOR_ELT(left, lci));
            switch (type) {
            case INTSXP : 
                lval.i = INTEGER(VECTOR_ELT(left,lci))[lr];        // factors are also INTSXP but we won't see factors here
                if (lval.i==NA_INTEGER) goto nextlr;
                size=sizeof(int);  // e.g. 4
                break;
            case LGLSXP :
                lval.i = LOGICAL(VECTOR_ELT(left,lci))[lr];
                if (lval.i==NA_LOGICAL) goto nextlr;
                size=sizeof(int);  // e.g. 4
                break;
            case STRSXP :
                //To revisit:
                //lval.str = STRING_ELT(VECTOR_ELT(left,lci),lr);
                //if (lval.str==NA_STRING) goto nextlr;
                //size=sizeof(SEXP);    // e.g. 4 (since SEXP is a pointer)
                //break;
            case REALSXP :
                // ************* TO DO *******************
                // Have discussed a decimal() class - fixed decimal points stored as integer
            default:
                error("only types internally integer are allowed in key. Type %d found", type);
            }
            if (type != TYPEOF(VECTOR_ELT(right, rci))) error("column %d of x has different type to column %d of i", rci, lci);
            prevlow = low;
            prevupp = upp;
            while(low < upp-1) {
                mid = low+((upp-low)/2);
                memcpy(&rval, (char *)DATAPTR(VECTOR_ELT(right,rci)) + mid*size, size);
                // if (isstr) rval.str = STRING_ELT(VECTOR_ELT(right,rci),mid);
                //else rval.i = INTEGER(VECTOR_ELT(right,rci))[mid];
                //if (rval==NA_INTEGER) error("NA in key columns");  // an NA in the key might not be detected. only detected if mid falls on it
                if (memcmp(&rval,&lval,size)==0) {        // we can't do rval==lval. Not because it might be string (it would be fine - see scmp in sort.c doing == on strings in cache) but because int (4 or 8) might not be the same size as str (4 or 8) on some platforms and this could change over time. In the union we don't want any unused space at the end to effect this equality test.
                    // split mid in two  and find start and end of this == series
                    newlow = mid;
                    newupp = mid;
                    while(newlow<upp-1) {  // consider switching off these while if only first or last are required. But these loops will be within the final page's and all in cache so unlikely to make much difference.
                        mid = newlow+((upp-newlow)/2);
                        memcpy(&rval, (char *)DATAPTR(VECTOR_ELT(right,rci)) + mid*size, size);
                        if (memcmp(&rval,&lval,size)==0) newlow=mid; else upp=mid;  //replace if with ?: using myvec[rval==lval];
                    }
                    while(low<newupp-1) {
                        mid = low+((newupp-low)/2);
                        memcpy(&rval, (char *)DATAPTR(VECTOR_ELT(right,rci)) + mid*size, size);
                        if (memcmp(&rval,&lval,size)==0) newupp=mid; else low=mid;
                    }
                    break;   // stop the while for this group and go to next column. If its the last column low and upp will surround the group
                }
                if (rval.i<lval.i) {   // if (type==INTSXP ? rval.i<lval.i : Rf_Scollate(rval.str, lval.str)<0) {  // for strings, we can jump to last line of scmp since we already dealt with NA and == above.
                    low=mid;
                } else {
                    upp=mid;
                }
            }
        }
        if (low<upp-1) {
            INTEGER(retFirst)[lr] = (low+1)+1;
            INTEGER(retLast)[lr] = (upp-1)+1;
        } else {
            if (low>prevlow && col==coln && (LOGICAL(roll)[0] ||
                                            (LOGICAL(rolltolast)[0] && upp<prevupp))) {
                INTEGER(retFirst)[lr] = INTEGER(retLast)[lr] = low+1;
                low -= 1; // for test 148
            }
        }
        nextlr :;
    }
    return(R_NilValue);
}





SEXP sortedstringmatch (SEXP ans, SEXP left, SEXP right, SEXP nomatch)
{
    // Called from sortedmatch.r.
    // Primarily for matching factor levels from i to x in [.data.table before calling binarysearch.
    // right must be a sorted string vector.
    // left may be unsorted but it currently has no advantage if left is also sorted (TO DO).
    // Neither left or right have duplicates, or any NAs, as is the case for factor levels.
    // ans is the result, the locations of left in right, possibly including NA

    int lr,nr,low,mid,upp;
    nr = length(right);
    for (lr=0; lr < length(left); lr++) {
        low = -1;   // TO DO: leave low alone if left is sorted
        upp = nr;
        while(low < upp-1) {
            mid = low + (upp-low)/2;
            if (Rf_Scollate(STRING_ELT(right,mid), STRING_ELT(left,lr))<0) {   // TO DO: as in binarysearch, consider replacing with ?: on 2 item vector
                low = mid;
            } else {
                upp = mid;
            }
        }
        INTEGER(ans)[lr] = (upp<nr && STRING_ELT(left,lr)==STRING_ELT(right,upp)) ? upp+1 : INTEGER(nomatch)[0];
    }
    return(R_NilValue);
}


SEXP sortedintegermatch (SEXP ans, SEXP left, SEXP right, SEXP nomatch)
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
}


