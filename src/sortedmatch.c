#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>


#ifdef BUILD_DLL
// For Windows only
#define EXPORT __declspec(dllexport)
EXPORT SEXP sortedmatchfirst ();
EXPORT SEXP sortedmatchlast ();
EXPORT SEXP unsortedmatchfirst ();
EXPORT SEXP unsortedmatchlast ();
EXPORT SEXP sortedstringmatch ();
EXPORT SEXP sortedintegermatch ();
#endif


static inline double MIN(double a, double b) {
    return(a < b ? a : b);
}

static inline double MAX(double a, double b) {
    return(a > b ? a : b);
}

SEXP sortedmatchfirst (SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP i, SEXP roll, SEXP rolltolast)
{
    //  x and y are both sorted data.table's i.e. plain old lists
    //  number of left cols and right cols must be the same - the match is done on equality between these columns
    //  outer: 1=first, 2=last, 3=all
    //  i is the result
    //  The web is full of efficient binary search algorithms, for example google for 'binary search':
    //  http://en.wikipedia.org/wiki/Binary_search
    //  http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary
    //  DONE: take account of integer overflow in upp+low : http://googleresearch.blogspot.com/2006/06/extra-extra-read-all-about-it-nearly.html.   (Actually we just avoid it by placing restriction on max number of rows.)
    
    int lr,nr,low,mid,upp,coln,col,d,noteq,allbutlastequal_low,allbutlastequal_upp,dist,lval,rval;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);  // the code below assumes NA is internally a negative number.
    nr = LENGTH(VECTOR_ELT(right,0));   // num rows in right hand table
    if (nr<2) error("Must have at least 2 values in right table");  // code can be faster by not dealing with 0 and 1 rows.
    if (nr>(1<<30)) error("Table has more than 1.073 billion rows. Currently programmed defensively against integer flow in signed 32bit i.e. doubling of (2^31-1)/2. This limit can be doubled quite easily by a few changes in the C. Alternatively by restricting to 64bit OS we can remove this limit.");
    // 1<<30 is 2^30
    // note that in R as.integer(2^31-1) is the largest signed integer, as.integer(2^31) = NA.
    // see wikipedia for references to integer overflow in  (low+upp)/2.
    // Here we do more logic, and dealing with integer overflow in the stage 1 while would possibly add some if's and certainly increase I-cache hits a little. Easier just to remove the possibility of overflow.
    // Moving to long and restricting to 64-bit would solve the limit, but then couldn't run on 32-bit platforms.
    low = -1;
    upp = nr;
    coln = LENGTH(leftcols);    // there may be more sorted columns in the right table, but we just need up to the number in the left.
    for (lr=0; lr < LENGTH(VECTOR_ELT(left,0)); lr++) {  // left row
        for(col=0; col<coln; col++) {
            if (INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr] == NA_INTEGER) goto nextlr;
        }
        noteq = 1; // when upp was last tested for equality, store whether it was equal. Initial value is 1=TRUE=means not equal
        allbutlastequal_upp = 0;
        allbutlastequal_low = 0;
        d = -1;
        dist = 1;  // the increment for upp in the 1st while
        INTEGER(i)[lr] = 0;     // By default we return no match (0) for this left value. Necessary to set this now, for the short-circuit goto if we find NAs.
        // 1. first while moves the upp up to go past the next search value required in the left list.  The name of this function "sortedlist" the sorted refers to the lookup table,  the table being looked up is always sorted.
        while (upp<nr && d<0) { //  the first time upp==nr, so this won't run
            for(col=0; col<coln; col++) {    // TO DO: Don't need to look at all the cols each time. However to make this more efficient will require more C code, so we get into instruction cache thrash issues.  We do use the fact that if col1 is <. then we don't need to check the other columns.
                rval = INTEGER(VECTOR_ELT(right,INTEGER(rightcols)[col]))[upp];
                // So keys must be integer columns. Characters stored as factors are fine.
                if (rval==NA_INTEGER) {
                    d = NA_INTEGER;
                    break;
                } else {
                    lval = INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr];
                    d = rval - lval;        // both rval and lval are guaranteed to be non-NA at this point. therefore no integer overflow can occur on this line
                    if (d != 0) break;
                }
                // We want to allow NA's in sorted matches to not interrupt the sort order. Feasible I-Cache being hit by extra check every time for non-NA, possible TO DO in future to tackle.
                // The normal case for this is when we have 2 sorted factors, where the level match in R means we get NAs in the middle of the sorted level match. This is fine, desirable even. We simply skip such NAs and continue with the sorted match.
                // We must test for NA both here in stage 1 while (doesn't run for first lr) and also in stage 2 while below.
                //  Note that roll is not allowed (by calling R code) when the last i column is a factor. This doesn't make sense. roll is intended for last column dates i.e. integer.  This is required because NA due to the level match are not dealt with in this roll logic.
            }
            if (d<0) {      // This is where it is assumed that NA_INTEGER internally is a negative number. This is why setkey must call order with na.last=FALSE.  TO DO: potential problem if setkey is called with rev(colA), because then na.last should be true in that case.
                low = upp;
                upp += dist;            // potential integer overflow here
                dist *= 2;              // potential integer overflow here
                allbutlastequal_low = (col==coln-1) && d!=NA_INTEGER;
            } else {
                noteq=d;
                allbutlastequal_upp = (col==coln-1);    // for rolltolast i.e. roll join unless the returned value would be the last one e.g. if last price should not flat line after stock dies
            }
            upp = MIN(upp,nr);
        }
        // 2. second while is the usual binary search given that low and upp span the value required.
        while(low < upp-1) {
            mid = (low + upp) / 2;   // // potential integer overflow here
            // low priority TO DO: improve to some linear estimate with a distance measure, possibly optional 'linear index' argument.
            for(col=0; col<coln; col++) {    // TO DO: Don't need to look at all the cols each time. However to make this more efficient will require more C code, so we get into instruction cache thrash issues.  We do use the fact that if col1 is <. then we don't need to check the other columns.
                rval = INTEGER(VECTOR_ELT(right,INTEGER(rightcols)[col]))[mid];
                if (rval==NA_INTEGER) {
                    d = NA_INTEGER;
                    break;
                } else {
                    lval = INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr];
                    d = rval - lval;
                    if (d != 0) break;
                }
            }
            if (d<0) {
                low=mid;
                allbutlastequal_low = (col==coln-1) && d!=NA_INTEGER;    // for roll
            } else {
                noteq=d;      // use of noteq, saves an if() here. Saves the last time the upp was tested for all columns equal
                upp=mid;
                allbutlastequal_upp = (col==coln-1);    // for rolltolast
            }
        }
        if (noteq || upp == nr) {     // noteq is more likely that upp==nr, so put that left of the short-circuit ||.   Using fact that noteq>0 is considered true ie not equal
            if ((INTEGER(roll)[0] && allbutlastequal_low) ||
                (INTEGER(rolltolast)[0] && allbutlastequal_low && allbutlastequal_upp)) {
                INTEGER(i)[lr] = low+1;
                low -= 1;   // the same row may be roll for the next row in the search too, low needs to be set back so the mid is tested again and allbutlastequal gets checked again for the next row in the search
            } 
            // else  INTEGER(i)[lr] = 0;  Already set by default at the start of the for() through lr.
        } else {
            INTEGER(i)[lr] = upp+1;
        }
        nextlr :;
    }
    return(R_NilValue);
}


SEXP sortedmatchlast (SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP i, SEXP roll, SEXP rolltolast)
{
    //  Same as sortedmatchfirst, but in the case of multiple
    //  matches to the search key, this returns the last one, rather than the first.
    //  It could also be achieved with the multiple="all" then doing j=last(),  but this sortedmatchlast would be faster.
    //  Instead of upp falling on the matched value,  it will be low that falls on the matched line now.
    //  When there are no repeated key values.  sortedmatchlast and sortedmatchfirst should return precisely the same result.
    
    int lr,nr,low,mid,upp,coln,col,d,noteq,allbutlastequal_upp,allbutlastequal_low,dist,lval,rval;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);
    nr = LENGTH(VECTOR_ELT(right,0));
    if (nr<2) error("must have at least 2 rows in x");
    if (nr>(1<<30)) error("Table has more than 1.073 billion rows. Currently programmed defensively against integer flow in signed 32bit i.e. doubling of (2^31-1)/2. This limit can be doubled quite easily by a few changes in the C. Alternatively by restricting to 64bit OS we can remove this limit.");
    low = -1;
    upp = nr;
    coln = LENGTH(leftcols);
    for (lr=0; lr < LENGTH(VECTOR_ELT(left,0)); lr++) {
        for(col=0; col<coln; col++) {
            if (INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr] == NA_INTEGER) goto nextlr;
        }
        noteq = 1; // when *low* was last tested for equality, store whether it was equal.
        allbutlastequal_upp = 0;
        allbutlastequal_low = 0;
        d = -1;
        dist = 1;
        INTEGER(i)[lr] = 0;
        while (upp<nr && d<=0) {
            for(col=0; col<coln; col++) {
                rval = INTEGER(VECTOR_ELT(right,INTEGER(rightcols)[col]))[upp];
                if (rval==NA_INTEGER) {
                    d = NA_INTEGER;
                    break;
                } else {
                    lval = INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr];
                    d = rval - lval;
                    if (d != 0) break;
                }
            }
            if (d<=0) {
                low = upp;
                upp += dist;
                dist *= 2;
                allbutlastequal_low = (col==coln-1) && d!=NA_INTEGER;
                noteq=-d;
            } else {
                allbutlastequal_upp = (col==coln-1);
            }
            upp = MIN(upp,nr);
        }
        // This time, the while below has up bias to return the *last* in a sequence of repeated key values
        while(low < upp-1) {
            mid = (low + upp) / 2;
            for(col=0; col<coln; col++) {
                rval = INTEGER(VECTOR_ELT(right,INTEGER(rightcols)[col]))[mid];
                if (rval==NA_INTEGER) {
                    d = NA_INTEGER;
                    break;
                } else {
                    lval = INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr];
                    d = rval - lval;
                    if (d != 0) break;
                }
            }
            if (d<=0) {
                low=mid;
                noteq=-d;
                allbutlastequal_low = (col==coln-1) && d!=NA_INTEGER;
            } else {
                upp=mid;
                allbutlastequal_upp = (col==coln-1);
            }
        }
        if (noteq || low == -1) {
            if ((INTEGER(roll)[0] && allbutlastequal_low) ||
                (INTEGER(rolltolast)[0] && allbutlastequal_low && allbutlastequal_upp)) {
                INTEGER(i)[lr] = low+1;
                low -= 1;
            }
            // else  INTEGER(i)[lr] = 0;  Already set by default at the start of the for() through lr.
        } else {
            INTEGER(i)[lr] = low+1;
            low -= 1;
        }
        nextlr :;
    }
    return(R_NilValue);
}


// SEXP sortedmatchall (SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP i, SEXP roll, SEXP rolltolast)
// {
//
    // Again, very similar to sortedmatchfirst and sortedmatchlast.
    // This time, instead of the first or all, all the matches are returned.

    // This would most likely occur when a unique key is referenced by less columns than the key has e.g.
    //    a key (stock,date) could be looked up by stock only, to return all dates for that stock.

    // The function is passed in a list rather than a vector, and the possibly-multiple match rows
    // assigned into the list positions. The calling R function then unlists and takes the subset. This
    // allows us to easily create a list of data.tables as a result in future if prefered.
    // If the roll matches to repeated values, note that only the last one is taken as always in the roll.
    // Its only when there is an exact match to multiple that multiple are returned.

    // Another way is to pass in a 2 column i and return just the first and last row for each sequence,
    // of multiple match. Easier to do this than allocate the memory for the sequences inside the C.

    // We call sortmatechedfirst, then sortmatchlast, and then
    // take the sequences between the 2 to get the row ranges.  This re-uses the code above, means
    // less likely to be errors and is simpler.  It may be a little slower because we loose the benefit
    // of a localised search forward once the start of a range is found due to reduced page fetches.
// }



SEXP unsortedmatchfirst (SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP i, SEXP roll, SEXP rolltolast)
{
    // left table is unsorted, right table is sorted.
    // If the left table is large and the right table is large, then sorting the left table first may be
    // quicker depending on how long to sort the left table. This is up to user via use of J() or SJ()
    // The code in this function is smaller and may save I-cache.
    // POSSIBLE TO DO: split roll and rolltolast into seperate functions to reduce instruction cache useage
    
    int lr,nr,low,mid,upp,coln,col,d=0,noteq,allbutlastequal_low,allbutlastequal_upp,lval,rval;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);
    nr = LENGTH(VECTOR_ELT(right,0));   // num rows in right hand table
    if (nr<2) error("must have at least 2 rows in x");  // code can be fast by not dealing with 0 and 1 rows.
    if (nr>(1<<30)) error("Table has more than 1.073 billion rows. Currently programmed defensively against integer flow in signed 32bit i.e. doubling of (2^31-1)/2. This limit can be doubled quite easily by a few changes in the C. Alternatively by restricting to 64bit OS we can remove this limit.");
    coln = LENGTH(leftcols);    // there may be more sorted columns in the right table, but we just need up to the number in the left.
    
    for (lr=0; lr < LENGTH(VECTOR_ELT(left,0)); lr++) {  // left row
        for(col=0; col<coln; col++) {
            if (INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr] == NA_INTEGER) goto nextlr;
        }
        noteq = 1; // when upp was last tested for equality, store whether it was equal. Initial value is 1=TRUE=means not equal
        allbutlastequal_upp = 0;
        allbutlastequal_low = 0;
        upp = nr;
        low = -1;
        INTEGER(i)[lr] = 0;
        // so in this unsorted match, we always do binary search on the whole table, for each of the lookups in the left table. The left table isn't sorted so we don't have the luxury of a single simultaneous pass through both tables.
        while(low < upp-1) {
            mid = (low + upp) / 2;
            for(col=0; col<coln; col++) {
                rval = INTEGER(VECTOR_ELT(right,INTEGER(rightcols)[col]))[mid];
                if (rval==NA_INTEGER) {
                    d = NA_INTEGER;
                    break;
                } else {
                    lval = INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr];
                    d = rval - lval;
                    if (d != 0) break;
                }
            }
            if (d<0) {
                low=mid;
                allbutlastequal_low = (col==coln-1) && d!=NA_INTEGER;
            } else {
                noteq=d;
                upp=mid;
                allbutlastequal_upp = (col==coln-1);
            }
        }
        if (noteq || upp == nr) {
            if ((INTEGER(roll)[0] && allbutlastequal_low) ||
                (INTEGER(rolltolast)[0] && allbutlastequal_low && allbutlastequal_upp)) {
                INTEGER(i)[lr] = low+1;
            }
            // else INTEGER(i)[lr] = 0;
        } else {
            INTEGER(i)[lr] = upp+1;
        }
        nextlr :;
    }
    return(R_NilValue);
}



SEXP unsortedmatchlast (SEXP left, SEXP right, SEXP leftcols, SEXP rightcols, SEXP i, SEXP roll, SEXP rolltolast)
{
    int lr,nr,low,mid,upp,coln,col,d=0,noteq,allbutlastequal_low,allbutlastequal_upp,lval,rval;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);
    nr = LENGTH(VECTOR_ELT(right,0));   // num rows in right hand table
    if (nr<2) error("must have at least 2 rows in x");  // code can be fast by not dealing with 0 and 1 rows.
    if (nr>(1<<30)) error("Table has more than 1.073 billion rows. Currently programmed defensively against integer flow in signed 32bit i.e. doubling of (2^31-1)/2. This limit can be doubled quite easily by a few changes in the C. Alternatively by restricting to 64bit OS we can remove this limit.");
    coln = LENGTH(leftcols);    // there may be more sorted columns in the right table, but we just need up to the number in the left.
    
    for (lr=0; lr < LENGTH(VECTOR_ELT(left,0)); lr++) {  // left row
        for(col=0; col<coln; col++) {
            if (INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr] == NA_INTEGER) goto nextlr;
        }
        noteq = 1; // when upp was last tested for equality, store whether it was equal. Initial value is 1=TRUE=means not equal
        allbutlastequal_upp = 0;
        allbutlastequal_low = 0;
        upp = nr;
        low = -1;
        INTEGER(i)[lr] = 0;
        // so in this unsorted match, we always do binary search on the whole table, for each of the lookups in the left table. The left table isn't sorted so we don't have the luxury of a single simultaneous pass through both tables.
        while(low < upp-1) {
            mid = (low + upp) / 2;
            for(col=0; col<coln; col++) {
                rval = INTEGER(VECTOR_ELT(right,INTEGER(rightcols)[col]))[mid];
                if (rval==NA_INTEGER) {
                    d = NA_INTEGER;
                    break;
                } else {
                    lval = INTEGER(VECTOR_ELT(left,INTEGER(leftcols)[col]))[lr];
                    d = rval - lval;                
                    if (d != 0) break;
                }
            }
            if (d<=0) {
                low=mid;
                noteq=-d;
                allbutlastequal_low = (col==coln-1) && d!=NA_INTEGER;
            } else {
                upp=mid;
                allbutlastequal_upp = (col==coln-1);
            }
        }
        if (noteq || low == -1) {
            if ((INTEGER(roll)[0] && allbutlastequal_low) ||
                (INTEGER(rolltolast)[0] && allbutlastequal_low && allbutlastequal_upp)) {
                INTEGER(i)[lr] = low+1;
            }
            // else INTEGER(i)[lr] = 0;
        } else {
            INTEGER(i)[lr] = low+1;
        }
        nextlr :;
    }
    return(R_NilValue);
}


SEXP sortedstringmatch (SEXP ans, SEXP left, SEXP right, SEXP nomatch)
{
    // Specialized exact match for 2 sorted unique string vectors e.g. 2 factors with sorted levels.
    // The sorted match algorithm has been given several kickers here :
    //  1. Binary is replaced with linear estimation given alphabetical distribution is
    //     even. True for stock identifiers.
    //  2. A full strcmp is not always performed for each string. In fact the full string passed
    //     in may not need to be read if it first characters mean it is not found in the right. The
    //     binary search allows us to
    //     maintain a partial match in the first n characters. When we obtain a match in the
    //     first n characters between bounds low and upp, we go to the next character at that
    //     point. The comparison at each bound reduction is merely a single byte compare.
    // This method avoids looking at the entire key passed in, and we hope to approach
    // close to perfect hash table performance.
    // Downside is no consideration for fast inserts to the key, a sort(c(key,newval)) would be required,
    // currently. For static database read-only its ok.
    
    // left and right are sorted string vectors,  both without duplicates
    // left will be matched into right
    // i is the result, the locations of left in right, possibly including NA
    
    // for simplicity first, we implement one function which does not take advantage if the left vector is not sorted.
    // also we do use strcmp (which should short-circuit early for us) but the implementation is inconsistent with the comments above.
    
    // DO TO: Deal with NA strings

    int lr,nr,low,mid,upp,d;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);
    nr = length(right);
    // nr may be 0, thats fine, low<upp-1 will not be true in that case,  sortedmatch(1:3,as.integer(NULL)) = c(NA,NA,NA)
    if (nr>(1<<30)) error("String vector has length more than 1.073 billion items. Currently programmed defensively against integer flow in signed 32bit i.e. doubling of (2^31-1)/2. This limit can be doubled quite easily by a few changes in the C. Alternatively by restricting to 64bit OS we can remove this limit.");

    for (lr=0; lr < length(left); lr++) {
        low = -1;
        upp = nr;
        INTEGER(ans)[lr] = INTEGER(nomatch)[0];     // Assign missing, unless we find a match. Default in R is NA like match.
        while(low < upp-1) {
            mid = (low + upp) / 2;
            d = strcmp(CHAR(STRING_ELT(left,lr)), CHAR(STRING_ELT(right,mid)));
            switch( (d>0)-(d<0) ) {
            case -1:
                upp = mid; break;
            case 1:
                low = mid; break;
            case 0:
                INTEGER(ans)[lr] = mid+1;
                goto nextlr;    // may find it early by chance, if so stop now, no duplicates to worry about
            }
        }
        nextlr :;
    }
    return(R_NilValue);
}

// To maybe do in future :
//  repeat {
/*       lchar = leftstring[lr][c];
       rchar = rightstring[mid][c];
       if (lchar == '\0' && rchar == '\0') {
            // upp falls on precise string match, return it
            INTEGER(i)[lr] = upp+1;
            goto nextlr;
       }
       // if (lchar == '\0' || rchar == '\0') then d will be <0 or >0
       d = lchar - rchar;    // either could be the NULL terminator, space, upper or lower case
*/



SEXP sortedintegermatch (SEXP ans, SEXP left, SEXP right, SEXP nomatch)
{
    // Corollary of sortedstringmatch, see comments above, but for integers.
    // The comparison operation is the only line different, but we have a different
    // function to save the if.  It could be done with a pointer to the "-" operator function, or define one?
    // DOTO: deal with NAs in the integer vector
    int lr,nr,low,mid,upp,d;
    if (NA_INTEGER > 0) error("expected internal value of NA_INTEGER %d to be negative",NA_INTEGER);
    nr = length(right);
    if (nr>(1<<30)) error("Integer vector has length more than 1.073 billion items. Currently programmed defensively against integer flow in signed 32bit i.e. doubling of (2^31-1)/2. This limit can be doubled quite easily by a few changes in the C. Alternatively by restricting to 64bit OS we can remove this limit.");

    for (lr=0; lr < length(left); lr++) {
        low = -1;
        upp = nr;
        INTEGER(ans)[lr] = INTEGER(nomatch)[0];     // Assign missing, unless we find a match. Default in R is NA like match.
        if (INTEGER(left)[lr] == NA_INTEGER) continue;
        while(low < upp-1) {
            mid = (low + upp) / 2;
            if (INTEGER(right)[mid] == NA_INTEGER) {
                d = NA_INTEGER;
            } else {
                d = INTEGER(left)[lr] - INTEGER(right)[mid];
            }
            switch( (d>0)-(d<0) ) {     // So NA is assumed to be negative,  and appears first before the non-NAs
            case -1:
                upp = mid; break;
            case 1:
                low = mid; break;
            case 0:
                INTEGER(ans)[lr] = mid+1;
                goto nextlr;    // may find it early by chance, if so stop now, no duplicates to worry about
            }
        }
        nextlr :;
    }
    return(R_NilValue);
}


