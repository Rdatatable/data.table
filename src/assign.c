#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP assign();
#endif

// See dogroups.c for these shared variables.
int sizes[100];
int sizesSet;
void setSizes();
#define SIZEOF(x) sizes[TYPEOF(x)]
//

SEXP assign(SEXP dt, SEXP rows, SEXP cols, SEXP values, SEXP clearkey, SEXP name, SEXP rho)
{
    // For internal use only by [<-.data.table.
    int i, size, targetlen, vlen, v, r, dtncol, coln, protecti=0;
    SEXP targetcol, RHS, newdt, names, newnames, symbol;
    if (!sizesSet) setSizes();   // TO DO move into _init
    if (length(rows)==0) {
        targetlen = length(VECTOR_ELT(dt,0));
        // fast way to assign to whole column, without creating 1:nrow(x) vector up in R, or here in C
    } else {
        targetlen = length(rows);
    }
    vlen = length(values);
    if (TYPEOF(values)==NILSXP) {
        if (TYPEOF(cols)==STRSXP) error("Value of assignment is NULL, meaning delete column. But column is not present.");
    } else {
        if (vlen<1) error("RHS of assignment is zero length but not NULL. If you intend to delete the column use NULL. Otherwise the RHS must have length > 0");
        if (!isVectorAtomic(values) && !(isVector(values) && length(values)==targetlen))
            error("RHS of assignment is not NULL, not an an atomic vector (see ?is.atomic) and not a list() column. In future may allow a list() of values, same length as j");
        if (targetlen%vlen != 0) error("Tried to assign %d items to target of %d (can recycle but must be exact multiple)",vlen,targetlen);
    }
    dtncol = length(dt);
    if (TYPEOF(cols)==STRSXP) {
        // calling R passes STRSXP when adding new column(s) to end of table
        if (length(rows)!=0) error("Attempt to add new column(s) and set subset of rows at the same time. Create the new column(s) first, and then you'll be able to assign to a subset. If i is set to 1:nrow(x) then please remove that (no need, it's faster without).");
        PROTECT(newdt = allocVector(VECSXP, length(dt)+length(cols)));
        protecti++;
        memcpy(DATAPTR(newdt),DATAPTR(dt),length(dt)*sizeof(SEXP *));
        // maybe a loop instead if memcpy doesn't work out for some reason
        // for (i=0; i<length(dt); i++)
        //    SET_VECTOR_ELT(newdt, i, VECTOR_ELT(dt,i));
        names = getAttrib(dt, R_NamesSymbol);
        if (isNull(names)) error("names of data.table are null");
	    PROTECT(newnames = allocVector(STRSXP, length(newdt)));
	    protecti++;
	    for (i=0; i<length(dt); i++)
	        SET_STRING_ELT(newnames, i, STRING_ELT(names, i));
	    for (i=0; i<length(cols); i++)
	        SET_STRING_ELT(newnames, length(dt)+i, STRING_ELT(cols, i));
	    setAttrib(newdt, R_NamesSymbol, newnames);
        copyMostAttrib(dt, newdt);
        if (length(name)) {
            // Called from := in j
            // From R-ext section 5.9.8 :
            if (!isString(name) || length(name)!=1) error("Internal data.table error in assign.c. name is not a single string");
            if(!isEnvironment(rho)) error("Internal data.table error in assign.c. rho should be an environment");
            symbol = install(CHAR(STRING_ELT(name, 0)));
        } // else called from $<- or [<- where the slower (copying) `*tmp*` mechanism must be used (as of R 2.13.1)
        if (length(rows)==0 && targetlen==vlen) {
            for (i=0; i<length(cols); i++)
                SET_VECTOR_ELT(newdt,length(dt)+i,values);
            if (length(name)) setVar(symbol,newdt,rho);
            UNPROTECT(protecti);
            return(newdt);
        }
        // else allocate the column(s), and fall through to recycling subassignment later below
        for (i=0; i<length(cols); i++)
            SET_VECTOR_ELT(newdt,length(dt)+i,allocVector(TYPEOF(values),targetlen));
        if (length(name)) setVar(symbol,newdt,rho);
        dt = newdt;
    } else {
        if (TYPEOF(cols)!=INTSXP) error("Logical error in assign, TYPEOF(cols) is %d",TYPEOF(cols));  // Rinternals.h defines the type names/numbers 
    }
    for (i=0; i<length(cols); i++) {
        if (TYPEOF(cols)==STRSXP)
            coln = dtncol+i;                // each new column added above, dtncol=length(dt) before dt was enlarged
        else {
            coln = INTEGER(cols)[i]-1;      // existing columns
            if (TYPEOF(values)==NILSXP) {
                // delete column
                size=sizeof(SEXP *);
                memmove(DATAPTR(dt)+coln*size,     
                        DATAPTR(dt)+(coln+1)*size,
                        (length(dt)-coln-1)*size);
                SETLENGTH(dt,length(dt)-1);
                // TO DO: mark column vector as unused so can be gc'd. Maybe UNPROTECT_PTR?
                names = getAttrib(dt, R_NamesSymbol);
                memmove(DATAPTR(names)+coln*size,     
                        DATAPTR(names)+(coln+1)*size,
                        (length(names)-coln-1)*size);
                SETLENGTH(names,length(names)-1);
                continue;
            }
        }
        if (length(rows)==0 && targetlen==vlen) {
            // replace the column by reference
            // i needs to be missing at R level, not set to 1:nrow(dt)
            // So, this is how to change the type of a column (i.e. less easy, as it should be, for speed, correctness and to get the user thinking about their intent)
            if (TYPEOF(cols)==STRSXP) error("Internal logical error, cols is STRSXP and values is nrow long, but falling through to recycling branch");
            SET_VECTOR_ELT(dt,coln,values);   // i.e. replace existing column with the RHS  
        } else {
            targetcol = VECTOR_ELT(dt,coln);
            PROTECT(RHS = coerceVector(values,TYPEOF(targetcol)));
            protecti++;
            // i.e. coerce the RHS to match the type of the column (coerceVector returns early if already the same)
            // This is different to [<-.data.frame which changes the type of the column to match the RHS. A
            // data.table tends to be big so we don't want to do that! Also, typically the RHS is very small,
            // often a length 1 vector such as 42, which by default in R is type numeric. If the column is integer,
            // then intent of user was very likely to coerce 42 to integer and then assign that to the integer column,
            // not the other way around like [.data.frame
            // Most usual reason for coercing RHS several times is when assigning NA to different typed columns on left in for example DT[i,]<-NA
            if (TYPEOF(values)==REALSXP && TYPEOF(targetcol)==INTSXP) {
                warning("Coerced numeric RHS to integer because the column is integer; may have truncated precision. Either change the column to numeric first (by assigning a full column RHS using vector(), or rep()ing RHS to match nrows), or (more likely) coerce RHS to integer yourself (e.g. 1L or as.integer) to make your intent clear (and for speed), or, set the column type correct up front when you create the table and stick to it, please.");
            }
            size = SIZEOF(targetcol);
            if (length(rows)==0) {
                for (r=0; r<(targetlen/vlen); r++) {
                    memcpy((char *)DATAPTR(targetcol) + r*vlen*size,
                           (char *)DATAPTR(RHS),
                           vlen * size);
                }
            } else {
                i=0;
                for (r=0; r<(targetlen/vlen); r++) {
                    for (v=0;v<vlen;v++) {
                        memcpy((char *)DATAPTR(targetcol) + (INTEGER(rows)[i++]-1)*size, 
                               (char *)DATAPTR(RHS) + v*size,
                               size);
                    }
                }
            }
        }
    }
    if (LOGICAL(clearkey)[0]) {
        // If a key column is being assigned to, clear the key, since it may change the row ordering.
        // More likely that users will assign to non-key columns, though, most of the time.
        // Do this at C level to avoid any copies.
        setAttrib(dt, install("sorted"), R_NilValue);
    }
    UNPROTECT(protecti);
    return(dt);  // needed for `*tmp*` mechanism, but also returns here when recycling := (setVar already called above so return(dt) is redundant, likely harmless)
}

