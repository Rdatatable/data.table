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

SEXP growVector(SEXP x, R_len_t newlen);

SEXP assign(SEXP dt, SEXP rows, SEXP cols, SEXP newcolnames, SEXP values, SEXP clearkey, SEXP symbol, SEXP rho, SEXP revcolorder)
{
    // For internal use only by [<-.data.table.
    // newcolnames : add these columns (if any)
    // cols : column numbers corresponding to the values to set
    R_len_t i, j, size, targetlen, vlen, v, r, oldncol, coln, protecti=0;
    SEXP targetcol, RHS, newdt, names, newnames, nullint, thisvalue, thisv, targetlevels;
    if (!sizesSet) setSizes();   // TO DO move into _init
    if (length(rows)==0) {
        targetlen = length(VECTOR_ELT(dt,0));
        // fast way to assign to whole column, without creating 1:nrow(x) vector up in R, or here in C
    } else {
        targetlen = length(rows);
    }
    if (TYPEOF(cols)!=INTSXP) error("Logical error in assign, TYPEOF(cols) is %d",TYPEOF(cols));  // Rinternals.h defines the type numbers=>names
    if (!length(cols)) error("Logical error in assign, no column positions passed to assign");
    if (length(cols)!=length(revcolorder)) error("Logical error in assign, length(cols)!=length(revcolorder)");
    if (TYPEOF(values)==NILSXP) {
        if (length(newcolnames)) error("RHS is NULL, meaning delete column(s). But, at least one column is not present.");
        if (!length(cols)) error("RHS is NULL, meaning delete columns(s). But, no columns passed to delete.");
    } else {
        if (TYPEOF(values)==VECSXP) {
            if (length(cols)>1) {
                if (length(values)>length(cols)) warning("List of values is longer than the number of columns, ignoring some");  
                if (length(cols)%length(values) != 0) error("Supplied %d columns to be assigned a list length %d. Can recycle RHS but must be exact multiple.",length(cols),length(values));
            } // else it's a list() column being assigned to one column
        }
    }
    oldncol = length(dt);
    if (length(newcolnames)) {
        if (TYPEOF(newcolnames)!=STRSXP) error("newcolnames is not character vector");
        if (length(rows)!=0) error("Attempt to add new column(s) and set subset of rows at the same time. Create the new column(s) first, and then you'll be able to assign to a subset. If i is set to 1:nrow(x) then please remove that (no need, it's faster without).");
        PROTECT(newdt = allocVector(VECSXP, length(dt)+length(newcolnames)));
        protecti++;
        size = sizeof(SEXP *);
        memcpy((char *)DATAPTR(newdt),(char *)DATAPTR(dt),length(dt)*size);
        names = getAttrib(dt, R_NamesSymbol);
        if (isNull(names)) error("names of data.table are null");
	    PROTECT(newnames = allocVector(STRSXP, length(newdt)));
	    protecti++;
	    memcpy((char *)DATAPTR(newnames), (char *)DATAPTR(names), length(names)*size);
	    memcpy((char *)DATAPTR(newnames)+length(names)*size, (char *)DATAPTR(newcolnames), length(newcolnames)*size);
	    setAttrib(newdt, R_NamesSymbol, newnames);
        copyMostAttrib(dt, newdt);
        dt = newdt;
    }
    for (i=0; i<length(cols); i++) {
        coln = INTEGER(cols)[i]-1;
        if (TYPEOF(values)==VECSXP && (length(cols)>1 || length(values)==1))
            thisvalue = VECTOR_ELT(values,i%length(values));
        else
            thisvalue = values;   // One vector applied to all columns, often NULL or NA for example
        if (TYPEOF(thisvalue)==NILSXP)
            continue;   // delete column(s) afterwards, below this loop
        vlen = length(thisvalue);
        if (length(rows)==0 && targetlen==vlen) {
            SET_VECTOR_ELT(dt,coln,thisvalue);
            // plonk new column in as it's already the correct length
            // if column exists, 'replace' it (one way to change a column's type i.e. less easy, as it should be, for speed, correctness and to get the user thinking about their intent)        
            continue;
        }
        if (vlen<1) error("RHS of assignment is zero length but not NULL. If you intend to delete the column use NULL. Otherwise the RHS must have length > 0");
        if (!isVectorAtomic(thisvalue) && !(isVector(thisvalue) && length(thisvalue)==targetlen))
            error("RHS of assignment is not NULL, not an an atomic vector (see ?is.atomic) and not a list() column.");
        if (targetlen%vlen != 0) error("Tried to assign %d items to target of %d (can recycle but must be exact multiple). If users ask us to change this to a warning, we will change it; please ask maintainer('data.table').",vlen,targetlen);
        if (coln+1 > oldncol) {  // new column
            SET_VECTOR_ELT(dt,coln,allocVector(TYPEOF(thisvalue),targetlen));
        }
        targetcol = VECTOR_ELT(dt,coln);
        if (isFactor(targetcol)) {
            // Coerce RHS to appropriate levels of LHS, adding new levels as necessary (unlike base)
            // If it's the same RHS being assigned to several columns, we have to recoerce for each
            // one because the levels of each target are likely different
            if (isFactor(thisvalue)) {
                PROTECT(thisvalue = asCharacterFactor(thisvalue));
                protecti++;
            }
            for (j=0; j<length(thisvalue); j++) TRUELENGTH(STRING_ELT(thisvalue,j))=0;  // can skip in future
            targetlevels = getAttrib(targetcol, R_LevelsSymbol);
            for (j=0; j<length(targetlevels); j++) TRUELENGTH(STRING_ELT(targetlevels,j))=j+1;
            R_len_t addi = 0;
            SEXP addlevels=NULL;
            PROTECT(RHS = allocVector(INTSXP, length(thisvalue)));
            protecti++;
            for (j=0; j<length(thisvalue); j++) {
                thisv = STRING_ELT(thisvalue,j);
                if (TRUELENGTH(thisv)==0) {
                    if (addi==0) {
                        PROTECT(addlevels = allocVector(STRSXP, 100));
                        protecti++;
                    } else if (addi >= length(addlevels)) {
                        PROTECT(addlevels = growVector(addlevels, length(addlevels)+1000));
                        protecti++;
                    }
                    SET_STRING_ELT(addlevels,addi,thisv);
                    TRUELENGTH(thisv) = ++addi+length(targetlevels);  
                }
                INTEGER(RHS)[j] = TRUELENGTH(thisv);
            }
            if (addi > 0) {
                R_len_t oldlen = length(targetlevels);
                PROTECT(targetlevels = growVector(targetlevels, length(targetlevels)+addi));
                protecti++;
                size = sizeof(SEXP *);
                memcpy((char *)DATAPTR(targetlevels) + oldlen*size,
                       (char *)DATAPTR(addlevels),
                       addi*size);
                setAttrib(targetcol, R_LevelsSymbol, targetlevels);
            }
            for (j=0; j<length(targetlevels); j++) TRUELENGTH(STRING_ELT(targetlevels,j))=0;  // important to reinstate 0 for countingcharacterorder
        } else {
            PROTECT(RHS = coerceVector(thisvalue,TYPEOF(targetcol)));
            protecti++;
        }
        // i.e. coerce the RHS to match the type of the column (coerceVector returns early if already the same)
        // This is different to [<-.data.frame which changes the type of the column to match the RHS. A
        // data.table tends to be big so we don't want to do that. Also, typically the RHS is very small,
        // often a length 1 vector such as 42, which by default in R is type numeric. If the column is integer,
        // then intent of user was very likely to coerce 42 to integer and then assign that to the integer column,
        // not the other way around like [.data.frame
        // Most usual reason for coercing RHS several times (i.e. inside this loop) is when assigning NA
        // to different typed columns on left in for example DT[i,]<-NA
        if (TYPEOF(thisvalue)==REALSXP && (TYPEOF(targetcol)==INTSXP || TYPEOF(targetcol)==LGLSXP)) {
            warning("Coerced numeric RHS to %s to match the column's type; may have truncated precision. Either change the column to numeric first (by creating a new numeric vector length %d (nrows of entire table) yourself and assigning that; i.e. 'replace' column), or coerce RHS to integer yourself (e.g. 1L or as.integer) to make your intent clear (and for speed). Or, set the column type correctly up front when you create the table and stick to it, please.", TYPEOF(targetcol)==INTSXP ? "integer" : "logical", length(VECTOR_ELT(dt,0)));
        }
        if (TYPEOF(thisvalue)==INTSXP && TYPEOF(targetcol)==LGLSXP) {
            warning("Coerced integer RHS to logical to match the column's type. Either change the column to integer first (by creating a new integer vector length %d (nrows of entire table) yourself and assigning that; i.e. 'replace' column), or coerce RHS to logical yourself (e.g. as.logical) to make your intent clear (and for speed). Or, set the column type correctly up front when you create the table and stick to it, please.", length(VECTOR_ELT(dt,0)));
        }
        size = SIZEOF(targetcol);
        if (length(rows)==0) {
            for (r=0; r<(targetlen/vlen); r++) {
                memcpy((char *)DATAPTR(targetcol) + r*vlen*size,
                       (char *)DATAPTR(RHS),
                       vlen * size);
            }
        } else {
            j=0;
            for (r=0; r<(targetlen/vlen); r++) {
                for (v=0;v<vlen;v++) {
                    memcpy((char *)DATAPTR(targetcol) + (INTEGER(rows)[j++]-1)*size, 
                           (char *)DATAPTR(RHS) + v*size,
                           size);
                }
            }
        }
    }
    for (r=0; r<length(revcolorder); r++) {
        // Delete any columns assigned NULL (there was a 'continue' early in loop above)
        i = INTEGER(revcolorder)[r]-1;
        coln = INTEGER(cols)[i]-1;
        if (TYPEOF(values)==VECSXP)
            thisvalue = VECTOR_ELT(values,i%length(values));
        else
            thisvalue = values;
        if (TYPEOF(thisvalue)==NILSXP) {
            if (coln+1 > oldncol) error("Added new column but RHS is NULL");
            size=sizeof(SEXP *);
            memmove((char *)DATAPTR(dt)+coln*size,     
                    (char *)DATAPTR(dt)+(coln+1)*size,
                    (length(dt)-coln-1)*size);
            SETLENGTH(dt,length(dt)-1);
            // TO DO: mark column vector as unused so can be gc'd. Maybe UNPROTECT_PTR?
            names = getAttrib(dt, R_NamesSymbol);
            memmove((char *)DATAPTR(names)+coln*size,     
                    (char *)DATAPTR(names)+(coln+1)*size,
                    (length(names)-coln-1)*size);
            SETLENGTH(names,length(names)-1);
            if (length(names)==0) {
                // That was last column deleted, leaving NULL data.table, so we need to reset .row_names and names, so that it really is the NULL data.table.
                PROTECT(nullint=allocVector(INTSXP, 0));
                protecti++;
                setAttrib(dt, R_RowNamesSymbol, nullint);  // i.e. .set_row_names(0)
                setAttrib(dt, R_NamesSymbol, R_NilValue);
            }
            continue;
        }
    }
    if (LOGICAL(clearkey)[0]) {
        // If a key column is being assigned to, clear the key, since it may change the row ordering.
        // More likely that users will assign to non-key columns, though, most of the time.
        // Do this at C level to avoid any copies.
        setAttrib(dt, install("sorted"), R_NilValue);
    }
    if (length(symbol)) {
        // Called from := in j
        if(!isEnvironment(rho)) error("Internal data.table error in assign.c. rho should be an environment");
        setVar(symbol,dt,rho);
    } // else called from $<- or [<- where the slower copy via `*tmp*` mechanism must be used (as of R 2.13.1)
    UNPROTECT(protecti);
    return(dt);  // needed for `*tmp*` mechanism, and to return the new object after a := for compound syntax
}


