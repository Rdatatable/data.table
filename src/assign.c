#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h> 

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP assign();
EXPORT SEXP alloccolwrapper();
EXPORT SEXP truelength();
EXPORT SEXP settruelength();
#endif

// See dogroups.c for these shared variables.
int sizes[100];
int sizesSet;
void setSizes();
#define SIZEOF(x) sizes[TYPEOF(x)]
//

SEXP growVector(SEXP x, R_len_t newlen);
SEXP alloccol(SEXP dt, R_len_t n, SEXP symbol, SEXP rho);
SEXP *saveds;
R_len_t *savedtl, nalloc, nsaved;
void savetl_init(), savetl(SEXP s), savetl_end();

SEXP assign(SEXP dt, SEXP rows, SEXP cols, SEXP newcolnames, SEXP values, SEXP clearkey, SEXP symbol, SEXP rho, SEXP revcolorder, SEXP allocwarn)
{
    // For internal use only by [<-.data.table.
    // newcolnames : add these columns (if any)
    // cols : column numbers corresponding to the values to set
    R_len_t i, j, size, targetlen, vlen, v, r, oldncol, oldtncol, coln, protecti=0, n;
    SEXP targetcol, RHS, names, nullint, thisvalue, thisv, targetlevels, newcol, s;
    //Rprintf("Beginning %d %d\n", TYPEOF(dt), length(dt));
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
        if (!length(cols)) {
            warning("RHS is NULL, meaning delete columns(s). But, no columns in LHS to delete.");
            return(dt);
        }
    } else {
        if (TYPEOF(values)==VECSXP) {
            if (length(cols)>1) {
                if (length(values)>length(cols)) warning("List of values is longer than the number of columns, ignoring some");  
                if (length(cols)%length(values) != 0) error("Supplied %d columns to be assigned a list length %d. Can recycle RHS but must be exact multiple.",length(cols),length(values));
            } // else it's a list() column being assigned to one column
        }
    }
    //Rprintf("Step 1 %d %d\n", TYPEOF(dt), length(dt));
    oldncol = length(dt);
    if (length(newcolnames)) {
        if (TYPEOF(newcolnames)!=STRSXP) error("newcolnames is not character vector");
        if (length(rows)!=0) error("Attempt to add new column(s) and set subset of rows at the same time. Create the new column(s) first, and then you'll be able to assign to a subset. If i is set to 1:nrow(x) then please remove that (no need, it's faster without).");
        oldtncol = TRUELENGTH(dt);
        if (oldtncol<length(dt)) error("Internal logical error: truelength(dt)<length(DT)");
        //Rprintf("Step 2 %d %d\n", TYPEOF(dt), length(dt));
        if (oldtncol < oldncol+LENGTH(newcolnames)) {
            n = imax2(oldtncol+100, oldncol+2*LENGTH(newcolnames));
            if (LOGICAL(allocwarn)[0] && NAMED(dt)>1) warning("growing vector of column pointers from %d to %d. Only a shallow copy has been taken, see ?alloc.col. Only a potential issue if two variables point to the same data, and if not you can safely ignore this warning. To avoid this warning you could alloc.col() first, deep copy first using copy(), wrap with suppressWarnings(), or increase the 'datatable.alloccol' option.", oldtncol, n);
            // Note that the NAMED(dt)>1 doesn't work because .Call always sets to 2 (see R-ints), it seems. Work around
            // may be possible but not yet working. When the NAMED test works, we can drop allocwarn argument too because
            // that's just passed in as FALSE from [<- where we know `*tmp*` isn't really NAMED=2.
            // Note also that this growing will happen for missing columns assigned NULL, too. But so rare, we don't mind.
            PROTECT(dt = alloccol(dt,n, R_NilValue, R_NilValue));
            protecti++;
        }
        size = sizeof(SEXP *);
        names = getAttrib(dt, R_NamesSymbol);
	    memcpy((char *)DATAPTR(names)+LENGTH(names)*size, (char *)DATAPTR(newcolnames), LENGTH(newcolnames)*size);
	    // If object has been duplicated by main/duplicate.c and length copied, but truelength retained, then
	    // this memcpy is where overwrites occurred when names is 40 bytes before dt, for example, and dt got set to
	    // NULL by this memcpy. Now theres an alloccol(dt,length(dt)) in [<- and $<-.
        LENGTH(dt) = oldncol+LENGTH(newcolnames);
        LENGTH(names) = oldncol+LENGTH(newcolnames);
        // truelength's of both already set by alloccol
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
            //Rprintf("Plonking in value\n");
            //Rprintf("%d %d\n", TYPEOF(dt), length(dt));
            SET_VECTOR_ELT(dt,coln,thisvalue);
            //Rprintf("done\n");
            // plonk new column in as it's already the correct length
            // if column exists, 'replace' it (one way to change a column's type i.e. less easy, as it should be, for speed, correctness and to get the user thinking about their intent)        
            continue;
        }
        if (vlen<1) error("RHS of assignment is zero length but not NULL. If you intend to delete the column use NULL. Otherwise the RHS must have length > 0");
        if (!isVectorAtomic(thisvalue) && !(isVector(thisvalue) && length(thisvalue)==targetlen))
            error("RHS of assignment is not NULL, not an an atomic vector (see ?is.atomic) and not a list() column.");
        if (targetlen%vlen != 0) error("Tried to assign %d items to target of %d (can recycle but must be exact multiple). If users ask us to change this to a warning, we will change it; please ask maintainer('data.table').",vlen,targetlen);
        if (coln+1 > oldncol) {  // new column
            PROTECT(newcol = allocVector(TYPEOF(thisvalue),targetlen));
            protecti++;
            //Rprintf("Adding new column\n");
            SET_VECTOR_ELT(dt,coln,newcol);
            //Rprintf("done\n");
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
            targetlevels = getAttrib(targetcol, R_LevelsSymbol);
            if (isString(thisvalue)) {
                savetl_init();
                for (j=0; j<length(thisvalue); j++) {
                    s = STRING_ELT(thisvalue,j);
                    if (TRUELENGTH(s)!=0) {
                        savetl(s);  // pre-2.14.0 this will save all the uninitialised truelengths
                        TRUELENGTH(s)=0;
                    }
                }
                for (j=0; j<length(targetlevels); j++) {
                    s = STRING_ELT(targetlevels,j);
                    if (TRUELENGTH(s)!=0) savetl(s);
                    TRUELENGTH(s)=j+1;
                }
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
                for (j=0; j<length(targetlevels); j++) TRUELENGTH(STRING_ELT(targetlevels,j))=0;  // important to reinstate 0 for countingcharacterorder and HASHPRI (if any) as done by savetl_end().
                savetl_end();
            } else {
                // value is either integer or numeric vector
                if (TYPEOF(thisvalue)!=INTSXP && TYPEOF(thisvalue)!=REALSXP)
                    error("Trying to assign factor column %d a value of type %d i.e. not character, integer or numeric",i,TYPEOF(thisvalue));
                PROTECT(RHS = coerceVector(thisvalue,INTSXP));  // TYPEOF(targetcol) is INTSXP for factors
                protecti++;
                for (j=0; j<length(RHS); j++) {
                    if (INTEGER(RHS)[j]<1 || INTEGER(RHS)[j]>LENGTH(targetlevels)) {
                        warning("RHS contains %d which is outside the levels range ([1,%d]) of column %d, NAs generated", INTEGER(RHS)[j], LENGTH(targetlevels), i+1);
                        INTEGER(RHS)[j] = NA_INTEGER;
                    }
                }
            }
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
    names = getAttrib(dt, R_NamesSymbol);
    for (r=0; r<length(revcolorder); r++) {
        // Delete any columns assigned NULL (there was a 'continue' early in loop above)
        i = INTEGER(revcolorder)[r]-1;
        coln = INTEGER(cols)[i]-1;
        if (TYPEOF(values)==VECSXP)
            thisvalue = VECTOR_ELT(values,i%LENGTH(values));
        else
            thisvalue = values;
        if (TYPEOF(thisvalue)==NILSXP) {
            if (coln+1 > oldncol) {
                warning("RHS is NULL but column '%s' is not present to delete", CHAR(STRING_ELT(names,coln)));
                // It was added above and now we'll delete it again (just easier to code it this way)
            }
            size=sizeof(SEXP *);
            memmove((char *)DATAPTR(dt)+coln*size,     
                    (char *)DATAPTR(dt)+(coln+1)*size,
                    (LENGTH(dt)-coln-1)*size);
            LENGTH(dt) -= 1;
            // TO DO: mark column vector as unused so can be gc'd. Maybe UNPROTECT_PTR?
            
            memmove((char *)DATAPTR(names)+coln*size,     
                    (char *)DATAPTR(names)+(coln+1)*size,
                    (LENGTH(names)-coln-1)*size);
            LENGTH(names) -= 1;
            if (LENGTH(names)==0) {
                // That was last column deleted, leaving NULL data.table, so we need to reset .row_names, so that it really is the NULL data.table.
                PROTECT(nullint=allocVector(INTSXP, 0));
                protecti++;
                setAttrib(dt, R_RowNamesSymbol, nullint);  // i.e. .set_row_names(0)
                //setAttrib(dt, R_NamesSymbol, R_NilValue);
            }
        }
    }
    if (LOGICAL(clearkey)[0]) {
        // If a key column is being assigned to, clear the key, since it may change the row ordering.
        // More likely that users will assign to non-key columns, though, most of the time.
        // Do this at C level to avoid any copies.
        setAttrib(dt, install("sorted"), R_NilValue);
    }
    if (!isNull(symbol)) {
        // Called from := in j when adding columns, just needed when a realloc takes place 
        if(!isEnvironment(rho)) error("Internal data.table error in assign.c (1). rho should be an environment");
        setVar(symbol,dt,rho);
    } // else called from $<- or [<- where the slower copy via `*tmp*` mechanism must be used (as of R 2.13.1)
    UNPROTECT(protecti);
    return(dt);  // needed for `*tmp*` mechanism, and to return the new object after a := for compound syntax
}

void savetl_init() {
    nsaved = 0;
    nalloc = 100;
    saveds = Calloc(nalloc, SEXP);
    savedtl = Calloc(nalloc, R_len_t);
}

void savetl(SEXP s)
{
    if (nsaved>=nalloc) {
        nalloc *= 2;
        saveds = Realloc(saveds, nalloc, SEXP);
        savedtl = Realloc(savedtl, nalloc, R_len_t);
    }
    saveds[nsaved] = s;
    savedtl[nsaved] = TRUELENGTH(s);
    nsaved++;
    //Rprintf("saved %s %d\n", CHAR(s), TRUELENGTH(s));
}

void savetl_end() {
    int i;
    for (i=0; i<nsaved; i++) TRUELENGTH(saveds[i]) = savedtl[i];
    Free(saveds);
    Free(savedtl);
}

SEXP alloccol(SEXP dt, R_len_t n, SEXP symbol, SEXP rho)
{
    SEXP newdt, names, newnames;
    R_len_t l=length(dt), tl=0;   // this tl=0 is important for pre R 2.14.0
    if (!isNull(dt)) {
        if (TYPEOF(dt) != VECSXP) error("dt passed to alloccol isn't type VECSXP");
        tl = TRUELENGTH(dt);
    };
    if (l>n) warning("table has %d column slots in use. Attept to allocate less (%d)",l,n);
    //  can now reduce for [<- and $<- coping via *tmp*....if (tl>=n) warning("table already has %d column slots allocated, with %d in use. Attempt to allocate less (%d)",tl,l,n);
    
    // TO DO: Get assign to call alloccol rather than repeat the code ...
    if (n>tl) { 
        PROTECT(newdt = allocVector(VECSXP, n));
        PROTECT(newnames = allocVector(STRSXP, n));
        if (l) {
            memcpy((char *)DATAPTR(newdt),(char *)DATAPTR(dt),l*sizeof(SEXP *));
            names = getAttrib(dt, R_NamesSymbol);
            if (length(names) != l) error("unexpected length of names");
	        memcpy((char *)DATAPTR(newnames), (char *)DATAPTR(names), l*sizeof(SEXP *));
	    }
	    setAttrib(newdt, R_NamesSymbol, newnames);
        copyMostAttrib(dt, newdt);
        LENGTH(newdt) = l;  //prefered due to the SETLENGTH vs SET_LENGTH confusion (inconsistent with SET_TRUELENGTH)
        LENGTH(newnames) = l;
        TRUELENGTH(newdt) = n;
        TRUELENGTH(newnames) = n;
        dt=newdt;
        // SET_NAMED(dt,1);  // for some reason, R seems to set NAMED=2 via setAttrib?  Need NAMED to be 1 for passing to assign via a .C dance before .Call (which sets NAMED to 2), and we can't use .C with DUP=FALSE on lists.
        if (!isNull(symbol)) {
            if (!isEnvironment(rho)) error("Internal data.table error in assign.c (2). rho should be an environment");
            setVar(symbol,dt,rho);
        }
        UNPROTECT(2);
    } else {
        // Reduce the allocation (most likely to the same value as length).
        //if (n!=l) warning("Reducing alloc cols from %d to %d, but not to length (%d)",TRUELENGTH(dt),n,LENGTH(dt));
        TRUELENGTH(dt) = n;
        TRUELENGTH(getAttrib(dt,R_NamesSymbol)) = n;
    }
    return(dt);
}

SEXP alloccolwrapper(SEXP dt, SEXP newncol, SEXP symbol, SEXP rho) {
    return(alloccol(dt, INTEGER(newncol)[0], symbol, rho));
}

SEXP truelength(SEXP x) {
    SEXP ans;
    PROTECT(ans = allocVector(INTSXP, 1));
    if (!isNull(x)) {
       INTEGER(ans)[0] = TRUELENGTH(x);
    } else {
       INTEGER(ans)[0] = 0;
    }
    UNPROTECT(1);
    return(ans);
}

SEXP settruelength(SEXP x, SEXP n) {
    // Only needed in pre 2.14.0. From 2.14.0+, truelength is initialized to 0 by R.
    // For prior versions we set truelength to 0 in data.table creation, before calling alloc.col.
    TRUELENGTH(x) = INTEGER(n)[0];
    return(R_NilValue);
}

/*
SEXP pointer(SEXP x) {
    SEXP ans;
    PROTECT(ans = allocVector(REALSXP, 1));
    REAL(ans)[0] = (double)x;
    UNPROTECT(1);
    return(ans);
}

SEXP named(SEXP x) {
    SEXP y = (SEXP)(REAL(x)[0]);
    Rprintf("%d length = %d\n",NAMED(y), LENGTH(y));
    return(R_NilValue);
}

void setnamed(double *x, int *v) {   // call by .Call(,DUP=FALSE) only.
    SEXP y = (SEXP)(*x);
    Rprintf("%d length = %d\n",NAMED(y), LENGTH(y));
    SET_NAMED(y,*v);
}
*/


