#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h> 
#include <Rversion.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP assign();
EXPORT SEXP alloccolwrapper();
EXPORT SEXP truelength();
EXPORT SEXP settruelength();
EXPORT SEXP setlength();
EXPORT SEXP setcharvec();
#endif

// See dogroups.c for these shared variables.
int sizes[100];
char typename[100][30];
int sizesSet;
void setSizes();
SEXP SelfRefSymbol; 
#define SIZEOF(x) sizes[TYPEOF(x)]
//

extern SEXP growVector(SEXP x, R_len_t newlen, Rboolean verbose);
SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose);
SEXP *saveds;
R_len_t *savedtl, nalloc, nsaved;
void savetl_init(), savetl(SEXP s), savetl_end();

void setselfref(SEXP x) {   
    // Store pointer to itself so we can detect if the object has been copied. See
    // ?copy for why copies are not just inefficient but cause a problem for over-allocated data.tables.
    // Called from C only, not R level, so returns void.
    setAttrib(x, SelfRefSymbol, R_MakeExternalPtr(
        R_NilValue,                  // for identical() to return TRUE
        getAttrib(x, R_NamesSymbol), // to detect if names has been replaced and its tl lost, e.g. setattr(DT,"names",...)
        R_MakeExternalPtr(           // to avoid an infinite loop in object.size(), if prot=x here
            x,                       // to know if this data.table has been copied by key<-, attr<-, names<-, etc.
            R_NilValue,              // this tag and prot currently unused
            R_NilValue
        )
    ));
/*  *  base::identical doesn't check prot and tag of EXTPTR, just that the ptr itself is the
       same in both objects. R_NilValue is always equal to R_NilValue.  R_NilValue is a memory
       location constant within an R session, but can vary from session to session. So, it
       looks like a pointer to a user looking at attributes(DT), but they might wonder how it
       works if they realise the selfref of all data.tables all point to the same address (rather
       than to the table itself which would be reasonable to expect given the attributes name).
    *  p=NULL rather than R_NilValue works too, other than we need something other than NULL
       so we can detect tables loaded from disk (which set p to NULL, see 5.13 of R-exts).
    *  x is wrapped in another EXTPTR because of object.size (called by tables(), and by users).
       If the prot (or tag) was x directly it sends object.size into an infinite loop and then
       "segfault from C stack overflow" (object.size does count tag and prot, unlike identical,
       but doesn't count what's pointed to).
    *  Could use weak reference possibly, but the fact that they can get be set to R_NilValue
       by gc (?) didn't seem appropriate.
    *  If the .internal.selfref attribute is removed (e.g. by user code), nothing will break, but
       an extra copy will just be taken on next :=, with warning, with a new selfref.
    *  object.size will count size of names twice, but that's ok as only small.
    *  Thanks to Steve L for suggesting ExtPtr for this, rather than the previous REALSXP
       vector which required data.table to do a show/hide dance in a masked identical.
*/
}

Rboolean _selfrefok(SEXP x, Rboolean names, Rboolean verbose) {
    SEXP v, p, tag, prot;
    v = getAttrib(x, SelfRefSymbol);
    if (v==R_NilValue) {
        // .internal.selfref missing. This is expected and normal for i) a pre v1.7.8 data.table loaded
        //  from disk, and ii) every time a new data.table is over-allocated for the first time.
        return(FALSE);
    }
    p = R_ExternalPtrAddr(v);
    if (p==NULL) {
        if (verbose) Rprintf(".internal.selfref ptr is NULL. This is expected and normal for a data.table loaded from disk. If not, please report to datatable-help.\n");
        return(FALSE);
    }
    if (!isNull(p)) error("Internal error: .internal.selfref ptr is not NULL or R_NilValue");
    tag = R_ExternalPtrTag(v);
    if (!isString(tag)) error("Internal error: .internal.selfref tag doesn't point to a character vector");
    prot = R_ExternalPtrProtected(v);
    if (TYPEOF(prot) != EXTPTRSXP) error("Internal error: .internal.selfref prot is not itself an extptr");
    if (names) {
        return(getAttrib(x, R_NamesSymbol)==tag);
    } else {
        return(x==R_ExternalPtrAddr(prot));
    }
}

Rboolean selfrefok(SEXP x, Rboolean verbose) {   // for readability
    return(_selfrefok(x, FALSE, verbose));
}
Rboolean selfrefnamesok(SEXP x, Rboolean verbose) {
    return(_selfrefok(x, TRUE, verbose));
}

SEXP assign(SEXP dt, SEXP rows, SEXP cols, SEXP newcolnames, SEXP values, SEXP clearkey, SEXP revcolorder, SEXP verb)
{
    // For internal use only by [<-.data.table.
    // newcolnames : add these columns (if any)
    // cols : column numbers corresponding to the values to set
    R_len_t i, j, size, targetlen, vlen, r, oldncol, oldtncol, coln, protecti=0, /*n,*/ newcolnum;
    SEXP targetcol, RHS, names, nullint, thisvalue, thisv, targetlevels, newcol, s, colnam, class;
    Rboolean verbose = LOGICAL(verb)[0];
    
    if (!sizesSet) setSizes();   // TO DO move into _init
    
    if (isNull(dt)) error("assign has been passed a NULL dt");
    if (TYPEOF(dt) != VECSXP) error("dt passed to assign isn't type VECSXP");
    class = getAttrib(dt, R_ClassSymbol);
    if (isNull(class)) error("dt passed to assign has no class attribute. Please report to datatable-help.");
    if (!selfrefok(dt,verbose)) error("At an earlier point, this data.table has been copied by R. Avoid key<-, names<- and attr<- which in R currently (and oddly) all copy the whole data.table. Use set* syntax instead to avoid copying: setkey(), setnames() and setattr(). If this message doesn't help, please ask on datatable-help.");  // Produced at the point user wants to assign by reference using := after the use of names<-, attr<- or key<- (which might be never).
    // We can't catch those copies and call alloc.col afterwards, so we have to ask user to change. See comments in key<-.
    oldncol = LENGTH(dt);
    names = getAttrib(dt,R_NamesSymbol);
    if (isNull(names)) error("dt passed to assign has no names");
    if (length(names)!=oldncol)
        error("Internal error in assign: length of names (%d) is not length of dt (%d)",length(names),oldncol);

    if (oldncol<1) error("Cannot use := to add columns to an empty data.table, currently");
    if (length(rows)==0) {
        targetlen = length(VECTOR_ELT(dt,0));
        // fast way to assign to whole column, without creating 1:nrow(x) vector up in R, or here in C
    } else {
        targetlen = length(rows);
    }
    if (TYPEOF(cols)!=INTSXP) error("Logical error in assign, TYPEOF(cols) is %d",TYPEOF(cols));  // Rinternals.h defines the type numbers=>names
    if (!length(cols)) error("Logical error in assign, no column positions passed to assign");
    if (length(cols)!=length(revcolorder)) error("Logical error in assign, length(cols)!=length(revcolorder)");
    if (!isNull(newcolnames) && !isString(newcolnames)) error("newcolnames is supplied but isn't a character vector");
    if (TYPEOF(values)==NILSXP) {
        if (!length(cols)) {
            warning("RHS is NULL, meaning delete columns(s). But, no columns in LHS to delete.");
            return(dt);
        }
    } else {
        if (TYPEOF(values)==VECSXP) {
            if (length(cols)>1) {
                if (length(values)>length(cols))
                    warning("Supplied %d columns to be assigned a list (length %d) of values (%d unused)", length(cols), length(values), length(values)-length(cols));  
                else if (length(cols)%length(values) != 0)
                    warning("Supplied %d columns to be assigned a list (length %d) of values (recycled leaving remainder of %d items).",length(cols),length(values),length(cols)%length(values));
            } // else it's a list() column being assigned to one column       
        } 
    }
    // Check all inputs :
    for (i=0; i<length(cols); i++) {
        coln = INTEGER(cols)[i]-1;
        if (TYPEOF(values)==VECSXP && (length(cols)>1 || length(values)==1))
            thisvalue = VECTOR_ELT(values,i%LENGTH(values));
        else
            thisvalue = values;   // One vector applied to all columns, often NULL or NA for example
        vlen = length(thisvalue);
        if (coln+1 <= oldncol) colnam = STRING_ELT(names,coln);
        else colnam = STRING_ELT(newcolnames,coln-length(names));
        if (vlen<1) {
            if (coln+1 <= oldncol) {
                if (isNull(thisvalue)) continue;  // delete existing column(s) afterwards, near end of this function
                error("RHS of assignment to existing column '%s' is zero length but not NULL. If you intend to delete the column use NULL. Otherwise, the RHS must have length > 0; e.g., NA_integer_. If you are trying to change the column type to be an empty list column then, as with all column type changes, provide a full length RHS vector such as vector('list',nrow(DT)); i.e., 'plonk' in the new column.", CHAR(STRING_ELT(names,coln)));
            } else if (TYPEOF(thisvalue)!=VECSXP) {  // list() is ok for new columns
                newcolnum = coln-length(names);
                if (newcolnum<0 || newcolnum>=length(newcolnames))
                    error("Internal logical error. length(newcolnames)=%d, length(names)=%d, coln=%d", length(newcolnames), length(names), coln);
                if (isNull(thisvalue)) {
                    warning("Adding new column '%s' then assigning NULL (deleting it).",CHAR(STRING_ELT(newcolnames,newcolnum)));
                    continue;
                }
                error("RHS of assignment to new column '%s' is zero length but not empty list(). For new columns the RHS must either be empty list() to create an empty list column, or, have length > 0; e.g. NA_integer_, 0L, etc.", CHAR(STRING_ELT(newcolnames,newcolnum)));
            }
        }
        if (!isVector(thisvalue) && !(isVector(thisvalue) && length(thisvalue)==targetlen))
            error("RHS of assignment is not NULL, not an an atomic vector (see ?is.atomic) and not a list column.");
        if ((coln+1)<=oldncol && isFactor(VECTOR_ELT(dt,coln)) &&
            !isString(thisvalue) && TYPEOF(thisvalue)!=INTSXP && TYPEOF(thisvalue)!=REALSXP)
            error("Can't assign to column '%s' (type 'factor') a value of type '%s' (not character, factor, integer or numeric)", CHAR(STRING_ELT(names,coln)),typename[TYPEOF(thisvalue)]);
        if (vlen>targetlen)
            warning("Supplied %d items to be assigned to %d items of column '%s' (%d unused)", vlen, targetlen,CHAR(colnam),vlen-targetlen);  
        else if (vlen>0 && targetlen%vlen != 0)
            warning("Supplied %d items to be assigned to %d items of column '%s' (recycled leaving remainder of %d items).",vlen,targetlen,CHAR(colnam),targetlen%vlen);
    }
    // having now checked the inputs, from this point there should be no errors,
    // so we can now proceed to modify DT by reference.
    if (length(newcolnames)) {
        if (length(rows)!=0) error("Attempt to add new column(s) and set subset of rows at the same time. Create the new column(s) first, and then you'll be able to assign to a subset. If i is set to 1:nrow(x) then please remove that (no need, it's faster without).");
        oldtncol = TRUELENGTH(dt);   // TO DO: oldtncol can be just called tl now, as we won't realloc here any more.
        
        if (oldtncol<oldncol) error("Internal error, please report (including result of sessionInfo()) to datatable-help: oldtncol (%d) < oldncol (%d) but tl of class is marked.", oldtncol, oldncol);
        if (oldtncol>oldncol+1000) warning("tl (%d) is greater than 1000 items over-allocated (ncol = %d). If you didn't set the datatable.alloccol option very large, please report this to datatable-help including the result of sessionInfo().",oldtncol, oldncol); 
        
        if (oldtncol < oldncol+LENGTH(newcolnames))
            error("Internal logical error. DT passed to assign has not been allocated enough column slots. l=%d, tl=%d, adding %d", oldncol, oldtncol, LENGTH(newcolnames));
        if (!selfrefnamesok(dt,verbose))
            error("It appears that at some earlier point, names of this data.table have been reassigned. Please ensure to use setnames() rather than names<- or colnames<-. Otherwise, please report to datatable-help.");
            // Can growVector at this point easily enough, but it shouldn't happen in first place so leave it as
            // strong error message for now.
        else if (TRUELENGTH(names) != oldtncol)
            error("selfrefnames is ok but tl names [%d] != tl [%d]", TRUELENGTH(names), oldtncol);
        for (i=0; i<LENGTH(newcolnames); i++)
            SET_STRING_ELT(names,oldncol+i,STRING_ELT(newcolnames,i));
        LENGTH(dt) = oldncol+LENGTH(newcolnames);
        LENGTH(names) = oldncol+LENGTH(newcolnames);
        // truelengths of both already set by alloccol
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
        if (coln+1 > oldncol) {  // new column
            PROTECT(newcol = allocVector(TYPEOF(thisvalue),targetlen));
            protecti++;
            if (isFactor(thisvalue)) {
                 setAttrib(newcol, R_LevelsSymbol, getAttrib(thisvalue, R_LevelsSymbol));
                 setAttrib(newcol, R_ClassSymbol, getAttrib(thisvalue, R_ClassSymbol));
            }
            SET_VECTOR_ELT(dt,coln,newcol);
            if (vlen<1) continue;   // TO DO: come back and comment why this is here
            targetcol = newcol;
            RHS = thisvalue;
        } else {                 // existing column
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
                                PROTECT(addlevels = growVector(addlevels, length(addlevels)+1000, FALSE));
                                protecti++;
                            }
                            SET_STRING_ELT(addlevels,addi,thisv);
                            TRUELENGTH(thisv) = ++addi+length(targetlevels);  
                        }
                        INTEGER(RHS)[j] = TRUELENGTH(thisv);
                    }
                    if (addi > 0) {
                        R_len_t oldlen = length(targetlevels);
                        PROTECT(targetlevels = growVector(targetlevels, oldlen+addi, FALSE));
                        protecti++;
                        for (j=0; j<addi; j++)
                            SET_STRING_ELT(targetlevels, oldlen+j, STRING_ELT(addlevels, j));
                        setAttrib(targetcol, R_LevelsSymbol, targetlevels);
                    }
                    for (j=0; j<length(targetlevels); j++) TRUELENGTH(STRING_ELT(targetlevels,j))=0;  // important to reinstate 0 for countingcharacterorder and HASHPRI (if any) as done by savetl_end().
                    savetl_end();
                } else {
                    // value is either integer or numeric vector
                    if (TYPEOF(thisvalue)!=INTSXP && TYPEOF(thisvalue)!=REALSXP)
                        error("Internal logical error. Up front checks (before starting to modify DT) didn't catch type of RHS ('%s') assigning to factor column '%s'. Please report to datatable-help.", typename[TYPEOF(thisvalue)], CHAR(STRING_ELT(names,coln)));
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
        }
        size = SIZEOF(targetcol);
        //Rprintf("Recycling assign to coln %d of type '%s' vlen %d targetlen %d size %d\n", coln, typename[TYPEOF(targetcol)], vlen,targetlen,size);
        if (TYPEOF(RHS) != TYPEOF(targetcol)) error("Internal error, TYPEOF(targetcol)!=TYPEOF(RHS)");
        if (length(rows)==0) {
            switch (TYPEOF(targetcol)) {
            case STRSXP :
                for (r=0; r<targetlen; r++)
                    SET_STRING_ELT(targetcol, r, STRING_ELT(RHS, r%vlen));
                break;
            case VECSXP :
                for (r=0; r<targetlen; r++)
                    SET_VECTOR_ELT(targetcol, r, STRING_ELT(RHS, r%vlen));
                break;
            default :
                for (r=0; r<(targetlen/vlen); r++) {
                    memcpy((char *)DATAPTR(targetcol) + r*vlen*size,
                           (char *)DATAPTR(RHS),
                           vlen * size);
                }
                memcpy((char *)DATAPTR(targetcol) + r*vlen*size,
                       (char *)DATAPTR(RHS),
                       (targetlen%vlen) * size);
            }
        } else {
            switch (TYPEOF(targetcol)) {
            case STRSXP :
                for (r=0; r<targetlen; r++)
                    SET_STRING_ELT(targetcol, INTEGER(rows)[r]-1, STRING_ELT(RHS, r%vlen));
                break;
            case VECSXP :
                for (r=0; r<targetlen; r++)
                    SET_VECTOR_ELT(targetcol, INTEGER(rows)[r]-1, STRING_ELT(RHS, r%vlen));
                break;
            default :
                for (r=0; r<targetlen; r++)
                    memcpy((char *)DATAPTR(targetcol) + (INTEGER(rows)[r]-1)*size, 
                           (char *)DATAPTR(RHS) + (r%vlen) * size,
                           size);
            }
        }
    }
    for (r=0; r<length(revcolorder); r++) {
        // Delete any columns assigned NULL (there was a 'continue' early in loop above)
        i = INTEGER(revcolorder)[r]-1;
        coln = INTEGER(cols)[i]-1;
        if (TYPEOF(values)==VECSXP && LENGTH(values)>0)
            thisvalue = VECTOR_ELT(values,i%LENGTH(values));
        else
            thisvalue = values;
        if (isNull(thisvalue)) {
            // A new column being assigned NULL would have been warned above, added above, and now deleted (just easier
            // to code it this way e.g. so that other columns may be added or removed ok by the same query).
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
    UNPROTECT(protecti);
    return(dt);  // needed for `*tmp*` mechanism (when := isn't used), and to return the new object after a := for compound syntax.
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

SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose)
{
    SEXP newdt, names, newnames, class;
    R_len_t i, l, tl;
    if (!sizesSet) setSizes();   // TO DO move into _init
    if (isNull(dt)) error("alloccol has been passed a NULL dt");
    if (TYPEOF(dt) != VECSXP) error("dt passed to alloccol isn't type VECSXP");
    class = getAttrib(dt, R_ClassSymbol);
    if (isNull(class)) error("dt passed to alloccol has no class attribute. Please report result of traceback() to datatable-help.");
    l = LENGTH(dt);
    names = getAttrib(dt,R_NamesSymbol);
    // names may be NULL, when null.data.table() passes list() to alloccol for example.
    // So, careful to use length() on names, not LENGTH(). 
    if (length(names)!=l)
        error("Internal error: length of names (%d) is not length of dt (%d)",length(names),l);
    
    if (!selfrefok(dt,verbose)) {
        // TO DO: think we can remove all the settruelength's up in data.table.R now we just rely on selfrefok.
        tl=l;
        TRUELENGTH(dt)=l;
        if (!isNull(names)) TRUELENGTH(names)=l;
        setselfref(dt);
    } else {
        tl = TRUELENGTH(dt);
        if (tl<0) error("Internal error, tl of class is marked but tl<0.");  // R <= 2.13.2 and we didn't catch uninitialized tl somehow
        if (tl>0 && tl<l) error("Internal error, please report (including result of sessionInfo()) to datatable-help: tl (%d) < l (%d) but tl of class is marked.", tl, l);
        if (tl>l+1000) warning("tl (%d) is greater than 1000 items over-allocated (l = %d). If you didn't set the datatable.alloccol option to be very large, please report this to datatable-help including the result of sessionInfo().",tl,l);
        // TO DO:  realloc names if selfrefnamesok (users can setattr(x,"name") themselves for example.
        // if (TRUELENGTH(getAttrib(dt,R_NamesSymbol))!=tl)
        //    error("Internal error: tl of dt passes checks, but tl of names (%d) != tl of dt (%d)", tl, TRUELENGTH(getAttrib(dt,R_NamesSymbol)));
    }
    if (n<l) warning("table has %d column slots in use. Attept to allocate less (%d)",l,n);
    if (n>tl) {
        PROTECT(newdt = allocVector(VECSXP, n));   // to do, use growVector here.
        PROTECT(newnames = allocVector(STRSXP, n));
        for (i=0; i<l; i++) {
            SET_VECTOR_ELT(newdt,i,VECTOR_ELT(dt,i));
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
	    }
	    DUPLICATE_ATTRIB(newdt, dt);
        // copyMostAttrib(dt, newdt);
        LENGTH(newdt) = l;  //prefered due to the SETLENGTH vs SET_LENGTH confusion (inconsistent with SET_TRUELENGTH)
        LENGTH(newnames) = l;
        TRUELENGTH(newdt) = n;
        TRUELENGTH(newnames) = n;
        setAttrib(newdt, R_NamesSymbol, newnames);
        if (isNull(getAttrib(newdt, R_ClassSymbol))) error("newdt has null class");
        setselfref(newdt);
        // SET_NAMED(dt,1);  // for some reason, R seems to set NAMED=2 via setAttrib?  Need NAMED to be 1 for passing to assign via a .C dance before .Call (which sets NAMED to 2), and we can't use .C with DUP=FALSE on lists.
        dt = newdt;
        UNPROTECT(2);
    } else {
        // Reduce the allocation (most likely to the same value as length).
        //if (n!=l) warning("Reducing alloc cols from %d to %d, but not to length (%d)",TRUELENGTH(dt),n,LENGTH(dt));
        TRUELENGTH(dt) = n;
        if (!isNull(names)) TRUELENGTH(names) = n;
    }
    return(dt);
}

SEXP alloccolwrapper(SEXP dt, SEXP newncol, SEXP verbose) {
    return(alloccol(dt, INTEGER(newncol)[0], LOGICAL(verbose)[0]));
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

SEXP setlength(SEXP x, SEXP n) {
    // Currently, just so .SD can be created once at R level for largest group then setup for first group
    LENGTH(x) = INTEGER(n)[0];
    return(R_NilValue);
}

SEXP selfrefokwrapper(SEXP x, SEXP verbose) {
    SEXP ans;
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = _selfrefok(x,FALSE,LOGICAL(verbose)[0]);
    UNPROTECT(1);
    return(ans);
}

SEXP setcharvec(SEXP x, SEXP which, SEXP new)
{
    // checks have already been made at R level
    for (int i=0; i<LENGTH(which); i++)
        SET_STRING_ELT(x, INTEGER(which)[i]-1, STRING_ELT(new, i));
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


