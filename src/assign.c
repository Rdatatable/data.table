#include "data.table.h"
#include <Rdefines.h>
#include <Rmath.h> 
#include <Rversion.h>

static SEXP *saveds=NULL;
static R_len_t *savedtl=NULL, nalloc=0, nsaved=0;

static void finalizer(SEXP p)
{
    SEXP x;
    R_len_t n, l, tl;
    if(!R_ExternalPtrAddr(p)) error("Internal error: finalizer hasn't received an ExternalPtr");
    p = R_ExternalPtrTag(p);
    if (!isString(p)) error("Internal error: finalizer's ExternalPtr doesn't see names in tag");
    l = LENGTH(p);
    tl = TRUELENGTH(p);
    if (l<0 || tl<l) error("Internal error: finalizer sees l=%d, tl=%d",l,tl);
    n = tl-l;
    if (n==0) {
        // gc's ReleaseLargeFreeVectors() will have reduced R_LargeVallocSize by the correct amount
        // already, so nothing to do (but almost never the case).
        return;
    }
    x = PROTECT(allocVector(VECSXP, 50));   // 50 so it's big enough to be on LargeVector heap. See NodeClassSize in memory.c:allocVector
    SETLENGTH(x,50+n*2);  // 1*n for the names, 1*n for the VECSXP itself (both are over allocated)
    UNPROTECT(1);
    return;
}

void setselfref(SEXP x) {   
    SEXP p;
    // Store pointer to itself so we can detect if the object has been copied. See
    // ?copy for why copies are not just inefficient but cause a problem for over-allocated data.tables.
    // Called from C only, not R level, so returns void.
    setAttrib(x, SelfRefSymbol, p=R_MakeExternalPtr(
        R_NilValue,                  // for identical() to return TRUE. identical() doesn't look at tag and prot
        getAttrib(x, R_NamesSymbol), // to detect if names has been replaced and its tl lost, e.g. setattr(DT,"names",...)
        PROTECT(R_MakeExternalPtr(   // to avoid an infinite loop in object.size(), if prot=x here
            x,                       // to know if this data.table has been copied by key<-, attr<-, names<-, etc.
            R_NilValue,              // this tag and prot currently unused
            R_NilValue
        ))
    ));
    R_RegisterCFinalizerEx(p, finalizer, FALSE);
    UNPROTECT(1);  // The PROTECT above is needed by --enable-strict-barrier (it seems, iiuc)
    
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

static int _selfrefok(SEXP x, Rboolean checkNames, Rboolean verbose) {
    SEXP v, p, tag, prot, names;
    v = getAttrib(x, SelfRefSymbol);
    if (v==R_NilValue || TYPEOF(v)!=EXTPTRSXP) {
        // .internal.selfref missing is expected and normal for i) a pre v1.7.8 data.table loaded
        //  from disk, and ii) every time a new data.table is over-allocated for the first time.
        //  Not being an extptr is for when users contruct a data.table via structure() using dput, post
        //  a question, and find the extptr doesn't parse so put quotes around it (for example).
        //  In both cases the selfref is not ok.
        return 0;
    }
    p = R_ExternalPtrAddr(v);
    if (p==NULL) {
        if (verbose) Rprintf(".internal.selfref ptr is NULL. This is expected and normal for a data.table loaded from disk. If not, please report to datatable-help.\n");
        return -1;
    }
    if (!isNull(p)) error("Internal error: .internal.selfref ptr is not NULL or R_NilValue");
    tag = R_ExternalPtrTag(v);
    if (!(isNull(tag) || isString(tag))) error("Internal error: .internal.selfref tag isn't NULL or a character vector");
    names = getAttrib(x, R_NamesSymbol);
    if (names != tag && isString(names))
        SET_TRUELENGTH(names, LENGTH(names)); 
        // R copied this vector not data.table; it's not actually over-allocated. It looks over-allocated
        // because R copies the original vector's tl over despite allocating length.
    prot = R_ExternalPtrProtected(v);
    if (TYPEOF(prot) != EXTPTRSXP)   // Very rare. Was error(".internal.selfref prot is not itself an extptr").
        return 0;                    // See http://stackoverflow.com/questions/15342227/getting-a-random-internal-selfref-error-in-data-table-for-r
    if (x != R_ExternalPtrAddr(prot))
        SET_TRUELENGTH(x, LENGTH(x));  // R copied this vector not data.table, it's not actually over-allocated
    return checkNames ? names==tag : x==R_ExternalPtrAddr(prot);
}

static Rboolean selfrefok(SEXP x, Rboolean verbose) {   // for readability
    return(_selfrefok(x, FALSE, verbose)==1);
}
static Rboolean selfrefnamesok(SEXP x, Rboolean verbose) {
    return(_selfrefok(x, TRUE, verbose)==1);
}

static SEXP shallow(SEXP dt, R_len_t n)
{
    // called from alloccol where n is checked carefully, or from shallow() at R level
    // where n is set to truelength (i.e. a shallow copy only with no size change)
    SEXP newdt, names, newnames;
    R_len_t i,l;
    int protecti=0;
    PROTECT(newdt = allocVector(VECSXP, n));   // to do, use growVector here?
    protecti++;
    //copyMostAttrib(dt, newdt);   // including class
    DUPLICATE_ATTRIB(newdt, dt);
    // TO DO: keepattr() would be faster, but can't because shallow isn't merely a shallow copy. It
    //        also increases truelength. Perhaps make that distinction, then, and split out, but marked
    //        so that the next change knows to duplicate.
    //        Does copyMostAttrib duplicate each attrib or does it point? It seems to point, hence DUPLICATE_ATTRIB
    //        for now otherwise example(merge.data.table) fails (since attr(d4,"sorted") gets written by setnames).
    l = LENGTH(dt);
    for (i=0; i<l; i++)
        SET_VECTOR_ELT(newdt,i,VECTOR_ELT(dt,i));
    names = getAttrib(dt,R_NamesSymbol); 
    PROTECT(newnames = allocVector(STRSXP, n));
    protecti++;
    if (length(names)) {
        if (length(names) < l) error("Internal error: length(names)>0 but <length(dt)");
        for (i=0; i<l; i++)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
    } // else an unnamed data.table is valid e.g. unname(DT) done by ggplot2, and .SD may have its names cleared in dogroups, but shallow will always create names for data.table(NULL) which has 100 slots all empty so you can add to an empty data.table by reference ok.
    setAttrib(newdt, R_NamesSymbol, newnames);
    // setAttrib appears to change length and truelength, so need to do that first _then_ SET next,
    // otherwise (if the SET were were first) the 100 tl is assigned to length.
    SETLENGTH(newnames,l);
    SET_TRUELENGTH(newnames,n);
    SETLENGTH(newdt,l);
    SET_TRUELENGTH(newdt,n);
    setselfref(newdt);
    // SET_NAMED(dt,1);  // for some reason, R seems to set NAMED=2 via setAttrib?  Need NAMED to be 1 for passing to assign via a .C dance before .Call (which sets NAMED to 2), and we can't use .C with DUP=FALSE on lists.
    UNPROTECT(protecti);
    return(newdt);
}
        

SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose)
{
    SEXP names, class;
    R_len_t l, tl;
    if (isNull(dt)) error("alloccol has been passed a NULL dt");
    if (TYPEOF(dt) != VECSXP) error("dt passed to alloccol isn't type VECSXP");
    class = getAttrib(dt, R_ClassSymbol);
    if (isNull(class)) error("dt passed to alloccol has no class attribute. Please report result of traceback() to datatable-help.");
    l = LENGTH(dt);
    names = getAttrib(dt,R_NamesSymbol);
    // names may be NULL when null.data.table() passes list() to alloccol for example.
    // So, careful to use length() on names, not LENGTH(). 
    if (length(names)!=l) error("Internal error: length of names (%d) is not length of dt (%d)",length(names),l);
    if (!selfrefok(dt,verbose))
        return shallow(dt,n);  // e.g. test 848 and 851 in R > 3.0.2  
    // TO DO:  test realloc names if selfrefnamesok (users can setattr(x,"name") themselves for example.
    // if (TRUELENGTH(getAttrib(dt,R_NamesSymbol))!=tl)
    //    error("Internal error: tl of dt passes checks, but tl of names (%d) != tl of dt (%d)", tl, TRUELENGTH(getAttrib(dt,R_NamesSymbol)));
    
    tl = TRUELENGTH(dt);
    if (tl<0) error("Internal error, tl of class is marked but tl<0.");  // R <= 2.13.2 and we didn't catch uninitialized tl somehow
    if (tl>0 && tl<l) error("Internal error, please report (including result of sessionInfo()) to datatable-help: tl (%d) < l (%d) but tl of class is marked.", tl, l);
    if (tl>l+1000) warning("tl (%d) is greater than 1000 items over-allocated (l = %d). If you didn't set the datatable.alloccol option to be very large, please report this to datatable-help including the result of sessionInfo().",tl,l);
    if (n>tl) return(shallow(dt,n)); // usual case (increasing alloc)
    if (n<tl) warning("Attempt to reduce allocation from %d to %d ignored. Can only increase allocation via shallow copy.",tl,n);
              // otherwise the finalizer can't clear up the Large Vector heap
    return(dt);
}

SEXP alloccolwrapper(SEXP dt, SEXP newncol, SEXP verbose) {
    if (!isInteger(newncol) || length(newncol)!=1) error("n must be integer length 1. Has datatable.alloccol somehow become unset?");
    if (!isLogical(verbose) || length(verbose)!=1) error("verbose must be TRUE or FALSE"); 
    return(alloccol(dt, INTEGER(newncol)[0], LOGICAL(verbose)[0]));
}

SEXP shallowwrapper(SEXP dt) {
    if (!selfrefok(dt,FALSE))
        return(shallow(dt, length(dt)));  // a manually created data.table via dput() and structure(), for example
    else 
        return(shallow(dt, TRUELENGTH(dt)));
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

SEXP selfrefokwrapper(SEXP x, SEXP verbose) {
    return ScalarInteger(_selfrefok(x,FALSE,LOGICAL(verbose)[0]));
}


void memrecycle(SEXP target, SEXP where, int r, int len, SEXP source);

SEXP assign(SEXP dt, SEXP rows, SEXP cols, SEXP newcolnames, SEXP values, SEXP verb)
{
    // For internal use only by := in [.data.table, and set()
    // newcolnames : add these columns (if any)
    // cols : column names or numbers corresponding to the values to set
    // rows : row numbers to assign
    R_len_t i, j, nrow, targetlen, vlen, r, oldncol, oldtncol, coln, protecti=0, newcolnum;
    SEXP targetcol, RHS, names, nullint, thisvalue, thisv, targetlevels, newcol, s, colnam, class, tmp, colorder, key;
    Rboolean verbose = LOGICAL(verb)[0], anytodelete=FALSE, clearkey=FALSE, isDataTable=FALSE;
    char *s1, *s2, *s3;
    int *buf, k=0;
    size_t size; // must be size_t otherwise overflow later in memcpy
    if (isNull(dt)) error("assign has been passed a NULL dt");
    if (TYPEOF(dt) != VECSXP) error("dt passed to assign isn't type VECSXP");
    
    class = getAttrib(dt, R_ClassSymbol);
    if (isNull(class)) error("Input passed to assign has no class attribute. Must be a data.table or data.frame.");
    // Check if there is a class "data.table" somewhere (#5115).
    // We allow set() on data.frame too; e.g. package Causata uses set() on a data.frame in tests/testTransformationReplay.R
    // := is only allowed on a data.table. However, the ":=" = stop(...) message in data.table.R will have already
    // detected use on a data.frame before getting to this point.
    for (i=0; i<length(class); i++) {   // There doesn't seem to be an R API interface to inherits(), but manually here isn't too bad.
        if (strcmp(CHAR(STRING_ELT(class, i)), "data.table") == 0) break;
    }
    if (i<length(class))
        isDataTable = TRUE;
    else {
        for (i=0; i<length(class); i++) {
            if (strcmp(CHAR(STRING_ELT(class, i)), "data.frame") == 0) break;
        }
        if (i == length(class)) error("Input is not a data.table, data.frame or an object that inherits from either.");
        isDataTable = FALSE;   // meaning data.frame from now on. Can use set() on existing columns but not add new ones because DF aren't over-allocated.
    }
    oldncol = LENGTH(dt);
    names = getAttrib(dt,R_NamesSymbol);
    if (isNull(names)) error("dt passed to assign has no names");
    if (length(names)!=oldncol)
        error("Internal error in assign: length of names (%d) is not length of dt (%d)",length(names),oldncol);
    if (oldncol<1) error("Cannot use := to add columns to a null data.table (no columns), currently. You can use := to add (empty) columns to a 0-row data.table (1 or more empty columns), though.");
    nrow = length(VECTOR_ELT(dt,0));
    if (isNull(rows)) {
        targetlen = nrow;
        // fast way to assign to whole column, without creating 1:nrow(x) vector up in R, or here in C
    } else {
        if (!LENGTH(rows)) return(dt);
        if (isReal(rows)) {
            rows = PROTECT(rows = coerceVector(rows, INTSXP));
            protecti++;
            warning("Coerced i from numeric to integer. Please pass integer for efficiency; e.g., 2L rather than 2");
        }    
        if (!isInteger(rows))
            error("i is type '%s'. Must be integer, or numeric is coerced with warning. If i is a logical subset, simply wrap with which(), and take the which() outside the loop if possible for efficiency.", type2char(TYPEOF(rows)));
        targetlen = length(rows);
        Rboolean anyToDo = FALSE;
        for (i=0;i<targetlen;i++) {
            if ((INTEGER(rows)[i]<0 && INTEGER(rows)[i]!=NA_INTEGER) || INTEGER(rows)[i]>nrow)
                error("i[%d] is %d which is out of range [1,nrow=%d].",i+1,INTEGER(rows)[i],nrow);
            if (INTEGER(rows)[i]>=1) anyToDo = TRUE;
        }
        if (!anyToDo) return(dt);  // all items of rows either 0 or NA, nothing to do.  
    }
    if (!length(cols)) {
        warning("length(LHS) = 0, meaning no columns to delete or assign RHS to.");
        return(dt);
    }
    // FR #2077 - set able to add new cols by reference
    if (isString(cols)) {
        PROTECT(tmp = chmatch(cols, names, 0, FALSE));
        protecti++;
        buf = (int *) R_alloc(length(cols), sizeof(int));
        for (i=0; i<length(cols); i++) {
            if (INTEGER(tmp)[i] == 0) buf[k++] = i;
        }
        if (k>0) {
            if (!isDataTable) error("set() on a data.frame is for changing existing columns, not adding new ones. Please use a data.table for that. data.table's are over-allocated and don't shallow copy.");
            PROTECT(newcolnames = allocVector(STRSXP, k));
            protecti++;
            for (i=0; i<k; i++) { 
                SET_STRING_ELT(newcolnames, i, STRING_ELT(cols, buf[i]));
                INTEGER(tmp)[buf[i]] = oldncol+i+1;
            }
        }
        cols = tmp;
    } else {
        if (isReal(cols)) {
            cols = PROTECT(cols = coerceVector(cols, INTSXP));
            protecti++;
            warning("Coerced j from numeric to integer. Please pass integer for efficiency; e.g., 2L rather than 2");
        }
        if (!isInteger(cols))
            error("j is type '%s'. Must be integer, character, or numeric is coerced with warning.", type2char(TYPEOF(cols)));
    }
    if (any_duplicated(cols,FALSE)) error("Can't assign to the same column twice in the same query (duplicates detected).");
    if (!isNull(newcolnames) && !isString(newcolnames)) error("newcolnames is supplied but isn't a character vector");
    if (isNull(values)) {
        if (!length(cols)) {
            warning("RHS is NULL, meaning delete columns(s). But, no columns in LHS to delete.");
            return(dt);
        }
    } else {
        if (TYPEOF(values)==VECSXP) {
            if (length(cols)>1) {
                if (length(values)==0) error("Supplied %d columns to be assigned an empty list (which may be an empty data.table or data.frame since they are lists too). To delete multiple columns use NULL instead. To add multiple empty list columns, use list(list()).", length(cols));
                if (length(values)>length(cols))
                    warning("Supplied %d columns to be assigned a list (length %d) of values (%d unused)", length(cols), length(values), length(values)-length(cols));  
                else if (length(cols)%length(values) != 0)
                    warning("Supplied %d columns to be assigned a list (length %d) of values (recycled leaving remainder of %d items).",length(cols),length(values),length(cols)%length(values));
            } // else it's a list() column being assigned to one column       
        }
    }
    // Check all inputs :
    for (i=0; i<length(cols); i++) {
        coln = INTEGER(cols)[i];
        if (coln<1 || coln>oldncol+length(newcolnames)) {
            if (!isDataTable) error("Item %d of column numbers in j is %d which is outside range [1,ncol=%d]. set() on a data.frame is for changing existing columns, not adding new ones. Please use a data.table for that.", i+1, coln, oldncol);
            else error("Item %d of column numbers in j is %d which is outside range [1,ncol=%d]. Use column names instead in j to add new columns.", i+1, coln, oldncol);
        }
        coln--;
        if (TYPEOF(values)==VECSXP && (length(cols)>1 || length(values)==1))
            thisvalue = VECTOR_ELT(values,i%LENGTH(values));
        else
            thisvalue = values;   // One vector applied to all columns, often NULL or NA for example
        vlen = length(thisvalue);
        if (coln+1 <= oldncol) colnam = STRING_ELT(names,coln);
        else colnam = STRING_ELT(newcolnames,coln-length(names));
        if (coln+1 <= oldncol && isNull(thisvalue)) continue;  // delete existing column(s) afterwards, near end of this function
        if (vlen<1 && nrow>0) {
            if (coln+1 <= oldncol) {
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
        if (!(isVectorAtomic(thisvalue) || isNewList(thisvalue)))  // NULL had a continue earlier above
            error("RHS of assignment is not NULL, not an an atomic vector (see ?is.atomic) and not a list column.");
        if (isMatrix(thisvalue) && (j=INTEGER(getAttrib(thisvalue, R_DimSymbol))[1]) > 1)  // matrix passes above (considered atomic vector)
            warning("%d column matrix RHS of := will be treated as one vector", j);
        if ((coln+1)<=oldncol && isFactor(VECTOR_ELT(dt,coln)) &&
            !isString(thisvalue) && TYPEOF(thisvalue)!=INTSXP && !isReal(thisvalue))  // !=INTSXP includes factor
            error("Can't assign to column '%s' (type 'factor') a value of type '%s' (not character, factor, integer or numeric)", CHAR(STRING_ELT(names,coln)),type2char(TYPEOF(thisvalue)));
        if (nrow>0) {
            if (vlen>targetlen)
                warning("Supplied %d items to be assigned to %d items of column '%s' (%d unused)", vlen, targetlen,CHAR(colnam),vlen-targetlen);  
            else if (vlen>0 && targetlen%vlen != 0)
                warning("Supplied %d items to be assigned to %d items of column '%s' (recycled leaving remainder of %d items).",vlen,targetlen,CHAR(colnam),targetlen%vlen);
        }
    }
    key = getAttrib(dt,install("sorted"));
    if (length(key)) {
        // if assigning to any key column, then drop the key. any() and subsetVector() don't seem to be
        // exposed by R API at C level, so this is done here long hand.
        PROTECT(tmp = allocVector(STRSXP, LENGTH(cols)));
        protecti++;
        for (i=0;i<LENGTH(cols);i++) SET_STRING_ELT(tmp,i,STRING_ELT(names,INTEGER(cols)[i]-1));
        PROTECT(tmp = chmatch(tmp, key, 0, TRUE));
        protecti++;
        for (i=0;i<LENGTH(tmp);i++) if (LOGICAL(tmp)[i]) {clearkey=TRUE; break;}
    }
    // having now checked the inputs, from this point there should be no errors,
    // so we can now proceed to modify DT by reference.
    if (length(newcolnames)) {
        //if (length(rows)!=0) error("Attempt to add new column(s) and set subset of rows at the same time. Create the new column(s) first, and then you'll be able to assign to a subset. If i is set to 1:nrow(x) then please remove that (no need, it's faster without).");
        oldtncol = TRUELENGTH(dt);   // TO DO: oldtncol can be just called tl now, as we won't realloc here any more.
        
        if (oldtncol<oldncol) error("Internal error, please report (including result of sessionInfo()) to datatable-help: oldtncol (%d) < oldncol (%d) but tl of class is marked.", oldtncol, oldncol);
        if (oldtncol>oldncol+1000L) warning("truelength (%d) is greater than 1000 items over-allocated (length = %d). See ?truelength. If you didn't set the datatable.alloccol option very large, please report this to datatable-help including the result of sessionInfo().",oldtncol, oldncol); 
        
        if (oldtncol < oldncol+LENGTH(newcolnames))
            error("Internal logical error. DT passed to assign has not been allocated enough column slots. l=%d, tl=%d, adding %d", oldncol, oldtncol, LENGTH(newcolnames));
        if (!selfrefnamesok(dt,verbose))
            error("It appears that at some earlier point, names of this data.table have been reassigned. Please ensure to use setnames() rather than names<- or colnames<-. Otherwise, please report to datatable-help.");
            // Can growVector at this point easily enough, but it shouldn't happen in first place so leave it as
            // strong error message for now.
        else if (TRUELENGTH(names) != oldtncol)
            error("selfrefnames is ok but tl names [%d] != tl [%d]", TRUELENGTH(names), oldtncol);
        SETLENGTH(dt, oldncol+LENGTH(newcolnames));
        SETLENGTH(names, oldncol+LENGTH(newcolnames));
        for (i=0; i<LENGTH(newcolnames); i++)
            SET_STRING_ELT(names,oldncol+i,STRING_ELT(newcolnames,i));
        // truelengths of both already set by alloccol
    }
    for (i=0; i<length(cols); i++) {
        coln = INTEGER(cols)[i]-1;
        if (TYPEOF(values)==VECSXP && (LENGTH(cols)>1 || LENGTH(values)==1))
            thisvalue = VECTOR_ELT(values,i%LENGTH(values));
        else
            thisvalue = values;   // One vector applied to all columns, often NULL or NA for example
        if (TYPEOF(thisvalue)==NILSXP) {
            if (!isNull(rows)) error("When deleting columns, i should not be provided");
            anytodelete = TRUE;
            continue;   // delete column(s) afterwards, below this loop
        }
        vlen = length(thisvalue);
        if (length(rows)==0 && targetlen==vlen) {
            if (  NAMED(thisvalue)==2 ||  // set() protects the NAMED of atomic vectors from .Call setting arguments to 2 by wrapping with list
                 (TYPEOF(values)==VECSXP && i>LENGTH(values)-1)) { // recycled RHS would have columns pointing to others, #2298.
                if (verbose) {
                    if (NAMED(thisvalue)==2) Rprintf("RHS for item %d has been duplicated because NAMED is %d, but then is being plonked.\n",i+1, NAMED(thisvalue));
                    else Rprintf("RHS for item %d has been duplicated because the list of RHS values (length %d) is being recycled, but then is being plonked.\n", i+1, length(values));
                }
                thisvalue = duplicate(thisvalue);   // PROTECT not needed as assigned as element to protected list below.
            } else {
                if (verbose) Rprintf("Direct plonk of unnamed RHS, no copy.\n");  // e.g. DT[,a:=as.character(a)] as tested by 754.3
            }
            SET_VECTOR_ELT(dt,coln,thisvalue);                   // plonk new column in as it's already the correct length
            setAttrib(thisvalue, R_NamesSymbol, R_NilValue);     // clear names such as  DT[,a:=mapvector[a]]
            setAttrib(thisvalue, R_DimSymbol, R_NilValue);       // so that matrix is treated as vector
            setAttrib(thisvalue, R_DimNamesSymbol, R_NilValue);  // the 3rd of the 3 attribs not copied by copyMostAttrib, for consistency.
            continue;
        }
        if (coln+1 > oldncol) {  // new column
            if (length(rows)) 
                newcol = allocNAVector(TYPEOF(thisvalue),nrow);  // fill with NAs first for where 'rows' (a subset) doesn't touch
            else              
                newcol = allocVector(TYPEOF(thisvalue),nrow);    // PROTECT not needed, protected by SET_VECTOR_ELT on next line
            SET_VECTOR_ELT(dt,coln,newcol);
            if (isVectorAtomic(thisvalue)) copyMostAttrib(thisvalue,newcol);  // class etc but not names
            // else for lists (such as data.frame and data.table) treat them as raw lists and drop attribs
            if (vlen<1) continue;   // e.g. DT[,newcol:=integer()] (adding new empty column)
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
                if (isNull(targetlevels)) error("somehow this factor column has no levels");
                if (isString(thisvalue)) {
                    savetl_init();  // ** TO DO **: remove allocs that could fail between here and _end, or different way
                    for (j=0; j<length(thisvalue); j++) {
                        s = STRING_ELT(thisvalue,j);
                        if (TRUELENGTH(s)>0) {
                            savetl(s);  // pre-2.14.0 this will save all the uninitialised truelengths
                                        // so 2.14.0+ may be faster, but isn't required.
                                        // as from v1.8.0 we assume R's internal hash is positive, so don't
                                        // save the uninitialised truelengths that by chance are negative
                        }
                        SET_TRUELENGTH(s,0);
                    }
                    for (j=0; j<length(targetlevels); j++) {
                        s = STRING_ELT(targetlevels,j);
                        if (TRUELENGTH(s)>0) savetl(s);
                        SET_TRUELENGTH(s,j+1);
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
                            SET_TRUELENGTH(thisv,++addi+length(targetlevels));  
                        }
                        INTEGER(RHS)[j] = TRUELENGTH(thisv);
                    }
                    if (addi > 0) {
                        R_len_t oldlen = length(targetlevels);
                        PROTECT(targetlevels = growVector(targetlevels, oldlen+addi));
                        protecti++;
                        for (j=0; j<addi; j++)
                            SET_STRING_ELT(targetlevels, oldlen+j, STRING_ELT(addlevels, j));
                        setAttrib(targetcol, R_LevelsSymbol, targetlevels);
                    }
                    for (j=0; j<length(targetlevels); j++) SET_TRUELENGTH(STRING_ELT(targetlevels,j),0);  // important to reinstate 0 for countingcharacterorder and HASHPRI (if any) as done by savetl_end().
                    savetl_end();
                } else {
                    // value is either integer or numeric vector
                    if (TYPEOF(thisvalue)!=INTSXP && !isReal(thisvalue))
                        error("Internal logical error. Up front checks (before starting to modify DT) didn't catch type of RHS ('%s') assigning to factor column '%s'. Please report to datatable-help.", type2char(TYPEOF(thisvalue)), CHAR(STRING_ELT(names,coln)));
                    if (isReal(thisvalue)) {
                        PROTECT(RHS = coerceVector(thisvalue,INTSXP));
                        protecti++;
                        warning("Coerced 'double' RHS to 'integer' to match the factor column's underlying type. Character columns are now recommended (can be in keys), or coerce RHS to integer or character first.");
                    } else RHS = thisvalue;
                    for (j=0; j<length(RHS); j++) {
                        if (INTEGER(RHS)[j]<1 || INTEGER(RHS)[j]>LENGTH(targetlevels)) {
                            warning("RHS contains %d which is outside the levels range ([1,%d]) of column %d, NAs generated", INTEGER(RHS)[j], LENGTH(targetlevels), i+1);
                            INTEGER(RHS)[j] = NA_INTEGER;
                        }
                    }
                }
            } else {
                if (TYPEOF(targetcol) == TYPEOF(thisvalue))
                    RHS = thisvalue;
                else {
                    // coerce the RHS to match the type of the column, unlike [<-.data.frame, for efficiency.
                    if (isString(targetcol) && isFactor(thisvalue)) {
                        PROTECT(RHS = asCharacterFactor(thisvalue));
                        protecti++;
                        if (verbose) Rprintf("Coerced factor to character to match the column's type (coercion is inefficient)\n");   // TO DO:  datatable.pedantic would turn this into warning
                    } else {
                        PROTECT(RHS = coerceVector(thisvalue,TYPEOF(targetcol)));
                        protecti++;
                        // FR #2551, added test for equality between RHS and thisvalue to not provide the warning when length(thisvalue) == 1
                        if ( length(thisvalue) == 1 && TYPEOF(RHS) != VECSXP && TYPEOF(thisvalue) != VECSXP && (
                             (isReal(thisvalue) && isInteger(targetcol) && REAL(thisvalue)[0] == INTEGER(RHS)[0]) || 
                          (isLogical(thisvalue) && LOGICAL(thisvalue)[0] == NA_LOGICAL) || 
                                   (isReal(RHS) && isInteger(thisvalue)) )) {
                              ;
                          } else {
                            s1 = (char *)type2char(TYPEOF(targetcol));
                            s2 = (char *)type2char(TYPEOF(thisvalue));
                            if (isReal(thisvalue)) s3="; may have truncated precision"; else s3="";
                            warning("Coerced '%s' RHS to '%s' to match the column's type%s. Either change the target column to '%s' first (by creating a new '%s' vector length %d (nrows of entire table) and assign that; i.e. 'replace' column), or coerce RHS to '%s' (e.g. 1L, NA_[real|integer]_, as.*, etc) to make your intent clear and for speed. Or, set the column type correctly up front when you create the table and stick to it, please.", s2, s1, s3, s2, s2, LENGTH(VECTOR_ELT(dt,0)), s1);
                        }
                    }
                }
            }
        }
        memrecycle(targetcol, rows, 0, length(rows) ? LENGTH(rows) : LENGTH(targetcol), RHS);  // also called from dogroups
    }
    if (anytodelete) {
        // Delete any columns assigned NULL (there was a 'continue' earlier in loop above)
        // In reverse order to make repeated memmove easy. Otherwise cols would need to be updated as well after each delete.
        PROTECT(colorder = duplicate(cols));
        protecti++;
        R_isort(INTEGER(colorder),LENGTH(cols));
        PROTECT(colorder = match(cols, colorder, 0));   // actually matches colorder to cols (oddly, arguments are that way around)
        protecti++;
        // Can't find a visible R entry point to return ordering of cols, above is only way I could find.
        // Need ordering (rather than just sorting) because the RHS corresponds in order to the LHS.
        
        for (r=LENGTH(cols)-1; r>=0; r--) {
            i = INTEGER(colorder)[r]-1;
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
                SET_VECTOR_ELT(dt, LENGTH(dt)-1, R_NilValue);
                SETLENGTH(dt, LENGTH(dt)-1);
                // adding using := by group relies on NULL here to know column slot is empty.
                // good to tidy up the vector anyway.
                memmove((char *)DATAPTR(names)+coln*size,     
                    (char *)DATAPTR(names)+(coln+1)*size,
                    (LENGTH(names)-coln-1)*size);
                SET_STRING_ELT(names, LENGTH(names)-1, NA_STRING);  // no need really, just to be tidy.
                SETLENGTH(names, LENGTH(names)-1);
                if (LENGTH(names)==0) {
                    // That was last column deleted, leaving NULL data.table, so we need to reset .row_names, so that it really is the NULL data.table.
                    PROTECT(nullint=allocVector(INTSXP, 0));
                    protecti++;
                    setAttrib(dt, R_RowNamesSymbol, nullint);  // i.e. .set_row_names(0)
                    //setAttrib(dt, R_NamesSymbol, R_NilValue);
                }
            }
        }
    }
    if (clearkey) {
        // If a key column is being assigned to, clear the key, since it may change the row ordering.
        // More likely that users will assign to non-key columns, though, most of the time.
        setAttrib(dt, install("sorted"), R_NilValue);
    }
    UNPROTECT(protecti);
    return(dt);  // needed for `*tmp*` mechanism (when := isn't used), and to return the new object after a := for compound syntax.
}

void memrecycle(SEXP target, SEXP where, int start, int len, SEXP source)
// like memcpy but recycles source and takes care of aging
// 'where' a 1-based INTEGER vector subset of target to assign to,  or NULL or integer()
// assigns to target[start:start+len-1] or target[where[start:start+len-1]]  where start is 0-based
{
    int r = 0, w;
    if (len<1) return;
    int slen = length(source) > len ? len : length(source); // fix for 5647. when length(source) > len, slen must be len.
    if (slen<1) return;
    if (TYPEOF(target) != TYPEOF(source)) error("Internal error: TYPEOF(target)['%s']!=TYPEOF(source)['%s']", type2char(TYPEOF(target)),type2char(TYPEOF(source)));
    size_t size = SIZEOF(target);
    if (!length(where)) {
        switch (TYPEOF(target)) {
        case INTSXP : case REALSXP : case LGLSXP :
            break;
        case STRSXP :
            for (; r<slen; r++)     // only one SET_STRING_ELT per RHS item is needed to set generations (overhead)
                SET_STRING_ELT(target, start+r, STRING_ELT(source, r));
            break;
        case VECSXP :
            for (; r<slen; r++)
                SET_VECTOR_ELT(target, start+r, VECTOR_ELT(source, r));
                // TO DO: if := in future could ever change a list item's contents by reference, would need to duplicate at that point
            break;
        default :
            error("Unsupported type '%s'", type2char(TYPEOF(target)));
        }
        if (slen == 1) {  
            if (size==4) for (; r<len; r++)
                INTEGER(target)[start+r] = INTEGER(source)[0];   // copies pointer on 32bit, sizes checked in init.c
            else for (; r<len; r++)
                REAL(target)[start+r] = REAL(source)[0];         // copies pointer on 64bit, sizes checked in init.c
        } else if (slen<10) {    // 10 is just a guess for when memcpy is faster. Certainly memcpy is slower when slen==1, but that's the most common case by far so not high priority to discover the optimum here. TO DO: revisit
            if (size==4) for (; r<len; r++)
                INTEGER(target)[start+r] = INTEGER(source)[r%slen];
            else for (; r<len; r++)
                REAL(target)[start+r] = REAL(source)[r%slen];
        } else {
            for (r=r>0?1:0; r<(len/slen); r++) {   // if the first slen were done in the switch above, convert r=slen to r=1
                memcpy((char *)DATAPTR(target) + (start+r*slen)*size,
                       (char *)DATAPTR(source),
                       slen * size);
            }
            memcpy((char *)DATAPTR(target) + (start+r*slen)*size,
                   (char *)DATAPTR(source),
                   (len%slen) * size);
        }
    } else {
        switch (TYPEOF(target)) {
        case INTSXP : case REALSXP : case LGLSXP :
            break;
        case STRSXP :
            for (; r<slen; r++) {
                w = INTEGER(where)[start+r]; if (w<1) continue;  // 0 or NA
                SET_STRING_ELT(target, w-1, STRING_ELT(source, r));
            }
            break;
        case VECSXP :
            for (; r<slen; r++) {
                w = INTEGER(where)[start+r]; if (w<1) continue;
                SET_VECTOR_ELT(target, w-1, VECTOR_ELT(source, r));
            }
            break;
        default :
            error("Unsupported type '%s'", type2char(TYPEOF(target)));
        }    
        if (slen == 1) {
            if (size==4) for (; r<len; r++) {
                w = INTEGER(where)[start+r]; if (w<1) continue;
                INTEGER(target)[ w-1 ] = INTEGER(source)[0];
            } else for (; r<len; r++) {
                w = INTEGER(where)[start+r]; if (w<1) continue;
                REAL(target)[ w-1 ] = REAL(source)[0];
            }
        } else {
            if (size==4) for (; r<len; r++) {
                w = INTEGER(where)[start+r]; if (w<1) continue;
                INTEGER(target)[ w-1 ] = INTEGER(source)[r%slen];
            } else for (; r<len; r++) {
                w = INTEGER(where)[start+r]; if (w<1) continue;
                REAL(target)[ w-1 ] = REAL(source)[r%slen];
            }
        }
        // if slen>10 it may be worth memcpy, but we'd need to first know if 'where' was a contiguous subset
    }
}

SEXP allocNAVector(SEXPTYPE type, R_len_t n)
{
    // an allocVector following with initialization to NA since a subassign to a new column using :=
    // routinely leaves untouched items (rather than 0 or "" as allocVector does with it's memset)
    // We guess that author of allocVector would have liked to initialize with NA but was prevented since memset
    // is restricted to one byte.
    R_len_t i;
    SEXP v;
    PROTECT(v = allocVector(type, n));
    switch(type) {
    // NAs are special; no need to worry about generations.
    case INTSXP :
    case LGLSXP :
        for (i=0; i<n; i++) INTEGER(v)[i] = NA_INTEGER;
        break;
    case REALSXP :
        for (i=0; i<n; i++) REAL(v)[i] = NA_REAL;
        break;
    case STRSXP :
        for (i=0; i<n; i++) ((SEXP *)DATAPTR(v))[i] = NA_STRING;
        break;
    case VECSXP :
        // list columns already have each item initialized to NULL
        break;
    default :
        error("Unsupported type '%s'", type2char(type));
    }
    UNPROTECT(1);
    return(v);
}

void savetl_init() {
    if (nsaved || nalloc || saveds || savedtl) error("Internal error: savetl_init checks failed (%d %d %p %p). Please report to datatable-help.", nsaved, nalloc, saveds, savedtl);
    nsaved = 0;
    nalloc = 100;
    saveds = (SEXP *)malloc(nalloc * sizeof(SEXP));
    if (saveds == NULL) error("Couldn't allocate saveds in savetl_init");
    savedtl = (R_len_t *)malloc(nalloc * sizeof(R_len_t));
    if (savedtl == NULL) {
        free(saveds);
        error("Couldn't allocate saveds in savetl_init");
    }
}

void savetl(SEXP s)
{
    if (nsaved>=nalloc) {
        nalloc *= 2;
        char *tmp;
        tmp = (char *)realloc(saveds, nalloc * sizeof(SEXP));
        if (tmp == NULL) {
            savetl_end();
            error("Couldn't realloc saveds in savetl");
        }
        saveds = (SEXP *)tmp;
        tmp = (char *)realloc(savedtl, nalloc * sizeof(R_len_t));
        if (tmp == NULL) {
            savetl_end();
            error("Couldn't realloc savedtl in savetl");
        }
        savedtl = (R_len_t *)tmp;
    }
    saveds[nsaved] = s;
    savedtl[nsaved] = TRUELENGTH(s);
    nsaved++;
}

void savetl_end() {
    // Can get called if nothing has been saved yet (nsaved==0), or even if _init() hasn't been called yet (pointers NULL). Such
    // as to clear up before error. Also, it might be that nothing needed to be saved anyway.
    for (int i=0; i<nsaved; i++) SET_TRUELENGTH(saveds[i],savedtl[i]);
    free(saveds);  // does nothing on NULL input
    free(savedtl);
    nsaved = nalloc = 0;
    saveds = NULL;
    savedtl = NULL;
}

SEXP setcharvec(SEXP x, SEXP which, SEXP new)
{
    int w;
    if (!isString(x)) error("x must be a character vector");
    if (!isInteger(which)) error("'which' must be an integer vector");
    if (!isString(new)) error("'new' must be a character vector");
    if (LENGTH(new)!=LENGTH(which)) error("'new' is length %d. Should be the same as length of 'which' (%d)",LENGTH(new),LENGTH(which));
    for (int i=0; i<LENGTH(which); i++) {
        w = INTEGER(which)[i];
        if (w==NA_INTEGER || w<1 || w>LENGTH(x)) error("Item %d of 'which' is %d which is outside range of the length %d character vector", i+1,w,LENGTH(x));
        SET_STRING_ELT(x, w-1, STRING_ELT(new, i));
    }
    return R_NilValue;
}

SEXP setcolorder(SEXP x, SEXP o)
{
    // checks have already been made at R level in setcolorder()
    // reording columns by reference makes no difference to generations
    // so there's no need to use SET_* api.
    SEXP *tmp = Calloc(LENGTH(x),SEXP);
    for (int i=0; i<LENGTH(x); i++)
        tmp[i] = VECTOR_ELT(x, INTEGER(o)[i]-1);
    memcpy((char *)DATAPTR(x),(char *)tmp,LENGTH(x)*sizeof(char *)); // sizeof is of type size_t (unsigned) - so no overflow here
    SEXP names = getAttrib(x,R_NamesSymbol);
    if (isNull(names)) error("dt passed to setcolorder has no names");
    for (int i=0; i<LENGTH(x); i++)
        tmp[i] = STRING_ELT(names, INTEGER(o)[i]-1);
    memcpy((char *)DATAPTR(names),(char *)tmp,LENGTH(x)*sizeof(char *));
    // No need to change key (if any); sorted attribute is column names not positions
    Free(tmp);
    return(R_NilValue);
}

SEXP pointWrapper(SEXP to, SEXP to_idx, SEXP from, SEXP from_idx) {

    R_len_t i, fidx, tidx, l_to=length(to), l_from=length(from), l_idx=length(from_idx);
    if (!isNewList(to) || !isNewList(from)) error("'to' and 'from' must be of type list");
    if (length(from_idx) != length(to_idx) || l_idx == 0) error("'from_idx' and 'to_idx' must be non-empty integer vectors of same length.");
    for (i=0; i<l_idx; i++) {
        fidx = INTEGER(from_idx)[i]-1; // 1-based to 0-based
        tidx = INTEGER(to_idx)[i]-1;   // 1-based to 0-based
        if (fidx < 0 || fidx > l_from-1) error("invalid from_idx[%d]=%d, falls outside 1 and length(from)=%d.", i+1, fidx, l_from);
        if (tidx < 0 || tidx > l_to-1) error("invalid to_idx[%d]=%d, falls outside 1 and length(to)=%d.", i+1, tidx, l_to);
        SET_VECTOR_ELT(to, tidx, VECTOR_ELT(from, fidx));
    }
    return(to);
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


