#include "data.table.h"
#include <Rdefines.h>
#include <Rmath.h>

static void finalizer(SEXP p)
{
  SEXP x;
  R_len_t n, l, tl;
  if(!R_ExternalPtrAddr(p)) error("Internal error: finalizer hasn't received an ExternalPtr"); // # nocov
  p = R_ExternalPtrTag(p);
  if (!isString(p)) error("Internal error: finalizer's ExternalPtr doesn't see names in tag"); // # nocov
  l = LENGTH(p);
  tl = TRUELENGTH(p);
  if (l<0 || tl<l) error("Internal error: finalizer sees l=%d, tl=%d",l,tl); // # nocov
  n = tl-l;
  if (n==0) {
    // gc's ReleaseLargeFreeVectors() will have reduced R_LargeVallocSize by the correct amount
    // already, so nothing to do (but almost never the case).
    return;
  }
  x = PROTECT(allocVector(INTSXP, 50));  // 50 so it's big enough to be on LargeVector heap. See NodeClassSize in memory.c:allocVector
                                         // INTSXP rather than VECSXP so that GC doesn't inspect contents after LENGTH (thanks to Karl Miller, Jul 2015)
  SETLENGTH(x,50+n*2*sizeof(void *)/4);  // 1*n for the names, 1*n for the VECSXP itself (both are over allocated).
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
    PROTECT(getAttrib(x, R_NamesSymbol)),  // to detect if names has been replaced and its tl lost, e.g. setattr(DT,"names",...)
    PROTECT(R_MakeExternalPtr(   // to avoid an infinite loop in object.size(), if prot=x here
      x,                         // to know if this data.table has been copied by key<-, attr<-, names<-, etc.
      R_NilValue,                // this tag and prot currently unused
      R_NilValue
    ))
  ));
  R_RegisterCFinalizerEx(p, finalizer, FALSE);
  UNPROTECT(2);

/*
  *  base::identical doesn't check prot and tag of EXTPTR, just that the ptr itself is the
     same in both objects. R_NilValue is always equal to R_NilValue.  R_NilValue is a memory
     location constant within an R session, but can vary from session to session. So, it
     looks like a pointer to a user looking at attributes(DT), but they might wonder how it
     works if they realise the selfref of all data.tables all point to the same address (rather
     than to the table itself which would be reasonable to expect given the attribute's name).
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

/* There are two reasons the finalizer doesn't restore the LENGTH to TRUELENGTH. i) The finalizer
happens too late after GC has already released the memory, and ii) copies by base R (e.g.
[<- in write.table just before test 894) allocate at length LENGTH but copy the TRUELENGTH over.
If the finalizer sets LENGTH to TRUELENGTH, that's a fail as it wasn't really allocated at
TRUELENGTH when R did the copy.
Karl Miller suggested an ENVSXP so that restoring LENGTH in finalizer should work. This is the
closest I got to getting it to pass all tests :

  SEXP env = PROTECT(allocSExp(ENVSXP));
  defineVar(SelfRefSymbol, x, env);
  defineVar(R_NamesSymbol, getAttrib(x, R_NamesSymbol), env);
  setAttrib(x, SelfRefSymbol, p = R_MakeExternalPtr(
    R_NilValue,         // for identical() to return TRUE. identical() doesn't look at tag and prot
    R_NilValue, //getAttrib(x, R_NamesSymbol), // to detect if names has been replaced and its tl lost, e.g. setattr(DT,"names",...)
    PROTECT(            // needed when --enable-strict-barrier it seems, iiuc. TO DO: test under that flag and remove if not needed.
      env               // wrap x in env to avoid an infinite loop in object.size() if prot=x were here
    )
  ));
  R_RegisterCFinalizerEx(p, finalizer, FALSE);
  UNPROTECT(2);

Then in finalizer:
  SETLENGTH(names, tl)
  SETLENGTH(dt, tl)

and that finalizer indeed now happens before the GC releases memory (thanks to the env wrapper).

However, we still have problem (ii) above and it didn't pass tests involving base R copies.

We really need R itself to start setting TRUELENGTH to be the allocated length and then
for GC to release TRUELENGTH not LENGTH.  Would really tidy this up.

Moved out of ?setkey Details section in 1.12.2 (Mar 2019). Revisit this w.r.t. to recent versions of R.
  The problem (for \code{data.table}) with the copy by \code{key<-} (other than
  being slower) is that \R doesn't maintain the over-allocated truelength, but it
  looks as though it has. Adding a column by reference using \code{:=} after a
  \code{key<-} was therefore a memory overwrite and eventually a segfault; the
  over-allocated memory wasn't really there after \code{key<-}'s copy. \code{data.table}s now have an attribute \code{.internal.selfref} to catch and warn about such copies.
  This attribute has been implemented in a way that is friendly with
  \code{identical()} and \code{object.size()}.
*/

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
    if (verbose) Rprintf(".internal.selfref ptr is NULL. This is expected and normal for a data.table loaded from disk. If not, please report to data.table issue tracker.\n");
    return -1;
  }
  if (!isNull(p)) error("Internal error: .internal.selfref ptr is not NULL or R_NilValue"); // # nocov
  tag = R_ExternalPtrTag(v);
  if (!(isNull(tag) || isString(tag))) error("Internal error: .internal.selfref tag isn't NULL or a character vector"); // # nocov
  names = getAttrib(x, R_NamesSymbol);
  if (names != tag && isString(names))
    SET_TRUELENGTH(names, LENGTH(names));
    // R copied this vector not data.table; it's not actually over-allocated. It looks over-allocated
    // because R copies the original vector's tl over despite allocating length.
  prot = R_ExternalPtrProtected(v);
  if (TYPEOF(prot) != EXTPTRSXP)   // Very rare. Was error(".internal.selfref prot is not itself an extptr").
    return 0;                      // # nocov ; see http://stackoverflow.com/questions/15342227/getting-a-random-internal-selfref-error-in-data-table-for-r
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

static SEXP shallow(SEXP dt, SEXP cols, R_len_t n)
{
  // NEW: cols argument to specify the columns to shallow copy on. If NULL, all columns.
  // called from alloccol where n is checked carefully, or from shallow() at R level
  // where n is set to truelength (i.e. a shallow copy only with no size change)
  R_len_t i,l;
  int protecti=0;
  SEXP newdt = PROTECT(allocVector(VECSXP, n)); protecti++;   // to do, use growVector here?
  //copyMostAttrib(dt, newdt);   // including class
  DUPLICATE_ATTRIB(newdt, dt);
  // TO DO: keepattr() would be faster, but can't because shallow isn't merely a shallow copy. It
  //        also increases truelength. Perhaps make that distinction, then, and split out, but marked
  //        so that the next change knows to duplicate.
  //        Does copyMostAttrib duplicate each attrib or does it point? It seems to point, hence DUPLICATE_ATTRIB
  //        for now otherwise example(merge.data.table) fails (since attr(d4,"sorted") gets written by setnames).
  SEXP names = PROTECT(getAttrib(dt, R_NamesSymbol)); protecti++;
  SEXP newnames = PROTECT(allocVector(STRSXP, n)); protecti++;
  if (isNull(cols)) {
    l = LENGTH(dt);
    for (i=0; i<l; i++) SET_VECTOR_ELT(newdt, i, VECTOR_ELT(dt,i));
    if (length(names)) {
      if (length(names) < l) error("Internal error: length(names)>0 but <length(dt)"); // # nocov
      for (i=0; i<l; i++) SET_STRING_ELT(newnames, i, STRING_ELT(names,i));
    }
    // else an unnamed data.table is valid e.g. unname(DT) done by ggplot2, and .SD may have its names cleared in dogroups, but shallow will always create names for data.table(NULL) which has 100 slots all empty so you can add to an empty data.table by reference ok.
  } else {
    l = length(cols);
    for (i=0; i<l; i++) SET_VECTOR_ELT(newdt, i, VECTOR_ELT(dt,INTEGER(cols)[i]-1));
    if (length(names)) {
      // no need to check length(names) < l here. R-level checks if all value
      // in 'cols' are valid - in the range of 1:length(names(x))
      for (i=0; i<l; i++) SET_STRING_ELT( newnames, i, STRING_ELT(names,INTEGER(cols)[i]-1) );
    }
  }
  setAttrib(newdt, R_NamesSymbol, newnames);
  // setAttrib appears to change length and truelength, so need to do that first _then_ SET next,
  // otherwise (if the SET were were first) the 100 tl is assigned to length.
  SETLENGTH(newnames,l);
  SET_TRUELENGTH(newnames,n);
  SETLENGTH(newdt,l);
  SET_TRUELENGTH(newdt,n);
  setselfref(newdt);
  UNPROTECT(protecti);
  return(newdt);
}

SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose)
{
  SEXP names, klass;   // klass not class at request of pydatatable because class is reserved word in C++, PR #3129
  R_len_t l, tl;
  if (isNull(dt)) error("alloccol has been passed a NULL dt");
  if (TYPEOF(dt) != VECSXP) error("dt passed to alloccol isn't type VECSXP");
  klass = getAttrib(dt, R_ClassSymbol);
  if (isNull(klass)) error("dt passed to alloccol has no class attribute. Please report result of traceback() to data.table issue tracker.");
  l = LENGTH(dt);
  names = getAttrib(dt,R_NamesSymbol);
  // names may be NULL when null.data.table() passes list() to alloccol for example.
  // So, careful to use length() on names, not LENGTH().
  if (length(names)!=l) error("Internal error: length of names (%d) is not length of dt (%d)",length(names),l); // # nocov
  if (!selfrefok(dt,verbose))
    return shallow(dt,R_NilValue,(n>l) ? n : l);  // e.g. test 848 and 851 in R > 3.0.2
    // added (n>l) ? ... for #970, see test 1481.
  // TO DO:  test realloc names if selfrefnamesok (users can setattr(x,"name") themselves for example.
  // if (TRUELENGTH(getAttrib(dt,R_NamesSymbol))!=tl)
  //    error("Internal error: tl of dt passes checks, but tl of names (%d) != tl of dt (%d)", tl, TRUELENGTH(getAttrib(dt,R_NamesSymbol))); // # nocov

  tl = TRUELENGTH(dt);
  // R <= 2.13.2 and we didn't catch uninitialized tl somehow
  if (tl<0) error("Internal error, tl of class is marked but tl<0."); // # nocov
  if (tl>0 && tl<l) error("Internal error, please report (including result of sessionInfo()) to data.table issue tracker: tl (%d) < l (%d) but tl of class is marked.", tl, l); // # nocov
  if (tl>l+10000) warning("tl (%d) is greater than 10,000 items over-allocated (l = %d). If you didn't set the datatable.alloccol option to be very large, please report to data.table issue tracker including the result of sessionInfo().",tl,l);
  if (n>tl) return(shallow(dt,R_NilValue,n)); // usual case (increasing alloc)
  if (n<tl && verbose) Rprintf("Attempt to reduce allocation from %d to %d ignored. Can only increase allocation via shallow copy. Please do not use DT[...]<- or DT$someCol<-. Use := inside DT[...] instead.",tl,n);
        // otherwise the finalizer can't clear up the Large Vector heap
  return(dt);
}

int checkOverAlloc(SEXP x)
{
  if (isNull(x))
    error("Has getOption('datatable.alloccol') somehow become unset? It should be a number, by default 1024.");
  if (!isInteger(x) && !isReal(x))
    error("getOption('datatable.alloccol') should be a number, by default 1024. But its type is '%s'.", type2char(TYPEOF(x)));
  if (LENGTH(x) != 1)
    error("getOption('datatable.alloc') is a numeric vector ok but its length is %d. Its length should be 1.", LENGTH(x));
  int ans = isInteger(x) ? INTEGER(x)[0] : (int)REAL(x)[0];
  if (ans<0)
    error("getOption('datatable.alloc')==%d.  It must be >=0 and not NA.", ans);
  return ans;
}

SEXP alloccolwrapper(SEXP dt, SEXP overAllocArg, SEXP verbose) {
  if (!isLogical(verbose) || length(verbose)!=1) error("verbose must be TRUE or FALSE");
  int overAlloc = checkOverAlloc(overAllocArg);
  SEXP ans = PROTECT(alloccol(dt, length(dt)+overAlloc, LOGICAL(verbose)[0]));

  for(R_len_t i = 0; i < LENGTH(ans); i++) {
    // clear the same excluded by copyMostAttrib(). Primarily for data.table and as.data.table, but added here centrally (see #4890).

    setAttrib(VECTOR_ELT(ans, i), R_NamesSymbol, R_NilValue);
    setAttrib(VECTOR_ELT(ans, i), R_DimSymbol, R_NilValue);
    setAttrib(VECTOR_ELT(ans, i), R_DimNamesSymbol, R_NilValue);
  }

  UNPROTECT(1);
  return ans;
}

SEXP shallowwrapper(SEXP dt, SEXP cols) {
  // selfref will be FALSE on manually created data.table, e.g., via dput() or structure()
  if (!selfrefok(dt, FALSE)) {
    int n = isNull(cols) ? length(dt) : length(cols);
    return(shallow(dt, cols, n));
  } else return(shallow(dt, cols, TRUELENGTH(dt)));
}

SEXP truelength(SEXP x) {
  return ScalarInteger(isNull(x) ? 0 : TRUELENGTH(x));
}

SEXP selfrefokwrapper(SEXP x, SEXP verbose) {
  return ScalarInteger(_selfrefok(x,FALSE,LOGICAL(verbose)[0]));
}

int *_Last_updated = NULL;

SEXP assign(SEXP dt, SEXP rows, SEXP cols, SEXP newcolnames, SEXP values, SEXP verb)
{
  // For internal use only by := in [.data.table, and set()
  // newcolnames : add these columns (if any)
  // cols : column names or numbers corresponding to the values to set
  // rows : row numbers to assign
  R_len_t i, j, numToDo, targetlen, vlen, r, oldncol, oldtncol, coln, protecti=0, newcolnum, indexLength;
  SEXP targetcol, RHS, names, nullint, thisv, targetlevels, newcol, s, colnam, klass, tmp, colorder, key, index, a, assignedNames, indexNames;
  SEXP bindingIsLocked = getAttrib(dt, install(".data.table.locked"));
  Rboolean verbose = LOGICAL(verb)[0], anytodelete=FALSE, isDataTable=FALSE;
  const char *c1, *tc1, *tc2;
  int *buf, k=0, newKeyLength, indexNo;
  size_t size; // must be size_t otherwise overflow later in memcpy
  if (isNull(dt)) error("assign has been passed a NULL dt");
  if (TYPEOF(dt) != VECSXP) error("dt passed to assign isn't type VECSXP");
  if (length(bindingIsLocked) && LOGICAL(bindingIsLocked)[0])
    error(".SD is locked. Updating .SD by reference using := or set are reserved for future use. Use := in j directly. Or use copy(.SD) as a (slow) last resort, until shallow() is exported.");

  klass = getAttrib(dt, R_ClassSymbol);
  if (isNull(klass)) error("Input passed to assign has no class attribute. Must be a data.table or data.frame.");
  // Check if there is a class "data.table" somewhere (#5115).
  // We allow set() on data.frame too; e.g. package Causata uses set() on a data.frame in tests/testTransformationReplay.R
  // := is only allowed on a data.table. However, the ":=" = stop(...) message in data.table.R will have already
  // detected use on a data.frame before getting to this point.
  for (i=0; i<length(klass); i++) {   // There doesn't seem to be an R API interface to inherits(), but manually here isn't too bad.
    if (strcmp(CHAR(STRING_ELT(klass, i)), "data.table") == 0) break;
  }
  if (i<length(klass))
    isDataTable = TRUE;
  else {
    for (i=0; i<length(klass); i++) {
      if (strcmp(CHAR(STRING_ELT(klass, i)), "data.frame") == 0) break;
    }
    if (i == length(klass)) error("Input is not a data.table, data.frame or an object that inherits from either.");
    isDataTable = FALSE;   // meaning data.frame from now on. Can use set() on existing columns but not add new ones because DF aren't over-allocated.
  }
  oldncol = LENGTH(dt);
  names = getAttrib(dt,R_NamesSymbol);
  if (isNull(names)) error("dt passed to assign has no names");
  if (length(names)!=oldncol)
    error("Internal error in assign: length of names (%d) is not length of dt (%d)",length(names),oldncol); // # nocov
  if (isNull(dt)) {
    error("data.table is NULL; malformed. A null data.table should be an empty list. typeof() should always return 'list' for data.table."); // # nocov
    // Not possible to test because R won't permit attributes be attached to NULL (which is good and we like); warning from R 3.4.0+ tested by 944.5
  }
  const int nrow = LENGTH(dt) ? length(VECTOR_ELT(dt,0)) :
                                (isNewList(values) && length(values) ? length(VECTOR_ELT(values,0)) : length(values));
  //                            ^ when null data.table the new nrow becomes the fist column added
  if (isNull(rows)) {
    numToDo = nrow;
    targetlen = nrow;
    if (verbose) Rprintf("Assigning to all %d rows\n", nrow);
    // fast way to assign to whole column, without creating 1:nrow(x) vector up in R, or here in C
  } else {
    if (isReal(rows)) {
      rows = PROTECT(rows = coerceVector(rows, INTSXP));
      protecti++;
      warning("Coerced i from numeric to integer. Please pass integer for efficiency; e.g., 2L rather than 2");
    }
    if (!isInteger(rows))
      error("i is type '%s'. Must be integer, or numeric is coerced with warning. If i is a logical subset, simply wrap with which(), and take the which() outside the loop if possible for efficiency.", type2char(TYPEOF(rows)));
    targetlen = length(rows);
    numToDo = 0;
    const int *rowsd = INTEGER(rows);
    for (i=0; i<targetlen; i++) {
      if ((rowsd[i]<0 && rowsd[i]!=NA_INTEGER) || rowsd[i]>nrow)
        error("i[%d] is %d which is out of range [1,nrow=%d].",i+1,rowsd[i],nrow);  // set() reaches here (test 2005.2); := reaches the same error in subset.c first
      if (rowsd[i]>=1) numToDo++;
    }
    if (verbose) Rprintf("Assigning to %d row subset of %d rows\n", numToDo, nrow);
    // TODO: include in message if any rows are assigned several times (e.g. by=.EACHI with dups in i)
    if (numToDo==0) {
      if (!length(newcolnames)) {
        *_Last_updated = 0;
        return(dt); // all items of rows either 0 or NA. !length(newcolnames) for #759
      }
      if (verbose) Rprintf("Added %d new column%s initialized with all-NA\n",
                           length(newcolnames), (length(newcolnames)>1)?"s":"");
    }
  }
  if (!length(cols)) {
    warning("length(LHS)==0; no columns to delete or assign RHS to.");   // test 1295 covers
    *_Last_updated = 0;
    return(dt);
  }
  // FR #2077 - set able to add new cols by reference
  if (isString(cols)) {
    PROTECT(tmp = chmatch(cols, names, 0));
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
      cols = PROTECT(coerceVector(cols, INTSXP));
      protecti++;
      warning("Coerced j from numeric to integer. Please pass integer for efficiency; e.g., 2L rather than 2");
    }
    if (!isInteger(cols))
      error("j is type '%s'. Must be integer, character, or numeric is coerced with warning.", type2char(TYPEOF(cols)));
  }
  if (any_duplicated(cols,FALSE)) error("Can't assign to the same column twice in the same query (duplicates detected).");
  if (!isNull(newcolnames) && !isString(newcolnames)) error("newcolnames is supplied but isn't a character vector");
  bool RHS_list_of_columns = TYPEOF(values)==VECSXP && (length(cols)>1 || LENGTH(values)==1);
  if (verbose) Rprintf("RHS_list_of_columns == %s\n", RHS_list_of_columns ? "true" : "false");
  if (RHS_list_of_columns) {
    if (length(values)==0)
      error("Supplied %d columns to be assigned an empty list (which may be an empty data.table or data.frame since they are lists too). "
            "To delete multiple columns use NULL instead. To add multiple empty list columns, use list(list()).", length(cols));
    if (length(values)>1 && length(values)!=length(cols))
      error("Supplied %d columns to be assigned %d items. Please see NEWS for v1.12.2. Try adding/removing a list() or .() wrapper around the RHS.", length(cols), length(values));
    if (length(values)==1) {
      // c("colA","colB"):=list(13:15) should use 13:15 for both columns (recycle 1 item ok). So just change RHS so we don't have to deal with recycling-length-1 later
      values = VECTOR_ELT(values,0);
      RHS_list_of_columns = false;
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
    SEXP thisvalue = RHS_list_of_columns ? VECTOR_ELT(values, i) : values;
    vlen = length(thisvalue);
    if (isNull(thisvalue) && !isNull(rows)) error("When deleting columns, i should not be provided");  // #1082, #3089
    if (coln+1 <= oldncol) colnam = STRING_ELT(names,coln);
    else colnam = STRING_ELT(newcolnames,coln-length(names));
    if (coln+1 <= oldncol && isNull(thisvalue)) continue;  // delete existing column(s) afterwards, near end of this function
    //if (vlen<1 && nrow>0) {
    if (coln+1 <= oldncol && nrow>0 && vlen<1 && numToDo>0) { // numToDo > 0 fixes #2829, see test 1911
      error("RHS of assignment to existing column '%s' is zero length but not NULL. If you intend to delete the column use NULL. Otherwise, the RHS must have length > 0; e.g., NA_integer_. If you are trying to change the column type to be an empty list column then, as with all column type changes, provide a full length RHS vector such as vector('list',nrow(DT)); i.e., 'plonk' in the new column.", CHAR(STRING_ELT(names,coln)));
    }
    if (coln+1 > oldncol && TYPEOF(thisvalue)!=VECSXP) {  // list() is ok for new columns
      newcolnum = coln-length(names);
      if (newcolnum<0 || newcolnum>=length(newcolnames))
        error("Internal error in assign.c: length(newcolnames)=%d, length(names)=%d, coln=%d", length(newcolnames), length(names), coln); // # nocov
      if (isNull(thisvalue)) {
        warning("Column '%s' does not exist to remove",CHAR(STRING_ELT(newcolnames,newcolnum)));
        continue;
      }
      // RHS of assignment to new column is zero length but we'll use its type to create all-NA column of that type
    }
    if (isMatrix(thisvalue) && (j=INTEGER(getAttrib(thisvalue, R_DimSymbol))[1]) > 1)  // matrix passes above (considered atomic vector)
      warning("%d column matrix RHS of := will be treated as one vector", j);
    const SEXP existing = (coln+1)<=oldncol ? VECTOR_ELT(dt,coln) : R_NilValue;
    if (isFactor(existing) &&
      !isString(thisvalue) && TYPEOF(thisvalue)!=INTSXP && TYPEOF(thisvalue)!=LGLSXP && !isReal(thisvalue) && !isNewList(thisvalue)) {  // !=INTSXP includes factor
      error("Can't assign to column '%s' (type 'factor') a value of type '%s' (not character, factor, integer or numeric)",
            CHAR(STRING_ELT(names,coln)),type2char(TYPEOF(thisvalue)));
    }
    if (nrow>0 && targetlen>0 && vlen>1 && vlen!=targetlen && TYPEOF(existing)!=VECSXP) {
      // note that isNewList(R_NilValue) is true so it needs to be TYPEOF(existing)!=VECSXP above
      error("Supplied %d items to be assigned to %d items of column '%s'. If you wish to 'recycle' the RHS please "
            "use rep() to make this intent clear to readers of your code.", vlen, targetlen, CHAR(colnam));
    }
  }
  // having now checked the inputs, from this point there should be no errors so we can now proceed to
  // modify DT by reference. Other than if new columns are being added and the allocVec() fails with
  // out-of-memory. In that case the user will receive hard halt and know to rerun.
  if (length(newcolnames)) {
    oldtncol = TRUELENGTH(dt);   // TO DO: oldtncol can be just called tl now, as we won't realloc here any more.

    if (oldtncol<oldncol) {
      if (oldtncol==0) error("This data.table has either been loaded from disk (e.g. using readRDS()/load()) or constructed manually (e.g. using structure()). Please run setDT() or alloc.col() on it first (to pre-allocate space for new columns) before assigning by reference to it.");   // #2996
      error("Internal error: oldtncol(%d) < oldncol(%d). Please report to data.table issue tracker, including result of sessionInfo().", oldtncol, oldncol); // # nocov
    }
    if (oldtncol>oldncol+10000L) warning("truelength (%d) is greater than 10,000 items over-allocated (length = %d). See ?truelength. If you didn't set the datatable.alloccol option very large, please report to data.table issue tracker including the result of sessionInfo().",oldtncol, oldncol);
    if (oldtncol < oldncol+LENGTH(newcolnames))
      error("Internal error: DT passed to assign has not been allocated enough column slots. l=%d, tl=%d, adding %d", oldncol, oldtncol, LENGTH(newcolnames));  // # nocov
    if (!selfrefnamesok(dt,verbose))
      error("It appears that at some earlier point, names of this data.table have been reassigned. Please ensure to use setnames() rather than names<- or colnames<-. Otherwise, please report to data.table issue tracker.");  // # nocov
      // Can growVector at this point easily enough, but it shouldn't happen in first place so leave it as
      // strong error message for now.
    else if (TRUELENGTH(names) != oldtncol)
      error("Internal error: selfrefnames is ok but tl names [%d] != tl [%d]", TRUELENGTH(names), oldtncol);  // # nocov
    SETLENGTH(dt, oldncol+LENGTH(newcolnames));
    SETLENGTH(names, oldncol+LENGTH(newcolnames));
    for (i=0; i<LENGTH(newcolnames); i++)
      SET_STRING_ELT(names,oldncol+i,STRING_ELT(newcolnames,i));
    // truelengths of both already set by alloccol
  }
  for (i=0; i<length(cols); i++) {
    coln = INTEGER(cols)[i]-1;
    SEXP thisvalue = RHS_list_of_columns ? VECTOR_ELT(values, i) : values;
    if (TYPEOF(thisvalue)==NILSXP) {
      if (!isNull(rows)) error("Internal error: earlier error 'When deleting columns, i should not be provided' did not happen."); // # nocov
      anytodelete = TRUE;
      continue;   // delete column(s) afterwards, below this loop
    }
    vlen = length(thisvalue);
    if (length(rows)==0 && targetlen==vlen && (vlen>0 || nrow==0)) {
      if (  MAYBE_SHARED(thisvalue) ||  // set() protects the NAMED of atomic vectors from .Call setting arguments to 2 by wrapping with list
         (TYPEOF(values)==VECSXP && i>LENGTH(values)-1) || // recycled RHS would have columns pointing to others, #185.
         (TYPEOF(values)!=VECSXP && i>0) // assigning the same values to a second column. Have to ensure a copy #2540
         ) {
        if (verbose) {
          Rprintf("RHS for item %d has been duplicated because NAMED is %d, but then is being plonked. length(values)==%d; length(cols)==%d)\n",
                  i+1, NAMED(thisvalue), length(values), length(cols));
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
      newcol = allocNAVector(TYPEOF(thisvalue),nrow);
      // initialize with NAs for when 'rows' is a subset and it doesn't touch
      // do not try to save the time to NA fill (contiguous branch free assign anyway) since being
      // sure all items will be written to (isNull(rows), length(rows), vlen<1, targetlen) is not worth the risk.
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
              SET_STRING_ELT(addlevels,addi++,thisv);
              // if-else for #1718 fix
              SET_TRUELENGTH(thisv, (thisv != NA_STRING) ? (addi+length(targetlevels)) : NA_INTEGER);
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
          if (TYPEOF(thisvalue)!=INTSXP && TYPEOF(thisvalue)!=LGLSXP && !isReal(thisvalue))
            error("Internal error: up front checks (before starting to modify DT) didn't catch type of RHS ('%s') assigning to factor column '%s'. please report to data.table issue tracker.", type2char(TYPEOF(thisvalue)), CHAR(STRING_ELT(names,coln))); // # nocov
          if (isReal(thisvalue) || TYPEOF(thisvalue)==LGLSXP) {
            PROTECT(RHS = coerceVector(thisvalue,INTSXP));
            protecti++;
            // silence warning on singleton NAs
            if (INTEGER(RHS)[0] != NA_INTEGER) warning("Coerced '%s' RHS to 'integer' to match the factor column's underlying type. Character columns are now recommended (can be in keys), or coerce RHS to integer or character first.", type2char(TYPEOF(thisvalue)));
          } else {
            // make sure to copy thisvalue. May be modified below. See #2984
            RHS = PROTECT(duplicate(thisvalue));
            protecti++;
          }
          for (j=0; j<length(RHS); j++) {
            if ( (INTEGER(RHS)[j]<1 || INTEGER(RHS)[j]>LENGTH(targetlevels))
                 && INTEGER(RHS)[j] != NA_INTEGER) {
              warning("RHS contains %d which is outside the levels range ([1,%d]) of column %d, NAs generated", INTEGER(RHS)[j], LENGTH(targetlevels), i+1);
              INTEGER(RHS)[j] = NA_INTEGER;
            }
          }
        }
      } else {
        if (TYPEOF(targetcol)==TYPEOF(thisvalue) || TYPEOF(targetcol)==VECSXP)
          RHS = thisvalue;
        else {
          // coerce the RHS to match the type of the column, unlike [<-.data.frame, for efficiency.
          if (isString(targetcol) && isFactor(thisvalue)) {
            PROTECT(RHS = asCharacterFactor(thisvalue));
            protecti++;
            if (verbose) Rprintf("Coerced factor RHS to character to match the column's type. Avoid this coercion if possible, for efficiency, by creating RHS as type character.\n");
            // TO DO: datatable.pedantic could turn this into warning
          } else {
            PROTECT(RHS = coerceVector(thisvalue,TYPEOF(targetcol)));
            protecti++;
            char *s1 = (char *)type2char(TYPEOF(targetcol));
            char *s2 = (char *)type2char(TYPEOF(thisvalue));
            // FR #2551, added test for equality between RHS and thisvalue to not provide the warning when length(thisvalue) == 1
            if ( length(thisvalue)==1 && TYPEOF(RHS)!=VECSXP && (
                 ( isReal(thisvalue) && isInteger(targetcol) && REAL(thisvalue)[0]==INTEGER(RHS)[0] ) ||   // DT[,intCol:=4] rather than DT[,intCol:=4L]
                 ( isLogical(thisvalue) && LOGICAL(thisvalue)[0] == NA_LOGICAL ) ||                        // DT[,intCol:=NA]
                 ( isInteger(thisvalue) && isReal(targetcol) ) )) {
              if (verbose) Rprintf("Coerced length-1 RHS from %s to %s to match column's type.%s If this assign is happening a lot inside a loop, in particular via set(), then it may be worth avoiding this coercion by using R's type postfix on the value being assigned; e.g. typeof(0) vs typeof(0L), and typeof(NA) vs typeof(NA_integer_) vs typeof(NA_real_).\n", s2, s1,
                                    isInteger(targetcol) && isReal(thisvalue) ? " No precision was lost." : "");
              // TO DO: datatable.pedantic could turn this into warning. Or we could catch and avoid the coerceVector allocation ourselves using a single int.
            } else {
              if (isReal(thisvalue) && isInteger(targetcol)) {
                int w = INTEGER(isReallyReal(thisvalue))[0];  // first fraction present (1-based), 0 if none
                if (w>0) {
                  warning("Coerced double RHS to integer to match the type of the target column (column %d named '%s'). One or more RHS values contain fractions which have been lost; e.g. item %d with value %f has been truncated to %d.",
                          coln+1, CHAR(STRING_ELT(names, coln)), w, REAL(thisvalue)[w-1], INTEGER(RHS)[w-1]);
                } else {
                  warning("Coerced double RHS to integer to match the type of the target column (column %d named '%s'). The RHS values contain no fractions so would be more efficiently created as integer. Consider using R's 'L' postfix (typeof(0L) vs typeof(0)) to create constants as integer and avoid this warning. Wrapping the RHS with as.integer() will avoid this warning too but it's better if possible to create the RHS as integer in the first place so that the cost of the coercion can be avoided.", coln+1, CHAR(STRING_ELT(names, coln)));
                }
              } else {
                warning("Coerced %s RHS to %s to match the type of the target column (column %d named '%s'). If the target column's type %s is correct, it's best for efficiency to avoid the coercion and create the RHS as type %s. To achieve that consider R's type postfix: typeof(0L) vs typeof(0), and typeof(NA) vs typeof(NA_integer_) vs typeof(NA_real_). You can wrap the RHS with as.%s() to avoid this warning, but that will still perform the coercion. If the target column's type is not correct, it's best to revisit where the DT was created and fix the column type there; e.g., by using colClasses= in fread(). Otherwise, you can change the column type now by plonking a new column (of the desired type) over the top of it; e.g. DT[, `%s`:=as.%s(`%s`)]. If the RHS of := has nrow(DT) elements then the assignment is called a column plonk and is the way to change a column's type. Column types can be observed with sapply(DT,typeof).",
                s2, s1, coln+1, CHAR(STRING_ELT(names, coln)), s1, s1, s1, CHAR(STRING_ELT(names, coln)), s2, CHAR(STRING_ELT(names, coln)));
              }
            }
          }
        }
      }
    }
    memrecycle(targetcol, rows, 0, targetlen, RHS);  // also called from dogroups where these arguments are used more
  }
  *_Last_updated = numToDo;  // the updates have taken place with no error, so update .Last.updated now
  PROTECT(assignedNames = allocVector(STRSXP, LENGTH(cols)));
  protecti++;
  for (i=0;i<LENGTH(cols);i++) SET_STRING_ELT(assignedNames,i,STRING_ELT(names,INTEGER(cols)[i]-1));
  key = getAttrib(dt, sym_sorted);
  if (length(key)) {
    // if assigning to at least one key column, the key is truncated to one position before the first changed column.
    //any() and subsetVector() don't seem to be exposed by R API at C level, so this is done here long hand.
    PROTECT(tmp = chin(key, assignedNames));
    protecti++;
    newKeyLength = xlength(key);
    for (i=0;i<LENGTH(tmp);i++) if (LOGICAL(tmp)[i]) {
      // If a key column is being assigned to, set newKeyLength to the key element before since everything after that may have changed in order.
      newKeyLength = i;
      break;
    }
    if(newKeyLength == 0){
      // no valid key columns remain, remove the key
      setAttrib(dt, sym_sorted, R_NilValue);
    } else if (newKeyLength < xlength(key)){
      // new key is shorter than original one. Reassign
      PROTECT(tmp = allocVector(STRSXP, newKeyLength));
      protecti++;
      memcpy((char *)DATAPTR(tmp), (char *)DATAPTR(key), newKeyLength * sizeof(SEXP));
      setAttrib(dt, sym_sorted, tmp);
    }
    //else: no key column changed, nothing to be done
  }
  index = getAttrib(dt, install("index"));
  if (index != R_NilValue) {
    s = ATTRIB(index);
    indexNo = 0;
    // get a vector with all index names
    PROTECT(indexNames = allocVector(STRSXP, xlength(s)));
    protecti++;
    while(s != R_NilValue){
      SET_STRING_ELT(indexNames, indexNo, PRINTNAME(TAG(s)));
      indexNo++;
      s = CDR(s);
    }
    s = ATTRIB(index); // reset to first element
    indexNo = 0;
    while(s != R_NilValue) {
      a = TAG(s);
      indexLength = xlength(CAR(s));
      tc1 = c1 = CHAR(PRINTNAME(a));  // the index name; e.g. "__col1__col2"
      if (*tc1!='_' || *(tc1+1)!='_') {
        // fix for #1396
        if (verbose) {
          Rprintf("Dropping index '%s' as it doesn't have '__' at the beginning of its name. It was very likely created by v1.9.4 of data.table.\n", tc1);
        }
        setAttrib(index, a, R_NilValue);
        indexNo++;
        s = CDR(s);
        continue; // with next index
      }
      tc1 += 2; // tc1 always marks the start of a key column
      if (!*tc1) error("Internal error: index name ends with trailing __"); // # nocov
      // check the position of the first appearance of an assigned column in the index.
      // the new index will be truncated to this position.
      char *s4 = (char*) malloc(strlen(c1) + 3);
      if(s4 == NULL){
        error("Internal error: Couldn't allocate memory for s4."); // # nocov
      }
      memcpy(s4, c1, strlen(c1));
      memset(s4 + strlen(c1), '\0', 1);
      strcat(s4, "__"); // add trailing '__' to newKey so we can search for pattern '__colName__' also at the end of the index.
      newKeyLength = strlen(c1);
      for(int i = 0; i < xlength(assignedNames); i++){
        tc2 = CHAR(STRING_ELT(assignedNames, i));
        char *s5 = (char*) malloc(strlen(tc2) + 5); //4 * '_' + \0
        if(s5 == NULL){
          free(s4);                                                  // # nocov
          error("Internal error: Couldn't allocate memory for s5."); // # nocov
        }
        memset(s5, '_', 2);
        memset(s5 + 2, '\0', 1);
        strcat(s5, tc2);
        strcat(s5, "__");
        tc2 = strstr(s4, s5);
        if(tc2 == NULL){ // column is not part of key
          free(s5);
          continue;
        }
        if(tc2 - s4 < newKeyLength){ // new column match is before last match
          newKeyLength = tc2 - s4;
        }
        free(s5);
      }
      memset(s4 + newKeyLength, '\0', 1); // truncate the new key to the new length
      if(newKeyLength == 0){ // no valid key column remains. Drop the key
        setAttrib(index, a, R_NilValue);
        SET_STRING_ELT(indexNames, indexNo, NA_STRING);
        if (verbose) {
          Rprintf("Dropping index '%s' due to an update on a key column\n", c1+2);
        }
      } else if(newKeyLength < strlen(c1)){
        if(indexLength == 0 && // shortened index can be kept since it is just information on the order (see #2372)
           LOGICAL(chin(mkString(s4), indexNames))[0] == 0) {// index with shortened name not present yet
          SET_TAG(s, install(s4));
          SET_STRING_ELT(indexNames, indexNo, mkChar(s4));
          if (verbose) {
            Rprintf("Shortening index '%s' to '%s' due to an update on a key column\n", c1+2, s4 + 2);
          }
        } else{ // indexLength > 0 || shortened name present already
          // indexLength > 0 indicates reordering. Drop it to avoid spurious reordering in non-indexed columns (#2372)
          // shortened anme already present indicates that index needs to be dropped to avoid duplicate indices.
          setAttrib(index, a, R_NilValue);
          SET_STRING_ELT(indexNames, indexNo, NA_STRING);
          if (verbose) {
            Rprintf("Dropping index '%s' due to an update on a key column\n", c1+2);
          }
        }
      } //else: index is not affected by assign: nothing to be done
      free(s4);
      indexNo ++;
      s = CDR(s);
    }
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
      SEXP thisvalue = RHS_list_of_columns ? VECTOR_ELT(values, i) : values;
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
  UNPROTECT(protecti);
  return(dt);  // needed for `*tmp*` mechanism (when := isn't used), and to return the new object after a := for compound syntax.
}

static bool anyNamed(SEXP x) {
  if (MAYBE_REFERENCED(x)) return true;
  if (isNewList(x)) for (int i=0; i<LENGTH(x); i++)
    if (anyNamed(VECTOR_ELT(x,i))) return true;
  return false;
}

static char memrecycle_message[1000];

const char *memrecycle(SEXP target, SEXP where, int start, int len, SEXP source)
// like memcpy but recycles single-item source
// 'where' a 1-based INTEGER vector subset of target to assign to, or NULL or integer()
// assigns to target[start:start+len-1] or target[where[start:start+len-1]] where start is 0-based
{
  if (len<1) return NULL;
  int slen = length(source);
  if (slen==0) return NULL;
  if (slen>1 && slen!=len && (!isNewList(target) || isNewList(source)))
    error("Internal error: recycle length error not caught earlier. slen=%d len=%d", slen, len); // # nocov
  // Internal error because the column has already been added to the DT, so length mismatch should have been caught before adding the column.
  // for 5647 this used to limit slen to len, but no longer
  *memrecycle_message = '\0';
  int protecti=0;
  if (isNewList(source)) {
    // A list() column; i.e. target is a column of pointers to SEXPs rather than the much more common case
    // where memrecycle copies the DATAPTR data to the atomic target from the atomic source.
    // If any item within the list is NAMED then take a fresh copy. So far this has occurred from dogroups.c when
    // j returns .BY or similar specials as-is within a list(). Those specials are static inside
    // dogroups so if we don't copy now the last value written to them by dogroups becomes repeated in the result;
    // i.e. the wrong result.
    // If source is itself recycled later (many list() column items pointing to the same object) we are ok with that
    // since we now have a fresh copy and := will not assign with a list() column's cell value; := only changes the
    // SEXP pointed to.
    // If source is already not named (because j already created a fresh unnamed vector within a list()) we don't want to
    // duplicate unnecessarily, hence checking for named rather than duplicating always.
    // See #481, #1270 and tests 1341.* fail without this duplicate().
    if (anyNamed(source)) {
      source = PROTECT(duplicate(source));
      protecti++;
    }
  }
  if (!length(where)) {  // e.g. called from rbindlist with where=R_NilValue
    switch (TYPEOF(target)) {
    case RAWSXP:
      if (TYPEOF(source)!=RAWSXP) { source = PROTECT(coerceVector(source, RAWSXP)); protecti++; }
      if (slen==1) {
        // recycle single items
        Rbyte *td = RAW(target)+start;
        const Rbyte val = RAW(source)[0];
        for (int i=0; i<len; i++) td[i] = val;  // no R API inside loop as RAW()/INTEGER() etc have overhead even when inline functions
      } else {
        memcpy(RAW(target)+start, RAW(source), slen*SIZEOF(target));
      }
      break;
    case LGLSXP: case INTSXP :
      if (TYPEOF(source)!=LGLSXP && TYPEOF(source)!=INTSXP) { source = PROTECT(coerceVector(source, TYPEOF(target))); protecti++; }
      if (slen==1) {
        int *td = INTEGER(target)+start;
        const int val = INTEGER(source)[0];
        for (int i=0; i<len; i++) td[i] = val;
      } else {
        memcpy(INTEGER(target)+start, INTEGER(source), slen*SIZEOF(target));
      }
      break;
    case REALSXP : {
      bool si64 = INHERITS(source, char_integer64);
      bool ti64 = INHERITS(target, char_integer64);
      if (si64 && TYPEOF(source)!=REALSXP)
        error("Internal error: source has integer64 attribute but is type '%s' not REALSXP", type2char(TYPEOF(source))); // # nocov
      if (si64 == ti64) {
        if (TYPEOF(source)!=REALSXP) { source = PROTECT(coerceVector(source, REALSXP)); protecti++; }
        if (slen==1) {
          double *td = REAL(target)+start;
          const double val = REAL(source)[0];
          for (int i=0; i<len; i++) td[i] = val;
        } else {
          memcpy(REAL(target)+start, REAL(source), slen*SIZEOF(target));
        }
      } else if (si64) {
        error("Internal error: memrecycle source is integer64 but target is real and not integer64; target should be type integer64");  // # nocov
        /*
        double *td = REAL(target)+start;
        if (slen==1) {
          const double val = (double)(((int64_t *)REAL(source))[0]);
          for (int i=0; i<len; i++) td[i] = val;
        } else {
          const int64_t *val = (int64_t *)REAL(source);
          for (int i=0; i<len; i++) td[i] = (double)(val[i]);
        }*/
      } else {
        int64_t *td = (int64_t *)REAL(target)+start;
        const int mask = slen==1 ? 0 : INT_MAX;
        switch (TYPEOF(source)) {
        case RAWSXP: {
          const Rbyte *sd = RAW(source);  // sd = source data
          for (int i=0; i<len; ++i) td[i] = (int64_t)(sd[i&mask]);  // raw has no NA
        } break;
        case LGLSXP : case INTSXP : {
          const int *sd = INTEGER(source);
          for (int i=0; i<len; ++i) td[i] = sd[i]==NA_INTEGER ? INT64_MIN : (int64_t)(sd[i]);
        } break;
        case REALSXP : {
          int firstReal=0;
          if ((firstReal=INTEGER(isReallyReal(source))[0])) {
            sprintf(memrecycle_message, "coerced to integer64 but contains a non-integer value (%f at position %d); precision lost.", REAL(source)[firstReal-1], firstReal);
          }
          double *sd = REAL(source);
          for (int i=0; i<len; ++i) td[i] = R_FINITE(sd[i]) ? (int)(sd[i]) : NA_INTEGER;
        } break;
        default :
          error("Internal error: memrecycle integer64 column source is type '%s'", type2char(TYPEOF(source)));  // # nocov
        }
      }
    } break;
    case CPLXSXP :
      if (TYPEOF(source)!=CPLXSXP) { source = PROTECT(coerceVector(source, CPLXSXP)); protecti++; }
      if (slen==1) {
        Rcomplex *td = COMPLEX(target)+start;
        const Rcomplex val = COMPLEX(source)[0];
        for (int i=0; i<len; ++i) td[i] = val;
      } else {
        memcpy(COMPLEX(target)+start, COMPLEX(source), slen*SIZEOF(target));
      }
      break;
    case STRSXP :
      if (TYPEOF(source)!=STRSXP) { source = PROTECT(coerceVector(source, STRSXP)); protecti++; }
      if (slen==1) {
        const SEXP val = STRING_ELT(source, 0);
        for (int i=0; i<len; i++) SET_STRING_ELT(target, start+i, val);
      } else {
        const SEXP *val = STRING_PTR(source);
        for (int i=0; i<len; i++) SET_STRING_ELT(target, start+i, val[i]);
      }
      break;
    case VECSXP :
      if (TYPEOF(source)==VECSXP && len==slen) {
        for (int i=0; i<len; i++) SET_VECTOR_ELT(target, start+i, VECTOR_ELT(source, i));
      } else {
        const SEXP val = TYPEOF(source)==VECSXP ? VECTOR_ELT(source, 0) : source;
        for (int i=0; i<len; i++) SET_VECTOR_ELT(target, start+i, val);
      }
      break;
    default :
      error("Unsupported type in assign.c:memrecycle '%s' (no where)", type2char(TYPEOF(target)));  // # nocov
    }
  } else {
    if (TYPEOF(target)!=TYPEOF(source) && TYPEOF(target)!=VECSXP)
      error("Internal error: TYPEOF(target)['%s']!=TYPEOF(source)['%s'] in memrecycle (where)", type2char(TYPEOF(target)),type2char(TYPEOF(source))); // # nocov
    const int *wd = INTEGER(where)+start;
    const int mask = slen==1 ? 0 : INT_MAX;
    switch (TYPEOF(target)) {
    case LGLSXP: case INTSXP : {
      int *td = INTEGER(target);
      const int *sd = INTEGER(source);
      for (int i=0; i<len; i++) {
        const int w = wd[i];
        if (w<1) continue;  // 0 or NA
        td[w-1] = sd[i&mask];  // i&mask is for branchless recyle when slen==1
      }
    } break;
    case REALSXP : {
      double *td = REAL(target);
      const double *sd = REAL(source);
      for (int i=0; i<len; i++) {
        const int w = wd[i];
        if (w<1) continue;
        td[w-1] = sd[i&mask];
      }
    } break;
    case STRSXP : {
      const SEXP *sd = STRING_PTR(source);
      for (int i=0; i<len; i++) {
        const int w = wd[i];
        if (w<1) continue;
        SET_STRING_ELT(target, w-1, sd[i&mask]);
      }
    } break;
    case VECSXP : {
      if (TYPEOF(source)==VECSXP) {
        const SEXP *sd = VECTOR_PTR(source);
        for (int i=0; i<len; i++) {
          const int w = wd[i];
          if (w<1) continue;
          SET_VECTOR_ELT(target, w-1, sd[i&mask]);
        }
      } else {
        for (int i=0; i<len; i++) {
          const int w = wd[i];
          if (w<1) continue;
          SET_VECTOR_ELT(target, w-1, source);
        }
      }
    } break;
    default :
      error("Unsupported type in assign.c:memrecycle '%s' (where)", type2char(TYPEOF(target)));  // # nocov
    }
  }
  UNPROTECT(protecti);
  return memrecycle_message[0] ? memrecycle_message : NULL;
}

void writeNA(SEXP v, const int from, const int n)
// this is for use after allocVector() which does not initialize its result. It does write NA as you'd
// think, other than for VECSXP which allocVector() already initializes with NULL.
{
  const int to = from-1+n;  // together with <=to below with writing NA to position 2147483647 in mind
  switch(TYPEOF(v)) {
  case RAWSXP:
    memset(RAW(v)+from, 0, n*SIZEOF(v));
    break;
  case LGLSXP : {
    Rboolean *vd = (Rboolean *)LOGICAL(v);
    for (int i=from; i<=to; ++i) vd[i] = NA_LOGICAL;
  } break;
  case INTSXP : {
    // same whether factor or not
    int *vd = INTEGER(v);
    for (int i=from; i<=to; ++i) vd[i] = NA_INTEGER;
  } break;
  case REALSXP : {
    if (INHERITS(v, char_integer64)) {
      int64_t *vd = (int64_t *)REAL(v);
      for (int i=from; i<=to; ++i) vd[i] = INT64_MIN;
    } else {
      double *vd = REAL(v);
      for (int i=from; i<=to; ++i) vd[i] = NA_REAL;
    }
  } break;
  case STRSXP :
    // character columns are initialized with blank string (""). So replace the all-"" with all-NA_character_
    // Since "" and NA_character_ are global constants in R, it should be ok to not use SET_STRING_ELT here. But use it anyway for safety (revisit if proved slow)
    // If there's ever a way added to R API to pass NA_STRING to allocVector() to tell it to initialize with NA not "", would be great
    for (int i=from; i<=to; ++i) SET_STRING_ELT(v, i, NA_STRING);
    break;
  case VECSXP :
    // list columns already have each item initialized to NULL
    break;
  default :
    error("Internal error: writeNA passed a vector of type '%s'", type2char(TYPEOF(v)));  // # nocov
  }
}

SEXP allocNAVector(SEXPTYPE type, R_len_t n)
{
  // an allocVector following with initialization to NA since a subassign to a new column using :=
  // routinely leaves untouched items (rather than 0 or "" as allocVector does with its memset)
  // We guess that author of allocVector would have liked to initialize with NA but was prevented since memset
  // is restricted to one byte.
  SEXP v = PROTECT(allocVector(type, n));
  writeNA(v, 0, n);
  UNPROTECT(1);
  return(v);
}

static SEXP *saveds=NULL;
static R_len_t *savedtl=NULL, nalloc=0, nsaved=0;

void savetl_init() {
  if (nsaved || nalloc || saveds || savedtl) {
    error("Internal error: savetl_init checks failed (%d %d %p %p). please report to data.table issue tracker.", nsaved, nalloc, saveds, savedtl); // # nocov
  }
  nsaved = 0;
  nalloc = 100;
  saveds = (SEXP *)malloc(nalloc * sizeof(SEXP));
  savedtl = (R_len_t *)malloc(nalloc * sizeof(R_len_t));
  if (saveds==NULL || savedtl==NULL) {
    savetl_end();                                                        // # nocov
    error("Failed to allocate initial %d items in savetl_init", nalloc); // # nocov
  }
}

void savetl(SEXP s)
{
  if (nsaved==nalloc) {
    if (nalloc==INT_MAX) {
      savetl_end();                                                                                                     // # nocov
      error("Internal error: reached maximum %d items for savetl. Please report to data.table issue tracker.", nalloc); // # nocov
    }
    nalloc = nalloc>(INT_MAX/2) ? INT_MAX : nalloc*2;
    char *tmp = (char *)realloc(saveds, nalloc*sizeof(SEXP));
    if (tmp==NULL) {
      // C spec states that if realloc() fails the original block is left untouched; it is not freed or moved. We rely on that here.
      savetl_end();                                                      // # nocov  free(saveds) happens inside savetl_end
      error("Failed to realloc saveds to %d items in savetl", nalloc);   // # nocov
    }
    saveds = (SEXP *)tmp;
    tmp = (char *)realloc(savedtl, nalloc*sizeof(R_len_t));
    if (tmp==NULL) {
      savetl_end();                                                      // # nocov
      error("Failed to realloc savedtl to %d items in savetl", nalloc);  // # nocov
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
  free(saveds);  // possible free(NULL) which is safe no-op
  saveds = NULL;
  free(savedtl);
  savedtl = NULL;
  nsaved = nalloc = 0;
}

SEXP setcharvec(SEXP x, SEXP which, SEXP newx)
{
  int w;
  if (!isString(x)) error("x must be a character vector");
  if (!isInteger(which)) error("'which' must be an integer vector");
  if (!isString(newx)) error("'new' must be a character vector");
  if (LENGTH(newx)!=LENGTH(which)) error("'new' is length %d. Should be the same as length of 'which' (%d)",LENGTH(newx),LENGTH(which));
  for (int i=0; i<LENGTH(which); i++) {
    w = INTEGER(which)[i];
    if (w==NA_INTEGER || w<1 || w>LENGTH(x)) error("Item %d of 'which' is %d which is outside range of the length %d character vector", i+1,w,LENGTH(x));
    SET_STRING_ELT(x, w-1, STRING_ELT(newx, i));
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

