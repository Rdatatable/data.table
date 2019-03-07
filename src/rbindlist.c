#include "data.table.h"
#include <Rdefines.h>


// factorType is 1 for factor and 2 for ordered
// will simply unique normal factors and attempt to find global order for ordered ones
SEXP combineFactorLevels(SEXP factorLevels, int * factorType, Rboolean * isRowOrdered)
{
  return R_NilValue;
}
// need to provide this for use by fmelt.c too


/* TODO .. delete
  Arun's addition and changes to incorporate 'usenames=T/F' and 'fill=T/F' arguments to rbindlist */

/*
  l               = input list of data.tables/lists/data.frames
  n               = length(l)
  ans             = rbind'd result
  i               = an index over length of l
  use.names       = whether binding should check for names and bind them accordingly
  fill            = whether missing columns should be filled with NAs

  ans_ptr         = final_names - column names for 'ans' (list item 1)
  ans_ptr         = match_indices - when use.names=TRUE, for each element in l, what's the destination col index (in 'ans') for each of the cols (list item 2)
  n_rows          = total number of rows in 'ans'
  n_cols          = total number of cols in 'ans'
  max_type        = for each col in 'ans', what's the final SEXPTYPE? (for coercion if necessary)
  is_factor       = for each col in 'ans' mark which one's a factor (to convert to factor at the end)
  is_ofactor      = for each col in 'ans' mark which one's an ordered factor (to convert to ordered factor at the end)
  fn_rows         = the length of first column (rows) for each item in l.
  mincol          = get the minimum number of columns in an item from l. Used to check if 'fill=TRUE' is really necessary, even if set.
*/

/*
static void preprocess(SEXP l, Rboolean usenames, Rboolean fill, struct preprocessData *data) {

  R_len_t i, j, idx;
  SEXP li, lnames=R_NilValue, fnames, findices=R_NilValue, f_ind=R_NilValue, thiscol, col_name=R_NilValue, thisClass = R_NilValue;
  SEXPTYPE type;

  data->first = -1; data->lcount = 0; data->n_rows = 0; data->n_cols = 0; data->protecti = 0;
  data->max_type = NULL; data->is_factor = NULL; data->ans_ptr = R_NilValue; data->mincol=0;
  data->fn_rows = (int *)R_alloc(LENGTH(l), sizeof(int));
  data->colname = R_NilValue;

  // get first non null name, 'rbind' was doing a 'match.names' for each item.. which is a bit more time consuming.
  // And warning that it'll be matched by names is not necessary, I think, as that's the default for 'rbind'. We
  // should instead document it.
  for (i=0; i<LENGTH(l); i++) { // isNull is checked already in rbindlist
    li = VECTOR_ELT(l, i);
    if (isNull(li)) continue;
    if (TYPEOF(li) != VECSXP) error("Item %d of list input is not a data.frame, data.table or list",i+1);
    if (!LENGTH(li)) continue;
    col_name = getAttrib(li, R_NamesSymbol);
    if (!isNull(col_name)) break;
  }
  if (!isNull(col_name)) { data->colname = PROTECT(col_name); data->protecti++; }
  if (usenames) { lnames = PROTECT(allocVector(VECSXP, LENGTH(l))); data->protecti++;}
  for (i=0; i<LENGTH(l); i++) {
    data->fn_rows[i] = 0;  // careful to initialize before continues as R_alloc above doesn't initialize
    li = VECTOR_ELT(l, i);
    if (isNull(li)) continue;
    if (TYPEOF(li) != VECSXP) error("Item %d of list input is not a data.frame, data.table or list",i+1);
    if (!LENGTH(li)) continue;
    col_name = getAttrib(li, R_NamesSymbol);
    if (fill && isNull(col_name))
      error("fill=TRUE, but names of input list at position %d is NULL. All items of input list must have names set when fill=TRUE.", i+1);
    data->lcount++;
    data->fn_rows[i] = length(VECTOR_ELT(li, 0));
    if (data->first == -1) {
      data->first = i;
      data->n_cols = LENGTH(li);
      data->mincol = LENGTH(li);
      if (!usenames) {
        data->ans_ptr = PROTECT(allocVector(VECSXP, 2)); data->protecti++;
        if (isNull(col_name)) SET_VECTOR_ELT(data->ans_ptr, 0, data->colname);
        else SET_VECTOR_ELT(data->ans_ptr, 0, col_name);
      } else {
        if (isNull(col_name)) SET_VECTOR_ELT(lnames, i, data->colname);
        else SET_VECTOR_ELT(lnames, i, col_name);
      }
      data->n_rows += data->fn_rows[i];
      continue;
    } else {
      if (!fill && LENGTH(li) != data->n_cols)
        if (LENGTH(li) != data->n_cols) error("Item %d has %d columns, inconsistent with item %d which has %d columns. If instead you need to fill missing columns, use set argument 'fill' to TRUE.",i+1, LENGTH(li), data->first+1, data->n_cols);
    }
    if (data->mincol > LENGTH(li)) data->mincol = LENGTH(li);
    data->n_rows += data->fn_rows[i];
    if (usenames) {
      if (isNull(col_name)) SET_VECTOR_ELT(lnames, i, data->colname);
      else SET_VECTOR_ELT(lnames, i, col_name);
    }
  }
  if (usenames) {
    data->ans_ptr = PROTECT(match_names(lnames)); data->protecti++;
    fnames = VECTOR_ELT(data->ans_ptr, 0);
    findices = VECTOR_ELT(data->ans_ptr, 1);
    if (isNull(data->colname) && data->n_cols > 0)
      error("use.names=TRUE but no item of input list has any names.\n");
    if (!fill && length(fnames) != data->mincol) {
      error("Answer requires %d columns whereas one or more item(s) in the input list has only %d columns. This could be because the items in the list may not all have identical column names or some of the items may have duplicate names. In either case, if you're aware of this and would like to fill those missing columns, set the argument 'fill=TRUE'.", length(fnames), data->mincol);
    } else data->n_cols = length(fnames);
  }

  // decide type of each column
  // initialize the max types - will possibly increment later
  data->max_type  = (SEXPTYPE *)R_alloc(data->n_cols, sizeof(SEXPTYPE));
  data->is_factor = (int *)R_alloc(data->n_cols, sizeof(int));
  for (i = 0; i< data->n_cols; i++) {
    thisClass = R_NilValue;
    data->max_type[i] = 0;
    data->is_factor[i] = 0;
    if (usenames) f_ind = VECTOR_ELT(findices, i);
    for (j=data->first; j<LENGTH(l); j++) {
      if (data->is_factor[i] == 2) break;
      idx = (usenames) ? INTEGER(f_ind)[j] : i;
      li = VECTOR_ELT(l, j);
      if (isNull(li) || !LENGTH(li) || idx < 0) continue;
      thiscol = VECTOR_ELT(li, idx);
      // Fix for #705, check attributes
      if (j == data->first)
        thisClass = getAttrib(thiscol, R_ClassSymbol);
      if (isFactor(thiscol)) {
        data->is_factor[i] = (isOrdered(thiscol)) ? 2 : 1;
        data->max_type[i]  = STRSXP;
      } else {
        // Fix for #705, check attributes and error if non-factor class and not identical
        if (!data->is_factor[i] &&
          !R_compute_identical(thisClass, getAttrib(thiscol, R_ClassSymbol), 0) && !fill) {
          error("Class attributes at column %d of input list at position %d does not match with column %d of input list at position %d. Coercion of objects of class 'factor' alone is handled internally by rbind/rbindlist at the moment.", i+1, j+1, i+1, data->first+1);
        }
        type = TYPEOF(thiscol);
        if (type > data->max_type[i]) data->max_type[i] = type;
      }
    }
  }
}
*/

// function does c(idcol, nm), where length(idcol)=1
// fix for #1432, + more efficient to move the logic to C
/*
SEXP add_idcol(SEXP nm, SEXP idcol, int cols) {
  SEXP ans = PROTECT(allocVector(STRSXP, cols+1));
  SET_STRING_ELT(ans, 0, STRING_ELT(idcol, 0));
  for (int i=0; i<cols; i++) {
    SET_STRING_ELT(ans, i+1, STRING_ELT(nm, i));
  }
  UNPROTECT(1);
  return (ans);
}
*/

SEXP rbindlist(SEXP l, SEXP usenamesArg, SEXP fillArg, SEXP idcol) {

  //todelete ... R_len_t jj, ansloc, resi, i,j, idx, thislen;
  //struct preprocessData data;
  //Rboolean to_copy = FALSE, coerced=FALSE, isidcol = !isNull(idcol);
  //SEXP fnames = R_NilValue, findices = R_NilValue, f_ind = R_NilValue, ans, lf, li, target, thiscol, levels;
  //R_len_t protecti=0;

  // first level of error checks
  if (!isLogical(usenamesArg) || LENGTH(usenamesArg)!=1 || LOGICAL(usenamesArg)[0]==NA_LOGICAL)
    error("use.names= should be TRUE or FALSE");
  if (!isLogical(fillArg) || LENGTH(fillArg) != 1 || LOGICAL(fillArg)[0] == NA_LOGICAL)
    error("fill= should be TRUE or FALSE");
  if (!length(l)) return(l);
  if (TYPEOF(l) != VECSXP) error("Input to rbindlist must be a list. This list can contain data.tables, data.frames or plain lists.");
  bool usenames = LOGICAL(usenamesArg)[0];
  bool fill = LOGICAL(fillArg)[0];
  if (fill && !usenames) {
    warning("use.names= can not be FALSE when fill is TRUE. Setting use.names=TRUE.\n");
    usenames=true;
  }

  int ncol=-1, first=0;
  int64_t nrow=0;
  // pre-check for any errors here to save having to get cleanup right below when usenames
  for (int i=0; i<LENGTH(l); i++) {  // length(l)>0 checked above
    SEXP li = VECTOR_ELT(l, i);
    if (isNull(li)) continue;
    if (TYPEOF(li) != VECSXP) error("Item %d of input is not a data.frame, data.table or list", i+1);
    if (!LENGTH(li)) continue;
    if ((usenames||fill) && isNull(getAttrib(li, R_NamesSymbol))) error("When use.names=TRUE or fill=TRUE every item of the input must have column names. Item %d does not.", i+1);
    if (ncol==-1) { ncol=LENGTH(li); first=i; }
    else if (!fill && ncol!=LENGTH(li)) error("Item %d has %d columns, inconsistent with item %d which has %d columns. To fill missing columns use fill=TRUE.", i+1, LENGTH(li), first+1, ncol);
    nrow += length(VECTOR_ELT(li,0));
  }

  int *listmap = NULL; // each row will map the items of each list to the appropriate column in the final result
  if (usenames) {
    savetl_init();
    // first find number of unique column names present; i.e. length(unique(unlist(lapply(l,names))))
    int nuniq=0;
    for (int i=0; i<LENGTH(l); i++) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=LENGTH(li);
      if (isNull(li) || !LENGTH(li)) continue;
      const SEXP *cnp = STRING_PTR(getAttrib(li, R_NamesSymbol));
      for (int j=0; j<thisncol; j++) {
        SEXP s = cnp[j];
        if (TRUELENGTH(s)<0) continue;  // seen this name before
        if (TRUELENGTH(s)>0) savetl(s);
        SET_TRUELENGTH(s,--nuniq);
      }
    }
    nuniq = -nuniq;
    int *counts = (int *)R_alloc(nuniq, sizeof(int)); // counts of names for each colnames
    int *maxdup = (int *)R_alloc(nuniq, sizeof(int)); // the most number of dups for any name within one colname vector
    memset(maxdup, 0, nuniq*sizeof(int));
    for (int i=0; i<LENGTH(l); i++) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=LENGTH(li);
      if (isNull(li) || !LENGTH(li)) continue;
      const SEXP *cnp = STRING_PTR(getAttrib(li, R_NamesSymbol));
      memset(counts, 0, nuniq*sizeof(int));
      for (int j=0; j<thisncol; j++) {
        SEXP s = cnp[j];
        counts[ -TRUELENGTH(s)-1 ]++;
      }
      for (int u=0; u<nuniq; u++) {
        if (counts[u] > maxdup[u]) maxdup[u] = counts[u];
      }
    }
    ncol = 0;
    for (int u=0; u<nuniq; ++u) ncol+=maxdup[u];
    // ncol is now the final number of columns accounting for dups, if any

    // allocate a matrix:  nrows==length(list)  each entry contains which column to fetch for that final column
    int *listMap = (int *)R_alloc(nrow*ncol, sizeof(int));

    int *umap = R_alloc(ncol, sizeof(int));
    int *duplink = R_alloc(ncol, sizeof(int));
    memset(umap, 0, ncol*sizeof(int));
    memset(duplink, 0, ncol*sizeof(int));
    int nextcol = 0;

    for (int i=whichlongest;  ... i<LENGTH(l); i++) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=LENGTH(li);
      if (isNull(li) || !LENGTH(li)) continue;
      const SEXP *cnp = STRING_PTR(getAttrib(li, R_NamesSymbol));
      memset(counts, 0, nunique*sizeof(int));
      for (int j=0; j<thisncol; j++) {
        SEXP s = cnp[j];
        int w = -TRUELENGTH(s)-1;
        int wi = counts[w]++; // how many dups have we seen before of this name within this item
        for (int k=1; k<wi; k++) { w=duplink[w]; k++; }  // hop through the dups
        if (umap[w]==0) {
          // first time ever seen this name,
          umap[w] = nextcol++;
        } if (duplink[w]==0) {
          duplink[w] = nextcol;
          w = nextcol;
          umap[w] = nextcol++;
        } else {
          w = duplink[w];
        }
        listmap[i,j] = umap[w];
      }
    }
    // zero out our usage of tl
    for (int i=0; i<LENGTH(l); i++) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=LENGTH(li);
      if (isNull(li) || !LENGTH(li)) continue;
      const SEXP *cnp = STRING_PTR(getAttrib(li, R_NamesSymbol));
      for (int j=0; j<thisncol; j++) {
        SEXP s = cnp[j];
        SET_TRUELENGTH(s, 0);
      }
    }
    savetl_end();  // restore R's usage
    if (nextcol != ncol) error("Internal error. nextcol!=ncol when making listmap");  // # nocov
  }


  // get max type for each column, and the number of rows
  if (now==0 && ncol==0) {
    return(R_NilValue);
  }
  if (nrow>INT32_MAX) error("Total rows in the list is %lld which is larger than the maximum number of rows, currently %d", nrow, INT32_MAX);

  /*todelete ...
  fnames = VECTOR_ELT(data.ans_ptr, 0);
  if (isidcol) {
    fnames = PROTECT(add_idcol(fnames, idcol, data.n_cols));
    protecti++;
  }
  SEXP factorLevels = PROTECT(allocVector(VECSXP, data.lcount)); protecti++;
  Rboolean *isRowOrdered = (Rboolean *)R_alloc(data.lcount, sizeof(Rboolean));
  for (int i=0; i<data.lcount; i++) isRowOrdered[i] = FALSE;
  */

  SEXP ans = PROTECT(allocVector(VECSXP, ncol + isidcol)); protecti++; // do we need this protecti?
  SEXP tt;
  setAttrib(ans, R_NamesSymbol, tt=allocVector(STRSXP, ncol));


  // we need listmap to tell us which item to fetch for each of the final result columns, so we can go column by column

  for(int j=0; j<ncol; ++j) {

    maxType=0;
    thisClass = R_NilValue;
    isFactor=false;
    isInt64=false;
    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      if (isNull(li) || !LENGTH(li) || !length(VECTOR_ELT(li, 0))) continue;
      int w = listMap[i, j];
      if (w==-1) continue;  // column j of final result has no input from this item (fill must be true)
      SEXP thisCol = VECTOR_ELT(li, w);
      thisType = TYPEOF(thisCol);
      if (thisType>maxType) maxType=thisType;
      if (isFactor(thiscol)) isFactor=true;   // TODO isOrdered(thiscol) ? 2 : 1;
      if (INHERITS(getAttrib(thisCol, R_ClassSymbol), char_integer64)) isInt64=true;
      // TODO #705, check attributes and error if non-factor class and not identical
      // if (!data->is_factor[i] &&
      //    !R_compute_identical(thisClass, getAttrib(thiscol, R_ClassSymbol), 0) && !fill) {
      //   error("Class attributes at column %d of input list at position %d does not match with column %d of input list at position %d. Coercion of objects of class 'factor' alone is handled internally by rbind/rbindlist at the moment.", i+1, j+1, i+1, data->first+1);
      // }
    }
    if (isFactor) maxType=INTSXP;  // any items are factors then a factor is created (could be an option)
    SEXP target;
    SET_VECTOR_ELT(ans, j+isidcol, target=allocVector(maxType, nrow));  // does not initialize logical & numerics, but does initialize character and list
    int ansloc=0;
    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      if (isNull(li) || !LENGTH(li)) continue;
      const int thisnrow = length(VECTOR_ELT(li, 0));
      if (thisnrow==0) continue;
      int w = listmap[i, j];
      if (w==-1) {
        fill with NA
      } else {
        SEXP thiscol = VECTOR_ELT(li, w);
        switch(TYPEOF(target)) {
        case LGLSXP:
          memcpy(LOGICAL(target)+ansloc, LOGICAL(thiscol), thisnrow*SIZEOF(thiscol));
          break;
        case INTSXP:
          memcpy(INTEGER(target)+ansloc, INTEGER(thiscol), thisnrow*SIZEOF(thiscol));
          break;
        case REALSXP:
          memcpy(REAL(target)+ansloc, REAL(thiscol), thisnrow*SIZEOF(thiscol));
          break;
        case CPLXSXP :
          memcpy(COMPLEX(target)+ansloc, COMPLEX(thiscol), thisnrow*sizeof(Rcomplex));
          break;
        case VECSXP :
          for (int r=0; r<thisnrow; r++)
            SET_VECTOR_ELT(target, ansloc+r, VECTOR_ELT(thiscol,r));
          break;
        case STRSXP :
          isRowOrdered[resi] = FALSE;
          if (isFactor(thiscol)) {
            levels = getAttrib(thiscol, R_LevelsSymbol);
            if (isNull(levels)) error("Column %d of item %d has type 'factor' but has no levels; i.e. malformed.", j+1, i+1);
            for (r=0; r<thislen; r++)
              if (INTEGER(thiscol)[r]==NA_INTEGER)
                SET_STRING_ELT(target, ansloc+r, NA_STRING);
              else
                SET_STRING_ELT(target, ansloc+r, STRING_ELT(levels,INTEGER(thiscol)[r]-1));

            // add levels to factorLevels
            // changed "i" to "jj" and increment 'jj' after so as to fill only non-empty tables with levels
            SET_VECTOR_ELT(factorLevels, jj, levels); jj++;
            if (isOrdered(thiscol)) isRowOrdered[resi] = TRUE;
          } else {
            if (TYPEOF(thiscol) != STRSXP) error("Internal logical error in rbindlist.c (not STRSXP), please report to data.table issue tracker.");
            for (int r=0; r<thislen; r++) SET_STRING_ELT(target, ansloc+r, STRING_ELT(thiscol,r));

            // if this column is going to be a factor, add column to factorLevels
            // changed "i" to "jj" and increment 'jj' after so as to fill only non-empty tables with levels
            if (data.is_factor[j]) {
              SET_VECTOR_ELT(factorLevels, jj, thiscol);
              jj++;
            }
            // removed 'coerced=FALSE; UNPROTECT(1)' as it resulted in a stack imbalance.
            // anyways it's taken care of after the switch. So no need here.
          }
          break;
        default :
          error("Unsupported column type '%s'", type2char(TYPEOF(target)));
        }
      }
      ansloc += thisnrow;
    }



    if (usenames) {
      to_copy = TRUE;
      f_ind   = VECTOR_ELT(findices, j);
    } else {
      thiscol = VECTOR_ELT(lf, j);
      if (!isFactor(thiscol)) copyMostAttrib(thiscol, target); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
    }
    ansloc = 0;
    jj = 0; // to increment factorLevels
    resi = -1;
    for (i=data.first; i<LENGTH(l); i++) {
      li = VECTOR_ELT(l,i);
      if (!length(li)) continue;  // majority of time though, each item of l is populated
      thislen = data.fn_rows[i];
      idx = (usenames) ? INTEGER(f_ind)[i] : j;
      if (idx < 0) {
        ansloc += thislen;
        resi++;
        if (data.is_factor[j]) {
          isRowOrdered[resi] = FALSE;
          SET_VECTOR_ELT(factorLevels, jj, allocNAVector(data.max_type[j], 1)); // the only level here is NA.
          jj++;
        }
        continue;
      }
      thiscol = VECTOR_ELT(li, idx);
      if (thislen != length(thiscol)) error("Column %d of item %d is length %d, inconsistent with first column of that item which is length %d. rbind/rbindlist doesn't recycle as it already expects each item to be a uniform list, data.frame or data.table", j+1, i+1, length(thiscol), thislen);
      // couldn't figure out a way to this outside this loop when fill = TRUE.
      if (to_copy && !isFactor(thiscol)) {
        copyMostAttrib(thiscol, target);
        to_copy = FALSE;
      }
      resi++;  // after the first, there might be NULL or empty which are skipped, resi increments up until lcount
      if (TYPEOF(thiscol) != TYPEOF(target) && !isFactor(thiscol)) {
        thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));
        coerced = TRUE;
        // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
        // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
      }
      if (TYPEOF(target)!=STRSXP && TYPEOF(thiscol)!=TYPEOF(target)) {
        error("Internal error in rbindlist.c: type of 'thiscol' [%s] should have already been coerced to 'target' [%s]. please report to data.table issue tracker.",
              type2char(TYPEOF(thiscol)), type2char(TYPEOF(target)));
      }
      switch(TYPEOF(target)) {
      case LGLSXP:
        memcpy(LOGICAL(target)+ansloc, LOGICAL(thiscol), thislen*SIZEOF(thiscol));
        break;
      case INTSXP:
        memcpy(INTEGER(target)+ansloc, INTEGER(thiscol), thislen*SIZEOF(thiscol));
        break;
      case REALSXP:
        memcpy(REAL(target)+ansloc, REAL(thiscol), thislen*SIZEOF(thiscol));
        break;
      case CPLXSXP :
        memcpy(COMPLEX(target)+ansloc, COMPLEX(thiscol), thislen*sizeof(Rcomplex));
        break;
      case VECSXP :
        for (int r=0; r<thislen; r++)
          SET_VECTOR_ELT(target, ansloc+r, VECTOR_ELT(thiscol,r));
        break;
      case STRSXP :
        isRowOrdered[resi] = FALSE;
        if (isFactor(thiscol)) {
          levels = getAttrib(thiscol, R_LevelsSymbol);
          if (isNull(levels)) error("Column %d of item %d has type 'factor' but has no levels; i.e. malformed.", j+1, i+1);
          for (r=0; r<thislen; r++)
            if (INTEGER(thiscol)[r]==NA_INTEGER)
              SET_STRING_ELT(target, ansloc+r, NA_STRING);
            else
              SET_STRING_ELT(target, ansloc+r, STRING_ELT(levels,INTEGER(thiscol)[r]-1));

          // add levels to factorLevels
          // changed "i" to "jj" and increment 'jj' after so as to fill only non-empty tables with levels
          SET_VECTOR_ELT(factorLevels, jj, levels); jj++;
          if (isOrdered(thiscol)) isRowOrdered[resi] = TRUE;
        } else {
          if (TYPEOF(thiscol) != STRSXP) error("Internal logical error in rbindlist.c (not STRSXP), please report to data.table issue tracker.");
          for (int r=0; r<thislen; r++) SET_STRING_ELT(target, ansloc+r, STRING_ELT(thiscol,r));

          // if this column is going to be a factor, add column to factorLevels
          // changed "i" to "jj" and increment 'jj' after so as to fill only non-empty tables with levels
          if (data.is_factor[j]) {
            SET_VECTOR_ELT(factorLevels, jj, thiscol);
            jj++;
          }
          // removed 'coerced=FALSE; UNPROTECT(1)' as it resulted in a stack imbalance.
          // anyways it's taken care of after the switch. So no need here.
        }
        break;
      default :
        error("Unsupported column type '%s'", type2char(TYPEOF(target)));
      }
      ansloc += thislen;
      if (coerced) {
        UNPROTECT(1);
        coerced = FALSE;
      }
    }
    if (data.is_factor[j]) {
      SEXP finalFactorLevels = PROTECT(combineFactorLevels(factorLevels, &(data.is_factor[j]), isRowOrdered));
      SEXP factorLangSxp = PROTECT(lang3(install(data.is_factor[j] == 1 ? "factor" : "ordered"),
                         target, finalFactorLevels));
      SET_VECTOR_ELT(ans, j+isidcol, eval(factorLangSxp, R_GlobalEnv));
      UNPROTECT(2);  // finalFactorLevels, factorLangSxp
    }
  }

  // fix for #1432, + more efficient to move the logic to C
  if (isidcol) {
    R_len_t runidx = 1, cntridx = 0;
    SEXP lnames = getAttrib(l, R_NamesSymbol);
    if (isNull(lnames)) {
      SET_VECTOR_ELT(ans, 0, target=allocVector(INTSXP, data.n_rows) );
      for (i=0; i<LENGTH(l); i++) {
        for (j=0; j<data.fn_rows[i]; j++)
          INTEGER(target)[cntridx++] = runidx;
        runidx++;
      }
    } else {
      SET_VECTOR_ELT(ans, 0, target=allocVector(STRSXP, data.n_rows) );
      for (i=0; i<LENGTH(l); i++) {
        for (j=0; j<data.fn_rows[i]; j++)
          SET_STRING_ELT(target, cntridx++, STRING_ELT(lnames, i));
      }
    }
  }

  UNPROTECT(protecti);
  return(ans);
}

/*
## The section below implements "chmatch2_old" and "chmatch2" (faster version of chmatch2_old).
## It's basically 'pmatch' but without the partial matching part. These examples should
## make it clearer.
## Examples:
## chmatch2_old(c("a", "a"), c("a", "a"))     # 1,2  - the second 'a' in 'x' has a 2nd match in 'table'
## chmatch2_old(c("a", "a"), c("a", "b"))     # 1,NA - the second one doesn't 'see' the first 'a'
## chmatch2_old(c("a", "a"), c("a", "a.1"))   # 1,NA - differs from 'pmatch' output = 1,2
##
## The algorithm: given 'x' and 'y':
## dt = data.table(val=c(x,y), grp1 = rep(1:2, c(length(x),length(y))), grp2=c(1:length(x), 1:length(y)))
## dt[, grp1 := 0:(.N-1), by="val,grp1"]
## dt[, grp2[2], by="val,grp1"]
##
## NOTE: This is FAST, but not AS FAST AS it could be. See chmatch2 for a faster implementation (and bottom
## of this file for a benchmark). I've retained here for now. Ultimately, will've to discuss with Matt and
## probably export it??
*/
/* TODO delete ...
SEXP chmatch2_old(SEXP x, SEXP table, SEXP nomatch) {

  R_len_t i, j, k, nx, li, si, oi;
  if (TYPEOF(nomatch) != INTSXP || length(nomatch) != 1) error("'nomatch' must be an integer of length 1");
  if (!length(x) || isNull(x)) return(allocVector(INTSXP, 0));
  if (TYPEOF(x) != STRSXP) error("'x' must be a character vector");
  nx=length(x);
  if (!length(table) || isNull(table)) {
    SEXP ans = PROTECT(allocVector(INTSXP, nx));
    for (i=0; i<nx; i++) INTEGER(ans)[i] = INTEGER(nomatch)[0];
    UNPROTECT(1);
    return(ans);
  }
  if (TYPEOF(table) != STRSXP) error("'table' must be a character vector");
  // Done with special cases. On to the real deal.

  SEXP tt = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(tt, 0, x);
  SET_VECTOR_ELT(tt, 1, table);
  SEXP dt = PROTECT(unlist2(tt));

  // order - first time
  SEXP order = PROTECT(fast_order(dt, 2, 1));
  SEXP start = getAttrib(order, sym_starts);
  SEXP lens  = PROTECT(uniq_lengths(start, length(order))); // length(order) = nrow(dt)
  SEXP grpid = VECTOR_ELT(dt, 1);
  SEXP index = VECTOR_ELT(dt, 2);

  // replace dt[1], we don't need it anymore
  k=0;
  for (i=0; i<length(lens); i++) {
    for (j=0; j<INTEGER(lens)[i]; j++) {
      INTEGER(grpid)[INTEGER(order)[k+j]-1] = j;
    }
    k += j;
  }
  // order - again
  order = PROTECT(fast_order(dt, 2, 1));
  start = getAttrib(order, sym_starts);
  lens  = PROTECT(uniq_lengths(start, length(order)));

  SEXP ans = PROTECT(allocVector(INTSXP, nx));
  k = 0;
  for (i=0; i<length(lens); i++) {
    li = INTEGER(lens)[i];
    si = INTEGER(start)[i]-1;
    oi = INTEGER(order)[si]-1;
    if (oi > nx-1) continue;
    INTEGER(ans)[oi] = (li == 2) ? INTEGER(index)[INTEGER(order)[si+1]-1]+1 : INTEGER(nomatch)[0];
  }
  UNPROTECT(7);
  return(ans);
}
*/

// utility function used from within chmatch2
/*
static SEXP listlist(SEXP x) {

  R_len_t i,j,k, nl;
  SEXP lx, xo, xs, xl, tmp, ans, ans0, ans1;

  lx = PROTECT(allocVector(VECSXP, 1));
  SET_VECTOR_ELT(lx, 0, x);
  xo = PROTECT(fast_order(lx, 1, 1));
  xs = getAttrib(xo, sym_starts);
  xl = PROTECT(uniq_lengths(xs, length(x)));

  ans0 = PROTECT(allocVector(STRSXP, length(xs)));
  ans1 = PROTECT(allocVector(VECSXP, length(xs)));
  k=0;
  for (i=0; i<length(xs); i++) {
    SET_STRING_ELT(ans0, i, STRING_ELT(x, INTEGER(xo)[INTEGER(xs)[i]-1]-1));
    nl = INTEGER(xl)[i];
    SET_VECTOR_ELT(ans1, i, tmp=allocVector(INTSXP, nl) );
    for (j=0; j<nl; j++) {
      INTEGER(tmp)[j] = INTEGER(xo)[k+j];
    }
    k += j;
  }
  ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, ans0);
  SET_VECTOR_ELT(ans, 1, ans1);
  UNPROTECT(6);
  return(ans);
}
*/


/*
## While chmatch2_old works great, I find it inefficient in terms of both memory (stores 2 indices over the
## length of x+y) and speed (2 ordering and looping over unnecesssary amount of times). So, here's
## another stab at a faster version of 'chmatch2_old', leveraging the power of 'chmatch' and data.table's
## DT[ , list(list()), by=.] syntax.
##
## The algorithm:
## x.agg = data.table(x)[, list(list(rep(x, .N))), by=x]
## y.agg = data.table(y)[, list(list(rep(y, .N))), by=y]
## mtch  = chmatch(x.agg, y.agg, nomatch)                 ## here we look at only unique values!
## Now, it's just a matter of filling corresponding matches from x.agg's indices with y.agg's indices.
## BENCHMARKS ON THE BOTTOM OF THIS FILE
*/
SEXP chmatch2(SEXP x, SEXP y, SEXP nomatch) {

  R_len_t i, j, k, nx, ix, iy;
  SEXP xll, yll, xu, yu, ans, xl, yl, mx;
  if (TYPEOF(nomatch) != INTSXP || length(nomatch) != 1) error("'nomatch' must be an integer of length 1");
  if (!length(x) || isNull(x)) return(allocVector(INTSXP, 0));
  if (TYPEOF(x) != STRSXP) error("'x' must be a character vector");
  nx = length(x);
  if (!length(y) || isNull(y)) {
    ans = PROTECT(allocVector(INTSXP, nx));
    for (i=0; i<nx; i++) INTEGER(ans)[i] = INTEGER(nomatch)[0];
    UNPROTECT(1);
    return(ans);
  }
  if (TYPEOF(y) != STRSXP) error("'table' must be a character vector");
  // Done with special cases. On to the real deal.
  xll = PROTECT(listlist(x));
  yll = PROTECT(listlist(y));

  xu = VECTOR_ELT(xll, 0);
  yu = VECTOR_ELT(yll, 0);

  mx  = PROTECT(chmatch(xu, yu, 0, FALSE));
  ans = PROTECT(allocVector(INTSXP, nx));
  k=0;
  for (i=0; i<length(mx); i++) {
    xl = VECTOR_ELT(VECTOR_ELT(xll, 1), i);
    ix = length(xl);
    if (INTEGER(mx)[i] == 0) {
      for (j=0; j<ix; j++)
        INTEGER(ans)[INTEGER(xl)[j]-1] = INTEGER(nomatch)[0];
    } else {
      yl = VECTOR_ELT(VECTOR_ELT(yll, 1), INTEGER(mx)[i]-1);
      iy = length(yl);
      for (j=0; j < ix; j++)
        INTEGER(ans)[INTEGER(xl)[j]-1] = (j < iy) ? INTEGER(yl)[j] : INTEGER(nomatch)[0];
      k += ix;
    }
  }
  UNPROTECT(4);
  return(ans);

}

/*
## Benchmark:
set.seed(45L)
x <- sample(letters, 1e6, TRUE)
y <- sample(letters, 1e7, TRUE)
system.time(ans1 <- .Call("Cchmatch2_old", x,y,0L)) # 2.405 seconds
system.time(ans2 <- .Call("Cchmatch2", x,y,0L)) # 0.174 seconds
identical(ans1, ans2) # [1] TRUE
## Note: 'pmatch(x,y,0L)' dint finish still after about 5 minutes, so stopped.
## Speed up of about ~14x!!! nice ;). (And this uses lesser memory as well).
*/
