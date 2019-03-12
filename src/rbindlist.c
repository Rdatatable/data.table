#include "data.table.h"
#include <Rdefines.h>


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

SEXP rbindlist(SEXP l, SEXP usenamesArg, SEXP fillArg, SEXP idcolArg) {

  if (!isLogical(usenamesArg) || LENGTH(usenamesArg)!=1 || LOGICAL(usenamesArg)[0]==NA_LOGICAL)
    error("use.names= should be TRUE or FALSE");
  if (!isLogical(fillArg) || LENGTH(fillArg) != 1 || LOGICAL(fillArg)[0] == NA_LOGICAL)
    error("fill= should be TRUE or FALSE");
  if (!length(l)) return(l);
  if (TYPEOF(l) != VECSXP) error("Input to rbindlist must be a list. This list can contain data.tables, data.frames or plain lists.");
  bool usenames = LOGICAL(usenamesArg)[0];
  const bool fill = LOGICAL(fillArg)[0];
  if (fill && !usenames) {
    warning("use.names= can not be FALSE when fill is TRUE. Setting use.names=TRUE.\n");
    usenames=true;
  }
  const bool idcol = !isNull(idcolArg);
  if (idcol && (!isString(idcolArg) || LENGTH(idcolArg)!=1)) error("Internal error: rbindlist.c idcol is not a single string");  // # nocov

  int ncol=0, first=0;
  int64_t nrow=0;
  bool anyNames=false;
  // pre-check for any errors here to save having to get cleanup right below when usenames
  for (int i=0; i<LENGTH(l); i++) {  // length(l)>0 checked above
    SEXP li = VECTOR_ELT(l, i);
    if (isNull(li)) continue;
    if (TYPEOF(li) != VECSXP) error("Item %d of input is not a data.frame, data.table or list", i+1);
    const int thisncol = length(li);
    if (!thisncol) continue;
    // delete as now more flexible ... if (fill && isNull(getAttrib(li, R_NamesSymbol))) error("When fill=TRUE every item of the input must have column names. Item %d does not.", i+1);
    if (fill) {
      if (thisncol>ncol) ncol=thisncol;  // this section initializes ncol with max ncol. ncol may be increased when usenames is accounted for further down
    } else {
      if (ncol==0) { ncol=thisncol; first=i; }
      else if (thisncol!=ncol) error("Item %d has %d columns, inconsistent with item %d which has %d columns. To fill missing columns use fill=TRUE.", i+1, thisncol, first+1, ncol);
    }
    int nNames = length(getAttrib(li, R_NamesSymbol));
    if (nNames>0 && nNames!=thisncol) error("Item %d has %d columns but %d column names. Invalid object.", i+1, thisncol, nNames);
    if (nNames>0) anyNames=true;
    int thisnrow = length(VECTOR_ELT(li,0));
    for (int j=1; j<thisncol; ++j) {
      int tt = length(VECTOR_ELT(li, j));
      if (tt!=thisnrow) error("Column %d of item %d is length %d inconsistent with the first column of that item which is length %d. rbind/rbindlist expects each item in the input list to be a uniform list, data.frame or data.table", j+1, i+1, tt, thisnrow);
    }
    nrow += thisnrow;
  }
  if (nrow==0 && ncol==0) return(R_NilValue);
  if (nrow>INT32_MAX) error("Total rows in the list is %lld which is larger than the maximum number of rows, currently %d", nrow, INT32_MAX);
  if (usenames && !anyNames) error("use.names=TRUE but no item of input list has any names");

  int *colMap=NULL; // maps each column in final result to the column of each list item
  if (usenames) {
    // here we proceed as if fill=true for brevity (because accounting for dups is tricky) and then catch any missings after this branch
    // first find number of unique column names present; i.e. length(unique(unlist(lapply(l,names))))
    savetl_init();
    int nuniq=0;
    for (int i=0; i<LENGTH(l); i++) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=LENGTH(li);
      if (isNull(li) || !LENGTH(li)) continue;
      const SEXP cn = getAttrib(li, R_NamesSymbol);
      if (!length(cn)) continue;
      const SEXP *cnp = STRING_PTR(cn);
      for (int j=0; j<thisncol; j++) {
        SEXP s = cnp[j];
        if (TRUELENGTH(s)<0) continue;  // seen this name before
        if (TRUELENGTH(s)>0) savetl(s);
        SET_TRUELENGTH(s,-(++nuniq));
      }
    }
    int *counts = (int *)R_alloc(nuniq, sizeof(int)); // counts of names for each colnames
    int *maxdup = (int *)R_alloc(nuniq, sizeof(int)); // the most number of dups for any name within one colname vector
    memset(maxdup, 0, nuniq*sizeof(int));
    for (int i=0; i<LENGTH(l); i++) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=length(li);
      if (thisncol==0) continue;
      const SEXP cn = getAttrib(li, R_NamesSymbol);
      if (!length(cn)) continue;
      const SEXP *cnp = STRING_PTR(cn);
      memset(counts, 0, nuniq*sizeof(int));
      for (int j=0; j<thisncol; j++) {
        SEXP s = cnp[j];
        counts[ -TRUELENGTH(s)-1 ]++;
      }
      for (int u=0; u<nuniq; u++) {
        if (counts[u] > maxdup[u]) maxdup[u] = counts[u];
      }
    }
    int ttncol = 0;
    for (int u=0; u<nuniq; ++u) ttncol+=maxdup[u];
    if (ttncol>ncol) ncol=ttncol;
    // ncol is now the final number of columns accounting for unique and dups across all colnames

    // allocate a matrix:  nrows==length(list)  each entry contains which column to fetch for that final column
    // ****** TODO TODO ****** these allocs need taking up front or catching when fail to clean up tl
    colMap = (int *)R_alloc(LENGTH(l)*ncol, sizeof(int));
    for (int i=0; i<LENGTH(l)*ncol; ++i) colMap[i]=-1;   // 0-based so use -1
    int *uniqMap = (int *)R_alloc(ncol, sizeof(int)); // maps the ith unique string to the first time it occurs in the final result
    int *dupLink = (int *)R_alloc(ncol, sizeof(int));  // if a colname has occurred before (a dup) links from the 1st to the 2nd time in the final result, 2nd to 3rd, etc
    for (int i=0; i<ncol; ++i) {uniqMap[i] = dupLink[i] = -1;}
    int nextCol=0, lastDup=ncol-1;

    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=length(li);
      if (thisncol==0) continue;
      const SEXP cn = getAttrib(li, R_NamesSymbol);
      if (!length(cn)) {
        for (int j=0; j<thisncol; j++) colMap[i*ncol + j] = j;
      } else {
        const SEXP *cnp = STRING_PTR(cn);
        memset(counts, 0, nuniq*sizeof(int));
        for (int j=0; j<thisncol; j++) {
          SEXP s = cnp[j];
          int w = -TRUELENGTH(s)-1;
          int wi = counts[w]++; // how many dups have we seen before of this name within this item
          if (uniqMap[w]==-1) {
            // first time seen this name across all items
            uniqMap[w] = nextCol++;
          } else {
            while (wi && dupLink[w]>0) { w=dupLink[w]; --wi; }  // hop through the dups
            if (wi && dupLink[w]==-1) {
              // first time we've seen this number of dups of this name
              w = dupLink[w] = lastDup--;
              uniqMap[w] = nextCol++;
            }
          }
          colMap[i*ncol + uniqMap[w]] = j;
        }
      }
    }
    // zero out our usage of tl
    for (int i=0; i<LENGTH(l); i++) {
      SEXP li = VECTOR_ELT(l, i), cn=getAttrib(li, R_NamesSymbol);
      if (!length(li) || !length(cn)) continue;
      const SEXP *cnp = STRING_PTR(cn);
      const int thisncol = LENGTH(li);
      for (int j=0; j<thisncol; j++) {
        SEXP s = cnp[j];
        SET_TRUELENGTH(s, 0);
      }
    }
    savetl_end();  // restore R's usage

    /* to view map when debugging ...
    for (int i=0; i<LENGTH(l); ++i) {
      for (int j=0; j<ncol; ++j) Rprintf("%2d ", colMap[i*ncol + j]);
      Rprintf("\n");
    } */
  }

  if (!fill && usenames) {
    // ensure no missings.  We already checked earlier that ncol is consistent for all list items
    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      if (!length(li) || !length(getAttrib(li, R_NamesSymbol))) continue;
      for (int j=0; j<ncol; ++j) {
        if (colMap[i*ncol + j]==-1) {
          int missi = i;
          while (colMap[i*ncol + j]==-1 && i<LENGTH(l)) i++;
          if (i==LENGTH(l)) error("Internal error: could not find the first column name not present in earlier input list");  // nocov
          SEXP s = getAttrib(VECTOR_ELT(l, i), R_NamesSymbol);
          int w = colMap[i*ncol + j];
          const char *str = isString(s) ? CHAR(STRING_ELT(s,w)) : "";
          error("Column %d ['%s'] of input list %d is missing in input list %d. Use fill=TRUE to fill with NA.", w+1, str, i+1, missi+1);
        }
      }
    }
  }

  SEXP ans=PROTECT(allocVector(VECSXP, idcol + ncol)), ansNames;
  setAttrib(ans, R_NamesSymbol, ansNames=allocVector(STRSXP, idcol + ncol));
  if (idcol) {
    SET_STRING_ELT(ansNames, 0, STRING_ELT(idcolArg, 0));
    SEXP idval, listNames=getAttrib(l, R_NamesSymbol);
    if (length(listNames)) {
      SET_VECTOR_ELT(ans, 0, idval=allocVector(STRSXP, nrow));
      for (int i=0,ansloc=0; i<LENGTH(l); ++i) {
        SEXP li = VECTOR_ELT(l, i);
        if (!length(li)) continue;
        const int thisnrow = length(VECTOR_ELT(li, 0));
        SEXP thisname = STRING_ELT(listNames, i);
        for (int k=0; k<thisnrow; ++k) SET_STRING_ELT(idval, ansloc++, thisname);
      }
    } else {
      SET_VECTOR_ELT(ans, 0, idval=allocVector(INTSXP, nrow));
      int *idvald = INTEGER(idval);
      for (int i=0,ansloc=0; i<LENGTH(l); ++i) {
        SEXP li = VECTOR_ELT(l, i);
        if (!length(li)) continue;
        const int thisnrow = length(VECTOR_ELT(li, 0));
        for (int k=0; k<thisnrow; ++k) idvald[ansloc++] = i+1;
      }
    }
  }

  // colMap tells us which item to fetch for each of the final result columns, so we can stack column-by-column
  for(int j=0; j<ncol; ++j) {
    int maxType=0;
    bool factor=false;
    bool int64=false;
    bool foundName=false;
    //bool anyNotStringOrFactor=false;
    SEXP firstCol=R_NilValue;
    int firsti=-1, firstw=-1;
    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      if (!length(li)) continue;
      int w = usenames ? colMap[i*ncol + j] : j;
      if (w==-1) continue;  // column j of final result has no input from this item (fill must be true)
      if (!foundName) {
        SEXP cn=getAttrib(li, R_NamesSymbol);
        if (length(cn)) { SET_STRING_ELT(ansNames, idcol+j, STRING_ELT(cn, w)); foundName=true; }
      }
      SEXP thisCol = VECTOR_ELT(li, w);
      int thisType = TYPEOF(thisCol);
      if (thisType>maxType) maxType=thisType;
      if (isFactor(thisCol)) {
        if (isNull(getAttrib(thisCol,R_LevelsSymbol))) error("Column %d of input list %d has type 'factor' but has no levels; i.e. malformed.", w+1, i+1);
        factor=true;   // TODO isOrdered(thiscol) ? 2 : 1;
      } //else if (!isString(thisCol) && length(thisCol)) anyNotStringOrFactor=true;
      SEXP thisClass = getAttrib(thisCol, R_ClassSymbol);
      if (INHERITS(thisClass, char_integer64)) int64=true;
      if (firsti==-1) { firsti=i; firstw=w; firstCol=thisCol; }
      else if (!factor && !R_compute_identical(thisClass, getAttrib(firstCol,R_ClassSymbol), 0)) {
        error("Class attribute on column %d of input list %d does not match with the first item for this column (column %d of input list %d).", w+1, i+1, firstw+1, firsti+1);
      }
    }
    // in future warn ... if (factor && anyNotStringOrFactor) warning("Column %d contains a factor but not all items for the column are character or factor", idcol+j+1);
    if (maxType==0) error("Internal error: maxType==0 for result column %d", idcol+j+1);
    if (!foundName) { char buff[12]; sprintf(buff,"V%d",j+1), SET_STRING_ELT(ansNames, idcol+j, mkChar(buff)); }
    if (factor) maxType=INTSXP;  // any items are factors then a factor is created (could be an option)
    SEXP target;
    SET_VECTOR_ELT(ans, idcol+j, target=allocVector(maxType, nrow));  // does not initialize logical & numerics, but does initialize character and list
    if (int64) setAttrib(target, R_ClassSymbol, char_integer64);
    if (!factor) copyMostAttrib(firstCol, target); // all but names,dim and dimnames; mainly for class. And if so, we want a copy here, not keepattr's SET_ATTRIB.
    int ansloc=0;
    if (factor) {
      savetl_init();
      int nLevel=0, allocLevel=0;
      SEXP *levelsRaw = NULL;  // growing list of SEXP pointers. Raw since managed with raw realloc.
      for (int i=0; i<LENGTH(l); ++i) {
        SEXP li = VECTOR_ELT(l, i);
        if (!length(li)) continue;
        const int thisnrow = length(VECTOR_ELT(li, 0));
        if (thisnrow==0) continue;
        int w = usenames ? colMap[i*ncol + j] : j;
        if (w==-1) {
          writeNA(target, ansloc, thisnrow);
        } else {
          SEXP thisCol = VECTOR_ELT(li, w);
          bool coerced=false;
          SEXP thisColStr;
          if (isFactor(thisCol)) thisColStr = getAttrib(thisCol, R_LevelsSymbol);  // TODO these could fail and not clear-up tl; coerce first
          else if (isString(thisCol)) thisColStr = thisCol;
          else {
            thisColStr = PROTECT(coerceVector(thisCol, STRSXP));
            coerced=true;
          }
          const int n = length(thisColStr);
          const SEXP *thisColStrD = STRING_PTR(thisColStr);  // D for data
          for (int k=0; k<n; ++k) {
            SEXP s = thisColStrD[k];
            if (TRUELENGTH(s)<0) continue;  // seen this level before (handles finding unique within character columns too)
            if (TRUELENGTH(s)>0) savetl(s);
            if (allocLevel==nLevel) {
              // manual realloc needed because we need access to vector to clear up
              allocLevel = allocLevel==0 ? 1024 : MAX(allocLevel+(n-k), (int)((double)allocLevel*1.2));
              SEXP *newRaw = malloc(allocLevel * sizeof(SEXP));
              if (newRaw==NULL) {
                for (int k=0; k<nLevel; k++) SET_TRUELENGTH(levelsRaw[k], 0);  // if realloc failed it frees within realloc so we couldn't access it for this line
                savetl_end();
                error("Unable to allocate working memory %lld bytes to hold combined factor levels for result column %d when reading item %d of item %d",
                      allocLevel*sizeof(SEXP), idcol+j+1, w+1, i+1);
              } else {
                memcpy(newRaw, levelsRaw, nLevel*sizeof(SEXP));
                free(levelsRaw);
                levelsRaw = newRaw;
              }
            }
            SET_TRUELENGTH(s,-(++nLevel));
            levelsRaw[nLevel-1] = s;
          }
          int *targetd = INTEGER(target);
          if (isFactor(thisCol)) {
            // loop through levels. If all i == truelength(i) then just do a memcpy. Otherwise create an integer map and hop via that.
            bool nohop = true;
            for (int k=0; k<n; ++k) if (-TRUELENGTH(thisColStrD[k]) != k+1) { nohop=false; break; }
            if (nohop) memcpy(targetd+ansloc, INTEGER(thisCol), thisnrow*SIZEOF(thisCol));
            else {
              int *id = INTEGER(thisCol);
              for (int r=0; r<thisnrow; r++)
                targetd[ansloc+r] = id[r]==NA_INTEGER ? NA_INTEGER : -TRUELENGTH(thisColStrD[id[r]-1]);
            }
          } else {
            SEXP *sd = STRING_PTR(thisColStr);
            for (int r=0; r<thisnrow; r++) targetd[ansloc+r] = sd[r]==NA_STRING ? NA_INTEGER : -TRUELENGTH(sd[r]);
          }
          if (coerced) UNPROTECT(1);
        }
        ansloc += thisnrow;
      }
      for (int k=0; k<nLevel; ++k) SET_TRUELENGTH(levelsRaw[k], 0);
      savetl_end();
      SEXP levelsSxp;
      setAttrib(target, R_LevelsSymbol, levelsSxp=allocVector(STRSXP, nLevel));
      for (int k=0; k<nLevel; ++k) SET_STRING_ELT(levelsSxp, k, levelsRaw[k]);
      free(levelsRaw);
      setAttrib(target, R_ClassSymbol, ScalarString(char_factor));
    } else {
      for (int i=0; i<LENGTH(l); ++i) {
        SEXP li = VECTOR_ELT(l, i);
        if (!length(li)) continue;
        const int thisnrow = length(VECTOR_ELT(li, 0));
        if (thisnrow==0) continue;
        int w = usenames ? colMap[i*ncol + j] : j;
        if (w==-1) {
          writeNA(target, ansloc, thisnrow);
        } else {
          SEXP thisCol = VECTOR_ELT(li, w);
          bool coerced = false;
          if (TYPEOF(thisCol)!=TYPEOF(target)) {
            thisCol = PROTECT(coerceVector(thisCol, TYPEOF(target)));
            coerced = true;
          }
          switch(TYPEOF(target)) {
          case LGLSXP:
            memcpy(LOGICAL(target)+ansloc, LOGICAL(thisCol), thisnrow*SIZEOF(thisCol));
            break;
          case INTSXP:
            memcpy(INTEGER(target)+ansloc, INTEGER(thisCol), thisnrow*SIZEOF(thisCol));
            break;
          case REALSXP:
            // for integer 64 too, since types have been checked (TODO)
            memcpy(REAL(target)+ansloc, REAL(thisCol), thisnrow*SIZEOF(thisCol));
            break;
          case CPLXSXP :
            memcpy(COMPLEX(target)+ansloc, COMPLEX(thisCol), thisnrow*sizeof(Rcomplex));
            break;
          case VECSXP :
            for (int r=0; r<thisnrow; r++)
              SET_VECTOR_ELT(target, ansloc+r, VECTOR_ELT(thisCol,r));
            break;
          case STRSXP :
            for (int r=0; r<thisnrow; r++)
              SET_STRING_ELT(target, ansloc+r, STRING_ELT(thisCol,r));
            break;
          default :
            error("Unsupported column type '%s'", type2char(TYPEOF(target)));
          }
          if (coerced) UNPROTECT(1);
        }
        ansloc += thisnrow;
      }
    }

    // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
    // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
  }
  UNPROTECT(1);
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
/*
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
*/
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
