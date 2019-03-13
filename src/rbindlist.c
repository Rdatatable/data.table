#include "data.table.h"
#include <Rdefines.h>

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
            // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
            // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
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
  }
  UNPROTECT(1);
  return(ans);
}

