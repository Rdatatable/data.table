#include "data.table.h"
#include <Rdefines.h>

SEXP rbindlist(SEXP l, SEXP usenamesArg, SEXP fillArg, SEXP idcolArg)
{
  if (!isLogical(fillArg) || LENGTH(fillArg) != 1 || LOGICAL(fillArg)[0] == NA_LOGICAL)
    error("fill= should be TRUE or FALSE");
  if (!isLogical(usenamesArg) || LENGTH(usenamesArg)!=1)
    error("use.names= should be TRUE, FALSE, or not used (\"check\" by default)");  // R levels converts "check" to NA
  if (!length(l)) return(l);
  if (TYPEOF(l) != VECSXP) error("Input to rbindlist must be a list. This list can contain data.tables, data.frames or plain lists.");
  Rboolean usenames = LOGICAL(usenamesArg)[0];
  const bool fill = LOGICAL(fillArg)[0];
  if (fill && usenames!=TRUE) {
    if (usenames==FALSE) warning("use.names= cannot be FALSE when fill is TRUE. Setting use.names=TRUE.\n"); // else no warning if usenames==NA (default)
    usenames=TRUE;
  }
  const bool idcol = !isNull(idcolArg);
  if (idcol && (!isString(idcolArg) || LENGTH(idcolArg)!=1)) error("Internal error: rbindlist.c idcol is not a single string");  // # nocov
  int ncol=0, first=0;
  int64_t nrow=0, upperBoundUniqueNames=1;
  bool anyNames=false;
  int numZero=0, firstZeroCol=0, firstZeroItem=0;
  int *eachMax = (int *)R_alloc(LENGTH(l), sizeof(int));
  // pre-check for any errors here to save having to get cleanup right below when usenames
  for (int i=0; i<LENGTH(l); i++) {  // length(l)>0 checked above
    eachMax[i] = 0;
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
    upperBoundUniqueNames += nNames;
    int maxLen=0, whichMax=0;
    for (int j=0; j<thisncol; ++j) { int tt=length(VECTOR_ELT(li,j)); if (tt>maxLen) { maxLen=tt; whichMax=j; } }
    for (int j=0; j<thisncol; ++j) {
      int tt = length(VECTOR_ELT(li, j));
      if (tt>1 && tt!=maxLen) error("Column %d of item %d is length %d inconsistent with column %d which is length %d. Only length-1 columns are recycled.", j+1, i+1, tt, whichMax+1, maxLen);
      if (tt==0 && maxLen>0 && numZero++==0) { firstZeroCol = j; firstZeroItem=i; }
    }
    eachMax[i] = maxLen;
    nrow += maxLen;
  }
  if (numZero) {  // #1871
    SEXP names = getAttrib(VECTOR_ELT(l, firstZeroItem), R_NamesSymbol);
    const char *ch = names==R_NilValue ? "" : CHAR(STRING_ELT(names, firstZeroCol));
    warning("Column %d ['%s'] of item %d is length 0. This (and %d other%s like it) has been filled with NA (NULL for list columns) to make each item uniform.",
            firstZeroCol+1, ch, firstZeroItem+1, numZero-1, numZero==2?"":"s");
  }
  if (nrow==0 && ncol==0) return(R_NilValue);
  if (nrow>INT32_MAX) error("Total rows in the list is %lld which is larger than the maximum number of rows, currently %d", nrow, INT32_MAX);
  if (usenames==TRUE && !anyNames) error("use.names=TRUE but no item of input list has any names");

  int *colMap=NULL; // maps each column in final result to the column of each list item
  if (usenames==TRUE || usenames==NA_LOGICAL) {
    // here we proceed as if fill=true for brevity (accounting for dups is tricky) and then catch any missings after this branch
    // when use.names==NA we also proceed here as if use.names was TRUE to save new code and then check afterwards the map is 1:ncol for every item
    // first find number of unique column names present; i.e. length(unique(unlist(lapply(l,names))))
    SEXP *uniq = (SEXP *)malloc(upperBoundUniqueNames * sizeof(SEXP));  // upperBoundUniqueNames was initialized with 1 to ensure this is defined (otherwise 0 when no item has names)
    if (!uniq) error("Failed to allocate upper bound of %lld unique column names [sum(lapply(l,ncol))]", upperBoundUniqueNames);
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
        uniq[nuniq++] = s;
        SET_TRUELENGTH(s,-nuniq);
      }
    }
    if (nuniq>0) {
      SEXP *tt = realloc(uniq, nuniq*sizeof(SEXP));  // shrink to only what we need to release the spare
      if (!tt) free(uniq);  // shrink never fails; just keep codacy happy
      uniq = tt;
    }
    // now count the dups (if any) and how they're distributed across the items
    int *counts = (int *)calloc(nuniq, sizeof(int)); // counts of names for each colnames
    int *maxdup = (int *)calloc(nuniq, sizeof(int)); // the most number of dups for any name within one colname vector
    if (!counts || !maxdup) {
      // # nocov begin
      for (int i=0; i<nuniq; ++i) SET_TRUELENGTH(uniq[i], 0);
      free(uniq); free(counts); free(maxdup);
      savetl_end();
      error("Failed to allocate nuniq=%d items working memory in rbindlist.c", nuniq);
      // # nocov end
    }
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
    free(maxdup); maxdup=NULL;  // not needed again
    // ncol is now the final number of columns accounting for unique and dups across all colnames
    // allocate a matrix:  nrows==length(list)  each entry contains which column to fetch for that final column

    int *colMapRaw = (int *)malloc(LENGTH(l)*ncol * sizeof(int));  // the result of this scope used later
    int *uniqMap = (int *)malloc(ncol * sizeof(int)); // maps the ith unique string to the first time it occurs in the final result
    int *dupLink = (int *)malloc(ncol * sizeof(int)); // if a colname has occurred before (a dup) links from the 1st to the 2nd time in the final result, 2nd to 3rd, etc
    if (!colMapRaw || !uniqMap || !dupLink) {
      // # nocov start
      for (int i=0; i<nuniq; ++i) SET_TRUELENGTH(uniq[i], 0);
      free(uniq); free(counts); free(colMapRaw); free(uniqMap); free(dupLink);
      savetl_end();
      error("Failed to allocate ncol=%d items working memory in rbindlist.c", ncol);
      // # nocov end
    }
    for (int i=0; i<LENGTH(l)*ncol; ++i) colMapRaw[i]=-1;   // 0-based so use -1
    for (int i=0; i<ncol; ++i) {uniqMap[i] = dupLink[i] = -1;}
    int nextCol=0, lastDup=ncol-1;

    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      int thisncol=length(li);
      if (thisncol==0) continue;
      const SEXP cn = getAttrib(li, R_NamesSymbol);
      if (!length(cn)) {
        for (int j=0; j<thisncol; j++) colMapRaw[i*ncol + j] = j;
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
          colMapRaw[i*ncol + uniqMap[w]] = j;
        }
      }
    }
    for (int i=0; i<nuniq; ++i) SET_TRUELENGTH(uniq[i], 0);  // zero out our usage of tl
    free(uniq); free(counts); free(uniqMap); free(dupLink);  // all local scope so no need to set to NULL
    savetl_end();  // restore R's usage

    // colMapRaw is still allocated. It was allocated with malloc because we needed to catch if the alloc failed.
    // move it to R's heap so it gets automatically free'd on exit, and on any error between now and the end of rbindlist.
    colMap = (int *)R_alloc(LENGTH(l)*ncol, sizeof(int));
    // This R_alloc could fail with out-of-memory but given it is very small it's very unlikely. If it does fail, colMapRaw will leak.
    //   But colMapRaw leaking now in this very rare situation is better than colMapRaw leaking in the more likely but still rare conditions later.
    //   And it's better than having to trap all exit point from here to the end of rbindlist, which may not be possible; e.g. writeNA() could error inside it with unsupported type.
    //   This very unlikely leak could be fixed by using an on.exit() at R level rbindlist(); R-exts$6.1.2 refers to pwilcox for example. However, that would not
    //   solve the (mere) leak if we ever call rbindlist internally from other C functions.
    memcpy(colMap, colMapRaw, LENGTH(l)*ncol*sizeof(int));
    free(colMapRaw);  // local scope in this branch to ensure can't be used below

    // to view map when debugging ...
    // for (int i=0; i<LENGTH(l); ++i) { for (int j=0; j<ncol; ++j) Rprintf("%2d ",colMap[i*ncol + j]);  Rprintf("\n"); }
  }

  if (fill && usenames==NA_LOGICAL) error("Internal error: usenames==NA but fill=TRUE. usenames should have been set to TRUE earlier with warning.");
  if (!fill && (usenames==TRUE || usenames==NA_LOGICAL)) {
    // Ensure no missings in both cases, and (when usenames==NA) all columns in same order too
    // We proceeded earlier as if fill was true, so varying ncol items will have missings here
    const char *warnStr = usenames==NA_LOGICAL?" use.names='check' (default from v1.12.2) generates this warning and proceeds as if use.names=FALSE for backwards compatibility; TRUE in future.":"";
    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      if (!length(li) || !length(getAttrib(li, R_NamesSymbol))) continue;
      for (int j=0; j<ncol; ++j) {
        const int w = colMap[i*ncol + j];
        if (w==-1) {
          int missi = i;
          while (colMap[i*ncol + j]==-1 && i<LENGTH(l)) i++;
          if (i==LENGTH(l)) error("Internal error: could not find the first column name not present in earlier item");
          SEXP s = getAttrib(VECTOR_ELT(l, i), R_NamesSymbol);
          int w2 = colMap[i*ncol + j];
          const char *str = isString(s) ? CHAR(STRING_ELT(s,w2)) : "";
          (usenames==TRUE ? error : warning)(
            "Column %d ['%s'] of item %d is missing in item %d. Use fill=TRUE to fill with NA (NULL for list columns), or use.names=FALSE to ignore column names.%s",
            w2+1, str, i+1, missi+1, warnStr );
          i = LENGTH(l); // break from outer i loop
          break;         // break from inner j loop
        }
        if (w!=j && usenames==NA_LOGICAL) {
          SEXP s = getAttrib(VECTOR_ELT(l, i), R_NamesSymbol);
          if (!isString(s) || i==0) error("Internal error: usenames==NA but an out-of-order name has been found in an item with no names or the first item. [%d]", i);
          warning("Column %d ['%s'] of item %d appears in position %d in item %d. Set use.names=TRUE to match by column name, or use.names=FALSE to ignore column names.%s",
                  w+1, CHAR(STRING_ELT(s,w)), i+1, j+1, i, warnStr);
          i = LENGTH(l);
          break;
        }
      }
    }
  }
  if (usenames==NA_LOGICAL) {
    usenames=FALSE;  // for backwards compatibility, see warning above which says this will change to TRUE in future
    ncol = length(VECTOR_ELT(l, first));  // ncol was increased as if fill=true, so reduce it back given fill=false (fill==false checked above)
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
    int maxType=LGLSXP;  // initialize with LGLSXP for test 2002.3 which has col x NULL in both lists to be filled with NA for #1871
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
      if (TYPEORDER(thisType)>TYPEORDER(maxType)) maxType=thisType;
      if (isFactor(thisCol)) {
        if (isNull(getAttrib(thisCol,R_LevelsSymbol))) error("Column %d of item %d has type 'factor' but has no levels; i.e. malformed.", w+1, i+1);
        factor=true;   // TODO isOrdered(thiscol) ? 2 : 1;
      } //else if (!isString(thisCol) && length(thisCol)) anyNotStringOrFactor=true;
      if (INHERITS(thisCol, char_integer64)) {
        if (firsti>=0 && !length(getAttrib(firstCol, R_ClassSymbol))) { firsti=i; firstw=w; firstCol=thisCol; } // so the integer64 attribute gets copied to target below
        int64=true;
      }
      if (firsti==-1) { firsti=i; firstw=w; firstCol=thisCol; }
      else if (!factor && !int64 && !R_compute_identical(getAttrib(thisCol, R_ClassSymbol), getAttrib(firstCol,R_ClassSymbol), 0)) {
        error("Class attribute on column %d of item %d does not match with column %d of item %d.", w+1, i+1, firstw+1, firsti+1);
      }
    }
    // in future warn ... if (factor && anyNotStringOrFactor) warning("Column %d contains a factor but not all items for the column are character or factor", idcol+j+1);
    if (!foundName) { char buff[12]; sprintf(buff,"V%d",j+1), SET_STRING_ELT(ansNames, idcol+j, mkChar(buff)); }
    if (factor) maxType=INTSXP;  // any items are factors then a factor is created (could be an option)
    if (int64 && maxType!=REALSXP)
      error("Internal error: column %d of result is determined to be integer64 but maxType=='%s' != REALSXP", j+1, type2char(maxType)); // # nocov
    SEXP target;
    SET_VECTOR_ELT(ans, idcol+j, target=allocVector(maxType, nrow));  // does not initialize logical & numerics, but does initialize character and list
    if (!factor) copyMostAttrib(firstCol, target); // all but names,dim and dimnames; mainly for class. And if so, we want a copy here, not keepattr's SET_ATTRIB.
    int ansloc=0;
    if (factor) {
      savetl_init();
      int nLevel=0, allocLevel=0;
      SEXP *levelsRaw = NULL;  // growing list of SEXP pointers. Raw since managed with raw realloc.
      for (int i=0; i<LENGTH(l); ++i) {
        const int thisnrow = eachMax[i];
        if (thisnrow==0) continue;
        SEXP li = VECTOR_ELT(l, i);
        int w = usenames ? colMap[i*ncol + j] : j;
        SEXP thisCol;
        if (w==-1 || !length(thisCol=VECTOR_ELT(li, w))) {  // !length for zeroCol warning above; #1871
          writeNA(target, ansloc, thisnrow);
        } else {
          bool coerced=false;
          SEXP thisColStr;
          if (isFactor(thisCol)) thisColStr = getAttrib(thisCol, R_LevelsSymbol);
          else if (isString(thisCol)) thisColStr = thisCol;
          else {
            thisColStr = PROTECT(coerceVector(thisCol, STRSXP));  // TODO this could fail and not clear-up tl; coerce first or create list column without coerce
            coerced=true;
          }
          const int n = length(thisColStr);
          const SEXP *thisColStrD = STRING_PTR(thisColStr);  // D for data
          for (int k=0; k<n; ++k) {
            SEXP s = thisColStrD[k];
            if (TRUELENGTH(s)<0) continue;  // seen this level before (handles finding unique within character columns too)
            if (TRUELENGTH(s)>0) savetl(s);
            if (allocLevel==nLevel) {       // including initial time when allocLevel==nLevel==0
              SEXP *tt = NULL;
              if (allocLevel<INT_MAX) {
                int64_t new = (int64_t)allocLevel+n-k+1024; // if all remaining levels in this item haven't been seen before, plus 1024 margin in case of many very short levels
                allocLevel = (new>(int64_t)INT_MAX) ? INT_MAX : (int)new;
                tt = (SEXP *)realloc(levelsRaw, allocLevel*sizeof(SEXP));  // first time levelsRaw==NULL and realloc==malloc in that case
              }
              if (tt==NULL) {
                // # nocov start
                // C spec states that if realloc() fails the original block is left untouched; it is not freed or moved. We ...
                for (int k=0; k<nLevel; k++) SET_TRUELENGTH(levelsRaw[k], 0);   // ... rely on that in this for loop which uses levelsRaw.
                free(levelsRaw);
                savetl_end();
                error("Failed to allocate working memory for %d factor levels of result column %d when reading item %d of item %d", allocLevel, idcol+j+1, w+1, i+1);
                // # nocov end
              }
              levelsRaw = tt;
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
        const int thisnrow = eachMax[i];
        if (thisnrow==0) continue;
        SEXP li = VECTOR_ELT(l, i);
        int w = usenames ? colMap[i*ncol + j] : j;
        SEXP thisCol;
        if (w==-1 || !length(thisCol=VECTOR_ELT(li, w))) {
          writeNA(target, ansloc, thisnrow);  // writeNA is integer64 aware and writes INT64_MIN
        } else {
          const char *ret = memrecycle(target, R_NilValue, ansloc, thisnrow, thisCol);  // coerces if needed within memrecycle; possibly with a no-alloc direct coerce
          if (ret) warning("Column %d of item %d: %s", w+1, i+1, ret);  // currently just one warning when precision is lost; e.g. assigning 3.4 to integer64
        }
        ansloc += thisnrow;
      }
    }
  }
  UNPROTECT(1);
  return(ans);
}

