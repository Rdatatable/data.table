#include "data.table.h"
#include <Rdefines.h>
#include <ctype.h>   // for isdigit

SEXP rbindlist(SEXP l, SEXP usenamesArg, SEXP fillArg, SEXP idcolArg)
{
  if (!isLogical(fillArg) || LENGTH(fillArg) != 1 || LOGICAL(fillArg)[0] == NA_LOGICAL)
    error(_("fill= should be TRUE or FALSE"));
  if (!isLogical(usenamesArg) || LENGTH(usenamesArg)!=1)
    error(_("use.names= should be TRUE, FALSE, or not used (\"check\" by default)"));  // R levels converts "check" to NA
  if (!length(l)) return(l);
  if (TYPEOF(l) != VECSXP) error(_("Input to rbindlist must be a list. This list can contain data.tables, data.frames or plain lists."));
  Rboolean usenames = LOGICAL(usenamesArg)[0];
  const bool fill = LOGICAL(fillArg)[0];
  if (fill && usenames!=TRUE) {
    if (usenames==FALSE) warning(_("use.names= cannot be FALSE when fill is TRUE. Setting use.names=TRUE.")); // else no warning if usenames==NA (default)
    usenames=TRUE;
  }
  const bool idcol = !isNull(idcolArg);
  if (idcol && (!isString(idcolArg) || LENGTH(idcolArg)!=1)) error(_("Internal error: rbindlist.c idcol is not a single string"));  // # nocov
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
    if (TYPEOF(li) != VECSXP) error(_("Item %d of input is not a data.frame, data.table or list"), i+1);
    const int thisncol = length(li);
    if (!thisncol) continue;
    // delete as now more flexible ... if (fill && isNull(getAttrib(li, R_NamesSymbol))) error(_("When fill=TRUE every item of the input must have column names. Item %d does not."), i+1);
    if (fill) {
      if (thisncol>ncol) ncol=thisncol;  // this section initializes ncol with max ncol. ncol may be increased when usenames is accounted for further down
    } else {
      if (ncol==0) { ncol=thisncol; first=i; }
      else if (thisncol!=ncol) error(_("Item %d has %d columns, inconsistent with item %d which has %d columns. To fill missing columns use fill=TRUE."), i+1, thisncol, first+1, ncol);
    }
    int nNames = length(getAttrib(li, R_NamesSymbol));
    if (nNames>0 && nNames!=thisncol) error(_("Item %d has %d columns but %d column names. Invalid object."), i+1, thisncol, nNames);
    if (nNames>0) anyNames=true;
    upperBoundUniqueNames += nNames;
    int maxLen=0, whichMax=0;
    for (int j=0; j<thisncol; ++j) { int tt=length(VECTOR_ELT(li,j)); if (tt>maxLen) { maxLen=tt; whichMax=j; } }
    for (int j=0; j<thisncol; ++j) {
      int tt = length(VECTOR_ELT(li, j));
      if (tt>1 && tt!=maxLen) error(_("Column %d of item %d is length %d inconsistent with column %d which is length %d. Only length-1 columns are recycled."), j+1, i+1, tt, whichMax+1, maxLen);
      if (tt==0 && maxLen>0 && numZero++==0) { firstZeroCol = j; firstZeroItem=i; }
    }
    eachMax[i] = maxLen;
    nrow += maxLen;
  }
  if (numZero) {  // #1871
    SEXP names = getAttrib(VECTOR_ELT(l, firstZeroItem), R_NamesSymbol);
    const char *ch = names==R_NilValue ? "" : CHAR(STRING_ELT(names, firstZeroCol));
    warning(_("Column %d ['%s'] of item %d is length 0. This (and %d other%s like it) has been filled with NA (NULL for list columns) to make each item uniform."),
            firstZeroCol+1, ch, firstZeroItem+1, numZero-1, numZero==2?"":"s");
  }
  if (nrow==0 && ncol==0) return(R_NilValue);
  if (nrow>INT32_MAX) error(_("Total rows in the list is %"PRId64" which is larger than the maximum number of rows, currently %d"), (int64_t)nrow, INT32_MAX);
  if (usenames==TRUE && !anyNames) error(_("use.names=TRUE but no item of input list has any names"));

  int *colMap=NULL; // maps each column in final result to the column of each list item
  if (usenames==TRUE || usenames==NA_LOGICAL) {
    // here we proceed as if fill=true for brevity (accounting for dups is tricky) and then catch any missings after this branch
    // when use.names==NA we also proceed here as if use.names was TRUE to save new code and then check afterwards the map is 1:ncol for every item
    // first find number of unique column names present; i.e. length(unique(unlist(lapply(l,names))))
    SEXP *uniq = (SEXP *)malloc(upperBoundUniqueNames * sizeof(SEXP));  // upperBoundUniqueNames was initialized with 1 to ensure this is defined (otherwise 0 when no item has names)
    if (!uniq) error(_("Failed to allocate upper bound of %"PRId64" unique column names [sum(lapply(l,ncol))]"), (int64_t)upperBoundUniqueNames);
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
      // # nocov start
      for (int i=0; i<nuniq; ++i) SET_TRUELENGTH(uniq[i], 0);
      free(uniq); free(counts); free(maxdup);
      savetl_end();
      error(_("Failed to allocate nuniq=%d items working memory in rbindlist.c"), nuniq);
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
      error(_("Failed to allocate ncol=%d items working memory in rbindlist.c"), ncol);
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
    // for (int i=0; i<LENGTH(l); ++i) { for (int j=0; j<ncol; ++j) Rprintf(_("%2d "),colMap[i*ncol + j]);  Rprintf(_("\n")); }
  }

  if (fill && usenames==NA_LOGICAL) error(_("Internal error: usenames==NA but fill=TRUE. usenames should have been set to TRUE earlier with warning."));
  if (!fill && (usenames==TRUE || usenames==NA_LOGICAL)) {
    // Ensure no missings in both cases, and (when usenames==NA) all columns in same order too
    // We proceeded earlier as if fill was true, so varying ncol items will have missings here
    char buff[1001] = "";
    const char *extra = usenames==TRUE?"":_(" use.names='check' (default from v1.12.2) emits this message and proceeds as if use.names=FALSE for "\
                                            " backwards compatibility. See news item 5 in v1.12.2 for options to control this message.");
    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      if (!length(li) || !length(getAttrib(li, R_NamesSymbol))) continue;
      for (int j=0; j<ncol; ++j) {
        const int w = colMap[i*ncol + j];
        if (w==-1) {
          int missi = i;
          while (colMap[i*ncol + j]==-1 && i<LENGTH(l)) i++;
          if (i==LENGTH(l)) error(_("Internal error: could not find the first column name not present in earlier item"));
          SEXP s = getAttrib(VECTOR_ELT(l, i), R_NamesSymbol);
          int w2 = colMap[i*ncol + j];
          const char *str = isString(s) ? CHAR(STRING_ELT(s,w2)) : "";
          snprintf(buff, 1000, _("Column %d ['%s'] of item %d is missing in item %d. Use fill=TRUE to fill with NA (NULL for list columns), or use.names=FALSE to ignore column names.%s"),
                        w2+1, str, i+1, missi+1, extra );
          if (usenames==TRUE) error(buff);
          i = LENGTH(l); // break from outer i loop
          break;         // break from inner j loop
        }
        if (w!=j && usenames==NA_LOGICAL) {
          SEXP s = getAttrib(VECTOR_ELT(l, i), R_NamesSymbol);
          if (!isString(s) || i==0) error(_("Internal error: usenames==NA but an out-of-order name has been found in an item with no names or the first item. [%d]"), i);
          snprintf(buff, 1000, _("Column %d ['%s'] of item %d appears in position %d in item %d. Set use.names=TRUE to match by column name, or use.names=FALSE to ignore column names.%s"),
                               w+1, CHAR(STRING_ELT(s,w)), i+1, j+1, i, extra);
          i = LENGTH(l);
          break;
        }
      }
      if (buff[0]) {
        SEXP opt = GetOption(install("datatable.rbindlist.check"), R_NilValue);
        if (!isNull(opt) && !(isString(opt) && length(opt)==1)) {
          warning(_("options()$datatable.rbindlist.check is set but is not a single string. See news item 5 in v1.12.2."));
          opt = R_NilValue;
        }
        const char *o = isNull(opt) ? "message" : CHAR(STRING_ELT(opt,0));
        if      (strcmp(o,"message")==0) { eval(PROTECT(lang2(install("message"),PROTECT(ScalarString(mkChar(buff))))), R_GlobalEnv); UNPROTECT(2); }
        else if (strcmp(o,"warning")==0) warning(buff);
        else if (strcmp(o,"error")==0)   error(buff);
        else if (strcmp(o,"none")!=0)    warning(_("options()$datatable.rbindlist.check=='%s' which is not 'message'|'warning'|'error'|'none'. See news item 5 in v1.12.2."), o);
      }
    }
  }
  if (usenames==NA_LOGICAL) {
    usenames=FALSE;  // for backwards compatibility, see warning above which says this will change to TRUE in future
    ncol = length(VECTOR_ELT(l, first));  // ncol was increased as if fill=true, so reduce it back given fill=false (fill==false checked above)
  }

  int nprotect = 0;
  SEXP ans = PROTECT(allocVector(VECSXP, idcol + ncol)); nprotect++;
  SEXP ansNames;
  setAttrib(ans, R_NamesSymbol, ansNames=allocVector(STRSXP, idcol + ncol));
  if (idcol) {
    SET_STRING_ELT(ansNames, 0, STRING_ELT(idcolArg, 0));
    SEXP idval, listNames=getAttrib(l, R_NamesSymbol);
    if (length(listNames)) {
      SET_VECTOR_ELT(ans, 0, idval=allocVector(STRSXP, nrow));
      for (int i=0,ansloc=0; i<LENGTH(l); ++i) {
        SEXP li = VECTOR_ELT(l, i);
        if (!length(li)) continue;
        const int thisnrow = eachMax[i];
        SEXP thisname = STRING_ELT(listNames, i);
        for (int k=0; k<thisnrow; ++k) SET_STRING_ELT(idval, ansloc++, thisname);
      }
    } else {
      SET_VECTOR_ELT(ans, 0, idval=allocVector(INTSXP, nrow));
      int *idvald = INTEGER(idval);
      for (int i=0,ansloc=0; i<LENGTH(l); ++i) {
        SEXP li = VECTOR_ELT(l, i);
        if (!length(li)) continue;
        const int thisnrow = eachMax[i];
        for (int k=0; k<thisnrow; ++k) idvald[ansloc++] = i+1;
      }
    }
  }

  SEXP coercedForFactor = NULL;
  for(int j=0; j<ncol; ++j) {
    int maxType=LGLSXP;  // initialize with LGLSXP for test 2002.3 which has col x NULL in both lists to be filled with NA for #1871
    bool factor=false, orderedFactor=false;     // ordered factor is class c("ordered","factor"). isFactor() is true when isOrdered() is true.
    int longestLen=0, longestW=-1, longestI=-1; // just for ordered factor
    SEXP longestLevels=R_NilValue;              // just for ordered factor
    bool int64=false;
    const char *foundName=NULL;
    bool anyNotStringOrFactor=false;
    SEXP firstCol=R_NilValue;
    int firsti=-1, firstw=-1;
    for (int i=0; i<LENGTH(l); ++i) {
      SEXP li = VECTOR_ELT(l, i);
      if (!length(li)) continue;
      int w = usenames ? colMap[i*ncol + j] : j;  // colMap tells us which item to fetch for each of the final result columns, so we can stack column-by-column
      if (w==-1) continue;  // column j of final result has no input from this item (fill must be true)
      if (!foundName) {
        SEXP cn=PROTECT(getAttrib(li, R_NamesSymbol));
        if (length(cn)) { SEXP tt; SET_STRING_ELT(ansNames, idcol+j, tt=STRING_ELT(cn, w)); foundName=CHAR(tt); }
        UNPROTECT(1);
      }
      SEXP thisCol = VECTOR_ELT(li, w);
      int thisType = TYPEOF(thisCol);
      // Use >= for #546 -- TYPEORDER=0 for both LGLSXP and EXPRSXP (but also NULL)
      if (TYPEORDER(thisType)>=TYPEORDER(maxType) && !isNull(thisCol)) maxType=thisType;
      if (isFactor(thisCol)) {
        if (isNull(getAttrib(thisCol,R_LevelsSymbol))) error(_("Column %d of item %d has type 'factor' but has no levels; i.e. malformed."), w+1, i+1);
        factor = true;
        if (isOrdered(thisCol)) {
          orderedFactor = true;
          int thisLen = length(getAttrib(thisCol, R_LevelsSymbol));
          if (thisLen>longestLen) { longestLen=thisLen; longestLevels=getAttrib(thisCol, R_LevelsSymbol); /*for warnings later ...*/longestW=w; longestI=i; }
        }
      } else if (!isString(thisCol)) anyNotStringOrFactor=true;  // even for length 0 columns for consistency; test 2113.3
      if (INHERITS(thisCol, char_integer64)) {
        if (firsti>=0 && !length(getAttrib(firstCol, R_ClassSymbol))) { firsti=i; firstw=w; firstCol=thisCol; } // so the integer64 attribute gets copied to target below
        int64=true;
      }
      if (firsti==-1) { firsti=i; firstw=w; firstCol=thisCol; }
      else {
        if (!factor && !int64) {
          if (!R_compute_identical(PROTECT(getAttrib(thisCol, R_ClassSymbol)),
                                   PROTECT(getAttrib(firstCol, R_ClassSymbol)),
                                   0)) {
            error(_("Class attribute on column %d of item %d does not match with column %d of item %d."), w+1, i+1, firstw+1, firsti+1);
          }
          UNPROTECT(2);
        }
      }
    }

    if (!foundName) { static char buff[12]; snprintf(buff,12,"V%d",j+1), SET_STRING_ELT(ansNames, idcol+j, mkChar(buff)); foundName=buff; }
    if (factor) maxType=INTSXP;  // if any items are factors then a factor is created (could be an option)
    if (int64 && maxType!=REALSXP)
      error(_("Internal error: column %d of result is determined to be integer64 but maxType=='%s' != REALSXP"), j+1, type2char(maxType)); // # nocov
    SEXP target;
    SET_VECTOR_ELT(ans, idcol+j, target=allocVector(maxType, nrow));  // does not initialize logical & numerics, but does initialize character and list
    if (!factor) copyMostAttrib(firstCol, target); // all but names,dim and dimnames; mainly for class. And if so, we want a copy here, not keepattr's SET_ATTRIB.

    if (factor && anyNotStringOrFactor) {
      // in future warn, or use list column instead ... warning(_("Column %d contains a factor but not all items for the column are character or factor"), idcol+j+1);
      // some coercing from (likely) integer/numeric to character will be needed. But this coerce can feasibly fail with out-of-memory, so we have to do it up-front
      // before the savetl_init() because we have no hook to clean up tl if coerceVector fails.
      if (coercedForFactor==NULL) { coercedForFactor=PROTECT(allocVector(VECSXP, LENGTH(l))); nprotect++; }
      for (int i=0; i<LENGTH(l); ++i) {
        int w = usenames ? colMap[i*ncol + j] : j;
        if (w==-1) continue;
        SEXP thisCol = VECTOR_ELT(VECTOR_ELT(l, i), w);
        if (!isFactor(thisCol) && !isString(thisCol)) {
          SET_VECTOR_ELT(coercedForFactor, i, coerceVector(thisCol, STRSXP));
        }
      }
    }
    int ansloc=0;
    if (factor) {
      char warnStr[1000] = "";
      savetl_init();  // no error from now (or warning given options(warn=2)) until savetl_end
      int nLevel=0, allocLevel=0;
      SEXP *levelsRaw = NULL;  // growing list of SEXP pointers. Raw since managed with raw realloc.
      if (orderedFactor) {
        // If all sets of ordered levels are compatible (no ambiguities or conflicts) then an ordered factor is created, otherwise regular factor.
        // Currently the longest set of ordered levels is taken and all other ordered levels must be a compatible subset of that.
        // e.g. c( a<c<b, z<a<c<b, a<b ) => z<a<c<b  [ the longest is the middle one, and the other two are ordered subsets of it ]
        //      c( a<c<b, z<c<a<b, a<b ) => regular factor because it contains an ambiguity: is a<c or c<a?
        //      c( a<c<b, c<b, 'c,b'   ) => a<c<b  because the regular factor/character items c and b exist in the ordered levels
        //      c( a<c<b, c<b, 'c,d'   ) => a<c<b<d  'd' from non-ordered item added on the end of longest ordered levels
        //      c( a<c<b, c<b<d<e )  => regular factor because this case isn't yet implemented. a<c<b<d<e would be possible in future (extending longest at the beginning or end)
        const SEXP *sd = STRING_PTR(longestLevels);
        nLevel = allocLevel = longestLen;
        levelsRaw = (SEXP *)malloc(nLevel * sizeof(SEXP));
        if (!levelsRaw) { savetl_end(); error(_("Failed to allocate working memory for %d ordered factor levels of result column %d"), nLevel, idcol+j+1); }
        for (int k=0; k<longestLen; ++k) {
          SEXP s = sd[k];
          if (TRUELENGTH(s)>0) savetl(s);
          levelsRaw[k] = s;
          SET_TRUELENGTH(s,-k-1);
        }
        for (int i=0; i<LENGTH(l); ++i) {
          int w = usenames ? colMap[i*ncol + j] : j;
          if (w==-1) continue;
          SEXP thisCol = VECTOR_ELT(VECTOR_ELT(l, i), w);
          if (isOrdered(thisCol)) {
            SEXP levels = getAttrib(thisCol, R_LevelsSymbol);
            const SEXP *levelsD = STRING_PTR(levels);
            const int n = length(levels);
            for (int k=0, last=0; k<n; ++k) {
              SEXP s = levelsD[k];
              const int tl = TRUELENGTH(s);
              if (tl>=last) {  // if tl>=0 then also tl>=last because last<=0
                if (tl>=0) {
                  snprintf(warnStr, 1000,   // not direct warning as we're inside tl region
                  _("Column %d of item %d is an ordered factor but level %d ['%s'] is missing from the ordered levels from column %d of item %d. " \
                    "Each set of ordered factor levels should be an ordered subset of the first longest. A regular factor will be created for this column."),
                  w+1, i+1, k+1, CHAR(s), longestW+1, longestI+1);
                } else {
                  snprintf(warnStr, 1000,
                  _("Column %d of item %d is an ordered factor with '%s'<'%s' in its levels. But '%s'<'%s' in the ordered levels from column %d of item %d. " \
                    "A regular factor will be created for this column due to this ambiguity."),
                  w+1, i+1, CHAR(levelsD[k-1]), CHAR(s), CHAR(s), CHAR(levelsD[k-1]), longestW+1, longestI+1);
                  // k>=1 (so k-1 is ok) because when k==0 last==0 and this branch wouldn't happen
                }
                orderedFactor=false;
                i=LENGTH(l);  // break outer i loop
                break;        // break inner k loop
                // we leave the tl set for the longest levels; the regular factor will be created with the longest ordered levels first in case that useful for user
              }
              last = tl;  // negative ordinal; last should monotonically grow more negative if the levels are an ordered subset of the longest
            }
          }
        }
      }
      for (int i=0; i<LENGTH(l); ++i) {
        const int thisnrow = eachMax[i];
        SEXP li = VECTOR_ELT(l, i);
        if (!length(li)) continue;  // NULL items in the list() of DT/DF; not if thisnrow==0 because we need to retain (unused) factor levels (#3508)
        int w = usenames ? colMap[i*ncol + j] : j;
        if (w==-1) {
          writeNA(target, ansloc, thisnrow);
        } else {
          SEXP thisCol = VECTOR_ELT(li, w);
          SEXP thisColStr = isFactor(thisCol) ? getAttrib(thisCol, R_LevelsSymbol) : (isString(thisCol) ? thisCol : VECTOR_ELT(coercedForFactor, i));
          const int n = length(thisColStr);
          const SEXP *thisColStrD = STRING_PTR(thisColStr);  // D for data
          for (int k=0; k<n; ++k) {
            SEXP s = thisColStrD[k];
            if (s==NA_STRING ||             // remove NA from levels; test 1979 found by package emil when revdep testing 1.12.2 (#3473)
                TRUELENGTH(s)<0) continue;  // seen this level before; handles removing dups from levels as well as finding unique of character columns
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
                // C spec states that if realloc() fails (above) the original block (levelsRaw) is left untouched: it is not freed or moved. We ...
                for (int k=0; k<nLevel; k++) SET_TRUELENGTH(levelsRaw[k], 0);   // ... rely on that in this loop which uses levelsRaw.
                free(levelsRaw);
                savetl_end();
                error(_("Failed to allocate working memory for %d factor levels of result column %d when reading item %d of item %d"), allocLevel, idcol+j+1, w+1, i+1);
                // # nocov end
              }
              levelsRaw = tt;
            }
            SET_TRUELENGTH(s,-(++nLevel));
            levelsRaw[nLevel-1] = s;
          }
          int *targetd = INTEGER(target);
          if (isFactor(thisCol)) {
            const int *id = INTEGER(thisCol);
            if (length(thisCol)<=1) {
              // recycle length-1, or NA-fill length-0
              SEXP lev;
              const int val = (length(thisCol)==1 && id[0]!=NA_INTEGER && (lev=thisColStrD[id[0]-1])!=NA_STRING) ? -TRUELENGTH(lev) : NA_INTEGER;
              //                                                                                    ^^ #3915 and tests 2015.2-5
              for (int r=0; r<thisnrow; ++r) targetd[ansloc+r] = val;
            } else {
              // length(thisCol)==thisnrow alreay checked before this truelength-clobber region
              // If all i==truelength(i) then just do a memcpy since hop is identity. Otherwise hop via the integer map.
              bool hop = false;
              if (orderedFactor) {
                // retain the position of NA level (if any) and the integer mappings to it
                for (int k=0; k<n; ++k) {
                  SEXP s = thisColStrD[k];
                  if (s!=NA_STRING && -TRUELENGTH(s)!=k+1) { hop=true; break; }
                }
              } else {
                for (int k=0; k<n; ++k) {
                  SEXP s = thisColStrD[k];
                  if (s==NA_STRING || -TRUELENGTH(s)!=k+1) { hop=true; break; }
                }
              }
              if (hop) {
                if (orderedFactor) {
                  for (int r=0; r<thisnrow; ++r)
                    targetd[ansloc+r] = id[r]==NA_INTEGER ? NA_INTEGER : -TRUELENGTH(thisColStrD[id[r]-1]);
                } else {
                  for (int r=0; r<thisnrow; ++r) {
                    SEXP lev;
                    targetd[ansloc+r] = id[r]==NA_INTEGER || (lev=thisColStrD[id[r]-1])==NA_STRING ? NA_INTEGER : -TRUELENGTH(lev);
                  }
                }
              } else {
                memcpy(targetd+ansloc, id, thisnrow*SIZEOF(thisCol));
              }
            }
          } else {
            const SEXP *sd = STRING_PTR(thisColStr);
            if (length(thisCol)<=1) {
              const int val = (length(thisCol)==1 && sd[0]!=NA_STRING) ? -TRUELENGTH(sd[0]) : NA_INTEGER;
              for (int r=0; r<thisnrow; ++r) targetd[ansloc+r] = val;
            } else {
              for (int r=0; r<thisnrow; ++r) targetd[ansloc+r] = sd[r]==NA_STRING ? NA_INTEGER : -TRUELENGTH(sd[r]);
            }
          }
        }
        ansloc += thisnrow;
      }
      for (int k=0; k<nLevel; ++k) SET_TRUELENGTH(levelsRaw[k], 0);
      savetl_end();
      if (warnStr[0]) warning(warnStr);  // now savetl_end() has happened it's safe to call warning (could error if options(warn=2))
      SEXP levelsSxp;
      setAttrib(target, R_LevelsSymbol, levelsSxp=allocVector(STRSXP, nLevel));
      for (int k=0; k<nLevel; ++k) SET_STRING_ELT(levelsSxp, k, levelsRaw[k]);
      free(levelsRaw);
      if (orderedFactor) {
        SEXP tt;
        setAttrib(target, R_ClassSymbol, tt=allocVector(STRSXP, 2));
        SET_STRING_ELT(tt, 0, char_ordered);
        SET_STRING_ELT(tt, 1, char_factor);
      } else {
        setAttrib(target, R_ClassSymbol, ScalarString(char_factor));
      }
    } else {  // factor==false
      for (int i=0; i<LENGTH(l); ++i) {
        const int thisnrow = eachMax[i];
        if (thisnrow==0) continue;
        SEXP li = VECTOR_ELT(l, i);
        int w = usenames ? colMap[i*ncol + j] : j;
        SEXP thisCol;
        if (w==-1 || !length(thisCol=VECTOR_ELT(li, w))) {  // !length for zeroCol warning above; #1871
          writeNA(target, ansloc, thisnrow);  // writeNA is integer64 aware and writes INT64_MIN
        } else {
          if ((TYPEOF(target)==VECSXP || TYPEOF(target)==EXPRSXP) && TYPEOF(thisCol)!=TYPEOF(target)) {
            // do an as.list() on the atomic column; #3528
            thisCol = PROTECT(coerceVector(thisCol, TYPEOF(target))); nprotect++;
          }
          // else coerces if needed within memrecycle; with a no-alloc direct coerce from 1.12.4 (PR #3909)
          const char *ret = memrecycle(target, R_NilValue, ansloc, thisnrow, thisCol, 0, -1, idcol+j+1, foundName);
          if (ret) warning(_("Column %d of item %d: %s"), w+1, i+1, ret);
          // e.g. when precision is lost like assigning 3.4 to integer64; test 2007.2
          // TODO: but maxType should handle that and this should never warn
        }
        ansloc += thisnrow;
      }
    }
  }
  UNPROTECT(nprotect);  // ans, coercedForFactor, thisCol
  return(ans);
}
