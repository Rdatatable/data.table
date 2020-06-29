#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

// generate from 1 to n (a simple fun for melt, vecseq is convenient from R due to SEXP inputs)
SEXP seq_int(int n, int start) {
  if (n <= 0) return(R_NilValue);
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int *ians = INTEGER(ans);
  for (int i=0; i<n; ++i) ians[i] = start+i;
  UNPROTECT(1);
  return(ans);
}

// very specific "set_diff" for integers
SEXP set_diff(SEXP x, int n) {
  if (TYPEOF(x) != INTSXP) error(_("'x' must be an integer"));
  if (n <= 0) error(_("'n' must be a positive integer"));
  SEXP table = PROTECT(seq_int(n, 1));       // TODO: using match to 1:n seems odd here, why use match at all
  SEXP xmatch = PROTECT(match(x, table, 0)); // Old comment:took a while to realise: matches vec against x - thanks to comment from Matt in assign.c!
  const int *ixmatch = INTEGER(xmatch);
  int *buf = (int *) R_alloc(n, sizeof(int));
  int j=0;
  for (int i=0; i<n; ++i) {
    if (ixmatch[i] == 0) {
      buf[j++] = i+1;
    }
  }
  n = j;
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  if (n) memcpy(INTEGER(ans), buf, sizeof(int) * n); // sizeof is of type size_t - no integer overflow issues
  UNPROTECT(3);
  return(ans);
}

SEXP which(SEXP x, Rboolean val) {

  int j=0, n = length(x);
  SEXP ans;
  if (!isLogical(x)) error(_("Argument to 'which' must be logical"));
  const int *ix = LOGICAL(x);
  int *buf = (int *) R_alloc(n, sizeof(int));
  for (int i=0; i<n; ++i) {
    if (ix[i] == val) {
      buf[j++] = i+1;
    }
  }
  n = j;
  PROTECT(ans = allocVector(INTSXP, n));
  if (n) memcpy(INTEGER(ans), buf, sizeof(int) * n);

  UNPROTECT(1);
  return(ans);
}

// whichwrapper for R
SEXP whichwrapper(SEXP x, SEXP val) {
  // if (LOGICAL(val)[0] == NA_LOGICAL)
  //     error(_("val should be logical TRUE/FALSE"));
  return which(x, LOGICAL(val)[0]);
}

// hack by calling paste using eval. could change this to strcat, but not sure about buffer size for large data.tables... Any ideas Matthew?
SEXP concat(SEXP vec, SEXP idx) {

  SEXP s, t, v;
  int nidx=length(idx);

  if (TYPEOF(vec) != STRSXP) error(_("concat: 'vec must be a character vector"));
  if (!isInteger(idx) || length(idx) < 0) error(_("concat: 'idx' must be an integer vector of length >= 0"));
  const int *iidx = INTEGER(idx);
  for (int i=0; i<length(idx); ++i) {
    if (iidx[i] < 0 || iidx[i] > length(vec))
      error(_("Internal error in concat: 'idx' must take values between 0 and length(vec); 0 <= idx <= %d"), length(vec)); // # nocov
  }
  PROTECT(v = allocVector(STRSXP, nidx > 5 ? 5 : nidx));
  for (int i=0; i<length(v); ++i) {
    SET_STRING_ELT(v, i, STRING_ELT(vec, iidx[i]-1));
  }
  if (nidx > 5) SET_STRING_ELT(v, 4, mkChar("..."));
  PROTECT(t = s = allocList(3));
  SET_TYPEOF(t, LANGSXP);
  SETCAR(t, install("paste")); t = CDR(t);
  SETCAR(t, v); t = CDR(t);
  SETCAR(t, mkString(", "));
  SET_TAG(t, install("collapse"));
  UNPROTECT(2); // v, (t,s)
  return(eval(s, R_GlobalEnv));
}

// deal with measure.vars of type VECSXP
SEXP measurelist(SEXP measure, SEXP dtnames) {
  int i, n=length(measure), protecti=0;
  SEXP ans, tmp;
  ans = PROTECT(allocVector(VECSXP, n)); protecti++;
  for (i=0; i<n; i++) {
    switch(TYPEOF(VECTOR_ELT(measure, i))) {
      case STRSXP  : tmp = PROTECT(chmatch(VECTOR_ELT(measure, i), dtnames, 0)); protecti++; break;
      case REALSXP : tmp = PROTECT(coerceVector(VECTOR_ELT(measure, i), INTSXP)); protecti++; break;
      case INTSXP  : tmp = VECTOR_ELT(measure, i); break;
      default : error(_("Unknown 'measure.vars' type %s at index %d of list"), type2char(TYPEOF(VECTOR_ELT(measure, i))), i+1);
    }
    SET_VECTOR_ELT(ans, i, tmp);
  }
  UNPROTECT(protecti);
  return(ans);
}

// internal function just to unlist integer lists
static SEXP unlist_(SEXP xint) {
  int totn=0, n=length(xint);
  for (int i=0; i<n; ++i)
    totn += length(VECTOR_ELT(xint, i));
  SEXP ans = PROTECT(allocVector(INTSXP, totn));
  int *ians = INTEGER(ans), k=0;
  for (int i=0; i<n; ++i) {
    SEXP tmp = VECTOR_ELT(xint, i);
    const int *itmp = INTEGER(tmp), n2=length(tmp);
    for (int j=0; j<n2; ++j)
      ians[k++] = itmp[j];
  }
  UNPROTECT(1);
  return(ans);
}

SEXP checkVars(SEXP DT, SEXP id, SEXP measure, Rboolean verbose) {
  int i, ncol=LENGTH(DT), targetcols=0, protecti=0, u=0, v=0;
  SEXP thiscol, idcols = R_NilValue, valuecols = R_NilValue, tmp, tmp2, booltmp, unqtmp, ans;
  SEXP dtnames = PROTECT(getAttrib(DT, R_NamesSymbol)); protecti++;

  if (isNull(id) && isNull(measure)) {
    for (i=0; i<ncol; i++) {
      thiscol = VECTOR_ELT(DT, i);
      if ((isInteger(thiscol) || isNumeric(thiscol) || isLogical(thiscol)) && !isFactor(thiscol)) targetcols++;
    }
    idcols = PROTECT(allocVector(INTSXP, ncol-targetcols)); protecti++;
    tmp = PROTECT(allocVector(INTSXP, targetcols)); protecti++;
    for (i=0; i<ncol; i++) {
      thiscol = VECTOR_ELT(DT, i);
      if ((isInteger(thiscol) || isNumeric(thiscol) || isLogical(thiscol)) && !isFactor(thiscol)) {
        INTEGER(tmp)[u++] = i+1;
      } else
        INTEGER(idcols)[v++] = i+1;
    }
    valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(valuecols, 0, tmp);
    warning(_("id.vars and measure.vars are internally guessed when both are 'NULL'. All non-numeric/integer/logical type columns are considered id.vars, which in this case are columns [%s]. Consider providing at least one of 'id' or 'measure' vars in future."), CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
  } else if (!isNull(id) && isNull(measure)) {
    switch(TYPEOF(id)) {
      case STRSXP  : PROTECT(tmp = chmatch(id, dtnames, 0)); protecti++; break;
      case REALSXP : PROTECT(tmp = coerceVector(id, INTSXP)); protecti++; break;
      case INTSXP  : tmp = id; break;
      default : error(_("Unknown 'id.vars' type %s, must be character or integer vector"), type2char(TYPEOF(id)));
    }
    booltmp = PROTECT(duplicated(tmp, FALSE)); protecti++;
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error(_("One or more values in 'id.vars' is invalid."));
      else if (!LOGICAL(booltmp)[i]) targetcols++;
      else continue;
    }
    unqtmp = PROTECT(allocVector(INTSXP, targetcols)); protecti++;
    u = 0;
    for (i=0; i<length(booltmp); i++) {
      if (!LOGICAL(booltmp)[i]) {
        INTEGER(unqtmp)[u++] = INTEGER(tmp)[i];
      }
    }
    tmp2 = PROTECT(set_diff(unqtmp, ncol)); protecti++;
    valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(valuecols, 0, tmp2);
    idcols = tmp;
    if (verbose) {
      Rprintf(_("'measure.vars' is missing. Assigning all columns other than 'id.vars' columns as 'measure.vars'.\n"));
      if (length(tmp2)) Rprintf(_("Assigned 'measure.vars' are [%s].\n"), CHAR(STRING_ELT(concat(dtnames, tmp2), 0)));
    }
  } else if (isNull(id) && !isNull(measure)) {
    switch(TYPEOF(measure)) {
      case STRSXP  : tmp2 = PROTECT(chmatch(measure, dtnames, 0)); protecti++; break;
      case REALSXP : tmp2 = PROTECT(coerceVector(measure, INTSXP)); protecti++; break;
      case INTSXP  : tmp2 = measure; break;
      case VECSXP  : tmp2 = PROTECT(measurelist(measure, dtnames)); protecti++; break;
      default : error(_("Unknown 'measure.vars' type %s, must be character or integer vector/list"), type2char(TYPEOF(measure)));
    }
    tmp = tmp2;
    if (isNewList(measure)) {
      tmp = PROTECT(unlist_(tmp2)); protecti++;
    }
    booltmp = PROTECT(duplicated(tmp, FALSE)); protecti++;
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error(_("One or more values in 'measure.vars' is invalid."));
      else if (!LOGICAL(booltmp)[i]) targetcols++;
      else continue;
    }
    unqtmp = PROTECT(allocVector(INTSXP, targetcols)); protecti++;
    u = 0;
    for (i=0; i<length(booltmp); i++) {
      if (!LOGICAL(booltmp)[i]) {
        INTEGER(unqtmp)[u++] = INTEGER(tmp)[i];
      }
    }
    idcols = PROTECT(set_diff(unqtmp, ncol)); protecti++;
    if (isNewList(measure)) valuecols = tmp2;
    else {
      valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
      SET_VECTOR_ELT(valuecols, 0, tmp2);
    }
    if (verbose) {
      Rprintf(_("'id.vars' is missing. Assigning all columns other than 'measure.vars' columns as 'id.vars'.\n"));
      if (length(idcols)) Rprintf(_("Assigned 'id.vars' are [%s].\n"), CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
    }
  } else if (!isNull(id) && !isNull(measure)) {
    switch(TYPEOF(id)) {
      case STRSXP  : tmp = PROTECT(chmatch(id, dtnames, 0)); protecti++; break;
      case REALSXP : tmp = PROTECT(coerceVector(id, INTSXP)); protecti++; break;
      case INTSXP  : tmp = id; break;
      default : error(_("Unknown 'id.vars' type %s, must be character or integer vector"), type2char(TYPEOF(id)));
    }
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error(_("One or more values in 'id.vars' is invalid."));
    }
    idcols = PROTECT(tmp); protecti++;
    switch(TYPEOF(measure)) {
      case STRSXP  : tmp2 = PROTECT(chmatch(measure, dtnames, 0)); protecti++; break;
      case REALSXP : tmp2 = PROTECT(coerceVector(measure, INTSXP)); protecti++; break;
      case INTSXP  : tmp2 = measure; break;
      case VECSXP  : tmp2 = PROTECT(measurelist(measure, dtnames)); protecti++; break;
      default : error(_("Unknown 'measure.vars' type %s, must be character or integer vector"), type2char(TYPEOF(measure)));
    }
    tmp = tmp2;
    if (isNewList(measure)) {
      tmp = PROTECT(unlist_(tmp2)); protecti++;
    }
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error(_("One or more values in 'measure.vars' is invalid."));
    }
    if (isNewList(measure)) valuecols = tmp2;
    else {
      valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
      SET_VECTOR_ELT(valuecols, 0, tmp2);
    }
  }
  ans = PROTECT(allocVector(VECSXP, 2)); protecti++;
  SET_VECTOR_ELT(ans, 0, idcols);
  SET_VECTOR_ELT(ans, 1, valuecols);
  UNPROTECT(protecti);
  return(ans);
}

struct processData {
  SEXP RCHK;  // a 2 item list holding vars (result of checkVars) and naidx. PROTECTed up in fmelt so that preprocess() doesn't need to PROTECT. To pass rchk, #2865
  SEXP idcols, valuecols, naidx; // convenience pointers into RCHK[0][0], RCHK[0][1] and RCHK[1] respectively
  int lids, lvalues, lmax, lmin, totlen, nrow;
  int *isfactor, *leach, *isidentical;
  SEXPTYPE *maxtype;
  Rboolean narm;
};

static void preprocess(SEXP DT, SEXP id, SEXP measure, SEXP varnames, SEXP valnames, Rboolean narm, Rboolean verbose, struct processData *data) {

  SEXP vars,tmp,thiscol;
  SEXPTYPE type;
  int i,j;
  data->lmax = 0; data->lmin = 0; data->totlen = 0; data->nrow = length(VECTOR_ELT(DT, 0));
  SET_VECTOR_ELT(data->RCHK, 0, vars = checkVars(DT, id, measure, verbose));
  data->idcols = VECTOR_ELT(vars, 0);
  data->valuecols = VECTOR_ELT(vars, 1);
  data->lids = length(data->idcols);
  data->lvalues = length(data->valuecols);
  data->narm = narm;
  if (length(valnames) != data->lvalues) {
    if (isNewList(measure)) error(_("When 'measure.vars' is a list, 'value.name' must be a character vector of length =1 or =length(measure.vars)."));
    else error(_("When 'measure.vars' is either not specified or a character/integer vector, 'value.name' must be a character vector of length =1."));
  }
  if (length(varnames) != 1)
    error(_("'variable.name' must be a character/integer vector of length=1."));
  data->leach = (int *)R_alloc(data->lvalues, sizeof(int));
  data->isidentical = (int *)R_alloc(data->lvalues, sizeof(int));
  data->isfactor = (int *)R_alloc(data->lvalues, sizeof(int));
  data->maxtype = (SEXPTYPE *)R_alloc(data->lvalues, sizeof(SEXPTYPE));
  for (i=0; i<data->lvalues; i++) {
    tmp = VECTOR_ELT(data->valuecols, i);
    data->leach[i] = length(tmp);
    data->isidentical[i] = 1;  // TODO - why 1 and not Rboolean TRUE?
    data->isfactor[i] = 0;  // seems to hold 2 below, so not an Rboolean FALSE here. TODO - better name for variable?
    data->maxtype[i] = 0;   // R_alloc doesn't initialize so careful to here, relied on below
    data->lmax = (data->lmax > data->leach[i]) ? data->lmax : data->leach[i];
    data->lmin = (data->lmin < data->leach[i]) ? data->lmin : data->leach[i];
    for (j=0; j<data->leach[i]; j++) {
      thiscol = VECTOR_ELT(DT, INTEGER(tmp)[j]-1);
      if (isFactor(thiscol)) {
        data->isfactor[i] = (isOrdered(thiscol)) ? 2 : 1;
        data->maxtype[i]  = STRSXP;
      } else {
        type = TYPEOF(thiscol);
        if (type > data->maxtype[i]) data->maxtype[i] = type;
      }
    }
    for (j=0; j<data->leach[i]; j++) {
      thiscol = VECTOR_ELT(DT, INTEGER(tmp)[j]-1);
      if ( (!isFactor(thiscol) && data->maxtype[i] != TYPEOF(thiscol)) || (isFactor(thiscol) && data->maxtype[i] != STRSXP) ) {
        data->isidentical[i] = 0;
        break;
      }
    }
  }
  if (data->narm) {
    SET_VECTOR_ELT(data->RCHK, 1, data->naidx = allocVector(VECSXP, data->lmax));
  }
}

static SEXP combineFactorLevels(SEXP factorLevels, SEXP target, int * factorType, Rboolean * isRowOrdered)
// Finds unique levels directly in one pass with no need to create hash tables. Creates integer factor
// too in the same single pass. Previous version called factor(x, levels=unique) where x was type character
// and needed hash table.
// TODO keep the original factor columns as factor and use new technique in rbindlist.c. The calling
// environments are a little difference hence postponed for now (e.g. rbindlist calls writeNA which
// a general purpose combiner would need to know how many to write)
// factorType is 1 for factor and 2 for ordered
// will simply unique normal factors and attempt to find global order for ordered ones
{
  int maxlevels=0, nitem=length(factorLevels);
  for (int i=0; i<nitem; ++i) {
    SEXP this = VECTOR_ELT(factorLevels, i);
    if (!isString(this)) error(_("Internal error: combineFactorLevels in fmelt.c expects all-character input"));  // # nocov
    maxlevels+=length(this);
  }
  if (!isString(target)) error(_("Internal error: combineFactorLevels in fmelt.c expects a character target to factorize"));  // # nocov
  int nrow = length(target);
  SEXP ans = PROTECT(allocVector(INTSXP, nrow));
  SEXP *levelsRaw = (SEXP *)R_alloc(maxlevels, sizeof(SEXP));  // allocate for worst-case all-unique levels
  int *ansd = INTEGER(ans);
  const SEXP *targetd = STRING_PTR(target);
  savetl_init();
  // no alloc or any fail point until savetl_end()
  int nlevel=0;
  for (int i=0; i<nitem; ++i) {
    const SEXP this = VECTOR_ELT(factorLevels, i);
    const SEXP *thisd = STRING_PTR(this);
    const int thisn = length(this);
    for (int k=0; k<thisn; ++k) {
      SEXP s = thisd[k];
      if (s==NA_STRING) continue;  // NA shouldn't be in levels but remove it just in case
      int tl = TRUELENGTH(s);
      if (tl<0) continue;  // seen this level before
      if (tl>0) savetl(s);
      SET_TRUELENGTH(s,-(++nlevel));
      levelsRaw[nlevel-1] = s;
    }
  }
  for (int i=0; i<nrow; ++i) {
    if (targetd[i]==NA_STRING) {
      *ansd++ = NA_INTEGER;
    } else {
      int tl = TRUELENGTH(targetd[i]);
      *ansd++ = tl<0 ? -tl : NA_INTEGER;
    }
  }
  for (int i=0; i<nlevel; ++i) SET_TRUELENGTH(levelsRaw[i], 0);
  savetl_end();
  // now after savetl_end, we can alloc (which might fail)
  SEXP levelsSxp;
  setAttrib(ans, R_LevelsSymbol, levelsSxp=allocVector(STRSXP, nlevel));
  for (int i=0; i<nlevel; ++i) SET_STRING_ELT(levelsSxp, i, levelsRaw[i]);
  if (*factorType==2) {
    SEXP tt;
    setAttrib(ans, R_ClassSymbol, tt=allocVector(STRSXP, 2));
    SET_STRING_ELT(tt, 0, char_ordered);
    SET_STRING_ELT(tt, 1, char_factor);
  } else {
    setAttrib(ans, R_ClassSymbol, ScalarString(char_factor));
  }
  UNPROTECT(1);
  return ans;
}

SEXP getvaluecols(SEXP DT, SEXP dtnames, Rboolean valfactor, Rboolean verbose, struct processData *data) {
  for (int i=0; i<data->lvalues; ++i) {
    SEXP thisvaluecols = VECTOR_ELT(data->valuecols, i);
    if (!data->isidentical[i])
      warning(_("'measure.vars' [%s] are not all of the same type. By order of hierarchy, the molten data value column will be of type '%s'. All measure variables not of type '%s' will be coerced too. Check DETAILS in ?melt.data.table for more on coercion.\n"), CHAR(STRING_ELT(concat(dtnames, thisvaluecols), 0)), type2char(data->maxtype[i]), type2char(data->maxtype[i]));
    if (data->maxtype[i] == VECSXP && data->narm) {
      if (verbose) Rprintf(_("The molten data value type is a list at item %d. 'na.rm=TRUE' is ignored.\n"), i+1);
      data->narm = FALSE;
    }
  }
  if (data->narm) {
    SEXP seqcols = PROTECT(seq_int(data->lvalues, 1));
    for (int i=0; i<data->lmax; ++i) {
      SEXP tmp = PROTECT(allocVector(VECSXP, data->lvalues));
      for (int j=0; j<data->lvalues; ++j) {
        if (i < data->leach[j]) {
          SEXP thisvaluecols = VECTOR_ELT(data->valuecols, j);
          SET_VECTOR_ELT(tmp, j, VECTOR_ELT(DT, INTEGER(thisvaluecols)[i]-1));
        } else {
          SET_VECTOR_ELT(tmp, j, allocNAVector(data->maxtype[j], data->nrow));
        }
      }
      tmp = PROTECT(dt_na(tmp, seqcols));
      SEXP w;
      SET_VECTOR_ELT(data->naidx, i, w=which(tmp, FALSE));
      data->totlen += length(w);
      UNPROTECT(2); // tmp twice
    }
    UNPROTECT(1);  // seqcols
  } else {
    data->totlen = data->nrow * data->lmax;
  }
  SEXP flevels = PROTECT(allocVector(VECSXP, data->lmax));
  Rboolean *isordered = (Rboolean *)R_alloc(data->lmax, sizeof(Rboolean));
  SEXP ansvals = PROTECT(allocVector(VECSXP, data->lvalues));
  for (int i=0; i<data->lvalues; ++i) {
    bool thisvalfactor = (data->maxtype[i] == VECSXP) ? false : valfactor;
    SEXP target = PROTECT(allocVector(data->maxtype[i], data->totlen)); // to keep rchk happy
    SET_VECTOR_ELT(ansvals, i, target);
    UNPROTECT(1);  // still protected by virtue of being member of protected ansval.
    SEXP thisvaluecols = VECTOR_ELT(data->valuecols, i);
    int counter = 0;
    bool copyattr = false;
    for (int j=0; j<data->lmax; ++j) {
      int thisprotecti = 0;
      SEXP thiscol = (j < data->leach[i]) ? VECTOR_ELT(DT, INTEGER(thisvaluecols)[j]-1)
                       : allocNAVector(data->maxtype[i], data->nrow);
      if (!copyattr && data->isidentical[i] && !data->isfactor[i]) {
        copyMostAttrib(thiscol, target);
        copyattr = true;
      }
      if (TYPEOF(thiscol) != TYPEOF(target) && (data->maxtype[i] == VECSXP || !isFactor(thiscol))) {
        thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));  thisprotecti++;
      }
      const int *ithisidx = NULL;
      int thislen = 0;
      if (data->narm) {
        SEXP thisidx = VECTOR_ELT(data->naidx, j);
        ithisidx = INTEGER(thisidx);
        thislen = length(thisidx);
      }
      size_t size = SIZEOF(thiscol);
      switch (TYPEOF(target)) {
      case VECSXP :
        if (data->narm) {
          for (int k=0; k<thislen; ++k)
            SET_VECTOR_ELT(target, counter + k, VECTOR_ELT(thiscol, ithisidx[k]-1));
        } else {
          for (int k=0; k<data->nrow; ++k) SET_VECTOR_ELT(target, j*data->nrow + k, VECTOR_ELT(thiscol, k));
        }
        break;
      case STRSXP :
        if (data->isfactor[i]) {
          if (isFactor(thiscol)) {
            SET_VECTOR_ELT(flevels, j, getAttrib(thiscol, R_LevelsSymbol));
            thiscol = PROTECT(asCharacterFactor(thiscol));  thisprotecti++;
            isordered[j] = isOrdered(thiscol);
          } else SET_VECTOR_ELT(flevels, j, thiscol);
        }
        if (data->narm) {
          for (int k=0; k<thislen; ++k)
            SET_STRING_ELT(target, counter + k, STRING_ELT(thiscol, ithisidx[k]-1));
        } else {
          for (int k=0; k<data->nrow; ++k) SET_STRING_ELT(target, j*data->nrow + k, STRING_ELT(thiscol, k));
        }
        break;
      case REALSXP : {
        double *dtarget = REAL(target);
        const double *dthiscol = REAL(thiscol);
        if (data->narm) {
          for (int k=0; k<thislen; ++k)
            dtarget[counter + k] = dthiscol[ithisidx[k]-1];
        } else {
          memcpy(dtarget + j*data->nrow, dthiscol, data->nrow*size);
        }
      }
        break;
      case INTSXP :
      case LGLSXP : {
        int *itarget = INTEGER(target);
        const int *ithiscol = INTEGER(thiscol);
        if (data->narm) {
          for (int k=0; k<thislen; ++k)
            itarget[counter + k] = ithiscol[ithisidx[k]-1];
        } else {
          memcpy(itarget + j*data->nrow, ithiscol, data->nrow*size);
        }
      } break;
      default :
        error(_("Unknown column type '%s' for column '%s'."), type2char(TYPEOF(thiscol)), CHAR(STRING_ELT(dtnames, INTEGER(thisvaluecols)[i]-1)));
      }
      if (data->narm) counter += thislen;
      UNPROTECT(thisprotecti);  // inside inner loop (note that it's double loop) so as to limit use of protection stack
    }
    if (thisvalfactor && data->isfactor[i] && TYPEOF(target) != VECSXP) {
      //SEXP clevels = PROTECT(combineFactorLevels(flevels, &(data->isfactor[i]), isordered));
      //SEXP factorLangSxp = PROTECT(lang3(install(data->isfactor[i] == 1 ? "factor" : "ordered"), target, clevels));
      //SET_VECTOR_ELT(ansvals, i, eval(factorLangSxp, R_GlobalEnv));
      //UNPROTECT(2);  // clevels, factorLangSxp
      SET_VECTOR_ELT(ansvals, i, combineFactorLevels(flevels, target, &(data->isfactor[i]), isordered));
    }
  }
  UNPROTECT(2);  // flevels, ansvals. Not using two protection counters (protecti and thisprotecti) to keep rchk happy.
  return(ansvals);
}

SEXP getvarcols(SEXP DT, SEXP dtnames, Rboolean varfactor, Rboolean verbose, struct processData *data) {
  // reworked in PR#3455 to create character/factor directly for efficiency, and handle duplicates (#1754)
  // data->nrow * data->lmax == data->totlen
  int protecti=0;
  SEXP ansvars=PROTECT(allocVector(VECSXP, 1)); protecti++;
  SEXP target;
  if (data->lvalues==1 && length(VECTOR_ELT(data->valuecols, 0)) != data->lmax)
    error(_("Internal error: fmelt.c:getvarcols %d %d"), length(VECTOR_ELT(data->valuecols, 0)), data->lmax);  // # nocov
  if (!varfactor) {
    SET_VECTOR_ELT(ansvars, 0, target=allocVector(STRSXP, data->totlen));
    if (data->lvalues == 1) {
      const int *thisvaluecols = INTEGER(VECTOR_ELT(data->valuecols, 0));
      for (int j=0, ansloc=0; j<data->lmax; ++j) {
        const int thislen = data->narm ? length(VECTOR_ELT(data->naidx, j)) : data->nrow;
        SEXP str = STRING_ELT(dtnames, thisvaluecols[j]-1);
        for (int k=0; k<thislen; ++k) SET_STRING_ELT(target, ansloc++, str);
      }
    } else {
      for (int j=0, ansloc=0, level=1; j<data->lmax; ++j) {
        const int thislen = data->narm ? length(VECTOR_ELT(data->naidx, j)) : data->nrow;
        if (thislen==0) continue;  // so as not to bump level
        char buff[20];
        snprintf(buff, 20, "%d", level++);
        SEXP str = PROTECT(mkChar(buff));
        for (int k=0; k<thislen; ++k) SET_STRING_ELT(target, ansloc++, str);
        UNPROTECT(1);
      }
    }
  } else {
    SET_VECTOR_ELT(ansvars, 0, target=allocVector(INTSXP, data->totlen));
    SEXP levels;
    int *td = INTEGER(target);
    if (data->lvalues == 1) {
      SEXP thisvaluecols = VECTOR_ELT(data->valuecols, 0);
      int len = length(thisvaluecols);
      levels = PROTECT(allocVector(STRSXP, len)); protecti++;
      const int *vd = INTEGER(thisvaluecols);
      for (int j=0; j<len; ++j) SET_STRING_ELT(levels, j, STRING_ELT(dtnames, vd[j]-1));
      SEXP m = PROTECT(chmatch(levels, levels, 0)); protecti++;  // do we have any dups?
      int numRemove = 0;  // remove dups and any for which narm and all-NA
      int *md = INTEGER(m);
      for (int j=0; j<len; ++j) {
        if (md[j]!=j+1 /*dup*/ || (data->narm && length(VECTOR_ELT(data->naidx, j))==0)) { numRemove++; md[j]=0; }
      }
      if (numRemove) {
        SEXP newlevels = PROTECT(allocVector(STRSXP, len-numRemove)); protecti++;
        for (int i=0, loc=0; i<len; ++i) if (md[i]!=0) { SET_STRING_ELT(newlevels, loc++, STRING_ELT(levels, i)); }
        m = PROTECT(chmatch(levels, newlevels, 0)); protecti++;  // budge up the gaps
        md = INTEGER(m);
        levels = newlevels;
      }
      for (int j=0, ansloc=0; j<data->lmax; ++j) {
        const int thislen = data->narm ? length(VECTOR_ELT(data->naidx, j)) : data->nrow;
        for (int k=0; k<thislen; ++k) td[ansloc++] = md[j];
      }
    } else {
      int nlevel=0;
      levels = PROTECT(allocVector(STRSXP, data->lmax)); protecti++;
      for (int j=0, ansloc=0; j<data->lmax; ++j) {
        const int thislen = data->narm ? length(VECTOR_ELT(data->naidx, j)) : data->nrow;
        if (thislen==0) continue;  // so as not to bump level
        char buff[20];
        snprintf(buff, 20, "%d", nlevel+1);
        SET_STRING_ELT(levels, nlevel++, mkChar(buff));  // generate levels = 1:nlevels
        for (int k=0; k<thislen; ++k) td[ansloc++] = nlevel;
      }
      if (nlevel < data->lmax) {
        // data->narm is true and there are some all-NA items causing at least one 'if (thislen==0) continue' above
        // shrink the levels
        SEXP newlevels = PROTECT(allocVector(STRSXP, nlevel)); protecti++;
        for (int i=0; i<nlevel; ++i) SET_STRING_ELT(newlevels, i, STRING_ELT(levels, i));
        levels = newlevels;
      }
    }
    setAttrib(target, R_LevelsSymbol, levels);
    setAttrib(target, R_ClassSymbol, ScalarString(char_factor));
  }
  UNPROTECT(protecti);
  return(ansvars);
}

SEXP getidcols(SEXP DT, SEXP dtnames, Rboolean verbose, struct processData *data) {
  SEXP ansids = PROTECT(allocVector(VECSXP, data->lids));
  for (int i=0; i<data->lids; ++i) {
    int counter = 0;
    SEXP thiscol = VECTOR_ELT(DT, INTEGER(data->idcols)[i]-1);
    size_t size = SIZEOF(thiscol);
    SEXP target;
    SET_VECTOR_ELT(ansids, i, target=allocVector(TYPEOF(thiscol), data->totlen) );
    copyMostAttrib(thiscol, target); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
    switch(TYPEOF(thiscol)) {
    case REALSXP : {
      double *dtarget = REAL(target);
      const double *dthiscol = REAL(thiscol);
      if (data->narm) {
        for (int j=0; j<data->lmax; ++j) {
          SEXP thisidx = VECTOR_ELT(data->naidx, j);
          const int *ithisidx = INTEGER(thisidx);
          const int thislen = length(thisidx);
          for (int k=0; k<thislen; ++k)
            dtarget[counter + k] = dthiscol[ithisidx[k]-1];
          counter += thislen;
        }
      } else {
        for (int j=0; j<data->lmax; ++j)
          memcpy(dtarget + j*data->nrow, dthiscol, data->nrow*size);
      }
    }
      break;
    case INTSXP :
    case LGLSXP : {
      int *itarget = INTEGER(target);
      const int *ithiscol = INTEGER(thiscol);
      if (data->narm) {
        for (int j=0; j<data->lmax; ++j) {
          SEXP thisidx = VECTOR_ELT(data->naidx, j);
          const int *ithisidx = INTEGER(thisidx);
          const int thislen = length(thisidx);
          for (int k=0; k<thislen; ++k)
            itarget[counter + k] = ithiscol[ithisidx[k]-1];
          counter += thislen;
        }
      } else {
        for (int j=0; j<data->lmax; ++j)
          memcpy(itarget + j*data->nrow, ithiscol, data->nrow*size);
      }
    } break;
    case STRSXP : {
      if (data->narm) {
        for (int j=0; j<data->lmax; ++j) {
          SEXP thisidx = VECTOR_ELT(data->naidx, j);
          const int *ithisidx = INTEGER(thisidx);
          const int thislen = length(thisidx);
          for (int k=0; k<thislen; ++k)
            SET_STRING_ELT(target, counter + k, STRING_ELT(thiscol, ithisidx[k]-1));
          counter += thislen;
        }
      } else {
        const SEXP *s = STRING_PTR(thiscol);  // to reduce overhead of STRING_ELT() inside loop below. Read-only hence const.
        for (int j=0; j<data->lmax; ++j) {
          for (int k=0; k<data->nrow; ++k) {
            SET_STRING_ELT(target, j*data->nrow + k, s[k]);
          }
        }
      }
    }
      break;
    case VECSXP : {
      for (int j=0; j<data->lmax; ++j) {
        for (int k=0; k<data->nrow; ++k) {
          SET_VECTOR_ELT(target, j*data->nrow + k, VECTOR_ELT(thiscol, k));
        }
      }
    }
      break;
    default : error(_("Unknown column type '%s' for column '%s' in 'data'"), type2char(TYPEOF(thiscol)), CHAR(STRING_ELT(dtnames, INTEGER(data->idcols)[i]-1)));
    }
  }
  UNPROTECT(1);
  return (ansids);
}

SEXP fmelt(SEXP DT, SEXP id, SEXP measure, SEXP varfactor, SEXP valfactor, SEXP varnames, SEXP valnames, SEXP narmArg, SEXP verboseArg) {
  SEXP dtnames, ansvals, ansvars, ansids, ansnames, ans;
  Rboolean narm=FALSE, verbose=FALSE;

  if (!isNewList(DT)) error(_("Input is not of type VECSXP, expected a data.table, data.frame or list"));
  if (!isLogical(valfactor)) error(_("Argument 'value.factor' should be logical TRUE/FALSE"));
  if (!isLogical(varfactor)) error(_("Argument 'variable.factor' should be logical TRUE/FALSE"));
  if (!isLogical(narmArg)) error(_("Argument 'na.rm' should be logical TRUE/FALSE."));
  if (!isString(varnames)) error(_("Argument 'variable.name' must be a character vector"));
  if (!isString(valnames)) error(_("Argument 'value.name' must be a character vector"));
  if (!isLogical(verboseArg)) error(_("Argument 'verbose' should be logical TRUE/FALSE"));
  if (LOGICAL(verboseArg)[0] == TRUE) verbose = TRUE;
  int ncol = LENGTH(DT);
  if (!ncol) {
    if (verbose) Rprintf(_("ncol(data) is 0. Nothing to melt. Returning original data.table."));
    return(DT);
  }
  int protecti=0;
  dtnames = PROTECT(getAttrib(DT, R_NamesSymbol)); protecti++;
  if (isNull(dtnames)) error(_("names(data) is NULL. Please report to data.table-help"));
  if (LOGICAL(narmArg)[0] == TRUE) narm = TRUE;
  if (LOGICAL(verboseArg)[0] == TRUE) verbose = TRUE;
  struct processData data;
  data.RCHK = PROTECT(allocVector(VECSXP, 2)); protecti++;
  preprocess(DT, id, measure, varnames, valnames, narm, verbose, &data);
  // edge case no measure.vars
  if (!data.lmax) {
    SEXP tt = PROTECT(shallowwrapper(DT, data.idcols)); protecti++;
    ans = PROTECT(copyAsPlain(tt)); protecti++;
  } else {
    ansvals = PROTECT(getvaluecols(DT, dtnames, LOGICAL(valfactor)[0], verbose, &data)); protecti++;
    ansvars = PROTECT(getvarcols(DT, dtnames, LOGICAL(varfactor)[0], verbose, &data)); protecti++;
    ansids  = PROTECT(getidcols(DT, dtnames, verbose, &data)); protecti++;

    // populate 'ans'
    ans = PROTECT(allocVector(VECSXP, data.lids+1+data.lvalues)); protecti++; // 1 is for variable column
    for (int i=0; i<data.lids; i++) {
      SET_VECTOR_ELT(ans, i, VECTOR_ELT(ansids, i));
    }
    SET_VECTOR_ELT(ans, data.lids, VECTOR_ELT(ansvars, 0));
    for (int i=0; i<data.lvalues; i++) {
      SET_VECTOR_ELT(ans, data.lids+1+i, VECTOR_ELT(ansvals, i));
    }
    // fill in 'ansnames'
    ansnames = PROTECT(allocVector(STRSXP, data.lids+1+data.lvalues)); protecti++;
    for (int i=0; i<data.lids; i++) {
      SET_STRING_ELT(ansnames, i, STRING_ELT(dtnames, INTEGER(data.idcols)[i]-1));
    }
    SET_STRING_ELT(ansnames, data.lids, STRING_ELT(varnames, 0));
    for (int i=0; i<data.lvalues; i++) {
      SET_STRING_ELT(ansnames, data.lids+1+i, STRING_ELT(valnames, i));
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
  }
  UNPROTECT(protecti);
  return(ans);
}
