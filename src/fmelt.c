#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

// generate from 1 to n (a simple fun for melt, vecseq is convenient from R due to SEXP inputs)
SEXP seq_int(int n, int start) {
  if (n <= 0) return(R_NilValue);
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  for (int i=0; i<n; i++) INTEGER(ans)[i] = start+i;
  UNPROTECT(1);
  return(ans);
}

// very specific "set_diff" for integers
SEXP set_diff(SEXP x, int n) {
  if (TYPEOF(x) != INTSXP) error("'x' must be an integer");
  if (n <= 0) error("'n' must be a positive integer");
  SEXP table = PROTECT(seq_int(n, 1));       // TODO: using match to 1:n seems odd here, why use match at all
  SEXP xmatch = PROTECT(match(x, table, 0)); // Old comment:took a while to realise: matches vec against x - thanks to comment from Matt in assign.c!
  int *buf = (int *) R_alloc(n, sizeof(int));
  int j=0;
  for (int i=0; i<n; i++) {
    if (INTEGER(xmatch)[i] == 0) {
      buf[j++] = i+1;
    }
  }
  n = j;
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  if (n) memcpy(INTEGER(ans), buf, sizeof(int) * n); // sizeof is of type size_t - no integer overflow issues
  UNPROTECT(3);
  return(ans);
}

// plucked and modified from base (coerce.c and summary.c).
// for melt's `na.rm=TRUE` option
SEXP which_notNA(SEXP x) {
  SEXP v, ans;
  int i, j=0, n = length(x);

  PROTECT(v = allocVector(LGLSXP, n));
  switch (TYPEOF(x)) {
  case LGLSXP:
    for (i = 0; i < n; i++) LOGICAL(v)[i] = (LOGICAL(x)[i] != NA_LOGICAL);
    break;
  case INTSXP:
    for (i = 0; i < n; i++) LOGICAL(v)[i] = (INTEGER(x)[i] != NA_INTEGER);
    break;
  case REALSXP:
    for (i = 0; i < n; i++) LOGICAL(v)[i] = !ISNAN(REAL(x)[i]);
    break;
  case STRSXP:
    for (i = 0; i < n; i++) LOGICAL(v)[i] = (STRING_ELT(x, i) != NA_STRING);
    break;
  default:
    error("%s() applied to non-(list or vector) of type '%s'",
      "which_notNA", type2char(TYPEOF(x)));
  }

  int *buf = (int *) R_alloc(n, sizeof(int));
  for (i = 0; i < n; i++) {
    if (LOGICAL(v)[i] == TRUE) {
      buf[j] = i + 1;
      j++;
    }
  }
  n = j;
  PROTECT(ans = allocVector(INTSXP, n));
  if (n) memcpy(INTEGER(ans), buf, sizeof(int) * n);

  UNPROTECT(2);
  return(ans);
}

SEXP which(SEXP x, Rboolean val) {

  int i, j=0, n = length(x);
  SEXP ans;
  if (!isLogical(x)) error("Argument to 'which' must be logical");
  int *buf = (int *) R_alloc(n, sizeof(int));
  for (i = 0; i < n; i++) {
    if (LOGICAL(x)[i] == val) {
      buf[j] = i + 1;
      j++;
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
  //     error("val should be logical TRUE/FALSE");
  return which(x, LOGICAL(val)[0]);
}

// hack by calling paste using eval. could change this to strcat, but not sure about buffer size for large data.tables... Any ideas Matthew?
SEXP concat(SEXP vec, SEXP idx) {

  SEXP s, t, v;
  int i, nidx=length(idx);

  if (TYPEOF(vec) != STRSXP) error("concat: 'vec must be a character vector");
  if (!isInteger(idx) || length(idx) < 0) error("concat: 'idx' must be an integer vector of length >= 0");
  for (i=0; i<length(idx); i++) {
    if (INTEGER(idx)[i] < 0 || INTEGER(idx)[i] > length(vec))
      error("concat: 'idx' must take values between 0 and length(vec); 0 <= idx <= length(vec)");
  }
  PROTECT(v = allocVector(STRSXP, nidx > 5 ? 5 : nidx));
  for (i=0; i<length(v); i++) {
    SET_STRING_ELT(v, i, STRING_ELT(vec, INTEGER(idx)[i]-1));
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
      case STRSXP  : tmp = PROTECT(chmatch(VECTOR_ELT(measure, i), dtnames, 0, FALSE)); protecti++; break;
      case REALSXP : tmp = PROTECT(coerceVector(VECTOR_ELT(measure, i), INTSXP)); protecti++; break;
      case INTSXP  : tmp = VECTOR_ELT(measure, i); break;
      default : error("Unknown 'measure.vars' type %s at index %d of list", type2char(TYPEOF(VECTOR_ELT(measure, i))), i+1);
    }
    SET_VECTOR_ELT(ans, i, tmp);
  }
  UNPROTECT(protecti);
  return(ans);
}

// internal function just to unlist integer lists
static SEXP unlist_(SEXP xint) {
  int i,j,k=0,totn=0,n=length(xint);
  SEXP ans, tmp;
  for (i=0; i<n; i++)
    totn += length(VECTOR_ELT(xint, i));
  ans = PROTECT(allocVector(INTSXP, totn));
  for (i=0; i<n; i++) {
    tmp = VECTOR_ELT(xint, i);
    for (j=0; j<length(tmp); j++)
      INTEGER(ans)[k++] = INTEGER(tmp)[j];
  }
  UNPROTECT(1);
  return(ans);
}

SEXP checkVars(SEXP DT, SEXP id, SEXP measure, Rboolean verbose) {
  int i, ncol=LENGTH(DT), targetcols=0, protecti=0, u=0, v=0;
  SEXP thiscol, idcols = R_NilValue, valuecols = R_NilValue, tmp, tmp2, booltmp, unqtmp, ans;
  SEXP dtnames = PROTECT(getAttrib(DT, R_NamesSymbol));  // PROTECT for rchk
  protecti++;

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
    warning("To be consistent with reshape2's melt, id.vars and measure.vars are internally guessed when both are 'NULL'. All non-numeric/integer/logical type columns are considered id.vars, which in this case are columns [%s]. Consider providing at least one of 'id' or 'measure' vars in future.", CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
  } else if (!isNull(id) && isNull(measure)) {
    switch(TYPEOF(id)) {
      case STRSXP  : PROTECT(tmp = chmatch(id, dtnames, 0, FALSE)); protecti++; break;
      case REALSXP : PROTECT(tmp = coerceVector(id, INTSXP)); protecti++; break;
      case INTSXP  : tmp = id; break;
      default : error("Unknown 'id.vars' type %s, must be character or integer vector", type2char(TYPEOF(id)));
    }
    booltmp = PROTECT(duplicated(tmp, FALSE)); protecti++;
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error("One or more values in 'id.vars' is invalid.");
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
      Rprintf("'measure.vars' is missing. Assigning all columns other than 'id.vars' columns as 'measure.vars'.\n");
      if (length(tmp2)) Rprintf("Assigned 'measure.vars' are [%s].\n", CHAR(STRING_ELT(concat(dtnames, tmp2), 0)));
    }
  } else if (isNull(id) && !isNull(measure)) {
    switch(TYPEOF(measure)) {
      case STRSXP  : tmp2 = PROTECT(chmatch(measure, dtnames, 0, FALSE)); protecti++; break;
      case REALSXP : tmp2 = PROTECT(coerceVector(measure, INTSXP)); protecti++; break;
      case INTSXP  : tmp2 = measure; break;
      case VECSXP  : tmp2 = PROTECT(measurelist(measure, dtnames)); protecti++; break;
      default : error("Unknown 'measure.vars' type %s, must be character or integer vector/list", type2char(TYPEOF(measure)));
    }
    tmp = tmp2;
    if (isNewList(measure)) {
      tmp = PROTECT(unlist_(tmp2)); protecti++;
    }
    booltmp = PROTECT(duplicated(tmp, FALSE)); protecti++;
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error("One or more values in 'measure.vars' is invalid.");
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
      Rprintf("'id.vars' is missing. Assigning all columns other than 'measure.vars' columns as 'id.vars'.\n");
      if (length(idcols)) Rprintf("Assigned 'id.vars' are [%s].\n", CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
    }
  } else if (!isNull(id) && !isNull(measure)) {
    switch(TYPEOF(id)) {
      case STRSXP  : tmp = PROTECT(chmatch(id, dtnames, 0, FALSE)); protecti++; break;
      case REALSXP : tmp = PROTECT(coerceVector(id, INTSXP)); protecti++; break;
      case INTSXP  : tmp = id; break;
      default : error("Unknown 'id.vars' type %s, must be character or integer vector", type2char(TYPEOF(id)));
    }
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error("One or more values in 'id.vars' is invalid.");
    }
    idcols = PROTECT(tmp); protecti++;
    switch(TYPEOF(measure)) {
      case STRSXP  : tmp2 = PROTECT(chmatch(measure, dtnames, 0, FALSE)); protecti++; break;
      case REALSXP : tmp2 = PROTECT(coerceVector(measure, INTSXP)); protecti++; break;
      case INTSXP  : tmp2 = measure; break;
      case VECSXP  : tmp2 = PROTECT(measurelist(measure, dtnames)); protecti++; break;
      default : error("Unknown 'measure.vars' type %s, must be character or integer vector", type2char(TYPEOF(measure)));
    }
    tmp = tmp2;
    if (isNewList(measure)) {
      tmp = PROTECT(unlist_(tmp2)); protecti++;
    }
    for (i=0; i<length(tmp); i++) {
      if (INTEGER(tmp)[i] <= 0 || INTEGER(tmp)[i] > ncol)
        error("One or more values in 'measure.vars' is invalid.");
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
    if (isNewList(measure)) error("When 'measure.vars' is a list, 'value.name' must be a character vector of length =1 or =length(measure.vars).");
    else error("When 'measure.vars' is either not specified or a character/integer vector, 'value.name' must be a character vector of length =1.");
  }
  if (length(varnames) != 1)
    error("'variable.name' must be a character/integer vector of length=1.");
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

SEXP getvaluecols(SEXP DT, SEXP dtnames, Rboolean valfactor, Rboolean verbose, struct processData *data) {
  int i, j, k, counter=0, thislen=0;
  SEXP thisvaluecols, ansvals, thisidx=R_NilValue, flevels;
  Rboolean copyattr = FALSE, thisvalfactor;
  size_t size;
  for (i=0; i<data->lvalues; i++) {
    thisvaluecols = VECTOR_ELT(data->valuecols, i);
    if (!data->isidentical[i])
      warning("'measure.vars' [%s] are not all of the same type. By order of hierarchy, the molten data value column will be of type '%s'. All measure variables not of type '%s' will be coerced too. Check DETAILS in ?melt.data.table for more on coercion.\n", CHAR(STRING_ELT(concat(dtnames, thisvaluecols), 0)), type2char(data->maxtype[i]), type2char(data->maxtype[i]));
    if (data->maxtype[i] == VECSXP && data->narm) {
      if (verbose) Rprintf("The molten data value type is a list at item %d. 'na.rm=TRUE' is ignored.\n", i+1);
      data->narm = FALSE;
    }
  }
  if (data->narm) {
    SEXP seqcols = PROTECT(seq_int(data->lvalues, 1));
    for (i=0; i<data->lmax; i++) {
      SEXP tmp = PROTECT(allocVector(VECSXP, data->lvalues));
      for (j=0; j<data->lvalues; j++) {
        if (i < data->leach[j]) {
          thisvaluecols = VECTOR_ELT(data->valuecols, j);
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
  flevels = PROTECT(allocVector(VECSXP, data->lmax));
  Rboolean *isordered = (Rboolean *)R_alloc(data->lmax, sizeof(Rboolean));
  ansvals = PROTECT(allocVector(VECSXP, data->lvalues));
  for (i=0; i<data->lvalues; i++) {
    thisvalfactor = (data->maxtype[i] == VECSXP) ? FALSE : valfactor;
    SEXP target = PROTECT(allocVector(data->maxtype[i], data->totlen)); // to keep rchk happy
    SET_VECTOR_ELT(ansvals, i, target);
    UNPROTECT(1);  // still protected by virtue of being member of protected ansval.
    thisvaluecols = VECTOR_ELT(data->valuecols, i);
    counter = 0; copyattr = FALSE;
    for (j=0; j<data->lmax; j++) {
      int thisprotecti = 0;
      SEXP thiscol = (j < data->leach[i]) ? VECTOR_ELT(DT, INTEGER(thisvaluecols)[j]-1)
                       : allocNAVector(data->maxtype[i], data->nrow);
      if (!copyattr && data->isidentical[i] && !data->isfactor[i]) {
        copyMostAttrib(thiscol, target);
        copyattr = TRUE;
      }
      if (TYPEOF(thiscol) != TYPEOF(target) && (data->maxtype[i] == VECSXP || !isFactor(thiscol))) {
        thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));  thisprotecti++;
      }
      if (data->narm) {
        thisidx = VECTOR_ELT(data->naidx, j);
        thislen = length(thisidx);
      }
      size = SIZEOF(thiscol);
      switch (TYPEOF(target)) {
        case VECSXP :
        if (data->narm) {
          for (k=0; k<thislen; k++)
            SET_VECTOR_ELT(target, counter + k, VECTOR_ELT(thiscol, INTEGER(thisidx)[k]-1));
        } else {
          for (k=0; k<data->nrow; k++) SET_VECTOR_ELT(target, j*data->nrow + k, VECTOR_ELT(thiscol, k));
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
          for (k=0; k<thislen; k++)
            SET_STRING_ELT(target, counter + k, STRING_ELT(thiscol, INTEGER(thisidx)[k]-1));
        } else {
          for (k=0; k<data->nrow; k++) SET_STRING_ELT(target, j*data->nrow + k, STRING_ELT(thiscol, k));
        }
        break;
        case REALSXP :
        if (data->narm) {
          for (k=0; k<thislen; k++)
            REAL(target)[counter + k] = REAL(thiscol)[INTEGER(thisidx)[k]-1];
        } else {
          memcpy((char *)REAL(target)+j*data->nrow*size, (char *)REAL(thiscol), data->nrow*size);
        }
        break;
        case INTSXP :
        if (data->narm) {
          for (k=0; k<thislen; k++)
            INTEGER(target)[counter + k] = INTEGER(thiscol)[INTEGER(thisidx)[k]-1];
        } else {
          memcpy((char *)INTEGER(target)+j*data->nrow*size, (char *)INTEGER(thiscol), data->nrow*size);
        }
        break;
        case LGLSXP :
        if (data->narm) {
          for (k=0; k<thislen; k++)
            LOGICAL(target)[counter + k] = LOGICAL(thiscol)[INTEGER(thisidx)[k]-1];
        } else {
          memcpy((char *)LOGICAL(target)+j*data->nrow*size, (char *)LOGICAL(thiscol), data->nrow*size);
        }
        break;
        default : error("Unknown column type '%s' for column '%s'.", type2char(TYPEOF(thiscol)), CHAR(STRING_ELT(dtnames, INTEGER(thisvaluecols)[i]-1)));
      }
      if (data->narm) counter += thislen;
      UNPROTECT(thisprotecti);  // inside inner loop (note that it's double loop) so as to limit use of protection stack
    }
    if (thisvalfactor && data->isfactor[i] && TYPEOF(target) != VECSXP) {
      SEXP clevels = PROTECT(combineFactorLevels(flevels, &(data->isfactor[i]), isordered));
      SEXP factorLangSxp = PROTECT(lang3(install(data->isfactor[i] == 1 ? "factor" : "ordered"), target, clevels));
      SET_VECTOR_ELT(ansvals, i, eval(factorLangSxp, R_GlobalEnv));
      UNPROTECT(2);  // clevels, factorLangSxp
    }
  }
  UNPROTECT(2);  // flevels, ansvals. Not using two protection counters (protecti and thisprotecti) to keep rchk happy.
  return(ansvals);
}

SEXP getvarcols(SEXP DT, SEXP dtnames, Rboolean varfactor, Rboolean verbose, struct processData *data) {

  int i,j,k,cnt=0,nrows=0, nlevels=0, protecti=0, thislen, zerolen=0;
  SEXP ansvars, thisvaluecols, levels, target, matchvals, thisnames;

  ansvars = PROTECT(allocVector(VECSXP, 1)); protecti++;
  SET_VECTOR_ELT(ansvars, 0, target=allocVector(INTSXP, data->totlen) );
  if (data->lvalues == 1) {
    thisvaluecols = VECTOR_ELT(data->valuecols, 0);
    // tmp fix for #1055
    thisnames = PROTECT(allocVector(STRSXP, length(thisvaluecols))); protecti++;
    for (i=0; i<length(thisvaluecols); i++) {
      SET_STRING_ELT(thisnames, i, STRING_ELT(dtnames, INTEGER(thisvaluecols)[i]-1));
    }
    matchvals = PROTECT(match(thisnames, thisnames, 0)); protecti++;
    if (data->narm) {
      for (j=0; j<data->lmax; j++) {
        thislen = length(VECTOR_ELT(data->naidx, j));
        for (k=0; k<thislen; k++)
          INTEGER(target)[nrows + k] = INTEGER(matchvals)[j - zerolen]; // fix for #1359
        nrows += thislen;
        zerolen += (thislen == 0);
      }
      nlevels = data->lmax - zerolen;
    } else {
      for (j=0; j<data->lmax; j++) {
        for (k=0; k<data->nrow; k++)
          INTEGER(target)[data->nrow*j + k] = INTEGER(matchvals)[j];
      }
      nlevels = data->lmax;
    }
  } else {
    if (data->narm) {
      for (j=0; j<data->lmax; j++) {
        thislen = length(VECTOR_ELT(data->naidx, j));
        for (k=0; k<thislen; k++)
          INTEGER(target)[nrows + k] = j+1;
        nrows += thislen;
        nlevels += (thislen != 0);
      }
    } else {
      for (j=0; j<data->lmax; j++) {
        for (k=0; k<data->nrow; k++)
          INTEGER(target)[data->nrow*j + k] = j+1;
      }
      nlevels = data->lmax;
    }
  }
  SEXP tmp = PROTECT(mkString("factor")); protecti++;
  setAttrib(target, R_ClassSymbol, tmp);
  cnt = 0;
  if (data->lvalues == 1) {
    levels = PROTECT(allocVector(STRSXP, nlevels)); protecti++;
    thisvaluecols = VECTOR_ELT(data->valuecols, 0); // levels will be column names
    for (i=0; i<data->lmax; i++) {
      if (data->narm) {
        if (length(VECTOR_ELT(data->naidx, i)) == 0) continue;
      }
      SET_STRING_ELT(levels, cnt++, STRING_ELT(dtnames, INTEGER(thisvaluecols)[i]-1));
    }
  } else {
    SEXP tt = PROTECT(seq_int(nlevels, 1)); protecti++;      // generate levels = 1:nlevels
    levels = PROTECT(coerceVector(tt, STRSXP)); protecti++;  // tt PROTECTED for rchk
  }
  // base::unique is fast on vectors, and the levels on variable columns are usually small
  SEXP uniqueLangSxp = PROTECT(lang2(install("unique"), levels)); protecti++;
  tmp = PROTECT(eval(uniqueLangSxp, R_GlobalEnv)); protecti++;
  setAttrib(target, R_LevelsSymbol, tmp);
  if (!varfactor) SET_VECTOR_ELT(ansvars, 0, asCharacterFactor(target));
  UNPROTECT(protecti);
  return(ansvars);
}

SEXP getidcols(SEXP DT, SEXP dtnames, Rboolean verbose, struct processData *data) {

  int i,j,k, counter=0, thislen;
  SEXP ansids, thiscol, target, thisidx;
  size_t size;
  ansids = PROTECT(allocVector(VECSXP, data->lids));
  for (i=0; i<data->lids; i++) {
    counter = 0;
    thiscol = VECTOR_ELT(DT, INTEGER(data->idcols)[i]-1);
    size = SIZEOF(thiscol);
    SET_VECTOR_ELT(ansids, i, target=allocVector(TYPEOF(thiscol), data->totlen) );
    copyMostAttrib(thiscol, target); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
    switch(TYPEOF(thiscol)) {
    case REALSXP :
      if (data->narm) {
        for (j=0; j<data->lmax; j++) {
          thisidx = VECTOR_ELT(data->naidx, j);
          thislen = length(thisidx);
          for (k=0; k<thislen; k++)
            REAL(target)[counter + k] = REAL(thiscol)[INTEGER(thisidx)[k]-1];
          counter += thislen;
        }
      } else {
        for (j=0; j<data->lmax; j++)
          memcpy((char *)REAL(target)+j*data->nrow*size, (char *)REAL(thiscol), data->nrow*size);
      }
      break;
    case INTSXP :
      if (data->narm) {
        for (j=0; j<data->lmax; j++) {
          thisidx = VECTOR_ELT(data->naidx, j);
          thislen = length(thisidx);
          for (k=0; k<thislen; k++)
            INTEGER(target)[counter + k] = INTEGER(thiscol)[INTEGER(thisidx)[k]-1];
          counter += thislen;
        }
      } else {
        for (j=0; j<data->lmax; j++)
          memcpy((char *)INTEGER(target)+j*data->nrow*size, (char *)INTEGER(thiscol), data->nrow*size);
      }
      break;
    case LGLSXP :
      if (data->narm) {
        for (j=0; j<data->lmax; j++) {
          thisidx = VECTOR_ELT(data->naidx, j);
          thislen = length(thisidx);
          for (k=0; k<thislen; k++)
            LOGICAL(target)[counter + k] = LOGICAL(thiscol)[INTEGER(thisidx)[k]-1];
          counter += thislen;
        }
      } else {
        for (j=0; j<data->lmax; j++)
          memcpy((char *)LOGICAL(target)+j*data->nrow*size, (char *)LOGICAL(thiscol), data->nrow*size);
      }
      break;
    case STRSXP :
      if (data->narm) {
        for (j=0; j<data->lmax; j++) {
          thisidx = VECTOR_ELT(data->naidx, j);
          thislen = length(thisidx);
          for (k=0; k<thislen; k++)
            SET_STRING_ELT(target, counter + k, STRING_ELT(thiscol, INTEGER(thisidx)[k]-1));
          counter += thislen;
        }
      } else {
        const SEXP *s = STRING_PTR(thiscol);  // to reduce overhead of STRING_ELT() inside loop below. Read-only hence const.
        for (j=0; j<data->lmax; j++) {
          for (k=0; k<data->nrow; k++) {
            SET_STRING_ELT(target, j*data->nrow + k, s[k]);
          }
        }
      }
      break;
    case VECSXP :
      for (j=0; j<data->lmax; j++) {
        for (k=0; k<data->nrow; k++) {
          SET_VECTOR_ELT(target, j*data->nrow + k, VECTOR_ELT(thiscol, k));
        }
      }
      break;
    default : error("Unknown column type '%s' for column '%s' in 'data'", type2char(TYPEOF(thiscol)), CHAR(STRING_ELT(dtnames, INTEGER(data->idcols)[i]-1)));
    }
  }
  UNPROTECT(1);
  return (ansids);
}

SEXP fmelt(SEXP DT, SEXP id, SEXP measure, SEXP varfactor, SEXP valfactor, SEXP varnames, SEXP valnames, SEXP narmArg, SEXP verboseArg) {
  SEXP dtnames, ansvals, ansvars, ansids, ansnames, ans;
  Rboolean narm=FALSE, verbose=FALSE;

  if (!isNewList(DT)) error("Input is not of type VECSXP, expected a data.table, data.frame or list");
  if (!isLogical(valfactor)) error("Argument 'value.factor' should be logical TRUE/FALSE");
  if (!isLogical(varfactor)) error("Argument 'variable.factor' should be logical TRUE/FALSE");
  if (!isLogical(narmArg)) error("Argument 'na.rm' should be logical TRUE/FALSE.");
  if (!isString(varnames)) error("Argument 'variable.name' must be a character vector");
  if (!isString(valnames)) error("Argument 'value.name' must be a character vector");
  if (!isLogical(verboseArg)) error("Argument 'verbose' should be logical TRUE/FALSE");
  int ncol = LENGTH(DT);
  if (!ncol) {
    if (verbose) Rprintf("ncol(data) is 0. Nothing to melt. Returning original data.table.");
    return(DT);
  }
  dtnames = PROTECT(getAttrib(DT, R_NamesSymbol));
  int protecti=1;
  if (isNull(dtnames)) error("names(data) is NULL. Please report to data.table-help");
  if (LOGICAL(narmArg)[0] == TRUE) narm = TRUE;
  if (LOGICAL(verboseArg)[0] == TRUE) verbose = TRUE;
  struct processData data;
  data.RCHK = PROTECT(allocVector(VECSXP, 2)); protecti++;
  preprocess(DT, id, measure, varnames, valnames, narm, verbose, &data);
  // edge case no measure.vars
  if (!data.lmax) {
    ans = shallowwrapper(DT, data.idcols);
    ans = PROTECT(duplicate(ans)); protecti++;
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
