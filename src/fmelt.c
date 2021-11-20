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

static const char *concat(SEXP vec, SEXP idx) {
  if (!isString(vec)) error(_("concat: 'vec' must be a character vector"));
  if (!isInteger(idx) || length(idx) < 0) error(_("concat: 'idx' must be an integer vector of length >= 0"));

  static char ans[1024];  // so only one call to concat() per calling warning/error
  int nidx=length(idx), nvec=length(vec);
  ans[0]='\0';
  if (nidx==0) return ans;
  const int *iidx = INTEGER(idx);
  for (int i=0; i<nidx; ++i) {
    if (iidx[i]<1 || iidx[i]>nvec)
      error(_("Internal error in concat: 'idx' must take values between 1 and length(vec); 1 <= idx <= %d"), nvec); // # nocov
  }
  if (nidx>4) nidx=4;  // first 4 following by ... if there are more than 4
  int remaining=1018;  // leaving space for ", ...\0" at the end of the 1024, potentially
  char *pos=ans;
  int i=0;
  for (; i<nidx; ++i) {
    SEXP this = STRING_ELT(vec, iidx[i]-1);
    int len = length(this);
    if (len>remaining) break;
    strncpy(pos, CHAR(this), len);
    pos+=len;
    remaining-=len;
    *pos++ = ',';
    *pos++ = ' ';
  }
  if (length(vec)>4 || i<nidx /*4 or less but won't fit in 1024 chars*/) {
    *pos++='.'; *pos++='.'; *pos++='.';
  } else {
    pos-=2; // rewind the last ", "
  }
  *pos='\0';
  return ans;
}

// input: character vector of column names (maybe missing), output:
// integer vector of column indices with NA_INTEGER in the positions
// with missing inputs, and -1 in the positions with column names not
// found. Column names not found will eventually cause error via
// uniq_diff().
SEXP chmatch_na(SEXP x, SEXP table){
  SEXP ans;
  PROTECT(ans = chmatch(x, table, -1));
  for(int i=0; i<length(ans); i++){
    if(STRING_ELT(x, i) == NA_STRING){
      INTEGER(ans)[i] = NA_INTEGER;
    }
  }
  UNPROTECT(1);
  return ans;
}

// deal with measure.vars of type VECSXP
SEXP measurelist(SEXP measure, SEXP dtnames) {
  const int n=length(measure);
  SEXP ans = PROTECT(allocVector(VECSXP, n));
  for (int i=0; i<n; ++i) {
    SEXP x = VECTOR_ELT(measure, i);
    switch(TYPEOF(x)) {
    case STRSXP  :
      SET_VECTOR_ELT(ans, i, chmatch_na(x, dtnames));
      break;
    case REALSXP :
      SET_VECTOR_ELT(ans, i, coerceVector(x, INTSXP));
      break;
    case INTSXP  :
      SET_VECTOR_ELT(ans, i, x);
      break;
    default :
      error(_("Unknown 'measure.vars' type %s at index %d of list"), type2char(TYPEOF(x)), i+1);
    }
  }
  UNPROTECT(1);
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

// convert NA in user-supplied integer vector input to -1 in order to
// trigger error in uniq_diff().
SEXP na_to_negative(SEXP vec_with_NA){
  SEXP vec_with_negatives = PROTECT(allocVector(INTSXP, length(vec_with_NA)));
  for (int i=0; i<length(vec_with_NA); i++) {
    int input_i = INTEGER(vec_with_NA)[i];
    INTEGER(vec_with_negatives)[i] = (input_i == NA_INTEGER) ? -1 : input_i;
  }
  UNPROTECT(1);
  return vec_with_negatives;
}

// If neither id.vars nor measure.vars is provided by user, then this
// function decides which input columns are default measure.vars.
bool is_default_measure(SEXP vec) {
  return (isInteger(vec) || isNumeric(vec) || isLogical(vec)) && !isFactor(vec);
}

// maybe unlist, then unique, then set_diff.
SEXP uniq_diff(SEXP int_or_list, int ncol, bool is_measure) {
  SEXP int_vec = PROTECT(isNewList(int_or_list) ? unlist_(int_or_list) : int_or_list);
  SEXP is_duplicated = PROTECT(duplicated(int_vec, FALSE)); 
  int n_unique_cols = 0;
  for (int i=0; i<length(int_vec); ++i) {
    int col_number = INTEGER(int_vec)[i];
    bool good_number = 0 < col_number && col_number <= ncol;
    if (is_measure) good_number |= (col_number==NA_INTEGER);
    if (!good_number) {
      if (is_measure) {
        error(_("One or more values in 'measure.vars' is invalid."));
      } else {
        error(_("One or more values in 'id.vars' is invalid."));
      }
    } else if (!LOGICAL(is_duplicated)[i]) n_unique_cols++;
  }
  SEXP unique_col_numbers = PROTECT(allocVector(INTSXP, n_unique_cols)); 
  int unique_i = 0;
  for (int i=0; i<length(is_duplicated); ++i) {
    if (!LOGICAL(is_duplicated)[i]) {
      INTEGER(unique_col_numbers)[unique_i++] = INTEGER(int_vec)[i];
    }
  }
  UNPROTECT(3);
  return set_diff(unique_col_numbers, ncol);
}

SEXP cols_to_int_or_list(SEXP cols, SEXP dtnames, bool is_measure) {
  switch(TYPEOF(cols)) {
  case STRSXP  : return chmatch(cols, dtnames, 0); 
  case REALSXP : return coerceVector(cols, INTSXP); 
  case INTSXP  : return na_to_negative(cols); 
  case VECSXP  : if(is_measure)return measurelist(cols, dtnames); 
  default : 
    if (is_measure) {
      error(_("Unknown 'measure.vars' type %s, must be character or integer vector/list"), type2char(TYPEOF(cols)));
    } else {
      error(_("Unknown 'id.vars' type %s, must be character or integer vector"), type2char(TYPEOF(cols)));
    }
  }
}

SEXP checkVars(SEXP DT, SEXP id, SEXP measure, Rboolean verbose) {
  int ncol=LENGTH(DT), n_target_cols=0, protecti=0, target_col_i=0, id_col_i=0;
  SEXP thiscol, idcols = R_NilValue, valuecols = R_NilValue, ans;
  SEXP dtnames = PROTECT(getAttrib(DT, R_NamesSymbol)); protecti++;
  if (isNull(id) && isNull(measure)) {
    for (int i=0; i<ncol; ++i) {
      thiscol = VECTOR_ELT(DT, i);
      if (is_default_measure(thiscol)) n_target_cols++;
    }
    idcols = PROTECT(allocVector(INTSXP, ncol-n_target_cols)); protecti++;
    SEXP target_cols = PROTECT(allocVector(INTSXP, n_target_cols)); protecti++;
    for (int i=0; i<ncol; ++i) {
      thiscol = VECTOR_ELT(DT, i);
      if (is_default_measure(thiscol)) {
        INTEGER(target_cols)[target_col_i++] = i+1;
      } else
        INTEGER(idcols)[id_col_i++] = i+1;
    }
    valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(valuecols, 0, target_cols);
    warning(_("id.vars and measure.vars are internally guessed when both are 'NULL'. All non-numeric/integer/logical type columns are considered id.vars, which in this case are columns [%s]. Consider providing at least one of 'id' or 'measure' vars in future."), concat(dtnames, idcols));
  } else if (!isNull(id) && isNull(measure)) {
    idcols = PROTECT(cols_to_int_or_list(id, dtnames, false)); protecti++; 
    valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
    SET_VECTOR_ELT(valuecols, 0, uniq_diff(idcols, ncol, false));
    if (verbose) {
      Rprintf(_("'measure.vars' is missing. Assigning all columns other than 'id.vars' columns as 'measure.vars'.\n"));
      SEXP value_col = VECTOR_ELT(valuecols, 0);
      if (length(value_col)) Rprintf(_("Assigned 'measure.vars' are [%s].\n"), concat(dtnames, value_col));
    }
  } else if (isNull(id) && !isNull(measure)) {
    SEXP measure_int_or_list = cols_to_int_or_list(measure, dtnames, true);
    idcols = PROTECT(uniq_diff(measure_int_or_list, ncol, true)); protecti++;
    if (isNewList(measure)) valuecols = measure_int_or_list;
    else {
      valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
      SET_VECTOR_ELT(valuecols, 0, measure_int_or_list);
    }
    if (verbose) {
      Rprintf(_("'id.vars' is missing. Assigning all columns other than 'measure.vars' columns as 'id.vars'.\n"));
      if (length(idcols)) Rprintf(_("Assigned 'id.vars' are [%s].\n"), concat(dtnames, idcols));
    }
  } else if (!isNull(id) && !isNull(measure)) {
    idcols = PROTECT(cols_to_int_or_list(id, dtnames, false)); protecti++;
    uniq_diff(idcols, ncol, false);//for error checking.
    SEXP measure_int_or_list = 
      PROTECT(cols_to_int_or_list(measure, dtnames, true)); protecti++;
    uniq_diff(measure_int_or_list, ncol, true);//error checking.
    if (isNewList(measure)) valuecols = measure_int_or_list;
    else {
      valuecols = PROTECT(allocVector(VECSXP, 1)); protecti++;
      SET_VECTOR_ELT(valuecols, 0, measure_int_or_list);
    }
  }
  ans = PROTECT(allocVector(VECSXP, 2)); protecti++;
  SET_VECTOR_ELT(ans, 0, idcols);
  SET_VECTOR_ELT(ans, 1, valuecols);//List of integer vectors.
  UNPROTECT(protecti);
  return(ans);
}

struct processData {
  SEXP RCHK;      // a 2 item list holding vars (result of checkVars) and not_NA_indices. PROTECTed up in fmelt so that preprocess() doesn't need to PROTECT. To pass rchk, #2865
  SEXP idcols,    // convenience pointers into RCHK[0][0], RCHK[0][1] and RCHK[1] respectively
    variable_table, // NULL or data for variable column(s).
    valuecols,    // list with one element per output/value column, each element is an integer vector.
    not_NA_indices;
  int *isfactor,
    *leach,       // length of each element of the valuecols(measure.vars) list.
    *isidentical; // are all inputs for this value column the same type?
  int lids,       // number of id columns.
    lvars,        // number of variable columns.
    lvalues,      // number of value columns.
    lmax,         // max length of valuecols elements / number of times to repeat ids.
    totlen,       // of output/long DT result of melt operation.
    nrow;         // of input/wide DT to be melted.
  SEXPTYPE *maxtype;
  Rboolean narm;  // remove missing values?
};

static void preprocess(SEXP DT, SEXP id, SEXP measure, SEXP varnames, SEXP valnames, Rboolean narm, Rboolean verbose, struct processData *data) {

  SEXP vars,tmp,thiscol;
  SEXPTYPE type;
  data->lmax = 0; data->totlen = 0; data->nrow = length(VECTOR_ELT(DT, 0));
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
    error(_("'variable.name' must be a character/integer vector of length 1."));
  data->leach = (int *)R_alloc(data->lvalues, sizeof(int));
  data->isidentical = (int *)R_alloc(data->lvalues, sizeof(int));
  data->isfactor = (int *)R_alloc(data->lvalues, sizeof(int));
  data->maxtype = (SEXPTYPE *)R_alloc(data->lvalues, sizeof(SEXPTYPE));
  // first find max type of each output column.
  for (int i=0; i<data->lvalues; ++i) { // for each output column.
    tmp = VECTOR_ELT(data->valuecols, i);
    data->leach[i] = length(tmp);
    if (data->leach[i] > data->lmax) {
      data->lmax = data->leach[i];
    }
    data->isidentical[i] = 1;  // TODO - why 1 and not Rboolean TRUE?
    data->isfactor[i] = 0;  // seems to hold 2 below, so not an Rboolean FALSE here. TODO - better name for variable?
    data->maxtype[i] = 0;   // R_alloc doesn't initialize so careful to here, relied on below
    for (int j=0; j<data->leach[i]; ++j) { // for each input column.
      int this_col_num = INTEGER(tmp)[j];
      if(this_col_num != NA_INTEGER){
        thiscol = VECTOR_ELT(DT, this_col_num-1);
        if (isFactor(thiscol)) {
          data->isfactor[i] = (isOrdered(thiscol)) ? 2 : 1;
          data->maxtype[i]  = STRSXP;
        } else {
          type = TYPEOF(thiscol);
          if (type > data->maxtype[i]) data->maxtype[i] = type;
        }
      }
    }
    // then compute isidentical for this output column.
    for (int j=0; j<data->leach[i]; ++j) {
      int this_col_num = INTEGER(tmp)[j];
      if(this_col_num != NA_INTEGER){
        thiscol = VECTOR_ELT(DT, this_col_num-1);
        if ( (!isFactor(thiscol) && data->maxtype[i] != TYPEOF(thiscol)) ||
             (isFactor(thiscol) && data->maxtype[i] != STRSXP) ) {
          data->isidentical[i] = 0;
          break;
        }
      }
    }
  }
  if (data->narm) {
    SET_VECTOR_ELT(data->RCHK, 1, data->not_NA_indices = allocVector(VECSXP, data->lmax));
  }
  // TDH 1 Oct 2020 variable table.
  data->variable_table = getAttrib(measure, sym_variable_table);
  if (isNull(data->variable_table)) {
    // We need to include this check first because isNewList(NULL) ==
    // TRUE
    data->lvars = 1;
  } else if (isNewList(data->variable_table)) {
    data->lvars = length(data->variable_table);
    if (data->lvars == 0) {
      error(_("variable_table attribute of measure.vars should be a data table with at least one column"));
    }
    for (int i=0; i<length(data->variable_table); ++i) {
      int nrow = length(VECTOR_ELT(data->variable_table, i));
      if (data->lmax != nrow) {
        error(_("variable_table attribute of measure.vars should be a data table with same number of rows as max length of measure.vars vectors =%d"), data->lmax);
      }
    }
  } else {//neither NULL nor DT.
    error(_("variable_table attribute of measure.vars should be either NULL or a data table"));
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

SEXP input_col_or_NULL(SEXP DT, struct processData* data, SEXP thisvaluecols, int out_col, int in_col) {
  if (in_col < data->leach[out_col]) {
    int input_column_num = INTEGER(thisvaluecols)[in_col];
    if (input_column_num != NA_INTEGER) {
      return VECTOR_ELT(DT, input_column_num-1);
    }
  }
  return R_NilValue;
}

SEXP getvaluecols(SEXP DT, SEXP dtnames, Rboolean valfactor, Rboolean verbose, struct processData *data) {
  for (int i=0; i<data->lvalues; ++i) {
    SEXP thisvaluecols = VECTOR_ELT(data->valuecols, i);
    if (!data->isidentical[i])
      warning(_("'measure.vars' [%s] are not all of the same type. By order of hierarchy, the molten data value column will be of type '%s'. All measure variables not of type '%s' will be coerced too. Check DETAILS in ?melt.data.table for more on coercion.\n"), concat(dtnames, thisvaluecols), type2char(data->maxtype[i]), type2char(data->maxtype[i]));
  }
  if (data->narm) {
    SEXP seqcols = PROTECT(seq_int(data->lvalues, 1));
    for (int i=0; i<data->lmax; ++i) {//element in measure vector.
      SEXP valuecols_data = PROTECT(allocVector(VECSXP, data->lvalues));
      int N_missing_columns = 0;
      for (int j=0; j<data->lvalues; ++j) {//which measure vector/output col.
        SEXP thisvaluecols = VECTOR_ELT(data->valuecols, j);
        SEXP vec_or_NULL = input_col_or_NULL(DT, data, thisvaluecols, j, i);
        if (vec_or_NULL == R_NilValue) {
          N_missing_columns++;
        }
        SET_VECTOR_ELT(valuecols_data, j, vec_or_NULL);
      }
      if (N_missing_columns==0) {
        SEXP any_missing = PROTECT(dt_na(valuecols_data, seqcols));
        SEXP missing_indices;
        SET_VECTOR_ELT(data->not_NA_indices, i, missing_indices=which(any_missing, FALSE));
        data->totlen += length(missing_indices);
        UNPROTECT(1); // any_missing
      } else {
        SET_VECTOR_ELT(data->not_NA_indices, i, allocVector(INTSXP, 0));
      }
      UNPROTECT(1); // valuecols_data
    }
    UNPROTECT(1);  // seqcols
  } else {
    data->totlen = data->nrow * data->lmax;
  }
  SEXP flevels = PROTECT(allocVector(VECSXP, data->lmax));
  Rboolean *isordered = (Rboolean *)R_alloc(data->lmax, sizeof(Rboolean));
  SEXP ansvals = PROTECT(allocVector(VECSXP, data->lvalues));
  for (int i=0; i<data->lvalues; ++i) {//for each output/value column.
    bool thisvalfactor = (data->maxtype[i] == VECSXP) ? false : valfactor;
    SEXP target = PROTECT(allocVector(data->maxtype[i], data->totlen)); // to keep rchk happy
    SET_VECTOR_ELT(ansvals, i, target);
    UNPROTECT(1);  // still protected by virtue of being member of protected ansval.
    SEXP thisvaluecols = VECTOR_ELT(data->valuecols, i); // integer vector of column ids.
    int counter = 0;
    bool copyattr = false;
    for (int j=0; j<data->lmax; ++j) {// for each input column.
      int thisprotecti = 0;
      SEXP thiscol = input_col_or_NULL(DT, data, thisvaluecols, i, j);
      if (thiscol == R_NilValue) {
        if (!data->narm) {
          writeNA(target, j*data->nrow, data->nrow, true);  // listNA=true #5053 
        }
      }else{
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
          SEXP thisidx = VECTOR_ELT(data->not_NA_indices, j);
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
          //TODO complex value type: case CPLXSXP: { } break;
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
      }
      UNPROTECT(thisprotecti);  // inside inner loop (note that it's double loop) so as to limit use of protection stack
    }
    if (thisvalfactor && data->isfactor[i] && TYPEOF(target) != VECSXP) {
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
  SEXP ansvars=PROTECT(allocVector(VECSXP, data->lvars)); protecti++;
  SEXP target;
  if (data->lvalues==1 && length(VECTOR_ELT(data->valuecols, 0)) != data->lmax)
    error(_("Internal error: fmelt.c:getvarcols %d %d"), length(VECTOR_ELT(data->valuecols, 0)), data->lmax);  // # nocov
  if (isNull(data->variable_table)) {
    if (!varfactor) {
      SET_VECTOR_ELT(ansvars, 0, target=allocVector(STRSXP, data->totlen));
      if (data->lvalues == 1) {//one value column to output.
        const int *thisvaluecols = INTEGER(VECTOR_ELT(data->valuecols, 0));
        for (int j=0, ansloc=0; j<data->lmax; ++j) {
          const int thislen = data->narm ? length(VECTOR_ELT(data->not_NA_indices, j)) : data->nrow;
          SEXP str = STRING_ELT(dtnames, thisvaluecols[j]-1);
          for (int k=0; k<thislen; ++k) SET_STRING_ELT(target, ansloc++, str);
        }
      } else {//multiple value columns to output.
        for (int j=0, ansloc=0, level=1; j<data->lmax; ++j) {
          const int thislen = data->narm ? length(VECTOR_ELT(data->not_NA_indices, j)) : data->nrow;
          char buff[20];
          snprintf(buff, 20, "%d", level++);
          for (int k=0; k<thislen; ++k) SET_STRING_ELT(target, ansloc++, mkChar(buff));
        }
      }
    } else {// varfactor==TRUE
      SET_VECTOR_ELT(ansvars, 0, target=allocVector(INTSXP, data->totlen));
      SEXP levels;
      int *td = INTEGER(target);
      if (data->lvalues == 1) {//one value column to output.
        SEXP thisvaluecols = VECTOR_ELT(data->valuecols, 0);
        int len = length(thisvaluecols);
        levels = PROTECT(allocVector(STRSXP, len)); protecti++;
        const int *vd = INTEGER(thisvaluecols);
        for (int j=0; j<len; ++j) SET_STRING_ELT(levels, j, STRING_ELT(dtnames, vd[j]-1));
        SEXP m = PROTECT(chmatch(levels, levels, 0)); protecti++;  // do we have any dups?
        int numRemove = 0;  // remove dups and any for which narm and all-NA
        int *md = INTEGER(m);
        for (int j=0; j<len; ++j) {
          if (md[j]!=j+1 /*dup*/ || (data->narm && length(VECTOR_ELT(data->not_NA_indices, j))==0)) { numRemove++; md[j]=0; }
        }
        if (numRemove) {
          SEXP newlevels = PROTECT(allocVector(STRSXP, len-numRemove)); protecti++;
          for (int i=0, loc=0; i<len; ++i) if (md[i]!=0) { SET_STRING_ELT(newlevels, loc++, STRING_ELT(levels, i)); }
          m = PROTECT(chmatch(levels, newlevels, 0)); protecti++;  // budge up the gaps
          md = INTEGER(m);
          levels = newlevels;
        }
        for (int j=0, ansloc=0; j<data->lmax; ++j) {
          const int thislen = data->narm ? length(VECTOR_ELT(data->not_NA_indices, j)) : data->nrow;
          for (int k=0; k<thislen; ++k) td[ansloc++] = md[j];
        }
      } else {//multiple output columns.
        int nlevel=0;
        levels = PROTECT(allocVector(STRSXP, data->lmax)); protecti++;
        for (int j=0, ansloc=0; j<data->lmax; ++j) {
          const int thislen = data->narm ? length(VECTOR_ELT(data->not_NA_indices, j)) : data->nrow;
          char buff[20];
          snprintf(buff, 20, "%d", nlevel+1);
          SET_STRING_ELT(levels, nlevel++, mkChar(buff));  // generate levels = 1:nlevels
          for (int k=0; k<thislen; ++k) td[ansloc++] = nlevel;
        }
      }
      setAttrib(target, R_LevelsSymbol, levels);
      setAttrib(target, R_ClassSymbol, ScalarString(char_factor));
    }
  } else { //variable_table specified
    for (int out_col_i=0; out_col_i<data->lvars; ++out_col_i) {
      SEXP out_col = VECTOR_ELT(data->variable_table, out_col_i);
      SET_VECTOR_ELT(ansvars, out_col_i, target=allocVector(TYPEOF(out_col), data->totlen));
      for (int j=0, ansloc=0; j<data->lmax; ++j) {
        const int thislen = data->narm ? length(VECTOR_ELT(data->not_NA_indices, j)) : data->nrow;
        switch (TYPEOF(target)) {
        case STRSXP :
          for (int k=0; k<thislen; ++k)
            SET_STRING_ELT(target, ansloc++, STRING_ELT(out_col, j));
          break;
        case REALSXP :
          for (int k=0; k<thislen; ++k)
            REAL(target)[ansloc++] = REAL(out_col)[j];
          break;
        case INTSXP :
        case LGLSXP :
          for (int k=0; k<thislen; ++k)
            INTEGER(target)[ansloc++] = INTEGER(out_col)[j];
          if (isFactor(out_col)) {
            // Do we need a copy here?
            setAttrib(target, R_LevelsSymbol, getAttrib(out_col, R_LevelsSymbol));
            setAttrib(target, R_ClassSymbol, ScalarString(char_factor));
          }
          break;
        default :
          error(_("variable_table does not support column type '%s' for column '%s'."), type2char(TYPEOF(out_col)), CHAR(STRING_ELT(getAttrib(data->variable_table, R_NamesSymbol), out_col_i)));
        }
      }
    }
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
          SEXP thisidx = VECTOR_ELT(data->not_NA_indices, j);
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
          SEXP thisidx = VECTOR_ELT(data->not_NA_indices, j);
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
          SEXP thisidx = VECTOR_ELT(data->not_NA_indices, j);
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
      if (data->narm) {
        for (int j=0; j<data->lmax; ++j) {
          SEXP thisidx = VECTOR_ELT(data->not_NA_indices, j);
          const int *ithisidx = INTEGER(thisidx);
          const int thislen = length(thisidx);
          for (int k=0; k<thislen; ++k)
            SET_VECTOR_ELT(target, counter + k, VECTOR_ELT(thiscol, ithisidx[k]-1));
          counter += thislen;
        }
      } else {
        for (int j=0; j<data->lmax; ++j) {
          for (int k=0; k<data->nrow; ++k) {
            SET_VECTOR_ELT(target, j*data->nrow + k, VECTOR_ELT(thiscol, k));
          }
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
    int ncol_ans = data.lids+data.lvars+data.lvalues;
    ans = PROTECT(allocVector(VECSXP, ncol_ans)); protecti++; // 1 is for variable column
    for (int i=0; i<data.lids; i++) {
      SET_VECTOR_ELT(ans, i, VECTOR_ELT(ansids, i));
    }
    for (int i=0; i<data.lvars; i++) {
      SET_VECTOR_ELT(ans, data.lids+i, VECTOR_ELT(ansvars, i));
    }
    for (int i=0; i<data.lvalues; i++) {
      SET_VECTOR_ELT(ans, data.lids+data.lvars+i, VECTOR_ELT(ansvals, i));
    }
    // fill in 'ansnames'
    ansnames = PROTECT(allocVector(STRSXP, ncol_ans)); protecti++;
    for (int i=0; i<data.lids; i++) {
      SET_STRING_ELT(ansnames, i, STRING_ELT(dtnames, INTEGER(data.idcols)[i]-1));
    }
    if (isNull(data.variable_table)) {
      SET_STRING_ELT(ansnames, data.lids, STRING_ELT(varnames, 0));
    } else {
      for (int i=0; i<data.lvars; i++) {
        SET_STRING_ELT(ansnames, data.lids+i, STRING_ELT(getAttrib(data.variable_table, R_NamesSymbol), i));
      }
    }
    for (int i=0; i<data.lvalues; i++) {
      SET_STRING_ELT(ansnames, data.lids+data.lvars+i, STRING_ELT(valnames, i));
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
  }
  UNPROTECT(protecti);
  return(ans);
}
