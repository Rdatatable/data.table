#include "data.table.h"

bool isRealReallyInt(SEXP x) {
  if (!isReal(x)) return(false);
  R_xlen_t n=xlength(x), i=0;
  double *dx = REAL(x);
  while (i<n &&
         ( ISNA(dx[i]) ||
         ( R_FINITE(dx[i]) && dx[i] == (int)(dx[i])))) {
    i++;
  }
  return i==n;
}

SEXP isReallyReal(SEXP x) {
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = 0;
  // return 0 (FALSE) when not type double, or is type double but contains integers
  // used to error if not passed type double but this needed extra is.double() calls in calling R code
  // which needed a repeat of the argument. Hence simpler and more robust to return 0 when not type double.
  if (isReal(x)) {
    int n=length(x), i=0;
    double *dx = REAL(x);
    while (i<n &&
        ( ISNA(dx[i]) ||
        ( R_FINITE(dx[i]) && dx[i] == (int)(dx[i])))) {
      i++;
    }
    if (i<n) INTEGER(ans)[0] = i+1;  // return the location of first element which is really real; i.e. not an integer
  }
  UNPROTECT(1);
  return(ans);
}

bool allNA(SEXP x, bool errorForBadType) {
  // less space and time than all(is.na(x)) at R level because that creates full size is.na(x) first before all()
  // whereas this allNA can often return early on testing the first value without reading the rest
  const int n = length(x);
  if (n==0) // empty vectors (including raw(), NULL, and list()) same as R's all(is.na()) true result; tests 2116.*
    return true;
  switch (TYPEOF(x)) {
  case RAWSXP: // raw doesn't support NA so always false (other than length 0 case above)
    return false;
  case LGLSXP:
  case INTSXP: {
    const int *xd = INTEGER(x);
    for (int i=0; i<n; ++i)    if (xd[i]!=NA_INTEGER) {
      return false;
    }
    return true;
  }
  case REALSXP:
    if (Rinherits(x,char_integer64)) {
      const int64_t *xd = (int64_t *)REAL(x);
      for (int i=0; i<n; ++i)  if (xd[i]!=NA_INTEGER64) {
        return false;
      }
    } else {
      const double *xd = REAL(x);
      for (int i=0; i<n; ++i)  if (!ISNAN(xd[i])) {
        return false;
      }
    }
    return true;
  case STRSXP: {
    const SEXP *xd = STRING_PTR(x);
    for (int i=0; i<n; ++i)    if (xd[i]!=NA_STRING) {
      return false;
    }
    return true;
  }}
  if (!errorForBadType) return false;
  error(_("Unsupported type '%s' passed to allNA()"), type2char(TYPEOF(x)));  // e.g. VECSXP; tests 2116.16-18
  // turned off allNA list support for now to avoid accidentally using it internally where we did not intend; allNA not yet exported
  //   https://github.com/Rdatatable/data.table/pull/3909#discussion_r329065950
}

SEXP allNAR(SEXP x) {
  return ScalarLogical(allNA(x, /*errorForBadType=*/true));
}

/* range_int
 * used in exprCols(v4:v2) to produce 4:2 sequence
 * and when inverse=T to produce all columns sequence
 */
SEXP range_int(int from, int to) {
  if (from==NA_INTEGER || to==NA_INTEGER)
    error(_("internal error: NA was supplied to generate integer range")); // # nocov
  int len = to - from;
  int ord = len>=0 ? 1 : -1;
  len = len * ord;
  SEXP ans = PROTECT(allocVector(INTSXP, len+1));
  int *ansp = INTEGER(ans);
  for (int i=0; i<=len; i++)
    ansp[i] = from+i*ord;
  UNPROTECT(1);
  return ans;
}

/* colnamesInt
 * for provided data.table (or a list-like) and a subset of its columns, it returns integer positions of those columns in DT
 * handle columns input as: integer, double, character, logical and NULL (handled as seq_along(x))
 * inverse proceed to inverted selection to handle '!' or '-' calls
 * adds validation for:
 *   correct range [1,ncol], and if type real checks whole integer
 *   existing columns for character
 *   optionally check for no duplicates
 */
SEXP colnamesInt(SEXP x, SEXP cols, SEXP check_dups, SEXP inverse) {
  if (!isNewList(x))
    error(_("'x' argument must be data.table compatible"));
  if (!IS_TRUE_OR_FALSE(check_dups))
    error(_("'check_dups' argument must be TRUE or FALSE"));
  if (!IS_TRUE_OR_FALSE(inverse))
    error(_("'inverse' argument must be TRUE or FALSE"));
  int protecti = 0;
  R_len_t nx = length(x);
  R_len_t nc = length(cols);
  if (isLogical(cols) && nc != nx)
    error(_("argument specifying columns is logical and has different length than number of columns in a table"));
  SEXP ricols = R_NilValue;
  bool binverse = LOGICAL(inverse)[0];
  if ((!binverse && isNull(cols)) || (binverse && length(cols)==0)) { // seq_along(x)
    ricols = PROTECT(allocVector(INTSXP, nx)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<nx; i++)
      icols[i] = i+1;
  } else if ((!binverse && length(cols)==0) || (binverse && isNull(cols))) { // integer(0)
    ricols = PROTECT(allocVector(INTSXP, 0)); protecti++;
  } else if (isInteger(cols) || isReal(cols)) {
    if (isInteger(cols)) {
      ricols = cols;
    } else if (isReal(cols)) {
      if (!isRealReallyInt(cols))
        error(_("argument specifying columns is type 'double' and one or more items in it are not whole integers"));
      ricols = PROTECT(coerceVector(cols, INTSXP)); protecti++;
    }
    int *icols = INTEGER(ricols);
    for (int i=0; i<nc; i++) {
      if ((icols[i]>nx) || (icols[i]<1))
        error(_("argument specifying columns specify non existing column(s): cols[%d]=%d"), i+1, icols[i]); // handles NAs also
    }
    if (binverse) { // could be improved
      SEXP notricols = ricols;
      SEXP allcols = PROTECT(range_int(1, nx)); protecti++;
      SEXP setdiff_call = PROTECT(lang3(install("setdiff"), allcols, notricols)); protecti++;
      ricols = PROTECT(eval(setdiff_call, R_GlobalEnv)); protecti++;
    }
  } else if (isString(cols)) {
    SEXP xnames = getAttrib(x, R_NamesSymbol);
    if (isNull(xnames))
      error(_("'x' argument data.table has no names"));
    ricols = PROTECT(chmatch(cols, xnames, 0)); protecti++;
    int *icols = INTEGER(ricols);
    if (!binverse) {
      for (int i=0; i<nc; i++) {
        if (icols[i]==0)
          error(_("argument specifying columns specify non existing column(s): cols[%d]='%s'"), i+1, CHAR(STRING_ELT(cols, i))); // handles NAs also
      }
    } else { // could be improved, handling waning of DT[, -"col_not_there"] adds a lot
      SEXP inot_cols = PROTECT(allocVector(INTSXP, nc)); protecti++;
      int *pinot_cols = INTEGER(inot_cols);
      int not_cols_n = 0;
      for (int i=0; i<nc; i++) {
        if (icols[i]==0)
          pinot_cols[not_cols_n++] = i+1;
      }
      SEXP notricols = R_NilValue;
      if (not_cols_n) {
        SETLENGTH(inot_cols, not_cols_n);
        SEXP not_cols = PROTECT(eval(lang3(install("["), cols, inot_cols), R_GlobalEnv)); protecti++;
        SEXP collapse = PROTECT(lang3(install("paste"), not_cols, ScalarString(PRINTNAME(install(", "))))); protecti++;
        SET_TAG(CDDR(collapse), install("collapse"));
        SEXP collapsed = PROTECT(eval(collapse, R_GlobalEnv)); protecti++;
        warning("column(s) not excluded because not found: %s", CHAR(STRING_ELT(collapsed, 0)));
        notricols = ricols; // setdiff'ed anyway so missing cols can stay here
      } else {
        notricols = ricols;
      }
      SEXP allcols = PROTECT(range_int(1, nx)); protecti++;
      SEXP setdiff_call = PROTECT(lang3(install("setdiff"), allcols, notricols)); protecti++;
      ricols = PROTECT(eval(setdiff_call, R_GlobalEnv)); protecti++;
    }
  } else if (isLogical(cols)) {
    int *lcols = LOGICAL(cols);
    ricols = PROTECT(allocVector(INTSXP, nc)); protecti++;
    int *icols = LOGICAL(ricols);
    int ntrue = 0;
    for (int i=0; i<nc; i++) {
      if (lcols[i]==NA_LOGICAL)
        error(_("argument specifying columns is logical and has NAs"));
      else if ((!binverse && lcols[i]) || (binverse && !lcols[i]))
        icols[ntrue++] = i+1;
    }
    SETLENGTH(ricols, ntrue);
  } else {
    error(_("argument specifying columns must be character, numeric or logical"));
  }
  if (LOGICAL(check_dups)[0] && any_duplicated(ricols, FALSE))
    error(_("argument specifying columns specify duplicated column(s)"));
  UNPROTECT(protecti);
  return ricols;
}

/* replace_dot_alias
 * we don't just simply alias .=list because i) list is a primitive (faster to iterate) and ii) we test for use
 * of "list" in several places so it saves having to remember to write "." || "list" in those places
 * replace_dot_aliasR wrapper should be used to prevent update of input
 */
SEXP replace_dot_alias(SEXP x) {
  if (isLanguage(x) && !isFunction(CAR(x))) {
    if (CAR(x) == sym_bquote) // handling `.` inside bquote, #1912
      return x;
    if (CAR(x) == sym_dot)
      SETCAR(x, sym_list);
    for (SEXP t=x; t!=R_NilValue; t=CDR(t)) {
      if (!isNull(CADR(t)))
        SETCADR(t, replace_dot_alias(CADR(t)));
    }
  }
  return x;
}
SEXP replace_dot_aliasR(SEXP x) {
  return replace_dot_alias(MAYBE_REFERENCED(x) ? duplicate(x) : x);
}

/* funCols
 * helper that calls lapply(DT, fun) to produce logical vector
 */
SEXP funCols(SEXP x, SEXP fun, SEXP rho) {
  SEXP lfn = PROTECT(LCONS(sym_lapply, LCONS(x, LCONS(fun, R_NilValue))));
  SEXP lst = PROTECT(eval(lfn, rho));
  SEXP log = PROTECT(allocVector(LGLSXP, length(x)));
  int *logp = LOGICAL(log);
  for (int i=0; i<length(lst); ++i) {
    SEXP lsti = VECTOR_ELT(lst, i);
    if (length(lsti)!=1 || !isLogical(lsti) || LOGICAL(lsti)[0]==NA_LOGICAL)
      error("When argument for columns selection is a function, it is applied to each column; the output of this function must be a non-missing boolean scalar signalling inclusion/exclusion of the column. However, these conditions were not met at least for: %s", CHAR(STRING_ELT(getAttrib(x, R_NamesSymbol), i)));
    logp[i] = LOGICAL(lsti)[0];
  }
  UNPROTECT(3);
  return log;
}

/* exprCols
 * turns NSE expression into columns int
 * helper for j and .SDcols
 * handles v1, "v1", v1:v3, !"v1", -"v1", paste0("v",1:3)
 * does not evaluate only for `symbol:symbol` expression, or `symbol` when "symbol" is existing column
 * with is updated by reference thus should be copied before being passed to this function
 */
SEXP exprCols(SEXP x, SEXP expr, SEXP mode, SEXP with, SEXP rho) {
  int protecti=0;
  if (!isString(mode) || length(mode)!=1 || STRING_ELT(mode, 0)==NA_STRING)
    error(_("internal error: mode argument has to be character, non-NA of length 1")); // # nocov
  if (isNull(expr))
    error(_("columns selection is NULL")); // expr=NULL
  if (!isNewList(x))
    error(_("'x' argument must be data.table"));
  bool mode_j=false, mode_sd=false;
  if (strcmp(CHAR(STRING_ELT(mode, 0)), "j")==0)
    mode_j = true;
  else if (strcmp(CHAR(STRING_ELT(mode, 0)), ".SDcols")==0)
    mode_sd = true;
  else
    error(_("Internal error: invalid 'mode' argument in exprCols function, should have been caught before. please report to data.table issue tracker.")); // # nocov
  // colnamesInt handles inverse selection: !cols_var, !"V2", !V2, !paste0("V",2:3)
  bool inverse = isLanguage(expr) && (CAR(expr)==sym_bang || (CAR(expr)==sym_minus && length(expr)==2)); // length(expr)==2, otherwise V3-V2 will pick it up
  if (inverse)
    expr = CADR(expr);
  bool peeled = false;
  SEXP sym_parenthesis = install("(");
  while (isLanguage(expr) && CAR(expr)==sym_parenthesis) {
    expr = CADR(expr);
    peeled = true;
  }
  SEXP sym_brackets = install("{");
  while (isLanguage(expr) && CAR(expr)==sym_brackets && length(expr)==2) {
    expr = CADR(expr); // verify #376, 2121.2
    peeled = true;
  }
  SEXP sym_eval = install("eval"); // test num??
  if (isLanguage(expr) && CAR(expr)==sym_eval) {
    error("to do, eval call in j/.sdcols");
  }
  if (isNull(expr))
    error(_("columns selection is NULL")); // expr=((NULL))
  // non-evaluated case: V3:V2
  // single symbol V2 may or may not be evaluated, see below
  if (isLanguage(expr) && CAR(expr)==sym_colon && ((mode_j && (!peeled || inverse)) || mode_sd)) {  // 3:2, V3:V2, min(V3):min(V2)
    SEXP lhs = CADR(expr), rhs = CADDR(expr);
    if ((isSymbol(lhs) && isSymbol(rhs)) || ((isInteger(lhs) || isReal(lhs)) && (isInteger(rhs) || isReal(rhs)))) {
      if (isSymbol(lhs) && isSymbol(rhs)) { // V3:V2
        lhs = colnamesInt(x, ScalarString(PRINTNAME(lhs)), ScalarLogical(false), ScalarLogical(false)); // may raise error if lhs column does not exists
        rhs = colnamesInt(x, ScalarString(PRINTNAME(rhs)), ScalarLogical(false), ScalarLogical(false));
      } else { // 3:2
        lhs = colnamesInt(x, lhs, ScalarLogical(false), ScalarLogical(false));
        rhs = colnamesInt(x, rhs, ScalarLogical(false), ScalarLogical(false));
      }
      if (!isInteger(lhs) || !isInteger(rhs) || length(lhs)!=1 || length(rhs)!=1 || LOGICAL(lhs)[0]==NA_LOGICAL || LOGICAL(rhs)[0]==NA_LOGICAL)
        error(_("internal error: LHS and RHS of `:` call should be integer non-NA scalars already")); // # nocov
      SEXP ricols = PROTECT(range_int(INTEGER(lhs)[0], INTEGER(rhs)[0])); protecti++;
      LOGICAL(with)[0] = 0;
      UNPROTECT(protecti);
      return colnamesInt(x, ricols, ScalarLogical(false), ScalarLogical(inverse)); // handle inverse
    } else {
      Rprintf("sym_colon to evaluate later, a valid colon expression\n");
      LOGICAL(with)[0] = 1;
      // evaluates later on: 3:2, f(V3):f(V2)
      //Rf_PrintValue(lhs);
      //Rf_PrintValue(rhs);
    }
  }
  // patterns will not evaluate as well because we wrap expr into quote(expr) before calling eval on do_patterns call
  SEXP sym_patterns = install("patterns");
  if (isLanguage(expr) && CAR(expr)==sym_patterns && mode_sd) {
    SEXP xnames = getAttrib(x, R_NamesSymbol);
    if (isNull(xnames))
      error(_("'x' argument data.table has no names"));
    SEXP sym_quote = install("quote"); // .SDcols = Reduce(intersect, do_patterns(colsub, names_x))
    SEXP sym_do_patterns = install("do_patterns");
    SEXP lpat = PROTECT(eval(LCONS(sym_do_patterns, LCONS(LCONS(sym_quote, LCONS(expr, R_NilValue)), LCONS(xnames, R_NilValue))), rho)); protecti++;
    SEXP sym_Reduce = install("Reduce");
    SEXP sym_intersect = install("intersect");
    SEXP ricols = eval(LCONS(sym_Reduce, LCONS(sym_intersect, LCONS(lpat, R_NilValue))), rho);
    LOGICAL(with)[0] = 0;
    UNPROTECT(protecti);
    return colnamesInt(x, ricols, ScalarLogical(false), ScalarLogical(inverse)); // handle inverse
  }
  // single symbol V2 might be also a function, to check that we need to evaluate, thus first we check if "V2" is existing column name, if not then we evaluate to see if it is a function.
  // adding support of 'with' argument here may improve control, when with=FALSE then we could evaluate straight away
  //Rprintf("symbol testing\n");
  SEXP expr_sym = R_NilValue, expr_sym_match = R_NilValue;
  if (isSymbol(expr) && mode_j) {  // V1, is.numeric
    SEXP xnames = getAttrib(x, R_NamesSymbol);
    if (isNull(xnames))
      error(_("'x' argument data.table has no names"));
    expr_sym = PROTECT(ScalarString(PRINTNAME(expr))); protecti++; // "V1", "is.numeric"
    expr_sym_match = PROTECT(chmatch(expr_sym, xnames, 0)); protecti++;
    if (INTEGER(expr_sym_match)[0]!=0) {
      if (inverse) {
        LOGICAL(with)[0] = 1;
        UNPROTECT(protecti);
        return R_NilValue;
      } else {
        LOGICAL(with)[0] = 0;
        UNPROTECT(protecti);
        // no need to handle inverse because -V1 and !V1 should contiunue evaluation
        //colnamesInt(x, expr_sym_match, ScalarLogical(false), ScalarLogical(inverse)); // handle inverse
        return expr_sym_match;
      }
    }
  }
  /*
   * there is a clash in handling j and .SDcols in the single place
   * problem lies in the fact that .SDcols needs to evaluate to figure out that argument supplied is a function
   * on the other hand j should not evaluate calls (other than `c`, `paste`, etc.), but just symbol (and when not already exists in dt names)
   * because to aim is to know a set of columns that user attempts to get, not evaluate a j expression
   * thus branching below
   */
  SEXP cols = R_NilValue;
  SEXP value = R_NilValue;
  // various cases where all have to be evaluated: c("V1","V2"), paste0("V",1:2), is.numeric, function(x) is.numeric(x), also `cols` symbol when there is no column named "cols" (INTEGER(expr_sym_match)[0]), or already evaluated "V1"
  if (mode_j) {
    if (isLanguage(expr) || isSymbol(expr)) {
      if (isLanguage(expr) && !peeled && ( // !peeled required to escape eval of c("V1","V3") as (c("V1","V3"))
          CAR(expr)==install("c") || CAR(expr)==install("paste") || CAR(expr)==install("paste0")
      )) {
        /*
         ( (!length(av<-all.vars(jsub)) || all(substring(av,1L,2L)=="..")) &&
         root %chin% c("","c","paste","paste0","-","!") &&
         missingby )
         */
        value = PROTECT(eval(expr, rho)); protecti++;  // evaluated value: c(2L,3L), c("V1","V2"), but not call objects anymore
      } else {
        LOGICAL(with)[0] = 1;
        return R_NilValue;
      }
    } else { // "V1,V2" should evaluate, unless in by
      if (!peeled) {
        value = expr; // already evaluated
      } else { // (("V1,V2"))
        LOGICAL(with)[0] = 1;
        return R_NilValue;
      }
    }
  } else if (mode_sd) {
    value = PROTECT(eval(expr, rho)); protecti++;
  }
  if (isFunction(value) && mode_sd) { // expr could be either symbol or language: f, function(x) x
    cols = PROTECT(funCols(x, expr, rho)); protecti++;
  } else {
    cols = value;
  }
  if (isNull(cols))
    error("internal error: cols should not be NULL by now"); // # or could be NULL when .SD provided, but eval(.SD) may already resolve that, no nocov for now
  UNPROTECT(protecti);
  return colnamesInt(x, cols, ScalarLogical(false), ScalarLogical(inverse));
}

void coerceFill(SEXP fill, double *dfill, int32_t *ifill, int64_t *i64fill) {
  if (xlength(fill) != 1) error(_("%s: fill argument must be length 1"), __func__);
  if (isInteger(fill)) {
    if (INTEGER(fill)[0]==NA_INTEGER) {
      ifill[0] = NA_INTEGER; dfill[0] = NA_REAL; i64fill[0] = NA_INTEGER64;
    } else {
      ifill[0] = INTEGER(fill)[0];
      dfill[0] = (double)(INTEGER(fill)[0]);
      i64fill[0] = (int64_t)(INTEGER(fill)[0]);
    }
  } else if (isReal(fill)) {
    if (Rinherits(fill,char_integer64)) {  // Rinherits true for nanotime
      int64_t rfill = ((int64_t *)REAL(fill))[0];
      if (rfill==NA_INTEGER64) {
        ifill[0] = NA_INTEGER; dfill[0] = NA_REAL; i64fill[0] = NA_INTEGER64;
      } else {
        ifill[0] = (rfill>INT32_MAX || rfill<=INT32_MIN) ? NA_INTEGER : (int32_t)rfill;
        dfill[0] = (double)rfill;
        i64fill[0] = rfill;
      }
    } else {
      double rfill = REAL(fill)[0];
      if (ISNAN(rfill)) {
        // NA -> NA, NaN -> NaN
        ifill[0] = NA_INTEGER; dfill[0] = rfill; i64fill[0] = NA_INTEGER64;
      } else {
        ifill[0] = (!R_FINITE(rfill) || rfill>INT32_MAX || rfill<=INT32_MIN) ? NA_INTEGER : (int32_t)rfill;
        dfill[0] = rfill;
        i64fill[0] = (!R_FINITE(rfill) || rfill>(double)INT64_MAX || rfill<=(double)INT64_MIN) ? NA_INTEGER64 : (int64_t)rfill;
      }
    }
  } else if (isLogical(fill) && LOGICAL(fill)[0]==NA_LOGICAL) {
    ifill[0] = NA_INTEGER; dfill[0] = NA_REAL; i64fill[0] = NA_INTEGER64;
  } else {
    error(_("%s: fill argument must be numeric"), __func__);
  }
}
SEXP coerceFillR(SEXP fill) {
  int protecti=0;
  double dfill=NA_REAL;
  int32_t ifill=NA_INTEGER;
  int64_t i64fill=NA_INTEGER64;
  coerceFill(fill, &dfill, &ifill, &i64fill);
  SEXP ans = PROTECT(allocVector(VECSXP, 3)); protecti++;
  SET_VECTOR_ELT(ans, 0, allocVector(INTSXP, 1));
  SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(ans, 2, allocVector(REALSXP, 1));
  INTEGER(VECTOR_ELT(ans, 0))[0] = ifill;
  REAL(VECTOR_ELT(ans, 1))[0] = dfill;
  ((int64_t *)REAL(VECTOR_ELT(ans, 2)))[0] = i64fill;
  setAttrib(VECTOR_ELT(ans, 2), R_ClassSymbol, ScalarString(char_integer64));
  UNPROTECT(protecti);
  return ans;
}

inline bool INHERITS(SEXP x, SEXP char_) {
  // Thread safe inherits() by pre-calling install() in init.c and then
  // passing those char_* in here for simple and fast non-API pointer compare.
  // The thread-safety aspect here is only currently actually needed for list columns in
  // fwrite() where the class of the cell's vector is tested; the class of the column
  // itself is pre-stored by fwrite (for example in isInteger64[] and isITime[]).
  // Thread safe in the limited sense of correct and intended usage :
  // i) no API call such as install() or mkChar() must be passed in.
  // ii) no attrib writes must be possible in other threads.
  SEXP klass;
  if (isString(klass = getAttrib(x, R_ClassSymbol))) {
    for (int i=0; i<LENGTH(klass); i++) {
      if (STRING_ELT(klass, i) == char_) return true;
    }
  }
  return false;
}

bool Rinherits(SEXP x, SEXP char_) {
  // motivation was nanotime which is S4 and inherits from integer64 via S3 extends
  // R's C API inherits() does not cover S4 and returns FALSE for nanotime, as does our own INHERITS above.
  // R's R-level inherits() calls objects.c:inherits2 which calls attrib.c:R_data_class2 and
  // then attrib.c:S4_extends which itself calls R level methods:::.extendsForS3 which then calls R level methods::extends.
  // Since that chain of calls is so complicated and involves evaluating R level anyway, let's just reuse it.
  // Rinherits prefix with 'R' to signify i) it may call R level and is therefore not thread safe, and ii) includes R level inherits which covers S4.
  bool ans = INHERITS(x, char_);        // try standard S3 class character vector first
  if (!ans && char_==char_integer64)    // save the eval() for known S4 classes that inherit from integer64
    ans = INHERITS(x, char_nanotime);   // comment this out to test the eval() works for nanotime
  if (!ans && IS_S4_OBJECT(x)) {        // if it's not S4 we can save the overhead of R eval()
    SEXP vec = PROTECT(ScalarString(char_));           // TODO: cover this branch by making two new test S4 classes: one that
    SEXP call = PROTECT(lang3(sym_inherits, x, vec));  //       does inherit from integer64 and one that doesn't
    ans = LOGICAL(eval(call, R_GlobalEnv))[0]==1;
    UNPROTECT(2);
  }
  return ans;
}

SEXP copyAsPlain(SEXP x) {
  // v1.12.2 and before used standard R duplicate() to do this. But that's not guaranteed to not return an ALTREP.
  // e.g. ALTREP 'wrapper' on factor column (with materialized INTSXP) in package VIM under example(hotdeck)
  //      .Internal(inspect(x[[5]]))
  //      @558adf4d9508 13 INTSXP g0c0 [OBJ,NAM(7),ATT]  wrapper [srt=-2147483648,no_na=0]
  // 'AsPlain' is intended to convey unALTREP-ing; i.e. materializing and removing any ALTREP attributes too
  // For non-ALTREP this should do the same as R's duplicate(); but doesn't quite currently, so has to divert to duplicated() for now
  // Intended for use on columns; to either un-ALTREP them or duplicate shared memory columns; see copySharedColumns() below
  // Not intended to be called on a DT VECSXP where a concept of 'deep' might refer to whether the columns are copied

  if (!ALTREP(x)) return duplicate(x);
  // would prefer not to have this line, but without it test 1639.064 fails :
  //   Running test id 1639.064      Error in `[.data.table`(r, -ii) :
  //   Item 2 of i is -1 and item 1 is NA. Cannot mix negatives and NA.
  //   Calls: test.data.table ... FUN -> make.levels -> rbindlist -> [ -> [.data.table
  // Perhaps related to row names and the copyMostAttrib() below is not quite sufficient

  size_t n = XLENGTH(x);
  SEXP ans = PROTECT(allocVector(TYPEOF(x), XLENGTH(x)));
  switch (TYPEOF(ans)) {
  case RAWSXP:
    memcpy(RAW(ans),     RAW(x),     n*sizeof(Rbyte));           // # nocov; add coverage when ALTREP is turned on for all types
    break;                                                       // # nocov
  case LGLSXP:
    memcpy(LOGICAL(ans), LOGICAL(x), n*sizeof(Rboolean));        // # nocov
    break;                                                       // # nocov
  case INTSXP:
    memcpy(INTEGER(ans), INTEGER(x), n*sizeof(int));             // covered by 10:1 after test 178
    break;
  case REALSXP:
    memcpy(REAL(ans),    REAL(x),    n*sizeof(double));          // covered by as.Date("2013-01-01")+seq(1,1000,by=10) after test 1075
    break;
  case CPLXSXP:
    memcpy(COMPLEX(ans), COMPLEX(x), n*sizeof(Rcomplex));        // # nocov
    break;                                                       // # nocov
  case STRSXP: {
    const SEXP *xp=STRING_PTR(x);                                // covered by as.character(as.hexmode(1:500)) after test 642
    for (R_xlen_t i=0; i<n; ++i) SET_STRING_ELT(ans, i, xp[i]);
  } break;
  case VECSXP: {
    const SEXP *xp=VECTOR_PTR(x);                                // # nocov
    for (R_xlen_t i=0; i<n; ++i) SET_VECTOR_ELT(ans, i, xp[i]);  // # nocov
  } break;                                                       // # nocov
  default:
    error(_("Internal error: unsupported type '%s' passed to copyAsPlain()"), type2char(TYPEOF(x))); // # nocov
  }
  copyMostAttrib(x, ans); // e.g. factor levels, class etc, but not names, dim or dimnames
  if (ALTREP(ans))
    error(_("Internal error: type '%s' passed to copyAsPlain() but it seems copyMostAttrib() retains ALTREP attributes"), type2char(TYPEOF(x))); // # nocov
  UNPROTECT(1);
  return ans;
}

void copySharedColumns(SEXP x) {
  const int ncol = length(x);
  if (!isNewList(x) || ncol==1) return;
  bool *shared = (bool *)R_alloc(ncol, sizeof(bool)); // on R heap in case alloc fails
  int *savetl = (int *)R_alloc(ncol, sizeof(int));  // on R heap for convenience but could be a calloc
  int nShared=0, thistl=0;
  const SEXP *xp = VECTOR_PTR(x);
  for (int i=0; i<ncol; ++i) {
    SEXP thiscol = xp[i];
    if (ALTREP(thiscol) || (thistl=TRUELENGTH(thiscol))<0) {
      shared[i] = true;
      nShared++;
      // do not copyAsPlain() here as the alloc might fail: careful to restore tl first to all columns.
      // Aside: thistl is which column shares the same address as this one in case that's ever useful in future.
    } else {
      shared[i] = false;
      savetl[i] = thistl;  // these are vectors which are all expected to have tl, unlike CHARSXP which often don't (savetl() has CHARSXP in mind)
      SET_TRUELENGTH(thiscol, -i-1);  // just on plain vectors, not on ALTREP
    }
  }
  // now we know nShared and which ones they are (if any), restore original tl back to all columns
  for (int i=0; i<ncol; ++i) {
    if (!shared[i]) SET_TRUELENGTH(VECTOR_ELT(x, i), savetl[i]);
  }
  // now that truelength has been restored for all columns, we can finally call copyAsPlain()
  if (nShared) {
    for (int i=0; i<ncol; ++i) {
      if (shared[i])
        SET_VECTOR_ELT(x, i, copyAsPlain(VECTOR_ELT(x, i)));
    }
    if (GetVerbose()) Rprintf(_("Found and copied %d column%s with a shared memory address\n"), nShared, nShared>1?"s":"");
    // GetVerbose() (slightly expensive call of all options) called here only when needed
  }
}

// lock, unlock and islocked at C level :
// 1) for speed to reduce overhead
// 2) to avoid an R level wrapper which bumps MAYBE_SHARED; see the unlock after eval(jval) in data.table.R, #1341 #2245
SEXP lock(SEXP DT) {
  setAttrib(DT, sym_datatable_locked, ScalarLogical(TRUE));
  return DT;
}
SEXP unlock(SEXP DT) {
  setAttrib(DT, sym_datatable_locked, R_NilValue);
  return DT;
}
bool islocked(SEXP DT) {
  SEXP att = getAttrib(DT, sym_datatable_locked);
  return isLogical(att) && LENGTH(att)==1 && LOGICAL(att)[0]==1;
}
SEXP islockedR(SEXP DT) {
  return ScalarLogical(islocked(DT));
}

bool need2utf8(SEXP x) {
  const int xlen = length(x);
  SEXP *xd = STRING_PTR(x);
  for (int i=0; i<xlen; i++) {
    if (NEED2UTF8(xd[i]))
      return(true);
  }
  return(false);
}

SEXP coerceUtf8IfNeeded(SEXP x) {
  if (!need2utf8(x))
    return(x);
  const int xlen = length(x);
  SEXP ans = PROTECT(allocVector(STRSXP, xlen));
  SEXP *xd = STRING_PTR(x);
  for (int i=0; i<xlen; i++) {
    SET_STRING_ELT(ans, i, ENC2UTF8(xd[i]));
  }
  UNPROTECT(1);
  return(ans);
}
