#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

extern size_t sizes[100];
#define SIZEOF(x) sizes[TYPEOF(x)]

extern SEXP chmatch(SEXP x, SEXP table, R_len_t nomatch, Rboolean in);
extern SEXP allocNAVector(SEXPTYPE type, R_len_t n);
extern SEXP coerce_to_char(SEXP s, SEXP env);

// generate from 1 to n (a simple fun for melt, vecseq is convenient from R due to SEXP inputs)
SEXP seq_int(int n, int start) {
    SEXP ans = R_NilValue;
    int i;
    if (n <= 0) return(ans);
    PROTECT(ans = allocVector(INTSXP, n));
    for (i=0; i<n; i++) INTEGER(ans)[i] = start+i;
    UNPROTECT(1);
    return(ans);
}

// very specific "set_diff" for integers
SEXP set_diff(SEXP x, int n) {
    SEXP ans, xmatch;
    int i, j = 0, *buf;
    if (TYPEOF(x) != INTSXP) error("'x' must be an integer");
    if (n <= 0) error("'n' must be a positive integer");
    xmatch = match(x, seq_int(n, 1), 0); // took a while to realise: matches vec against x - thanks to comment from Matthew in assign.c!
    
    buf = (int *) R_alloc(n, sizeof(int));
    for (i=0; i<n; i++) {
        if (INTEGER(xmatch)[i] == 0) {
            buf[j++] = i+1;
        }
    }
    n = j;
    PROTECT(ans = allocVector(INTSXP, n));
    memcpy(INTEGER(ans), buf, sizeof(int) * n); // sizeof is of type size_t - no integer overflow issues
    UNPROTECT(1);
    return(ans);
}

// plucked and modified from base (coerce.c and summary.c). 
// for melt's `na.rm=TRUE` option
SEXP which_notNA(SEXP x) {
    SEXP v, ans;
    int i, j=0, n = length(x), *buf;
    
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
    
    buf = (int *) R_alloc(n, sizeof(int));
    for (i = 0; i < n; i++) {
        if (LOGICAL(v)[i] == TRUE) {
            buf[j] = i + 1;
            j++;
        }
    }
    n = j;
    PROTECT(ans = allocVector(INTSXP, n));
    memcpy(INTEGER(ans), buf, sizeof(int) * n);
    
    UNPROTECT(2);
    return(ans);
}

SEXP which(SEXP x) {
    
    int i, j=0, n = length(x), *buf;
    SEXP ans;
    if (!isLogical(x)) error("argument to 'which' must be logical");
    buf = (int *) R_alloc(n, sizeof(int));
    for (i = 0; i < n; i++) {
        if (LOGICAL(x)[i] == TRUE) {
            buf[j] = i + 1;
            j++;
        }
    }
    n = j;
    PROTECT(ans = allocVector(INTSXP, n));
    memcpy(INTEGER(ans), buf, sizeof(int) * n);
    
    UNPROTECT(1);
    return(ans);
}

// hack by calling paste using eval. could change this to strcat, but not sure about buffer size for large data.tables... Any ideas Matthew?
SEXP concat(SEXP vec, SEXP idx) {
    
    SEXP s, t, v;
    int i;
    
    if (TYPEOF(vec) != STRSXP) error("concat: 'vec must be a character vector");
    if (!isInteger(idx) || length(idx) < 0) error("concat: 'idx' must be an integer vector of length >= 0");
    for (i=0; i<length(idx); i++) {
        if (INTEGER(idx)[i] < 0 || INTEGER(idx)[i] > length(vec)) 
            error("concat: 'idx' must take values between 0 and length(vec); 0 <= idx <= length(vec)");
    }
    PROTECT(v = allocVector(STRSXP, length(idx)));
    for (i=0; i<length(idx); i++) {
        SET_STRING_ELT(v, i, STRING_ELT(vec, INTEGER(idx)[i]-1));
    }
    PROTECT(t = s = allocList(3));
    SET_TYPEOF(t, LANGSXP);
    SETCAR(t, install("paste")); t = CDR(t);
    SETCAR(t, v); t = CDR(t);
    SETCAR(t, mkString(", "));
    SET_TAG(t, install("collapse"));
    UNPROTECT(2); // v, (t,s)
    return(eval(s, R_GlobalEnv));
}

SEXP checkVars(SEXP DT, SEXP id, SEXP measure, Rboolean verbose) {
    int i, ncol=LENGTH(DT), targetcols=0, protecti=0, u=0, v=0;
    SEXP thiscol, idcols = R_NilValue, valuecols = R_NilValue, tmp, booltmp, unqtmp, ans;
    SEXP dtnames = getAttrib(DT, R_NamesSymbol);
    
    if (isNull(id) && isNull(measure)) {
        for (i=0; i<ncol; i++) {
            thiscol = VECTOR_ELT(DT, i);
            if ((isInteger(thiscol) || isNumeric(thiscol) || isLogical(thiscol)) && !isFactor(thiscol)) targetcols++;
        }
        PROTECT(idcols = allocVector(INTSXP, ncol-targetcols)); protecti++;
        PROTECT(valuecols = allocVector(INTSXP, targetcols)); protecti++;
        for (i=0; i<ncol; i++) {
            thiscol = VECTOR_ELT(DT, i);
            if ((isInteger(thiscol) || isNumeric(thiscol) || isLogical(thiscol)) && !isFactor(thiscol)) {
                INTEGER(valuecols)[u++] = i+1;
            } else
                INTEGER(idcols)[v++] = i+1;
        }
        warning("To be consistent with reshape2's melt, id.vars and measure.vars are internally guessed when both are 'NULL'. All non-numeric/integer/logical type columns are conisdered id.vars, which in this case are columns '%s'. Consider providing at least one of 'id' or 'measure' vars in future.", CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
    } else if (!isNull(id) && isNull(measure)) {
        switch(TYPEOF(id)) {
            case STRSXP  : PROTECT(tmp = chmatch(id, dtnames, 0, FALSE)); protecti++; break;
            case REALSXP : PROTECT(tmp = coerceVector(id, INTSXP)); protecti++; break;
            case INTSXP  : PROTECT(tmp = id); protecti++; break;
            default : error("Unknown 'id.var' type %s, must be character or integer vector", type2char(TYPEOF(id)));
        }
        PROTECT(booltmp = duplicated(tmp, FALSE)); protecti++;
        for (i=0; i<length(tmp); i++) {
            if (INTEGER(tmp)[i] <= 0) error("Column '%s' not found in 'data'", CHAR(STRING_ELT(id, i)));
            else if (INTEGER(tmp)[i] > ncol) error("id.var value exceeds ncol(data)");
            else if (!LOGICAL(booltmp)[i]) targetcols++;
            else continue;
        }
        PROTECT(unqtmp = allocVector(INTSXP, targetcols)); protecti++;
        u = 0;
        for (i=0; i<length(booltmp); i++) {
            if (!LOGICAL(booltmp)[i]) {
                INTEGER(unqtmp)[u++] = INTEGER(tmp)[i];
            }
        }
        PROTECT(valuecols = set_diff(unqtmp, ncol)); protecti++;
        PROTECT(idcols = tmp); protecti++;
        if (verbose) Rprintf("'measure.var' is missing. Assigning all columns other than 'id.var' columns which are %s as 'measure.var'.\n", CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
    } else if (isNull(id) && !isNull(measure)) {
        switch(TYPEOF(measure)) {
            case STRSXP  : PROTECT(tmp = chmatch(measure, dtnames, 0, FALSE)); protecti++; break;
            case REALSXP : PROTECT(tmp = coerceVector(measure, INTSXP)); protecti++; break;
            case INTSXP  : PROTECT(tmp = measure); protecti++; break;
            default : error("Unknown 'measure.var' type %s, must be character or integer vector", type2char(TYPEOF(measure)));
        }
        PROTECT(booltmp = duplicated(tmp, FALSE)); protecti++;
        for (i=0; i<length(tmp); i++) {
            if (INTEGER(tmp)[i] <= 0) error("Column '%s' not found in 'data'", CHAR(STRING_ELT(id, i)));
            else if (INTEGER(tmp)[i] > ncol) error("measure.var value exceeds ncol(data)");
            else if (!LOGICAL(booltmp)[i]) targetcols++;
            else continue;
        }
        PROTECT(unqtmp = allocVector(INTSXP, targetcols)); protecti++;
        u = 0;
        for (i=0; i<length(booltmp); i++) {
            if (!LOGICAL(booltmp)[i]) {
                INTEGER(unqtmp)[u++] = INTEGER(tmp)[i];
            }
        }
        PROTECT(idcols = set_diff(unqtmp, ncol)); protecti++;
        PROTECT(valuecols = tmp); protecti++;
        if (verbose) Rprintf("'id.var' is missing. Assigning all columns other than 'measure.var' columns as 'id.var'. Assigned 'id.var's are %s.\n", CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
    } else if (!isNull(id) && !isNull(measure)) {
        switch(TYPEOF(id)) {
            case STRSXP  : PROTECT(tmp = chmatch(id, dtnames, 0, FALSE)); protecti++; break;
            case REALSXP : PROTECT(tmp = coerceVector(id, INTSXP)); protecti++; break;
            case INTSXP  : PROTECT(tmp = id); protecti++; break;
            default : error("Unknown 'id.var' type %s, must be character or integer vector", type2char(TYPEOF(id)));
        }
        for (i=0; i<length(tmp); i++) {
            if (INTEGER(tmp)[i] <= 0) error("Column '%s' or not found in 'data'", CHAR(STRING_ELT(id, i)));
            else if (INTEGER(tmp)[i] > ncol) error("measure.var value exceeds ncol(data)");
        }
        PROTECT(idcols = allocVector(INTSXP, length(tmp))); protecti++;
        idcols = tmp;
        switch(TYPEOF(measure)) {
            case STRSXP  : PROTECT(tmp = chmatch(measure, dtnames, 0, FALSE)); protecti++; break;
            case REALSXP : PROTECT(tmp = coerceVector(measure, INTSXP)); protecti++; break;
            case INTSXP  : PROTECT(tmp = measure); protecti++; break;
            default : error("Unknown 'measure.var' type %s, must be character or integer vector", type2char(TYPEOF(measure)));
        }
        for (i=0; i<length(tmp); i++) {
            if (INTEGER(tmp)[i] <= 0) error("Column '%s' not found in 'data'", CHAR(STRING_ELT(id, i)));
            else if (INTEGER(tmp)[i] > ncol) error("measure.var value exceeds ncol(data)");
        }
        PROTECT(valuecols = allocVector(INTSXP, length(measure))); protecti++;
        valuecols = tmp;
    }

    PROTECT(ans = allocVector(VECSXP, 2)); protecti++;
    SET_VECTOR_ELT(ans, 0, idcols);
    SET_VECTOR_ELT(ans, 1, valuecols);
    UNPROTECT(protecti);
    return(ans);
}

SEXP fmelt(SEXP DT, SEXP id, SEXP measure, SEXP varfactor, SEXP valfactor, SEXP var_name, SEXP val_name, SEXP na_rm, SEXP drop_levels, SEXP print_out) {
    
    int i, j, k, nrow, ncol, protecti=0, lids=-1, lvalues=-1, totlen=0, counter=0, thislen=0;
    SEXP thiscol, ans, dtnames, ansnames, idcols, valuecols, levels, factorLangSxp;
    SEXP vars, target, idxkeep = R_NilValue, thisidx = R_NilValue;
    Rboolean isfactor=FALSE, isidentical=TRUE, narm = FALSE, droplevels=FALSE, verbose=FALSE;
    SEXPTYPE valtype=NILSXP;
    size_t size;

    if (TYPEOF(DT) != VECSXP) error("Input is not of type VECSXP, expected a data.table, data.frame or list");
    if (TYPEOF(valfactor) != LGLSXP) error("Argument 'value.factor' should be logical TRUE/FALSE");
    if (TYPEOF(varfactor) != LGLSXP) error("Argument 'variable.factor' should be logical TRUE/FALSE");
    if (TYPEOF(na_rm) != LGLSXP) error("Argument 'na.rm' should be logical TRUE/FALSE");
    if (LOGICAL(na_rm)[0] == TRUE) narm = TRUE;
    if (TYPEOF(print_out) != LGLSXP) error("Argument 'verbose' should be logical TRUE/FALSE");
    if (LOGICAL(print_out)[0] == TRUE) verbose = TRUE;
    // check for var and val names
    if (TYPEOF(var_name) != STRSXP || length(var_name) != 1) error("Argument 'variable.name' must be a character vector of length 1");
    if (TYPEOF(val_name) != STRSXP || length(val_name) != 1) error("Argument 'value.name' must be a character vector of length 1");

    // droplevels future feature request, maybe... should ask on data.table-help
    // if (!isLogical(drop_levels)) error("Argument 'drop.levels' should be logical TRUE/FALSE");
    // if (LOGICAL(drop_levels)[0] == TRUE) droplevels = TRUE;
    // if (droplevels && !narm) warning("Ignoring argument 'drop.levels'. 'drop.levels' should be set to remove any unused levels as a result of setting 'na.rm=TRUE'. Here there is nothing to do because 'na.rm=FALSE'");
    
    ncol = LENGTH(DT);
    nrow = length(VECTOR_ELT(DT, 0));
    if (ncol <= 0) {
        warning("ncol(data) is 0. Nothing to do, returning original data.table.");
        return(DT);
    }
    PROTECT(dtnames = getAttrib(DT, R_NamesSymbol)); protecti++;
    if (isNull(dtnames)) error("names(data) is NULL. Please report to data.table-help");
    
    vars = checkVars(DT, id, measure, verbose);
    PROTECT(idcols = VECTOR_ELT(vars, 0)); protecti++;
    PROTECT(valuecols = VECTOR_ELT(vars, 1)); protecti++; // <~~~ not protecting vars leads to  segfault (on big data)
    
    lids = length(idcols);
    lvalues = length(valuecols);
    
    // edgecase where lvalues = 0 and lids > 0
    if (lvalues == 0 && lids > 0) {
        if (verbose) Rprintf("length(measure.var) is 0. Edge case detected. Nothing to melt. Returning data.table with all 'id.vars' which are columns %s\n", CHAR(STRING_ELT(concat(dtnames, idcols), 0)));
        PROTECT(ansnames = allocVector(STRSXP, lids)); protecti++;
        PROTECT(ans = allocVector(VECSXP, lids)); protecti++;
        for (i=0; i<lids; i++) {
            SET_VECTOR_ELT(ans, i, VECTOR_ELT(DT, INTEGER(idcols)[i]-1));
            SET_STRING_ELT(ansnames, i, STRING_ELT(dtnames, INTEGER(idcols)[i]-1));
        }
        setAttrib(ans, R_NamesSymbol, ansnames);
        UNPROTECT(protecti);
        return(ans);
    }
    if (lvalues == 0 && lids == 0 && verbose)
        Rprintf("length(measure.var) and length(id.var) are both 0. Edge case detected. Nothing to melt.\n"); // <~~ don't think this will ever happen though with all the checks
    // set names for 'ans' - the output list
    PROTECT(ansnames = allocVector(STRSXP, lids+2)); protecti++;
    for (i=0; i<lids; i++) {
        SET_STRING_ELT(ansnames, i, STRING_ELT(dtnames, INTEGER(idcols)[i]-1));
    }
    SET_STRING_ELT(ansnames, lids, mkChar(CHAR(STRING_ELT(var_name, 0)))); // mkChar("variable")
    SET_STRING_ELT(ansnames, lids+1, mkChar(CHAR(STRING_ELT(val_name, 0)))); // mkChar("value")
    
    // get "value" column
    for (i=0; i<lvalues; i++) {
        thiscol = VECTOR_ELT(DT, INTEGER(valuecols)[i]-1);
        if (!isfactor && isFactor(thiscol)) isfactor = TRUE;
        if (TYPEOF(thiscol) > valtype) valtype = TYPEOF(thiscol);
    }
    if (isfactor && valtype != VECSXP) valtype = STRSXP;

    for (i=0; i<lvalues; i++) {
        thiscol = VECTOR_ELT(DT, INTEGER(valuecols)[i]-1);
        if (TYPEOF(thiscol) != valtype && isidentical) {
            if (!(isFactor(thiscol) && valtype == STRSXP)) {
                isidentical = FALSE; // for Date like column (not implemented for now)
                warning("All 'measure.vars are NOT of the SAME type. By order of hierarchy, the molten data value column will be of type '%s'. Therefore all measure variables that are not of type '%s' will be coerced to. Check the DETAILS section of ?melt.data.table for more on coercion.\n", type2char(valtype), type2char(valtype));
                break;
            }
        }
    }

    if (valtype == VECSXP && narm) {
        narm = FALSE;
        if (verbose) Rprintf("The molten data value type is a list. 'na.rm=TRUE' is therefore ignored.\n");
    }
    if (narm) {
        PROTECT(idxkeep = allocVector(VECSXP, lvalues)); protecti++;
        for (i=0; i<lvalues; i++) {
            SET_VECTOR_ELT(idxkeep, i, which_notNA(VECTOR_ELT(DT, INTEGER(valuecols)[i]-1)));
            totlen += length(VECTOR_ELT(idxkeep, i));
        }
    } else 
        totlen = nrow * lvalues;
    
    PROTECT(ans = allocVector(VECSXP, lids + 2)); protecti++;
    target = PROTECT(allocVector(valtype, totlen));
    for (i=0; i<lvalues; i++) {
        thiscol = VECTOR_ELT(DT, INTEGER(valuecols)[i]-1);
        if (isFactor(thiscol))
            thiscol = asCharacterFactor(thiscol);
        if (TYPEOF(thiscol) != valtype && !isFactor(thiscol)) {
            // thiscol = valtype == STRSXP ? PROTECT(coerce_to_char(thiscol, R_GlobalEnv)) : PROTECT(coerceVector(thiscol, valtype));
            // protecti++; // for now, no preserving of class attributes
            thiscol = PROTECT(coerceVector(thiscol, valtype)); protecti++;
        }
        size = SIZEOF(thiscol);
        if (narm) {
            thisidx = VECTOR_ELT(idxkeep, i);
            thislen = length(thisidx);
        }
        switch(valtype) {
            case VECSXP :
            if (narm) {
                for (j=0; j<thislen; j++)
                    SET_VECTOR_ELT(target, counter + j, VECTOR_ELT(thiscol, INTEGER(thisidx)[j]-1));
            } else {
                for (j=0; j<nrow; j++) SET_VECTOR_ELT(target, i*nrow + j, VECTOR_ELT(thiscol, j));
            }
            break;
            case STRSXP :
            if (narm) {
                for (j=0; j<thislen; j++)
                    SET_STRING_ELT(target, counter + j, STRING_ELT(thiscol, INTEGER(thisidx)[j]-1));
            } else {
                for (j=0; j<nrow; j++) SET_STRING_ELT(target, i*nrow + j, STRING_ELT(thiscol, j));
            }
            break;
            case REALSXP : 
            if (narm) {
                for (j=0; j<thislen; j++)
                    REAL(target)[counter + j] = REAL(thiscol)[INTEGER(thisidx)[j]-1];
            } else {
                memcpy((char *)DATAPTR(target)+i*nrow*size, (char *)DATAPTR(thiscol), nrow*size);
            }
            break;
            case INTSXP : 
            if (narm) {
                for (j=0; j<thislen; j++)
                    INTEGER(target)[counter + j] = INTEGER(thiscol)[INTEGER(thisidx)[j]-1];
            } else {
                memcpy((char *)DATAPTR(target)+i*nrow*size, (char *)DATAPTR(thiscol), nrow*size);
            }
            break;
            case LGLSXP :
            if (narm) {
                for (j=0; j<thislen; j++)
                    LOGICAL(target)[counter + j] = LOGICAL(thiscol)[INTEGER(thisidx)[j]-1];
            } else {
                memcpy((char *)DATAPTR(target)+i*nrow*size, (char *)DATAPTR(thiscol), nrow*size);
            }
            break;
            default : error("Unknown column type '%s' for column '%s' in 'data'", type2char(TYPEOF(thiscol)), CHAR(STRING_ELT(dtnames, INTEGER(valuecols)[i]-1)));
        }
        if (narm) counter += thislen;
        // if (isidentical && valtype != VECSXP) // for now, no preserving of class attributes
        //     setAttrib(target, R_ClassSymbol, getAttrib(VECTOR_ELT(DT, INTEGER(valuecols)[0]-1), R_ClassSymbol)); // for Date like column
    }
    // check for factor
    if (LOGICAL(valfactor)[0] == TRUE && valtype == VECSXP) warning("argument 'value.factor' ignored because 'value' column is a list\n");
    if (LOGICAL(valfactor)[0] == TRUE && valtype != VECSXP) {
        PROTECT(factorLangSxp = allocList(2));
        SET_TYPEOF(factorLangSxp, LANGSXP);
        SETCAR(factorLangSxp, install("factor"));
        SETCAR(CDR(factorLangSxp), target);
        SET_VECTOR_ELT(ans, lids+1, eval(factorLangSxp, R_GlobalEnv)); // last column
        UNPROTECT(1); // factorLangSxp
    } else 
        SET_VECTOR_ELT(ans, lids+1, target);    
    UNPROTECT(1); // target
    
    // get "variable" column
    counter = 0, i=0;
    target = PROTECT(allocVector(INTSXP, totlen));
     for (j=0; j<lvalues; j++) {
        if (narm) {
            thislen = length(VECTOR_ELT(idxkeep, j));
            for (k=0; k<thislen; k++)
                INTEGER(target)[counter + k] = i+1;
            counter += thislen;
            if (thislen > 0 || !droplevels) i++;
        } else {
            for (k=0; k<nrow; k++)
                INTEGER(target)[nrow*j + k] = j+1;
        }
    }
    setAttrib(target, R_ClassSymbol, mkString("factor"));
    if (narm && droplevels) {
        counter = 0;
        for (j=0; j<lvalues; j++) {
            if (length(VECTOR_ELT(idxkeep, j)) > 0) counter++;
        }
    } else counter = lvalues;
    levels = PROTECT(allocVector(STRSXP, counter));
    i = 0;
    for (j=0; j<lvalues; j++) {
        if (narm && droplevels) {
            if (length(VECTOR_ELT(idxkeep, j)) > 0)
                SET_STRING_ELT(levels, i++, STRING_ELT(dtnames, INTEGER(valuecols)[j]-1));
        } else 
            SET_STRING_ELT(levels, j, STRING_ELT(dtnames, INTEGER(valuecols)[j]-1));
    }
    setAttrib(target, R_LevelsSymbol, levels);
    UNPROTECT(1); // levels
    if (LOGICAL(varfactor)[0] == FALSE)
        target = asCharacterFactor(target);
    SET_VECTOR_ELT(ans, lids, target);
    UNPROTECT(1); // target
    
    // generate idcols (left part)
    for (i=0; i<lids; i++) {
        counter = 0;
        thiscol = VECTOR_ELT(DT, INTEGER(idcols)[i]-1);
        size = SIZEOF(thiscol);
        target = PROTECT(allocVector(TYPEOF(thiscol), totlen)); 
        switch(TYPEOF(thiscol)) {
            case REALSXP :
            if (narm) {
                for (j=0; j<lvalues; j++) {
                    thisidx = PROTECT(VECTOR_ELT(idxkeep, j));
                    thislen = length(thisidx);
                    for (k=0; k<thislen; k++)
                        REAL(target)[counter + k] = REAL(thiscol)[INTEGER(thisidx)[k]-1];
                    counter += thislen;
                    UNPROTECT(1); // thisidx
                } 
            } else { 
                for (j=0; j<lvalues; j++)
                    memcpy((char *)DATAPTR(target)+j*nrow*size, (char *)DATAPTR(thiscol), nrow*size);
            }
            break;
            case INTSXP :
            if (narm) {
                for (j=0; j<lvalues; j++) {
                    thisidx = PROTECT(VECTOR_ELT(idxkeep, j));
                    thislen = length(thisidx);
                    for (k=0; k<thislen; k++)
                        INTEGER(target)[counter + k] = INTEGER(thiscol)[INTEGER(thisidx)[k]-1];
                    counter += thislen;
                    UNPROTECT(1); // thisidx
                } 
            } else {
                for (j=0; j<lvalues; j++)
                    memcpy((char *)DATAPTR(target)+j*nrow*size, (char *)DATAPTR(thiscol), nrow*size);
            }
            break;
            case LGLSXP :
            if (narm) {
                for (j=0; j<lvalues; j++) {
                    thisidx = PROTECT(VECTOR_ELT(idxkeep, j));
                    thislen = length(thisidx);
                    for (k=0; k<thislen; k++)
                        LOGICAL(target)[counter + k] = LOGICAL(thiscol)[INTEGER(thisidx)[k]-1];
                    counter += thislen;
                    UNPROTECT(1); // thisidx
                } 
            } else {
                for (j=0; j<lvalues; j++)
                    memcpy((char *)DATAPTR(target)+j*nrow*size, (char *)DATAPTR(thiscol), nrow*size);
            }
            break;
            case STRSXP :
            if (narm) {
                for (j=0; j<lvalues; j++) {
                    thisidx = PROTECT(VECTOR_ELT(idxkeep, j));
                    thislen = length(thisidx);
                    for (k=0; k<thislen; k++)
                        SET_STRING_ELT(target, counter + k, STRING_ELT(thiscol, INTEGER(thisidx)[k]-1));
                    counter += thislen;
                    UNPROTECT(1); // thisidx
                } 
            } else {
                // SET_STRING_ELT for j=0 and memcpy for j>0, WHY?
                // From assign.c's memcrecycle - only one SET_STRING_ELT per RHS item is needed to set generations (overhead)
                for (k=0; k<nrow; k++) SET_STRING_ELT(target, k, STRING_ELT(thiscol, k));
                for (j=1; j<lvalues; j++)
                    memcpy((char *)DATAPTR(target)+j*nrow*size, (char *)DATAPTR(target), nrow*size);
            }
            break;
            case VECSXP :
            for (j=0; j<lvalues; j++) {
                for (k=0; k<nrow; k++) {
                    SET_VECTOR_ELT(target, j*nrow + k, VECTOR_ELT(thiscol, k));
                }
            }
            break;
            default : error("Unknown column type '%s' for column '%s' in 'data'", type2char(TYPEOF(thiscol)), CHAR(STRING_ELT(dtnames, INTEGER(idcols)[i]-1)));
        }
        copyMostAttrib(thiscol, target); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
        SET_VECTOR_ELT(ans, i, target);
        UNPROTECT(1); // target
    }
                
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(protecti);
    return(ans);
}
