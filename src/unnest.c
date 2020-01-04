SEXP unnest(SEXP x, SEXP cols) {
  int k = LENGTH(cols);
  // nothing to unnest -- need to go further, just be sure to copy
  if (k == 0)
    return (duplicate(x));
  int n = LENGTH(VECTOR_ELT(x, 0));
  int p = LENGTH(x);

  if (TYPEOF(cols) != INTSXP)
    error(_("cols must be an integer vector, got %s"), type2char(TYPEOF(cols)));
  int *colp = INTEGER(cols);

  // ignore non-VECSXP elements of cols; use lcols and lk (list-only versions)
  int lcols[k], j;
  int lk=0;
  for (int i=0; i<k; i++) {
    j = colp[i];
    if (j < 1 || j > p)
      error(_("cols to unnest must be in [1, ncol(x)=%d], but cols[%d]=%d"), p, i, j);
    switch(TYPEOF(VECTOR_ELT(x, j-1))) {
    case RAWSXP :
    case LGLSXP :
    case INTSXP :
    case REALSXP :
    case CPLXSXP :
    case STRSXP : break;
    case VECSXP : {
      lcols[lk++] = j-1; // move to 0-based
    } break;
    default:
      error(_("Unsupported type: %s"), type2char(TYPEOF(VECTOR_ELT(x, j))));
    }
  }

  int row_counts[n];
  int eachrep[n];

  double this_row;
  int out_rows=0, i=0;
  // find the mapping i -> # rows corresponding to i in output,
  //   also check feasibility of output
  while (i < n) {
    this_row = 1;
    for (int jj=0; jj<lk; jj++)
      this_row *= LENGTH(VECTOR_ELT(VECTOR_ELT(x, lcols[jj]), i));
    if (this_row > INT_MAX)
      error("Implied number of unnested rows from row %d (%.0f) exceeds %d, the maximum currently allowed.", i, this_row, INT_MAX);
    if (out_rows > INT_MAX - this_row) {
      double out_rows_dbl = (double) out_rows;
      for (;i<n; i++)
        out_rows_dbl += row_counts[i];
      error("Implied number of unnested rows (%.0f) exceeds %d, the maximum currently allowed.", out_rows_dbl, INT_MAX);
    }
    row_counts[i] = (int) this_row;
    out_rows += row_counts[i++];
    eachrep[i] = 1; // initializing
  }

  // TODO: factor columns?
  SEXP ans = PROTECT(allocVector(VECSXP, p));
  copyMostAttrib(x, ans);
  int outi=0;
  for (int j=p-1, lj=lk-1; j>=0; j--) {
    Rprintf("j=%d\n", j);
    SEXP xj = VECTOR_ELT(x, j), ansj;
    if (lj >= 0 && j == lcols[lj]) { // vec col: apply unnesting
      // TODO: type bumping for mismatch
      SET_VECTOR_ELT(ans, j, ansj=allocVector(TYPEOF(VECTOR_ELT(xj, 0)), out_rows));
      for (int i=0; i<n; i++) {
        Rprintf("\ti=%d\n", i);
        cycle_vector(VECTOR_ELT(xj, i), ansj + outi, lj, eachrep[i], row_counts[i]);
        eachrep[i] *= LENGTH(VECTOR_ELT(xj, i));
        outi += row_counts[i];
      }
      lj--;
    } else { // non-vec col: simply copy
      SET_VECTOR_ELT(ans, j, ansj=allocVector(TYPEOF(xj), out_rows));
      switch(TYPEOF(xj)) {
      case RAWSXP: {
        const Rbyte *source = RAW(xj);
        Rbyte *target = RAW(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(Rbyte));
          }
          outi += row_counts[i];
        }
      } break;
      case LGLSXP:
      case INTSXP: {
        const int *source = INTEGER(xj);
        int *target = INTEGER(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(int));
          }
          outi += row_counts[i];
        }
      } break;
      case REALSXP: {
        const double *source = REAL(xj);
        double *target = REAL(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(double));
          }
          outi += row_counts[i];
        }
      } break;
      case CPLXSXP: {
        const Rcomplex *source = COMPLEX(xj);
        Rcomplex *target = COMPLEX(ansj);
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            memcpy(target + outi + repi, &source[i], sizeof(Rcomplex));
          }
          outi += row_counts[i];
        }
      } break;
      case STRSXP: {
        for (int i=0; i<n; i++) {
          for(int repi=0; repi<row_counts[i]; repi++) {
            SET_STRING_ELT(ansj, outi + repi, STRING_ELT(xj, i));
          }
          outi += row_counts[i];
        }
      } break;
      default: error(_("Unsupported column type %s"), type2char(TYPEOF(xj)));
      }
    }
    copyMostAttrib(xj, ansj);
  }
  Rprintf("done\n");

  // copy names
  SEXP ansNames;
  SEXP xNames = PROTECT(getAttrib(x, R_NamesSymbol));
  setAttrib(ans, R_NamesSymbol, ansNames=allocVector(STRSXP, p));
  for (int j=0; j<p; j++) {
    SET_STRING_ELT(ansNames, j, STRING_ELT(xNames, j));
  }
  UNPROTECT(2);
  return (ans);
}
