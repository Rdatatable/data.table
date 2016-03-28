#include <R.h>
#include <errno.h>
#include <Rinternals.h>

void writefile(SEXP list_of_columns,
               SEXP filename,
               SEXP col_sep_exp,
               SEXP row_sep_exp,
               SEXP na_exp,
               SEXP quote_cols,
               SEXP qmethod_escape_exp,
               SEXP append) {

  int error_number = 0;
  int qmethod_escape = *LOGICAL(qmethod_escape_exp);
  
  errno = 0; /* clear flag possibly set by previous errors */
  
  char col_sep = *CHAR(STRING_ELT(col_sep_exp, 0));
  const char *row_sep = CHAR(STRING_ELT(row_sep_exp, 0));
  const char *na_str = CHAR(STRING_ELT(na_exp, 0));
  const char QUOTE_CHAR = '"';
  const char ESCAPE_CHAR = '\\';

  /* open input file in correct mode */
  const char *open_mode = "wb";
  if (*LOGICAL(append)) open_mode = "ab";
  FILE *f = fopen(CHAR(STRING_ELT(filename, 0)), open_mode);
  if (f == NULL) goto end;
  
  R_xlen_t ncols = LENGTH(list_of_columns);
  R_xlen_t nrows = LENGTH(VECTOR_ELT(list_of_columns, 0));
  
  for (R_xlen_t row_i = 0; row_i < nrows; ++row_i) {
    for (int col_i = 0; col_i < ncols; ++col_i) {
      
      if (col_i > 0) fputc(col_sep, f);
      
      SEXP column = VECTOR_ELT(list_of_columns, col_i);
      
      switch(TYPEOF(column)) {
      case INTSXP:
        if (INTEGER(column)[row_i] == NA_INTEGER) fputs(na_str, f);
        else fprintf(f, "%d", INTEGER(column)[row_i]);
        break;
      
      case REALSXP:
        if (ISNA(REAL(column)[row_i])) fputs(na_str, f);
        else fprintf(f, "%g", REAL(column)[row_i]);
        break;
      
      default: /* assuming STRSXP */
        if (STRING_ELT(column, row_i) == NA_STRING) fputs(na_str, f);
        else {
          int quote = LOGICAL(quote_cols)[col_i];
          if (quote) fputc(QUOTE_CHAR, f);
          for (const char *ch = CHAR(STRING_ELT(column, row_i)); *ch != '\0'; ++ch) {
            if (quote) {
              if (*ch == QUOTE_CHAR) {
                if (qmethod_escape) fputc(ESCAPE_CHAR, f);
                else fputc(QUOTE_CHAR, f); /* qmethod = "double" */
              }
              if (qmethod_escape && *ch == ESCAPE_CHAR) fputc(ESCAPE_CHAR, f);
            }
            fputc(*ch, f);
          }
          if (quote) fputc(QUOTE_CHAR, f);
        }
        break;
      }
    }
    if (fputs(row_sep, f) < 0) goto end;
  }
  
  end:
    error_number = errno;
  if (f != NULL) fclose(f);
  if (error_number) error(strerror(errno));
}