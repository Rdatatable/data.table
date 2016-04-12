#include "data.table.h"
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
               SEXP append)
{
  if (!isNewList(list_of_columns)) error("fwrite must be passed an object of type list, data.table or data.frame");
  RLEN ncols = length(list_of_columns);
  if (ncols==0) error("fwrite must be passed a non-empty list");
  RLEN nrows = length(VECTOR_ELT(list_of_columns, 0));
  for (int i=1; i<ncols; i++) {
    if (nrows != length(VECTOR_ELT(list_of_columns, i)))
      error("Column %d's length (%d) is not the same as column 1's length (%d)", i+1, length(VECTOR_ELT(list_of_columns, i)), nrows);
  }

  //int error_number = 0;
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
  if (f == NULL) error("Unable to open file: %s", filename);
  int true_false;
  
  // prefetch levels of factor columns (if any) to save getAttrib on every field on every row of any factor column
  SEXP levels[ncols];  // on-stack vla
  for (int col_i=0; col_i<ncols; col_i++) {
    SEXP column = VECTOR_ELT(list_of_columns, col_i);
    levels[col_i] = isFactor(column) ? getAttrib(column, R_LevelsSymbol) : NULL;
  }
  
  for (RLEN row_i = 0; row_i < nrows; ++row_i) {
    for (int col_i = 0; col_i < ncols; ++col_i) {
      
      if (col_i > 0) fputc(col_sep, f);
      
      SEXP column = VECTOR_ELT(list_of_columns, col_i);
      SEXP str = NULL;
      switch(TYPEOF(column)) {
      case LGLSXP:
        true_false = LOGICAL(column)[row_i];
        fputs(true_false == NA_LOGICAL ? na_str : (true_false ? "TRUE" : "FALSE"), f);
        break;
      case REALSXP:
        if (ISNA(REAL(column)[row_i])) fputs(na_str, f);
        else fprintf(f, "%.15g", REAL(column)[row_i]);
        break;
      case INTSXP:
        if (INTEGER(column)[row_i] == NA_INTEGER) {
          fputs(na_str, f);
          break;
        }
        if (levels[col_i] != NULL) {   // isFactor(column) == TRUE
          str = STRING_ELT(levels[col_i], INTEGER(column)[row_i]-1);
          // fall through to STRSXP case
        } else {
          fprintf(f, "%d", INTEGER(column)[row_i]);
          break;
        }
      case STRSXP:
        if (str==NULL) str = STRING_ELT(column, row_i);
        if (str==NA_STRING) fputs(na_str, f);
        else {
          int quote = LOGICAL(quote_cols)[col_i];
          if (quote) fputc(QUOTE_CHAR, f);
          for (const char *ch = CHAR(str); *ch != '\0'; ++ch) {
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
      default:
         error("Column %d's type is '%s' - not yet implemented.", col_i+1,type2char(TYPEOF(column)) );
      }
    }
    fputs(row_sep, f);
  }
  
  //end:   // don't mind the goto really. it's more that the levels vla prevents goto in gcc. Will do differently anyway.
  //  error_number = errno;
  if (f != NULL) fclose(f);
  //if (error_number) error(strerror(errno));
}


