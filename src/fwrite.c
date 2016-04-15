#include "data.table.h"
#include <R.h>
#include <errno.h>
#include <Rinternals.h>
#include <unistd.h>  // for access()
#include <fcntl.h>
#include <time.h>
#ifdef WIN32
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#define WRITE _write
#define CLOSE _close
#else
#define WRITE write
#define CLOSE close
#endif

static inline int maxStrLen(SEXP x, int na_len) {
  // The max nchar of any string in a column or factor level
  int max=na_len, nrow=length(x);
  for (int i=0; i<nrow; i++) {
    int l = LENGTH(STRING_ELT(x,i));
    if (l>max) max=l;
  }
  // TODO if(quote) count them here (we hopped already to get LENGTH), add that quote count to max
  //      exactly and remove the *(1+quote) later
  //      if there are no quotes present in any string in a column save in quotePresent[ncol] lookup
  //      and switch to memcpy when it's known no quotes are present to be escaped for that column
  return max;
}

#define QUOTE_FIELD \
  *ch++ = QUOTE; \
  for (const char *ch2 = CHAR(str); *ch2 != '\0'; ch2++) { \
    if (*ch2 == QUOTE) *ch++ = ESCAPE_QUOTE; \
    if (qmethod_escape && *ch2 == ESCAPE) *ch++ = ESCAPE; \
    *ch++ = *ch2; \
  } \
  *ch++ = QUOTE

SEXP writefile(SEXP list_of_columns,
               SEXP filenameArg,
               SEXP col_sep_Arg,
               SEXP row_sep_Arg,
               SEXP na_Arg,
               SEXP quoteArg,           // TRUE|FALSE
               SEXP qmethod_escapeArg,  // TRUE|FALSE
               SEXP append,             // TRUE|FALSE
               SEXP col_names,          // TRUE|FALSE
               SEXP verboseArg)
{
  if (!isNewList(list_of_columns)) error("fwrite must be passed an object of type list, data.table or data.frame");
  RLEN ncols = length(list_of_columns);
  if (ncols==0) error("fwrite must be passed a non-empty list");
  RLEN nrows = length(VECTOR_ELT(list_of_columns, 0));
  for (int i=1; i<ncols; i++) {
    if (nrows != length(VECTOR_ELT(list_of_columns, i)))
      error("Column %d's length (%d) is not the same as column 1's length (%d)", i+1, length(VECTOR_ELT(list_of_columns, i)), nrows);
  }
  const Rboolean verbose = LOGICAL(verboseArg)[0];
  const Rboolean quote = LOGICAL(quoteArg)[0];
  
  const char col_sep = *CHAR(STRING_ELT(col_sep_Arg, 0));  // DO NOT DO: allow multichar separator (bad idea)
  
  const char *row_sep = CHAR(STRING_ELT(row_sep_Arg, 0));
  const int row_sep_len = strlen(row_sep);  // someone somewhere might want a trailer on every line
  const char *na_str = CHAR(STRING_ELT(na_Arg, 0));
  const int na_len = strlen(na_str);
  const char QUOTE = '"';
  const char ESCAPE = '\\';
  const Rboolean qmethod_escape = LOGICAL(qmethod_escapeArg)[0];
  const char ESCAPE_QUOTE = qmethod_escape ? ESCAPE : QUOTE;
  const char *filename = CHAR(STRING_ELT(filenameArg, 0));

  errno = 0;   // clear flag possibly set by previous errors
#ifdef WIN32
  int f = _open(filename, _O_WRONLY | _O_BINARY | _O_CREAT | (LOGICAL(append)[0] ? _O_APPEND : _O_TRUNC), _S_IWRITE);
  // row_sep must be passed from R level as '\r\n' on Windows since write() only auto-converts \n to \r\n
  // in _O_TEXT mode. We use O_BINARY for full control and perhaps speed since O_TEXT must have to deep branch an if('\n')
#else
  int f = open(filename, O_WRONLY | O_CREAT | (LOGICAL(append)[0] ? O_APPEND : O_TRUNC), 0644);
#endif
  if (f == -1) {
    if( access( filename, F_OK ) != -1 )
      error("File exists and failed to open for writing. Do you have write permission to it? Is this Windows and does another process such as Excel have it open? File: %s", filename);
    else 
      error("Unable to create new file for writing (it does not exist already). Do you have permission to write here and is there space on the disk? File: %s", filename); 
  }
  int true_false;
  
  clock_t t0=clock();
  // i) prefetch levels of factor columns (if any) to save getAttrib on every field on every row of any factor column
  // ii) calculate certain upper bound of line length
  SEXP levels[ncols];  // on-stack vla
  int lineLenMax = 2;  // initialize with eol max width of \r\n on windows
  for (int col_i=0; col_i<ncols; col_i++) {
    SEXP column = VECTOR_ELT(list_of_columns, col_i);
    switch(TYPEOF(column)) {
    case LGLSXP:
      lineLenMax+=5;  // width of FALSE
      break;
    case REALSXP:
      lineLenMax+=20;   // 15 (+ 5 for safety)
      break;
    case INTSXP:
      if (isFactor(column)) {
        levels[col_i] = getAttrib(column, R_LevelsSymbol);
        lineLenMax += maxStrLen(levels[col_i], na_len)*(1+quote) + quote*2;
        //                                    ^^^^^^^^^^ in case every character in the field is a quote, each to be escaped (!)
      } else {
        levels[col_i] = NULL;
        lineLenMax+=11;   // 11 + sign
      }
      break;
    case STRSXP:
      lineLenMax += maxStrLen(column, na_len)*(1+quote) + quote*2;
      break;
    default:
      error("Column %d's type is '%s' - not yet implemented.", col_i+1,type2char(TYPEOF(column)) );
    }
    lineLenMax++;  // column separator
  }
  
  int bufSize = 4*1024*1024; // 4MB buffer. Large enough to fit many lines. Small enough to fit in cache.
  if (lineLenMax > bufSize) bufSize = lineLenMax;
  int writeTrigger = bufSize-lineLenMax-1;    // When to write.
  // TODO:  int linesPerBuf = bufSize/lineLenMax
  if (verbose) Rprintf("lineLenMax: %d\nbufSize: %d\nwriteTrigger: %d\n", lineLenMax, bufSize, writeTrigger);
  
  char *buffer = Calloc(bufSize, char);
  if (buffer == NULL) error("Unable to allocate %dMB buffer", bufSize/(1024*1024));

  clock_t tlineLenMax=clock()-t0;
  t0=clock();
  clock_t t1,tformat=0,twrite=0;
  // clock_t tt0,tSTR=0,tNUM=0;
  SEXP str;
  char *ch = buffer;
  int numWrite=0;
  
  // Write the column names
  if (LOGICAL(col_names)[0]) {
    SEXP names = getAttrib(list_of_columns, R_NamesSymbol);
    if (names!=NULL) {
      if (LENGTH(names) != ncols) error("Internal error: length of column names is not equal to the number of columns. Please report.");
      for (int col_i=0; col_i<ncols; col_i++) {
        str = STRING_ELT(names, col_i);
        
        // The lineLenMax was just for data rows, not the column names.
        // Be robust to enormously long column names e.g. 100,000 character single column name which might not fit in the buffer
        int maxLen = 2 + (str==NA_STRING ? na_len : LENGTH(str)*(1+quote)+quote*2);  
        //           ^ to cover sep or windows eol
        if (maxLen >= bufSize) error("The buffer isn't big enough to hold single column name %d", col_i+1);
        if (ch-buffer+maxLen >= bufSize) {
          if (ch == buffer) error("Internal error: ch>buffer at this point");
          // write out the column names we've written to the buffer already and rewind the buffer
          if (WRITE(f, buffer, (int)(ch-buffer)) == -1) { close(f); error("Error writing to file: %s", filename); }
          numWrite++;
          ch = buffer;
        }
        // now proceed safely to write the column name to the buffer
        
        if (str==NA_STRING) {
          if (na_len) { memcpy(ch, na_str, na_len); ch += na_len; }
          break;
        }
        if (quote) {
          QUOTE_FIELD;
        } else {
          memcpy(ch, CHAR(str), LENGTH(str));
          ch += LENGTH(str);
        }
        *ch++ = col_sep;
      }
      ch--;  // backup onto the last col_sep after the last column
      memcpy(ch, row_sep, row_sep_len);  // replace it with the newline 
      ch += row_sep_len;
      
      // For gigantic data row lengths the buffer may only be able to hold one row at a time. So write the column
      // names now and clear the buffer, just in case, ready for the first data row.
      if (WRITE(f, buffer, (int)(ch-buffer)) == -1) { close(f); error("Error writing to file: %s", filename); }
      numWrite++;
      ch = buffer;
    }
  }
  
  // Write the data rows
  for (RLEN row_i = 0; row_i < nrows; row_i++) {
    for (int col_i = 0; col_i < ncols; col_i++) {
      SEXP column = VECTOR_ELT(list_of_columns, col_i);
      switch(TYPEOF(column)) {
      case LGLSXP:
        true_false = LOGICAL(column)[row_i];
        if (true_false == NA_LOGICAL) {
          if (na_len) { memcpy(ch, na_str, na_len); ch += na_len; }
        } else if (true_false) {
          memcpy(ch,"TRUE",4);   // Other than strings, field widths are limited which we check elsewhere here to ensure
          ch += 4;
        } else {
          memcpy(ch,"FALSE",5);
          ch += 5;
        }
        break;
      case REALSXP:
        if (ISNA(REAL(column)[row_i])) {
          if (na_len) { memcpy(ch, na_str, na_len); ch += na_len; }
        } else {
          //tt0 = clock();
          ch += sprintf(ch, "%.15G", REAL(column)[row_i]);
          //tNUM += clock()-tt0;
        }
        break;
      case INTSXP:
        if (INTEGER(column)[row_i] == NA_INTEGER) {
          if (na_len) { memcpy(ch, na_str, na_len); ch += na_len; }
        } else if (levels[col_i] != NULL) {   // isFactor(column) == TRUE
          str = STRING_ELT(levels[col_i], INTEGER(column)[row_i]-1);
          if (quote) {
            QUOTE_FIELD;
          } else {
            memcpy(ch, CHAR(str), LENGTH(str));
            ch += LENGTH(str);
          }
        } else {
          ch += sprintf(ch, "%d", INTEGER(column)[row_i]);
        }
        break;
      case STRSXP:
        str = STRING_ELT(column, row_i);
        if (str==NA_STRING) {
          if (na_len) { memcpy(ch, na_str, na_len); ch += na_len; }
        } else if (quote) {
          QUOTE_FIELD;
        } else {
          //tt0 = clock();
          memcpy(ch, CHAR(str), LENGTH(str));  // could have large fields. Doubt call overhead is much of an issue on small fields.
          ch += LENGTH(str);
          //tSTR += clock()-tt0;
        }
        break;
      default:
         error("Column %d's type is '%s' - not yet implemented.", col_i+1,type2char(TYPEOF(column)) );
      }
      *ch++ = col_sep;
    }
    ch--;  // backup onto the last col_sep after the last column
    memcpy(ch, row_sep, row_sep_len);  // replace it with the newline. TODO: replace memcpy call with eol1 eol2 --eolLen 
    ch += row_sep_len;
    // Rprintf("Writing a line out length %d %10s\n", (int)(ch-buffer), buffer);
    if ((ch-buffer)>writeTrigger) {
      t1 = clock(); tformat += t1-t0; t0 = t1;
      if (WRITE(f, buffer, (int)(ch-buffer)) == -1) { close(f); error("Error writing to file: %s", filename); }
      t1 = clock(); twrite += t1-t0; t0 = t1;
      numWrite++;
      ch = buffer;
    }
  }
  if (ch>buffer) {
    // write last batch remaining in buffer
    t1 = clock(); tformat += t1-t0; t0 = t1;
    if (WRITE(f, buffer, (int)(ch-buffer)) == -1) { close(f); error("Error writing to file: %s", filename); }
    numWrite++;
    t1 = clock(); twrite += t1-t0; t0 = t1;
  }
  if (CLOSE(f)) error("Error closing file: %s", filename);
  Free(buffer);
  clock_t total = tlineLenMax + tformat + twrite; 
  if (verbose) {
    Rprintf("%8.3fs (%3.0f%%) calc max line length\n", 1.0*tlineLenMax/CLOCKS_PER_SEC, 100.0*tlineLenMax/total);
    Rprintf("%8.3fs (%3.0f%%) format\n", 1.0*tformat/CLOCKS_PER_SEC, 100.0*tformat/total);
    Rprintf("%8.3fs (%3.0f%%) write (%d calls)\n", 1.0*twrite/CLOCKS_PER_SEC, 100.0*twrite/total, numWrite);
    //Rprintf("  %8.3fs (%3.0f%%) STR\n", 1.0*tSTR/CLOCKS_PER_SEC, 100.0*tSTR/tformat);
    //Rprintf("  %8.3fs (%3.0f%%) NUM\n", 1.0*tNUM/CLOCKS_PER_SEC, 100.0*tNUM/tformat);
  }
  return(R_NilValue);  // must always return SEXP from C level otherwise hang on Windows
}



