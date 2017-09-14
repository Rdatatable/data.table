
#include "data.table.h"

/* Non-agnostic writers
 * Where possible we use common agnostic writers. But in these cases there are unavoidable differences
 * in the structure of the data being written.
 */

static void writeString(SEXP col, int row, char **thisCh)
{
  SEXP x = STRING_ELT(col, row);
  char *ch = *thisCh;
  if (x == NA_STRING) {
    // NA is not quoted by write.csv even when quote=TRUE to distinguish from "NA"
    write_chars(na, &ch);
  } else {
    Rboolean q = quote;
    if (q==NA_LOGICAL) { // quote="auto"
      const char *tt = CHAR(x);
      if (*tt == '\0') {
        // Empty strings are always quoted: this distinguishes them from NAs
        *ch = '"'; ch[1] = '"';
        *thisCh += 2;
        return;
      }
      while (*tt!='\0' && *tt!=sep && *tt!=sep2 && *tt!='\n' && *tt!='"') *ch++ = *tt++;
      // Windows includes \n in its \r\n so looking for \n only is sufficient
      // sep2 is set to '\0' when no list columns are present
      if (*tt=='\0') {
        // most common case: no sep, newline or " contained in string
        *thisCh = ch;  // advance caller over the field already written
        return;
      }
      ch = *thisCh; // rewind the field written since it needs to be quoted
      q = TRUE;
    }
    if (q==FALSE) {
      write_chars(CHAR(x), &ch);
    } else {
      *ch++ = '"';
      const char *tt = CHAR(x);
      if (qmethod_escape) {
        while (*tt!='\0') {
          if (*tt=='"' || *tt=='\\') *ch++ = '\\';
          *ch++ = *tt++;
        }
      } else {
        // qmethod='double'
        while (*tt!='\0') {
          if (*tt=='"') *ch++ = '"';
          *ch++ = *tt++;
        }
      }
      *ch++ = '"';
    }
  }
  *thisCh = ch;
}

static void writeFactor(SEXP column, int i, char **thisCh) {
  char *ch = *thisCh;
  if (INTEGER(column)[i]==NA_INTEGER) write_chars(na, &ch);
  else writeString(getAttrib(column, R_LevelsSymbol), INTEGER(column)[i]-1, &ch);
  *thisCh = ch;
}

static void writeList(SEXP, int, char **); // prototype needed because it calls back to whichWriter too

static writer_fun_t whichWriter(SEXP column) {
  switch(TYPEOF(column)) {
  case LGLSXP:
    return logicalAsInt ? writeInteger : writeLogical;
  case INTSXP:
    if (isFactor(column))                return writeFactor;
    if (dateTimeAs==DATETIMEAS_EPOCH)    return writeInteger;
    if (INHERITS(column, char_ITime))    return writeITime;
    if (INHERITS(column, char_Date))     return writeDateInt;
    return writeInteger;
  case REALSXP:
    if (INHERITS(column, char_nanotime) && dateTimeAs!=DATETIMEAS_EPOCH) return writeNanotime;
    if (INHERITS(column, char_integer64))return writeInteger;
    if (dateTimeAs==DATETIMEAS_EPOCH)    return writeNumeric;
    if (INHERITS(column, char_Date))     return writeDateReal;
    if (INHERITS(column, char_POSIXct))  return writePOSIXct;
    return writeNumeric;
  case STRSXP:
    return writeString;
  case VECSXP:
    return writeList;
  default:
    return NULL;
  }
}

static void writeList(SEXP column, int i, char **thisCh) {
  SEXP v = VECTOR_ELT(column,i);
  writer_fun_t fun = whichWriter(v);
  if (TYPEOF(v)==VECSXP || fun==NULL) {
    error("Row %d of list column is type '%s' - not yet implemented. fwrite() can write list columns containing atomic vectors of type logical, integer, integer64, double, character and factor, currently.", i+1, type2char(TYPEOF(v)));
  }
  char *ch = *thisCh;
  write_chars(sep2start, &ch);
  for (int j=0; j<LENGTH(v); j++) {
    (*fun)(v, j, &ch);
    *ch++ = sep2;
  }
  if (LENGTH(v)) ch--; // backup over the last sep2 after the last item
  write_chars(sep2end, &ch);
  *thisCh = ch;
}



SEXP writefile(SEXP DFin,               // any list of same length vectors; e.g. data.frame, data.table
               SEXP filename_Arg,
               SEXP sep_Arg,
               SEXP sep2_Arg,
               SEXP eol_Arg,
               SEXP na_Arg,
               SEXP dec_Arg,
               SEXP quote_Arg,          // 'auto'=NA_LOGICAL|TRUE|FALSE
               SEXP qmethod_escapeArg,  // TRUE|FALSE
               SEXP append,             // TRUE|FALSE
               SEXP row_names,          // TRUE|FALSE
               SEXP col_names,          // TRUE|FALSE
               SEXP logicalAsInt_Arg,   // TRUE|FALSE
               SEXP dateTimeAs_Arg,     // 0=ISO(yyyy-mm-dd),1=squash(yyyymmdd),2=epoch,3=write.csv
               SEXP buffMB_Arg,         // [1-1024] default 8MB
               SEXP nThread,
               SEXP showProgress_Arg,
               SEXP verbose_Arg)
{
  if (!isNewList(DFin)) error("fwrite must be passed an object of type list; e.g. data.frame, data.table");
  RLEN ncol = length(DFin);
  if (ncol==0) {
    warning("fwrite was passed an empty list of no columns. Nothing to write.");
    return R_NilValue;
  }
  RLEN nrow = length(VECTOR_ELT(DFin, 0));

  const Rboolean showProgress = LOGICAL(showProgress_Arg)[0];
  time_t start_time = time(NULL);
  time_t next_time = start_time+2; // start printing progress meter in 2 sec if not completed by then

  verbose = LOGICAL(verbose_Arg)[0];

  sep = *CHAR(STRING_ELT(sep_Arg, 0));  // DO NOT DO: allow multichar separator (bad idea)
  sep2start = CHAR(STRING_ELT(sep2_Arg, 0));
  sep2 = *CHAR(STRING_ELT(sep2_Arg, 1));
  sep2end = CHAR(STRING_ELT(sep2_Arg, 2));

  const char *eol = CHAR(STRING_ELT(eol_Arg, 0));
  // someone might want a trailer on every line so allow any length string as eol

  na = CHAR(STRING_ELT(na_Arg, 0));
  dec = *CHAR(STRING_ELT(dec_Arg,0));
  quote = LOGICAL(quote_Arg)[0];
  // When NA is a non-empty string, then we must quote all string fields
  if (*na != '\0' && quote == NA_LOGICAL) quote = TRUE;
  qmethod_escape = LOGICAL(qmethod_escapeArg)[0];
  const char *filename = CHAR(STRING_ELT(filename_Arg, 0));
  logicalAsInt = LOGICAL(logicalAsInt_Arg)[0];
  dateTimeAs = INTEGER(dateTimeAs_Arg)[0];
  squash = (dateTimeAs==1);
  int nth = INTEGER(nThread)[0];
  int firstListColumn = 0;
  clock_t t0=clock();

  SEXP DF = DFin;
  int protecti = 0;
  if (dateTimeAs == DATETIMEAS_WRITECSV) {
    int j=0; while(j<ncol && !INHERITS(VECTOR_ELT(DFin,j), char_POSIXct)) j++;
    if (j<ncol) {
      // dateTimeAs=="write.csv" && there exist some POSIXct columns; coerce them
      DF = PROTECT(allocVector(VECSXP, ncol));
      protecti++;
      // potentially large if ncol=1e6 as reported in #1903 where using large VLA caused stack overflow
      SEXP s = PROTECT(allocList(2));
      SET_TYPEOF(s, LANGSXP);
      SETCAR(s, install("format.POSIXct"));
      for (int j=0; j<ncol; j++) {
        SEXP column = VECTOR_ELT(DFin, j);
        if (INHERITS(column, char_POSIXct)) {
          SETCAR(CDR(s), column);
          SET_VECTOR_ELT(DF, j, eval(s, R_GlobalEnv));
        } else {
          SET_VECTOR_ELT(DF, j, column);
        }
      }
      UNPROTECT(1);  // s, not DF
    }
  }

  // Allocate lookup vector to writer function for each column. For simplicity and robustness via many fewer lines
  // of code and less logic need. Secondly, for efficiency to save deep switch and branches later.
  // Don't use a VLA as ncol could be > 1e6 columns
  writer_fun_t *fun = (writer_fun_t *)R_alloc(ncol, sizeof(writer_fun_t));
  for (int j=0; j<ncol; j++) {
    SEXP column = VECTOR_ELT(DF, j);
    if (nrow != length(column))
      error("Column %d's length (%d) is not the same as column 1's length (%d)", j+1, length(column), nrow);
    if ((fun[j] = whichWriter(column)) == NULL)
      error("Column %d's type is '%s' - not yet implemented.", j+1, type2char(TYPEOF(column)) );
    if (TYPEOF(column)==VECSXP && firstListColumn==0) firstListColumn = j+1;
  }

  if (!firstListColumn) {
    if (verbose) Rprintf("No list columns are present. Setting sep2='' otherwise quote='auto' would quote fields containing sep2.\n");
    sep2='\0';
  } else {
    if (verbose) Rprintf("If quote='auto', fields will be quoted if the field contains either sep ('%c') or sep2[2] ('%c') because column %d is a list column.\n", sep, sep2, firstListColumn );
    if (dec==sep) error("Internal error: dec != sep was checked at R level");
    if (dec==sep2 || sep==sep2)
      error("sep ('%c'), sep2[2L] ('%c') and dec ('%c') must all be different when list columns are present. Column %d is a list column.", sep, sep2, dec, firstListColumn);
  }

  // user may want row names even when they don't exist (implied row numbers as row names)
  Rboolean doRowNames = LOGICAL(row_names)[0];
  SEXP rowNames = NULL;
  if (doRowNames) {
    rowNames = getAttrib(DFin, R_RowNamesSymbol);
    if (!isString(rowNames)) rowNames=NULL;
  }

  // Estimate max line length of a 1000 row sample (100 rows in 10 places).
  // 'Estimate' even of this sample because quote='auto' may add quotes and escape embedded quotes.
  // Buffers will be resized later if there are too many line lengths outside the sample, anyway.
  // maxLineLen is required to determine a reasonable rowsPerBatch.
  int maxLineLen = 0;
  int na_len = strlen(na);
  int step = nrow<1000 ? 100 : nrow/10;
  for (int start=0; start<nrow; start+=step) {
    int end = (nrow-start)<100 ? nrow : start+100;
    for (int i=start; i<end; i++) {
      int thisLineLen=0;
      if (doRowNames) {
        if (rowNames) thisLineLen += LENGTH(STRING_ELT(rowNames,i));
        else thisLineLen += 1+(int)log10(nrow);
        if (quote==TRUE) thisLineLen+=2;
        thisLineLen++; // sep
      }
      for (int j=0; j<ncol; j++) {
        SEXP column = VECTOR_ELT(DF, j);
        static char tmp[64]; // +- 15digits dec e +- nnn \0 = 23 + 9 safety = 32. Covers integer64 too (20 digits).
                             // nanotime could be 36 chars: yyyy-mm-ddTHH:MM:SS.000000000+00:00. So be safe with 64.
        char *ch=tmp;
        if (TYPEOF(column)==STRSXP) thisLineLen+=LENGTH(STRING_ELT(column, i));
        else if (isFactor(column)) {
          int k = INTEGER(column)[i];
          thisLineLen += (k==NA_INTEGER) ? na_len : LENGTH(STRING_ELT(getAttrib(column,R_LevelsSymbol), k-1));
        } else if (TYPEOF(column)==VECSXP) {
          // a list column containing atomic vectors in each cell
          // for thisLineLen calculation we don't want to allocate tmp wide enough for the potentially
          // long field. Just to know its width we can add up the width of each item individually.
          SEXP v = VECTOR_ELT(column,i);
          thisLineLen += strlen(sep2start);
          if (TYPEOF(v)==STRSXP) for (int k=0; k<LENGTH(v); k++) thisLineLen += LENGTH(STRING_ELT(v, k));
          else if (isFactor(v)) {
            SEXP l = getAttrib(v, R_LevelsSymbol);
            for (int k=0; k<LENGTH(v); k++) {
              thisLineLen += INTEGER(v)[k]==NA_INTEGER ? na_len : LENGTH(STRING_ELT(l, INTEGER(v)[k]-1));
            }
          } else {
            writer_fun_t fun = whichWriter(v);
            if (fun==NULL) error("Column %d is a list column but on row %d is type '%s' - not yet implemented. fwrite() can write list columns containing atomic vectors of type logical, integer, integer64, double, character and factor, currently.", j+1, i+1, type2char(TYPEOF(v)));
            for (int k=0; k<LENGTH(v); k++) {
              (*fun)(v, k, &ch); thisLineLen+=(int)(ch-tmp); ch=tmp;
            }
          }
          thisLineLen += LENGTH(v); // sep2 after each field. LENGTH(v) could be 0 so don't -1 (only estimate anyway)
          thisLineLen += strlen(sep2end);
        } else {
          // regular atomic columns like integer, integer64, double, date and time
          (*fun[j])(column, i, &ch);
          thisLineLen += (int)(ch-tmp);
          ch = tmp;
        }
        thisLineLen++; // column sep
      } // next column
      if (thisLineLen > maxLineLen) maxLineLen = thisLineLen;
    }
  }
  maxLineLen += strlen(eol);
  if (verbose) Rprintf("maxLineLen=%d from sample. Found in %.3fs\n", maxLineLen, 1.0*(clock()-t0)/CLOCKS_PER_SEC);

  int f;
  if (*filename=='\0') {
    f=-1;  // file="" means write to standard output
    eol = "\n";  // We'll use Rprintf(); it knows itself about \r\n on Windows
  } else {
#ifdef WIN32
    f = _open(filename, _O_WRONLY | _O_BINARY | _O_CREAT | (LOGICAL(append)[0] ? _O_APPEND : _O_TRUNC), _S_IWRITE);
    // eol must be passed from R level as '\r\n' on Windows since write() only auto-converts \n to \r\n in
    // _O_TEXT mode. We use O_BINARY for full control and perhaps speed since O_TEXT must have to deep branch an if('\n')
#else
    f = open(filename, O_WRONLY | O_CREAT | (LOGICAL(append)[0] ? O_APPEND : O_TRUNC), 0666);
#endif
    if (f == -1) {
      int erropen = errno;
      if( access( filename, F_OK ) != -1 )
        error("%s: '%s'. Failed to open existing file for writing. Do you have write permission to it? Is this Windows and does another process such as Excel have it open?", strerror(erropen), filename);
      else
        error("%s: '%s'. Unable to create new file for writing (it does not exist already). Do you have permission to write here, is there space on the disk and does the path exist?", strerror(erropen), filename);
    }
  }
  t0=clock();

  if (verbose) {
    Rprintf("Writing column names ... ");
    if (f==-1) Rprintf("\n");
  }
  if (LOGICAL(col_names)[0]) {
    SEXP names = getAttrib(DFin, R_NamesSymbol);
    if (names!=R_NilValue) {
      if (LENGTH(names) != ncol) error("Internal error: length of column names is not equal to the number of columns. Please report.");
      // allow for quoting even when not.
      int buffSize = 2/*""*/ +1/*,*/;
      for (int j=0; j<ncol; j++) buffSize += 1/*"*/ +2*LENGTH(STRING_ELT(names, j)) +1/*"*/ +1/*,*/;
      //     in case every name full of quotes(!) to be escaped ^^
      buffSize += strlen(eol) +1/*\0*/;
      char *buffer = malloc(buffSize);
      if (buffer == NULL) error("Unable to allocate %d buffer for column names", buffSize);
      char *ch = buffer;
      if (doRowNames) {
        if (quote!=FALSE) { *ch++='"'; *ch++='"'; } // to match write.csv
        *ch++ = sep;
      }
      for (int j=0; j<ncol; j++) {
        writeString(names, j, &ch);
        *ch++ = sep;
      }
      ch--;  // backup onto the last sep after the last column
      write_chars(eol, &ch);  // replace it with the newline
      if (f==-1) { *ch='\0'; Rprintf(buffer); }
      else if (WRITE(f, buffer, (int)(ch-buffer))==-1) {
        int errwrite=errno;
        close(f); // the close might fail too but we want to report the write error
        free(buffer);
        error("%s: '%s'", strerror(errwrite), filename);
      }
      free(buffer);
    }
  }
  if (verbose) Rprintf("done in %.3fs\n", 1.0*(clock()-t0)/CLOCKS_PER_SEC);
  if (nrow == 0) {
    if (verbose) Rprintf("No data rows present (nrow==0)\n");
    if (f!=-1 && CLOSE(f)) error("%s: '%s'", strerror(errno), filename);
    UNPROTECT(protecti);
    return(R_NilValue);
  }

  // Decide buffer size and rowsPerBatch for each thread
  // Once rowsPerBatch is decided it can't be changed, but we can increase buffer size if the lines
  // turn out to be longer than estimated from the sample.
  // buffSize large enough to fit many lines to i) reduce calls to write() and ii) reduce thread sync points
  // It doesn't need to be small in cache because it's written contiguously.
  // If we don't use all the buffer for any reasons that's ok as OS will only page in the pages touched.
  // So, generally the larger the better up to max filesize/nth to use all the threads. A few times
  //   smaller than that though, to achieve some load balancing across threads since schedule(dynamic).
  int buffMB = INTEGER(buffMB_Arg)[0]; // checked at R level between 1 and 1024
  if (buffMB<1 || buffMB>1024) error("buffMB=%d outside [1,1024]", buffMB); // check it again even so
  size_t buffSize = 1024*1024*buffMB;
  if (maxLineLen > buffSize) buffSize=2*maxLineLen;  // A very long line; at least 1,048,576 characters
  rowsPerBatch =
    (10*maxLineLen > buffSize) ? 1 :  // very long lines (100,000 characters+) we'll just do one row at a time.
    0.5 * buffSize/maxLineLen;        // Aim for 50% buffer usage. See checkBuffer for comments.
  if (rowsPerBatch > nrow) rowsPerBatch=nrow;
  int numBatches = (nrow-1)/rowsPerBatch + 1;
  if (numBatches < nth) nth = numBatches;
  if (verbose) {
    Rprintf("Writing %d rows in %d batches of %d rows (each buffer size %dMB, showProgress=%d, nth=%d) ... ",
    nrow, numBatches, rowsPerBatch, buffMB, showProgress, nth);
    if (f==-1) Rprintf("\n");
  }
  t0 = clock();

  failed=0;  // static global so checkBuffer can set it. -errno for malloc or realloc fails, +errno for write fail
  Rboolean hasPrinted=FALSE;
  Rboolean anyBufferGrown=FALSE;
  int maxBuffUsedPC=0;

  #pragma omp parallel num_threads(nth)
  {
    char *ch, *buffer;               // local to each thread
    ch = buffer = malloc(buffSize);  // each thread has its own buffer
    // Don't use any R API alloc here (e.g. R_alloc); they are
    // not thread-safe as per last sentence of R-exts 6.1.1.

    if (buffer==NULL) {failed=-errno;}
    // Do not rely on availability of '#omp cancel' new in OpenMP v4.0 (July 2013).
    // OpenMP v4.0 is in gcc 4.9+ (https://gcc.gnu.org/wiki/openmp) but
    // not yet in clang as of v3.8 (http://openmp.llvm.org/)
    // If not-me failed, I'll see shared 'failed', fall through loop, free my buffer
    // and after parallel section, single thread will call R API error() safely.

    size_t myAlloc = buffSize;
    size_t myMaxLineLen = maxLineLen;
    // so we can realloc(). Should only be needed if there are very long single CHARSXP
    // much longer than occurred in the sample for maxLineLen. Or for list() columns
    // contain vectors which are much longer than occurred in the sample.

    #pragma omp single
    {
      nth = omp_get_num_threads();  // update nth with the actual nth (might be different than requested)
    }
    int me = omp_get_thread_num();

    #pragma omp for ordered schedule(dynamic)
    for(RLEN start=0; start<nrow; start+=rowsPerBatch) {
      if (failed) continue;  // Not break. See comments above about #omp cancel
      int end = ((nrow-start)<rowsPerBatch) ? nrow : start+rowsPerBatch;

      for (RLEN i=start; i<end; i++) {
        char *lineStart = ch;
        if (doRowNames) {
          if (rowNames==NULL) {
            if (quote!=FALSE) *ch++='"';  // default 'auto' will quote the row.name numbers
            write_positive_int(i+1, &ch);
            if (quote!=FALSE) *ch++='"';
          } else {
            writeString(rowNames, i, &ch);
          }
          *ch++=sep;
        }
        for (int j=0; j<ncol; j++) {
          (*fun[j])(VECTOR_ELT(DF, j), i, &ch);
          *ch++ = sep;
        }
        ch--;  // backup onto the last sep after the last column. ncol>=1 because 0-columns was caught earlier.
        write_chars(eol, &ch);  // replace it with the newline.

        // Track longest line seen so far. If we start to see longer lines than we saw in the
        // sample, we'll realloc the buffer. The rowsPerBatch chosen based on the (very good) sample,
        // must fit in the buffer. Can't early write and reset buffer because the
        // file output would be out-of-order. Can't change rowsPerBatch after the 'parallel for' started.
        size_t thisLineLen = ch-lineStart;
        if (thisLineLen > myMaxLineLen) myMaxLineLen=thisLineLen;
        checkBuffer(&buffer, &myAlloc, &ch, myMaxLineLen);
        if (failed) break; // this thread stop writing rows; fall through to clear up and error() below
      }
      #pragma omp ordered
      {
        if (!failed) { // a thread ahead of me could have failed below while I was working or waiting above
          if (f==-1) {
            *ch='\0';  // standard C string end marker so Rprintf knows where to stop
            Rprintf(buffer);
            // nth==1 at this point since when file=="" (f==-1 here) fwrite.R calls setDTthreads(1)
            // Although this ordered section is one-at-a-time it seems that calling Rprintf() here, even with a
            // R_FlushConsole() too, causes corruptions on Windows but not on Linux. At least, as observed so
            // far using capture.output(). Perhaps Rprintf() updates some state or allocation that cannot be done
            // by slave threads, even when one-at-a-time. Anyway, made this single-threaded when output to console
            // to be safe (setDTthreads(1) in fwrite.R) since output to console doesn't need to be fast.
          } else {
            if (WRITE(f, buffer, (int)(ch-buffer)) == -1) {
              failed=errno;
            }
            if (myAlloc > buffSize) anyBufferGrown = TRUE;
            int used = 100*((double)(ch-buffer))/buffSize;  // percentage of original buffMB
            if (used > maxBuffUsedPC) maxBuffUsedPC = used;
            time_t now;
            if (me==0 && showProgress && (now=time(NULL))>=next_time && !failed) {
              // See comments above inside the f==-1 clause.
              // Not only is this ordered section one-at-a-time but we'll also Rprintf() here only from the
              // master thread (me==0) and hopefully this will work on Windows. If not, user should set
              // showProgress=FALSE until this can be fixed or removed.
              int ETA = (int)((nrow-end)*(((double)(now-start_time))/end));
              if (hasPrinted || ETA >= 2) {
                if (verbose && !hasPrinted) Rprintf("\n");
                Rprintf("\rWritten %.1f%% of %d rows in %d secs using %d thread%s. "
                        "anyBufferGrown=%s; maxBuffUsed=%d%%. ETA %d secs.      ",
                         (100.0*end)/nrow, nrow, (int)(now-start_time), nth, nth==1?"":"s",
                         anyBufferGrown?"yes":"no", maxBuffUsedPC, ETA);
                R_FlushConsole();    // for Windows
                next_time = now+1;
                hasPrinted = TRUE;
              }
            }
            // May be possible for master thread (me==0) to call R_CheckUserInterrupt() here.
            // Something like:
            // if (me==0) {
            //   failed = TRUE;  // inside ordered here; the slaves are before ordered and not looking at 'failed'
            //   R_CheckUserInterrupt();
            //   failed = FALSE; // no user interrupt so return state
            // }
            // But I fear the slaves will hang waiting for the master (me==0) to complete the ordered
            // section which may not happen if the master thread has been interrupted. Rather than
            // seeing failed=TRUE and falling through to free() and close() as intended.
            // Could register a finalizer to free() and close() perhaps :
            // http://r.789695.n4.nabble.com/checking-user-interrupts-in-C-code-tp2717528p2717722.html
            // Conclusion for now: do not provide ability to interrupt.
            // write() errors and malloc() fails will be caught and cleaned up properly, however.
          }
          ch = buffer;  // back to the start of my buffer ready to fill it up again
        }
      }
    }
    free(buffer);
    // all threads will call this free on their buffer, even if one or more threads had malloc
    // or realloc fail. If the initial malloc failed, free(NULL) is ok and does nothing.
  }
  // Finished parallel region and can call R API safely now.
  if (hasPrinted) {
    if (!failed) {
      // clear the progress meter
      Rprintf("\r                                                                       "
              "                                                              \r");
      R_FlushConsole();  // for Windows
    } else {
      // unless failed as we'd like to see anyBufferGrown and maxBuffUsedPC
      Rprintf("\n");
    }
  }
  if (f!=-1 && CLOSE(f) && !failed)
    error("%s: '%s'", strerror(errno), filename);
  // quoted '%s' in case of trailing spaces in the filename
  // If a write failed, the line above tries close() to clean up, but that might fail as well. So the
  // '&& !failed' is to not report the error as just 'closing file' but the next line for more detail
  // from the original error.
  if (failed<0) {
    error("%s. One or more threads failed to malloc or realloc their private buffer. nThread=%d and initial buffMB per thread was %d.\n", strerror(-failed), nth, buffMB);
  } else if (failed>0) {
    error("%s: '%s'", strerror(failed), filename);
  }
  if (verbose) Rprintf("done (actual nth=%d, anyBufferGrown=%s, maxBuffUsed=%d%%)\n",
                       nth, anyBufferGrown?"yes":"no", maxBuffUsedPC);
  UNPROTECT(protecti);
  return(R_NilValue);
}

