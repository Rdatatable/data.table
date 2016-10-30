#include "data.table.h"
#include <errno.h>
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

#define NUM_SF 15
#define DECIMAL_SEP '.'  // TODO allow other decimal separator e.g. ','

// Globals for this file only (written once to hold parameters passed from R level)
static int na_len;
static const char *na_str;

static inline void writeInteger(int x, char **thisCh)
{
  char *ch = *thisCh;
  if (x == NA_INTEGER) {
    if (na_len) { memcpy(ch, na_str, na_len); ch += na_len; }
  } else if (x == 0) {
    *ch++ = '0';
  } else {
    if (x<0) { *ch++ = '-'; x=-x; }
    // avoid log() call for speed. write backwards then reverse when we know how long
    int width = 0;
    while (x>0) { *ch++ = '0'+x%10; x /= 10; width++; }
    for (int i=width/2; i>0; i--) {
      char tmp=*(ch-i);
      *(ch-i) = *(ch-width+i-1);
      *(ch-width+i-1) = tmp;
    }
  }
  *thisCh = ch;
}

SEXP traceAccuracy() {
  if (sizeof(long double)!=16) Rprintf("sizeof long double is %d not 16",sizeof(long double));
  if (sizeof(unsigned long long)!=8) Rprintf("sizeof unsigned long long is %d not 8\n",sizeof(unsigned long long));  
  if (ULLONG_MAX != 18446744073709551615ull)  // 2^64-1
      Rprintf("Expected ULLONG_MAX=%llu but it is %llu\n",
                             18446744073709551615ull, ULLONG_MAX);      
  unsigned long long foo =    5000000000000000000ull;
  //            when we add  10000000000000000000ull later,
  //  careful to check acc <= 8446744073709551615ull
  unsigned long long thresh = 1000000000000000000ull;
  for (int i=1, dp=-19; i<=52; i++) {
    long double ld1, ld2;
    memset(&ld1, 0, 16);  // the compiler/machine may well not use all the bits e.g. 80 not 128.
    memset(&ld2, 0, 16);  //    so clear that memory so that memcpy can work below
    ld1 = 1.L/(1ull<<i);  
    ld2 = (long double)ldexpl(1.0L,-i);
    int cmp=memcmp(&ld1,&ld2,16);
    const char *err = (cmp!=0) ? "**bits differ" : "";
    Rprintf("{%llu, %d}, // 2^%03d = %.60Lf %.60Lf  %s\n",foo,dp,-i,ld1,ld2,err);
    foo>>=1;
    if (foo<thresh) { foo*=10; dp--; }
  }
  return(R_NilValue);
}
/* same on linux and windows
{5000000000000000000, -19}, // 2^-01 = 0.500000000000000000000000000000000000000000000000000000000000
{2500000000000000000, -19}, // 2^-02 = 0.250000000000000000000000000000000000000000000000000000000000
{1250000000000000000, -19}, // 2^-03 = 0.125000000000000000000000000000000000000000000000000000000000
{6250000000000000000, -20}, // 2^-04 = 0.062500000000000000000000000000000000000000000000000000000000
{3125000000000000000, -20}, // 2^-05 = 0.031250000000000000000000000000000000000000000000000000000000
{1562500000000000000, -20}, // 2^-06 = 0.015625000000000000000000000000000000000000000000000000000000
{7812500000000000000, -21}, // 2^-07 = 0.007812500000000000000000000000000000000000000000000000000000
{3906250000000000000, -21}, // 2^-08 = 0.003906250000000000000000000000000000000000000000000000000000
{1953125000000000000, -21}, // 2^-09 = 0.001953125000000000000000000000000000000000000000000000000000
{9765625000000000000, -22}, // 2^-10 = 0.000976562500000000000000000000000000000000000000000000000000
{4882812500000000000, -22}, // 2^-11 = 0.000488281250000000000000000000000000000000000000000000000000
{2441406250000000000, -22}, // 2^-12 = 0.000244140625000000000000000000000000000000000000000000000000
{1220703125000000000, -22}, // 2^-13 = 0.000122070312500000000000000000000000000000000000000000000000
{6103515625000000000, -23}, // 2^-14 = 0.000061035156250000000000000000000000000000000000000000000000
{3051757812500000000, -23}, // 2^-15 = 0.000030517578125000000000000000000000000000000000000000000000  
{1525878906250000000, -23}, // 2^-16 = 0.000015258789062500000000000000000000000000000000000000000000  
{7629394531250000000, -24}, // 2^-17 = 0.000007629394531250000000000000000000000000000000000000000000  
{3814697265625000000, -24}, // 2^-18 = 0.000003814697265625000000000000000000000000000000000000000000  
{1907348632812500000, -24}, // 2^-19 = 0.000001907348632812500000000000000000000000000000000000000000  
{9536743164062500000, -25}, // 2^-20 = 0.000000953674316406250000000000000000000000000000000000000000  
{4768371582031250000, -25}, // 2^-21 = 0.000000476837158203125000000000000000000000000000000000000000  
{2384185791015625000, -25}, // 2^-22 = 0.000000238418579101562500000000000000000000000000000000000000  
{1192092895507812500, -25}, // 2^-23 = 0.000000119209289550781250000000000000000000000000000000000000  
{5960464477539062500, -26}, // 2^-24 = 0.000000059604644775390625000000000000000000000000000000000000  
{2980232238769531250, -26}, // 2^-25 = 0.000000029802322387695312500000000000000000000000000000000000  
{1490116119384765625, -26}, // 2^-26 = 0.000000014901161193847656250000000000000000000000000000000000  
{7450580596923828120, -27}, // 2^-27 = 0.000000007450580596923828125000000000000000000000000000000000  
{3725290298461914060, -27}, // 2^-28 = 0.000000003725290298461914062500000000000000000000000000000000  
{1862645149230957030, -27}, // 2^-29 = 0.000000001862645149230957031250000000000000000000000000000000  
{9313225746154785150, -28}, // 2^-30 = 0.000000000931322574615478515625000000000000000000000000000000  
{4656612873077392575, -28}, // 2^-31 = 0.000000000465661287307739257812500000000000000000000000000000  
{2328306436538696287, -28}, // 2^-32 = 0.000000000232830643653869628906250000000000000000000000000000  
{1164153218269348143, -28}, // 2^-33 = 0.000000000116415321826934814453125000000000000000000000000000  
{5820766091346740710, -29}, // 2^-34 = 0.000000000058207660913467407226562500000000000000000000000000   
{2910383045673370355, -29}, // 2^-35 = 0.000000000029103830456733703613281250000000000000000000000000  
{1455191522836685177, -29}, // 2^-36 = 0.000000000014551915228366851806640625000000000000000000000000  
{7275957614183425880, -30}, // 2^-37 = 0.000000000007275957614183425903320312500000000000000000000000  
{3637978807091712940, -30}, // 2^-38 = 0.000000000003637978807091712951660156250000000000000000000000  
{1818989403545856470, -30}, // 2^-39 = 0.000000000001818989403545856475830078125000000000000000000000
{9094947017729282350, -31}, // 2^-40 = 0.000000000000909494701772928237915039062500000000000000000000
{4547473508864641175, -31}, // 2^-41 = 0.000000000000454747350886464118957519531250000000000000000000
{2273736754432320587, -31}, // 2^-42 = 0.000000000000227373675443232059478759765625000000000000000000
{1136868377216160293, -31}, // 2^-43 = 0.000000000000113686837721616029739379882812500000000000000000
{5684341886080801460, -32}, // 2^-44 = 0.000000000000056843418860808014869689941406250000000000000000
{2842170943040400730, -32}, // 2^-45 = 0.000000000000028421709430404007434844970703125000000000000000
{1421085471520200365, -32}, // 2^-46 = 0.000000000000014210854715202003717422485351562500000000000000
{7105427357601001820, -33}, // 2^-47 = 0.000000000000007105427357601001858711242675781250000000000000
{3552713678800500910, -33}, // 2^-48 = 0.000000000000003552713678800500929355621337890625000000000000
{1776356839400250455, -33}, // 2^-49 = 0.000000000000001776356839400250464677810668945312500000000000
{8881784197001252270, -34}, // 2^-50 = 0.000000000000000888178419700125232338905334472656250000000000
{4440892098500626135, -34}, // 2^-51 = 0.000000000000000444089209850062616169452667236328125000000000
{2220446049250313067, -34}, // 2^-52 = 0.000000000000000222044604925031308084726333618164062500000000
*/

union {
  double d;
  unsigned long long ull;
} u;

static inline void writeNumeric(double x, char **thisCh)
{
  // hand-rolled / specialized for speed
  // *thisCh is safely the output destination with enough space (ensured via calculating maxLineLen up front)
  // technique similar to base R (format.c:formatReal and printutils.c:EncodeReal0)
  // differences/tricks :
  //   i) no buffers. writes straight to the final file buffer passed to write()
  //  ii) no C libary calls such as sprintf() where the fmt string has to be interpretted over and over
  // iii) no need to return variables or flags.  Just writes.
  //  iv) shorter, easier to read and reason with. In one self contained place.
  char *ch = *thisCh;
  if (!R_FINITE(x)) {
    if (ISNAN(x)) {
      if (na_len) { memcpy(ch, na_str, na_len); ch += na_len; }  // by default na_len==0 and the memcpy call will be skipped
    } else if (x>0) {
      *ch++ = 'I'; *ch++ = 'n'; *ch++ = 'f';
    } else {
      *ch++ = '-'; *ch++ = 'I'; *ch++ = 'n'; *ch++ = 'f';
    }
  } else if (x == 0.0) {
    *ch++ = '0';   // and we're done.  so much easier rather than passing back special cases
  } else {
    if (x < 0.0) { *ch++ = '-'; x = -x; }  // and we're done on sign, already written. no need to pass back sign
    
    /*
    int exp = (int)floorl(log10l((long double)x));
    unsigned long long l = (unsigned long long)((long double)x * powl(10.0L, NUM_SF-exp));
    */
    
    u.d = x;
    unsigned long long fraction = u.ull & ((1ULL<<52)-1);  // TO const & 0x...
    int exponent = ((int)(u.ull>>52) & 0x7FF) - 1023;
    long double acc = 0;
    for (int i=0; i<52; i++) { 
      // important smallest first; i.e. 2^-52
      if ( fraction & (1ULL<<i) ) acc+=ldexpl(1.0L,i-52);
      // TODO lookup table and avoid branch
      // TODO skip batches of 0 in the fraction by skipping over trailing 0 bytes perhaps
    }
    long double y = ldexpl(1.0L+acc, exponent);
    
    //long double y = 1.0 + fraction / 4503599627370496.0L;  // 2^52 as long double
    //y = ldexpl(y, exponent-1023);
    // long double y = (long double)frexpl(x, &e2);
    // int nd = (int)(e2 * 0.30102999566);  // * log10(2)
    // y *= (powl(2.0, e2) / powl(10.0, nd));  // doesn't seem efficient or accurate
    
    // since y was 1.* above so surely we know
    int exp = (int)floorl(log10l(y));
    unsigned long long l = (unsigned long long)(y * powl(10.0L, NUM_SF-exp));
    /*
    if (y<1) { y *= 1e15; exp++; }
    else  
    y *= 1e15;
    unsigned long long l = (unsigned long long)y;
    int exp = 
    
    int m = log10(2^e2)
    y * powl(10.0, NUM_DF)
    
    int exp = (int)floor(log10(x));
    
    unsigned long long l = (unsigned long long)(y *
                                                 (long double)powl((long double)10,(long double)(NUM_SF-exp)));
    //exp+=nd;
    */
    
    // TODO?: use lookup table like base R? ................. ^^^^
    //        here in fwrite it might make a difference whereas in base R other very
    //        significant write.table inefficiency dominates.
    // long double needed for 1729.9 to ensure 1e-310 doesn't write as 0.
    // Peppered with long double as windows seems to need that
    // l now contains NUM_SF+1 digits.
    // ULL for compound accuracy. If double, the repeated base 10 ops below could compound errors
    if (l%10 >= 5) l+=10; // use the last digit to round
    l /= 10;
    if (l == 0) {
      if (*(ch-1)=='-') ch--;
      *ch++ = '0';
    } else {
      // Count trailing zeros and therefore s.f. present in l
      int trailZero = 0;
      while (l%10 == 0) { l /= 10; trailZero++; }
      int sf = NUM_SF - trailZero;
      if (sf==0) {sf=1; exp++;}  // e.g. l was 9999999[5-9] rounded to 10000000 which added 1 digit
      
      // l is now an unsigned long that doesn't start or end with 0
      // sf is the number of digits now in l
      // exp is e<exp> were l to be written with the decimal sep after the first digit
      int dr = sf-exp-1; // how many characters to print to the right of the decimal place
      int width=0;       // field width were it written decimal format. Used to decide whether to or not.
      int dl0=0;         // how many 0's to add to the left of the decimal place before starting l
      if (dr<=0) { dl0=-dr; dr=0; width=sf+dl0; }  // 1, 10, 100, 99000
      else {
        if (sf>dr) width=sf+1;                     // 1.234 and 123.4
        else { dl0=1; width=dr+1+dl0; }            // 0.1234, 0.0001234
      }
      // So:  3.1416 => l=31416, sf=5, exp=0     dr=4; dl0=0; width=6
      //      30460  => l=3046, sf=4, exp=4      dr=0; dl0=1; width=5
      //      0.0072 => l=72, sf=2, exp=-3       dr=4; dl0=1; width=6
      if (width <= sf + (sf>1) + 2 + (abs(exp)>99?3:2)) {
         //              ^^^^ to not include 1 char for dec in -7e-04 where sf==1
         //                      ^ 2 for 'e+'/'e-'
         // decimal format ...
         ch += width-1;
         if (dr) {
           while (dr && sf) { *ch--='0'+l%10; l/=10; dr--; sf--; }
           while (dr) { *ch--='0'; dr--; }
           *ch-- = DECIMAL_SEP;
         }
         while (dl0) { *ch--='0'; dl0--; }
         while (sf) { *ch--='0'+l%10; l/=10; sf--; }
         // ch is now 1 before the first char of the field so position it afterward again, and done
         ch += width+1;
      } else {
        // scientific ...
        ch += sf;  // sf-1 + 1 for dec
        for (int i=sf; i>1; i--) {
          *ch-- = '0' + l%10;   
          l /= 10;
        }
        if (sf == 1) ch--; else *ch-- = DECIMAL_SEP;
        *ch = '0' + l;
        ch += sf + (sf>1);
        *ch++ = 'e';  // lower case e to match base::write.csv
        if (exp < 0) { *ch++ = '-'; exp=-exp; }
        else { *ch++ = '+'; }  // to match base::write.csv
        if (exp < 100) {
          *ch++ = '0' + (exp / 10);
          *ch++ = '0' + (exp % 10);
        } else {
          *ch++ = '0' + (exp / 100);
          *ch++ = '0' + (exp / 10) % 10;
          *ch++ = '0' + (exp % 10);
        }
      }
    }
  }
  *thisCh = ch;
}

SEXP writefile(SEXP list_of_columns,
               SEXP filenameArg,
               SEXP col_sep_Arg,
               SEXP row_sep_Arg,
               SEXP na_Arg,
               SEXP quoteArg,           // TRUE|FALSE
               SEXP qmethod_escapeArg,  // TRUE|FALSE
               SEXP append,             // TRUE|FALSE
               SEXP col_names,          // TRUE|FALSE
               SEXP verboseArg,
               SEXP turboArg)
{
  if (!isNewList(list_of_columns)) error("fwrite must be passed an object of type list, data.table or data.frame");
  RLEN ncols = length(list_of_columns);
  if (ncols==0) error("fwrite must be passed a non-empty list");
  RLEN nrows = length(VECTOR_ELT(list_of_columns, 0));
  for (int i=1; i<ncols; i++) {
    if (nrows != length(VECTOR_ELT(list_of_columns, i)))
      error("Column %d's length (%d) is not the same as column 1's length (%d)", i+1, length(VECTOR_ELT(list_of_columns, i)), nrows);
  }
#ifndef _OPENMP
  Rprintf("Your platform/environment has not detected OpenMP support. fwrite() will still work, but slower in single threaded mode.\n");
  // Rprintf rather than warning() because warning() would cause test.data.table() to error about the unexpected warnings
#endif 
  const Rboolean verbose = LOGICAL(verboseArg)[0];
  const Rboolean quote = LOGICAL(quoteArg)[0];
  const Rboolean turbo = LOGICAL(turboArg)[0];
  
  const char col_sep = *CHAR(STRING_ELT(col_sep_Arg, 0));  // DO NOT DO: allow multichar separator (bad idea)
  
  const char *row_sep = CHAR(STRING_ELT(row_sep_Arg, 0));
  const int row_sep_len = strlen(row_sep);  // someone somewhere might want a trailer on every line
  na_str = CHAR(STRING_ELT(na_Arg, 0));
  na_len = strlen(na_str);
  const char QUOTE = '"';
  const char ESCAPE = '\\';
  const Rboolean qmethod_escape = LOGICAL(qmethod_escapeArg)[0];
  const char ESCAPE_QUOTE = qmethod_escape ? ESCAPE : QUOTE;
  const char *filename = CHAR(STRING_ELT(filenameArg, 0));

  errno = 0;   // clear flag possibly set by previous errors
  int f;
  if (*filename=='\0') f=-1;  // file="" means write to standard output
  else { 
#ifdef WIN32
    f = _open(filename, _O_WRONLY | _O_BINARY | _O_CREAT | (LOGICAL(append)[0] ? _O_APPEND : _O_TRUNC), _S_IWRITE);
    // row_sep must be passed from R level as '\r\n' on Windows since write() only auto-converts \n to \r\n
    // in _O_TEXT mode. We use O_BINARY for full control and perhaps speed since O_TEXT must have to deep branch an if('\n')
#else
    f = open(filename, O_WRONLY | O_CREAT | (LOGICAL(append)[0] ? O_APPEND : O_TRUNC), 0644);
#endif
    if (f == -1) {
      if( access( filename, F_OK ) != -1 )
        error("File exists and failed to open for writing. Do you have write permission to it? Is this Windows and does another process such as Excel have it open? File: %s", filename);
      else 
        error("Unable to create new file for writing (it does not exist already). Do you have permission to write here and is there space on the disk? File: %s", filename); 
    }
  }
  int true_false;
  
  clock_t t0=clock();
  // i) prefetch levels of factor columns (if any) to save getAttrib on every field on every row of any factor column
  // ii) calculate certain upper bound of line length
  SEXP levels[ncols];  // on-stack vla
  int lineLenMax = 2;  // initialize with eol max width of \r\n on windows
  int sameType = TYPEOF(VECTOR_ELT(list_of_columns, 0));
  for (int col_i=0; col_i<ncols; col_i++) {
    SEXP column = VECTOR_ELT(list_of_columns, col_i);
    if (TYPEOF(column) != sameType) sameType = 0;
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
        sameType = 0; // TODO: enable deep-switch-avoidance for all columns factor
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
  clock_t tlineLenMax=clock()-t0;
  if (verbose) Rprintf("Maximum line length is %d calculated in %.3fs\n", lineLenMax, 1.0*tlineLenMax/CLOCKS_PER_SEC);
  // TODO: could parallelize by column, but currently no need as insignificant time
  t0=clock();

  if (verbose) Rprintf("Writing column names ... ");
  if (LOGICAL(col_names)[0]) {
    SEXP names = getAttrib(list_of_columns, R_NamesSymbol);  
    if (names!=NULL) {
      if (LENGTH(names) != ncols) error("Internal error: length of column names is not equal to the number of columns. Please report.");
      int bufSize = 0;
      for (int col_i=0; col_i<ncols; col_i++) bufSize += LENGTH(STRING_ELT(names, col_i));
      bufSize *= 1+quote;  // in case every colname is filled with quotes to be escaped!
      bufSize += ncols*(2*quote + 1) + 3;
      char *buffer = malloc(bufSize);
      if (buffer == NULL) error("Unable to allocate %dMB buffer for column names", bufSize/(1024*1024));
      char *ch = buffer;
      for (int col_i=0; col_i<ncols; col_i++) {
        SEXP str = STRING_ELT(names, col_i);
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
      if (f==-1) { *ch='\0'; Rprintf(buffer); }
      else if (WRITE(f, buffer, (int)(ch-buffer))==-1) { close(f); error("Error writing to file: %s", filename); }
      free(buffer);
    }
  }
  if (verbose) Rprintf("done in %.3fs\n", 1.0*(clock()-t0)/CLOCKS_PER_SEC);
  if (nrows == 0) {
    if (verbose) Rprintf("No data rows present (nrow==0)\n");
    if (f!=-1 && CLOSE(f)) error("Error closing file: %s", filename);
    return(R_NilValue);
  }

  // Decide buffer size on each core
  // Large enough to fit many lines (to reduce calls to write()). Small enough to fit in each core's cache.
  // If the lines turn out smaller, that's ok. we just won't use all the buffer in that case. But we must be
  // sure to allow for worst case; i.e. every row in the batch all being the maximum line length.
  int bufSize = 1*1024*1024;  // 1MB  TODO: experiment / fetch cache size
  if (lineLenMax > bufSize) bufSize = lineLenMax;
  const int rowsPerBatch = bufSize/lineLenMax;
  const int numBatches = (nrows-1)/rowsPerBatch + 1;
  if (verbose) Rprintf("Writing data rows in %d batches of %d rows (each buffer size %.3fMB, turbo=%d) ... ",
    numBatches, rowsPerBatch, 1.0*bufSize/(1024*1024), turbo);
  t0 = clock();
  
  int nth;
  #pragma omp parallel num_threads(getDTthreads())
  {
    char *buffer = malloc(bufSize);  // one buffer per thread
    // TODO Ask Norm how to error() safely ... if (buffer == NULL) error("Unable to allocate %dMB buffer", bufSize/(1024*1024)); 
    char *ch = buffer;
    #pragma omp single
    {
      nth = omp_get_num_threads();
    }    
    #pragma omp for ordered schedule(dynamic)
    for(RLEN start_row = 0; start_row < nrows; start_row += rowsPerBatch) { 
      int upp = start_row + rowsPerBatch;
      if (upp > nrows) upp = nrows;
      if (turbo && sameType == REALSXP) {
        // avoid deep switch. turbo switches on both sameType and specialized writeNumeric
        for (RLEN row_i = start_row; row_i < upp; row_i++) {
          for (int col_i = 0; col_i < ncols; col_i++) {
            SEXP column = VECTOR_ELT(list_of_columns, col_i);
            writeNumeric(REAL(column)[row_i], &ch);
            *ch++ = col_sep;
          }
          ch--;  // backup onto the last col_sep after the last column
          memcpy(ch, row_sep, row_sep_len);  // replace it with the newline.
          ch += row_sep_len;
        }
      } else if (turbo && sameType == INTSXP) {
        for (RLEN row_i = start_row; row_i < upp; row_i++) {
          for (int col_i = 0; col_i < ncols; col_i++) {
            SEXP column = VECTOR_ELT(list_of_columns, col_i);
            writeInteger(INTEGER(column)[row_i], &ch);
            *ch++ = col_sep;
          }
          ch--;
          memcpy(ch, row_sep, row_sep_len);
          ch += row_sep_len;
        }
      } else {
        for (RLEN row_i = start_row; row_i < upp; row_i++) {
          for (int col_i = 0; col_i < ncols; col_i++) {
            SEXP column = VECTOR_ELT(list_of_columns, col_i);
            SEXP str;
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
                if (turbo) {
                  // if there are any problems with the hand rolled double writing, then turbo=FALSE reverts to standard library
                  writeNumeric(REAL(column)[row_i], &ch);
                } else {
                  //tt0 = clock();
                  ch += sprintf(ch, "%.15G", REAL(column)[row_i]);
                  //tNUM += clock()-tt0;
                }
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
                if (turbo) {
                  writeInteger(INTEGER(column)[row_i], &ch);
                } else {
                  ch += sprintf(ch, "%d", INTEGER(column)[row_i]);
                }
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
            // default:
            // An uncovered type would have already thrown above when calculating maxLineLen earlier
            }
            *ch++ = col_sep;
          }
          ch--;  // backup onto the last col_sep after the last column
          memcpy(ch, row_sep, row_sep_len);  // replace it with the newline. TODO: replace memcpy call with eol1 eol2 --eolLen 
          ch += row_sep_len;
        }
      }
      #pragma omp ordered
      {
        if (f==-1) { *ch='\0'; Rprintf(buffer); }
        else WRITE(f, buffer, (int)(ch-buffer));
        // TODO: safe way to throw error from this thread if write fails (e.g. out disk space)
        //       { close(f); error("Error writing to file: %s", filename) };
        ch = buffer;
      }
    }
    free(buffer);
  }
  if (verbose) Rprintf("all %d threads done\n", nth);  // TO DO: report elapsed time since (clock()-t0)/NTH is only estimate
  if (f!=-1 && CLOSE(f)) error("Error closing file: %s", filename);
  return(R_NilValue);  // must always return SEXP from C level otherwise hang on Windows
}



