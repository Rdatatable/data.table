#include "data.table.h"
#include <Rdefines.h>
#include <ctype.h>
#include <errno.h>

#ifdef WIN32         // means WIN64, too
#include <windows.h>
#include <stdio.h>
#include <tchar.h>
#include <inttypes.h>  // for PRId64
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>   // for open()
#include <unistd.h>  // for close()
#endif
#include <signal.h> // the debugging machinery + breakpoint aidee

/*****    TO DO    *****
Restore test 1339 (balanced embedded quotes, see ?fread already updated).
Confirm: http://stackoverflow.com/questions/23833294/data-tablefread-doesnt-like-missing-values-in-first-column
construct test and investigate skip for completeness here: http://stackoverflow.com/questions/22086780/data-table-fread-error
http://stackoverflow.com/questions/22229109/r-data-table-fread-command-how-to-read-large-files-with-irregular-separators
http://r.789695.n4.nabble.com/Odd-problem-using-fread-to-read-in-a-csv-file-no-data-just-headers-tp4686302.html
And even more diagnostics to verbose=TRUE so we can see where crashes are.
Detect and coerce dates and times. By searching for - and :, and dateTtime etc, or R's own method or fasttime. POSIXct default, for microseconds? : http://stackoverflow.com/questions/14056370/cast-string-to-idatetime
Add as.colClasses to fread.R after return from C level (e.g. for colClasses "Date", although as slow as read.csv via character)
Allow comment char to ignore. Important in format detection. But require valid line data before comment character in the read loop? See http://stackoverflow.com/a/18922269/403310
Deal with row.names e.g. http://stackoverflow.com/questions/15448732/reading-csv-with-row-names-by-fread
Test Garrett's two files again (wrap around ,,,,,, and different row lengths that the wc -l now fixes)
Post from patricknik on 5 Jan re ""b"" in a field. And Aykut Firat on email.
Save repeated ch<eof checking in main read step. Last line might still be tricky if last line has no eol.
Test using at least "grep read.table ...Rtrunk/tests/
Look for any non-alpha-numeric characters in the output and try each of them. That way can handle bell character as well and save testing separators which aren't there.
Column all 0 and 1 treated as logical?
---
Secondary separator for list() columns, such as columns 11 and 12 in BED (no need for strsplit).
Add LaF comparison.
as.read.table=TRUE/FALSE option.  Or fread.table and fread.csv (see http://r.789695.n4.nabble.com/New-function-fread-in-v1-8-7-tp4653745p4654194.html).
*****/

static const char *eof; 
static char sep, eol, eol2;  // sep2 TO DO
static int quoteRule;
static int eolLen;
static SEXP nastrings;
static _Bool verbose, any_number_like_nastrings, blank_is_a_nastring;
static _Bool stripWhite;  // only applies to unquoted character columns; numeric fields always stripped
static _Bool skipEmptyLines, fill;
static cetype_t ienc;

// Define our own fread type codes, different to R's SEXPTYPE :
// i) INTEGER64 is not in R but an add on packages using REAL, we need to make a distinction here, without using
// class (for speed)
// ii) 0:n codes makes it easier to bump through types in this order using ++.
#define SXP_LGL    0   // LGLSXP    String values T,F,TRUE,FALSE,True,False
#define SXP_INT    1   // INTSXP
#define SXP_INT64  2   // REALSXP
#define SXP_REAL   3   // REALSXP
#define SXP_STR    4   // STRSXP
#define SXP_NULL   5   // NILSXP i.e. skip column (last so that all types can be bumped up to it by user)

#define NUMTYPE    6
static int TypeSxp[NUMTYPE] = {LGLSXP,INTSXP,REALSXP,REALSXP,STRSXP,NILSXP};
#define NUT        8   // Number of User Types (just for colClasses where "numeric"/"double" are equivalent)
static const char UserTypeName[NUT][10] = {"logical", "integer", "integer64", "numeric", "character", "NULL", "double", "CLASS" };
// important that first 6 correspond to TypeSxp.  "CLASS" is the fall back to character then as.class at R level ("CLASS" string is just a placeholder).
static int UserTypeNameMap[NUT] = { SXP_LGL, SXP_INT, SXP_INT64, SXP_REAL, SXP_STR, SXP_NULL, SXP_REAL, SXP_STR };
static char quote;
typedef _Bool (*reader_fun_t)(const char **, SEXP, int);

#define NJUMPS     10    // how many places in the file to jump to and test types there (the very end is added as 11th)
                         // not too many though so as not to slow down wide files; e.g. 10,000 columns
#define JUMPLINES 100    // at each jump, how many lines to guess column types

const char *fnam=NULL, *origmmp, *mmp;   // origmmp just needed to pass to munmap when BOM is skipped over using mmp+=3.
size_t filesize;
#ifdef WIN32
HANDLE hFile=0;
HANDLE hMap=0;
void closeFile() {
    if (fnam!=NULL) {
        UnmapViewOfFile(origmmp);
        CloseHandle(hMap);
        CloseHandle(hFile);
    }
}
#else
int fd=-1;
void closeFile() {    
    if (fnam!=NULL) {
      if (munmap((char *)origmmp, filesize) || close(fd)) error("%s: '%s'", strerror(errno), fnam);
    }
}
#endif

void STOP(const char *format, ...) {
    // Solves: http://stackoverflow.com/questions/18597123/fread-data-table-locks-files
    va_list args;
    va_start(args, format);
    char msg[2000];
    vsnprintf(msg, 2000, format, args);
    va_end(args);
    closeFile();  // some errors point to data in the file, hence via msg buffer first
    error(msg);
}

// Helper for error and warning messages to extract next 10 chars or \n if occurs first
// Used exclusively together with "%.*s"
int STRLIM(const char *ch, int limit) {
  int maxwidth = MIN(limit, (int)(eof-ch));
  char *newline = memchr(ch, eol, maxwidth);
  return (newline==NULL ? maxwidth : (int)(newline-ch));
}

void printTypes(unsigned char *type, int ncol) {
  // e.g. files with 10,000 columns, don't print all of it to verbose output.
  int tt=(ncol<=110?ncol:90); for (int i=0; i<tt; i++) Rprintf("%d",type[i]);
  if (ncol>110) { Rprintf("..."); for (int i=ncol-10; i<ncol; i++) Rprintf("%d",type[i]); }
}

static inline void skip_white(const char **this) {
  // skip space so long as sep isn't space and skip tab so long as sep isn't tab
  const char *ch = *this;
  while(ch<eof && (*ch==' ' || *ch== '\t') && *ch!=sep) ch++;
  *this = ch;
}

static inline _Bool on_sep(const char **this) {
  const char *ch = *this;
  if (sep==' ' && ch<eof && *ch==' ') {
    while (ch+1<eof && *(ch+1)==' ') ch++;  // move to last of this sequence of spaces
    if (ch+1==eof || *(ch+1)==eol) ch++;    // if that's followed by eol or eof then move onto those
  }
  *this = ch;
  return ch>=eof || *ch==sep || *ch==eol;
}

static inline void next_sep(const char **this) {
  const char *ch = *this;
  while (ch<eof && *ch!=sep && *ch!=eol) ch++;
  on_sep(&ch); // to deal with multiple spaces when sep==' '
  *this = ch;
}

static inline _Bool is_nastring(const char *lch) {
  skip_white(&lch);
  const char *start = lch;
  for (int i=0; i<length(nastrings); i++) {
    SEXP this = STRING_ELT(nastrings,i);   // these (if any) fixed constants will be hot so no fetch concerns
    int nchar = LENGTH(this);
    if (lch+nchar-1<eof && strncmp(lch, CHAR(this), nchar)==0) {
      lch += nchar;
      skip_white(&lch);
      if (lch>=eof || *lch==sep || *lch==eol) return TRUE;
      lch = start;
    }
  }
  return FALSE;
}

static _Bool Field(const char **this, SEXP targetCol, int targetRow)
{
  const char *ch = *this;
  if (stripWhite) skip_white(&ch);  // before and after quoted field's quotes too (e.g. test 1609) but never inside quoted fields
  const char *fieldStart=ch;
  Rboolean quoted = FALSE;
  if (*ch!=quote || quoteRule==3) {
    // unambiguously not quoted. simply search for sep|eol. If field contains sep|eol then it must be quoted instead.
    while(ch<eof && *ch!=sep && *ch!=eol) ch++;
  } else {
    // the field is quoted and quotes are correctly escaped (quoteRule 0 and 1)
    // or the field is quoted but quotes are not escaped (quoteRule 2)
    // or the field is not quoted but the data contains a quote at the start (quoteRule 2 too)
    int eolCount = 0;
    quoted = TRUE;
    fieldStart = ch+1; // step over opening quote
    switch(quoteRule) {
    case 0:  // quoted with embedded quotes doubled; the final unescaped " must be followed by sep|eol 
      while (++ch<eof && eolCount<100) {  // TODO: expose this 100 to user to allow them to increase
        eolCount += (*ch==eol);
        // 100 prevents runaway opening fields by limiting eols. Otherwise the whole file would be read in the sep and
        // quote rule testing step.
        if (*ch==quote) {
          if (ch+1<eof && *(ch+1)==quote) { ch++; continue; }
          break;  // found undoubled closing quote
        }
      }
      if (ch>=eof || *ch!=quote) return FALSE;
      break;
    case 1:  // quoted with embedded quotes escaped; the final unescaped " must be followed by sep|eol
      while (++ch<eof && *ch!=quote && eolCount<100) {
        eolCount += (*ch==eol);
        ch += (*ch=='\\');
      }
      if (ch>=eof || *ch!=quote) return FALSE;
      break;
    case 2:  // (i) quoted (perhaps because the source system knows sep is present) but any quotes were not escaped at all,
             // so look for ", to define the end.   (There might not be any quotes present to worry about, anyway).
             // (ii) not-quoted but there is a quote at the beginning so it should have been, look for , at the end
             // If no eol are present in the data (i.e. rows are rows), then this should work ok e.g. test 1453
             // since we look for ", and the source system quoted when , is present, looking for ", should work well.
             // No eol may occur inside fields, under this rule.
      {
        const char *ch2 = ch;  
        while (++ch<eof && *ch!=eol) {
          if (*ch==quote && (ch+1>=eof || *(ch+1)==sep || *(ch+1)==eol)) {ch2=ch; break;}   // (*1) regular ", ending
          if (*ch==sep) {
            // first sep in this field
            // if there is a ", afterwards but before the next \n, use that; the field was quoted and it's still case (i) above.
            // Otherwise break here at this first sep as it's case (ii) above (the data contains a quote at the start and no sep)
            ch2 = ch;
            while (++ch2<eof && *ch2!=eol) {
              if (*ch2==quote && (ch2+1>=eof || *(ch2+1)==sep || *(ch2+1)==eol)) {
                ch = ch2; // (*2) move on to that first ", -- that's this field's ending
                break;
              }
            }
            break;
          }
        }
        if (ch!=ch2) { fieldStart--; quoted=FALSE; } // field ending is this sep (neither (*1) or (*2) happened)
      }
      break;
    default:
      return FALSE;  // Internal error: undefined quote rule
    }
  }
  int fieldLen = (int)(ch-fieldStart);
  if (stripWhite && !quoted) {
    while(fieldLen>0 && (fieldStart[fieldLen-1]==' ' || fieldStart[fieldLen-1]=='\t')) fieldLen--;
    // this white space (' ' or '\t') can't be sep otherwise it would have stopped the field earlier at the first sep
  }
  if (quoted) { ch++; if (stripWhite) skip_white(&ch); }
  if (!on_sep(&ch)) return FALSE;
  if (targetCol) {
    #pragma omp critical
    // shared critical (not named) important since the buff to ans copy has a critical on SET_* too and we don't
    // want one thread over there calling SET_* when we are here calling SET_*. Shared critical means only one thread can be
    // in any of the criticals at once, which is exactly what we want.
    // This is an 'orphan' directive as we're in a function called from the parallel region.
    {
      SEXP thisStr;
      if (fieldLen==0) {
        thisStr = blank_is_a_nastring ? NA_STRING : R_BlankString;
      } else {
        thisStr = mkCharLenCE(fieldStart, fieldLen, ienc);
        // Check nastrings now while page is hot. Just one "NA" current default but empty in future when numerics
        // handle NA string variants directly. Faster to go ahead with mkChar and then do == on pointer, than always
        // do a strcmp first.
        for (int k=0; k<length(nastrings); k++) {
          if (thisStr == STRING_ELT(nastrings,k)) { thisStr = NA_STRING; break; }
        }
      }
      SET_STRING_ELT(targetCol, targetRow, thisStr);
    }
  }
  *this = ch; // Update caller's ch. This may be after fieldStart+fieldLen due to quotes and/or whitespace
  return TRUE;
}

static _Bool SkipField(const char **this, SEXP targetCol, int targetRow)
{
   // wrapper around Field for SXP_NULL to save a branch in the main data reader loop and
   // to make the *fun[] lookup a bit clearer
   return Field(this,NULL,0);
}

static inline int countfields(const char **this)
{
  const char *ch = *this;
  if (sep==' ') while (ch<eof && *ch==' ') ch++;  // Correct to be sep==' ' only and not skip_white(). 
  int ncol = 1;
  while (ch<eof && *ch!=eol) {
    if (!Field(&ch,NULL,0)) return -1;   // -1 means this line not valid for this sep and quote rule
    // Field() leaves *ch resting on sep, eol or >=eof.  (Checked inside Field())
    if (ch<eof && *ch!=eol) { ncol++; ch++; } // move over sep (which will already be last ' ' if sep=' '). TODO. Can be removed this line?
                //   ^^  Not *ch==sep because sep==eol when readLines
  }
  ch += eolLen; // may step past eof but that's ok as we never use ==eof in this file, always >=eof or <eof.
  *this = ch;
  return ncol;
}

static inline _Bool nextGoodLine(const char **this, int ncol)
{
  const char *ch = *this;
  // we may have landed inside quoted field containing embedded sep and/or embedded \n
  // find next \n and see if 5 good lines follow. If not try next \n, and so on, until we find the real \n
  // We don't know which line number this is, either, because we jumped straight to it. So return TRUE/FALSE for
  // the line number and error message to be worked out up there.
  int attempts=0;
  while (ch<eof && attempts++<30) {
    while (ch<eof && *ch!=eol) ch++;
    if (ch<eof) ch+=eolLen;
    int i = 0, thisNcol=0;
    const char *ch2 = ch;
    while (ch2<eof && i<5 && ( (thisNcol=countfields(&ch2))==ncol || (thisNcol==0 && (skipEmptyLines || fill)))) i++;
    if (i==5 || ch2>=eof) break;
  }
  if (ch<eof && attempts<30) { *this = ch; return TRUE; }
  return FALSE;
}

static _Bool StrtoI64(const char **this, SEXP targetCol, int targetRow)
{
    // Specialized clib strtoll that :
    // i) skips leading isspace() too but other than field separator and eol (e.g. '\t' and ' \t' in FUT1206.txt)
    // ii) has fewer branches for speed as no need for non decimal base
    // iii) updates global ch directly saving arguments
    // iv) safe for mmap which can't be \0 terminated on Windows (but can be on unix and mac)
    // v) fails if whole field isn't consumed such as "3.14" (strtol consumes the 3 and stops)
    // ... all without needing to read into a buffer at all (reads the mmap directly)
    const char *ch = *this;
    skip_white(&ch);  //  ',,' or ',   ,' or '\t\t' or '\t   \t' etc => NA
    if (on_sep(&ch)) {  // most often ',,' 
      if (targetCol) REAL(targetCol)[targetRow]=NA_INT64_D;
      *this = ch;
      return TRUE;
    }
    const char *start=ch;
    int sign=1;
    if (ch<eof && (*ch=='-' || *ch=='+')) sign -= 2*(*ch++=='-');
    _Bool ok = ch<eof && '0'<=*ch && *ch<='9';  // a single - or + with no [0-9] is !ok and considered type character
    long long acc = 0;
    while (ch<eof && '0'<=*ch && *ch<='9' && acc<(LLONG_MAX-10)/10) { // compiler should optimize last constant expression
      // Conveniently, LLONG_MIN  = -9223372036854775808 and LLONG_MAX  = +9223372036854775807
      // so the full valid range is [-LLONG_MAX,+LLONG_MAX] and NA==LLONG_MIN==-LLONG_MAX-1
      acc *= 10;
      acc += *ch-'0';
      ch++;
    }
    if (targetCol) REAL(targetCol)[targetRow] = LLtoD(sign * acc);
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    //Rprintf("StrtoI64 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    *this = ch;
    if (ok && !any_number_like_nastrings) return TRUE;  // most common case, return 
    _Bool na = is_nastring(start);
    if (ok && !na) return TRUE;
    if (targetCol) REAL(targetCol)[targetRow] = NA_INT64_D;
    next_sep(&ch);  // consume the remainder of field, if any
    *this = ch;
    return na;
}    

static _Bool StrtoI32(const char **this, SEXP targetCol, int targetRow)
{
    // Very similar to StrtoI64 (see it for comments). We can't make a single function and switch on TYPEOF(targetCol) to
    // know I64 or I32 because targetCol is NULL when testing types and when dropping columns.
    const char *ch = *this;
    skip_white(&ch);
    if (on_sep(&ch)) {  // most often ',,' 
      if (targetCol) INTEGER(targetCol)[targetRow]=NA_INTEGER;
      *this = ch;
      return TRUE;
    }
    const char *start=ch;
    int sign=1;
    if (ch<eof && (*ch=='-' || *ch=='+')) sign -= 2*(*ch++=='-');
    _Bool ok = ch<eof && '0'<=*ch && *ch<='9';
    int acc = 0;
    while (ch<eof && '0'<=*ch && *ch<='9' && acc<(INT_MAX-10)/10) {  // NA_INTEGER==INT_MIN==-2147483648==-INT_MAX(+2147483647)-1
      acc *= 10;
      acc += *ch-'0';
      ch++;
    }
    if (targetCol) INTEGER(targetCol)[targetRow] = sign * acc;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    //Rprintf("StrtoI32 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    *this = ch;
    if (ok && !any_number_like_nastrings) return TRUE;
    _Bool na = is_nastring(start);
    if (ok && !na) return TRUE;
    if (targetCol) INTEGER(targetCol)[targetRow] = NA_INTEGER;
    next_sep(&ch);
    *this = ch;
    return na;
}

static _Bool StrtoD(const char **this, SEXP targetCol, int targetRow)
{
    // Specialized strtod for same reasons as Strtoll (leading \t dealt with), but still uses stdlib:strtod
    // R's R_Strtod5 uses strlen() on input, so we can't use that here unless we copy field to a buffer (slow)
    // Could fork glibc's strtod and change it to pass in dec rather than via locale but see :
    //    http://www.exploringbinary.com/how-glibc-strtod-works/
    // i.e. feel more comfortable relying on glibc for speed and robustness (and more eyes have been on it)
    const char *ch = *this;
    skip_white(&ch);
    if (on_sep(&ch)) {
      if (targetCol) REAL(targetCol)[targetRow]=NA_REAL;
      *this = ch;
      return TRUE;
    }
    const char *start=ch;
    errno = 0;
    double d = strtod(start, (char **)&ch);
    /*if (errno==ERANGE && ch>start) {
      ch = start;
      errno = 0;
      d = (double)strtold(start, (char **)&ch);
      // errno is checked further below where ok= is set
      if (ERANGEwarning) {  // FALSE initially when detecting types then its set TRUE just before reading data.
        #pragma omp critical
        warning("C function strtod() returned ERANGE for one or more fields. The first was string input '%.*s'. It was read using (double)strtold() as numeric value %.16E (displayed here using %%.16E); loss of accuracy likely occurred. This message is designed to tell you exactly what has been done by fread's C code, so you can search yourself online for many references about double precision accuracy and these specific C functions. You may wish to use colClasses to read the column as character instead and then coerce that column using the Rmpfr package for greater accuracy.", ch-start, start, d);
        ERANGEwarning = FALSE;   // once only warning
        // This is carefully worded as an ERANGE warning because that's precisely what it is.  Calling it a 'precision' warning
        // might lead the user to think they'll get a precision warning on "1.23456789123456789123456789123456789" too, but they won't
        // as that will be read fine by the first strtod() with silent loss of precision. IIUC.
      }
    }*/
    skip_white(&ch);
    Rboolean ok = (errno==0 || errno==ERANGE) && ch>start && on_sep(&ch);
    if (targetCol) REAL(targetCol)[targetRow]=d;
    *this = ch;
    if (ok && !any_number_like_nastrings) return TRUE;
    Rboolean na = is_nastring(start);
    if (ok && !na) return TRUE;
    if (targetCol) REAL(targetCol)[targetRow] = NA_REAL;
    next_sep(&ch);
    *this = ch;
    return na;
}

static _Bool StrtoB(const char **this, SEXP targetCol, int targetRow)
{
    // These usually come from R when it writes out.
    const char *ch = *this;
    skip_white(&ch);
    if (targetCol) LOGICAL(targetCol)[targetRow]=NA_LOGICAL;
    if (on_sep(&ch)) { *this = ch; return TRUE; }  // empty field most commonly ',,' 
    const char *start=ch;
    if (*ch=='T') {
        if (targetCol) LOGICAL(targetCol)[targetRow] = TRUE;
        ch++;
        if (on_sep(&ch)) { *this=ch; return TRUE; }
        if (ch+2<eof && *ch=='R' && *++ch=='U' && *++ch=='E' && ++ch && on_sep(&ch)) { *this=ch; return TRUE; }
        ch = start+1;
        if (ch+2<eof && *ch=='r' && *++ch=='u' && *++ch=='e' && ++ch && on_sep(&ch)) { *this=ch; return TRUE; }
    }
    else if (*ch=='F') {
        if (targetCol) LOGICAL(targetCol)[targetRow] = FALSE;
        ch++;
        if (on_sep(&ch)) { *this=ch; return TRUE; }
        if (ch+3<eof && *ch=='A' && *++ch=='L' && *++ch=='S' && *++ch=='E' && ++ch && on_sep(&ch)) { *this=ch; return TRUE; }
        ch = start+1;
        if (ch+3<eof && *ch=='a' && *++ch=='l' && *++ch=='s' && *++ch=='e' && ++ch && on_sep(&ch)) { *this=ch; return TRUE; }
    }
    if (targetCol) LOGICAL(targetCol)[targetRow] = NA_LOGICAL;
    next_sep(&ch);
    *this=ch;
    return is_nastring(start);
}


SEXP readfile(SEXP input, SEXP separg, SEXP nrowsarg, SEXP headerarg, SEXP nastringsarg, SEXP verbosearg, SEXP skip, SEXP select, SEXP drop, SEXP colClasses, SEXP integer64, SEXP dec, SEXP encoding, SEXP quoteArg, SEXP stripWhiteArg, SEXP skipEmptyLinesArg, SEXP fillArg, SEXP showProgressArg)
// can't be named fread here because that's already a C function (from which the R level fread function took its name)
{
    SEXP ans;
    R_len_t j, k, protecti=0;
    const char *pos;
    Rboolean header, allchar;
    verbose=LOGICAL(verbosearg)[0];
    double t0 = wallclock();
    PROTECT_INDEX pi;
    
    // Encoding, #563: Borrowed from do_setencoding from base R
    // https://github.com/wch/r-source/blob/ca5348f0b5e3f3c2b24851d7aff02de5217465eb/src/main/util.c#L1115
    // Check for mkCharLenCE function to locate as to where where this is implemented.
    // cetype_t ienc;
    if (!strcmp(CHAR(STRING_ELT(encoding, 0)), "Latin-1")) ienc = CE_LATIN1;
    else if (!strcmp(CHAR(STRING_ELT(encoding, 0)), "UTF-8")) ienc = CE_UTF8;
    else ienc = CE_NATIVE;

    stripWhite = LOGICAL(stripWhiteArg)[0];
    skipEmptyLines = LOGICAL(skipEmptyLinesArg)[0];
    fill = LOGICAL(fillArg)[0];

    // quoteArg for those rare cases when default scenario doesn't cut it.., FR #568
    if (!isString(quoteArg) || LENGTH(quoteArg)!=1 || strlen(CHAR(STRING_ELT(quoteArg,0))) > 1)
        error("quote must either be empty or a single character");
    quote = CHAR(STRING_ELT(quoteArg,0))[0];

    if (!isLogical(showProgressArg) || LENGTH(showProgressArg)!=1 || LOGICAL(showProgressArg)[0]==NA_LOGICAL)
        error("Internal error: showProgress is not TRUE or FALSE. Please report.");
    const Rboolean showProgress = LOGICAL(showProgressArg)[0];
    
    if (!isString(dec) || LENGTH(dec)!=1 || strlen(CHAR(STRING_ELT(dec,0))) != 1)
        error("dec must be a single character");
    const char decChar = *CHAR(STRING_ELT(dec,0));
    
    fnam = NULL;  // reset global, so STOP() can call closeFile() which sees fnam

    reader_fun_t fun[NUMTYPE] = {&StrtoB, &StrtoI32, &StrtoI64, &StrtoD, &Field, &SkipField};
    
    // raise(SIGINT);
    // ********************************************************************************************
    //   Check inputs.
    // ********************************************************************************************
    
    if (!isLogical(headerarg) || LENGTH(headerarg)!=1) error("'header' must be 'auto', TRUE or FALSE");
    // 'auto' was converted to NA at R level
    header = LOGICAL(headerarg)[0];
    if (!isNull(nastringsarg) && !isString(nastringsarg)) error("'na.strings' is type '%s'.  Must be a character vector.", type2char(TYPEOF(nastrings)));
    nastrings = nastringsarg;  // static global so we can use it in field processors
    any_number_like_nastrings = FALSE;
    blank_is_a_nastring = FALSE;
    // if we know there are no nastrings which are numbers (like -999999) then in the number
    // field processors we can save an expensive step in checking the nastrings. Since if the field parses as a number,
    // we then know it can't be NA provided any_number_like_nastrings==FALSE.
    for (int i=0; i<length(nastrings); i++) {
      int nchar = LENGTH(STRING_ELT(nastrings,i));
      if (nchar==0) { blank_is_a_nastring=TRUE; continue; }
      const char *start=CHAR(STRING_ELT(nastrings,i));
      if (isspace(start[0]) || isspace(start[nchar-1])) error("na.strings[%d]=='%s' has whitespace at the beginning or end", i+1, start);
      if (strcmp(start,"T")==0    || strcmp(start,"F")==0 ||
          strcmp(start,"TRUE")==0 || strcmp(start,"FALSE")==0 ||
          strcmp(start,"True")==0 || strcmp(start,"False")==0) error("na.strings[%d]=='%s' is recognized as type boolean. This string is not permitted in 'na.strings'.", i+1, start);
      char *end;
      errno = 0;
      strtod(start, &end);  // careful not to let "" (R_BlankString) to get to here as strtod considers that numeric
      if (errno==0 && (int)(end-start)==nchar) {
        any_number_like_nastrings = TRUE;
        if (verbose) Rprintf("na.strings[%d]=='%s' is numeric so all numeric fields will have to check na.strings\n", i+1, start); 
      }
    }
    if (verbose && !any_number_like_nastrings) Rprintf("None of the %d 'na.strings' are numeric (such as '-9999') which is normal and best for performance.\n", LENGTH(nastrings));
    int nrowLimit = INT_MAX;
    if (isReal(nrowsarg)) {
      if (R_FINITE(REAL(nrowsarg)[0])) nrowLimit = (int)(REAL(nrowsarg)[0]);
    } else {
      nrowLimit = INTEGER(nrowsarg)[0];
    }
    if (nrowLimit<0) error("nrows must be >=0", nrowLimit);
    
    if (!( (isInteger(skip) && LENGTH(skip)==1 && INTEGER(skip)[0]>=0)  // NA_INTEGER is covered by >=0
         ||(isString(skip) && LENGTH(skip)==1))) error("'skip' must be a length 1 vector of type numeric or integer >=0, or single character search string");
    if (!isNull(separg)) {
        if (!isString(separg) || LENGTH(separg)!=1 || strlen(CHAR(STRING_ELT(separg,0)))!=1) error("'sep' must be 'auto' or a single character");
        if (*CHAR(STRING_ELT(separg,0))==quote) error("sep = '%c' = quote, is not an allowed separator.",quote);
        if (*CHAR(STRING_ELT(separg,0)) == decChar) error("The two arguments to fread 'dec' and 'sep' are equal ('%c').", decChar);
    }
    if (!isString(integer64) || LENGTH(integer64)!=1) error("'integer64' must be a single character string");
    if (strcmp(CHAR(STRING_ELT(integer64,0)), "integer64")!=0 &&
        strcmp(CHAR(STRING_ELT(integer64,0)), "double")!=0 &&
        strcmp(CHAR(STRING_ELT(integer64,0)), "numeric")!=0 &&
        strcmp(CHAR(STRING_ELT(integer64,0)), "character")!=0)
        error("integer64='%s' which isn't 'integer64'|'double'|'numeric'|'character'", CHAR(STRING_ELT(integer64,0)));
    if (!isNull(select) && !isNull(drop)) error("Supply either 'select' or 'drop' but not both");
    
    // ********************************************************************************************
    //   Point to text input, or open and mmap file
    // ********************************************************************************************
    const char *ch, *ch2;
    ch = ch2 = (const char *)CHAR(STRING_ELT(input,0));
    while (*ch2!='\n' && *ch2) ch2++;
    if (*ch2=='\n' || !*ch) {
        if (verbose) Rprintf("Input contains a \\n (or is \"\"). Taking this to be text input (not a filename)\n");
        filesize = strlen(ch);
        mmp = ch;
        eof = mmp+filesize;
        if (*eof!='\0') error("Internal error: last byte of character input isn't \\0");
    } else {
        if (verbose) Rprintf("Input contains no \\n. Taking this to be a filename to open\n");
        fnam = R_ExpandFileName(ch);  // for convenience so user doesn't have to call path.expand() themselves
#ifndef WIN32
        fd = open(fnam, O_RDONLY);
        if (fd==-1) error("file not found: %s",fnam);
        struct stat stat_buf;
        if (fstat(fd,&stat_buf) == -1) {close(fd); error("Opened file ok but couldn't obtain file size: %s", fnam);}
        filesize = stat_buf.st_size;
        if (filesize<=0) {close(fd); error("File is empty: %s", fnam);}
        if (verbose) Rprintf("File opened, filesize is %.6f GB.\nMemory mapping ... ", 1.0*filesize/(1024*1024*1024));
        
        // No MAP_POPULATE for faster nrows=10 and to make possible earlier progress bar in row count stage
        // Mac doesn't appear to support MAP_POPULATE anyway (failed on CRAN when I tried).
        // TO DO?: MAP_HUGETLB for Linux but seems to need admin to setup first. My Hugepagesize is 2MB (>>2KB, so promising)
        //         https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
        mmp = origmmp = (const char *)mmap(NULL, filesize, PROT_READ, MAP_PRIVATE, fd, 0);
        if (mmp == MAP_FAILED) {
            close(fd);
#else
        // Following: http://msdn.microsoft.com/en-gb/library/windows/desktop/aa366548(v=vs.85).aspx
        hFile = INVALID_HANDLE_VALUE;
        int attempts = 0;
        while(hFile==INVALID_HANDLE_VALUE && attempts<5) {
            hFile = CreateFile(fnam, GENERIC_READ, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
            // FILE_SHARE_WRITE is required otherwise if the file is open in Excel, CreateFile fails. Should be ok now.
            if (hFile==INVALID_HANDLE_VALUE) {
                if (GetLastError()==ERROR_FILE_NOT_FOUND) error("File not found: %s",fnam);
                if (attempts<4) Sleep(250);  // 250ms
            }
            attempts++;
            // Looped retry to avoid ephemeral locks by system utilities as recommended here : http://support.microsoft.com/kb/316609
        }
        if (hFile==INVALID_HANDLE_VALUE) error("Unable to open file after %d attempts (error %d): %s", attempts, GetLastError(), fnam);
        LARGE_INTEGER liFileSize;
        if (GetFileSizeEx(hFile,&liFileSize)==0) { CloseHandle(hFile); error("GetFileSizeEx failed (returned 0) on file: %s", fnam); }
        filesize = (size_t)liFileSize.QuadPart;
        if (filesize<=0) { CloseHandle(hFile); error("File is empty: %s", fnam); }
        hMap=CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL); // filesize+1 not allowed here, unlike mmap where +1 is zero'd
        if (hMap==NULL) { CloseHandle(hFile); error("This is Windows, CreateFileMapping returned error %d for file %s", GetLastError(), fnam); }
        if (verbose) Rprintf("File opened, filesize is %.6f GB.\nMemory mapping ... ", 1.0*filesize/(1024*1024*1024));
        mmp = origmmp = (const char *)MapViewOfFile(hMap,FILE_MAP_READ,0,0,filesize);
        if (mmp == NULL) {
            CloseHandle(hMap);
            CloseHandle(hFile);
#endif
            if (sizeof(char *)==4)
                error("Opened file ok, obtained its size on disk (%.1fMB) but couldn't memory map it. This is a 32bit machine. You don't need more RAM per se but this fread function is tuned for 64bit addressability at the expense of large file support on 32bit machines. You probably need more RAM to store the resulting data.table, anyway. And most speed benefits of data.table are on 64bit with large RAM, too. Please upgrade to 64bit.", filesize/(1024.0*1024));
                // if we support this on 32bit, we may need to use stat64 instead, as R does
            else if (sizeof(char *)==8)
                error("Opened file ok, obtained its size on disk (%.1fMB), but couldn't memory map it. This is a 64bit machine so this is surprising. Please report to datatable-help.", filesize/1024^2);
            else
                error("Opened file ok, obtained its size on disk (%.1fMB), but couldn't memory map it. Size of pointer is %d on this machine. Probably failing because this is neither a 32bit or 64bit machine. Please report to datatable-help.", filesize/1024^2, sizeof(char *));
        }
        if (EOF > -1) error("Internal error. EOF is not -1 or less\n");
        if (mmp[filesize-1] < 0) error("mmap'd region has EOF at the end");
        eof = mmp+filesize;  // byte after last byte of file.  Never dereference eof as it's not mapped.
        if (verbose) Rprintf("ok\n");  // to end 'Memory mapping ... '
    }
    double tMap = wallclock();
    // From now use STOP() wrapper instead of error(), for Windows to close file so as not to lock the file after an error.
    
    // ********************************************************************************************
    //   Auto detect eol, first eol where there are two (i.e. CRLF)
    // ********************************************************************************************
    // take care of UTF8 BOM, #1087 and #1465
    if (!memcmp(mmp, "\xef\xbb\xbf", 3)) mmp += 3;   // this is why we need 'origmmp'
    ch = mmp;
    while (ch<eof && *ch!='\n' && *ch!='\r') {
        if (*ch==quote) while(++ch<eof && *ch!=quote) {};  // allows protection of \n and \r inside column names
        ch++;                                              // this 'if' needed in case opening protection is not closed before eof
    }
    if (ch>=eof) {
        if (ch>eof) STOP("Internal error: ch>eof when detecting eol");
        if (verbose) Rprintf("Input ends before any \\r or \\n observed. Input will be treated as a single row.\n");
        eol=eol2='\n'; eolLen=1;
    } else {
        eol=eol2=*ch; eolLen=1;
        if (eol=='\r') {
            if (ch+1<eof && *(ch+1)=='\n') {
                if (verbose) Rprintf("Detected eol as \\r\\n (CRLF) in that order, the Windows standard.\n");
                eol2='\n'; eolLen=2;
            } else {
                if (ch+1<eof && *(ch+1)=='\r')
                    STOP("Line ending is \\r\\r\\n. R's download.file() appears to add the extra \\r in text mode on Windows. Please download again in binary mode (mode='wb') which might be faster too. Alternatively, pass the URL directly to fread and it will download the file in binary mode for you.");
                    // NB: on Windows, download.file from file: seems to condense \r\r too. So 
                if (verbose) Rprintf("Detected eol as \\r only (no \\n or \\r afterwards). An old Mac 9 standard, discontinued in 2002 according to Wikipedia.\n");
            }
        } else if (eol=='\n') {
            if (ch+1<eof && *(ch+1)=='\r') {
                warning("Detected eol as \\n\\r, a highly unusual line ending. According to Wikipedia the Acorn BBC used this. If it is intended that the first column on the next row is a character column where the first character of the field value is \\r (why?) then the first column should start with a quote (i.e. 'protected'). Proceeding with attempt to read the file.\n");
                eol2='\r'; eolLen=2;
            } else if (verbose) Rprintf("Detected eol as \\n only (no \\r afterwards), the UNIX and Mac standard.\n");
        } else
            STOP("Internal error: if no \\r or \\n found then ch should be eof");
    }

    // ********************************************************************************************
    //   Position to line skip+1 or line containing skip="string"
    // ********************************************************************************************
    int line = 1;
    // line is for error and warning messages so considers raw \n whether inside quoted fields or not, just
    // like wc -l, head -n and tail -n
    ch = pos = mmp;
    if (isString(skip)) {
        ch = strstr(mmp, CHAR(STRING_ELT(skip,0)));
        if (!ch) STOP("skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)", CHAR(STRING_ELT(skip,0)));
        while (ch>mmp && *(ch-1)!=eol2) ch--;  // move to beginning of line
        pos = ch;
        ch = mmp;
        while (ch<pos) line+=(*ch++==eol);
        if (verbose) Rprintf("Found skip='%s' on line %d. Taking this to be header row or first row of data.\n", CHAR(STRING_ELT(skip,0)), line);
        ch = pos;
    } else if (INTEGER(skip)[0]>0) {
        while (ch<eof && line<=INTEGER(skip)[0]) line+=(*ch++==eol);
        if (ch>=eof) STOP("skip=%d but the input only has %d line%s", INTEGER(skip)[0], line, line>1?"s":"");
        ch += (eolLen-1); // move over eol2 on Windows to be on start of desired line
        pos = ch;
    }
    
    // skip blank input at the start
    const char *lineStart = ch;
    while (ch<eof && isspace(*ch)) {   // isspace matches ' ', \t, \n and \r
      if (*ch==eol) { ch+=eolLen; lineStart=ch; line++; } else ch++;
    }
    if (ch>=eof) STOP("Input is either empty, fully whitespace, or skip has been set after the last non-whitespace.");
    if (verbose) {
      if (lineStart>ch) Rprintf("Moved forward to first non-blank line (%d)\n", line);
      Rprintf("Positioned on line %d starting: <<%.*s>>\n", line, STRLIM(lineStart, 30), lineStart);
    }
    ch = pos = lineStart;
    
    // *********************************************************************************************************
    //   Auto detect separator, quoting rule, first line and ncol, simply, using jump 0 only
    // *********************************************************************************************************
    const char *seps;
    int nseps=0;
    if (isNull(separg)) {
      seps=",|;\t ";  // separators, in order of preference. See ?fread.
      if (verbose) Rprintf("Detecting sep ...\n");
      nseps = strlen(seps);
    } else {
      seps = CHAR(STRING_ELT(separg,0));  // length 1 string of 1 character was checked above
      nseps = 1;
      if (verbose) Rprintf("Using supplied sep '%s'\n", seps[0]=='\t' ? "\\t" : seps);
    }
    
    int topNumLines=0;        // the most number of lines with the same number of fields, so far
    int topNumFields=1;       // how many fields that was, to resolve ties
    int topNmax=0;            // for that sep and quote rule, what was the max number of columns (just for fill=TRUE)
    char topSep=eol;          // which sep that was, by default \n to mean single-column input (1 field)
    int topQuoteRule=0;       // which quote rule that was
    const char *firstJumpEnd=eof; // remember where the winning jumpline from jump 0 ends, to know its size excluding header
    
    int nJumpLines = JUMPLINES;
    // Always sample as if nrows= wasn't supplied. That's probably *why* they are setting nrow=0 to get the column names
    // and types, without actually reading the data yet. Most likely to check consistency across a set of files.
    int numFields[nJumpLines+1];   // +1 to cover the likely header row. Don't know at this stage whether it is present or not.
    int numLines[nJumpLines+1];
    
    for (int s=0; s<nseps; s++) {
      sep = seps[s];
      for (quoteRule=0; quoteRule<4; quoteRule++) {  // quote rule in order of preference
        ch = pos;
        // Rprintf("Trying sep='%c' with quoteRule %d ...", sep, quoteRule);
        for (int i=0; i<=nJumpLines; i++) { numFields[i]=0; numLines[i]=0; } // clear VLAs
        int i=-1; // The slot we're counting the currently contiguous consistent ncol
        int thisLine=0, lastncol=0;
        while (ch<eof && ++thisLine<=nJumpLines+1) {
          int thisncol = countfields(&ch);   // using this sep and quote rule; moves ch to start of next line
          if (thisncol<0) { numFields[0]=-1; break; }  // invalid file with this sep and quote rule; abort this rule
          if (lastncol!=thisncol) { numFields[++i]=thisncol; lastncol=thisncol; } // new contiguous consistent ncol started
          numLines[i]++;
        }
        if (numFields[0]==-1) continue;
        _Bool updated=FALSE;
        int nmax=0;
        
        i=-1; while (numLines[++i]) {
          if (numFields[i] > nmax) nmax=numFields[i];  // for fill=TRUE to know max number of columns
          if (numFields[i]>1 &&    // the default sep='\n' (whole lines, single column) shuld take precedence 
              ( numLines[i]>topNumLines ||
               (numLines[i]==topNumLines && numFields[i]>topNumFields && sep!=' '))) {
            topNumLines=numLines[i]; topNumFields=numFields[i]; topSep=sep; topQuoteRule=quoteRule; topNmax=nmax;
            firstJumpEnd = ch;  // So that after the header we know how many bytes jump point 0 is
            updated = TRUE;
            // Two updates can happen for the same sep and quoteRule (e.g. issue_1113_fread.txt where sep=' ') so the
            // updated flag is just to print once.
          }
        }
        if (verbose && updated) {
          Rprintf("  sep=="); Rprintf(sep=='\t' ? "'\\t'" : "'%c'(ascii %d)", sep, sep);
          Rprintf("  with %d lines of %d fields using quote rule %d\n", topNumLines, topNumFields, topQuoteRule);
        }
      }
    }
    
    int ncol;
    quoteRule = topQuoteRule;
    sep = topSep;
    ch = pos;
    if (fill) {
      // start input from first populated line, already pos.
      ncol = topNmax;
    } else {
      // find the top line with the consistent number of fields.  There might be irregular header lines above it.
      //int nmax=0, thisLine=-1, whichmax=0;
      ncol = topNumFields;
      int thisLine=-1; while (ch<eof && ++thisLine<nJumpLines) {
        const char *lineStart = ch;
        if (countfields(&ch)==ncol) { ch=pos=lineStart; line+=thisLine; break; }
      }
    }
    // For standard regular separated files, we're now on the first byte of the file.
    
    if (ncol<1 || line<1) STOP("Internal error: ncol==%d line==%d after detecting sep, ncol and first line");
    int tt = countfields(&ch);
    ch = pos; // move back to start of line since countfields() moved to next
    if (!fill && tt!=ncol) STOP("Internal error: first line has field count %d but expecting %d", tt, ncol);
    if (verbose) {
      Rprintf("Detected %d columns on line %d. This line is either column names or first data row (first 30 chars): <<%.*s>>\n",
               tt, line, STRLIM(pos, 30), pos);
      if (fill) Rprintf("fill=TRUE and the most number of columns found is %d\n", ncol);
    }
        
    // ********************************************************************************************
    //   Detect and assign column names (if present)
    // ********************************************************************************************
    SEXP names = PROTECT(allocVector(STRSXP, ncol));
    protecti++;
    allchar=TRUE;
    {
      if (sep==' ') while (ch<eof && *ch==' ') ch++;
      int field=0; while(ch<eof && *ch!=eol && field<ncol) {
        const char *this = ch;
        //Rprintf("Field %d <<%.*s>>\n", i, STRLIM(ch,20), ch);
        skip_white(&ch);
        if (allchar && !on_sep(&ch) && StrtoD(&ch,NULL,0)) allchar=FALSE;  // considered testing at least one isalpha, but we want 1E9 to be considered a value for this purpose not a column name
        ch = this;  // rewind to the start of this field
        Field(&ch,NULL,0);  // StrtoD does not consume quoted fields according to the quote rule, so redo with Field()
        if (ch<eof && *ch!=eol) { ch++; field++; }
      }
      //if (fill && field<ncol-1) { allchar=FALSE; }
      if ((!fill && field<ncol-1) || (ch<eof && *ch!=eol)) STOP("Not positioned correctly after testing format of header row. Read %d fields out of expected %d and finished on <<%.*s>>'",field+1,ncol,STRLIM(ch,30),ch);
    }
    if (verbose && header!=NA_LOGICAL) Rprintf("'header' changed by user from 'auto' to %s\n", header?"TRUE":"FALSE");
    if (header==FALSE || (header==NA_LOGICAL && !allchar)) {
        if (verbose && header==NA_LOGICAL) Rprintf("Some fields on line %d are not type character. Treating as a data row and using default column names.\n", line);
        for (int i=0; i<ncol; i++) {
            char buff[10];
            sprintf(buff,"V%d",i+1);
            SET_STRING_ELT(names, i, mkChar(buff));
        }
        ch = pos;  // back to start of first row. Treat as first data row, no column names present.
        // now check previous line which is being discarded and give helpful msg to user ...
        if (ch>mmp && INTEGER(skip)[0]==0) {
          ch -= (eolLen+1);
          if (ch<mmp) ch=mmp;  // for when mmp[0]=='\n'
          while (ch>mmp && *ch!=eol2) ch--;
          if (ch>mmp) ch++;
          const char *prevStart = ch;
          int tmp = countfields(&ch);
          if (tmp==ncol) STOP("Internal error: row before first data row has the same number of fields but we're not using it.");
          if (tmp>1) warning("Starting data input on line %d <<%.*s>> with %d fields and discarding line %d <<%.*s>> before it because it has a different number of fields (%d).", line, STRLIM(pos, 30), pos, ncol, line-1, STRLIM(prevStart, 30), prevStart, tmp);
        }
        if (ch!=pos) STOP("Internal error. ch!=pos after prevBlank check");     
    } else {
        if (verbose && header==NA_LOGICAL) Rprintf("All the fields on line %d are character fields. Treating as the column names.\n", line);
        ch = pos;
        line++;
        if (sep==' ') while (ch<eof && *ch==' ') ch++;
        for (int i=0; i<ncol; i++) {
            // Skip trailing spaces and call 'Field()' here as it's already designed to handle quote vs non-quote cases.
            // Also Fiedl() it takes care of leading spaces. Easier to understand the logic.
            Field(&ch,names,i);
            if (STRING_ELT(names,i)==NA_STRING || STRING_ELT(names,i)==R_BlankString) {
                char buff[10];
                sprintf(buff,"V%d",i+1);
                SET_STRING_ELT(names, i, mkChar(buff));
            }
            if (ch<eof && *ch!=eol && i<ncol-1) ch++; // move the beginning char of next field
        }
        while (ch<eof && *ch!=eol) ch++; // no need for skip_white() here
        if (ch<eof && *ch==eol) ch+=eolLen;  // now on first data row (row after column names)
        pos = ch;
    }
    int row1Line = line;
    double tLayout = wallclock();
    
    // *****************************************************************************************************************
    //   Make best guess at column types using 100 rows at 10 points, including the very first, middle and very last row.
    //   At the same time, calc mean and sd of row lengths in sample. Use for very good estimate of nrow.
    // *****************************************************************************************************************
    unsigned char type[ncol];
    _Bool typeOk[ncol];
    for (int i=0; i<ncol; i++) { 
      type[i]   =0;   // 0 because it's the lowest type ('logical')
      typeOk[i] =TRUE;   // set to 0 later to output one message per column if the guessed type is not sufficient
    }
    
    size_t startSize=(size_t)(firstJumpEnd-pos);  // the size in bytes of the first JUMPLINES from the start (jump point 0)
    int nJumps = NJUMPS+1;
    // +1 for the extra sample at the very end (this is sampled and format checked but not jumped to when reading)
    if (startSize==0 || startSize*NJUMPS*2 > (eof-pos)) { nJumps=1; }
    // *2 to get a good spacing. We don't want overlaps resulting in double counting.
    // nJumps==1 means the whole (small) file will be sampled with one thread
    if (verbose) {
      Rprintf("Number of sampling jump points  = %d because ",nJumps);
      if (startSize==0) Rprintf("startSize=0\n");
      else Rprintf("%lld startSize * %d NJUMPS * 2 = %d %s %d bytes from line %d to eof\n",
                   startSize, NJUMPS, startSize*NJUMPS*2, nJumps==1 ? ">" : "<=", (size_t)(eof-pos), row1Line);
    }

    int sampleLines=0;
    size_t sampleBytes=0;
    double sumLen=0.0, sumLenSq=0.0;
    int minLen=INT_MAX, maxLen=-1;   // int_max so the first if(thisLen<minLen) is always true; similarly for max
    const char *lastRowEnd=pos;
    for (int j=0; j<nJumps; j++) {
        ch = ( j==0 ? pos : (j==nJumps-1 ? eof-(size_t)(0.5*startSize) : pos + j*((size_t)(eof-pos)/NJUMPS)));
        if (j>0 && !nextGoodLine(&ch, ncol)) STOP("Could not find first good line start after jump point %d when sampling.", j);
        _Bool bumped = 0;   // whether this jump found any different types; used just in verbose mode to limit output to relevant
        const char *thisStart = ch;
        const char *thisEnd = (nJumps==1 ? eof : MIN(ch+startSize,eof));  // MIN is needed for the last extra jump for very end
        
        int i = 0;
        while(ch<thisEnd) {
            const char *lineStart = ch;
            if (sep==' ') while (ch<eof && *ch==' ') ch++;  // multiple sep=' ' at the lineStart does not mean sep(!)
            skip_white(&ch);  // solely to detect blank lines, otherwise could leave to field processors which handle leading white
            if (ch>=eof || *ch==eol) {
              if (!skipEmptyLines && !fill) break;
              lineStart = ch;  // to avoid 'Line finished early' below and get to the sampleLines++ block at the end of this while
            }
            i++;
            int field=0;
            const char *fieldStart=ch;  // Needed outside loop for error messages below
            while (ch<eof && *ch!=eol && field<ncol) {
                //Rprintf("Field %d: <<%.*s>>\n", field+1, STRLIM(ch,20), ch);
                fieldStart=ch;
                while (type[field]<=SXP_STR && !(*fun[type[field]])(&ch,NULL,0)) {
                  ch=fieldStart;
                  if (type[field]<SXP_STR) { type[field]++; bumped=TRUE; }
                  else {
                    // the field couldn't be read with this quote rule, try again with next one
                    // Trying the next rule will only be successful if the number of fields is consistent with it
                    if (quoteRule<3) {
                      if (verbose) Rprintf("Bumping quote rule from %d to %d due to field %d on line %d starting <<%.*s>>\n",
                                   quoteRule, quoteRule+1, field+1, line+i-1, STRLIM(fieldStart,200), fieldStart);
                      quoteRule++;
                      bumped=TRUE;
                      ch = lineStart;  // Try whole line again, in case it's a hangover from previous field
                      field=0;
                      continue;
                    }
                    STOP("Even quoteRule 3 was insufficient!");
                  }
                }
                if (ch<eof && *ch!=eol) {ch++; field++;}
            }
            if (field<ncol-1 && !fill) {
                if (ch<eof && *ch!=eol) STOP("Internal error: line has finished early but not on an eol or eof (fill=FALSE). Please report as bug.");
                else if (ch>lineStart) STOP("Line has too few fields when detecting types. Use fill=TRUE to pad with NA. Expecting %d fields but found %d: <<%.*s>>", ncol, field+1, STRLIM(lineStart,200), lineStart);
            }
            if (ch<eof) {
                if (*ch!=eol || field>=ncol) {   // the || >=ncol is for when a comma ends the line with eol straight after 
                  if (field!=ncol) STOP("Internal error: Line has too many fields but field(%d)!=ncol(%d)", field, ncol);
                  STOP("Line %d starting <<%.*s>> has more than the expected %d fields. Separator %d occurs at position %d which is character %d of the last field: <<%.*s>>. Consider setting 'comment.char=' if there is a trailing comment to be ignored.",
                      line+i-1, STRLIM(lineStart,10), lineStart, ncol, ncol, (int)(ch-lineStart), (int)(ch-fieldStart), STRLIM(fieldStart,200), fieldStart);
                }
                ch += eolLen;
            } else {
                // if very last field was quoted, check if it was completed with an ending quote ok.
                // not necessarily a problem (especially if we detected no quoting), but we test it and nice to have
                // a warning regardless of quoting rule just incase file has been inadvertently truncated
                // This warning is early at type skipping around stage before reading starts, so user can cancel early
                if (type[ncol-1]==SXP_STR && *fieldStart==quote && *(ch-1)!=quote) {
                  if (quoteRule<2) STOP("Internal error: Last field of last field should select quote rule 2"); 
                  warning("Last field of last line starts with a quote but is not finished with a quote before end of file: <<%.*s>>", 
                          STRLIM(fieldStart, 200), fieldStart);
                }
            }
            lastRowEnd = ch; // just to get the end position of the very last good row, before whitespace or footer before eof
            int thisLineLen = (int)(ch-lineStart);  // ch is now on start of next line so this includes eolLen already
            sampleLines++;
            sumLen += thisLineLen;
            sumLenSq += pow(thisLineLen,2);
            if (thisLineLen<minLen) minLen=thisLineLen;
            if (thisLineLen>maxLen) maxLen=thisLineLen;
        }
        sampleBytes += (size_t)(ch-thisStart);  // ch not thisEnd because last line could have gone past thisEnd (which is fine)
        if (verbose && (bumped || j==0 || j==nJumps-1)) {
          Rprintf("Type codes (jump %02d)    : ",j); printTypes(type, ncol);
          Rprintf("  Quote rule %d\n", quoteRule);
        }
    }
    while (ch<eof && isspace(*ch)) ch++;
    if (ch<eof) {
      warning("Found the last consistent line but text exists afterwards (discarded): <<%.*s>>", STRLIM(ch,200), ch);
    }
    
    int estnrow=0;
    int allocnrow=0, orig_allocnrow=0;
    double meanLineLen=0;
    if (sampleLines==0) {
      // column names only are present; e.g. fread("A\n")
    } else {
      meanLineLen = (double)sumLen/sampleLines;
      estnrow = (int)ceil((double)(lastRowEnd-pos)/meanLineLen);  // only used for progress meter and verbose line below
      double sd = sqrt( (sumLenSq - pow(sumLen,2)/sampleLines)/(sampleLines-1) );
      if (!isfinite(sd)) sd=1.0;
      allocnrow = (int)ceil((double)(lastRowEnd-pos)/MAX((meanLineLen-2*sd),minLen));
      orig_allocnrow = allocnrow = MIN(MAX(allocnrow, ceil(1.1*estnrow)), 2*estnrow);
      // orig_ for when nrows= is passed in. We need the original later to calc initial buffer sizes
      // sd can be very close to 0.0 sometimes, so apply a +10% minimum
      // blank lines have length 1 so for fill=TRUE apply a +100% maximum. It'll be grown if needed.
      if (verbose) {
        Rprintf("=====\n Sampled %d rows (handled \\n inside quoted fields) at %d jump points including middle and very end\n", sampleLines, nJumps);
        Rprintf(" Bytes from first data row on line %d to the end of last row: %lld\n", row1Line, (size_t)(lastRowEnd-pos));
        Rprintf(" Line length: mean=%.2f sd=%.2f min=%d max=%d\n", meanLineLen, sd, minLen, maxLen);
        Rprintf(" Estimated nrow: %lld / %.2f = %d\n", (size_t)(lastRowEnd-pos), meanLineLen, estnrow);
        Rprintf(" Initial alloc = %d rows (%d + %d%%) using bytes/max(mean-2*sd,min) clamped between [1.1*estn, 2.0*estn]\n",
                 allocnrow, estnrow, (int)(100.0*allocnrow/estnrow-100.0));
      }
      if (nJumps==1) {
        if (verbose) Rprintf(" All rows were sampled since file is small so we know nrow=%d exactly\n", sampleLines);
        estnrow = allocnrow = sampleLines;
      } else {
        if (sampleLines > allocnrow) STOP("Internal error: sampleLines(%d) > allocnrow(%d)", sampleLines, allocnrow);
      }
      if (nrowLimit<allocnrow) {
        if (verbose) Rprintf(" Alloc limited to lower nrows=%d passed in.\n", nrowLimit);
        estnrow = allocnrow = nrowLimit;
      }
      if (verbose) Rprintf("=====\n");
    }
    
    // ********************************************************************************************
    //   Apply colClasses, select and integer64
    // ********************************************************************************************
    ch = pos;
    int numNULL = 0;
    SEXP colTypeIndex, items, itemsInt, UserTypeNameSxp;
    int tmp[ncol]; for (int i=0; i<ncol; i++) tmp[i]=0;  // used to detect ambiguities (dups) in user's input
    if (isLogical(colClasses)) {
        // allNA only valid logical input
        for (int k=0; k<LENGTH(colClasses); k++) if (LOGICAL(colClasses)[k] != NA_LOGICAL) STOP("when colClasses is logical it must be all NA. Position %d contains non-NA: %d", k+1, LOGICAL(colClasses)[k]);
        if (verbose) Rprintf("Argument colClasses is ignored as requested by provided NA values\n");
    } else if (length(colClasses)) {
        UserTypeNameSxp = PROTECT(allocVector(STRSXP, NUT));
        protecti++;
        for (int i=0; i<NUT; i++) SET_STRING_ELT(UserTypeNameSxp, i, mkChar(UserTypeName[i]));
        if (isString(colClasses)) {
            // this branch unusual for fread: column types for all columns in one long unamed character vector
            if (length(getAttrib(colClasses, R_NamesSymbol))) STOP("Internal error: colClasses has names, but these should have been converted to list format at R level");
            if (LENGTH(colClasses)!=1 && LENGTH(colClasses)!=ncol) STOP("colClasses is unnamed and length %d but there are %d columns. See ?data.table for colClasses usage.", LENGTH(colClasses), ncol);
            colTypeIndex = PROTECT(chmatch(colClasses, UserTypeNameSxp, NUT, FALSE));  // if type not found then read as character then as. at R level
            protecti++;
            for (int k=0; k<ncol; k++) {
                if (STRING_ELT(colClasses, LENGTH(colClasses)==1 ? 0 : k) == NA_STRING) {
                    if (verbose) Rprintf("Column %d ('%s') was detected as type '%s'. Argument colClasses is ignored as requested by provided NA value\n", k+1, CHAR(STRING_ELT(names,k)), UserTypeName[type[k]] );
                    continue;
                }
                int thisType = UserTypeNameMap[ INTEGER(colTypeIndex)[ LENGTH(colClasses)==1 ? 0 : k] -1 ];
                if (type[k]<thisType) {
                    if (verbose) Rprintf("Column %d ('%s') was detected as type '%s' but bumped to '%s' as requested by colClasses\n", k+1, CHAR(STRING_ELT(names,k)), UserTypeName[type[k]], UserTypeName[thisType] );
                    type[k]=thisType;
                    if (thisType == SXP_NULL) numNULL++;
                } else if (verbose && type[k]>thisType) warning("Column %d ('%s') has been detected as type '%s'. Ignoring request from colClasses to read as '%s' (a lower type) since NAs (or loss of precision) may result.\n", k+1, CHAR(STRING_ELT(names,k)), UserTypeName[type[k]], UserTypeName[thisType]);
            }
        } else {  // normal branch here
            if (!isNewList(colClasses)) STOP("colClasses is not type list or character vector");
            if (!length(getAttrib(colClasses, R_NamesSymbol))) STOP("colClasses is type list but has no names");
            colTypeIndex = PROTECT(chmatch(getAttrib(colClasses, R_NamesSymbol), UserTypeNameSxp, NUT, FALSE));
            protecti++;
            for (int i=0; i<LENGTH(colClasses); i++) {
                int thisType = UserTypeNameMap[INTEGER(colTypeIndex)[i]-1];
                items = VECTOR_ELT(colClasses,i);
                if (thisType == SXP_NULL) {
                    if (!isNull(drop) || !isNull(select)) STOP("Can't use NULL in colClasses when select or drop is used as well.");
                    drop = items;
                    continue;
                }
                if (isString(items)) itemsInt = PROTECT(chmatch(items, names, NA_INTEGER, FALSE));
                else itemsInt = PROTECT(coerceVector(items, INTSXP));
                protecti++;
                for (j=0; j<LENGTH(items); j++) {
                    k = INTEGER(itemsInt)[j];
                    if (k==NA_INTEGER) {
                        if (isString(items)) STOP("Column name '%s' in colClasses[[%d]] not found", CHAR(STRING_ELT(items, j)),i+1);
                        else STOP("colClasses[[%d]][%d] is NA", i+1, j+1);
                    } else {
                        if (k<1 || k>ncol) STOP("Column number %d (colClasses[[%d]][%d]) is out of range [1,ncol=%d]",k,i+1,j+1,ncol);
                        k--;
                        if (tmp[k]++) STOP("Column '%s' appears more than once in colClasses", CHAR(STRING_ELT(names,k)));
                        if (type[k]<thisType) {
                            if (verbose) Rprintf("Column %d ('%s') was detected as type '%s' but bumped to '%s' as requested by colClasses[[%d]]\n", k+1, CHAR(STRING_ELT(names,k)), UserTypeName[type[k]], UserTypeName[thisType], i+1 );
                            type[k]=thisType;
                            if (thisType == SXP_NULL) numNULL++;
                        } else if (verbose && type[k]>thisType) Rprintf("Column %d ('%s') has been detected as type '%s'. Ignoring request from colClasses[[%d]] to read as '%s' (a lower type) since NAs would result.\n", k+1, CHAR(STRING_ELT(names,k)), UserTypeName[type[k]], i+1, UserTypeName[thisType]);
                    }
                }
            }
        }
    }
    int readInt64As = SXP_INT64;
    if (strcmp(CHAR(STRING_ELT(integer64,0)), "integer64")!=0) {
        if (strcmp(CHAR(STRING_ELT(integer64,0)), "character")==0)
            readInt64As = SXP_STR;
        else // either 'double' or 'numeric' as checked above in input checks
            readInt64As = SXP_REAL;
        for (int i=0; i<ncol; i++) if (type[i]==SXP_INT64) {
            type[i] = readInt64As;
            if (verbose) Rprintf("Column %d ('%s') has been detected as type 'integer64'. But reading this as '%s' according to the integer64 parameter.\n", i+1, CHAR(STRING_ELT(names,i)), CHAR(STRING_ELT(integer64,0)));
        }
    }
    if (verbose) { Rprintf("Type codes (colClasses) : "); printTypes(type, ncol); Rprintf("\n"); }
    if (length(drop)) {
        if (any_duplicated(drop,FALSE)) STOP("Duplicates detected in drop");
        if (isString(drop)) itemsInt = PROTECT(chmatch(drop, names, NA_INTEGER, FALSE));
        else itemsInt = PROTECT(coerceVector(drop, INTSXP));
        protecti++;
        for (j=0; j<LENGTH(drop); j++) {
            k = INTEGER(itemsInt)[j];
            if (k==NA_INTEGER) {
                if (isString(drop)) warning("Column name '%s' in 'drop' not found", CHAR(STRING_ELT(drop, j)));
                else warning("drop[%d] is NA", j+1);
            } else {
                if (k<1 || k>ncol) warning("Column number %d (drop[%d]) is out of range [1,ncol=%d]",k,j+1,ncol);
                else { type[k-1] = SXP_NULL; numNULL++; }
            }
        }
    }
    if (length(select)) {
        if (any_duplicated(select,FALSE)) STOP("Duplicates detected in select");
        if (isString(select)) {
            // invalid cols check part of #1445 moved here (makes sense before reading the file)
            itemsInt = PROTECT(chmatch(select, names, NA_INTEGER, FALSE));
            for (int i=0; i<length(select); i++) if (INTEGER(itemsInt)[i]==NA_INTEGER) 
                warning("Column name '%s' not found in column name header (case sensitive), skipping.", CHAR(STRING_ELT(select, i)));
            UNPROTECT(1);
            PROTECT_WITH_INDEX(itemsInt, &pi);
            REPROTECT(itemsInt = chmatch(names, select, NA_INTEGER, FALSE), pi); protecti++;
            for (int i=0; i<ncol; i++) if (INTEGER(itemsInt)[i]==NA_INTEGER) { type[i]=SXP_NULL; numNULL++; }
        } else {
            itemsInt = PROTECT(coerceVector(select, INTSXP)); protecti++;
            for (int i=0; i<ncol; i++) tmp[i]=SXP_NULL;
            for (int i=0; i<LENGTH(itemsInt); i++) {
                k = INTEGER(itemsInt)[i];
                if (k<1 || k>ncol) STOP("Column number %d (select[%d]) is out of range [1,ncol=%d]",k,i+1,ncol);
                tmp[k-1] = type[k-1];
            }
            for (int i=0; i<ncol; i++) type[i] = tmp[i];
            numNULL = ncol - LENGTH(itemsInt);
        }
    }
    if (verbose) { Rprintf("Type codes (drop|select): "); printTypes(type, ncol); Rprintf("\n"); }
    double tColType = wallclock();
    
    // ********************************************************************************************
    //   Allocate the result columns
    // ********************************************************************************************
    if (verbose) Rprintf("Allocating %d column slots (%d - %d dropped)\n", ncol-numNULL, ncol, numNULL);
    ans=PROTECT(allocVector(VECSXP,ncol-numNULL));  // safer to leave over allocation to alloc.col on return in fread.R
    protecti++;
    size_t ansSize = SIZEOF(ans)*(ncol-numNULL)*2;  // the VECSXP and its column names  (exclude global character cache usage)
    if (numNULL==0) {
        setAttrib(ans,R_NamesSymbol,names);
    } else {
        SEXP resnames;
        resnames = PROTECT(allocVector(STRSXP, ncol-numNULL));  protecti++;
        for (int i=0,resi=0; i<ncol; i++) if (type[i]!=SXP_NULL) {
            SET_STRING_ELT(resnames,resi++,STRING_ELT(names,i));
        }
        setAttrib(ans, R_NamesSymbol, resnames);
    }
    for (int i=0,resi=0; i<ncol; i++) {
        if (type[i] == SXP_NULL) continue;
        SEXP thiscol = allocVector(TypeSxp[ type[i] ], allocnrow);
        SET_VECTOR_ELT(ans,resi++,thiscol);  // no need to PROTECT thiscol, see R-exts 5.9.1
        if (type[i]==SXP_INT64) setAttrib(thiscol, R_ClassSymbol, ScalarString(char_integer64));
        SET_TRUELENGTH(thiscol, allocnrow);
        ansSize += SIZEOF(thiscol)*allocnrow;
    }
    double tAlloc = wallclock();
    
    // ********************************************************************************************
    //   madvise sequential
    // ********************************************************************************************
    // Off by default as not sure we can rely on n threads _each_ sequential counting as sequential, on
    // all platforms. It could hurt performance if not.
    // TODO: add function argument to allow users to try it out.
    if (fnam!=NULL) {
      #ifdef MADV_SEQUENTIAL
        //int ret = madvise((void *)origmmp, (size_t)filesize, MADV_SEQUENTIAL);
        // read ahead and drop behind each read point as they move through
        //if (verbose) Rprintf("madvise sequential: %s\n", ret ? strerror(errno) : "ok");
      #else
        // if (verbose) Rprintf("madvise sequential: not available on Windows\n");
        // Maybe try PrefetchVirtualMemory from Windows 8+ it seems
      #endif
      // madvise is just a request, does nothing otherwise and returns instantly. No need to time it.
    }
    
    // ********************************************************************************************
    //   Read the data
    // ********************************************************************************************
    ch = pos;   // back to start of first data row
    int hasPrinted=0;  // the percentage last printed so it prints every 2% without many calls to wallclock()
    _Bool stopTeam=FALSE;
    int numTypeErr=0, numTypeErrCols=0;
    char *typeErr=NULL;  size_t typeErrSize=0;
    #define stopErrSize 1000
    char stopErr[stopErrSize+1]="";  // must be compile time size: the message is generated and we can't free before STOP
    int ansi=0;   // the current row number in ans that we are writing to
    const char *prevThreadEnd = pos;  // the position after the last line the last thread processed (for checking)
    SEXP *buff;
    size_t workSize=0;
    int buffGrown=0;
    int nth = getDTthreads();   // TODO add nThread function argument
    int chunkMB=1;
    // 1MB chunks for each thread.  Thinking that ideal is nth*chunkMB*3 < total cache (untested)
    //   Where 3 =  1) file pages 2) buffers 3) final result
    size_t chunkBytes = MAX(5*maxLen, chunkMB*1024*1024);  // chunk size given to each thread. 1MB * nth should fit in cache
    if (nJumps/*from sampling*/>1 && nth>1) {
      // ensure data size is split into same sized chunks (no remainder in last chunk) and a multiple of nth
      nJumps = (int)((size_t)(lastRowEnd-pos)/chunkBytes);  // (int) rounds down
      if (nJumps==0) nJumps=1;
      else if (nJumps>nth) nJumps = nth*(1+(nJumps-1)/nth);
      chunkBytes = (size_t)(lastRowEnd-pos)/nJumps;
    } else nJumps=1;
    nth = MIN(nJumps, nth);
    
    #pragma omp parallel num_threads(nth)
    {
      int me = omp_get_thread_num();
      #pragma omp master
      {
        int nth_actual = omp_get_num_threads();
        if (nth_actual>nth) {
          snprintf(stopErr, stopErrSize, "OpenMP error: omp_get_num_threads()=%d > num_threads(nth=%d) directive",
                                         nth_actual, nth);
          stopTeam=TRUE;
        } else if (nth_actual < nth) {
          if (verbose) Rprintf("Team started with %d threads despite num_threads(nth=%d) directive. Likely limited at system level.\n", 
                               nth_actual, nth);
          nth = nth_actual;
        }
        if (!stopTeam) {
          if (verbose) Rprintf("Reading %d chunks of %.3fMB (%d rows) using %d threads\n",
                                nJumps, (double)chunkBytes/(1024*1024), (int)(chunkBytes/meanLineLen), nth);
          if (!(buff = malloc(nth*LENGTH(ans)*sizeof(SEXP *)))) {
            snprintf(stopErr, stopErrSize, "Failed to allocate buffer pointers, size %d*%d*%d", nth, LENGTH(ans), (int)sizeof(SEXP *));
            // ncols can be many thousands so we can't rely on a VLA
            stopTeam = TRUE;
          } else if (nJumps==1) {
            // most likely a small file, we've sampled all of it and so we know nrow exactly already. Or nth has been limited.
            // point buffers directly to the result to save alloc and time copying
            for (int i=0; i<LENGTH(ans); i++) buff[i] = VECTOR_ELT(ans,i);
          } else {
            int initialBuffSize = MAX(orig_allocnrow/nJumps,128);
            // minimum 128 to avoid lots of tiny growing on small files; e.g. if fread is ever highly iterated on flag/status files
            // orig_allocnrow typically 10-20% bigger than estimated final nrow, so the buffers are small with the same overage %
            // buffers will be grown if i) we observe a lot of out-of-sample short lines or ii) the lines are shorter in the
            // first half of the file than the second half (for example)
            for (int i=0; i<LENGTH(ans); i++) {   // LENGTH(ans) not ncol because LENGTH(ans)<ncol when numNULL>0
              SEXP this = VECTOR_ELT(ans,i);
              for (int j=0; j<nth; j++) {
                buff[j*LENGTH(ans)+i] = PROTECT(allocVector(TYPEOF(this), initialBuffSize));
                protecti++;
                workSize += SIZEOF(this)*initialBuffSize;
              }
            }
          }
        }
      }
      #pragma omp barrier
      #pragma omp for ordered schedule(dynamic)
      for (int jump=0; jump<nJumps; jump++) {
        if (stopTeam) continue;
        int myStopReason=0;
        int j=-1;  // in this scope to be used in error message in ordered
        const char *ch = pos+jump*chunkBytes;
        const char *nextJump = jump<nJumps-1 ? ch+chunkBytes : lastRowEnd-1;
        int buffi=0;  // the row read so far from this jump point. We don't know how many rows exactly this will be yet
        SEXP *mybuff = buff + me*LENGTH(ans);
        if (jump>0 && !nextGoodLine(&ch, ncol)) { stopTeam=TRUE; Rprintf("yes %d\n",jump); continue; }  // TODO add error message 
        const char *thisThreadStart=ch;
        const char *lineStart=ch;
        while (ch<=nextJump) {     // including = for when nextJump happens to fall exactly on a line start 
          if (buffi==LENGTH(mybuff[0])) {
            // buffer full due to unusually short lines in this chunk vs the sample; e.g. #2070
            #pragma omp critical
            for (int i=0; i<LENGTH(ans); i++) {
              mybuff[i] = PROTECT(growVector(mybuff[i], buffi*2));
              protecti++;
            }
            buffGrown++;  // just for verbose message afterwards
          }
          lineStart = ch;  // for error message
          if (sep==' ') while (ch<eof && *ch==' ') ch++;  // multiple sep=' ' at the lineStart does not mean sep(!)
          skip_white(&ch);  // solely for blank lines otherwise could leave to field processors which handle leading white
          if (ch>=eof || *ch==eol) {
            if (skipEmptyLines) { ch+=eolLen; continue; }
            else if (!fill) { myStopReason=1; break; }
            // in ordered we'll make the error message when we know the line number and stopTeam then 
          }
          j=0;
          int resj=0;
          while (j<ncol) {
            // Rprintf("Field %d: '%.10s' as type %d\n", j+1, ch, type[j]);
            const char *fieldStart = ch;
            SEXP buffcol = mybuff[resj];
            if (!(*fun[type[j]])(&ch,buffcol,buffi)) {  // normally returns success(1) and buffcol[buffi] is assigned inside *fun.
              #pragma omp atomic
              numTypeErr++;  // if seen a type fail before in this column, just atomic inc and move on
              if (typeOk[j]) {
                #pragma omp critical
                // happens rarely and when it does only once per column, so shared critical (unnamed) with other critical below
                // is ok to give openMP the best chance to save managing separate criticals
                {
                  typeOk[j] = FALSE;  // don't do this section again for any more fails in this column
                  numTypeErrCols++;   // == sum(typeOk==0)
                  // Can't Rprintf because we're likely not master. So accumulate message and print afterwards.
                  // We don't know row number yet, as we jumped here
                  // TODO - auto rerun with right classes rather than worry about getting row number in here; still put this into verbose
                  char temp[1001];
                  int len = snprintf(temp, 1000, "Column %d ('%s') guessed '%s' but contains <<%.*s>>\n",
                          j+1, CHAR(STRING_ELT(names,j)), UserTypeName[type[j]], (int)(ch-fieldStart), fieldStart);
                  typeErr = realloc(typeErr, typeErrSize+len+1);
                  strcpy(typeErr+typeErrSize, temp);
                  typeErrSize += len;
                }
              }
            }
            resj += (type[j++]!=SXP_NULL);
            if (ch>=eof || *ch==eol) break;
            ch++;
          }
          if (j<ncol)  {
            // not enough columns observed
            if (!fill) { myStopReason = 2; break; }
            while (j<ncol) {
              SEXP buffcol = mybuff[resj];
              switch (type[j]) {  // TODO: make type an enum to ensure switch uses jump table here
              case SXP_LGL:
                LOGICAL(buffcol)[buffi] = NA_LOGICAL;
                break;
              case SXP_INT:
                INTEGER(buffcol)[buffi] = NA_INTEGER;
                break;
              case SXP_INT64:
                REAL(buffcol)[buffi] = NA_INT64_D;
                break;
              case SXP_REAL:
                REAL(buffcol)[buffi] = NA_REAL;
                break;
              case SXP_STR:
                #pragma omp critical
                SET_STRING_ELT(buffcol, buffi, blank_is_a_nastring ? NA_STRING : R_BlankString);
                // Yes, R's allocVector already initialized with R_BlankString so we could potential just assign NA_STRING
                // directly. However, here we are rewriting buff many times (not the final ans once). CHARSXP from me's last
                // chunk will be in this buffer so that CHARSXP's refs will need to be decremented by SET_STRING_ELT.
              }
              resj += (type[j++]!=SXP_NULL);
            }
          }
          if (ch<eof && *ch!=eol) { myStopReason = 3; break; }
          ch+=eolLen;
          buffi++;
        }
        
        int myansi=0;  // which row in the final result to write my buffer to
        int howMany=0; // how many to write. 
        #pragma omp ordered
        {
          if (!myStopReason && !stopTeam) {
            // Normal branch
            if (prevThreadEnd != thisThreadStart) {
              snprintf(stopErr, stopErrSize,
                "Jump %d did not end exactly where jump %d found its first good line start. "
                "end(%p)<<%.*s>> != start(prev%+d)<<%.*s>>",
                jump-1, jump, prevThreadEnd, STRLIM(prevThreadEnd,50), prevThreadEnd,
                (int)(thisThreadStart-prevThreadEnd), STRLIM(thisThreadStart,50), thisThreadStart);
              stopTeam=TRUE;
            } else {
              myansi = ansi;  // fetch shared ansi -- where to write my results to the answer.
              prevThreadEnd = ch; // tell the next thread where I finished so it can check it started exactly there
              if (myansi<nrowLimit) {
                // Normal branch
                howMany = MIN(buffi, nrowLimit-myansi);
                ansi += howMany;  // update shared ansi to tell the next thread which row I am going to finish on
                // The next thread can now go ahead and copy its results to ans at the same time as I copy my results to ans
              } else {
                stopTeam=TRUE;
                // nrowLimit was supplied and these required rows were handled by previous jumps as I was running
              }
            }
          } else if (!stopTeam) {
            // stopping here in ordered enables to stop on the first error in the file. we now know the row number since
            // previous jumps have already passed through here
            int line=ansi+buffi+row1Line;
            switch(myStopReason) {
            case 1:
              snprintf(stopErr, stopErrSize,
              "Line %d is empty. It is outside the sample rows. " \
              "Set fill=TRUE to treat it as an NA row, or blank.lines.skip=TRUE to skip it", line);
              // TODO - include a few (numbered) lines before and after in the message
              break;
            case 2:
              snprintf(stopErr, stopErrSize,
              "Expecting %d cols but line %d contains only %d cols (sep='%c'). " \
              "Consider fill=TRUE. <<%.*s>>",
              ncol, line, j, sep, STRLIM(lineStart, 500), lineStart);
              break;
            case 3:
              snprintf(stopErr, stopErrSize,
              "Too many fields on line %d outside the sample. Read all %d expected columns but more are present. <<%.*s>>",
              line, ncol, STRLIM(lineStart, 500), lineStart);
              break;
            default:
              snprintf(stopErr, stopErrSize, "Internal error: unknown myStopReason %d", myStopReason);
            }
            stopTeam=TRUE;
          }
        } // end ordered
        if (stopTeam) continue;
        
        // Copy my buffer to ans now while these pages are hot in my core
        // All threads do this at the same time, as soon as the previous thread knows the number of rows, in ordered above
        if (nJumps==1) {
          // buffers were pointed directly to ans (small file or we were thread limited) so nothing left to do
        } else {
          if (howMany < buffi) {
            // nrows was set by user (a limit of rows to read) and this is the last jump that fills up the required nrows
            stopTeam=TRUE; // otherwise this thread would pick up the next jump and read that wastefully before stopping at ordered
          }
          for (int j=0; j<LENGTH(ans); j++) {
            SEXP col = VECTOR_ELT(ans, j);
            if (TYPEOF(col)!=STRSXP) {
              size_t size = SIZEOF(col);
              memcpy((char *)DATAPTR(col)+myansi*size, (char *)DATAPTR(mybuff[j]), howMany*size);
            } else {
              #pragma omp critical
              // SET_STRING_ELT decs old and incs new. old will be R_BlankString (global in R, never gc'd) initialized
              // by allocVector but new is shared within this column and others and not thread safe. Hence shared critical.
              {
                int dest=myansi;
                SEXP *val = (SEXP *)DATAPTR(mybuff[j]);
                for (int i=0; i<howMany; i++) SET_STRING_ELT(col, dest++, *val++);
              }
            }
          }
        }
        if (me==0 && (hasPrinted || (showProgress && jump<nth && (nJumps/nth-1)*(wallclock()-tAlloc)>3.0))) {
          // As in fwrite, only the master thread (me==0) should Rprintf otherwise C stack usage problems it seems
          // See comments in fwrite.c for how we might in future be able to R_CheckUserInterrupt() here.
          // Jump 0 might not be assigned to thread 0, hence jump<nth above to use me==0 in the first wave for the time estimate
          int p = (int)(100.0*(jump+1)/nJumps);
          if (p>=hasPrinted) {
            Rprintf("\rRead %d%% of %d estimated rows", p, estnrow);
            hasPrinted = p+2;  // print every 2%
          }
          #ifdef WIN32
          R_FlushConsole();
          #endif
        }
      }
    }  // end OpenMP parallel
    free(buff);
    double tRead = wallclock();
    double tot=tRead-t0;
    if (hasPrinted || verbose) {
        Rprintf("\rRead %d rows x %d columns from %.3fGB file in ", ansi, ncol-numNULL, 1.0*filesize/(1024*1024*1024));
        Rprintf("%02d:%06.3f ", (int)tot/60, fmod(tot,60.0));
        Rprintf("wall clock time (can be slowed down by any other open apps even if seemingly idle)\n");
        // since parallel, clock() cycles is parallel too: so wall clock will have to do
    }
    if (verbose) Rprintf("Thread buffers were grown %d times (if all %d threads each grew once, this figure would be %d)\n", buffGrown, nth, nth);
    if (numTypeErr) {
        Rprintf(typeErr);
        R_FlushConsole();
        free(typeErr);
        STOP("The guessed column type was insufficient for %d values in %d columns. The first in each column was printed to the console. Use colClasses to set these column classes manually.", numTypeErr, numTypeErrCols);
    }
    if (stopTeam && stopErr[0]!='\0') STOP(stopErr); // else nrowLimit applied and stopped early normally
    if (ansi > allocnrow) {
      if (nrowLimit>allocnrow) STOP("Internal error: ansi(%d)>allocnrow(%d) but nrows=%d (not limited)", ansi, allocnrow, nrowLimit);
      // for the last jump that fills nrow limit, then ansi is +=buffi which is >allocnrow and correct
    } else if (ansi == allocnrow) {
      if (verbose) Rprintf("Read %d rows. Exactly what was estimated and allocated up front\n", ansi);
    } else {
      for (int i=0; i<LENGTH(ans); i++) SETLENGTH(VECTOR_ELT(ans,i), ansi);
    }
    if (verbose) {
        if (tot<0.000001) tot=0.000001;  // to avoid nan% output in some trivially small tests where tot==0.000s
        Rprintf("%8.3fs (%3.0f%%) Memory map\n", tMap-t0, 100.0*(tMap-t0)/tot);
        Rprintf("%8.3fs (%3.0f%%) sep, ncol and header detection\n", tLayout-tMap, 100.0*(tLayout-tMap)/tot);
        Rprintf("%8.3fs (%3.0f%%) Column type detection using %d sample rows from %d jump points\n", tColType-tLayout, 100.0*(tColType-tLayout)/tot, sampleLines, nJumps);
        Rprintf("%8.3fs (%3.0f%%) Allocation of %d rows x %d cols (%.3fGB) plus %.3fGB of temporary buffers\n", tAlloc-tColType, 100.0*(tAlloc-tColType)/tot, allocnrow, ncol, (double)ansSize/(1024*1024*1024), (double)workSize/(1024*1024*1024));
        Rprintf("%8.3fs (%3.0f%%) Reading data\n", tRead-tAlloc, 100.0*(tRead-tAlloc)/tot);
        Rprintf("%8.3fs        Total\n", tot);
    }
    UNPROTECT(protecti);
    closeFile();
    return(ans);
}

