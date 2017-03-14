#include "data.table.h"
#include <Rdefines.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

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

static const char *ch, *eof; 
static char sep, eol, eol2;  // sep2 TO DO
static int quoteRule;
static int eolLen, line;
static SEXP nastrings;
static Rboolean verbose, ERANGEwarning, any_number_like_nastrings;
static Rboolean stripWhite;  // only applies to unquoted character columns; numeric fields always stripped 
static clock_t tCoerce, tCoerceAlloc;
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
static const char TypeName[NUMTYPE][10] = {"LGL","INT","INT64","REAL","STR","NULL"};  // for messages and errors
static int TypeSxp[NUMTYPE] = {LGLSXP,INTSXP,REALSXP,REALSXP,STRSXP,NILSXP};
#define NUT        8   // Number of User Types (just for colClasses where "numeric"/"double" are equivalent)
static const char UserTypeName[NUT][10] = {"logical", "integer", "integer64", "numeric", "character", "NULL", "double", "CLASS" };
// important that first 6 correspond to TypeName.  "CLASS" is the fall back to character then as.class at R level ("CLASS" string is just a placeholder).
static int UserTypeNameMap[NUT] = { SXP_LGL, SXP_INT, SXP_INT64, SXP_REAL, SXP_STR, SXP_NULL, SXP_REAL, SXP_STR };
static char quote;
typedef Rboolean (*reader_fun_t)(SEXP, int);

// Not sure how these will end up. So not exposed to user, yet.
#define SEPLINES  30      // how many lines (from line skip) to use to guess sep and quote rule
#define NJUMPS    10      // how many points to jump to
#define JUMPLINES 100     // at each jump point how many lines to use to guess column types

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

static inline void skip_white(const char **this) {
  // skip space so long as sep isn't space and skip tab so long as sep isn't tab
  const char *lch = *this;
  while(lch<eof && (*lch==' ' || *lch== '\t') && *lch!=sep) lch++;
  *this = lch;
}

static inline _Bool on_sep() {
  if (sep==' ' && ch<eof && *ch==' ') {
    while (ch+1<eof && *(ch+1)==' ') ch++;  // move to last of this sequence of spaces
    if (ch+1==eof || *(ch+1)==eol) ch++;    // if that's followed by eol or eof then move onto those
  }
  return ch>=eof || *ch==sep || *ch==eol;
}

static inline void next_sep() {
  while (ch<eof && *ch!=sep && *ch!=eol) ch++;
  on_sep(); // to deal with multiple spaces when sep==' '
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

static Rboolean Field(SEXP targetCol, int targetRow)
{
  if (ch>=eof) STOP("Internal error: ch>=eof when Field() called");
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
      STOP("Internal error: undefined quote rule %d", quoteRule);
    }
  }
  int fieldLen = (int)(ch-fieldStart);
  if (stripWhite && !quoted) {
    while(fieldLen>0 && (fieldStart[fieldLen-1]==' ' || fieldStart[fieldLen-1]=='\t')) fieldLen--;
    // this white space (' ' or '\t') can't be sep otherwise it would have stopped the field earlier at the first sep
  }
  if (quoted) { ch++; if (stripWhite) skip_white(&ch); }
  if (!on_sep()) return FALSE;
  if (targetCol) SET_STRING_ELT(targetCol, targetRow, fieldLen==0 ? R_BlankString : mkCharLenCE(fieldStart, fieldLen, ienc));
  return TRUE;
}

static int countfields()
{
  if (sep==' ') while (ch<eof && *ch==' ') ch++;  // Correct to be sep==' ' only and not skip_white(). 
  int ncol = 1;
  while (ch<eof && *ch!=eol) {
    if (!Field(NULL,0)) return -1;   // -1 means this line not valid for this sep and quote rule
    // Field() leaves *ch resting on sep, eol or >=eof.  (Checked inside Field())
    if (ch<eof && *ch!=sep && *ch!=eol) STOP("Internal error: Field didn't stop on sep, eol or eof");
    if (ch<eof && *ch!=eol) { ncol++; ch++; } // move over sep (which will already be last ' ' if sep=' ').
                //   ^^  Not *ch==sep because sep==eol when readLines
  }
  ch+=eolLen; // may step past eof but that's ok as we never use ==eof in this file, always >=eof or <eof.
  return ncol;
}

static inline Rboolean StrtoI64(SEXP targetCol, int targetRow)
{
    // Specialized clib strtoll that :
    // i) skips leading isspace() too but other than field separator and eol (e.g. '\t' and ' \t' in FUT1206.txt)
    // ii) has fewer branches for speed as no need for non decimal base
    // iii) updates global ch directly saving arguments
    // iv) safe for mmap which can't be \0 terminated on Windows (but can be on unix and mac)
    // v) fails if whole field isn't consumed such as "3.14" (strtol consumes the 3 and stops)
    // ... all without needing to read into a buffer at all (reads the mmap directly)
    skip_white(&ch);  //  ',,' or ',   ,' or '\t\t' or '\t   \t' etc => NA
    if (on_sep()) {  // most often ',,' 
      if (targetCol) REAL(targetCol)[targetRow]=NA_INT64_D;
      return TRUE;
    }
    const char *start=ch;
    int sign=1;
    if (ch<eof && (*ch=='-' || *ch=='+')) sign -= 2*(*ch++=='-');
    Rboolean ok = ch<eof && '0'<=*ch && *ch<='9';  // a single - or + with no [0-9] is !ok and considered type character
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
    ok = ok && on_sep();
    //Rprintf("StrtoI64 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    if (ok && !any_number_like_nastrings) return TRUE;  // most common case, return 
    _Bool na = is_nastring(start);
    if (ok && !na) return TRUE;
    if (targetCol) REAL(targetCol)[targetRow] = NA_INT64_D;
    next_sep();  // consume the remainder of field, if any
    return na;
}    

static inline Rboolean StrtoI32(SEXP targetCol, int targetRow)
{
    // Very similar to StrtoI64 (see it for comments). We can't make a single function and switch on TYPEOF(targetCol) to
    // know I64 or I32 because targetCol is NULL when testing types and when dropping columns.
    skip_white(&ch);
    if (on_sep()) {  // most often ',,' 
      if (targetCol) INTEGER(targetCol)[targetRow]=NA_INTEGER;
      return TRUE;
    }
    const char *start=ch;
    int sign=1;
    if (ch<eof && (*ch=='-' || *ch=='+')) sign -= 2*(*ch++=='-');
    Rboolean ok = ch<eof && '0'<=*ch && *ch<='9';
    int acc = 0;
    while (ch<eof && '0'<=*ch && *ch<='9' && acc<(INT_MAX-10)/10) {  // NA_INTEGER==INT_MIN==-2147483648==-INT_MAX(+2147483647)-1
      acc *= 10;
      acc += *ch-'0';
      ch++;
    }
    if (targetCol) INTEGER(targetCol)[targetRow] = sign * acc;
    skip_white(&ch);
    ok = ok && on_sep();
    //Rprintf("StrtoI32 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    if (ok && !any_number_like_nastrings) return TRUE;
    Rboolean na = is_nastring(start);
    if (ok && !na) return TRUE;
    if (targetCol) INTEGER(targetCol)[targetRow] = NA_INTEGER;
    next_sep();
    return na;
}

static inline Rboolean StrtoD(SEXP targetCol, int targetRow)
{
    // Specialized strtod for same reasons as Strtoll (leading \t dealt with), but still uses stdlib:strtod
    // R's R_Strtod5 uses strlen() on input, so we can't use that here unless we copy field to a buffer (slow)
    // Could fork glibc's strtod and change it to pass in dec rather than via locale but see :
    //    http://www.exploringbinary.com/how-glibc-strtod-works/
    // i.e. feel more comfortable relying on glibc for speed and robustness (and more eyes have been on it)
    skip_white(&ch);
    if (on_sep()) {
      if (targetCol) REAL(targetCol)[targetRow]=NA_REAL;
      return TRUE;
    }
    const char *start=ch;
    errno = 0;
    double d = strtod(start, (char **)&ch);
    if (errno==ERANGE && ch>start) {
      ch = start;
      errno = 0;
      d = (double)strtold(start, (char **)&ch);
      // errno is checked further below where ok= is set
      if (ERANGEwarning) {  // FALSE initially when detecting types then its set TRUE just before reading data.
        warning("C function strtod() returned ERANGE for one or more fields. The first was string input '%.*s'. It was read using (double)strtold() as numeric value %.16E (displayed here using %%.16E); loss of accuracy likely occurred. This message is designed to tell you exactly what has been done by fread's C code, so you can search yourself online for many references about double precision accuracy and these specific C functions. You may wish to use colClasses to read the column as character instead and then coerce that column using the Rmpfr package for greater accuracy.", ch-start, start, d);
        ERANGEwarning = FALSE;   // once only warning
        // This is carefully worded as an ERANGE warning because that's precisely what it is.  Calling it a 'precision' warning
        // might lead the user to think they'll get a precision warning on "1.23456789123456789123456789123456789" too, but they won't
        // as that will be read fine by the first strtod() with silent loss of precision. IIUC.
      }
    }
    skip_white(&ch);
    Rboolean ok = errno==0 && ch>start && on_sep();
    if (targetCol) REAL(targetCol)[targetRow]=d;
    if (ok && !any_number_like_nastrings) return TRUE;
    Rboolean na = is_nastring(start);
    if (ok && !na) return TRUE;
    if (targetCol) REAL(targetCol)[targetRow] = NA_REAL;
    next_sep();
    return na;
}

static inline Rboolean StrtoB(SEXP targetCol, int targetRow)
{
    // These usually come from R when it writes out.
    skip_white(&ch);
    if (targetCol) LOGICAL(targetCol)[targetRow]=NA_LOGICAL;
    if (on_sep()) return TRUE;  // empty field most commonly ',,' 
    const char *start=ch;
    if (*ch=='T') {
        if (targetCol) LOGICAL(targetCol)[targetRow] = TRUE;
        ch++;
        if (on_sep()) return TRUE;
        if (ch+2<eof && *ch=='R' && *++ch=='U' && *++ch=='E' && ++ch && on_sep()) return TRUE;
        ch = start+1;
        if (ch+2<eof && *ch=='r' && *++ch=='u' && *++ch=='e' && ++ch && on_sep()) return TRUE;
    }
    else if (*ch=='F') {
        if (targetCol) LOGICAL(targetCol)[targetRow] = FALSE;
        ch++;
        if (on_sep()) return TRUE;
        if (ch+3<eof && *ch=='A' && *++ch=='L' && *++ch=='S' && *++ch=='E' && ++ch && on_sep()) return TRUE;
        ch = start+1;
        if (ch+3<eof && *ch=='a' && *++ch=='l' && *++ch=='s' && *++ch=='e' && ++ch && on_sep()) return TRUE;
    }
    if (targetCol) LOGICAL(targetCol)[targetRow] = NA_LOGICAL;
    next_sep();
    return is_nastring(start);
}


SEXP readfile(SEXP input, SEXP separg, SEXP nrowsarg, SEXP headerarg, SEXP nastringsarg, SEXP verbosearg, SEXP skip, SEXP select, SEXP drop, SEXP colClasses, SEXP integer64, SEXP dec, SEXP encoding, SEXP quoteArg, SEXP stripWhiteArg, SEXP skipEmptyLinesArg, SEXP fillArg, SEXP showProgressArg)
// can't be named fread here because that's already a C function (from which the R level fread function took its name)
{
    SEXP ans, thisstr;
    R_len_t j, k, protecti=0, nrow=0, ncol=0;
    const char *pos, *ch2, *lineStart;
    Rboolean header, allchar, skipEmptyLines, fill;
    verbose=LOGICAL(verbosearg)[0];
    clock_t t0 = clock();
    ERANGEwarning = FALSE;  // just while detecting types, then TRUE before the read data loop
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

    reader_fun_t fun[NUMTYPE] = {&StrtoB, &StrtoI32, &StrtoI64, &StrtoD, &Field, &Field};
    
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
    // if we know there are no nastrings which are numbers (like -999999) then in the number
    // field processors we can save an expensive step in checking the nastrings. Since if the field parses as a number,
    // we then know it can't be NA provided any_number_like_nastrings==FALSE.
    for (int i=0; i<length(nastrings); i++) {
      int nchar = LENGTH(STRING_ELT(nastrings,i));
      if (nchar==0) continue;  // otherwise "" is considered numeric by strtod
      const char *start=CHAR(STRING_ELT(nastrings,i));
      if (isspace(start[0]) || isspace(start[nchar-1])) error("na.strings[%d]=='%s' has whitespace at the beginning or end", i+1, start);
      if (!strcmp(start,"T") || !strcmp(start,"F") ||
          !strcmp(start,"TRUE") || !strcmp(start,"FALSE") ||
          !strcmp(start,"True") || !strcmp(start,"False")) error("na.strings[%d]=='%s' is recognized as type boolean. This string is not permitted in 'na.strings'.", i+1, start);
      char *end;
      errno = 0;
      strtod(start, &end);
      if (errno==0 && (int)(end-start)==nchar) {
        any_number_like_nastrings = TRUE;
        if (verbose) Rprintf("na.strings[%d]=='%s' is numeric so all numeric fields will have to check na.strings\n", i+1, start); 
      }
    }
    if (verbose && !any_number_like_nastrings) Rprintf("None of the %d 'na.strings' are numeric (such as '-9999') which is normal and best for performance.\n", LENGTH(nastrings)); 
    if (!isInteger(nrowsarg) || LENGTH(nrowsarg)!=1 || INTEGER(nrowsarg)[0]==NA_INTEGER) error("'nrows' must be a single non-NA number of type numeric or integer");
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
        
        // No MAP_POPULATE for faster nrow=10 and to make possible earlier progress bar in row count stage
        // Mac doesn't appear to support MAP_POPULATE anyway (failed on CRAN when I tried).
        // TO DO?: MAP_HUGETLB for Linux but seems to need admin to setup first. My Hugepagesize is 2MB (>>2KB, so promising)
        //         https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
        mmp = origmmp = (const char *)mmap(NULL, filesize, PROT_READ, MAP_PRIVATE, fd, 0);
        if (mmp == MAP_FAILED) {
            close(fd);
#else
        // Following: http://msdn.microsoft.com/en-gb/library/windows/desktop/aa366548(v=vs.85).aspx
        hFile = INVALID_HANDLE_VALUE;
        i = 0;
        while(hFile==INVALID_HANDLE_VALUE && i<5) {
            hFile = CreateFile(fnam, GENERIC_READ, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
            // FILE_SHARE_WRITE is required otherwise if the file is open in Excel, CreateFile fails. Should be ok now.
            if (hFile==INVALID_HANDLE_VALUE) {
                if (GetLastError()==ERROR_FILE_NOT_FOUND) error("File not found: %s",fnam);
                if (i<4) Sleep(250);  // 250ms
            }
            i++;
            // Looped retry to avoid ephemeral locks by system utilities as recommended here : http://support.microsoft.com/kb/316609
        }
        if (hFile==INVALID_HANDLE_VALUE) error("Unable to open file after %d attempts (error %d): %s", i, GetLastError(), fnam);
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
    clock_t tMap = clock();
    // From now use STOP() wrapper instead of error(), for Windows to close file so as not to lock the file after an error.
    
    // ********************************************************************************************
    //   Auto detect eol, first eol where there are two (i.e. CRLF)
    // ********************************************************************************************
    // take care of UTF8 BOM, #1087 and #1465
    if (!memcmp(mmp, "\xef\xbb\xbf", 3)) mmp += 3;   // this is why we need 'origmmp'
    ch = mmp;
    while (ch<eof && *ch!='\n' && *ch!='\r') {
        if (*ch==quote) while(++ch<eof && *ch!=quote) {};  // allows protection of \n and \r inside column names
        ch++;                                            // this 'if' needed in case opening protection is not closed before eof
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
    line = 1;
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
    lineStart = ch;
    while (ch<eof && isspace(*ch)) {   // isspace matches ' ', \t, \n and \r
      if (*ch==eol) { ch+=eolLen; lineStart=ch; line++; } else ch++;
    }
    if (ch>=eof) STOP("Input is either empty, fully whitespace, or skip has been set after the last non-whitespace.");
    if (verbose) {
      if (lineStart>ch) Rprintf("Moved forward to first non-blank line (%d)\n", line);
      Rprintf("Positioned on line %d starting: <<%.*s>>\n", line, STRLIM(lineStart, 30), lineStart);
    }
    ch = pos = lineStart;
    
    // ********************************************************************************************
    //   Auto detect separator, quoting rule, first line and ncol, simply.
    // ********************************************************************************************
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
    
    int topNumLines=0;   // the most number of lines with the same number of fields, so far
    int topNumFields=1;  // how many fields that was, to resolve ties
    int topNmax=0;       // for that sep and quote rule, what was the max number of columns (just for fill=TRUE)
    char topSep=eol;     // which sep that was, by default \n to mean single-column input (1 field)
    int topQuoteRule=0;  // which quote rule that was
    
    int numFields[SEPLINES];
    int numLines[SEPLINES];
    for (int i=0; i<SEPLINES; i++) { numFields[i]=0; numLines[i]=0; } // initialize VLAs
    for (int s=0; s<nseps; s++) {
      sep = seps[s];
      for (quoteRule=0; quoteRule<4; quoteRule++) {  // quote rule in order of preference
        ch = pos;
        // Rprintf("Trying sep='%c' with quoteRule %d ...", sep, quoteRule);
        int i=0; while (numFields[i]) { numFields[i]=0; numLines[i]=0; i++; }  // reset used slots
        int thisLine = 0;
        while (ch<eof && ++thisLine<=SEPLINES) {
          ncol = countfields();   // using this sep and quote rule; moves ch to start of next line
          // if (verbose) Rprintf("sep=%c  quoteRule=%d  Line %d has %d cols ending %.5s\n", sep, quoteRule, thisLine, ncol, ch-eolLen-5);
          if (ncol<0) { numFields[0]=-1; break; } // invalid file with this sep and quote rule; abort this rule
          if (ncol<=1) continue;  // skip blank lines and sep-not-found when counting
          int i=0; while (numFields[i]!=0 && numFields[i]!=ncol) i++;  // find matching slot, or first free one
          numFields[i] = ncol;    // just for the first time; harmless overwrite after that since numFields[i] already == ncol
          numLines[i]++;          // increment count of this number of fields
        }
        if (numFields[0]==-1) continue;
        _Bool updated=FALSE;
        int nmax=0;
        i=-1; while (numLines[++i]) {
          if (numFields[i] > nmax) nmax=numFields[i];  // for fill=TRUE to know max number of columns
          if (numLines[i]>topNumLines ||
             (numLines[i]==topNumLines && numFields[i]>topNumFields && sep!=' ')) {
            topNumLines=numLines[i]; topNumFields=numFields[i]; topSep=sep; topQuoteRule=quoteRule; topNmax=nmax;
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
      int thisLine=-1; while (ch<eof && ++thisLine<SEPLINES) {
        const char *lineStart = ch;
        if (countfields()==ncol) { ch=pos=lineStart; line+=thisLine; break; }
      }
    }
    // For standard regular separated files, we're now on the first byte of the file.
    
    if (ncol<1 || line<1) STOP("Internal error: ncol==%d line==%d after detecting sep, ncol and first line");
    int tt = countfields();
    ch = pos; // move back to start of line since countfields() moved to next
    if (!fill && tt!=ncol) STOP("Internal error: first line has field count %d but expecting %d", tt, ncol);
    if (verbose) {
      Rprintf("Detected %d columns on line %d. This line will be taken as either column names or first data row (first 30 chars): <<%.*s>>\n",
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
        if (allchar && !on_sep() && StrtoD(NULL, 0)) allchar=FALSE;  // considered testing at least one isalpha, but we want 1E9 to be considered a value for this purpose not a column name
        ch = this;  // rewind to the start of this field
        Field(NULL,0);  // StrtoD does not consume quoted fields according to the quote rule, so redo with Field()
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
          int tmp = countfields();
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
            Field(names,i);
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
    clock_t tLayout = clock();
    
    // ********************************************************************************************
    //   Count number of rows
    // ********************************************************************************************    
    ch = pos;
    nrow = INTEGER(nrowsarg)[0];
    if (pos>=eof || (*pos==eol && !fill && !skipEmptyLines)) {
        nrow=0;
        if (verbose) Rprintf("Byte after header row is eof or eol, 0 data rows present.\n");
    } else if (nrow>-1) {
        if (verbose) Rprintf("nrow set to nrows passed in (%d)\n", nrow);
        // Intended for nrow=10 to see top 10 rows quickly without touching remaining pages
    } else {
        long long neol=1, nsep=0, tmp;
        // handle most frequent case first
        if (!fill) {
            // goal: quick row count with no branches and cope with embedded sep and \n
            while (ch<eof) {
                neol+=(*ch==eol);
                nsep+=(*ch++==sep);
            }
        } else {
            // goal: don't count newlines within quotes, 
            // don't rely on 'sep' because it'll provide an underestimate
            if (!skipEmptyLines) {
                while (ch<eof) {
                    if (*ch!=quote) neol += (*ch==eol);
                    else while(ch+1<eof && *(++ch)!=quote); // quits at next quote
                    ch++;
                }
            } else {
                while (ch<eof) {
                    if (*ch!=quote) neol += (*ch==eol && *(ch-1)!=eol);
                    else while(ch+1<eof && *(++ch)!=quote); // quits at next quote
                    ch++;
                }                
            }
        }
        if (ch!=eof) STOP("Internal error: ch!=eof after counting sep and eol");
        int i=0; int endblanks=0;
        if (!fill) {
            while (i==0 && ch>pos) {
                // count blank lines at the end
                i=0; while (ch>pos && *--ch!=eol2) i += !isspace(*ch);
                endblanks += (i==0);
                ch -= eolLen-1;
            }
        } else if (fill && !skipEmptyLines) endblanks = (*(ch-1) == eol);

        // if (endblanks==0) There is non white after the last eol. Ok and dealt with. TO DO: reference test id here in comment
        if (ncol==1 || fill) tmp = neol-endblanks;
        else tmp = MIN( nsep/(ncol-1),  neol-endblanks );   // good quick estimate with embedded sep and eol in mind
        if (verbose || tmp>INT_MAX) {
            if (!fill) {
                Rprintf("Count of eol: %lld (including %d at the end)\n",neol,endblanks);
                if (ncol==1) Rprintf("ncol==1 so sep count ignored\n");
                else {
                    Rprintf("Count of sep: %lld\n",nsep);
                    Rprintf("nrow = MIN( nsep [%lld] / (ncol [%d] -1), neol [%lld] - endblanks [%d] ) = %lld\n", nsep, ncol, neol, endblanks, tmp);
                }
            } else {
                if (!skipEmptyLines) 
                    Rprintf("nrow = neol [%lld] - endblanks [%d] = %lld\n", neol, endblanks, tmp);
                else Rprintf("nrow = neol (after discarding blank lines) = %lld\n", tmp);
            }
            if (tmp > INT_MAX) STOP("nrow larger than current 2^31 limit");
        }
        nrow = tmp;
        // Advantages of exact count: i) no need to slightly over allocate (by 5%, say) so no need to clear up on heap during gc(),
        // and ii) no need to implement realloc if estimate doesn't turn out to be large enough (e.g. if sample rows are wider than file average).
        // TO DO: goes away if we allow over-allocated columns.
        // The old estimate method based on size of first 10 rows :
        // estn = (R_len_t)ceil(1.05 * 10 * (filesize-(pos-mmp)) / (pos2-pos1)) +5;  // +5 for small files
        // if (verbose) Rprintf("Estimated nrows: %d ( 1.05*%d*(%ld-(%ld-%ld))/(%ld-%ld) )\n",estn,10,filesize,pos,mmp,pos2,pos1);
    }
    clock_t tRowCount = clock();
    
    // *****************************************************************************************************************
    //   Make best guess at column types using 100 rows at 10 points, including the very first, middle and very last row
    // *****************************************************************************************************************
    int   type[ncol];
    _Bool typeOk[ncol];
    for (int i=0; i<ncol; i++) { 
      type[i]   =0;   // 0 because it's the lowest type ('logical')
      typeOk[i] =1;   // set to 0 later to output one message per column if the guessed type is not sufficient
    }   
    int numPoints = nrow>1000 ? 11  : 1;
    int eachNrows = nrow>1000 ? 100 : nrow;  // if nrow<=1000, test all the rows in a single iteration
    for (j=0; j<numPoints; j++) {
        if (j<10) {
            ch = pos + j*(eof-pos)/10;
        } else {
            ch = eof - 50*(eof-pos)/nrow;
            // include very last line by setting last point apx 50 lines from
            // end and testing 100 lines from there until eof.
        }
        if (j>0) {
            // we may have landed inside quoted field containing embedded sep and/or embedded \n
            // find next \n and see if 5 good lines follow. If not try next \n, and so on, until we find the real \n
            // We don't know which line number this is because we jumped straight to it
            int attempts=0;
            while (ch<eof && attempts++<30) {
                while (ch<eof && *ch!=eol) ch++;
                if (ch<eof) ch+=eolLen;
                const char *thisStart = ch;
                int i = 0, thisNcol=0;
                while (ch<eof && i<5 && ( (thisNcol=countfields())==ncol || (thisNcol==0 && (skipEmptyLines || fill)))) i++;
                ch = thisStart;
                if (i==5) break;
            }
            if (ch>=eof || attempts>=30) {
                if (verbose) Rprintf("Couldn't find 5 good lines from jump point %d. Not using it.\n", j);
                continue;
            }
        }
        int i = 0;
        while(i<eachNrows && ch<eof) {  // Test 100 lines from each of the 10 points in the file
            if (sep==' ') while (ch<eof && *ch==' ') ch++;  // special for sep==' '. Correct not to be skip_white().
            skip_white(&ch);
            if (ch<eof && *ch==eol) {
              ch += eolLen;
              if (skipEmptyLines || fill) continue;
              else break;
            }
            i++;
            lineStart = ch;
            int field=0;
            const char *fieldStart=ch;  // Needed outside loop for error messages below
            while (ch<eof && *ch!=eol && field<ncol) {
                //Rprintf("Field %d: <<%.*s>>\n", field+1, STRLIM(ch,20), ch);
                fieldStart=ch;
                while (type[field]<=SXP_STR && !(*fun[type[field]])(NULL,0)) {
                  ch=fieldStart;
                  if (type[field]<SXP_STR) type[field]++;
                  else {
                    // the field couldn't be read with this quote rule, try again with next one
                    // Trying the next rule will only be successful if the number of fields is consistent with it
                    if (quoteRule<3) {
                      if (verbose) Rprintf("Bumping quote rule from %d to %d due to field %d on line %d starting <<%.*s>>\n",
                                   quoteRule, quoteRule+1, field+1, line+i-1, STRLIM(fieldStart,200), fieldStart);
                      quoteRule++;
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
                else STOP("Line has finished early when detecting types. Try fill=TRUE to pad with NA. Expecting %d fields but found %d: <<%.*s>>", ncol, field+1, STRLIM(lineStart,200), lineStart);
            }
            if (ch<eof) {
                if (*ch!=eol) {
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
        }
        if (verbose) { Rprintf("Type codes (point %2d): ",j); for (i=0; i<ncol; i++) Rprintf("%d",type[i]); Rprintf("\n"); }
    }
    ch = pos;
    
    // ********************************************************************************************
    //   Apply colClasses, select and integer64
    // ********************************************************************************************
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
    if (verbose) { Rprintf("Type codes: "); for (int i=0; i<ncol; i++) Rprintf("%d",type[i]); Rprintf(" (after applying colClasses and integer64)\n"); }
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
    if (verbose) { Rprintf("Type codes: "); for (int i=0; i<ncol; i++) Rprintf("%d",type[i]); Rprintf(" (after applying drop or select, if supplied)\n"); }
    clock_t tColType = clock();
    
    // ********************************************************************************************
    // Allocate columns for known nrow
    // ********************************************************************************************
    if (verbose) Rprintf("Allocating %d column slots (%d - %d dropped)\n", ncol-numNULL, ncol, numNULL);
    ans=PROTECT(allocVector(VECSXP,ncol-numNULL));  // safer to leave over allocation to alloc.col on return in fread.R
    protecti++;
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
        SEXP thiscol = allocVector(TypeSxp[ type[i] ], nrow);
        SET_VECTOR_ELT(ans,resi++,thiscol);  // no need to PROTECT thiscol, see R-exts 5.9.1
        if (type[i]==SXP_INT64) setAttrib(thiscol, R_ClassSymbol, ScalarString(char_integer64));
        SET_TRUELENGTH(thiscol, nrow);
    }
    clock_t tAlloc = clock();
    
    // ********************************************************************************************
    //   Read the data
    // ********************************************************************************************
    tCoerce = tCoerceAlloc = 0;
    ch = pos;   // back to start of first data row
    ERANGEwarning = TRUE;
    clock_t nexttime = t0+2*CLOCKS_PER_SEC;  // start printing % done after a few seconds. If doesn't appear then you know mmap is taking a while.
                                             // We don't want to be bothered by progress meter for quick tasks
    Rboolean hasPrinted=FALSE, whileBreak=FALSE;
    int numTypeFails=0, numTypeFailCols=0;
    int i = 0;
    while (i<nrow && ch<eof) {
        if (showProgress && clock()>nexttime) {
            Rprintf("\rRead %.1f%% of %d rows", (100.0*i)/nrow, nrow);   // prints straight away if the mmap above took a while, is the idea
            R_FlushConsole();    // for Windows
            nexttime = clock()+CLOCKS_PER_SEC;
            hasPrinted = TRUE;
        }
        R_CheckUserInterrupt();
        int batchend = MIN(i+10000, nrow);    // batched into 10k rows to save (expensive) calls to clock()
        while(i<batchend && ch<eof) {
            //Rprintf("Row %d : %.10s\n", i+1, ch);
            if (sep==' ') while (ch<eof && *ch==' ') ch++;  // correct not to be skip_white()
            if (ch>=eof || *ch==eol) {
                if (skipEmptyLines) { ch+=eolLen; continue; }
                else if (!fill) {
                    whileBreak = TRUE;  // break the enclosing while too, without changing i
                    break;              // break this while
                }
            }
            int j=-1, resj=-1;
            while (++j<ncol) {
                // Rprintf("Field %d: '%.10s' as type %d\n", j+1, ch, type[j]);
                const char *start = ch;
                SEXP thiscol = (type[j]==SXP_NULL) ? NULL : VECTOR_ELT(ans, ++resj);
                if (!(*fun[type[j]])(thiscol, i)) {
                  numTypeFails++;
                  if (typeOk[j]) {
                    numTypeFailCols++;
                    if (hasPrinted) Rprintf("\n");
                    Rprintf("Column %d ('%s') was guessed at type '%s' but row %d contains <<%.*s>>.\n",
                            j+1, CHAR(STRING_ELT(names,j)), UserTypeName[type[j]], i+1, (int)(ch-start), start);
                    typeOk[j] = 0;
                  }
                }
                ch += (ch<eof && *ch!=eol);
            }
            
            if (j<ncol)  {
              // not enough columns observed
              if (!fill) STOP("Expecting %d cols but line %d contains only %d cols (sep='%c'). Consider fill=TRUE or perhaps quote=''. Line %d starts: <<%.*s>>",
                              ncol, line, j, sep, line, STRLIM(lineStart, 500), lineStart);
              for (; resj<LENGTH(ans); resj++) {
                SEXP thiscol = VECTOR_ELT(ans, resj);
                switch (type[j]) {
                case SXP_LGL:
                  LOGICAL(thiscol)[i] = NA_LOGICAL;
                  break;
                case SXP_INT:
                  INTEGER(thiscol)[i] = NA_INTEGER;
                  break;
                case SXP_INT64:
                  REAL(thiscol)[i] = NA_INT64_D;
                  break;
                case SXP_REAL:
                  REAL(thiscol)[i] = NA_REAL;
                  break;
                case SXP_STR:
                  SET_STRING_ELT(thiscol, i, NA_STRING);   // or R_BlankString? or put the ternary right here depending on nastrings
                }
              }
            }
            if (ch<eof && *ch!=eol) STOP("Too many fields on line XXXX when reading");
            ch+=eolLen;
            pos = ch;  // start of line position only needed to include the whole line in any error message
            line++;
            i++;
        }
        if (whileBreak) break;
    }
    if (numTypeFails) STOP("The guessed column type was insufficient for %d values in %d columns. The first in each column was printed to the console. Use colClasses to set these column classes manually.", numTypeFails, numTypeFailCols);
    if (showProgress && hasPrinted) {
        j = 1+(clock()-t0)/CLOCKS_PER_SEC;
        Rprintf("\rRead %d rows and %d (of %d) columns from %.3f GB file in %02d:%02d:%02d\n", i, ncol-numNULL, ncol, 1.0*filesize/(1024*1024*1024), j/3600, (j%3600)/60, j%60);
        R_FlushConsole();
    }
    clock_t tRead = clock();
    
    // Warn about any non-whitespace not read at the end
    while (ch<eof && isspace(*ch)) ch++;
    if (ch<eof) {
        ch2 = ch;
        while (ch2<eof && *ch2!=eol) ch2++;
        if (INTEGER(nrowsarg)[0] == -1 || i < nrow) warning("Stopped reading at empty line %d but text exists afterwards (discarded): <<%.*s>>", line, ch2-ch, ch);
    }
    if (i<nrow) {
        // the condition above happens usually when the file contains many newlines. This is not necesarily something to be worried about. I've therefore commented the warning part, and retained the verbose message. If there are cases where lines don't get read in, we can revisit this warning. Fixes #1116.
        // if (nrow-i > 100 && (double)i/nrow < 0.95)
            // warning("Read less rows (%d) than were allocated (%d). Run again with verbose=TRUE and please report.",i,nrow);
        // else if (verbose)
        if (verbose)
            Rprintf("Read fewer rows (%d) than were allocated (%d).\n", i, nrow);
        nrow = i;
    } else {
        if (i!=nrow) STOP("Internal error: i [%d] > nrow [%d]", i, nrow);
        if (verbose) Rprintf("Read %d rows. Exactly what was estimated and allocated up front\n", i);
    }
    for (j=0; j<ncol-numNULL; j++) SETLENGTH(VECTOR_ELT(ans,j), nrow);
    
    // ********************************************************************************************
    //   Convert na.strings to NA for character columns
    // ********************************************************************************************
    for (k=0; k<length(nastrings); k++) {
        thisstr = STRING_ELT(nastrings,k);
        for (j=0; j<ncol-numNULL; j++) {
            SEXP thiscol = VECTOR_ELT(ans,j);
            if (TYPEOF(thiscol)==STRSXP) {
                for (i=0; i<nrow; i++)
                    if (STRING_ELT(thiscol,i)==thisstr) SET_STRING_ELT(thiscol, i, NA_STRING);
            }
        }
    }
    if (verbose) {
        clock_t tn = clock(), tot=tn-t0;
        if (tot<1) tot=1;  // to avoid nan% output in some trivial tests where tot==0
        Rprintf("%8.3fs (%3.0f%%) Memory map (rerun may be quicker)\n", 1.0*(tMap-t0)/CLOCKS_PER_SEC, 100.0*(tMap-t0)/tot);
        Rprintf("%8.3fs (%3.0f%%) sep, ncol and header detection\n", 1.0*(tLayout-tMap)/CLOCKS_PER_SEC, 100.0*(tLayout-tMap)/tot);
        Rprintf("%8.3fs (%3.0f%%) Count rows (wc -l)\n", 1.0*(tRowCount-tLayout)/CLOCKS_PER_SEC, 100.0*(tRowCount-tLayout)/tot);
        Rprintf("%8.3fs (%3.0f%%) Column type detection (100 rows at 10 points)\n", 1.0*(tColType-tRowCount)/CLOCKS_PER_SEC, 100.0*(tColType-tRowCount)/tot);
        Rprintf("%8.3fs (%3.0f%%) Allocation of %dx%d result (xMB) in RAM\n", 1.0*(tAlloc-tColType)/CLOCKS_PER_SEC, 100.0*(tAlloc-tColType)/tot, nrow, ncol);
        Rprintf("%8.3fs (%3.0f%%) Reading data\n", 1.0*(tRead-tAlloc-tCoerce)/CLOCKS_PER_SEC, 100.0*(tRead-tAlloc-tCoerce)/tot);
        Rprintf("%8.3fs (%3.0f%%) Changing na.strings to NA\n", 1.0*(tn-tRead)/CLOCKS_PER_SEC, 100.0*(tn-tRead)/tot);
        Rprintf("%8.3fs        Total\n", 1.0*tot/CLOCKS_PER_SEC);
    }
    UNPROTECT(protecti);
    closeFile();
    return(ans);
}

