#include "data.table.h"
#include <Rdefines.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

#ifdef WIN32         // means WIN64, too
#include <windows.h>
#include <stdio.h>
#include <tchar.h>
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
colClasses shouldn't be ignored but rather respected and then warn if data accuracy is lost. See first NOTE in NEWS.
Detect and coerce dates and times. By searching for - and :, and dateTtime etc, or R's own method or fasttime. POSIXct default, for microseconds? : http://stackoverflow.com/questions/14056370/cast-string-to-idatetime
Fill in too-short lines :  http://stackoverflow.com/questions/21124372/fread-doesnt-like-lines-with-less-fields-than-other-lines
Allow to increase from 100 rows at 10 points
madvise is too eager when reading just the top 10 rows.
Add as.colClasses to fread.R after return from C level (e.g. for colClasses "Date", although as slow as read.csv via character)
Allow comment char to ignore. Important in format detection. But require valid line data before comment character in the read loop? See http://stackoverflow.com/a/18922269/403310
Jim Holtman requested column names be stipped of whitespace.  On datatable-help 22 Jan 2014.
Deal with row.names e.g. http://stackoverflow.com/questions/15448732/reading-csv-with-row-names-by-fread
Test Garrett's two files again (wrap around ,,,,,, and different row lengths that the wc -l now fixes)
Post from patricknik on 5 Jan re ""b"" in a field. And Aykut Firat on email.
Warn about non whitespace (unprotected by comment.char) after the last column on any line (currently skipped silently)
Warning about any imperfect number of columns
Check tests exist for logical columns (currently read as character). T/True/TRUE/true are allowed in main/src/util.c
A few TO DO inline in the code, including some speed fine tuning e.g. specialize Ispace and any other lib calls.
Save repeated ch<eof checking in main read step. Last line might still be tricky if last line has no eol.
test using at least "grep read.table ...Rtrunk/tests/
---
Secondary separator for list() columns, such as columns 11 and 12 in BED (no need for strsplit).
If nrow is small, say 20, then it shouldn't mmap the entire file (that's probably *why* user just wants to look at head). Try MMAP_DONTNEED in that case to save needing to map the file in chunks.
Add LaF comparison.
as.read.table=TRUE/FALSE option.  Or fread.table and fread.csv (see http://r.789695.n4.nabble.com/New-function-fread-in-v1-8-7-tp4653745p4654194.html).

*****/

static const char *ch, *eof; 
static char sep, eol, eol2;  // sep2 TO DO
static int eolLen, line, field;
static Rboolean verbose, ERANGEwarning;
static clock_t tCoerce, tCoerceAlloc;

// Define our own fread type codes, different to R's SEXPTYPE :
// i) INTEGER64 is not in R but an add on packages using REAL, we need to make a distinction here, without using class (for speed)
// ii) 0:n codes makes it easier to bump through types in this order using ++.
#define SXP_LGL    0   // LGLSXP    String values T,F,TRUE,FALSE,True,False
#define SXP_INT    1   // INTSXP
#define SXP_INT64  2   // REALSXP
#define SXP_REAL   3   // REALSXP
#define SXP_STR    4   // STRSXP
#define SXP_NULL   5   // NILSXP i.e. skip column (last so that all types can be bumped up to it by user)
static const char TypeName[6][10] = {"LGL","INT","INT64","REAL","STR","NULL"};  // for messages and errors
static int TypeSxp[6] = {LGLSXP,INTSXP,REALSXP,REALSXP,STRSXP,NILSXP};
static union {double d; long long l; int b;} u;   // b=boolean, can hold NA_LOGICAL
static const char *fieldStart, *fieldEnd;
static int fieldLen;
#define NUT        8   // Number of User Types (just for colClasses where "numeric"/"double" are equivalent)
static const char UserTypeName[NUT][10] = {"logical", "integer", "integer64", "numeric", "character", "NULL", "double", "CLASS" };  // important that first 6 correspond to TypeName.  "CLASS" is the fall back to character then as.class at R level ("CLASS" string is just a placeholder).
static int UserTypeNameMap[NUT] = { SXP_LGL, SXP_INT, SXP_INT64, SXP_REAL, SXP_STR, SXP_NULL, SXP_REAL, SXP_STR };
// quote
const char *quote;
static int quoteStatus, stripWhite;

const char *fnam=NULL, *mmp;
size_t filesize;
#ifdef WIN32
HANDLE hFile=0;
HANDLE hMap=0;
void closeFile() {
    if (fnam!=NULL) {
        UnmapViewOfFile(mmp);
        CloseHandle(hMap);
        CloseHandle(hFile);
    }
}
#else
int fd=-1;
void closeFile() {    
    if (fnam!=NULL) {
        munmap((char *)mmp, filesize);
        close(fd);
    }
}
#endif
// To solve: http://stackoverflow.com/questions/18597123/fread-data-table-locks-files
void STOP(const char *format, ...) {
    va_list args;
    va_start(args, format);
    char msg[2000];
    vsnprintf(msg, 2000, format, args);
    va_end(args);
    closeFile();  // some errors point to data in the file, hence via msg buffer first
    error(msg);
}
// ********************************************************************************************
// NA handling.
// algorithm is following
// 1) Strto*() checks whether we can convert substring into given * type
// 2) If not, we try to iteratively char-by-char starting from begining of the substring 
// look forward for maximum max(nchar(nastrings)) symbols:
// ********************************************************************************************
// max_na_nchar = max(nchar(nastrings))
// lch = pointer to begining of substring we want to check
// for (i in 0:max_na_nchar) {
//   ch = lch[i]
//   if( any of nastrings contain ch at position i)
//      continue
//   else return FALSE
// }
// return TRUE
// ********************************************************************************************
// for checking "if" condition we manage mask "na_mask" with length na_len = length(nastrings). 
// 1 on mask position i means that nastring[i] is still candidate for given substring.
// 0 means this substring can't be casted into nastring[i], so nastring[i] is not candidate.
int *NA_MASK;
// means nastrings == 0; will do nothing with nastrings
int FLAG_NA_STRINGS_NULL;
const char **NA_STRINGS;
int NA_MAX_NCHAR;
int NASTRINGS_LEN;
int *EACH_NA_STRING_LEN;
// calculates maximum string length for a given R character vector
int get_maxlen(SEXP char_vec) {
  int maxlen = -1;
  int cur_len;
  for (int i=0; i< LENGTH(char_vec); i++) {
    cur_len = strlen(CHAR(STRING_ELT(char_vec, i)));
    maxlen = (cur_len > maxlen) ? cur_len : maxlen;
  }
  return maxlen;
}
// initialize mask. At the begining we assume any nastring can be candidate.
static inline void init_mask() {
  for(int i = 0; i < NASTRINGS_LEN; i++)
    NA_MASK[i] = 1;
}
static inline int can_cast_to_na(const char* lch) {
  const char *lch2 = lch;
  // nastrings==NULL => do nothing
  // TODO: move this out of this function and into strto* functions directly?
  if(FLAG_NA_STRINGS_NULL) {
    return 0;
  }
  init_mask();
  // check whether mask contains any candidates which still potentially can be casted to NA
  int non_zero_left = NASTRINGS_LEN;
  //case when lch is empty string!
  int na_found_flag = 1;
  int pos = 0;
  int j;
  const char *nastring_iter;
  // look for possible NA strings:
  // max forward symbols = max length of the nastring template
  while (pos < NA_MAX_NCHAR && lch2 != eof && *lch2 != sep && *lch2 != eol) {
    j = 0;
    na_found_flag = 0;
    // check whether any of the nastrings template contains current (i-th forward) symbol at i-th forward positions
    // iterate through nastrings
    while( j < NASTRINGS_LEN && non_zero_left > 0 ) {
      // not marked before
      if( NA_MASK[j] != 0) {
        nastring_iter = NA_STRINGS[j];
        // nastring[j] candidate founded
        if(EACH_NA_STRING_LEN[j] == pos + 1 && nastring_iter[pos] == *lch2) {
          na_found_flag = 1;
        }
        // nastring[j] candidate has smaller length than we are looking
        // or doesn't contain necessary symbol at position we are cheking
        if(EACH_NA_STRING_LEN[j] < pos + 1 || nastring_iter[pos] != *lch2) {
          NA_MASK[j] = 0;
          non_zero_left--;
        }
      }
      j++;
    }
    //all elements of mask == 0 means we can't convert tested substring to NA
    if(non_zero_left == 0) {
      return 0;
    }
    pos++;
    lch2++;
  }
  // found delimiter after right NA candidate or empty string
  if(na_found_flag && (lch2 == eof || *lch2 == sep || *lch2 == eol)) {
    ch = lch2;
    return 1;
  }
  else return 0;
}
// ********************************************************************************************

static inline void skip_spaces() {
    while(ch<eof && *ch==' ') ch++;
}

static inline void Field()
{
    quoteStatus=0;
    if (*ch==quote[0]) { // protected, now look for the next ", so long as it doesn't leave unbalanced unquoted regions
        quoteStatus=1;
        fieldStart = ch+1;
        int eolCount=0;  // just >0 is used currently but may as well count
        Rboolean noEmbeddedEOL=FALSE, quoteProblem=FALSE;
        while(++ch<eof) {
            if (*ch!=quote[0]) {
                if (noEmbeddedEOL && *ch==eol) { quoteProblem=TRUE; break; }
                eolCount+=(*ch==eol);
                continue;  // fast return in most cases of characters
            }
            if (ch+1==eof || *(ch+1)==sep || *(ch+1)==eol) break;
            // " followed by sep|eol|eof dominates a field ending with \" (for support of Windows style paths)
            
            if (*(ch-1)!='\\') {
                if (ch+1<eof && *(ch+1)==quote[0]) { ch++; continue; }  // skip doubled-quote
                // unescaped subregion
                if (eolCount) {ch++; quoteProblem=TRUE; break;}
                // *ch!=sep needed for detecting cases mentioned in SO post under #1462
                while (++ch<eof && (*ch!=quote[0] || *(ch-1)=='\\') && *ch!=eol && *ch!=sep);
                if (ch==eof || *ch==eol || *ch==sep) {quoteProblem=TRUE; break;}
                noEmbeddedEOL = 1;
            } else if (ch+1<eof && *(ch+1)==quote[0] && ch+2<eof && *(ch+2)!=sep) { ch++; continue; }
            // above else if is necessary for #1164. ch+2 condition is to take care of cases like <"blabla \"",> where sep=',' (test 1336.1)
        }
        if (quoteProblem || ch==eof) {
            // "..." logic has failed. Delegate to normal routine instead of erroring. Solves many cases, especially when files have both proper balanced, and imbalanced quotes. I think this is more towards fread's philosophy than having an explicit 'quote' argument..
            quoteStatus=0;
            ch = fieldStart-1;
        } else {
            fieldLen = (int)(ch-fieldStart);
            ch++; // from closing quote to rest on sep|eol|eof
        }
    }
    if (!quoteStatus) {
        // either "" logic failed and we're here, or we're directly here.
        // unprotected, look for next next [sep|eol]
        if (sep==' ') {
            fieldStart = ch;
            while(ch<eof && *ch!=sep && *ch!=eol) ch++;
            fieldLen = (int)(ch-fieldStart);
            if (stripWhite) {
                skip_spaces();
                if (ch<eof && *ch!=eol) ch--;  // leave on last space before new field's text
            }
        } else {
            fieldStart=ch;
            while(ch<eof && *ch!=sep && *ch!=eol) ch++;
            if (stripWhite) {
                fieldEnd=ch-1;
                while(fieldEnd>=fieldStart && *fieldEnd==' ') fieldEnd--;
                fieldLen = (int)(fieldEnd-fieldStart)+1;
            } else fieldLen = (int)(ch-fieldStart);
        }
    }
    // Rprintf("Processed field %.*s\n", (int)(ch-fieldStart), fieldStart);
}

static int countfields()
{
    int ncol=0;
    if (ch==eof) return 0;
    if (*ch==eol) { ch+=eolLen; return 0; }
    while (1) {
        ncol++;
        skip_spaces();
        Field();
        if (ch==eof || *ch==eol) {
            if (ch<eof) ch+=eolLen;          // move ch to the start of the next line
            return ncol;
        }
        if (*ch!=sep) STOP("Internal error: Field() has ended with '%c' not sep='%c'", *ch, sep);
        ch++;
    }
}

static inline Rboolean Strtoll()
{
    // Specialized strtoll that :
    // i) skips leading isspace(), too, but other than field separator and eol (e.g. '\t' and ' \t' in FUT1206.txt)
    // ii) has fewer branches for speed as no need for non decimal base
    // iii) updates global ch directly saving arguments
    // iv) safe for mmap which can't be \0 terminated on Windows (but can be on unix and mac)
    // v) fails if whole field isn't consumed such as "3.14" (strtol consumes the 3 and stops)
    // ... all without needing to read into a buffer at all (reads the mmap directly)
    const char *lch=ch; int sign=1;
    while (lch<eof && isspace(*lch) && *lch!=sep && *lch!=eol) lch++;
    if (lch==eof || *lch==sep || *lch==eol) {   //  ',,' or ',   ,' or '\t\t' or '\t   \t' etc => NA
        ch = lch;                               // advance global ch over empty field
        return(TRUE);                           // caller already set u.l=NA_INTEGR or u.d=NA_REAL as appropriate
    }
    // moved this if-statement to the top for #1314
    if(can_cast_to_na(lch)) { 
        // ch pointer already set to end of nastring by can_cast_to_na() function
        return(TRUE);
    }
    if (*lch=='-') { sign=0; lch++; if (*lch<'0' || *lch>'9') return(FALSE); }   // + or - symbols alone should be character
    else if (*lch=='+') { sign=1; lch++; if (*lch<'0' || *lch>'9') return(FALSE); }
    long long acc = 0;
    while (lch<eof && '0'<=*lch && *lch<='9' && acc<(LLONG_MAX-10)/10) { 
        acc *= 10;            // overflow avoided, thanks to BDR and CRAN's new ASAN checks, 25 Feb 2014
        acc += *lch-'0';      // have assumed compiler will optimize the constant expression (LLONG_MAX-10)/10
        lch++;                // TO DO can remove lch<eof when last row is specialized in case of no final eol
        
    }
    // take care of leading spaces
    while(lch<eof && *lch!=sep && *lch==' ') lch++;
    //int len = lch-start;
    //Rprintf("Strtoll field '%.*s' has len %d\n", lch-ch+1, ch, len);
    if (lch==eof || *lch==sep || *lch==eol) {   // only if field fully consumed (e.g. not ,123456A,)
        ch = lch;
        u.l = sign ? acc : -acc;
        return(TRUE);     // either int or int64 read ok, result in u.l.  INT_MIN and INT_MAX checked by caller, as appropriate
    }
    return(FALSE);  // invalid integer such as "3.14", "123ABC," or "12345678901234567890" (larger than even int64) => bump type.
}

static inline Rboolean readspecialvalue(const char *nptr, char **endptr, double *x, Rboolean positive)
{
   if (nptr[0] == 'I' && nptr[1] == 'N' && nptr[2] == 'F') { // INF
       nptr = nptr+3;
       *x = (positive == TRUE)?INFINITY:-INFINITY;
   } else if (nptr[0] == 'I' && nptr[1] == 'N' && nptr[2] == 'D') { // IND
       nptr = nptr+3;
       *x = (positive == TRUE)?NAN:-NAN;
   } else if (nptr[0] == 'S' && nptr[1] == 'N' && nptr[2] == 'A' && nptr[3] == 'N') { // SNAN
       nptr = nptr+4;
       *x = (positive == TRUE)?NAN:-NAN;
   } else if (nptr[0] == 'Q' && nptr[1] == 'N' && nptr[2] == 'A' && nptr[3] == 'N') { // QNAN    
       nptr = nptr+4;
       *x = (positive == TRUE)?NAN:-NAN;
   } else return(FALSE);

   // To handle something like 1.#INF00000
   while (*nptr == '0')
       nptr++;

   *endptr = (char *)nptr;
   return(TRUE);
}

static inline Rboolean check_win_special(const char *nptr, char **endptr, double *x)
{
    if (nptr[0] == '1' && nptr[1] == '.' && nptr[2] == '#')
        return readspecialvalue(nptr+3, endptr, x, TRUE);
    if (nptr[0] == '-' && nptr[1] == '1' && nptr[2] == '.' && nptr[3] == '#')
        return readspecialvalue(nptr+4, endptr, x, FALSE);
    return(FALSE);
}

static inline double strtod_wrapper(const char *nptr, char **endptr)
{
    double x;
    if (check_win_special(nptr, endptr, &x))
        return x;
    return strtod(nptr, endptr);
}

static inline double strtold_wrapper(const char *nptr, char **endptr)
{
    double x;
    if (check_win_special(nptr, endptr, &x))
        return x;
    return strtold(nptr, endptr);
}

static inline Rboolean Strtob()
{
    // String (T,F,True,False,TRUE or FALSE) to boolean.  These usually come from R when it writes out.
    const char *start=ch;
    // moved this if-statement to top for #1314
    if(can_cast_to_na(ch)) {
      u.b = NA_LOGICAL;
      // ch pointer already set to end of nastring by can_cast_to_na() function
      return(TRUE);
    } else if (*ch=='T') {
        u.b = TRUE;
        if (++ch==eof || *ch==sep || *ch==eol) return(TRUE);
        if (*ch=='R' && *++ch=='U' && *++ch=='E' && (++ch==eof || *ch==sep || *ch==eol)) return(TRUE);
        ch = start+1;
        if (*ch=='r' && *++ch=='u' && *++ch=='e' && (++ch==eof || *ch==sep || *ch==eol)) return(TRUE);
    }
    else if (*ch=='F') {
        u.b = FALSE;
        if (++ch==eof || *ch==sep || *ch==eol) return(TRUE);
        if (*ch=='A' && *++ch=='L' && *++ch=='S' && *++ch=='E' && (++ch==eof || *ch==sep || *ch==eol)) return(TRUE);
        ch = start+1;
        if (*ch=='a' && *++ch=='l' && *++ch=='s' && *++ch=='e' && (++ch==eof || *ch==sep || *ch==eol)) return(TRUE);
    }
    ch = start;
    return(FALSE);     // invalid boolean, need to bump type.
}


static int numDP(double *v, R_len_t n)
{
    // highly cut down version of formatReal() just for what we need, since formatReal is not in R's API and r-devel now prevents it
    char buffer[10];
    int maxdp=0, this;
    double intpart; // not used or needed (only interested in dp after .)
    for (R_len_t i=0; i<n && maxdp<6; i++) {
        if (!R_FINITE(v[i])) continue;
        snprintf(buffer, 9, "%.6f", modf(v[i],&intpart));  // 0.123456 => 6,  0.120000 => 2
        this=6;
        while (this>0 && buffer[this+1]=='0') this--;
        if (this>maxdp) maxdp=this;
    }
    return(maxdp);
}

static SEXP coerceVectorSoFar(SEXP v, int oldtype, int newtype, R_len_t sofar, R_len_t col)
{
    // Like R's coerceVector() but :
    // i) we only need to coerce elements up to the row read so far, for speed.
    // ii) we can directly change type of vectors without an allocation when the size of the data type doesn't change
    SEXP newv;
    R_len_t i, protecti=0;
    int dp;
    clock_t tCoerce0 = clock();
    const char *lch=ch;
    while (lch!=eof && *lch!=sep && *lch!=eol) lch++;  // lch now marks the end of field, used in verbose messages and errors
    if (verbose) Rprintf("Bumping column %d from %s to %s on data row %d, field contains '%.*s'\n",
                         col+1, TypeName[oldtype], TypeName[newtype], sofar+1, lch-ch, ch);
    if (sizes[TypeSxp[oldtype]]<4) STOP("Internal error: SIZEOF oldtype %d < 4", oldtype);
    if (sizes[TypeSxp[newtype]]<4) STOP("Internal error: SIZEOF newtype %d < 4", newtype);
    if (sizes[TypeSxp[oldtype]] == sizes[TypeSxp[newtype]] && newtype != SXP_STR) {   // after && is quick fix. TO DO: revisit
        SET_TYPEOF(v, TypeSxp[newtype]);  // SET_TYPEOF() not TYPEOF= for Karl Millar and rho.
        newv=v;
    } else {
        clock_t tCoerceAlloc0 = clock();
        PROTECT(newv = allocVector(TypeSxp[newtype], LENGTH(v)));
        protecti++;
        tCoerceAlloc += clock()-tCoerceAlloc0;
        // This was 1.3s (all of tCoerce) when testing on 2008.csv; might have triggered a gc, included.
        // Happily, mid read bumps are very rarely needed, due to testing types at the start, middle and end of the file, first.
    }
    setAttrib(newv, R_ClassSymbol, newtype==SXP_INT64 ? ScalarString(mkChar("integer64")) : R_NilValue);
    switch(newtype) {
    case SXP_INT :
        switch(oldtype) {
        case SXP_LGL :
            for (i=0; i<sofar; i++) INTEGER(newv)[i] = INTEGER(v)[i];
            break;
        default :
            STOP("Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
        }
        break;
    case SXP_INT64:
        switch(oldtype) {
        case SXP_LGL : case SXP_INT :
            for (i=0; i<sofar; i++) {
                REAL(newv)[i] = (INTEGER(v)[i]==NA_INTEGER ? (u.l=NAINT64,u.d) : (u.l=(long long)INTEGER(v)[i],u.d));
            }
            break;
        default :
            STOP("Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
        }
        break;
    case SXP_REAL:
        switch(oldtype) {
        case SXP_LGL : case SXP_INT :
            for (i=0; i<sofar; i++) REAL(newv)[i] = (INTEGER(v)[i]==NA_INTEGER ? NA_REAL : (double)INTEGER(v)[i]);
            break;
        case SXP_INT64 :
            for (i=0; i<sofar; i++) REAL(newv)[i] = ((*(long long *)&REAL(v)[i] == NAINT64) ? NA_REAL : (double)(*(long long *)&REAL(v)[i]));
            break;
        default :
            STOP("Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
        }
        break;
    case SXP_STR:
        warning("Bumped column %d to type character on data row %d, field contains '%.*s'. Coercing previously read values in this column from logical, integer or numeric back to character which may not be lossless; e.g., if '00' and '000' occurred before they will now be just '0', and there may be inconsistencies with treatment of ',,' and ',NA,' too (if they occurred in this column before the bump). If this matters please rerun and set 'colClasses' to 'character' for this column. Please note that column type detection uses a sample of 1,000 rows (100 rows at 10 points) so hopefully this message should be very rare. If reporting to datatable-help, please rerun and include the output from verbose=TRUE.\n", col+1, sofar+1, lch-ch, ch);
        static char buffer[129];  // 25 to hold [+-]2^63, with spare space to be safe and snprintf too
        switch(oldtype) {
        case SXP_LGL : case SXP_INT :
            // This for loop takes 0.000007 seconds if the bump occurs on line 179, for example, timing showed
            for (i=0; i<sofar; i++) {
                if (INTEGER(v)[i] == NA_INTEGER)
                    SET_STRING_ELT(newv,i,R_BlankString);
                else {
                    snprintf(buffer,128,"%d",INTEGER(v)[i]);
                    SET_STRING_ELT(newv, i, mkChar(buffer));
                }
            }
            break;
        case SXP_INT64 :
            for (i=0; i<sofar; i++) {
                if (ISNA(REAL(v)[i]))
                    SET_STRING_ELT(newv,i,R_BlankString);
                else {
                    #ifdef WIN32
                        snprintf(buffer,128,"%I64d",*(long long *)&REAL(v)[i]);
                    #else
                       snprintf(buffer,128,"%lld",*(long long *)&REAL(v)[i]);
                    #endif
                    SET_STRING_ELT(newv, i, mkChar(buffer));
                }
            }
            break;
        case SXP_REAL :
            dp = numDP(REAL(v), sofar);  // would use formatReal() but it's not in R's API and r-devel now prevents it
            for (i=0; i<sofar; i++) {
                if (ISNA(REAL(v)[i]))
                    SET_STRING_ELT(newv,i,R_BlankString);
                else {
                    snprintf(buffer,128,"%.*f",dp,REAL(v)[i]);
	                SET_STRING_ELT(newv, i, mkChar(buffer));
	            }
            }
            break;
        default :
            STOP("Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
        }
        break;
    default :
        STOP("Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
    }
    UNPROTECT(protecti);
    tCoerce += clock()-tCoerce0;
    return(newv);
}

// These defines recreate the original source code
#define TEMPLATE_Strtod Strtod
#define TEMPLATE_strtod strtod
#define TEMPLATE_strtold strtold
#define TEMPLATE_readfile readfile
#include "fread_readfile_template.h"
#undef TEMPLATE_Strtod
#undef TEMPLATE_strtod 
#undef TEMPLATE_strtold 
#undef TEMPLATE_readfile 

// This causes a new function 'readfile_vsinfnan' to be created which uses
// strtod_wrapper and strtold_wrapper, so that e.g. 1.#INF and 1.#IND, used
// by the VC compiler, are interpreted as infinity and NaN.
#define TEMPLATE_Strtod Strtod_vsinfnan
#define TEMPLATE_strtod strtod_wrapper
#define TEMPLATE_strtold strtold_wrapper
#define TEMPLATE_readfile readfile_vsinfnan
#include "fread_readfile_template.h"

