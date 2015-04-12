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
Allow to increase to top 500, middle 500 and bottom 500.
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
static const char *fieldStart;
static int fieldLen;
#define NUT        8   // Number of User Types (just for colClasses where "numeric"/"double" are equivalent)
static const char UserTypeName[NUT][10] = {"logical", "integer", "integer64", "numeric", "character", "NULL", "double", "CLASS" };  // important that first 6 correspond to TypeName.  "CLASS" is the fall back to character then as.class at R level ("CLASS" string is just a placeholder).
static int UserTypeNameMap[NUT] = { SXP_LGL, SXP_INT, SXP_INT64, SXP_REAL, SXP_STR, SXP_NULL, SXP_REAL, SXP_STR };

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

static inline void Field(int err)
{
    if (*ch=='\"') { // protected, now look for the next ", so long as it doesn't leave unbalanced unquoted regions
        fieldStart = ch+1;
        int eolCount=0;  // just >0 is used currently but may as well count
        Rboolean noEmbeddedEOL=FALSE, quoteProblem=FALSE;
        while(++ch<eof) {
            if (*ch!='\"') {
                if (noEmbeddedEOL && *ch==eol) { quoteProblem=TRUE; break; }
                eolCount+=(*ch==eol);
                continue;  // fast return in most cases of characters
            }
            
            if (ch+1==eof || *(ch+1)==sep || *(ch+1)==eol) break;
            // " followed by sep|eol|eof dominates a field ending with \" (for support of Windows style paths)
            
            if (*(ch-1)!='\\') {
                if (ch+1<eof && *(ch+1)=='\"') { ch++; continue; }  // skip doubled-quote
                // unescaped subregion
                if (eolCount) {ch++; quoteProblem=TRUE; break;}
                while (++ch<eof && (*ch!='\"' || *(ch-1)=='\\') && *ch!=eol);
                if (ch==eof || *ch==eol) {quoteProblem=TRUE; break;}
                noEmbeddedEOL = 1;
            }
        }
        if (quoteProblem || ch==eof) {
            if (!err) {
                fieldLen = -1; // so that countfields/separator testing knows line is invalid
                return;
            }
            STOP("Field %d on line %d starts with quote (\") but then has a problem. It can contain balanced unescaped quoted subregions but if it does it can't contain embedded \\n as well. Check for unbalanced unescaped quotes: %.*s", field+1, line, ch-fieldStart+1, fieldStart-1);
        }
        fieldLen = (int)(ch-fieldStart);
        ch++; // from closing quote to rest on sep|eol|eof
    } else {           // unprotected, look for next next [sep|eol]
        if (sep==' ') {
            while(ch<eof && *ch==sep) ch++;
            fieldStart = ch;
            while(ch<eof && *ch!=sep && *ch!=eol) ch++;
            fieldLen = (int)(ch-fieldStart);
            while(ch<eof && *ch==sep) ch++;
            if (ch<eof && *ch!=eol) ch--;  // leave on last space before new field's text
        } else {
            fieldStart = ch;
            while(ch<eof && *ch!=sep && *ch!=eol) ch++;
            fieldLen = (int)(ch-fieldStart);
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
        Field(0);
        if (fieldLen==-1) {
            while(ch<eof && *ch!=eol) ch++;  // advance to end of line (or next embedded \n - which we now don't know if really embedded)
            if (ch<eof) ch+=eolLen;          // move ch to the start of the next line
            return -1;                       // see test 1010
        }  
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
    const char *start = lch;                          // start of [+-][0-9]+
    if (*lch=='-') { sign=0; lch++; if (*lch<'0' || *lch>'9') return(FALSE); }   // + or - symbols alone should be character
    else if (*lch=='+') { sign=1; lch++; if (*lch<'0' || *lch>'9') return(FALSE); }
    long long acc = 0;
    while (lch<eof && '0'<=*lch && *lch<='9' && acc<(LLONG_MAX-10)/10) { 
        acc *= 10;            // overflow avoided, thanks to BDR and CRAN's new ASAN checks, 25 Feb 2014
        acc += *lch-'0';      // have assumed compiler will optimize the constant expression (LLONG_MAX-10)/10
        lch++;                // TO DO can remove lch<eof when last row is specialized in case of no final eol
    }
    int len = lch-start;
    //Rprintf("Strtoll field '%.*s' has len %d\n", lch-ch+1, ch, len);
    if (lch==eof || *lch==sep || *lch==eol) {   // only if field fully consumed (e.g. not ,123456A,)
        ch = lch;
        u.l = sign ? acc : -acc;
        return(TRUE);     // either int or int64 read ok, result in u.l.  INT_MIN and INT_MAX checked by caller, as appropriate
    }
    if (len==0 && lch<eof-1 && *lch++=='N' && *lch++=='A' && (lch==eof || *lch==sep || *lch==eol)) {   // ',NA,'
        ch = lch;       // advance over NA
        return(TRUE);   // NA_INTEGER or NA_REAL (for SXP_INT64) was already set by caller
    }
    return(FALSE);  // invalid integer such as "3.14", "123ABC," or "12345678901234567890" (larger than even int64) => bump type.
}

static inline Rboolean Strtod()
{
    // Specialized strtod for same reasons as Strtoll (leading \t dealt with), but still uses stdlib:strtod (hard to do better, unlike strtoll).
    // R's R_Strtod5 uses strlen() on input, so we can't use that here unless we copy field to a buffer (slow)
    // Could fork glibc's strtod and change it to pass in dec rather than via locale but see :
    //    http://www.exploringbinary.com/how-glibc-strtod-works/
    // i.e. feel more comfortable relying on glibc for speed and robustness (and more eyes have been on it)
    // Since French_France.1252 is always available on Windows, should not be an issue there: fread() automates the locale change and revert.
    // On unix, fr_FR.utf8 may need to be installed.
    const char *lch=ch;
    while (lch<eof && isspace(*lch) && *lch!=sep && *lch!=eol) lch++;
    if (lch==eof || *lch==sep || *lch==eol) {u.d=NA_REAL; ch=lch; return(TRUE); }  // e.g. ',,' or '\t\t'
    const char *start=lch;
    errno = 0;
    u.d = strtod(start, (char **)&lch);
    if (errno==0 && lch>start && (lch==eof || *lch==sep || *lch==eol)) {
        ch = lch;
        return(TRUE);  // double read ok (result in u.d). Done. Most common case.
    }
    if (errno==ERANGE && lch>start) {
        lch = start;
        errno = 0;
        u.d = (double)strtold(start, (char **)&lch);
        if (errno==0 && lch>start && (lch==eof || *lch==sep || *lch==eol)) {
            ch = lch;
            if (ERANGEwarning) {
                warning("C function strtod() returned ERANGE for one or more fields. The first was string input '%.*s'. It was read using (double)strtold() as numeric value %.16E (displayed here using %%.16E); loss of accuracy likely occurred. This message is designed to tell you exactly what has been done by fread's C code, so you can search yourself online for many references about double precision accuracy and these specific C functions. You may wish to use colClasses to read the column as character instead and then coerce that column using the Rmpfr package for greater accuracy.", lch-start, start, u.d);
                ERANGEwarning = FALSE;   // once only. Set to TRUE just before read data loop. FALSE initially when detecting types.
                // This is carefully worded as an ERANGE warning because that's precisely what it is.  Calling it a 'precision' warning
                // might lead the user to think they'll get a precision warning on "1.23456789123456789123456789123456789" too, but they won't
                // as that will be read fine by the first strtod() with silent loss of precision. IIUC.
            }
            return(TRUE);
        }
    }
    if (lch==start && lch<eof-1 && *lch++=='N' && *lch++=='A' && (lch==eof || *lch==sep || *lch==eol)) {
        ch = lch;
        u.d = NA_REAL;
        return(TRUE);
    }
    return(FALSE);     // invalid double, need to bump type.
}

static inline Rboolean Strtob()
{
    // String (T,F,True,False,TRUE or FALSE) to boolean.  These usually come from R when it writes out.
    const char *start=ch;
    if (*ch=='T') {
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
    else if (ch==eof || *ch==sep || *ch==eol ||
             (ch<eof-1 && *ch=='N' && *++ch=='A' && (++ch==eof || *ch==sep || *ch==eol))) {
        u.b = NA_LOGICAL;
        return(TRUE);
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
        TYPEOF(v) = TypeSxp[newtype];
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
        warning("Bumped column %d to type character on data row %d, field contains '%.*s'. Coercing previously read values in this column from logical, integer or numeric back to character which may not be lossless; e.g., if '00' and '000' occurred before they will now be just '0', and there may be inconsistencies with treatment of ',,' and ',NA,' too (if they occurred in this column before the bump). If this matters please rerun and set 'colClasses' to 'character' for this column. Please note that column type detection uses the first 5 rows, the middle 5 rows and the last 5 rows, so hopefully this message should be very rare. If reporting to datatable-help, please rerun and include the output from verbose=TRUE.\n", col+1, sofar+1, lch-ch, ch);
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

SEXP readfile(SEXP input, SEXP separg, SEXP nrowsarg, SEXP headerarg, SEXP nastrings, SEXP verbosearg, SEXP autostart, SEXP skip, SEXP select, SEXP drop, SEXP colClasses, SEXP integer64, SEXP dec, SEXP showProgressArg)
// can't be named fread here because that's already a C function (from which the R level fread function took its name)
{
    SEXP thiscol, ans, thisstr;
    R_len_t i, resi, j, resj, k, protecti=0, nrow=0, ncol=0;
    int thistype;
    const char *pos, *ch2, *lineStart;
    Rboolean header, allchar;
    verbose=LOGICAL(verbosearg)[0];
    clock_t t0 = clock();
    ERANGEwarning = FALSE;  // just while detecting types, then TRUE before the read data loop
    if (!isInteger(showProgressArg) || LENGTH(showProgressArg)!=1 || (INTEGER(showProgressArg)[0]!=0 && INTEGER(showProgressArg)[0]!=1))
        error("showProgress must be 0 or 1, currently");
    int showProgress = INTEGER(showProgressArg)[0];
    if (!isString(dec) || LENGTH(dec)!=1 || strlen(CHAR(STRING_ELT(dec,0))) != 1)
        error("dec must be a single character");
    const char decChar = *CHAR(STRING_ELT(dec,0));
    
    fnam = NULL;  // reset global, so STOP() can call closeFile() which sees fnam

    if (NA_INTEGER != INT_MIN) error("Internal error: NA_INTEGER (%d) != INT_MIN (%d).", NA_INTEGER, INT_MIN);  // relied on by Stroll
    if (sizeof(double) != 8) error("Internal error: sizeof(double) is %d bytes, not 8.", sizeof(double));
    if (sizeof(long long) != 8) error("Internal error: sizeof(long long) is %d bytes, not 8.", sizeof(long long));
    
    // ********************************************************************************************
    //   Check inputs.
    // ********************************************************************************************
    
    if (!isLogical(headerarg) || LENGTH(headerarg)!=1) error("'header' must be 'auto', TRUE or FALSE"); // 'auto' was converted to NA at R level
    header = LOGICAL(headerarg)[0];
    if (!isNull(nastrings) && !isString(nastrings)) error("'na.strings' is type '",type2char(TYPEOF(nastrings)),"'. Must be a character vector.");
    if (!isInteger(nrowsarg) || LENGTH(nrowsarg)!=1 || INTEGER(nrowsarg)[0]==NA_INTEGER) error("'nrows' must be a single non-NA number of type numeric or integer");
    if (!isInteger(autostart) || LENGTH(autostart)!=1 || INTEGER(autostart)[0]<0) error("'autostart' must be a length 1 vector of type numeric or integer and >=0");  // NA_INTEGER is covered by <1
    if (isNumeric(skip)) { skip = PROTECT(coerceVector(skip, INTSXP)); protecti++; }
    if (!( (isInteger(skip) && LENGTH(skip)==1 && INTEGER(skip)[0]>=0)  // NA_INTEGER is covered by >=0
         ||(isString(skip) && LENGTH(skip)==1))) error("'skip' must be a length 1 vector of type numeric or integer >=0, or single character search string");
    if (!isNull(separg)) {
        if (!isString(separg) || LENGTH(separg)!=1 || strlen(CHAR(STRING_ELT(separg,0)))!=1) error("'sep' must be 'auto' or a single character");
        if (*CHAR(STRING_ELT(separg,0))=='\"') error("sep='\"' is not an allowed separator");
        if (*CHAR(STRING_ELT(separg,0)) == decChar) error("The two arguments to fread 'dec' and 'sep' are equal ('%c')", decChar);
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
        // Would be nice to print 'Memory mapping' when not verbose, but then it would also print for small files
        // which would be annoying. If we could estimate if the mmap was likely to take more than 2 seconds (and thus
        // the % meter to kick in) then that'd be ideal. A simple size check isn't enough because it might already
        // be cached from a previous run. Perhaps spawn an event, which is cancelled if mmap returns within 2 secs.
#ifdef MAP_POPULATE
        mmp = (const char *)mmap(NULL, filesize, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);    // TO DO?: MAP_HUGETLB
#else
        mmp = (const char *)mmap(NULL, filesize, PROT_READ, MAP_PRIVATE, fd, 0);    // for Mac
#endif
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
        mmp = (const char *)MapViewOfFile(hMap,FILE_MAP_READ,0,0,filesize);
        if (mmp == NULL) {
            CloseHandle(hMap);
            CloseHandle(hFile);
#endif
            if (sizeof(char *)==4)
                error("Opened file ok, obtained its size on disk (%.1fMB), but couldn't memory map it. This is a 32bit machine. You don't need more RAM per se but this fread function is tuned for 64bit addressability, at the expense of large file support on 32bit machines. You probably need more RAM to store the resulting data.table, anyway. And most speed benefits of data.table are on 64bit with large RAM, too. Please either upgrade to 64bit (e.g. a 64bit netbook with 4GB RAM can cost just £300), or make a case for 32bit large file support to datatable-help.", filesize/(1024.0*1024));
                // if we support this on 32bit, we may need to use stat64 instead, as R does
            else if (sizeof(char *)==8)
                error("Opened file ok, obtained its size on disk (%.1fMB), but couldn't memory map it. This is a 64bit machine so this is surprising. Please report to datatable-help.", filesize/1024^2);
            else
                error("Opened file ok, obtained its size on disk (%.1fMB), but couldn't memory map it. Size of pointer is %d on this machine. Probably failing because this is neither a 32bit or 64bit machine. Please report to datatable-help.", filesize/1024^2, sizeof(char *));
        }
#ifndef WIN32
        // if (madvise((char *)mmp, filesize+1, MADV_SEQUENTIAL | MADV_WILLNEED) == -1) warning("Mapped file ok but madvise failed");
        // TO DO: commented this out for the case of nrow=100, more testing to do.
#endif
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
    ch = mmp;
    while (ch<eof && *ch!='\n' && *ch!='\r') {
        if (*ch=='\"') while(++ch<eof && *ch!='\"') {};  // allows protection of \n and \r inside column names
        ch++;                                            // this 'if' needed in case opening protection is not closed before eof
    }
    if (ch>=eof) {
        if (ch>eof) STOP("Internal error: ch>eof when detecting eol");
        if (verbose) Rprintf("Input ends before any \\r or \\n observed. Input will be treated as a single data row.\n");
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
    //   Position to line skip+1 or line containing skip="string" or line autostart
    // ********************************************************************************************
    line = 1; pos = mmp;
    // line is for error and warning messages so considers embedded \n, just like wc -l, head -n and tail -n
    if (isString(skip)) {
        ch = strstr(mmp, CHAR(STRING_ELT(skip,0)));
        if (!ch) STOP("skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)", CHAR(STRING_ELT(skip,0)));
        while (ch>mmp && *(ch-1)!=eol2) ch--;  // move to beginning of line
        pos = ch;
        ch = mmp;
        while (ch<pos) line+=(*ch++==eol);
        if (verbose) Rprintf("Found skip='%s' on line %d.\n", CHAR(STRING_ELT(skip,0)), line);
        ch = pos;
    } else {
        ch = mmp;
        int tmp = INTEGER(skip)[0];
        tmp = tmp>0 ? tmp+1 : INTEGER(autostart)[0];
        while (ch<eof && line<tmp) {
            while (ch<eof && *ch!=eol) ch++;
            if (ch<eof) ch+=eolLen;
            line++;
        }
        pos = ch;
        if (verbose) Rprintf("Positioned on line %d after skip or autostart\n", line);
        
        while (ch<eof && isspace(*ch) && *ch!=eol) ch++;
        Rboolean thisLineBlank = (ch==eof || *ch==eol);
        ch = pos;
        if (INTEGER(skip)[0]>0 && !thisLineBlank) {
            if (verbose) Rprintf("This line isn't blank and skip>0 so we're done\n");
        } else if (thisLineBlank) {
            if (verbose) Rprintf("This line is blank. Moving to the next non-blank ... ");
            while (ch<eof) {
                while (ch<eof && isspace(*ch) && *ch!=eol) ch++;
                if (ch<eof && *ch==eol) { line++; ch+=eolLen; pos=ch; }
                else break;
            }
            if (ch==eof) STOP("Input is either empty or fully whitespace after the skip or autostart. Run again with verbose=TRUE.");
            if (verbose) Rprintf("line %d\n", line);
        } else {
            if (verbose) Rprintf("This line is the autostart and not blank so searching up for the last non-blank ... ");
            // 'autostart' = select-sub-table-using-line-within
            line++;
            while (ch>=mmp && !thisLineBlank) {
                pos = ch;
                line--;
                ch -= eolLen+1;
                i = 0;
                while (ch>=mmp && *ch!=eol2) { i+=!isspace(*ch); ch--; }
                ch++;
                thisLineBlank = i==0;
            }
            ch = pos;
            if (verbose) Rprintf("line %d\n", line);
        }
    }
    if (pos>mmp && *(pos-1)!=eol2) STOP("Internal error. No eol2 immediately before line %d, '%.1s' instead", line, pos-1);
    

    // ********************************************************************************************
    //   Auto detect separator, number of fields, and location of first row
    // ********************************************************************************************
    const char *seps;
    if (isNull(separg)) {
        seps=",\t |;:";  // separators, in order of preference. See ?fread. (colon last as it can appear in time fields)
        if (verbose) Rprintf("Detecting sep ... ");
    } else {
        seps = (const char *)CHAR(STRING_ELT(separg,0));  // length 1 string of 1 character, checked above
        if (verbose) Rprintf("Using supplied sep '%s' ... ", seps[0]=='\t'?"\\t":seps);
    }
    int nseps = strlen(seps);
    
    const char *topStart=ch, *thisStart=ch;
    char topSep=seps[0];
    int topLine=0, topLen=0, topNcol=-1;
    for (int s=0; s<nseps; s++) {
        if (seps[s] == decChar) continue;
        ch=pos; sep=seps[s];
        i=0;
        int thisLine=line, thisLen=0, thisNcol=-1;  // this* = this run's starting *
        while(ch<=eof && ++i<=30) {
            lineStart = ch;
            ncol = countfields();
            if (ncol==-1) {
                if (thisNcol==-1) break;  // if first row has quote problem, move straight on to test a different sep
                ncol=thisNcol;   // skip the quote problem row for now (consider part of current run)
            }
            if (ncol==thisNcol) thisLen++;
            if (ch==eof || i==30 || ncol!=thisNcol) {
                // this* still refers to the previous run which has just finished
                // Rprintf("\nRun: s='%c' thisLine=%d thisLen=%d thisNcol=%d", sep, thisLine, thisLen, thisNcol);
                if (thisNcol>1 && (thisLen>topLen ||     // longest run wins
                                   (thisLen==topLen && sep==topSep && thisNcol>topNcol))) {  // if tied, the one that divides it more (test 1328, 2 rows)
                    topStart = thisStart;
                    topLine = thisLine;
                    topLen = thisLen;
                    topNcol = thisNcol;
                    topSep = sep;
                }
                if (lineStart==eof) break;
                thisStart = lineStart;
                thisLine = line+i-1;
                thisLen = 1;
                thisNcol = ncol;
            }
        }
    }
    if (topNcol<2) {
        if (verbose) Rprintf("Deducing this is a single column input.\n");
        sep=eol;
        ch=pos;
        line=1;
        ncol=1;
    } else {
        sep=topSep;
        ch=pos=topStart;
        line=topLine;
        ncol=topNcol;
        if (verbose) {
            if (isNull(separg)) { if (sep=='\t') Rprintf("'\\t'\n"); else Rprintf("'%c'\n", sep); }
            else Rprintf("found ok\n");
        } 
    }
    if (verbose) {
        if (sep!=eol) Rprintf("Detected %d columns. Longest stretch was from line %d to line %d\n",ncol,line,line+topLen-1);
        ch2 = ch; while(++ch2<eof && *ch2!=eol && ch2-ch<10);
        Rprintf("Starting data input on line %d (either column names or first row of data). First 10 characters: %.*s\n", line, (int)(ch2-ch), ch);
    }
    if (ch>mmp) {
        if (*(ch-1)!=eol2) STOP("Internal error. No eol2 immediately before line %d after sep detection.", line);
        // warn if previous line is not blank, unless skip was provided ...
        if (isInteger(skip) && INTEGER(skip)[0]==0) {
            ch2 = ch-eolLen-1;
            i = 0;
            while (ch2>=mmp && *ch2!=eol2) { i+=!isspace(*ch2); ch2--; }
            ch2++;
            if (i>0) warning("Starting data input on line %d and discarded previous non-empty line: %.*s", line, ch-ch2-eolLen, ch2);
        }
    }
    if (ch!=pos) STOP("Internal error. ch!=pos after sep detection");
    
    // ********************************************************************************************
    //   Detect and assign column names (if present)
    // ********************************************************************************************
    SEXP names = PROTECT(allocVector(STRSXP, ncol));
    protecti++;
    allchar=TRUE;
    for (i=0; i<ncol; i++) {
        if (ch<eof && *ch=='\"') {
            while(++ch<eof && (*ch!='\"' || (ch+1<eof && *(ch+1)!=sep && *(ch+1)!=eol))) {};
            if (ch<eof && *ch++!='\"') STOP("Internal error: quoted field ends before EOF but not with \"sep");
        } else {                              // if field reads as double ok then it's INT/INT64/REAL; i.e., not character (and so not a column name)
            if (*ch!=sep && *ch!=eol && Strtod())  // blank column names (,,) considered character and will get default names
                allchar=FALSE;                     // considered testing at least one isalpha, but we want 1E9 to be a value not a column name
            else while(ch<eof && *ch!=eol && *ch!=sep) ch++;  // skip over unquoted character field
        }
        if (i<ncol-1) {   // not the last column (doesn't have a separator after it)
            if (ch<eof && *ch!=sep) STOP("Unexpected character ending field %d of line %d: %.*s", i+1, line, ch-pos+5, pos);
            else if (ch<eof) ch++;
        } 
    }
    // *** TO DO discard any whitespace after last column name on first row before the eol ***
    if (ch<eof && *ch!=eol) STOP("Not positioned correctly after testing format of header row. ch='%c'",*ch);
    if (verbose && header!=NA_LOGICAL) Rprintf("'header' changed by user from 'auto' to %s\n", header?"TRUE":"FALSE");
    char buff[10]; // to construct default column names
    if (header==FALSE || (header==NA_LOGICAL && !allchar)) {
        if (verbose && header==NA_LOGICAL) Rprintf("Some fields on line %d are not type character (or are empty). Treating as a data row and using default column names.\n", line);
        for (i=0; i<ncol; i++) {
            sprintf(buff,"V%d",i+1);
            SET_STRING_ELT(names, i, mkChar(buff));
        }
        ch = pos;   // back to start of first row. Treat as first data row, no column names present.
    } else {
        if (verbose && header==NA_LOGICAL) Rprintf("All the fields on line %d are character fields. Treating as the column names.\n", line);
        ch = pos;
        line++;
        for (i=0; i<ncol; i++) {
            if (ch<eof && *ch=='\"') {ch++; ch2=ch; while(ch2<eof && (*ch2!='\"' || (ch2+1<eof && *(ch2+1)!=sep && *(ch2+1)!=eol))) ch2++;}
            else {ch2=ch; while(ch2<eof && *ch2!=sep && *ch2!=eol) ch2++;}
            if (ch2>ch) {
                SET_STRING_ELT(names, i, mkCharLen(ch, (int)(ch2-ch)));
            } else {
                sprintf(buff,"V%d",i+1);
                SET_STRING_ELT(names, i, mkChar(buff));
            }
            if (ch2<eof && *ch2=='\"') ch2++;
            if (i<ncol-1) ch=++ch2;
        }
        while (ch<eof && *ch!=eol) ch++;
        if (ch<eof && *ch==eol) ch+=eolLen;  // now on first data row (row after column names)
        pos = ch;
    }
    clock_t tLayout = clock();
    
    // ********************************************************************************************
    //   Count number of rows
    // ********************************************************************************************
    i = INTEGER(nrowsarg)[0];
    if (pos==eof || *pos==eol) {
        nrow=0;
        if (verbose) Rprintf("Byte after header row is eof or eol, 0 data rows present.\n");
    } else if (i>-1) {
        nrow = i;
        if (verbose) Rprintf("nrow set to nrows passed in (%d)\n", nrow);
        // Intended for nrow=10 to see top 10 rows quickly without touching remaining pages
    } else {
        long long neol=1, nsep=0, tmp;
        while (ch<eof) {   
            // goal: quick row count with no branches while still coping with embedded sep and \n, below
            neol+=(*ch==eol);
            nsep+=(*ch++==sep);
        }
        if (ch!=eof) STOP("Internal error: ch!=eof after counting sep and eol");
        i=0; int nblank=0;
        while (i==0 && ch>pos) {   
            // count blank lines at the end
            i=0; while (ch>pos && *--ch!=eol2) i += !isspace(*ch);
            nblank += (i==0);
            ch -= eolLen-1;
        }
        // if (nblank==0) There is non white after the last eol. Ok and dealt with. TO DO: reference test id here in comment
        if (ncol==1) tmp = neol-nblank;
        else tmp = MIN( nsep / (ncol-1),  neol-nblank );   // good quick estimate with embedded sep and eol in mind
        if (verbose || tmp>INT_MAX) {
            Rprintf("Count of eol: %lld (including %d at the end)\n",neol,nblank);
            Rprintf("Count of sep: %lld\n",nsep);
            if (ncol==1) Rprintf("ncol==1 so sep count ignored\n");
            else Rprintf("nrow = MIN( nsep [%lld] / ncol [%d] -1, neol [%lld] - nblank [%d] ) = %lld\n", nsep, ncol, neol, nblank, tmp);
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
    
    // ********************************************************************************************
    //   Make best guess at column types using first 5 rows, middle 5 rows and last 5 rows
    // ********************************************************************************************
    int type[ncol]; for (i=0; i<ncol; i++) type[i]=0;   // default type is lowest.
    const char *str, *thispos;
    for (j=0; j<(nrow>15?3:1); j++) {
        switch(j) {
        case 0: ch = pos;                  str="   first";  break;  // str same width so the codes line up vertically
        case 1: ch = pos + 1*(eof-pos)/3;  str="+ middle";  break;
        case 2: ch = pos + 2*(eof-pos)/3;  str="+   last";  break;  // 2/3 way through rather than end ... easier
        }
        if (j) {  
            // we may have landed inside quoted field containing embedded sep and/or embedded \n
            // find next \n and see if 5 good lines follow. If not try next \n, and so on, until we find the real \n
            // We don't know which line number this is because we jumped straight to it
            int attempts=0;
            while (ch<eof && attempts++<30) {
                while (ch<eof && *ch!=eol) ch++;
                if (ch<eof) ch+=eolLen;
                thispos = ch;
                i = 0;
                while (ch<eof && i<5 && countfields()==ncol) i++;
                if (i==5) {
                    ch=thispos;
                    break;
                }
            }
            if (i<5) {
                warning("Unable to find 5 lines with expected number of columns (%s)\n", str);
                continue;
            }
        }
        i = 0;
        while(i<5 && ch<eof && *ch!=eol) {
            i++;
            lineStart = ch;
            for (field=0;field<ncol;field++) {
                // Rprintf("Field %d: '%.20s'\n", field+1, ch);
                fieldLen=0;
                switch (type[field]) {
                case SXP_LGL:
                    if (Strtob()) break;
                    type[field]++;
                case SXP_INT:
                    ch2=ch;
                    u.l = NA_INTEGER;      // see comments in the main read step lower down. Similar switch as here.
                    if (Strtoll() && INT_MIN<=u.l && u.l<=INT_MAX) break;   
                    type[field]++; ch=ch2;
                case SXP_INT64:
                    if (Strtoll()) break;
                    type[field]++;
                case SXP_REAL:
                    if (Strtod()) break;
                    type[field]++;
                case SXP_STR:
                    Field(0);   // don't do err=1 here because we don't know 'line' when j=1|2. Leave error to throw in data read step.
                }
                if (fieldLen==-1) { i=5; break; } // stop this j early
                if (ch<eof && *ch==sep && field<ncol-1) {ch++; continue;}  // done, next field
                if (field<ncol-1) {
                    if (*ch>31) STOP("Expected sep ('%c') but '%c' ends field %d when detecting types (%s): %.*s", sep, *ch, field, str, ch-lineStart+1, lineStart);
                    else STOP("Expected sep ('%c') but new line, EOF (or other non printing character) ends field %d when detecting types (%s): %.*s", sep, field, str, ch-lineStart+1, lineStart);
                }
            }
            while (ch<eof && *ch!=eol) ch++;
            if (ch<eof && *ch==eol) ch+=eolLen;
        }
        if (verbose) { Rprintf("Type codes (%s 5 rows): ",str); for (i=0; i<ncol; i++) Rprintf("%d",type[i]); Rprintf("\n"); }
    }
    ch = pos;
    
    // ********************************************************************************************
    //   Apply colClasses, select and integer64
    // ********************************************************************************************
    int numNULL = 0;
    SEXP colTypeIndex, items, itemsInt, UserTypeNameSxp;
    int tmp[ncol]; for (i=0; i<ncol; i++) tmp[i]=0;  // used to detect ambiguities (dups) in user's input
    if (length(colClasses)) {
        UserTypeNameSxp = PROTECT(allocVector(STRSXP, NUT));
        protecti++;
        int thisType;
        for (i=0; i<NUT; i++) SET_STRING_ELT(UserTypeNameSxp, i, mkChar(UserTypeName[i]));
        if (isString(colClasses)) {
            // this branch unusual for fread: column types for all columns in one long unamed character vector
            if (length(getAttrib(colClasses, R_NamesSymbol))) STOP("Internal error: colClasses has names, but these should have been converted to list format at R level");
            if (LENGTH(colClasses)!=1 && LENGTH(colClasses)!=ncol) STOP("colClasses is unnamed and length %d but there are %d columns. See ?data.table for colClasses usage.", LENGTH(colClasses), ncol);
            colTypeIndex = PROTECT(chmatch(colClasses, UserTypeNameSxp, NUT, FALSE));  // if type not found then read as character then as. at R level
            protecti++;
            for (k=0; k<ncol; k++) {
                thisType = UserTypeNameMap[ INTEGER(colTypeIndex)[ LENGTH(colClasses)==1 ? 0 : k] -1 ];
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
            for (i=0; i<LENGTH(colClasses); i++) {
                thisType = UserTypeNameMap[INTEGER(colTypeIndex)[i]-1];
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
        for (i=0; i<ncol; i++) if (type[i]==SXP_INT64) {
            type[i] = readInt64As;
            if (verbose) Rprintf("Column %d ('%s') has been detected as type 'integer64'. But reading this as '%s' according to the integer64 parameter.\n", i+1, CHAR(STRING_ELT(names,i)), CHAR(STRING_ELT(integer64,0)));
        }
    }
    if (verbose) { Rprintf("Type codes: "); for (i=0; i<ncol; i++) Rprintf("%d",type[i]); Rprintf(" (after applying colClasses and integer64)\n"); }
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
            itemsInt = PROTECT(chmatch(names, select, NA_INTEGER, FALSE)); protecti++;
            for (i=0; i<ncol; i++) if (INTEGER(itemsInt)[i]==NA_INTEGER) { type[i]=SXP_NULL; numNULL++; }
        } else {
            itemsInt = PROTECT(coerceVector(select, INTSXP)); protecti++;
            for (i=0; i<ncol; i++) tmp[i]=SXP_NULL;
            for (i=0; i<LENGTH(itemsInt); i++) {
                k = INTEGER(itemsInt)[i];
                if (k<1 || k>ncol) STOP("Column number %d (select[%d]) is out of range [1,ncol=%d]",k,i+1,ncol);
                tmp[k-1] = type[k-1];
            }
            for (i=0; i<ncol; i++) type[i] = tmp[i];
            numNULL = ncol - LENGTH(itemsInt);
        }
    }
    if (verbose) { Rprintf("Type codes: "); for (i=0; i<ncol; i++) Rprintf("%d",type[i]); Rprintf(" (after applying drop or select (if supplied)\n"); }
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
        for (i=0,resi=0; i<ncol; i++) if (type[i]!=SXP_NULL) {
            SET_STRING_ELT(resnames,resi++,STRING_ELT(names,i));
        }
        setAttrib(ans, R_NamesSymbol, resnames);
    }
    for (i=0,resi=0; i<ncol; i++) {
        if (type[i] == SXP_NULL) continue;
        thistype = TypeSxp[ type[i] ];
        thiscol = allocVector(thistype,nrow);
        SET_VECTOR_ELT(ans,resi++,thiscol);  // no need to PROTECT thiscol, see R-exts 5.9.1
        if (type[i]==SXP_INT64) setAttrib(thiscol, R_ClassSymbol, ScalarString(mkChar("integer64")));
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
    int batchend; Rboolean hasPrinted=FALSE, whileBreak=FALSE;
    i = 0;
    while (i<nrow && ch<eof) {
        if (showProgress==1 && clock()>nexttime) {
            Rprintf("\rRead %.1f%% of %d rows", (100.0*i)/nrow, nrow);   // prints straight away if the mmap above took a while, is the idea
            R_FlushConsole();    // for Windows
            nexttime = clock()+CLOCKS_PER_SEC;
            hasPrinted = TRUE;
        }
        R_CheckUserInterrupt();
        batchend = MIN(i+10000, nrow);    // batched into 10k rows to save (expensive) calls to clock()
        for (; i<batchend && ch<eof; i++) {
            //Rprintf("Row %d : %.10s\n", i+1, ch);
            if (*ch==eol) {
                // blank line causes early stop.  TO DO: allow blank line skips
                whileBreak = TRUE;  // break the enclosing while too, without changing i
                break;              // break this for
            }
            for (j=0,resj=0; j<ncol; resj+=(type[j]!=SXP_NULL),j++) {
                //Rprintf("Field %d: '%.10s' as type %d\n", j+1, ch, type[j]);
                thiscol = VECTOR_ELT(ans, resj);
                switch (type[j]) {
                case SXP_LGL:
                    if (Strtob()) { LOGICAL(thiscol)[i] = u.b; break; }
                    SET_VECTOR_ELT(ans, resj, thiscol = coerceVectorSoFar(thiscol, type[j]++, SXP_INT, i, j));
                case SXP_INT:
                    ch2=ch; u.l=NA_INTEGER;
                    if (Strtoll() && INT_MIN<=u.l && u.l<=INT_MAX) {  // relies on INT_MIN==NA.INTEGER, checked earlier
                        INTEGER(thiscol)[i] = (int)u.l;
                        break;   //  Most common case. Done with this field. Strtoll already moved ch for us to sit on next sep or eol.
                    }
                    ch=ch2;  // moves ch back ready for type bump and reread of this field (an INT64 would have been read fine by Strtoll)
                    SET_VECTOR_ELT(ans, resj, thiscol = coerceVectorSoFar(thiscol, type[j], readInt64As, i, j));
                    type[j] = readInt64As;
                    if (readInt64As == SXP_REAL) goto case_SXP_REAL;  // a goto here seems readable and reasonable to me
                    if (readInt64As == SXP_STR) goto case_SXP_STR;
                case SXP_INT64:
                    // fix for #488. PREVIOSULY: u.d = NA_REAL;
                    u.l = NAINT64; // NAINT64 is defined in data.table.h, = LLONG_MIN
                    if (Strtoll()) { REAL(thiscol)[i] = u.d; break; }
                    SET_VECTOR_ELT(ans, resj, thiscol = coerceVectorSoFar(thiscol, type[j]++, SXP_REAL, i, j));
                    // A bump from INT to STR will bump through INT64 and then REAL before STR, coercing each time. Deliberately done this way. It's
                    // a small and very rare cost (see comments in coerceVectorSoFar), for better speed 99% of the time (saving deep branches).
                    // TO DO: avoid coercing several times and bump straight to the new type once, somehow.
                case SXP_REAL: case_SXP_REAL:
                    if (Strtod()) { REAL(thiscol)[i] = u.d; break; }
                    SET_VECTOR_ELT(ans, resj, thiscol = coerceVectorSoFar(thiscol, type[j]++, SXP_STR, i, j));
                case SXP_STR: case SXP_NULL: case_SXP_STR:
                    Field(1);
                    if (type[j]==SXP_STR) SET_STRING_ELT(thiscol, i, mkCharLen(fieldStart, fieldLen));
                }
                if (ch<eof && *ch==sep && j<ncol-1) {ch++; continue;}  // done, next field
                if (j<ncol-1) {
                    if (*ch>31) STOP("Expected sep ('%c') but '%c' ends field %d on line %d when reading data: %.*s", sep, *ch, j+1, line, ch-pos+1, pos);
                    else STOP("Expected sep ('%c') but new line or EOF ends field %d on line %d when reading data: %.*s", sep, j+1, line, ch-pos+1, pos);
                    // print whole line here because it's often something earlier in the line that messed up
                }
            }
            //Rprintf("At end of line with i=%d and ch='%.10s'\n", i, ch);
            while (ch<eof && *ch!=eol) ch++; // discard after end of line, but before \n. TO DO: warn about uncommented text here
            if (ch<eof && *ch==eol) ch+=eolLen;
            pos = ch;  // start of line position only needed to include the whole line in any error message
            line++;
        }
        if (whileBreak) break;
    }
    if (showProgress==1 && hasPrinted) {
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
        warning("Stopped reading at empty line %d but text exists afterwards (discarded): %.*s", line, ch2-ch, ch);
    }
    if (i<nrow) {
        if (nrow-i > 100 && (double)i/nrow < 0.95)
            warning("Read less rows (%d) than were allocated (%d). Run again with verbose=TRUE and please report.",i,nrow);
        else if (verbose)
            Rprintf("Read slightly fewer rows (%d) than were allocated (%d).\n", i, nrow);
        nrow = i;
    } else {
        if (i!=nrow) STOP("Internal error: i [%d] > nrow [%d]", i, nrow);
        if (verbose) Rprintf("Read %d rows. Exactly what was estimated and allocated up front\n", i);
    }
    for (j=0; j<ncol-numNULL; j++) SETLENGTH(VECTOR_ELT(ans,j), nrow);
    
    // ********************************************************************************************
    //   Convert na.strings to NA
    // ********************************************************************************************
    for (k=0; k<length(nastrings); k++) {
        thisstr = STRING_ELT(nastrings,k);
        for (j=0; j<ncol-numNULL; j++) {
            thiscol = VECTOR_ELT(ans,j);
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
        Rprintf("%8.3fs (%3.0f%%) sep and header detection\n", 1.0*(tLayout-tMap)/CLOCKS_PER_SEC, 100.0*(tLayout-tMap)/tot);
        Rprintf("%8.3fs (%3.0f%%) Count rows (wc -l)\n", 1.0*(tRowCount-tLayout)/CLOCKS_PER_SEC, 100.0*(tRowCount-tLayout)/tot);
        Rprintf("%8.3fs (%3.0f%%) Column type detection (first, middle and last 5 rows)\n", 1.0*(tColType-tRowCount)/CLOCKS_PER_SEC, 100.0*(tColType-tRowCount)/tot);
        Rprintf("%8.3fs (%3.0f%%) Allocation of %dx%d result (xMB) in RAM\n", 1.0*(tAlloc-tColType)/CLOCKS_PER_SEC, 100.0*(tAlloc-tColType)/tot, nrow, ncol);
        Rprintf("%8.3fs (%3.0f%%) Reading data\n", 1.0*(tRead-tAlloc-tCoerce)/CLOCKS_PER_SEC, 100.0*(tRead-tAlloc-tCoerce)/tot);
        Rprintf("%8.3fs (%3.0f%%) Allocation for type bumps (if any), including gc time if triggered\n", 1.0*tCoerceAlloc/CLOCKS_PER_SEC, 100.0*tCoerceAlloc/tot);
        Rprintf("%8.3fs (%3.0f%%) Coercing data already read in type bumps (if any)\n", 1.0*(tCoerce-tCoerceAlloc)/CLOCKS_PER_SEC, 100.0*(tCoerce-tCoerceAlloc)/tot);
        Rprintf("%8.3fs (%3.0f%%) Changing na.strings to NA\n", 1.0*(tn-tRead)/CLOCKS_PER_SEC, 100.0*(tn-tRead)/tot);
        Rprintf("%8.3fs        Total\n", 1.0*tot/CLOCKS_PER_SEC);
    }
    UNPROTECT(protecti);
    closeFile();
    return(ans);
}

