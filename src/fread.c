#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
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
\n inside a protected field: http://stackoverflow.com/questions/21006661/fread-and-a-quoted-multi-line-column-value
Jim Holtman requested column names be stipped of whitespace.  On datatable-help 22 Jan 2014.
Deal with row.names e.g. http://stackoverflow.com/questions/15448732/reading-csv-with-row-names-by-fread
Test Garrett's two files again (wrap around ,,,,,, and different row lengths that the wc -l now fixes)
Post from patricknik on 5 Jan re ""b"" in a field. And Aykut Firat on email.
Warn about non whitespace (unprotected by comment.char) after the last column on any line (currently skipped silently)
Warning about any imperfect number of columns
Check and correct nline in error messages
Allow logical columns (currently read as character). T/True/TRUE/true are allowed in main/src/util.c
A few TO DO inline in the code, including some speed fine tuning e.g. specialize Ispace and any other lib calls.
Save repeated ch<eof checking in main read step. Last line might still be tricky if last line has no eol.
test using at least "grep read.table ...Rtrunk/tests/
---
Secondary separator for list() columns, such as columns 11 and 12 in BED (no need for strsplit).
If nrow is small, say 20, then it shouldn't mmap the entire file (that's probably *why* user just wants to look at head). Try MMAP_DONTNEED in that case to save needing to map the file in chunks.
Add LaF comparison.
as.read.table=TRUE/FALSE option.  Or fread.table and fread.csv (see http://r.789695.n4.nabble.com/New-function-fread-in-v1-8-7-tp4653745p4654194.html).

*****/

extern size_t sizes[100];
extern SEXP chmatch(SEXP x, SEXP table, R_len_t nomatch, Rboolean in);

static const char *ch, *eof; 
static char sep, eol, eol2;  // sep2 TO DO
static int eolLen;
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
static union {double d; long long l; Rboolean b;} u;
#define NUT        8   // Number of User Types (just for colClasses where "numeric"/"double" are equivalent)
static const char UserTypeName[NUT][10] = {"logical", "integer", "integer64", "numeric", "character", "NULL", "double", "CLASS" };  // important that first 6 correspond to TypeName.  "CLASS" is the fall back to character then as.class at R level ("CLASS" string is just a placeholder).
static int UserTypeNameMap[NUT] = { SXP_LGL, SXP_INT, SXP_INT64, SXP_REAL, SXP_STR, SXP_NULL, SXP_REAL, SXP_STR };

// Each error() needs to close the file first (on Windows). 
// http://stackoverflow.com/questions/18597123/fread-data-table-locks-files
// Tried to use 'goto exit:' but that gave 'jump into scope of identifier with variably modified type' due to
// the (nice and want to keep) C99 "int type[ncol]" further down.  But error() is called from functions above anyway, so
// goto wouldn't work for those (can't goto out of a function's scope).
// A functional wrapper around error() would be nice, but then passing on va_list issues bite, since errors are constructed. And each call's
// arguments would need to be switched depending on Windows/non-Windows.
// Globals and a macro almost worked but some error messages include mmap'd data (e.g. the field contents), so the file
// close has to be after that. Precluding error() directly.
// Hence globals, errormsg and a function EXIT() ...
const char *fnam=NULL, *mmp;
char errormsg[1024] = "";
size_t filesize;
#ifdef WIN32
HANDLE hFile=0;
HANDLE hMap=0;
void EXIT() {
    if (fnam!=NULL) {
        UnmapViewOfFile(mmp);
        CloseHandle(hMap);
        CloseHandle(hFile);
    }
    if (errormsg[0] != '\0') error(errormsg);
}
#else
int fd=-1;
void EXIT() {
    if (fnam!=NULL) {
        munmap((char *)mmp, filesize);
        close(fd);
    }
    if (errormsg[0] != '\0') error(errormsg);
}
#endif

static int countfields(int err)
{
    int ncol=0;
    const char *lch;  // lch = local ch
    Rboolean protected;
    lch = ch;
    if (sep=='\"') {sprintf(errormsg, "Internal error: sep is \", not an allowed separator"); EXIT();}
    if (lch==eof || *lch==eol) return 0;
    ncol=1;                  // only empty lines (first char eol) have 0 fields. Even one space is classed as one field.
    protected = *lch=='\"';  // if beginning of line starts with " then the first field is protected
    lch++;                   // lch might be start of file (mmp), careful not to check lch-1 for '\' before that (not mapped). So ++ first.
    while (lch<eof && *lch!=eol) {
        if (!protected) {
            ncol += (*lch==sep);
            if (*lch=='\"' && *(lch-1)==sep) protected = TRUE;
        } else {
            if (*lch==sep && *(lch-1)=='\"') { protected = FALSE; lch--; }
        }
        lch++;
    }
    if (err) {
        // countfields is used for varying separators, so we only want the error in one of the calls, see calling usage below.
        if (protected && *(lch-1)=='\"') protected = FALSE;   // The line or file ends with a quote to end the last field's protection
        if (protected) {sprintf(errormsg, "A field starting with quote (\") doesn't end with a quote on this line: %.*s\n", (int)(lch-ch), ch); EXIT();}
    }
    return(ncol);
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
    // Specialized strtod for same reasons as Strtoll (leading \t dealt with), but still uses strtod (hard to do better, unlike strtoll).
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
                warning("C function strtod() returned ERANGE for one or more fields. The first was string input '%.*s'. It was read using (double)strtold() as numeric value %.16E (displayed here using %%.16E); loss of accuracy likely occurred. This message is designed to tell you exactly what has been done by fread's C code, so you can search yourself online for many references about double precision accuracy and these specific C functions. You may wish to use colClasses to read the column as character instead and then coerce that column using the Rmpfr package for greater accuracy.", (int)(lch-start), start, u.d);
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
    if (sizes[TypeSxp[oldtype]]<4) {sprintf(errormsg,"Internal error: SIZEOF oldtype %d < 4", oldtype); EXIT();}
    if (sizes[TypeSxp[newtype]]<4) {sprintf(errormsg,"Internal error: SIZEOF newtype %d < 4", newtype); EXIT();}
    if (sizes[TypeSxp[oldtype]] == sizes[TypeSxp[newtype]]) {
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
    case SXP_INT64:
        switch(oldtype) {
        case SXP_INT :
            for (i=0; i<sofar; i++) REAL(newv)[i] = (INTEGER(v)[i]==NA_INTEGER ? NA_REAL : (u.l=(long long)INTEGER(v)[i],u.d));
            break;
        default :
            sprintf(errormsg, "Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
            EXIT();
        }
        break;
    case SXP_REAL:
        switch(oldtype) {
        case SXP_INT :
            for (i=0; i<sofar; i++) REAL(newv)[i] = (INTEGER(v)[i]==NA_INTEGER ? NA_REAL : (double)INTEGER(v)[i]);
            break;
        case SXP_INT64 :
            for (i=0; i<sofar; i++) REAL(newv)[i] = (ISNA(REAL(v)[i]) ? NA_REAL : (double)(*(long long *)&REAL(v)[i]));
            break;
        default :
            sprintf(errormsg, "Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
            EXIT();
        }
        break;
    case SXP_STR:
        warning("Bumped column %d to type character on data row %d, field contains '%.*s'. Coercing previously read values in this column from integer or numeric back to character which may not be lossless; e.g., if '00' and '000' occurred before they will now be just '0', and there may be inconsistencies with treatment of ',,' and ',NA,' too (if they occurred in this column before the bump). If this matters please rerun and set 'colClasses' to 'character' for this column. Please note that column type detection uses the first 5 rows, the middle 5 rows and the last 5 rows, so hopefully this message should be very rare. If reporting to datatable-help, please rerun and include the output from verbose=TRUE.\n", col+1, sofar+1, lch-ch, ch);
        static char buffer[129];  // 25 to hold [+-]2^63, with spare space to be safe and snprintf too
        switch(oldtype) {
        case SXP_INT :
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
                    snprintf(buffer,128,
                    #ifdef WIN32
                       "%I64d"
                    #else
                       "%lld"
                    #endif
                    ,*(long long *)&REAL(v)[i]);
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
            sprintf(errormsg, "Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
            EXIT();
        }
        break;
    default :
        sprintf(errormsg, "Internal error: attempt to bump from type %d to type %d. Please report to datatable-help.", oldtype, newtype);
        EXIT();
    }
    UNPROTECT(protecti);
    tCoerce += clock()-tCoerce0;
    return(newv);
}

SEXP readfile(SEXP input, SEXP separg, SEXP nrowsarg, SEXP headerarg, SEXP nastrings, SEXP verbosearg, SEXP autostart, SEXP skip, SEXP select, SEXP drop, SEXP colClasses, SEXP integer64, SEXP showProgressArg)
// can't be named fread here because that's already a C function (from which the R level fread function took its name)
{
    SEXP thiscol, ans, thisstr;
    R_len_t i, resi, j, resj, k, protecti=0, nrow=0, ncol=0, nline, flines;
    int thistype;
    const char *pos, *pos1, *ch2, *linestart;
    Rboolean header, allchar, skipon=FALSE;
    verbose=LOGICAL(verbosearg)[0];
    clock_t t0 = clock();
    ERANGEwarning = FALSE;  // just while detecting types, then TRUE before the read data loop
    if (!isInteger(showProgressArg) || LENGTH(showProgressArg)!=1 || (INTEGER(showProgressArg)[0]!=0 && INTEGER(showProgressArg)[0]!=1))
        error("showProgress must be 0 or 1, currently");
    int showProgress = INTEGER(showProgressArg)[0];
    
    errormsg[0] = '\0';  // reset globals.  I know, I know, see comments above where EXIT() is defined. Better ideas?
    fnam = NULL;

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
    if (!isInteger(autostart) || LENGTH(autostart)!=1 || INTEGER(autostart)[0]<1) error("'autostart' must be a length 1 vector of type numeric or integer and >=1");  // NA_INTEGER is covered by <1
    if (isNumeric(skip)) { skip = PROTECT(coerceVector(skip, INTSXP)); protecti++; }
    if (!( (isInteger(skip) && LENGTH(skip)==1 && INTEGER(skip)[0]>=-1)  // NA_INTEGER is covered by >=-1
         ||(isString(skip) && LENGTH(skip)==1))) error("'skip' must be a length 1 vector of type numeric or integer >=-1, or single character search string");  
    if (!isNull(separg) && (!isString(separg) || LENGTH(separg)!=1 || strlen(CHAR(STRING_ELT(separg,0)))!=1)) error("'sep' must be 'auto' or a single character");
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
        if (verbose) Rprintf("File opened, filesize is %.3f GB\n", 1.0*filesize/(1024*1024*1024));
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
        if (verbose) Rprintf("File opened, filesize is %.3f GB\n", 1.0*filesize/(1024*1024*1024));
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
        if (verbose) Rprintf("File is opened and mapped ok\n");
    }
    clock_t tMap = clock();
    // From now use EXIT() wrapper instead of error(), to close it on Windows so as not to lock the file after an error.
    
    // ********************************************************************************************
    //   Auto detect eol, first eol where there are two (i.e. CRLF)
    // ********************************************************************************************
    ch = mmp;
    while (ch<eof && *ch!='\n' && *ch!='\r') {
        if (*ch=='\"') while(++ch<eof && *ch!='\"');  // allow protection of \n and \r inside column names.  TO DO: ignore escaped \"
        ch++;                                         // this 'if' needed in case opening protection is not closed before eof
    }
    if (ch>=eof) {
        if (ch>eof) { sprintf(errormsg, "Internal error: ch>eof when detecting eol"); EXIT(); }
        if (verbose) Rprintf("Input ends before any \\r or \\n observed. Input will be treated as a single data row.\n");
        eol=eol2='\n'; eolLen=1;
    } else {
        eol=eol2=*ch; eolLen=1;
        if (eol=='\r') {
            if (ch+1<eof && *(ch+1)=='\n') {
                if (verbose) Rprintf("Detected eol as \\r\\n (CRLF) in that order, the Windows standard.\n");
                eol2='\n'; eolLen=2;
            } else if (verbose) Rprintf("Detected eol as \\r only (no \\n afterwards). An old Mac 9 standard, discontinued in 2002 according to Wikipedia.\n");
        } else if (eol=='\n') {
            if (ch+1<eof && *(ch+1)=='\r') {
                warning("Detected eol as \\n\\r, a highly unusual line ending. According to Wikipedia the Acorn BBC used this. If it is intended that the first column on the next row is a character column where the first character of the field value is \\r (why?) then the first column should start with a quote (i.e. 'protected'). Proceeding with attempt to read the file.\n");
                eol2='\r'; eolLen=2;
            } else if (verbose) Rprintf("Detected eol as \\n only (no \\r afterwards), the UNIX and Mac standard.\n");
        } else
            { sprintf(errormsg, "Internal error: if no \\r or \\n found then ch should be eof"); EXIT(); }
    }

    // ********************************************************************************************
    //   Position to either autostart (default) or skip+1 or skip="string"
    // ********************************************************************************************
    nline = 0; int lastnonblank = 0; pos = mmp;
    if (isString(skip)) {
        skipon = TRUE;
        ch = strstr(mmp, CHAR(STRING_ELT(skip,0)));
        if (!ch) { sprintf(errormsg, "skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)", CHAR(STRING_ELT(skip,0))); EXIT(); }
        while (ch>mmp && *(ch-1)!=eol2) ch--;
        pos = ch;
        nline = -1;
        if (verbose) Rprintf("Found skip string '%s'. Using this line as column names row. nline=-1 now refers to this line as it would need more time to count the \\n before this point.\n", CHAR(STRING_ELT(skip,0)));
    } else {
        ch = mmp;
        skipon = INTEGER(skip)[0]>=0;
        i = 0;
        while (nline<(skipon ? INTEGER(skip)[0]+1 : INTEGER(autostart)[0]) && ch<eof) {
            int lasti = i; i = 0;
            pos1 = ch;
            while (ch<eof && *ch!=eol) i += !isspace(*ch++);
            nline++;
            if (ch<eof && *ch==eol) ch+=eolLen;
            if (i && (!lastnonblank || lasti)) pos=pos1, lastnonblank=nline;  // last non blank as long as there's a non blank above that too (to avoid one line footers in short files)
        }
        if (lastnonblank==0) { sprintf(errormsg, "Input is either empty or fully whitespace in the first %d rows", nline); EXIT(); }
        nline = lastnonblank;   // nline>nonblank when short files (e.g. under 30 rows) with trailing newlines
        if (pos>mmp && *(pos-1)!=eol2) { sprintf(errormsg, "Internal error. No eol2 immediately before line %d, '%.1s' instead", nline, pos-1); EXIT(); }
    }
        
    // ********************************************************************************************
    //   Auto detect separator, number of fields, and location of first data row 
    // ********************************************************************************************
    const char *seps;
    const char *verbText = INTEGER(skip)[0]>=0 ? "'skip' has been supplied" : "the last non blank line in the first 'autostart'";
    if (isNull(separg)) {
        if (verbose) Rprintf("Using line %d to detect sep (%s) ... ", nline, verbText);
        seps=",\t |;:";  // separators, in order of preference. See ?fread. (colon last as it can appear in time fields)
    } else {
        seps = (const char *)CHAR(STRING_ELT(separg,0));  // length 1 string
        if (verbose) Rprintf("Looking for supplied sep '%s' on line %d (%s) ... ", seps[0]=='\t'?"\\t":seps, nline, verbText);
    }
    int nseps = strlen(seps);
    const char *top=pos, *thistop=pos; // see how high we can get with each sep (until we don't read the same number of fields)
    char topsep=0;               // topsep stores the highest so far
    int topnline=nline;          // the top's corresponding line number
    for (i=0; i<nseps; i++) {
        ch=pos; sep=0;                             // starting from autostart for each sep
        while (ch<eof && *ch!=eol) {               // ch<eof is for one row input with no eol
            if (*ch==seps[i]) { sep=*ch; break; }  // this sep exists on this line
            ch++;
        }
        if (sep==0) {                              // this sep wasn't found
            if (i==nseps-1 && topsep==0) sep=eol;  // last sep && none found => single column input. Now search up for last nonblank line.
            else continue;
        }
        if (topsep==0) topsep=sep;       // First sep found is the top so far. Important for single row input.
        ch = pos; j = 0; thistop = pos;  // back to start of autostart, again
        if (ch==mmp || skipon) continue; // one line input (no lines above to test for consistency) or 'skip' override is set
        ncol = countfields(0);            // ncol on autostart using this separator (sep is global which countfieds() uses)
        do {  ch-=eolLen;                // search up line by line until different number of fields, or (likely) hit the start of file
              while (ch>mmp && *(ch-1)!=eol2) ch--;
        } while (countfields(0)==ncol && (thistop=ch) && ++j && ch>mmp);   // relies on short circuit of first &&
        if (thistop<top) { top=thistop; topsep=sep; topnline=nline-j; }
        // often the header row itself resolves (one sep will get up to and including the header, the other only to the top data row)
    }
    if (topsep==0) { sprintf(errormsg, "Internal error: topsep not set"); EXIT(); }
    sep = topsep;
    if (isNull(separg)) {
        if (verbose) {
            if (sep==eol) Rprintf("none found (see ?fread). Deducing this is a single column input. Otherwise, please specify 'sep' manually.\n");
            else if (sep=='\t') Rprintf("sep='\\t'\n"); else Rprintf("sep='%c'\n", sep);
        }
    } else {
        if (sep==eol && seps[0]!='\n') {if(verbose)Rprintf("\n");sprintf(errormsg,"The supplied 'sep' was not found on line %d. To read the file as a single character column set sep='\\n'.", nline);EXIT();}
        else if (verbose) Rprintf("found ok\n");
    }
    ch = pos;    // back to beginning of autostart
    ncol = countfields(0);
    if (verbose && sep!=eol) Rprintf("Found %d columns\n",ncol);
    ch = pos = top; nline = topnline;
    if ((j=countfields(0)) != ncol) {sprintf(errormsg, "Internal error: Moved to top with sep '%c' but ncol (%d) != ncol on autostart (%d)\n", sep, j, ncol); EXIT(); }
    if (verbose) Rprintf("First row with %d fields occurs on line %d (either column names or first row of data)\n", ncol, nline);
    if (pos > mmp) {
        // test line above for any quote issues that may have resulting in data start row being too low
        ch-=eolLen;
        while (ch>mmp && *(ch-1)!=eol2) ch--;
        countfields(1);  // count the fields with this separator and error if any protected fields not ending in quote
    }
    ch = pos;
    
    // ********************************************************************************************
    //   Detect and assign column names (if present)
    // ********************************************************************************************
    SEXP names = PROTECT(allocVector(STRSXP, ncol));
    protecti++;
    allchar=TRUE;
    for (i=0; i<ncol; i++) {
        if (ch<eof && *ch=='\"') {while(++ch<eof && *ch!=eol && !(*ch=='\"' && *(ch-1)!='\\')) {}; if (ch<eof && *ch++!='\"') {sprintf(errormsg, "Format error on line %d: '%.*s'", nline, (int)(ch-pos+1), pos);EXIT();} }
        else {                              // if field reads as double ok then it's INT/INT64/REAL; i.e., not character (and so not a column name)
            if (*ch!=sep && *ch!=eol && Strtod())  // blank column names (,,) considered character and will get default names
                allchar=FALSE;                     // considered testing at least one isalpha, but we want 1E9 to be a value not a column name
            else while(ch<eof && *ch!=eol && *ch!=sep) ch++;  // skip over character field
        }
        if (i<ncol-1) {   // not the last column (doesn't have a separator after it)
            if (ch<eof && *ch!=sep) {sprintf(errormsg, "Unexpected character (%.5s) ending field %d of line %d", ch, i+1, nline);EXIT();}
            else if (ch<eof) ch++;
        } 
    }
    // *** TO DO discard any whitespace after last column name on first row before the eol ***
    if (ch<eof && *ch!=eol) {sprintf(errormsg,"Not positioned correctly after testing format of header row. ch='%c'",*ch);EXIT();}
    if (verbose && header!=NA_LOGICAL) Rprintf("'header' changed by user from 'auto' to %s\n", header?"TRUE":"FALSE");
    char buff[10]; // to construct default column names
    if (header==FALSE || (header==NA_LOGICAL && !allchar)) {
        if (verbose && header==NA_LOGICAL) Rprintf("Some fields on line %d are not type character (or are empty). Treating as a data row and using default column names.\n", nline);
        for (i=0; i<ncol; i++) {
            sprintf(buff,"V%d",i+1);
            SET_STRING_ELT(names, i, mkChar(buff));
        }
        ch = pos;   // back to start of first row. Treat as first data row, no column names present.
    } else {
        if (verbose && header==NA_LOGICAL) Rprintf("All the fields on line %d are character fields. Treating as the column names.\n", nline);
        ch = pos;
        nline++;
        for (i=0; i<ncol; i++) {
            if (ch<eof && *ch=='\"') {ch++; ch2=ch; while(ch2<eof && *ch2!='\"' && *ch2!=eol) ch2++;}
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
    if (pos==eof || *pos==eol) {
        nrow=0;
        if (verbose) Rprintf("Byte after header row is eof or eol, 0 data rows present.\n");
    } else {
        nrow=1; while (ch<eof) nrow+=(*ch++==eol);
        if (verbose) Rprintf("Count of eol after first data row: %d\n",nrow);
        // Advantages of exact count: i) no need to slightly over allocate (by 5%, say) and no need to clear up on heap during gc(),
        // and ii) no need to implement realloc if estimate doesn't turn out to be large enough (e.g. if sample rows are wider than file average).
        // The old estimate method based on size of first 10 rows :
        // estn = (R_len_t)ceil(1.05 * 10 * (filesize-(pos-mmp)) / (pos2-pos1)) +5;  // +5 for small files
        // if (verbose) Rprintf("Estimated nrows: %d ( 1.05*%d*(%ld-(%ld-%ld))/(%ld-%ld) )\n",estn,10,filesize,pos,mmp,pos2,pos1);
        if (ch!=eof) {sprintf(errormsg,"Internal error: ch!=eof after counting eol");EXIT();}
        i=0; int nblank=0;
        while (i==0 && ch>pos) {   // subtract blank lines at the end from the row count
            i=0; while (ch>pos && *--ch!=eol2) i += !isspace(*ch);
            nblank += (i==0);
            ch -= eolLen-1;
        }
        // if (nblank==0) There is non white after the last eol. Ok and dealt with.
        nrow-=nblank;
        if (verbose) Rprintf("Subtracted %d for last eol and any trailing empty lines, leaving %d data rows\n",nblank,nrow);
    }
    i = INTEGER(nrowsarg)[0];
    if (i>-1 && i<nrow) {
        nrow = i;
        if (verbose) Rprintf("nrow limited to nrows passed in (%d)\n", nrow);
    }
    clock_t tRowCount = clock();
    
    // ********************************************************************************************
    //   Make best guess at column types using first 5 rows, middle 5 rows and last 5 rows
    // ********************************************************************************************
    int type[ncol]; for (i=0; i<ncol; i++) type[i]=0;   // default type is lowest.
    const char *end=pos, *str;
    for (i = 0; i<5; i++) {   // ch is on eol before last line, search back another 5 rows
        while (ch>pos && *--ch!=eol2);
        ch -= eolLen-1;
    }
    end = ch + eolLen;
    for (j=0; j<(nrow>15?3:1); j++) {
        switch(j) {
        case 0: ch = pos;                str="   first";  break;  // str same width so the codes line up vertically
        case 1: ch = pos + (eof-pos)/2;  str="+ middle";  break;
        case 2: ch = end;                str="+   last";  break;
        }
        if (j) {  // find start of next line
            while (ch<eof && *ch!=eol) ch++;
            if (ch<eof && *ch==eol) ch+=eolLen;
        }
        flines = 0;
        while(flines<5 && flines<nrow && ch<eof && *ch!=eol) {
            flines++;
            linestart = ch;
            for (i=0;i<ncol;i++) {
                // Rprintf("Field %d: '%.20s'\n", i+1, ch);        
                switch (type[i]) {
                case SXP_LGL:
                    if (Strtob()) break;
                    type[i]++;
                case SXP_INT:
                    ch2=ch;
                    u.l = NA_INTEGER;      // see comments in the main read step lower down. Similar switch as here.
                    if (Strtoll() && INT_MIN<=u.l && u.l<=INT_MAX) break;   
                    type[i]++; ch=ch2;
                case SXP_INT64:
                    if (Strtoll()) break;
                    type[i]++;
                case SXP_REAL:
                    if (Strtod()) break;
                    type[i]++;
                case SXP_STR:
                    if (*ch=='\"') // protected, now look for the next [^\]"[,|eol]
                        while(++ch<eof && *ch!=eol && !(*ch==sep && *(ch-1)=='\"' && (ch<mmp+2 || *(ch-2)!='\\')));
                    else           // unprotected, look for next next [,|eol]
                        while(ch<eof && *ch!=sep && *ch!=eol) ch++;
                }
                if (ch<eof && *ch==sep && i<ncol-1) {ch++; continue;}  // done, next field
                if (i<ncol-1) {sprintf(errormsg,"Expected sep ('%c') but '%c' ends field %d on line %d when detecting types: %.*s", sep, *ch, i+1, nline+flines-2, (int)(ch-linestart+1), linestart);EXIT();}
            }
            while (ch<eof && *ch!=eol) ch++;
            if (ch<eof && *ch==eol) ch+=eolLen;
        }
        if (verbose) { Rprintf("Type codes (%s 5 rows): ",str); for (i=0; i<ncol; i++) Rprintf("%d",type[i]); Rprintf("\n"); }
    }
    
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
            if (length(getAttrib(colClasses, R_NamesSymbol))) {sprintf(errormsg,"Internal error: colClasses has names, but these should have been converted to list format at R level");EXIT();}
            if (LENGTH(colClasses)!=1 && LENGTH(colClasses)!=ncol) {sprintf(errormsg, "colClasses is unnamed and length %d but there are %d columns. See ?data.table for colClasses usage.", LENGTH(colClasses), ncol);EXIT();}
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
            if (!isNewList(colClasses)) {sprintf(errormsg,"colClasses is not type list or character vector");EXIT();}
            if (!length(getAttrib(colClasses, R_NamesSymbol))) {sprintf(errormsg,"colClasses is type list but has no names");EXIT();}
            colTypeIndex = PROTECT(chmatch(getAttrib(colClasses, R_NamesSymbol), UserTypeNameSxp, NUT, FALSE));
            protecti++;
            for (i=0; i<LENGTH(colClasses); i++) {
                thisType = UserTypeNameMap[INTEGER(colTypeIndex)[i]-1];
                items = VECTOR_ELT(colClasses,i);
                if (thisType == SXP_NULL) {
                    if (!isNull(drop) || !isNull(select)) {sprintf(errormsg,"Can't use NULL in colClasses when select or drop is used as well."); EXIT();}
                    drop = items;
                    continue;
                }
                if (isString(items)) itemsInt = PROTECT(chmatch(items, names, NA_INTEGER, FALSE));
                else itemsInt = PROTECT(coerceVector(items, INTSXP));
                protecti++;
                for (j=0; j<LENGTH(items); j++) {
                    k = INTEGER(itemsInt)[j];
                    if (k==NA_INTEGER) {
                        if (isString(items)) sprintf(errormsg,"Column name '%s' in colClasses[[%d]] not found", CHAR(STRING_ELT(items, j)),i+1);
                        else sprintf(errormsg,"colClasses[[%d]][%d] is NA", i+1, j+1);
                        EXIT();
                    } else {
                        if (k<1 || k>ncol) {sprintf(errormsg,"Column number %d (colClasses[[%d]][%d]) is out of range [1,ncol=%d]",k,i+1,j+1,ncol);EXIT();}
                        k--;
                        if (tmp[k]++) {sprintf(errormsg,"Column '%s' appears more than once in colClasses", CHAR(STRING_ELT(names,k))); EXIT();}
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
        if (any_duplicated(drop,FALSE)) {sprintf(errormsg,"Duplicates detected in drop"); EXIT();}
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
        if (any_duplicated(select,FALSE)) {sprintf(errormsg,"Duplicates detected in select"); EXIT();}
        if (isString(select)) {
            itemsInt = PROTECT(chmatch(names, select, NA_INTEGER, FALSE)); protecti++;
            for (i=0; i<ncol; i++) if (INTEGER(itemsInt)[i]==NA_INTEGER) { type[i]=SXP_NULL; numNULL++; }
        } else {
            itemsInt = PROTECT(coerceVector(select, INTSXP)); protecti++;
            for (i=0; i<ncol; i++) tmp[i]=SXP_NULL;
            for (i=0; i<LENGTH(itemsInt); i++) {
                k = INTEGER(itemsInt)[i];
                if (k<1 || k>ncol) {sprintf(errormsg,"Column number %d (select[%d]) is out of range [1,ncol=%d]",k,i+1,ncol);EXIT();}
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
    while (i<nrow) {
        if (showProgress==1 && clock()>nexttime) {
            Rprintf("\rRead %.1f%% of %d rows", (100.0*i)/nrow, nrow);   // prints straight away if the mmap above took a while, is the idea
            R_FlushConsole();    // for Windows
            nexttime = clock()+CLOCKS_PER_SEC;
            hasPrinted = TRUE;
        }
        R_CheckUserInterrupt();
        batchend = i+10000;    // batched into 10k rows to save (expensive) calls to clock()
        if (batchend>nrow) batchend=nrow;
        for (; i<batchend; i++) {
            //Rprintf("Row %d : %.10s\n", i+1, ch);
            if (*ch==eol) {
                while (ch<eof && isspace(*ch)) ch++;
                if (ch<eof) {
                    ch2 = ch;
                    while (ch2<eof && *ch2!=eol) ch2++;
                    if (isString(skip))
                        warning("Stopped reading at empty line, %d lines after the 'skip' string was found, but text exists afterwards (discarded): %.*s", i, ch2-ch+1, ch);
                    else 
                        warning("Stopped reading at empty line %d of file, but text exists afterwards (discarded): %.*s", nline+i, ch2-ch+1, ch);
                }
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
                    u.d = NA_REAL;
                    if (Strtoll()) { REAL(thiscol)[i] = u.d; break; }
                    SET_VECTOR_ELT(ans, resj, thiscol = coerceVectorSoFar(thiscol, type[j]++, SXP_REAL, i, j));
                    // A bump from INT to STR will bump through INT64 and then REAL before STR, coercing each time. Deliberately done this way. It's
                    // a small and very rare cost (see comments in coerceVectorSoFar), for better speed 99% of the time (saving deep branches).
                    // TO DO: avoid coercing several times and bump straight to the new type once, somehow.
                case SXP_REAL: case_SXP_REAL:
                    if (Strtod()) { REAL(thiscol)[i] = u.d; break; }
                    SET_VECTOR_ELT(ans, resj, thiscol = coerceVectorSoFar(thiscol, type[j]++, SXP_STR, i, j));
                case SXP_STR: case SXP_NULL: case_SXP_STR:
                    ch2=ch;
                    if (*ch=='\"') { // protected, now look for the next [^\]"[,|eol]
                        while(++ch2<eof && *ch2!=eol && !(*ch2==sep && *(ch2-1)=='\"' && (ch2<mmp+2 || *(ch2-2)!='\\')));
                        if (type[j]==SXP_STR) SET_STRING_ELT(thiscol, i, mkCharLen(ch+1, (int)(ch2-ch-2))); // else skip field
                    } else {           // unprotected, look for next next [,|eol]
                        while(ch2<eof && *ch2!=sep && *ch2!=eol) ch2++;
                        if (type[j]==SXP_STR) SET_STRING_ELT(thiscol, i, mkCharLen(ch, (int)(ch2-ch)));
                    }
                    ch = ch2;
                }
                if (ch<eof && *ch==sep && j<ncol-1) {ch++; continue;}  // done, next field
                if (j<ncol-1) {sprintf(errormsg, "Expected sep ('%c') but '%c' ends field %d on line %d when reading data: %.*s", sep, *ch, j+1, i+nline, (int)(ch-pos+1), pos);EXIT();}
            }
            //Rprintf("At end of line with i=%d and ch='%.10s'\n", i, ch);
            while (ch<eof && *ch!=eol) ch++; // discard after end of line, but before \n. TO DO: warn about uncommented text here
            if (ch<eof && *ch==eol) ch+=eolLen;
            pos = ch;  // start of line position only needed to include the whole line in any error message
        }
        if (whileBreak) break;
    }
    if (showProgress==1 && hasPrinted) {
        j = 1+(clock()-t0)/CLOCKS_PER_SEC;
        Rprintf("\rRead %d rows and %d (of %d) columns from %.3f GB file in %02d:%02d:%02d\n", i, ncol-numNULL, ncol, 1.0*filesize/(1024*1024*1024), j/3600, (j%3600)/60, j%60);
        R_FlushConsole();
    }
    clock_t tRead = clock();
    nrow = i;  // A blank line in the middle will have caused reading to stop earlier than expected.
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
    if (errormsg[0] != '\0') {
        warning("Internal error, please report to datatable-help. Data error with no EXIT(): %s", errormsg);
        errormsg[0] = '\0';
    }
    EXIT();  // just close the file (if fnam!=NULL)
    return(ans);
}

