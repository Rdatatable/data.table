#ifdef CLOCK_REALTIME
  #include <time.h>      // clock_gettime for wallclock()
#else
  #include <sys/time.h>  // gettimeofday for wallclock()
#endif
#ifdef WIN32             // means WIN64, too, oddly
  #include <windows.h>
  #include <stdbool.h>   // true and false
#else
  #include <sys/mman.h>  // mmap
  #include <sys/stat.h>  // fstat for filesize
  #include <fcntl.h>     // open
  #include <unistd.h>    // close
  #include <stdbool.h>   // true and false
  #include <ctype.h>     // isspace
  #include <errno.h>     // errno
  #include <string.h>    // strerror
  #include <stdarg.h>    // va_list, va_start
  #include <stdio.h>     // vsnprintf
  #include <math.h>      // ceil, sqrt, isfinite
#endif
#include <omp.h>
#include "fread.h"
#include "freadLookups.h"

// Private globals to save passing all of them through to highly iterated field processors
static const char *eof;
static char sep, eol, eol2;
static int eolLen;
static char quote, dec;
static int quoteRule;
static const char* const* NAstrings;
static _Bool any_number_like_NAstrings=false;
static _Bool blank_is_a_NAstring=false;
static _Bool stripWhite=true;  // only applies to character columns; numeric fields always stripped
static _Bool skipEmptyLines=false, fill=false;

typedef _Bool (*reader_fun_t)(const char **, void *);
static double NA_FLOAT64;  // takes fread.h:NA_FLOAT64_VALUE

#define JUMPLINES 100    // at each of the 100 jumps how many lines to guess column types (10,000 sample lines)

// Private globals so they can be cleaned up both on error and on successful return
static const char *fnam = NULL;
static void *mmp = NULL;
static size_t fileSize;
static _Bool typeOnStack = true;
static int8_t *type = NULL, *size = NULL;
static lenOff *colNames = NULL;
static signed char *oldType = NULL;
static freadMainArgs args;  // global for use by DTPRINT

const char typeName[NUMTYPE][10] = {"drop", "bool8", "int32", "int32", "int64", "float64", "string"};
int8_t     typeSize[NUMTYPE]     = { 0,      1,       4,       4,       8,       8,         8      };
// size_t to prevent potential overflow of n*typeSize[i] (standard practice)

// NAN and INFINITY constants are float, so cast to double once up front.
static const double NAND = (double)NAN;
static const double INFD = (double)INFINITY;

/**
 * Drops `const` qualifier from a `const char*` variable, equivalent of
 * `const_cast<char*>` in C++.
 */
static char* _const_cast(const char *ptr) {
  union { const char *a; char *b; } tmp = { ptr };
  return tmp.b;
}

/**
 * Free any resources / memory buffers allocated by the fread() function, and
 * bring all global variables to a "clean slate". This function must always be
 * executed when fread() exits, either successfully or not.
 */
void freadCleanup(void)
{
  // If typeOnStack is true, then `type` was `alloca`-ed, and therefore must not be freed!
  if (!typeOnStack) { free(type); free(size); }
  type = NULL; size = NULL;
  free(colNames); colNames = NULL;
  free(oldType); oldType = NULL;
  if (mmp != NULL) {
    // Important to unmap as OS keeps internal reference open on file. Process is not exiting as
    // we're a .so/.dll here. If this was a process exiting we wouldn't need to unmap.
    //
    // Note that if there was an error unmapping the view of file, then we should not attempt
    // to call STOP() for 2 reasons: 1) freadCleanup() may have itself been called from STOP(),
    // and we would not want to overwrite the original error message; and 2) STOP() function
    // may call freadCleanup(), thus resulting in an infinite loop.
    #ifdef WIN32
      int ret = UnmapViewOfFile(mmp);
      if (!ret) DTPRINT("System error %d unmapping view of file\n", GetLastError());
    #else
      int ret = munmap(mmp, fileSize);
      if (ret) DTPRINT("System errno %d unmapping file\n", errno);
    #endif
    mmp = NULL;
  } else {
    if (eof) *_const_cast(eof) = '\0';
    // for direct char * input (e.g. tests) we temporarily put eol there so restore '\0'
    // if (eof) for when file is empty and STOP() is called before eof has been set (test 885)
  }
  fileSize = 0;
  sep = eol = eol2 = quote = dec = '\0';
  eolLen = 0;
  quoteRule = -1;
  any_number_like_NAstrings = false;
  blank_is_a_NAstring = false;
  stripWhite = true;
  skipEmptyLines = false;
  fill = false;
  // following are borrowed references: do not free
  fnam = NULL;
  eof = NULL;
  NAstrings = NULL;
}


#define CEIL(x)  ((int)(double)ceil(x))
static inline size_t umax(size_t a, size_t b) { return a > b ? a : b; }
static inline size_t umin(size_t a, size_t b) { return a < b ? a : b; }
static inline int imin(int a, int b) { return a < b ? a : b; }

/** Return value of `x` clamped to the range [upper, lower] */
static inline int64_t clamp_i64(int64_t x, int64_t lower, int64_t upper) {
  return x < lower ? lower : x > upper? upper : x;
}


// Helper for error and warning messages to extract next 10 chars or \n if occurs first
// Used exclusively together with "%.*s"
static int STRLIM(const char *ch, int limit) {
  size_t maxwidth = umin((size_t)limit, (size_t)(eof-ch));
  char *newline = memchr(ch, eol, maxwidth);
  return (newline==NULL ? (int)maxwidth : (int)(newline - ch));
}

static void printTypes(int ncol) {
  // e.g. files with 10,000 columns, don't print all of it to verbose output.
  int tt=(ncol<=110?ncol:90); for (int i=0; i<tt; i++) DTPRINT("%d",type[i]);
  if (ncol>110) { DTPRINT("..."); for (int i=ncol-10; i<ncol; i++) DTPRINT("%d",type[i]); }
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

static inline _Bool is_NAstring(const char *fieldStart) {
  skip_white(&fieldStart);  // updates local fieldStart
  const char* const* nastr = NAstrings;
  while (*nastr) {
    const char *ch1 = fieldStart;
    const char *ch2 = *nastr;
    while (ch1<eof && *ch1 == *ch2) { ch1++; ch2++; }  // not using strncmp due to eof not being '\0'
    if (*ch2=='\0') {
      skip_white(&ch1);
      if (ch1>=eof || *ch1==sep || *ch1==eol) return true;
      // if "" is in NAstrings then true will be returned as intended
    }
    nastr++;
  }
  return false;
}

static _Bool Field(const char **this, void *target)
{
  const char *ch = *this;
  if (stripWhite) skip_white(&ch);  // before and after quoted field's quotes too (e.g. test 1609) but never inside quoted fields
  const char *fieldStart=ch;
  _Bool quoted = false;
  if (*ch!=quote || quoteRule==3) {
    // unambiguously not quoted. simply search for sep|eol. If field contains sep|eol then it must be quoted instead.
    while(ch<eof && *ch!=sep && *ch!=eol) ch++;
  } else {
    // the field is quoted and quotes are correctly escaped (quoteRule 0 and 1)
    // or the field is quoted but quotes are not escaped (quoteRule 2)
    // or the field is not quoted but the data contains a quote at the start (quoteRule 2 too)
    int eolCount = 0;
    quoted = true;
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
      if (ch>=eof || *ch!=quote) return false;
      break;
    case 1:  // quoted with embedded quotes escaped; the final unescaped " must be followed by sep|eol
      while (++ch<eof && *ch!=quote && eolCount<100) {
        eolCount += (*ch==eol);
        ch += (*ch=='\\');
      }
      if (ch>=eof || *ch!=quote) return false;
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
        if (ch!=ch2) { fieldStart--; quoted=false; } // field ending is this sep (neither (*1) or (*2) happened)
      }
      break;
    default:
      return false;  // Internal error: undefined quote rule
    }
  }
  int fieldLen = (int)(ch-fieldStart);
  if (stripWhite && !quoted) {
    while(fieldLen>0 && (fieldStart[fieldLen-1]==' ' || fieldStart[fieldLen-1]=='\t')) fieldLen--;
    // this white space (' ' or '\t') can't be sep otherwise it would have stopped the field earlier at the first sep
  }
  if (quoted) { ch++; if (stripWhite) skip_white(&ch); }
  if (!on_sep(&ch)) return false;
  if (fieldLen==0) {
    if (blank_is_a_NAstring) fieldLen=INT32_MIN;
  } else {
    if (is_NAstring(fieldStart)) fieldLen=INT32_MIN;
  }
  ((lenOff *)target)->len = fieldLen;
  ((lenOff *)target)->off = (uint32_t)(fieldStart-*this);  // agnostic & thread-safe
  *this = ch; // Update caller's ch. This may be after fieldStart+fieldLen due to quotes and/or whitespace
  return true;
}

static inline int countfields(const char **this)
{
  static char trash[8];  // see comment on other trash declaration
  const char *ch = *this;
  if (sep==' ') while (ch<eof && *ch==' ') ch++;  // multiple sep==' ' at the start does not mean sep
  skip_white(&ch);
  int ncol = 0;
  if (ch<eof && *ch==eol) {
    ch+=eolLen;
  } else while (ch<eof) {
    if (!Field(&ch,trash)) return -1;   // -1 means this line not valid for this sep and quote rule
    // Field() leaves *ch resting on sep, eol or >=eof. Checked inside Field().
    ncol++;
    if (ch<eof && *ch==eol) { ch+=eolLen; break; }
    ch++;  // move over sep (which will already be last ' ' if sep=' ').
  }
  *this = ch;
  return ncol;
}

static inline _Bool nextGoodLine(const char **this, int ncol)
{
  const char *ch = *this;
  // we may have landed inside quoted field containing embedded sep and/or embedded \n
  // find next \n and see if 5 good lines follow. If not try next \n, and so on, until we find the real \n
  // We don't know which line number this is, either, because we jumped straight to it. So return true/false for
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
  if (ch<eof && attempts<30) { *this = ch; return true; }
  return false;
}

static _Bool StrtoI64(const char **this, void *target)
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
      *(int64_t *)target = NA_INT64;
      *this = ch;
      return true;
    }
    const char *start=ch;
    int sign=1;
    _Bool quoted = false;
    if (ch<eof && (*ch==quote)) { quoted=true; ch++; }
    if (ch<eof && (*ch=='-' || *ch=='+')) sign -= 2*(*ch++=='-');
    _Bool ok = ch<eof && '0'<=*ch && *ch<='9';  // a single - or + with no [0-9] is !ok and considered type character
    int64_t acc = 0;
    while (ch<eof && '0'<=*ch && *ch<='9' && acc<(INT64_MAX-10)/10) { // compiler should optimize last constant expression
      // Conveniently, INT64_MIN == -9223372036854775808 and INT64_MAX == +9223372036854775807
      // so the full valid range is now symetric [-INT64_MAX,+INT64_MAX] and NA==INT64_MIN==-INT64_MAX-1
      acc *= 10;
      acc += *ch-'0';
      ch++;
    }
    if (quoted) { if (ch>=eof || *ch!=quote) return false; else ch++; }
    // TODO: if (!targetCol) return early?  Most of the time, not though.
    *(int64_t *)target = sign * acc;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    //DTPRINT("StrtoI64 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    *this = ch;
    if (ok && !any_number_like_NAstrings) return true;  // most common case, return
    _Bool na = is_NAstring(start);
    if (ok && !na) return true;
    *(int64_t *)target = NA_INT64;
    next_sep(&ch);  // TODO: can we delete this? consume the remainder of field, if any
    *this = ch;
    return na;
}


static _Bool StrtoI32_bare(const char **this, void *target)
{
    const char *ch = *this;
    if (*ch==sep || *ch==eol) { *(int32_t *)target = NA_INT32; return true; }
    if (sep==' ') return false;  // bare doesn't do sep=' '. TODO - remove
    _Bool neg = *ch=='-';
    ch += (neg || *ch=='+');
    const char *start = ch;  // for overflow guard using field width
    uint_fast64_t acc = 0;   // using unsigned to be clear that acc will never be negative
    uint_fast8_t digit;
    while ( (digit=(uint_fast8_t)(*ch-'0'))<10 ) {  // see init.c for checks of unsigned cast
      acc *= 10;     // optimizer has best chance here; e.g. (x<<2 + x)<<1 or x<<3 + x<<1
      acc += digit;
      ch++;
    }
    *(int32_t *)target = neg ? -(int32_t)acc : (int32_t)acc;
    *this = ch;
    return (*ch==sep || *ch==eol) &&
           (acc ? *start!='0' && acc<=INT32_MAX && (ch-start)<=10 : ch-start==1);
    // If false, both *target and where *this is moved on to, are undefined.
    // INT32 range is NA==-2147483648(INT32_MIN) then symmetric [-2147483647,+2147483647] so we can just test INT32_MAX
    // The max (2147483647) happens to be 10 digits long, hence <=10.
    // Leading 0 (such as 001 and 099 but not 0, +0 or -0) will return false and cause bump to _padded which has the
    // option to treat as integer or string with further cost. Otherwise width would not be sufficient check here in _bare.
}


static _Bool StrtoI32_full(const char **this, void *target)
{
    // Very similar to StrtoI64 (see it for comments). We can't make a single function and switch on TYPEOF(targetCol) to
    // know I64 or I32 because targetCol is NULL when testing types and when dropping columns.
    const char *ch = *this;
    skip_white(&ch);
    if (on_sep(&ch)) {  // most often ',,'
      *(int32_t *)target = NA_INT32;
      *this = ch;
      return true;
    }
    const char *start=ch;
    int sign=1;
    _Bool quoted = false;
    if (ch<eof && (*ch==quote)) { quoted=true; ch++; }
    if (ch<eof && (*ch=='-' || *ch=='+')) sign -= 2*(*ch++=='-');
    _Bool ok = ch<eof && '0'<=*ch && *ch<='9';
    int acc = 0;
    while (ch<eof && '0'<=*ch && *ch<='9' && acc<(INT32_MAX-10)/10) {  // NA==INT_MIN==-2147483648==-INT_MAX(+2147483647)-1
      acc *= 10;
      acc += *ch-'0';
      ch++;
    }
    if (quoted) { if (ch>=eof || *ch!=quote) return false; else ch++; }
    *(int32_t *)target = sign * acc;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    //DTPRINT("StrtoI32 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    *this = ch;
    if (ok && !any_number_like_NAstrings) return true;
    _Bool na = is_NAstring(start);
    if (ok && !na) return true;
    *(int32_t *)target = NA_INT32;
    next_sep(&ch);
    *this = ch;
    return na;
}


// generate freadLookups.h
// TODO: review ERANGE checks and tests; that range outside [1.7e-308,1.7e+308] coerces to [0.0,Inf]
/*
f = "~/data.table/src/freadLookups.h"
cat("const long double pow10lookup[701] = {\n", file=f, append=FALSE)
for (i in (-350):(349)) cat("1.0E",i,"L,\n", sep="", file=f, append=TRUE)
cat("1.0E350L\n};\n", file=f, append=TRUE)
*/

static _Bool StrtoD(const char **this, void *target)
{
    // [+|-]N.M[E|e][+|-]E or Inf or NAN

    const char *ch = *this;
    skip_white(&ch);
    if (on_sep(&ch)) {
      *(double *)target = NA_FLOAT64;
      *this = ch;
      return true;
    }
    _Bool quoted = false;
    if (ch<eof && (*ch==quote)) { quoted=true; ch++; }
    int sign=1;
    double d = NAND;
    const char *start=ch;
    if (ch<eof && (*ch=='-' || *ch=='+')) sign -= 2*(*ch++=='-');
    _Bool ok = ch<eof && (('0'<=*ch && *ch<='9') || *ch==dec);  // a single - or + with no [0-9] is !ok and considered type character
    if (!ok) {
      if      (ch<eof && *ch=='I' && *(ch+1)=='n' && *(ch+2)=='f') { ch+=3; d = sign*INFD; ok=true; }
      else if (ch<eof && *ch=='N' && *(ch+1)=='A' && *(ch+2)=='N') { ch+=3; d = NAND; ok=true; }
    } else {
      uint64_t acc = 0;
      while (ch<eof && '0'<=*ch && *ch<='9' && acc<(UINT64_MAX-10)/10) { // compiler should optimize last constant expression
        // UNIT64_MAX == 18446744073709551615
        acc *= 10;
        acc += (uint64_t)(*ch - '0');
        ch++;
      }
      const char *decCh = (ch<eof && *ch==dec) ? ++ch : NULL;
      while (ch<eof && '0'<=*ch && *ch<='9' && acc<(UINT64_MAX-10)/10) {
        acc *= 10;
        acc += (uint64_t)(*ch - '0');
        ch++;
      }
      int e = decCh ? -(int)(ch-decCh) : 0;
      if (decCh) while (ch<eof && '0'<=*ch && *ch<='9') ch++; // lose precision
      else       while (ch<eof && '0'<=*ch && *ch<='9') { e--; ch++; }  // lose precision but retain scale
      if (ch<eof && (*ch=='E' || *ch=='e')) {
        ch++;
        int esign=1;
        if (ch<eof && (*ch=='-' || *ch=='+')) esign -= 2*(*ch++=='-');
        int eacc = 0;
        while (ch<eof && '0'<=*ch && *ch<='9' && eacc<(INT32_MAX-10)/10) {
          eacc *= 10;
          eacc += *ch-'0';
          ch++;
        }
        e += esign * eacc;
      }
      d = (double)(sign * (long double)acc * pow10lookup[350+e]);
    }
    if (quoted) { if (ch>=eof || *ch!=quote) return false; else ch++; }
    *(double *)target = d;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    *this = ch;
    if (ok && !any_number_like_NAstrings) return true;
    _Bool na = is_NAstring(start);
    if (ok && !na) return true;
    *(double *)target = NA_FLOAT64;
    next_sep(&ch);
    *this = ch;
    return na;
}

static _Bool StrtoB(const char **this, void *target)
{
    // These usually come from R when it writes out.
    const char *ch = *this;
    skip_white(&ch);
    *(int8_t *)target = NA_BOOL8;
    if (on_sep(&ch)) { *this=ch; return true; }  // empty field ',,'
    const char *start=ch;
    _Bool quoted = false;
    if (ch<eof && (*ch==quote)) { quoted=true; ch++; }
    if (quoted && *ch==quote) { ch++; if (on_sep(&ch)) {*this=ch; return true;} else return false; }  // empty quoted field ',"",'
    _Bool logical01 = false;  // expose to user and should default be true?
    if ( ((*ch=='0' || *ch=='1') && logical01) || (*ch=='N' && ch+1<eof && *(ch+1)=='A' && ch++)) {
        *(int8_t *)target = (*ch=='1' ? true : (*ch=='0' ? false : NA_BOOL8));
        ch++;
    } else if (*ch=='T') {
        *(int8_t *)target = true;
        if (++ch+2<eof && ((*ch=='R' && *(ch+1)=='U' && *(ch+2)=='E') ||
                           (*ch=='r' && *(ch+1)=='u' && *(ch+2)=='e'))) ch+=3;
    } else if (*ch=='F') {
        *(int8_t *)target = false;
        if (++ch+3<eof && ((*ch=='A' && *(ch+1)=='L' && *(ch+2)=='S' && *(ch+3)=='E') ||
                           (*ch=='a' && *(ch+1)=='l' && *(ch+2)=='s' && *(ch+3)=='e'))) ch+=4;
    }
    if (quoted) { if (ch>=eof || *ch!=quote) return false; else ch++; }
    if (on_sep(&ch)) { *this=ch; return true; }
    *(int8_t *)target = NA_BOOL8;
    next_sep(&ch);
    *this=ch;
    return is_NAstring(start);
}

static reader_fun_t fun[NUMTYPE] = {&Field, &StrtoB, &StrtoI32_bare, &StrtoI32_full, &StrtoI64, &StrtoD, &Field};

static double wallclock()
{
    double ans = 0;
#ifdef CLOCK_REALTIME
    struct timespec tp;
    if (0==clock_gettime(CLOCK_REALTIME, &tp))
        ans = (double) tp.tv_sec + 1e-9 * (double) tp.tv_nsec;
#else
    struct timeval tv;
    if (0==gettimeofday(&tv, NULL))
        ans = (double) tv.tv_sec + 1e-6 * (double) tv.tv_usec;
#endif
    return ans;
}



int freadMain(freadMainArgs __args) {
    args = __args;  // assign to global for use by DTPRINT() in other functions
    double t0 = wallclock();
    _Bool verbose = args.verbose;
    _Bool warningsAreErrors = args.warningsAreErrors;

    if (fnam != NULL || mmp != NULL || colNames != NULL || oldType != NULL) {
      STOP("Internal error: Previous fread() session was not cleaned up properly");
    }

    int nth = args.nth;
    if (nth <= 0) STOP("nThreads must be >= 1, received %d", nth);
    if (nth > omp_get_max_threads()) {
      nth = omp_get_max_threads();
      DTPRINT("Limited nth=%d to omp_get_max_threads()=%d\n", args.nth, nth);
    }

    typeOnStack = false;
    type = NULL;
    size = NULL;
    uint64_t ui64 = NA_FLOAT64_I64;
    memcpy(&NA_FLOAT64, &ui64, 8);

    NAstrings = args.NAstrings;
    any_number_like_NAstrings = false;
    blank_is_a_NAstring = false;
    // if we know there are no nastrings which are numbers (like -999999) then in the number
    // field processors we can save an expensive step in checking the NAstrings. If the field parses as a number,
    // we then when any_number_like_nastrings==FALSE we know it can't be NA.
    const char * const* nastr = NAstrings;
    while (*nastr) {
      if (**nastr == '\0') {
        blank_is_a_NAstring = true;
        nastr++;
        continue;
      }
      const char *ch = *nastr;
      size_t nchar = strlen(ch);
      if (isspace(ch[0]) || isspace(ch[nchar-1]))
        STOP("freadMain: NAstring <<%s>> has whitespace at the beginning or end", ch);
      if (strcmp(ch,"T")==0    || strcmp(ch,"F")==0 ||
          strcmp(ch,"TRUE")==0 || strcmp(ch,"FALSE")==0 ||
          strcmp(ch,"True")==0 || strcmp(ch,"False")==0 ||
          strcmp(ch,"1")==0    || strcmp(ch,"0")==0)
        STOP("freadMain: NAstring <<%s>> is recognized as type boolean, this is not permitted.", ch);
      char *end;
      errno = 0;
      strtod(ch, &end);  // careful not to let "" get to here (see continue above) as strtod considers "" numeric
      if (errno==0 && (size_t)(end - ch) == nchar) any_number_like_NAstrings = true;
      nastr++;
    }
    if (verbose) {
      if (*NAstrings == NULL) {
        DTPRINT("No NAstrings provided.\n");
      } else {
        DTPRINT("NAstrings = [");
        const char * const* s = NAstrings;
        while (*s++) DTPRINT(*s? "<<%s>>, " : "<<%s>>", *(s-1));
        DTPRINT("]\n");
        DTPRINT("%s of the NAstrings are numeric (such as '-9999').\n",
                any_number_like_NAstrings ? "One or more" : "None");
      }
    }

    stripWhite = args.stripWhite;
    skipEmptyLines = args.skipEmptyLines;
    fill = args.fill;
    dec = args.dec;
    quote = args.quote;

    // ********************************************************************************************
    //   Point to text input if it contains \n, or open and mmap file if not
    // ********************************************************************************************
    const char *sof;  // start-of-file
    const char *ch;
    fnam = NULL;
    mmp = NULL;
    if (args.input != NULL) {
        if (verbose) DTPRINT("`input` argument is given, interpreting as raw text to read\n");
        fnam = "<input>";
        fileSize = strlen(args.input);
        sof = args.input;
        eof = sof+fileSize;
        if (*eof!='\0') STOP("Internal error: last byte of character input isn't \\0");
    } else if (args.filename != NULL) {
        if (verbose) DTPRINT("`filename` argument given, attempting to open a file with such name\n");
        fnam = args.filename;
#ifndef WIN32
        int fd = open(fnam, O_RDONLY);
        if (fd==-1) STOP("file not found: %s",fnam);
        struct stat stat_buf;
        if (fstat(fd,&stat_buf) == -1) {close(fd); STOP("Opened file ok but couldn't obtain file size: %s", fnam);}
        fileSize = (size_t) stat_buf.st_size;
        if (fileSize == 0) {close(fd); STOP("File is empty: %s", fnam);}
        if (verbose) {
            DTPRINT("File opened, size %.6f GB.\n", 1.0*fileSize/(1024*1024*1024));
            DTPRINT("Memory mapping ... ");
        }

        // No MAP_POPULATE for faster nrows=10 and to make possible earlier progress bar in row count stage
        // Mac doesn't appear to support MAP_POPULATE anyway (failed on CRAN when I tried).
        // TO DO?: MAP_HUGETLB for Linux but seems to need admin to setup first. My Hugepagesize is 2MB (>>2KB, so promising)
        //         https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
        mmp = mmap(NULL, fileSize+1, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);  // COW pages. +1 for '\0' we'll add
        close(fd);  // we don't need to keep file handle open
        if (mmp == MAP_FAILED) {
#else
        // Following: http://msdn.microsoft.com/en-gb/library/windows/desktop/aa366548(v=vs.85).aspx
        HANDLE hFile = INVALID_HANDLE_VALUE;
        int attempts = 0;
        while(hFile==INVALID_HANDLE_VALUE && attempts<5) {
            hFile = CreateFile(fnam, GENERIC_READ, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
            // FILE_SHARE_WRITE is required otherwise if the file is open in Excel, CreateFile fails. Should be ok now.
            if (hFile==INVALID_HANDLE_VALUE) {
                if (GetLastError()==ERROR_FILE_NOT_FOUND) STOP("File not found: %s",fnam);
                if (attempts<4) Sleep(250);  // 250ms
            }
            attempts++;
            // Looped retry to avoid ephemeral locks by system utilities as recommended here : http://support.microsoft.com/kb/316609
        }
        if (hFile==INVALID_HANDLE_VALUE) STOP("Unable to open file after %d attempts (error %d): %s", attempts, GetLastError(), fnam);
        LARGE_INTEGER liFileSize;
        if (GetFileSizeEx(hFile,&liFileSize)==0) { CloseHandle(hFile); STOP("GetFileSizeEx failed (returned 0) on file: %s", fnam); }
        fileSize = (size_t)liFileSize.QuadPart;
        if (fileSize<=0) { CloseHandle(hFile); STOP("File is empty: %s", fnam); }
        if (verbose) {
            DTPRINT("File opened, size %.6f GB.\n", (double)fileSize/(1024*1024*1024));
            DTPRINT("Memory mapping ... ");
        }
        DWORD hi = (fileSize) >> 32;            // tried very very hard again on 26 & 27th April 2017 to over-map file by 1 byte
        DWORD lo = (fileSize) & 0xFFFFFFFFull;  // on Windows for COW/FILE_MAP_COPY on read-only file, with no joy.
        HANDLE hMap=CreateFileMapping(hFile, NULL, PAGE_WRITECOPY, hi, lo, NULL);
        if (hMap==NULL) { CloseHandle(hFile); STOP("This is Windows, CreateFileMapping returned error %d for file %s", GetLastError(), fnam); }
        mmp = MapViewOfFile(hMap,FILE_MAP_COPY,0,0,fileSize);  // fileSize must be <= hilo passed to CreateFileMapping above.
        CloseHandle(hMap);  // we don't need to keep the file open; the MapView keeps an internal reference;
        CloseHandle(hFile); //   see https://msdn.microsoft.com/en-us/library/windows/desktop/aa366537(v=vs.85).aspx
        if (mmp == NULL) {
#endif
            if (sizeof(char *)==4) {
                STOP("Opened file ok, obtained its size on disk (%.1fMB) but couldn't memory map it. This is a 32bit machine. You don't need more RAM per se but this fread function is tuned for 64bit addressability at the expense of large file support on 32bit machines. You probably need more RAM to store the resulting data.table, anyway. And most speed benefits of data.table are on 64bit with large RAM, too. Please upgrade to 64bit.", (double)fileSize/(1024*1024));
                // if we support this on 32bit, we may need to use stat64 instead, as R does
            } else if (sizeof(char *)==8) {
                STOP("Opened file ok, obtained its size on disk (%.1fMB), but couldn't memory map it. This is a 64bit machine so this is surprising. Please report.", (double)fileSize/(1024*1024));
            } else {
                STOP("Opened file ok, obtained its size on disk (%.1fMB), but couldn't memory map it. Size of pointer is %d on this machine. Probably failing because this is neither a 32bit or 64bit machine. Please report.", (double)fileSize/(1024*1024), sizeof(char *));
            }
        }
        sof = (const char*) mmp;
        eof = sof+fileSize;  // byte after last byte of file.
        if (verbose) DTPRINT("ok\n");  // to end 'Memory mapping ... '
    } else {
        sof = NULL;
        STOP("Neither `input` nor `filename` are given, nothing to read.");
    }
    double tMap = wallclock();

    // ********************************************************************************************
    //   Check whether the file contains BOM (Byte Order Mark), and if yes strip it, modifying
    //   `mmp`. Also, presence of BOM allows us to reliably detect the file's encoding.
    //   See: https://en.wikipedia.org/wiki/Byte_order_mark
    //   See: issues #1087 and #1465
    // ********************************************************************************************
    if (fileSize >= 3 && memcmp(sof, "\xEF\xBB\xBF", 3) == 0) {
      sof += 3;
      // ienc = CE_UTF8;
      if (args.verbose) DTPRINT("UTF-8 byte order mark EF BB BF found at the start of the file and skipped.\n");
    }
    else if (fileSize >= 4 && memcmp(sof, "\x84\x31\x95\x33", 4) == 0) {
      sof += 4;
      // ienc = CE_GB18030;
      if (args.verbose) DTPRINT("GB-18030 byte order mark 84 31 95 33 found at the start of the file and skipped.\n");
      DTWARN("GB-18030 encoding detected, however fread() is unable to decode it. Some character fields may be garbled.\n");
    }
    else if (fileSize >= 2 && sof[0] + sof[1] == '\xFE' + '\xFF') {  // either 0xFE 0xFF or 0xFF 0xFE
      STOP("File is encoded in UTF-16, this encoding is not supported by fread(). Please recode the file to UTF-8.");
    }

    // ********************************************************************************************
    //   Auto detect eol, first eol where there are two (i.e. CRLF)
    // ********************************************************************************************
    ch = sof;
    while (ch<eof && *ch!='\n' && *ch!='\r') {
        if (*ch==quote) while(++ch<eof && *ch!=quote) {}; // (TODO unbounded to fix) allows protection of \n and \r inside column names
        ch++;                                             // this 'if' needed in case opening protection is not closed before eof
    }
    if (ch>=eof) {
        if (ch>eof) STOP("Internal error: ch>eof when detecting eol");
        if (verbose) DTPRINT("Input ends before any \\r or \\n observed. Input will be treated as a single row.\n");
        eol=eol2='\n'; eolLen=1;
    } else {
        eol=eol2=*ch; eolLen=1;
        if (eol=='\r') {
            if (ch+1<eof && *(ch+1)=='\n') {
                if (verbose) DTPRINT("Detected eol as \\r\\n (CRLF) in that order, the Windows standard.\n");
                eol2='\n'; eolLen=2;
            } else {
                if (ch+1<eof && *(ch+1)=='\r')
                    STOP("Line ending is \\r\\r\\n. R's download.file() appears to add the extra \\r in text mode on Windows. Please download again in binary mode (mode='wb') which might be faster too. Alternatively, pass the URL directly to fread and it will download the file in binary mode for you.");
                    // NB: on Windows, download.file from file: seems to condense \r\r too. So
                if (verbose) DTPRINT("Detected eol as \\r only (no \\n or \\r afterwards). An old Mac 9 standard, discontinued in 2002 according to Wikipedia.\n");
            }
        } else if (eol=='\n') {
            if (ch+1<eof && *(ch+1)=='\r') {
                DTWARN("Detected eol as \\n\\r, a highly unusual line ending. According to Wikipedia the Acorn BBC used this. If it is intended that the first column on the next row is a character column where the first character of the field value is \\r (why?) then the first column should start with a quote (i.e. 'protected'). Proceeding with attempt to read the file.\n");
                eol2='\r'; eolLen=2;
            } else if (verbose) DTPRINT("Detected eol as \\n only (no \\r afterwards), the UNIX and Mac standard.\n");
        } else
            STOP("Internal error: if no \\r or \\n found then ch should be eof");
    }
    // Ensure file ends with eol so we don't need to check 'ch<eof && *ch...' everywhere in deep loops just because
    // the very last field in the file might finish abrubtly with no final \n (or \r\n on Windows).
    // The file was opened readonly and mapped with page level copy-on-write so this won't write to file.
    // On Windows we may only have a problem if the file i) does not end with newline already AND ii) it is an exact multiple
    // of the VirtualAlloc block size (64k) -- for such rare cases we could ask the Windows user to add an ending newline.
    // TODO - only need to do this *eof write if final data line does end abrubtly. This is a first step for now to see if it works.
    // On Linux and Mac it's safe since mmap() there accepts fileSize+1. On Windows, it writes into the last page MAP_COMMIT'd for this
    // file which is ok I think as VirtualAlloc() allocates on boundaries (64k on Windows). There may be a way on Windows
    // using Zw* lower level but I haven't yet managed to include headers correctly (I asked on r-package-devel on 28 Apr).
    *_const_cast(eof) = eol;

    // ********************************************************************************************
    //   Position to line skip+1 or line containing skip="string"
    // ********************************************************************************************
    int line = 1;
    // line is for error and warning messages so considers raw \n whether inside quoted fields or not, just
    // like wc -l, head -n and tail -n
    const char *pos = sof;
    ch = pos;
    if (args.skipString!=NULL) {
        ch = strstr(sof, args.skipString);
        if (!ch) STOP("skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)",
                      args.skipString);
        while (ch>sof && *(ch-1)!=eol2) ch--;  // move to beginning of line
        pos = ch;
        ch = sof;
        while (ch<pos) line+=(*ch++==eol);
        if (verbose) DTPRINT("Found skip='%s' on line %d. Taking this to be header row or first row of data.\n",
                                  args.skipString, line);
        ch = pos;
    } else if (args.skipNrow>0) {
        while (ch<eof && line<=args.skipNrow) line+=(*ch++==eol);
        if (ch>=eof) STOP("skip=%d but the input only has %d line%s", args.skipNrow, line, line>1?"s":"");
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
      if (lineStart>ch) DTPRINT("Moved forward to first non-blank line (%d)\n", line);
      DTPRINT("Positioned on line %d starting: <<%.*s>>\n", line, STRLIM(lineStart, 30), lineStart);
    }
    ch = pos = lineStart;

    // *********************************************************************************************************
    //   Auto detect separator, quoting rule, first line and ncol, simply, using jump 0 only
    //
    //   Always sample as if nrows= wasn't supplied. That's probably *why* user is setting nrow=0 to get the
    //   column names and types, without actually reading the data yet. Most likely to check consistency across
    //   a set of files.
    // *********************************************************************************************************
    char seps[]=",|;\t ";  // default seps in order of preference. See ?fread.
    // using seps[] not *seps for writeability (http://stackoverflow.com/a/164258/403310)
    int nseps = (int) strlen(seps);

    if (args.sep == quote && quote!='\0') STOP("sep == quote ('%c') is not allowed", quote);
    if (dec=='\0') STOP("dec='' not allowed. Should be '.' or ','");
    if (args.sep == dec) STOP("sep == dec ('%c') is not allowed", dec);
    if (quote == dec) STOP("quote == dec ('%c') is not allowed", dec);
    if (args.sep == '\0') {  // default is '\0' meaning 'auto'
      if (verbose) DTPRINT("Detecting sep ...\n");
    } else {
      seps[0]=args.sep; seps[1]='\0'; nseps=1;
      if (verbose) DTPRINT("Using supplied sep '%s'\n", args.sep=='\t' ? "\\t" : seps);
    }

    int topNumLines=0;        // the most number of lines with the same number of fields, so far
    int topNumFields=1;       // how many fields that was, to resolve ties
    char topSep=eol;          // which sep that was, by default \n to mean single-column input (1 field)
    int topQuoteRule=0;       // which quote rule that was
    int topNmax=1;            // for that sep and quote rule, what was the max number of columns (just for fill=true)
                              //   (when fill=true, the max is usually the header row and is the longest but there are more
                              //    lines of fewer)
    const char *firstJumpEnd=NULL; // remember where the winning jumpline from jump 0 ends, to know its size excluding header

    // We will scan the input line-by-line (at most `JUMPLINES + 1` lines; "+1"
    // covers the header row, at this stage we don't know if it's present), and
    // detect the number of fields on each line. If several consecutive lines
    // have the same number of fields, we'll call them a "contiguous group of
    // lines". Arrays `numFields` and `numLines` contain information about each
    // contiguous group of lines encountered while scanning the first JUMPLINES
    // + 1 lines: 'numFields` gives the number the count of fields in each
    // group, and `numLines` has the number of lines in each group.
    int numFields[JUMPLINES+1];
    int numLines[JUMPLINES+1];
    for (int s=0; s<nseps; s++) {
      sep = seps[s];
      for (quoteRule=0; quoteRule<4; quoteRule++) {  // quote rule in order of preference
        ch = pos;
        // if (args.verbose) DTPRINT("Trying sep='%c' with quoteRule %d ...\n", sep, quoteRule);
        for (int i=0; i<=JUMPLINES; i++) { numFields[i]=0; numLines[i]=0; } // clear VLAs
        int i=-1; // The slot we're counting the currently contiguous consistent ncol
        int thisLine=0, lastncol=-1;
        while (ch<eof && thisLine++<JUMPLINES) {
          int thisncol = countfields(&ch);   // using this sep and quote rule; moves ch to start of next line
          if (thisncol<0) { numFields[0]=-1; break; }  // invalid file with this sep and quote rule; abort
          if (thisncol!=lastncol) { numFields[++i]=thisncol; lastncol=thisncol; } // new contiguous consistent ncol started
          numLines[i]++;
        }
        if (numFields[0]==-1) continue;
        if (firstJumpEnd==NULL) firstJumpEnd=ch;  // if this wins (doesn't get updated), it'll be single column input
        _Bool updated=false;
        int nmax=0;

        i=-1; while (numLines[++i]) {
          if (numFields[i] > nmax) nmax=numFields[i];  // for fill=true to know max number of columns
          //if (args.verbose) DTPRINT("numLines[i]=%d, topNumLines=%d, numFields[i]=%d, topNumFields=%d\n",
          //                           numLines[i], topNumLines, numFields[i], topNumFields);
          if (numFields[i]>1 &&
              ( numLines[i]>topNumLines ||   // most number of consistent ncol wins
               (numLines[i]==topNumLines && numFields[i]>topNumFields && sep!=' '))) {  // ties resolved by numFields
            topNumLines=numLines[i]; topNumFields=numFields[i]; topSep=sep; topQuoteRule=quoteRule; topNmax=nmax;
            firstJumpEnd = ch;  // So that after the header we know how many bytes jump point 0 is
            updated = true;
            // Two updates can happen for the same sep and quoteRule (e.g. issue_1113_fread.txt where sep=' ') so the
            // updated flag is just to print once.
          }
        }
        if (verbose && updated) {
          DTPRINT("  sep=="); DTPRINT(sep=='\t' ? "'\\t'" : "'%c'(ascii %d)", sep, sep);
          DTPRINT("  with %d lines of %d fields using quote rule %d\n", topNumLines, topNumFields, topQuoteRule);
        }
      }
    }
    if (!firstJumpEnd) STOP("Internal error: no sep won");

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
      int thisLine=-1;
      while (ch<eof && ++thisLine<JUMPLINES) {
        const char *ch0 = ch;
        if (countfields(&ch)==ncol) { ch=pos=ch0; line+=thisLine; break; }
      }
    }
    // For standard regular separated files, we're now on the first byte of the file.

    if (ncol<1 || line<1) STOP("Internal error: ncol==%d line==%d after detecting sep, ncol and first line", ncol, line);
    int tt = countfields(&ch);
    ch = pos; // move back to start of line since countfields() moved to next
    if (!fill && tt!=ncol) STOP("Internal error: first line has field count %d but expecting %d", tt, ncol);
    if (verbose) {
      DTPRINT("Detected %d columns on line %d. This line is either column names or first data row (first 30 chars): <<%.*s>>\n",
               tt, line, STRLIM(pos, 30), pos);
      if (fill) DTPRINT("fill=true and the most number of columns found is %d\n", ncol);
    }

    // ********************************************************************************************
    //   Detect and assign column names (if present)
    // ********************************************************************************************
    char trash[8]; // throw-away storage for processors to write to in this preamble. Saves deep 'if (target)' inside processors.
    const char *colNamesAnchor = ch;
    colNames = calloc((size_t)ncol, sizeof(lenOff));
    if (!colNames) STOP("Unable to allocate %d*%d bytes for column name pointers: %s", ncol, sizeof(lenOff), strerror(errno));
    _Bool allchar=true;
    if (sep==' ') while (ch<eof && *ch==' ') ch++;
    ch--;  // so we can ++ at the beginning inside loop.
    for (int field=0; field<tt; field++) {
      const char *this = ++ch;
      //DTPRINT("Field %d <<%.*s>>\n", i, STRLIM(ch,20), ch);
      skip_white(&ch);
      if (allchar && !on_sep(&ch) && StrtoD(&ch,trash)) allchar=false;  // don't stop early as we want to check all columns to eol here
      // considered looking for one isalpha present but we want 1E9 to be considered a value not a column name
      ch = this;  // rewind to the start of this field
      Field(&ch,trash);  // StrtoD does not consume quoted fields according to the quote rule, so redo with Field()
      // countfields() above already validated the line so no need to check again now.
    }
    if (ch<eof && *ch!=eol)
      STOP("Read %d expected fields in the header row (fill=%d) but finished on <<%.*s>>'",tt,fill,STRLIM(ch,30),ch);
    // already checked above that tt==ncol unless fill=TRUE
    // when fill=TRUE and column names shorter (test 1635.2), leave calloc initialized lenOff.len==0
    if (verbose && args.header!=NA_BOOL8) DTPRINT("'header' changed by user from 'auto' to %s\n", args.header?"true":"false");
    if (args.header==false || (args.header==NA_BOOL8 && !allchar)) {
        if (verbose && args.header==NA_BOOL8) DTPRINT("Some fields on line %d are not type character. Treating as a data row and using default column names.\n", line);
        // colNames was calloc'd so nothing to do; all len=off=0 already
        ch = pos;  // back to start of first row. Treat as first data row, no column names present.
        // now check previous line which is being discarded and give helpful msg to user ...
        if (ch>sof && args.skipNrow==0) {
          ch -= (eolLen+1);
          if (ch<sof) ch=sof;  // for when sof[0]=='\n'
          while (ch>sof && *ch!=eol2) ch--;
          if (ch>sof) ch++;
          const char *prevStart = ch;
          int tmp = countfields(&ch);
          if (tmp==ncol) STOP("Internal error: row before first data row has the same number of fields but we're not using it.");
          if (tmp>1) DTWARN("Starting data input on line %d <<%.*s>> with %d fields and discarding line %d <<%.*s>> before it because it has a different number of fields (%d).", line, STRLIM(pos, 30), pos, ncol, line-1, STRLIM(prevStart, 30), prevStart, tmp);
        }
        if (ch!=pos) STOP("Internal error. ch!=pos after prevBlank check");
    } else {
        if (verbose && args.header==NA_BOOL8) DTPRINT("All the fields on line %d are character fields. Treating as the column names.\n", line);
        ch = pos;
        line++;
        if (sep==' ') while (ch<eof && *ch==' ') ch++;
        ch--;
        for (int i=0; i<ncol; i++) {
            // Use Field() here as it's already designed to handle quotes, leading space etc.
            const char *start = ++ch;
            Field(&ch, colNames+i);  // stores the string length and offset as <uint,uint> in colnames[i]
            colNames[i].off += (size_t)(start-colNamesAnchor);
            if (ch>=eof || *ch==eol) break;   // already checked number of fields previously above
        }
        if (ch<eof && *ch!=eol) STOP("Internal error: reading colnames did not end on eol");
        if (ch<eof) ch+=eolLen;
        pos=ch;    // now on first data row (row after column names)
    }
    int row1Line = line;
    double tLayout = wallclock();

    // *****************************************************************************************************************
    //   Make best guess at column types using 100 rows at 100 points, including the very first, middle and very last row.
    //   At the same time, calc mean and sd of row lengths in sample for very good nrow estimate.
    // *****************************************************************************************************************
    typeOnStack = ncol<10000;
    if (typeOnStack) {
      type = (signed char *)alloca((size_t)ncol * sizeof(int8_t));  //TODO: combine?
      size = (signed char *)alloca((size_t)ncol * sizeof(int8_t));
    } else {
      type = (signed char *)malloc((size_t)ncol * sizeof(int8_t));
      size = (signed char *)malloc((size_t)ncol * sizeof(int8_t));
    }
    // (...?alloca:malloc)(...) doesn't compile as alloca is special.

    // 9.8KB is for sure fine on stack. Almost went for 1MB (1 million columns) but decided to be uber safe.
    // sizeof(signed char) == 1 checked in init.c. To free or not to free is in cleanup() based on typeOnStack
    if (!type) STOP("Failed to allocate %dx%d bytes for type: %s", ncol, sizeof(int8_t), strerror(errno));
    for (int j=0; j<ncol; j++) { size[j] = type[j] = 1; } // lowest enum is 1 (CT_BOOL8 at the time of writing). 0==CT_DROP

    size_t jump0size=(size_t)(firstJumpEnd-pos);  // the size in bytes of the first JUMPLINES from the start (jump point 0)
    int nJumps = 0;
    // how many places in the file to jump to and test types there (the very end is added as 11th or 101th)
    // not too many though so as not to slow down wide files; e.g. 10,000 columns.  But for such large files (50GB) it is
    // worth spending a few extra seconds sampling 10,000 rows to decrease a chance of costly reread even further.
    if (jump0size>0) {
      if (jump0size*100*2 < (size_t)(eof-pos)) nJumps=100;  // 100 jumps * 100 lines = 10,000 line sample
      else if (jump0size*10*2 < (size_t)(eof-pos)) nJumps=10;
      // *2 to get a good spacing. We don't want overlaps resulting in double counting.
      // nJumps==1 means the whole (small) file will be sampled with one thread
    }
    nJumps++; // the extra sample at the very end (up to eof) is sampled and format checked but not jumped to when reading
    if (verbose) {
      DTPRINT("Number of sampling jump points = %d because ",nJumps);
      if (jump0size==0) DTPRINT("jump0size==0\n");
      else DTPRINT("%zd bytes from row 1 to eof / (2 * %zd jump0size) == %zd\n",
                   (size_t)(eof-pos), jump0size, (size_t)(eof-pos)/(2*jump0size));
    }

    int sampleLines=0;
    size_t sampleBytes=0;
    double sumLen=0.0, sumLenSq=0.0;
    int minLen=INT32_MAX, maxLen=-1;   // int_max so the first if(thisLen<minLen) is always true; similarly for max
    const char *lastRowEnd=pos;
    for (int j=0; j<nJumps; j++) {
        ch = (j == 0) ? pos :
             (j == nJumps-1) ? eof - (size_t)(0.5*jump0size) :
                               pos + (size_t)j*((size_t)(eof-pos)/(size_t)(nJumps-1));
        if (j>0 && !nextGoodLine(&ch, ncol))
          STOP("Could not find first good line start after jump point %d when sampling.", j);
        if (ch<lastRowEnd) // otherwise an overlap would lead to double counting and a wrong estimate
          STOP("Internal error: Sampling jump point %d is before the last jump ended", j);
        _Bool bumped = 0;  // did this jump find any different types; to reduce verbose output to relevant lines
        const char *thisStart = ch;
        int jline = 0;  // line from this jump point
        while(ch<eof && (jline<JUMPLINES || j==nJumps-1)) {  // nJumps==1 implies sample all of input to eof; last jump to eof too
            const char *jlineStart = ch;
            if (sep==' ') while (ch<eof && *ch==' ') ch++;  // multiple sep=' ' at the jlineStart does not mean sep(!)
            skip_white(&ch);  // solely to detect blank lines, otherwise could leave to field processors
            if (ch>=eof || *ch==eol) {
              if (!skipEmptyLines && !fill) break;
              jlineStart = ch;  // to avoid 'Line finished early' below and get to the sampleLines++ block at the end of this while
            }
            jline++;
            int field=0;
            const char *fieldStart=ch;  // Needed outside loop for error messages below
            while (ch<eof && *ch!=eol && field<ncol) {
                //DTPRINT("<<%.*s>>", STRLIM(ch,20), ch);
                fieldStart=ch;
                while (type[field]<=CT_STRING && !(*fun[type[field]])(&ch,trash)) {
                  ch=fieldStart;
                  if (type[field]<CT_STRING) { type[field]++; bumped=true; }
                  else {
                    // the field could not be read with this quote rule, try again with next one
                    // Trying the next rule will only be successful if the number of fields is consistent with it
                    if (quoteRule<3) {
                      if (verbose)
                        DTPRINT("Bumping quote rule from %d to %d due to field %d on line %d of sampling jump %d starting <<%.*s>>\n",
                                 quoteRule, quoteRule+1, field+1, jline, j, STRLIM(fieldStart,200), fieldStart);
                      quoteRule++;
                      bumped=true;
                      ch = jlineStart;  // Try whole line again, in case it's a hangover from previous field
                      field=0;
                      continue;
                    }
                    STOP("Even quoteRule 3 was insufficient!");
                  }
                }
                //DTPRINT("%d", type[field]);
                if (ch<eof && *ch!=eol) {ch++; field++;}
            }
            if (field<ncol-1 && !fill) {
                if (ch<eof && *ch!=eol) {
                    STOP("Internal error: line has finished early but not on an eol or eof (fill=false). Please report as bug.");
                } else if (ch>jlineStart) {
                    STOP("Line has too few fields when detecting types. Use fill=TRUE to pad with NA. Expecting %d fields but found %d: <<%.*s>>", ncol, field+1, STRLIM(jlineStart,200), jlineStart);
                }
            }
            if (ch<eof) {
                if (*ch!=eol || field>=ncol) {   // the || >=ncol is for when a comma ends the line with eol straight after
                  if (field!=ncol) STOP("Internal error: Line has too many fields but field(%d)!=ncol(%d)", field, ncol);
                  STOP("Line %d from sampling jump %d starting <<%.*s>> has more than the expected %d fields. " \
                       "Separator %d occurs at position %d which is character %d of the last field: <<%.*s>>. " \
                       "Consider setting 'comment.char=' if there is a trailing comment to be ignored.",
                      jline, j, STRLIM(jlineStart,10), jlineStart, ncol, ncol, (int)(ch-jlineStart), (int)(ch-fieldStart),
                      STRLIM(fieldStart,200), fieldStart);
                }
                ch += eolLen;
            } else {
                // if very last field was quoted, check if it was completed with an ending quote ok.
                // not necessarily a problem (especially if we detected no quoting), but we test it and nice to have
                // a warning regardless of quoting rule just incase file has been inadvertently truncated
                // This warning is early at type skipping around stage before reading starts, so user can cancel early
                if (type[ncol-1]==CT_STRING && *fieldStart==quote && *(ch-1)!=quote) {
                  if (quoteRule<2) STOP("Internal error: Last field of last field should select quote rule 2");
                  DTWARN("Last field of last line starts with a quote but is not finished with a quote before end of file: <<%.*s>>",
                          STRLIM(fieldStart, 200), fieldStart);
                }
            }
            //DTPRINT("\n");
            lastRowEnd = ch; // Two reasons:  1) to get the end of the very last good row before whitespace or footer before eof
                             //               2) to check sample jumps don't overlap, otherwise double count and bad estimate
            int thisLineLen = (int)(ch-jlineStart);  // ch is now on start of next line so this includes eolLen already
            sampleLines++;
            sumLen += thisLineLen;
            sumLenSq += thisLineLen*thisLineLen;
            if (thisLineLen<minLen) minLen=thisLineLen;
            if (thisLineLen>maxLen) maxLen=thisLineLen;
        }
        sampleBytes += (size_t)(ch-thisStart);
        if (verbose && (bumped || j==0 || j==nJumps-1)) {
          DTPRINT("Type codes (jump %03d)    : ",j); printTypes(ncol);
          DTPRINT("  Quote rule %d\n", quoteRule);
        }
    }
    while (ch<eof && isspace(*ch)) ch++;
    if (ch<eof) {
      DTWARN("Found the last consistent line but text exists afterwards (discarded): <<%.*s>>", STRLIM(ch,200), ch);
    }

    int64_t estnrow=1, allocnrow=1;
    double meanLineLen=0;
    if (sampleLines<=1) {
      // column names only are present; e.g. fread("A\n")
    } else {
      size_t bytesRead = (size_t)(lastRowEnd - pos);
      meanLineLen = (double)sumLen/sampleLines;
      estnrow = CEIL(bytesRead/meanLineLen);  // only used for progress meter and verbose line below
      double sd = sqrt( (sumLenSq - (sumLen*sumLen)/sampleLines)/(sampleLines-1) );
      allocnrow = clamp_i64((int64_t)(bytesRead / fmax(meanLineLen - 2*sd, minLen)),
                            (int64_t)(1.1*estnrow), 2*estnrow);
      // sd can be very close to 0.0 sometimes, so apply a +10% minimum
      // blank lines have length 1 so for fill=true apply a +100% maximum. It'll be grown if needed.
      if (verbose) {
        DTPRINT("=====\n Sampled %d rows (handled \\n inside quoted fields) at %d jump points including middle and very end\n", sampleLines, nJumps);
        DTPRINT(" Bytes from first data row on line %d to the end of last row: %zd\n", row1Line, bytesRead);
        DTPRINT(" Line length: mean=%.2f sd=%.2f min=%d max=%d\n", meanLineLen, sd, minLen, maxLen);
        DTPRINT(" Estimated nrow: %zd / %.2f = %lld\n", bytesRead, meanLineLen, estnrow);
        DTPRINT(" Initial alloc = %lld rows (%lld + %d%%) using bytes/max(mean-2*sd,min) clamped between [1.1*estn, 2.0*estn]\n",
                 allocnrow, estnrow, (int)(100.0*allocnrow/estnrow-100.0));
      }
      if (nJumps==1) {
        if (verbose) DTPRINT(" All rows were sampled since file is small so we know nrow=%d exactly\n", sampleLines);
        estnrow = allocnrow = sampleLines;
      } else {
        if (sampleLines > allocnrow) STOP("Internal error: sampleLines(%ld) > allocnrow(%ld)", sampleLines, allocnrow);
      }
      if (args.nrowLimit<allocnrow) {
        if (verbose) DTPRINT(" Alloc limited to lower nrows=%lld passed in.\n", args.nrowLimit);
        estnrow = allocnrow = args.nrowLimit;
      }
      if (verbose) DTPRINT("=====\n");
    }

    // ********************************************************************************************
    //   Apply colClasses, select, drop and integer64
    // ********************************************************************************************
    ch = pos;
    oldType = (signed char *)malloc((size_t)ncol * sizeof(signed char));
    if (!oldType) STOP("Unable to allocate %d bytes to check user overrides of column types", ncol);
    memcpy(oldType, type, ncol) ;
    if (!userOverride(type, colNames, colNamesAnchor, ncol)) { // colNames must not be changed but type[] can be
      if (verbose) DTPRINT("Cancelled by user. userOverride() returned false.");
      freadCleanup();
      return 1;
    }
    int ndrop=0, nUserBumped=0;
    int rowSize = 0;
    int nStringCols = 0;
    int nNonStringCols = 0;
    for (int j=0; j<ncol; j++) {
      rowSize += (size[j] = typeSize[type[j]]);
      if (type[j]==CT_DROP) { ndrop++; continue; }
      if (type[j]<oldType[j])
        STOP("Attempt to override column %d <<%.*s>> of inherent type '%s' down to '%s' which will lose accuracy. " \
             "If this was intended, please coerce to the lower type afterwards. Only overrides to a higher type are permitted.",
             j+1, colNames[j].len, colNamesAnchor+colNames[j].off, typeName[oldType[j]], typeName[type[j]]);
      nUserBumped += type[j]>oldType[j];
      if (type[j] == CT_STRING) nStringCols++; else nNonStringCols++;
    }
    if (verbose) {
      DTPRINT("After %d type and %d drop user overrides : ", nUserBumped, ndrop);
      printTypes(ncol); DTPRINT("\n");
    }
    double tColType = wallclock();

    // ********************************************************************************************
    //   Allocate the result columns
    // ********************************************************************************************
    if (verbose) DTPRINT("Allocating %d column slots (%d - %d dropped)\n", ncol-ndrop, ncol, ndrop);
    size_t DTbytes = allocateDT(type, size, ncol, ndrop, allocnrow);
    double tAlloc = wallclock();

    // ********************************************************************************************
    //   madvise sequential
    // ********************************************************************************************
    // Read ahead and drop behind each point as they move through (assuming it's on a per thread basis).
    // Considered it but when processing string columns the buffers point to offsets in the mmp'd pages
    // which are revisited when writing the finished buffer to DT. So, it isn't sequential.
    // if (fnam!=NULL) {
    //   #ifdef MADV_SEQUENTIAL  // not on Windows. PrefetchVirtualMemory from Windows 8+ ?
    //   int ret = madvise((void *)mmp, (size_t)fileSize, MADV_SEQUENTIAL);
    //   #endif
    // }

    // ********************************************************************************************
    //   Read the data
    // ********************************************************************************************
    ch = pos;   // back to start of first data row
    int hasPrinted=0;  // the percentage last printed so it prints every 2% without many calls to wallclock()
    _Bool stopTeam=false, firstTime=true;  // _Bool for MT-safey (cannot ever read half written _Bool value)
    int nTypeBump=0, nTypeBumpCols=0;
    double tRead=0, tReread=0, tTot=0;  // overall timings outside the parallel region
    double thNextGoodLine=0, thRead=0, thPush=0;  // reductions of timings within the parallel region
    char *typeBumpMsg=NULL;  size_t typeBumpMsgSize=0;
    #define stopErrSize 1000
    char stopErr[stopErrSize+1]="";  // must be compile time size: the message is generated and we can't free before STOP
    int64_t DTi=0;   // the current row number in DT that we are writing to
    const char *prevJumpEnd = pos;  // the position after the last line the last thread processed (for checking)
    size_t workSize = 0;
    int buffGrown=0;
    size_t chunkBytes = umax((size_t)(1000*meanLineLen), 1ULL/*MB*/ *1024*1024);
    // chunkBytes is the distance between each jump point; it decides the number of jumps
    // We may want each chunk to write to its own page of the final column, hence 1000*maxLen
    // For the 44GB file with 12875 columns, the max line len is 108,497. We may want each chunk to write to its
    // own page (4k) of the final column, hence 1000 rows of the smallest type (4 byte int) is just
    // under 4096 to leave space for R's header + malloc's header.
    if (nJumps/*from sampling*/>1) {
      // ensure data size is split into same sized chunks (no remainder in last chunk) and a multiple of nth
      // when nth==1 we still split by chunk for consistency (testing) and code sanity
      nJumps = (int)((size_t)(lastRowEnd-pos)/chunkBytes);  // (int) rounds down
      if (nJumps==0) nJumps=1;
      else if (nJumps>nth) nJumps = nth*(1+(nJumps-1)/nth);
      chunkBytes = (size_t)((lastRowEnd-pos)/nJumps);
    } else {
      nJumps = 1;
    }
    int64_t initialBuffRows = allocnrow / nJumps;
    if (initialBuffRows > INT32_MAX) STOP("Buffer size %lld is too large\n", initialBuffRows);
    nth = imin(nJumps, nth);

    read:  // we'll return here to reread any columns with out-of-sample type exceptions
    #pragma omp parallel num_threads(nth)
    {
      int me = omp_get_thread_num();
      #pragma omp master
      nth = omp_get_num_threads();
      const char *thisJumpStart=NULL;  // The first good start-of-line after the jump point
      int64_t myDTi=0;  // which row in the final DT result I should start writing my chunk to
      int myNrow=0; // the number of rows in my chunk

      // Allocate thread-private row-major myBuff
      int myBuffRows = (int) initialBuffRows;  // Upon realloc, myBuffRows will increase to grown capacity
      char *myBuff = malloc((size_t)(rowSize*myBuffRows + 8)); // +8 for Field() to write to when CT_DROP is at the end and buffer is full
      if (!myBuff) stopTeam=true;
      #pragma omp master
      workSize += (size_t)(nth * rowSize * myBuffRows);

      #pragma omp for ordered schedule(dynamic) reduction(+:thNextGoodLine,thRead,thPush)
      for (int jump=0; jump<nJumps+nth; jump++) {
        if (stopTeam) continue;
        double tt0 = 0, tt1 = 0;
        if (verbose) { tt1 = tt0 = wallclock(); }

        if (myNrow) {
          // On the 2nd iteration onwards for this thread, push the data from the previous jump
          // Convoluted because the ordered section has to be last in some OpenMP implementations :
          // http://stackoverflow.com/questions/43540605/must-ordered-be-at-the-end
          // Hence why loop goes to nJumps+nth. Logically, this clause belongs after the ordered section.

          // Push buffer now to impl so that :
          //   i) lenoff.off can be "just" 32bit int from a local anchor rather than a 64bit offset from a global anchor
          //  ii) impl can do it in parallel if it wishes, otherwise it can have an orphan critical directive
          // iii) myBuff is hot, so this is the best time to transpose it to result, and first time possible as soon
          //      as we know the previous jump's number of rows.
          //  iv) so that myBuff can be small
          pushBuffer(myBuff, /*anchor=*/thisJumpStart, myNrow, myDTi, rowSize, nStringCols, nNonStringCols);
          if (verbose) { tt1 = wallclock(); thPush += tt1 - tt0; tt0 = tt1; }

          if (me==0 && (hasPrinted || (args.showProgress && jump/nth==4 &&
                                      ((double)nJumps/(nth*3)-1.0)*(wallclock()-tAlloc)>3.0))) {
            // Important for thread safety inside progess() that this is called not just from critical but that
            // it's the master thread too, hence me==0.
            // Jump 0 might not be assigned to thread 0; jump/nth==4 to wait for 4 waves to complete then decide once.
            int p = (int)(100.0*jump/nJumps);
            if (p>=hasPrinted) {
              // ETA TODO. Ok to call wallclock() now.
              progress(p, /*eta*/0);
              hasPrinted = p+2;  // update every 2%
            }
          }
          myNrow = 0;
        }
        if (jump>=nJumps) continue;  // nothing left to do. This jump was the dummy extra one.

        const char *tch = pos + (size_t)jump*chunkBytes;
        const char *nextJump = jump<nJumps-1 ? tch+chunkBytes+eolLen : lastRowEnd;
        // +eolLen is for when nextJump happens to fall exactly on a line start (or on eol2 on Windows). The
        // next thread will start one line later because nextGoodLine() starts by finding next eol
        // Easier to imagine eolLen==1 and tch<=nextJump in the while() below
        if (jump>0 && !nextGoodLine(&tch, ncol)) {
          stopTeam=true;
          DTPRINT("No good line could be found from jump point %d\n",jump); // TODO: change to stopErr
          continue;
        }
        thisJumpStart=tch;
        if (verbose) { tt1 = wallclock(); thNextGoodLine += tt1 - tt0; tt0 = tt1; }

        char *myBuffPos = myBuff;
        while (tch<nextJump) {
          if (myNrow == myBuffRows) {
            // buffer full due to unusually short lines in this chunk vs the sample; e.g. #2070
            myBuffRows *= 1.5;
            #pragma omp atomic
            buffGrown++;
            size_t diff = (size_t)(myBuffPos - myBuff);
            if (!(myBuff = realloc(myBuff, (size_t)(myBuffRows*rowSize + 8)))) {
              stopTeam=true;
              break;
            } else {
              myBuffPos = myBuff + diff;  // restore myBuffPos in case myBuff was moved by realloc
            }
          }
          const char *tlineStart = tch;  // for error message
          if (sep==' ') while (tch<eof && *tch==' ') tch++;  // multiple sep=' ' at the tlineStart does not mean sep(!)
          skip_white(&tch);  // solely for blank lines otherwise could leave to field processors which handle leading white
          if (tch>=eof || *tch==eol) {
            if (skipEmptyLines) { tch+=eolLen; continue; }
            else if (!fill) {
              #pragma omp critical
              if (!stopTeam) {
                stopTeam = true;
                snprintf(stopErr, stopErrSize,
                  "Row %lld is empty. It is outside the sample rows. "
                  "Set fill=true to treat it as an NA row, or blank.lines.skip=true to skip it",
                  myDTi + myNrow);
                  // TODO - include a few (numbered) lines before and after in the message.
              }
              break;
            }
          }
          int j=0;
          while (j<ncol) {
            // DTPRINT("Field %d: '%.10s' as type %d\n", j+1, tch, type[j]);
            const char *fieldStart = tch;
            int8_t joldType = type[j];   // fetch shared type once. Cannot read half-written byte.
            int8_t thisType = joldType;  // to know if it was bumped in (rare) out-of-sample type exceptions
            // always write to buffPos even when CT_DROP. It'll just overwrite on next non-CT_DROP
            while (!fun[abs(thisType)](&tch, myBuffPos)) {
              // normally returns success(1) and myBuffPos is assigned inside *fun.
              thisType = thisType<0 ? thisType-1 : -thisType-1;
              // guess is insufficient out-of-sample, type is changed to negative sign and then bumped. Continue to
              // check that the new type is sufficient for the rest of the column to be sure a single re-read will work.
              tch = fieldStart;
            }
            if (joldType == CT_STRING) ((lenOff *)myBuffPos)->off += (size_t)(fieldStart-thisJumpStart);
            else if (thisType != joldType) {  // rare out-of-sample type exception
              #pragma omp critical
              {
                joldType = type[j];  // fetch shared value again in case another thread bumped it while I was waiting.
                // Can't PRINT because we're likely not master. So accumulate message and print afterwards.
                if (thisType < joldType) {   // thisType<0 (type-exception)
                  char temp[1001];
                  int len = snprintf(temp, 1000,
                    "Column %d (\"%.*s\") bumped from '%s' to '%s' due to <<%.*s>> on row %lld\n",
                    j+1, colNames[j].len, colNamesAnchor + colNames[j].off,
                    typeName[abs(joldType)], typeName[abs(thisType)],
                    (int)(tch-fieldStart), fieldStart, myDTi+myNrow);
                  typeBumpMsg = realloc(typeBumpMsg, typeBumpMsgSize + (size_t)len + 1);
                  strcpy(typeBumpMsg+typeBumpMsgSize, temp);
                  typeBumpMsgSize += (size_t)len;
                  nTypeBump++;
                  if (joldType>0) nTypeBumpCols++;
                  type[j] = thisType;
                } // else other thread bumped to a (negative) higher or equal type, so do nothing
              }
            }
            myBuffPos += size[j++];
            if (tch>=eof || *tch==eol) break;
            tch++;
          }
          if (j<ncol)  {
            // not enough columns observed
            if (!fill) {
              #pragma omp critical
              if (!stopTeam) {
                stopTeam = true;
                snprintf(stopErr, stopErrSize,
                  "Expecting %d cols but row %lld contains only %d cols (sep='%c'). " \
                  "Consider fill=true. <<%.*s>>",
                  ncol, myDTi, j, sep, STRLIM(tlineStart, 500), tlineStart);
              }
              break;
            }
            while (j<ncol) {
              switch (type[j]) {
              case CT_BOOL8:
                *(int8_t *)myBuffPos = NA_BOOL8;
                break;
              case CT_INT32_BARE:
              case CT_INT32_FULL:
                *(int32_t *)myBuffPos = NA_INT32;
                break;
              case CT_INT64:
                *(int64_t *)myBuffPos = NA_INT64;
                break;
              case CT_FLOAT64:
                *(double *)myBuffPos = NA_FLOAT64;
                break;
              case CT_STRING:
                ((lenOff *)myBuffPos)->len = blank_is_a_NAstring ? INT8_MIN : 0;
                ((lenOff *)myBuffPos)->off = 0;
                break;
              default:
                break;
              }
              myBuffPos += size[j++];
            }
          }
          if (tch<eof && *tch!=eol) {
            #pragma omp critical
            if (!stopTeam) {
              stopTeam = true;
              snprintf(stopErr, stopErrSize,
                "Too many fields on out-of-sample row %lld. Read all %d expected columns but more are present. <<%.*s>>",
                myDTi, ncol, STRLIM(tlineStart, 500), tlineStart);
            }
            break;
          }
          tch+=eolLen;
          myNrow++;
        }
        if (verbose) { tt1 = wallclock(); thRead += tt1 - tt0; tt0 = tt1; }

        #pragma omp ordered
        {
          // stopTeam could be true if a previous thread already stopped while I was waiting my turn
          if (!stopTeam && prevJumpEnd != thisJumpStart) {
            snprintf(stopErr, stopErrSize,
              "Jump %d did not finish counting rows exactly where jump %d found its first good line start: "
              "prevEnd(%p)<<%.*s>> != thisStart(prevEnd%+d)<<%.*s>>",
              jump-1, jump, (const void*)prevJumpEnd, STRLIM(prevJumpEnd,50), prevJumpEnd,
              (int)(thisJumpStart-prevJumpEnd), STRLIM(thisJumpStart,50), thisJumpStart);
            stopTeam=true;
          }
          myDTi = DTi;  // fetch shared DTi (where to write my results to the answer). The previous thread just told me.
          if (myDTi>=args.nrowLimit) {
            // nrowLimit was supplied and a previous thread reached that limit while I was counting my rows
            stopTeam=true;
          }
          myNrow = (int) umin((size_t)myNrow, (size_t)(args.nrowLimit-myDTi)); // for the last jump that reaches nrowLimit
          // tell next thread 2 things :
          prevJumpEnd = tch; // i) the \n I finished on so it can check (above) it started exactly on that \n good line start
          DTi += myNrow;     // ii) which row in the final result it should start writing to. As soon as I know myNrow.
        }
        // END ORDERED.
        // Next thread can now start its ordered section and write its results to the final DT at the same time as me.
        // Ordered has to be last in some OpenMP implementations currently. Logically though, pushBuffer happens now.
      }
      // Each thread to free its own buffer.
      free(myBuff); myBuff=NULL;
    }
    // end parallel
    if (firstTime) {
      tReread = tRead = wallclock();
      tTot = tRead-t0;
      if (hasPrinted || verbose) {
        DTPRINT("\rRead %lld rows x %d columns from %.3fGB file in ", DTi, ncol-ndrop, 1.0*fileSize/(1024*1024*1024));
        DTPRINT("%02d:%06.3f ", (int)tTot/60, fmod(tTot,60.0));
        DTPRINT("wall clock time (can be slowed down by any other open apps even if seemingly idle)\n");
        // since parallel, clock() cycles is parallel too: so wall clock will have to do
      }
      // not-bumped columns are assigned type -CT_STRING in the rerun, so we have to count types now
      if (verbose) {
        DTPRINT("Thread buffers were grown %d times (if all %d threads each grew once, this figure would be %d)\n",
                 buffGrown, nth, nth);
        int typeCounts[NUMTYPE];
        for (int i=0; i<NUMTYPE; i++) typeCounts[i] = 0;
        for (int i=0; i<ncol; i++) typeCounts[ abs(type[i]) ]++;
        DTPRINT("Final type counts\n");
        for (int i=0; i<NUMTYPE; i++) DTPRINT("%10d : %-9s\n", typeCounts[i], typeName[i]);
      }
      if (nTypeBump) {
        if (hasPrinted || verbose) DTPRINT("Rereading %d columns due to out-of-sample type exceptions.\n", nTypeBumpCols);
        if (verbose) DTPRINT("%s", typeBumpMsg);
        // TODO - construct and output the copy and pastable colClasses argument so user can avoid the reread time in future.
        free(typeBumpMsg);
      }
    } else {
      tReread = wallclock();
      tTot = tReread-t0;
      if (hasPrinted || verbose) {
        DTPRINT("\rReread %lld rows x %d columns in ", DTi, nTypeBumpCols);
        DTPRINT("%02d:%06.3f\n", (int)(tReread-tRead)/60, fmod(tReread-tRead,60.0));
      }
    }
    if (stopTeam && stopErr[0]!='\0') STOP(stopErr); // else nrowLimit applied and stopped early normally
    if (DTi > allocnrow) {
      if (args.nrowLimit>allocnrow) STOP("Internal error: DTi(%lld)>allocnrow(%lld) but nrows=%lld (not limited)",
                                         DTi, allocnrow, args.nrowLimit);
      // for the last jump that fills nrow limit, then ansi is +=buffi which is >allocnrow and correct
    } else if (DTi == allocnrow) {
      if (verbose) DTPRINT("Read %lld rows. Exactly what was estimated and allocated up front\n", DTi);
    } else {
      allocnrow = DTi;
    }
    setFinalNrow(DTi);
    if (firstTime && nTypeBump) {
      rowSize = 0;
      nStringCols = 0;
      nNonStringCols = 0;
      for (int j=0, resj=-1; j<ncol; j++) {
        size[j] = 0;
        if (type[j] == CT_DROP) continue;
        resj++;
        if (type[j]<0) {
          // column was bumped due to out-of-sample type exception
          int newType = type[j] *= -1;   // final type for this column
          reallocColType(resj, newType);
          rowSize += (size[j] = typeSize[newType]);
          if (type[j] == CT_STRING) nStringCols++; else nNonStringCols++;
        } else if (type[j]>=1) {
          // we'll skip over non-bumped columns in the rerun, whilst still incrementing resi (hence not CT_DROP)
          // not -type[i] either because that would reprocess the contents of not-bumped columns wastefully
          type[j] = -CT_STRING;
        }
      }
      // reread from the beginning
      DTi = 0;
      prevJumpEnd = ch = pos;
      firstTime = false;
      goto read;
    }
    if (verbose) {
      DTPRINT("=============================\n");
      if (tTot<0.000001) tTot=0.000001;  // to avoid nan% output in some trivially small tests where tot==0.000s
      DTPRINT("%8.3fs (%3.0f%%) Memory map %.3fGB file\n", tMap-t0, 100.0*(tMap-t0)/tTot, 1.0*fileSize/(1024*1024*1024));
      DTPRINT("%8.3fs (%3.0f%%) sep=", tLayout-tMap, 100.0*(tLayout-tMap)/tTot);
        DTPRINT(sep=='\t' ? "'\\t'" : (sep=='\n' ? "'\\n'" : "'%c'"), sep);
        DTPRINT(" ncol=%d and header detection\n", ncol);
      DTPRINT("%8.3fs (%3.0f%%) Column type detection using %d sample rows\n",
        tColType-tLayout, 100.0*(tColType-tLayout)/tTot, sampleLines);
      DTPRINT("%8.3fs (%3.0f%%) Allocation of %lld rows x %d cols (%.3fGB)\n",
        tAlloc-tColType, 100.0*(tAlloc-tColType)/tTot, allocnrow, ncol, DTbytes/(1024.0*1024*1024));
      thNextGoodLine/=nth; thRead/=nth; thPush/=nth;
      double thWaiting = tRead-tAlloc-thNextGoodLine-thRead-thPush;
      DTPRINT("%8.3fs (%3.0f%%) Reading %d chunks of %.3fMB (%d rows) using %d threads\n",
        tRead-tAlloc, 100.0*(tRead-tAlloc)/tTot, nJumps, (double)chunkBytes/(1024*1024), (int)(chunkBytes/meanLineLen), nth);
      DTPRINT("   = %8.3fs (%3.0f%%) Finding first non-embedded \\n after each jump\n", thNextGoodLine, 100.0*thNextGoodLine/tTot);
      DTPRINT("   + %8.3fs (%3.0f%%) Parse to row-major thread buffers\n", thRead, 100.0*thRead/tTot);
      DTPRINT("   + %8.3fs (%3.0f%%) Transpose\n", thPush, 100.0*thPush/tTot);
      DTPRINT("   + %8.3fs (%3.0f%%) Waiting\n", thWaiting, 100.0*thWaiting/tTot);
      DTPRINT("%8.3fs (%3.0f%%) Rereading %d columns due to out-of-sample type exceptions\n",
        tReread-tRead, 100.0*(tReread-tRead)/tTot, nTypeBumpCols);
      DTPRINT("%8.3fs        Total\n", tTot);
    }
    freadCleanup();
    return 1;
}

