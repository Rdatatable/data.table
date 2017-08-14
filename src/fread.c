#if defined(CLOCK_REALTIME) && !defined(DISABLE_CLOCK_REALTIME)
#define HAS_CLOCK_REALTIME
#endif

#ifdef HAS_CLOCK_REALTIME
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

// On Windows variables of type `size_t` cannot be printed with "%zu" in the
// `snprintf()` function. For those variables we will cast them into
// `long long int` before printing; and this #define makes it slightly simpler.
#define llint long long int


// Private globals to save passing all of them through to highly iterated field processors
static const char *eof;
static char sep, eol, eol2;
static char whiteChar; // what to consider as whitespace to skip: ' ', '\t' or 0 means both (when sep!=' ' && sep!='\t')
static int eolLen;
static char quote, dec;
static char finalByte;

// Quote rule:
//   0 = Fields may be quoted, any quote inside the field is doubled. This is
//       the CSV standard. For example: <<...,"hello ""world""",...>>
//   1 = Fields may be quoted, any quotes inside are escaped with a backslash.
//       For example: <<...,"hello \"world\"",...>>
//   2 = Fields may be quoted, but any quotes inside will appear verbatim and
//       not escaped in any way. It is not always possible to parse the file
//       unambiguously, but we give it a try anyways. A quote will be presumed
//       to mark the end of the field iff it is followed by the field separator.
//       Under this rule eol characters cannot appear inside the field.
//       For example: <<...,"hello "world"",...>>
//   3 = Fields are not quoted at all. Any quote characters appearing anywhere
//       inside the field will be treated as any other regular characters.
//       Example: <<...,hello "world",...>>
//
static int quoteRule;
static const char* const* NAstrings;
static _Bool any_number_like_NAstrings=false;
static _Bool blank_is_a_NAstring=false;
static _Bool stripWhite=true;  // only applies to character columns; numeric fields always stripped
static _Bool skipEmptyLines=false, fill=false;

static double NA_FLOAT64;  // takes fread.h:NA_FLOAT64_VALUE

#define JUMPLINES 100    // at each of the 100 jumps how many lines to guess column types (10,000 sample lines)

// Private globals so they can be cleaned up both on error and on successful return
static void *mmp = NULL;
static size_t fileSize;
static int8_t *type = NULL, *size = NULL;
static lenOff *colNames = NULL;
static int8_t *oldType = NULL;
static freadMainArgs args;  // global for use by DTPRINT

const char typeName[NUMTYPE][10] = {"drop", "bool8", "int32", "int32", "int64", "float64", "string"};
int8_t     typeSize[NUMTYPE]     = { 0,      1,       4,       4,       8,       8,         8      };

// NAN and INFINITY constants are float, so cast to double once up front.
static const double NAND = (double)NAN;
static const double INFD = (double)INFINITY;

// Forward declarations
static int Field(const char **this, lenOff *target);


//=================================================================================================
//
//   Utility functions
//
//=================================================================================================

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
  free(type); type = NULL;
  free(size); size = NULL;
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
  }
  fileSize = 0;
  sep = whiteChar = eol = eol2 = quote = dec = finalByte = '\0';
  eolLen = 0;
  quoteRule = -1;
  any_number_like_NAstrings = false;
  blank_is_a_NAstring = false;
  stripWhite = true;
  skipEmptyLines = false;
  fill = false;
  // following are borrowed references: do not free
  eof = NULL;
  NAstrings = NULL;
}


#define CEIL(x)  ((size_t)(double)ceil(x))
static inline size_t umax(size_t a, size_t b) { return a > b ? a : b; }
static inline size_t umin(size_t a, size_t b) { return a < b ? a : b; }
static inline int imin(int a, int b) { return a < b ? a : b; }

/** Return value of `x` clamped to the range [upper, lower] */
static inline size_t clamp_szt(size_t x, size_t lower, size_t upper) {
  return x < lower ? lower : x > upper? upper : x;
}


/**
 * Helper for error and warning messages to extract an input line starting at
 * `*ch` and until an end of line, but no longer than `limit` characters.
 * This function returns the string copied into an internal static buffer. Cannot
 * be called more than twice per single printf() invocation.
 * Parameter `limit` cannot exceed 500.
 */
static const char* strlim(const char *ch, size_t limit) {
  static char buf[1002];
  static int flip = 0;
  size_t maxwidth = umin(umin(limit, 500), (size_t)(eof - ch));
  char *newline = memchr(ch, eol, maxwidth);
  if (newline) maxwidth = (size_t)(newline - ch);
  char *ptr = buf + 501 * flip;
  flip = 1 - flip;
  strncpy(ptr, ch, maxwidth);
  ptr[maxwidth] = '\0';
  return ptr;
}


static void printTypes(int ncol) {
  // e.g. files with 10,000 columns, don't print all of it to verbose output.
  int tt=(ncol<=110?ncol:90); for (int i=0; i<tt; i++) DTPRINT("%d",type[i]);
  if (ncol>110) { DTPRINT("..."); for (int i=ncol-10; i<ncol; i++) DTPRINT("%d",type[i]); }
}

static inline void skip_white(const char **this) {
  // skip space so long as sep isn't space and skip tab so long as sep isn't tab
  const char *ch = *this;
  if (whiteChar == 0) {   // whiteChar==0 means skip both ' ' and '\t';  sep is neither ' ' nor '\t'.
    while (*ch == ' ' || *ch == '\t') ch++;
  } else {
    while (*ch == whiteChar) ch++;  // sep is ' ' or '\t' so just skip the other one.
  }
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

/**
 * Compute the number of fields on the current line (taking into account the
 * global `sep`, `eol` and `quoteRule`), and move the parsing location to the
 * beginning of the next line.
 * Returns the number of fields on the current line, or -1 if the line cannot
 * be parsed using current settings.
 * This does not need to be particularly efficient; it's just used for format detection.
 */
static inline int countfields(const char **this)
{
  static lenOff trash;  // see comment on other trash declarations
  const char *ch = *this;
  if (sep==' ') while (ch<eof && *ch==' ') ch++;  // multiple sep==' ' at the start does not mean sep
  skip_white(&ch);
  int ncol = 0;
  if (ch<eof && *ch==eol) {
    ch+=eolLen;
  } else while (ch<eof) {
    if (Field(&ch, &trash)) return -1;   // -1 means this line not valid for this sep and quote rule
    // Field() leaves *ch resting on sep, eol or >=eof. Checked inside Field().
    ncol++;
    if (*ch==eol) { ch+=eolLen; break; }
    ch+=(*ch==sep);  // move over sep (which will already be last ' ' if sep=' ').
  }
  if (ch==eof && finalByte && (finalByte==sep || ncol==0 || eof[-1]==sep)) ncol++;
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


double wallclock(void)
{
    double ans = 0;
#ifdef HAS_CLOCK_REALTIME
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


/**
 * Helper function to print file's size in human-readable format. This will
 * produce strings such as:
 *     44.74GB (48043231704 bytes)
 *     921MB (965757797 bytes)
 *     2.206MB (2313045 bytes)
 *     38.69KB (39615 bytes)
 *     214 bytes
 *     0 bytes
 * The function returns a pointer to a static string buffer, so the caller
 * should not attempt to deallocate the buffer, or call this function from
 * multiple threads at the same time, or hold on to the value returned for
 * extended periods of time.
 */
static char* filesize_to_str(size_t fsize)
{
  #define NSUFFIXES 4
  #define BUFFSIZE 100
  static char suffixes[NSUFFIXES] = {'T', 'G', 'M', 'K'};
  static char output[BUFFSIZE];
  llint isize = (llint) fsize;
  for (int i = 0; i <= NSUFFIXES; i++) {
    int shift = (NSUFFIXES - i) * 10;
    if ((fsize >> shift) == 0) continue;
    int ndigits = 3;
    for (; ndigits >= 1; ndigits--) {
      if ((fsize >> (shift + 12 - ndigits * 3)) == 0) break;
    }
    if (ndigits == 0 || (fsize == (fsize >> shift << shift))) {
      if (i < NSUFFIXES) {
        snprintf(output, BUFFSIZE, "%lld%cB (%lld bytes)",
                 isize >> shift, suffixes[i], isize);
        return output;
      }
    } else {
      snprintf(output, BUFFSIZE, "%.*f%cB (%lld bytes)",
               ndigits, (double)fsize / (1 << shift), suffixes[i], isize);
      return output;
    }
  }
  if (fsize == 1) return "1 byte";
  snprintf(output, BUFFSIZE, "%lld bytes", isize);
  return output;
}



//=================================================================================================
//
//   Field parsers
//
//=================================================================================================

static int Field(const char **this, lenOff *target)
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
      if (*ch==quote || (ch==eof && finalByte==quote)) break;   // we can safely reference *eof which is always '\0'
      return 1;
    case 1:  // quoted with embedded quotes escaped; the final unescaped " must be followed by sep|eol
      while (++ch<eof && *ch!=quote && eolCount<100) {
        eolCount += (*ch==eol);
        ch += (*ch=='\\' && ch<eof-1);
      }
      if (*ch==quote || (ch==eof && finalByte==quote)) break;
      return 1;
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
      return 1;  // Internal error: undefined quote rule
    }
  }
  int fieldLen = (int)(ch-fieldStart);
  if (stripWhite && !quoted) {
    while(fieldLen>0 && (fieldStart[fieldLen-1]==' ' || fieldStart[fieldLen-1]=='\t')) fieldLen--;
    // this white space (' ' or '\t') can't be sep otherwise it would have stopped the field earlier at the first sep
  }
  if (quoted) { ch++; if (stripWhite) skip_white(&ch); }
  if (!on_sep(&ch)) return 1;
  if (fieldLen==0) {
    if (blank_is_a_NAstring) fieldLen=INT32_MIN;
  } else {
    if (is_NAstring(fieldStart)) fieldLen=INT32_MIN;
  }
  target->len = fieldLen;
  target->off = (int32_t)(fieldStart - *this);  // agnostic & thread-safe
  *this = ch; // Update caller's ch. This may be after fieldStart+fieldLen due to quotes and/or whitespace
  return 0;
}


static int StrtoI64(const char **this, int64_t *target)
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
      *target = NA_INT64;
      *this = ch;
      return 0;
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
    if (quoted) { if (ch>=eof || *ch!=quote) return 1; else ch++; }
    // TODO: if (!targetCol) return early?  Most of the time, not though.
    *target = sign * acc;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    //DTPRINT("StrtoI64 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    *this = ch;
    if (ok && !any_number_like_NAstrings) return 0;  // most common case, return
    _Bool na = is_NAstring(start);
    if (ok && !na) return 0;
    *target = NA_INT64;
    next_sep(&ch);  // TODO: can we delete this? consume the remainder of field, if any
    *this = ch;
    return !na;
}


static int StrtoI32_bare(const char **this, int32_t *target)
{
    const char *ch = *this;
    if (*ch==sep || *ch==eol) { *target = NA_INT32; return 0; }
    if (sep==' ') return 1;  // bare doesn't do sep=' '. TODO - remove
    _Bool neg = *ch=='-';
    ch += (neg || *ch=='+');
    const char *start = ch;  // for overflow guard using field width
    uint_fast64_t acc = 0;   // using unsigned to be clear that acc will never be negative
    uint_fast8_t digit;
    while ( (digit=(uint_fast8_t)(*ch-'0'))<10 ) {  // see init.c for checks of unsigned uint_fast8_t) cast
      acc *= 10;     // optimizer has best chance here; e.g. (x<<2 + x)<<1 or x<<3 + x<<1
      acc += digit;
      ch++;
    }
    *target = neg ? -(int32_t)acc : (int32_t)acc;   // cast 64bit acc to 32bit; range checked in return below
    *this = ch;
    return (*ch!=sep && *ch!=eol) ||
           (acc ? *start=='0' || acc>INT32_MAX || (ch-start)>10 : ch-start!=1);
    // If false, both *target and where *this is moved on to, are undefined.
    // INT32 range is NA==-2147483648(INT32_MIN) then symmetric [-2147483647,+2147483647] so we can just test INT32_MAX
    // The max (2147483647) happens to be 10 digits long, hence <=10.
    // Leading 0 (such as 001 and 099 but not 0, +0 or -0) will return false and cause bump to _padded which has the
    // option to treat as integer or string with further cost. Otherwise width would not be sufficient check here in _bare.
}


static int StrtoI32_full(const char **this, int32_t *target)
{
    // Very similar to StrtoI64 (see it for comments). We can't make a single function and switch on TYPEOF(targetCol) to
    // know I64 or I32 because targetCol is NULL when testing types and when dropping columns.
    const char *ch = *this;
    skip_white(&ch);
    if (on_sep(&ch)) {  // most often ',,'
      *target = NA_INT32;
      *this = ch;
      return 0;
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
    if (quoted) { if (ch>=eof || *ch!=quote) return 1; else ch++; }
    *target = sign * acc;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    //DTPRINT("StrtoI32 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    *this = ch;
    if (ok && !any_number_like_NAstrings) return 0;
    _Bool na = is_NAstring(start);
    if (ok && !na) return 0;
    *target = NA_INT32;
    next_sep(&ch);
    *this = ch;
    return !na;
}


// generate freadLookups.h
// TODO: review ERANGE checks and tests; that range outside [1.7e-308,1.7e+308] coerces to [0.0,Inf]
/*
f = "~/data.table/src/freadLookups.h"
cat("const long double pow10lookup[701] = {\n", file=f, append=FALSE)
for (i in (-350):(349)) cat("1.0E",i,"L,\n", sep="", file=f, append=TRUE)
cat("1.0E350L\n};\n", file=f, append=TRUE)
*/

static int StrtoD(const char **this, double *target)
{
    // [+|-]N.M[E|e][+|-]E or Inf or NAN

    const char *ch = *this;
    skip_white(&ch);
    if (on_sep(&ch)) {
      *target = NA_FLOAT64;
      *this = ch;
      return 0;
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
      d = (unsigned)(e + 350) <= 700 ? (double)(sign * (long double)acc * pow10lookup[350+e])
          : e < -350 ? 0 : sign * INFD;
    }
    if (quoted) { if (ch>=eof || *ch!=quote) return 1; else ch++; }
    *target = d;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    *this = ch;
    if (ok && !any_number_like_NAstrings) return 0;
    _Bool na = is_NAstring(start);
    if (ok && !na) return 0;
    *target = NA_FLOAT64;
    next_sep(&ch);
    *this = ch;
    return !na;
}

static int StrtoB(const char **this, int8_t *target)
{
    // These usually come from R when it writes out.
    const char *ch = *this;
    skip_white(&ch);
    *target = NA_BOOL8;
    if (on_sep(&ch)) { *this=ch; return 0; }  // empty field ',,'
    const char *start=ch;
    _Bool quoted = false;
    if (ch<eof && (*ch==quote)) { quoted=true; ch++; }
    if (quoted && *ch==quote) { ch++; if (on_sep(&ch)) {*this=ch; return 0;} else return 1; }  // empty quoted field ',"",'
    _Bool logical01 = false;  // expose to user and should default be true?
    if ( ((*ch=='0' || *ch=='1') && logical01) || (*ch=='N' && ch+1<eof && *(ch+1)=='A' && ch++)) {
        *target = (*ch=='1' ? 1 : (*ch=='0' ? 0 : NA_BOOL8));
        ch++;
    } else if (*ch=='T' || *ch=='t') {
        *target = 1;
        if ((ch+3<eof && ch[1]=='R' && ch[2]=='U' && ch[3]=='E') ||
            (ch+3<eof && ch[1]=='r' && ch[2]=='u' && ch[3]=='e')) ch += 4;
    } else if (*ch=='F' || *ch=='f') {
        *target = 0;
        if ((ch+4<eof && ch[1] == 'A' && ch[2] == 'L' && ch[3] == 'S' && ch[4] == 'E') ||
            (ch+4<eof && ch[1] == 'a' && ch[2] == 'l' && ch[3] == 's' && ch[4] == 'e')) ch += 5;
    }
    if (quoted) { if (ch>=eof || *ch!=quote) return 1; else ch++; }
    if (on_sep(&ch)) { *this=ch; return 0; }
    *target = NA_BOOL8;
    next_sep(&ch);
    *this=ch;
    return !is_NAstring(start);
}

typedef int (*reader_fun_t)(const char **ptr, void *target);
static reader_fun_t fun[NUMTYPE] = {
  (reader_fun_t) &Field,
  (reader_fun_t) &StrtoB,
  (reader_fun_t) &StrtoI32_bare,
  (reader_fun_t) &StrtoI32_full,
  (reader_fun_t) &StrtoI64,
  (reader_fun_t) &StrtoD,
  (reader_fun_t) &Field
};



//=================================================================================================
//
// Main fread() function that does all the job of reading a text/csv file.
//
// Returns 1 if it finishes successfully, and 0 otherwise.
//
//=================================================================================================
int freadMain(freadMainArgs _args) {
    args = _args;  // assign to global for use by DTPRINT() in other functions
    double t0 = wallclock();

    //*********************************************************************************************
    // [1] Extract the arguments and check their validity
    //*********************************************************************************************
    _Bool verbose = args.verbose;
    _Bool warningsAreErrors = args.warningsAreErrors;
    if (verbose) DTPRINT("[1] Check arguments\n");

    if (mmp || colNames || oldType || type || size) {
      STOP("Internal error: Previous fread() session was not cleaned up properly");
    }

    int nth = args.nth;
    {
      int maxth = omp_get_max_threads();
      if (nth > maxth) nth = maxth;
      if (nth <= 0) nth += maxth;
      if (nth <= 0) nth = 1;
      if (verbose) DTPRINT("  Using %d threads (omp_get_max_threads()=%d, nth=%d)\n", nth, maxth, args.nth);
    }

    uint64_t ui64 = NA_FLOAT64_I64;
    memcpy(&NA_FLOAT64, &ui64, 8);

    size_t nrowLimit = (size_t) args.nrowLimit;
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
        DTPRINT("  No NAstrings provided.\n");
      } else {
        DTPRINT("  NAstrings = [");
        const char * const* s = NAstrings;
        while (*s++) DTPRINT(*s? "<<%s>>, " : "<<%s>>", s[-1]);
        DTPRINT("]\n");
        if (any_number_like_NAstrings)
          DTPRINT("  One or more of the NAstrings looks like a number.\n");
        else
          DTPRINT("  None of the NAstrings look like numbers.\n");
      }
      if (args.skipNrow) DTPRINT("  skip num lines = %lld\n", args.skipNrow);
      if (args.skipString) DTPRINT("  skip to string = <<%s>>\n", args.skipString);
      DTPRINT("  show progress = %d\n", args.showProgress);
    }

    stripWhite = args.stripWhite;
    skipEmptyLines = args.skipEmptyLines;
    fill = args.fill;
    dec = args.dec;
    quote = args.quote;
    if (args.sep == quote && quote!='\0') STOP("sep == quote ('%c') is not allowed", quote);
    if (dec=='\0') STOP("dec='' not allowed. Should be '.' or ','");
    if (args.sep == dec) STOP("sep == dec ('%c') is not allowed", dec);
    if (quote == dec) STOP("quote == dec ('%c') is not allowed", dec);

    // File parsing context: pointer to the start of file, and to the end of
    // the file. The `sof` pointer may be shifted in order to skip over
    // "irrelevant" parts: the BOM mark, the banner, the headers, the skipped
    // lines, etc. Similarly, `eof` may be adjusted to take out the footer of
    // the file.
    const char *sof = NULL;
    // const char *eof = NULL;
    const char *ch = NULL;


    //*********************************************************************************************
    // [2] Open and memory-map the input file, setting up the parsing context
    //     (sof, eof, ch).
    //*********************************************************************************************
    if (verbose) DTPRINT("[2] Opening the file\n");
    mmp = NULL;
    finalByte = '\0';    // finalByte only ever !='\0' for files and when the file's final data line is ended abrubtly without last line ending
    if (args.input) {
        if (verbose) DTPRINT("`input` argument is given, interpreting as raw text to read\n");
        sof = args.input;
        fileSize = strlen(sof);
        eof = sof+fileSize;
        if (*eof!='\0') STOP("Internal error: last byte of character input isn't \\0");
    } else
    if (args.filename) {
        if (verbose) DTPRINT("  Opening file %s\n", args.filename);
        const char* fnam = args.filename;
#ifndef WIN32
        int fd = open(fnam, O_RDONLY);
        if (fd==-1) STOP("file not found: %s",fnam);
        struct stat stat_buf;
        if (fstat(fd, &stat_buf) == -1) {
          close(fd);
          STOP("Opened file ok but couldn't obtain its size: %s", fnam);
        }
        fileSize = (size_t) stat_buf.st_size;
        if (fileSize == 0) {close(fd); STOP("File is empty: %s", fnam);}
        if (verbose) {
            DTPRINT("  File opened, size = %s.\n", filesize_to_str(fileSize));
            DTPRINT("  Memory mapping ... ");
        }

        // No MAP_POPULATE for faster nrows=10 and to make possible earlier progress bar in row count stage
        // Mac doesn't appear to support MAP_POPULATE anyway (failed on CRAN when I tried).
        // TO DO?: MAP_HUGETLB for Linux but seems to need admin to setup first. My Hugepagesize is 2MB (>>2KB, so promising)
        //         https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
        mmp = mmap(NULL, fileSize, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);  // COW for last page finalByte
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
            DTPRINT("  File opened, size = %s.\n", filesize_to_str(fileSize));
            DTPRINT("  Memory mapping ... ");
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
          int nbit = 8*sizeof(char *);
          STOP("Opened %s file ok but could not memory map it. This is a %dbit process. %s.", filesize_to_str(fileSize), nbit,
                nbit<=32 ? "Please upgrade to 64bit" : "There is probably not enough contiguous virtual memory available");
        }
        sof = (const char*) mmp;
        if (verbose) DTPRINT("ok\n");  // to end 'Memory mapping ... '
    } else {
        STOP("Neither `input` nor `filename` are given, nothing to read.");
    }
    eof = sof + fileSize;
    double tMap = wallclock();


    //*********************************************************************************************
    // [3] Check whether the file contains BOM (Byte Order Mark), and if yes
    //     strip it, modifying `sof`. If the last byte in file is 0x1A (Ctrl+Z)
    //     then skip it too, modifying `eof`.
    //
    //     Also, presence of BOM allows us to sometimes detect file's encoding.
    //     See: https://en.wikipedia.org/wiki/Byte_order_mark
    //     See: issues #1087, #1465 and #1612
    //*********************************************************************************************
    if (verbose) DTPRINT("[3] Detect and skip BOM\n");
    if (fileSize >= 3 && memcmp(sof, "\xEF\xBB\xBF", 3) == 0) {
      sof += 3;
      // ienc = CE_UTF8;
      if (verbose) DTPRINT("  UTF-8 byte order mark EF BB BF found at the start of the file and skipped.\n");
    }
    else if (fileSize >= 4 && memcmp(sof, "\x84\x31\x95\x33", 4) == 0) {
      sof += 4;
      // ienc = CE_GB18030;
      if (verbose) DTPRINT("  GB-18030 byte order mark 84 31 95 33 found at the start of the file and skipped.\n");
      DTWARN("GB-18030 encoding detected, however fread() is unable to decode it. Some character fields may be garbled.\n");
    }
    else if (fileSize >= 2 && sof[0] + sof[1] == '\xFE' + '\xFF') {  // either 0xFE 0xFF or 0xFF 0xFE
      STOP("File is encoded in UTF-16, this encoding is not supported by fread(). Please recode the file to UTF-8.");
    }
    if (eof[-1] == '\x1A') {
      eof--;
      if (verbose) DTPRINT("  Last byte of input found to be 0x1A (Ctrl+Z) and removed.\n");
    }
    if (eof<=sof) STOP("Input is empty after removing BOM and any terminal control characters");

    //*********************************************************************************************
    // [4] Auto detect end-of-line character(s)
    //
    //     This section initializes variables `eol`, `eol2` and `eolLen`. If
    //     `eolLen` is 1, then we set both `eol` and `eol2` to the same
    //     character, otherwise `eolLen` is 2 and we set `eol` and `eol2` to the
    //     first and the second line-separator characters respectively.
    //*********************************************************************************************
    if (verbose) DTPRINT("[4] Detect end-of-line character(s)\n");

    // Scan the file from the end backwards until the first newline character is found. This avoids the more common of the
    // rare case of different newlines embedded in quoted column names. It also avoids the cost of passing over a potentially
    // very large number of column names just to find the line ending. BUT we will avoid line-ending detection altogether next
    // and this (including eolLen) will go away.

    ch = eof-1;
    while (ch>=sof && (*ch==' ' || *ch=='\t')) ch--;
    if (ch>=sof && *ch!='\n' && *ch!='\r') {
      if (args.filename) {
        if (verbose) DTPRINT("  File ends abruptly with '%c'. If problems occur, please append a newline to properly end the last record; e.g. 'echo >> file.csv'.\n", *ch);
        eof--;
        finalByte = *eof;
        *_const_cast(eof) = '\0';  // cow page
        // finalByte could be ' ' or '\t' here because the last field may contain trailing whitespace and stripWhite
        // could be false. That would only be the case though if other non-whitespace occurred after the final newline.
        ch = eof-1;
      }
      // else char* input already ended in \0 ok, checked above
      // Now keep searching backwards to find the last newline
      while (ch>=sof && *ch!='\n' && *ch!='\r') ch--;
    }
    if (ch<sof) {
      if (verbose) DTPRINT("  Input contains no \\r or \\n. Treating as a single row.\n");
      eol = eol2 = '\n';
      eolLen = 1;
    }
    else {
        // `ch` is pointing to the very last \r or \n in the input
        eol=eol2=*ch; eolLen=1;
        if (eol2=='\n') {
            if (ch-1>=sof && *(ch-1)=='\r') {
                if (verbose) DTPRINT("  Detected eol as \\r\\n (CRLF) in that order, the Windows standard.\n");
                eol='\r'; eolLen=2;
                if (ch-2>=sof && *(ch-2)=='\r') {
                    STOP("Line ending is \\r\\r\\n. R's download.file() appears to add the extra \\r in text mode on Windows. Please download again in binary mode (mode='wb') which might be faster too. Alternatively, pass the URL directly to fread and it will download the file in binary mode for you.");
                    // NB: on Windows, download.file from file seems to condense \r\r too.
                }
            } else {
                if (verbose) DTPRINT("  Detected eol as \\n only, the UNIX and Mac standard.\n");
            }
        }
        else {
            // eol2=='\r'
            if (ch-1>=sof && *(ch-1)=='\n') {
                DTWARN("Detected eol as \\n\\r, a highly unusual line ending. According to Wikipedia the Acorn BBC used this. If it is intended that the first column on the next row is a character column where the first character of the field value is \\r (why?) then the first column should start with a quote (i.e. 'protected'). Proceeding with attempt to read the file.\n");
                eol='\n'; eolLen=2;
            } else {
                if (verbose) DTPRINT("Detected line ending as \\r. An old Mac 9 standard discontinued in 2002 according to Wikipedia.\n");
            }
        }
        if (args.filename && finalByte=='\0') {
            // Use the final newline byte(s) to write '\0' and move eof back to there.
            // No need to remember finalByte here as we checked above that there is only whitespace afterwards.
            eof = ch - (eolLen-1);
            *_const_cast(eof) = '\0';  // cow last page
        }
    }
    // We have now ensured the input ends on eof and that *eof=='\0' too.
    // If the file ended abruptly (rare), the last character was remembered in finalByte.
    // We have made most files which end properly with final end-of-line now abruptly end with a \0 instead (in the cow page).
    // This may seem counterintuitive but now we have consistency within the constraints of no mmap of fileSize+1.
    // In all but Field processing, we don't need to test for eof at all, since \0 is terminal just as well as \r or \n.
    // When we need to, we now have two options: i) if (*ch && ...) (i.e. !='\0') which saves a compare, or ii) ch<eof as usual
    // If UTF strings contain \0 we can branch in that rare case to test if ch==eof too if necessary. We have that option.
    // If a field does not end with sep or eol, it's only in that rare case do we then need to test if it is \0 or not.

    //*********************************************************************************************
    // [6] Position to line `skipNrow+1` or to line containing `skipString`.
    //
    //     This section moves the `sof` pointer passing over the lines that the
    //     user requested to skip.
    //
    //     We also compute the `line` variable, which tracks the current line
    //     number within the file (similar to __LINE__ macro). Note that `line`
    //     variable is different from the logical row number: it doesn't attempt
    //     to skip newlines within quoted fields. Thus this line is similar to
    //     what text editors report, or bash commands like "wc -l", "head -n"
    //     or "tail -n".
    //*********************************************************************************************
    if (verbose) DTPRINT("[6] Skipping initial rows if needed\n");

    int line = 1;
    // line is for error and warning messages so considers raw \n whether inside quoted fields or not, just
    // like wc -l, head -n and tail -n
    const char *pos = sof;
    ch = pos;
    if (args.skipString) {
        ch = strstr(sof, args.skipString);  // as there is now a \0 at the end, this is safely bounded
        if (!ch) STOP("skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)",
                      args.skipString);
        while (ch>sof && *(ch-1)!=eol2) ch--;  // move to beginning of line
        pos = ch;
        ch = sof;
        while (ch<pos) line+=(*ch++==eol);
        if (verbose) DTPRINT("Found skip='%s' on line %d. Taking this to be header row or first row of data.\n",
                                  args.skipString, line);
        ch = pos;
    } else
    // Skip the first `skipNrow` lines of input.
    if (args.skipNrow>0) {
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
    if (ch>=eof && !finalByte) STOP("Input is either empty, fully whitespace, or skip has been set after the last non-whitespace.");
    if (verbose) {
      if (lineStart>ch) DTPRINT("  Moved forward to first non-blank line (%d)\n", line);
      DTPRINT("  Positioned on line %d starting: <<%s>>\n", line, strlim(lineStart, 30));
    }
    ch = pos = lineStart;


    //*********************************************************************************************
    // [7] Auto detect separator, quoting rule, first line and ncol, simply,
    //     using jump 0 only.
    //
    //     Always sample as if nrows= wasn't supplied. That's probably *why*
    //     user is setting nrow=0 to get the column names and types, without
    //     actually reading the data yet. Most likely to check consistency
    //     across a set of files.
    //*********************************************************************************************
    if (verbose) DTPRINT("[7] Detect separator, quoting rule, and ncolumns\n");

    int nseps;
    char seps[]=",|;\t ";  // default seps in order of preference. See ?fread.
    // using seps[] not *seps for writeability (http://stackoverflow.com/a/164258/403310)

    if (args.sep == '\0') {  // default is '\0' meaning 'auto'
      if (verbose) DTPRINT("  Detecting sep ...\n");
      nseps = (int) strlen(seps);
    } else {
      seps[0] = args.sep;
      seps[1] = '\0';
      nseps = 1;
      if (verbose) DTPRINT("  Using supplied sep '%s'\n", args.sep=='\t' ? "\\t" : seps);
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
    // + 1 lines: 'numFields` gives the count of fields in each group, and
    // `numLines` has the number of lines in each group.
    int numFields[JUMPLINES+1];
    int numLines[JUMPLINES+1];
    for (int s=0; s<nseps; s++) {
      sep = seps[s];
      whiteChar = (sep==' ' ? '\t' : (sep=='\t' ? ' ' : 0));  // 0 means both ' ' and '\t' to be skipped
      for (quoteRule=0; quoteRule<4; quoteRule++) {  // quote rule in order of preference
        ch = pos;
        // if (verbose) DTPRINT("  Trying sep='%c' with quoteRule %d ...\n", sep, quoteRule);
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

        i = -1;
        while (numLines[++i]) {
          if (numFields[i] > nmax) nmax=numFields[i];  // for fill=true to know max number of columns
          //if (args.verbose) DTPRINT("numLines[i]=%d, topNumLines=%d, numFields[i]=%d, topNumFields=%d\n",
          //                           numLines[i], topNumLines, numFields[i], topNumFields);
          if (numFields[i]>1 &&
              ( numLines[i]>topNumLines ||   // most number of consistent ncol wins
               (numLines[i]==topNumLines && numFields[i]>topNumFields && sep!=' '))) {  // ties resolved by numFields
            topNumLines = numLines[i];
            topNumFields = numFields[i];
            topSep = sep;
            topQuoteRule = quoteRule;
            topNmax = nmax;
            firstJumpEnd = ch;  // So that after the header we know how many bytes jump point 0 is
            updated = true;
            // Two updates can happen for the same sep and quoteRule (e.g. issue_1113_fread.txt where sep=' ') so the
            // updated flag is just to print once.
          }
        }
        if (verbose && updated) {
          DTPRINT(sep<' '? "  sep=%#02x" : "  sep='%c'", sep);
          DTPRINT("  with %d lines of %d fields using quote rule %d\n", topNumLines, topNumFields, topQuoteRule);
        }
      }
    }
    if (!firstJumpEnd) STOP("Internal error: no sep won");

    int ncol;
    quoteRule = topQuoteRule;
    sep = topSep;
    whiteChar = (sep==' ' ? '\t' : (sep=='\t' ? ' ' : 0));
    ch = pos;
    if (fill) {
      // start input from first populated line, already pos.
      ncol = topNmax;
    } else {
      // find the top line with the consistent number of fields.  There might be irregular header lines above it.
      ncol = topNumFields;
      int thisLine=-1;
      while (ch<eof && ++thisLine<JUMPLINES) {
        const char *ch2 = ch;   // lineStart
        if (countfields(&ch)==ncol) { ch=pos=ch2; line+=thisLine; break; }
      }
    }
    // For standard regular separated files, we're now on the first byte of the file.

    if (ncol<1 || line<1) STOP("Internal error: ncol==%d line==%d after detecting sep, ncol and first line", ncol, line);
    int tt = countfields(&ch);
    ch = pos; // move back to start of line since countfields() moved to next
    if (!fill && tt!=ncol) STOP("Internal error: first line has field count %d but expecting %d", tt, ncol);
    if (verbose) {
      DTPRINT("  Detected %d columns on line %d. This line is either column "
              "names or first data row. Line starts as: <<%s>>\n",
              tt, line, strlim(pos, 30));
      DTPRINT("  Quote rule picked = %d\n", quoteRule);
      if (fill) DTPRINT("  fill=true and the most number of columns found is %d\n", ncol);
    }


    //*********************************************************************************************
    // [8] Detect and assign column names (if present)
    //
    //     This section also moves the `sof` pointer to point at the first row
    //     of data ("removing" the column names).
    //*********************************************************************************************
    if (verbose) DTPRINT("[8] Determine column names\n");

    // throw-away storage for processors to write to in this preamble.
    // Saves deep 'if (target)' inside processors.
    double trash_val; // double so that this storage is aligned. char trash[8] would not be aligned.
    void *trash = (void*)&trash_val;

    const char *colNamesAnchor = ch;
    colNames = calloc((size_t)ncol, sizeof(lenOff));
    if (!colNames) STOP("Unable to allocate %d*%d bytes for column name pointers: %s", ncol, sizeof(lenOff), strerror(errno));
    _Bool allchar=true;
    if (sep==' ') while (ch<eof && *ch==' ') ch++;
    ch--;  // so we can ++ at the beginning inside loop.
    for (int field=0; field<tt; field++) {
      const char *this = ++ch;
      // DTPRINT("Field %d <<%s>>\n", field, strlim(ch, 20));
      skip_white(&ch);
      if (allchar && !on_sep(&ch) && !StrtoD(&ch, (double *)trash)) allchar=false;  // don't stop early as we want to check all columns to eol here
      if (allchar && ch==eof && finalByte>='0' && finalByte<='9') { allchar=false; continue; }    // test 893
      // considered looking for one isalpha present but we want 1E9 to be considered a value not a column name
      ch = this;  // rewind to the start of this field
      Field(&ch, (lenOff *)trash);  // StrtoD does not consume quoted fields according to the quote rule, so redo with Field()
      // countfields() above already validated the line so no need to check again now.
    }
    if (ch<eof && *ch!=eol)
      STOP("Read %d expected fields in the header row (fill=%d) but finished on <<%s>>'", tt, fill, strlim(ch,30));
    // already checked above that tt==ncol unless fill=TRUE
    // when fill=TRUE and column names shorter (test 1635.2), leave calloc initialized lenOff.len==0
    if (verbose && args.header!=NA_BOOL8) DTPRINT("  'header' changed by user from 'auto' to %s\n", args.header?"true":"false");
    if (args.header==false || (args.header==NA_BOOL8 && !allchar)) {
        if (verbose && args.header==NA_BOOL8) DTPRINT("  Some fields on line %d are not type character. Treating as a data row and using default column names.\n", line);
        // colNames was calloc'd so nothing to do; all len=off=0 already
        ch = pos;  // back to start of first row. Treat as first data row, no column names present.
        // now check previous line which is being discarded and give helpful msg to user ...
        if (ch>sof && args.skipNrow==0 && args.skipString==NULL) {
          ch -= (eolLen+1);
          if (ch<sof) ch=sof;  // for when sof[0]=='\n'
          while (ch>sof && *ch!=eol2) ch--;
          if (ch>sof) ch++;
          const char *prevStart = ch;
          int tmp = countfields(&ch);
          if (tmp==ncol) STOP("Internal error: row before first data row has the same number of fields but we're not using it.");
          if (tmp>1) DTWARN("Starting data input on line %d <<%s>> with %d fields and discarding line %d <<%s>> before it because it has a different number of fields (%d).", line, strlim(pos, 30), ncol, line-1, strlim(prevStart, 30), tmp);
        }
        if (ch!=pos) STOP("Internal error. ch!=pos after prevBlank check");
    } else {
        if (verbose && args.header==NA_BOOL8) {
          DTPRINT("  All the fields on line %d are character fields. Treating as the column names.\n", line);
        }
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
        if (ch==eof && finalByte) {
            // single line containing column names only without ending newline
            *_const_cast(eof) = finalByte;  // put back the saved finalByte, no other field processing will happen and we are done
            colNames[ncol-1].len++;
        }
        else if (*ch!=eol && *ch!='\0') STOP("Internal error: reading colnames did not end on eol or '\\0' but '%c'", *ch);
        else if (ch<eof) ch+=eolLen;
        pos=ch;    // now on first data row (row after column names)
    }
    int row1Line = line;
    double tLayout = wallclock();


    //*********************************************************************************************
    // [9] Make best guess at column types using 100 rows at 100 points,
    //     including the very first, middle and very last row.
    //     At the same time, calc mean and sd of row lengths in sample for very
    //     good nrow estimate.
    //*********************************************************************************************
    if (verbose) DTPRINT("[9] Detect column types\n");
    type = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));
    size = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));
    if (!type || !size) STOP("Failed to allocate %d x 2 bytes for type/size: %s", ncol, strerror(errno));

    for (int j = 0; j < ncol; j++) {
      // initialize with the first (lowest) type, 1==CT_BOOL8 at the time of writing. If we add CT_BOOL1 or CT_BOOL2 in
      /// future, using 1 here means this line won't need to be changed. CT_DROP is 0 and 1 is the first type.
      type[j] = 1;
      size[j] = typeSize[type[j]];
    }

    size_t jump0size=(size_t)(firstJumpEnd-pos);  // the size in bytes of the first JUMPLINES from the start (jump point 0)
    // how many places in the file to jump to and test types there (the very end is added as 11th or 101th)
    // not too many though so as not to slow down wide files; e.g. 10,000 columns.  But for such large files (50GB) it is
    // worth spending a few extra seconds sampling 10,000 rows to decrease a chance of costly reread even further.
    int nJumps = 0;
    size_t sz = (size_t)(eof - pos);
    if (jump0size>0) {
      if (jump0size*100*2 < sz) nJumps=100;  // 100 jumps * 100 lines = 10,000 line sample
      else if (jump0size*10*2 < sz) nJumps=10;
      // *2 to get a good spacing. We don't want overlaps resulting in double counting.
      // nJumps==1 means the whole (small) file will be sampled with one thread
    }
    nJumps++; // the extra sample at the very end (up to eof) is sampled and format checked but not jumped to when reading
    if (verbose) {
      DTPRINT("  Number of sampling jump points = %d because ", nJumps);
      if (jump0size==0) DTPRINT("jump0size==0\n");
      else DTPRINT("(%zd bytes from row 1 to eof) / (2 * %zd jump0size) == %zd\n",
                   sz, jump0size, sz/(2*jump0size));
    }

    size_t sampleLines=0;
    double sumLen=0.0, sumLenSq=0.0;
    int minLen=INT32_MAX, maxLen=-1;   // int_max so the first if(thisLen<minLen) is always true; similarly for max
    const char *lastRowEnd=pos;
    for (int j=0; j<nJumps; j++) {
        ch = (j == 0) ? pos :
             (j == nJumps-1) ? eof - (size_t)(0.5*jump0size) :
                               pos + (size_t)j*((size_t)(eof-pos)/(size_t)(nJumps-1));
        if (ch<lastRowEnd) ch=lastRowEnd;  // Overlap when apx 1,200 lines (just over 11*100) with short lines at the beginning and longer lines near the end, #2157
        if (ch>=eof) break;                // The 9th jump could reach the end in the same situation and that's ok. As long as the end is sampled is what we want.
        if (j>0 && !nextGoodLine(&ch, ncol))
          STOP("Could not find first good line start after jump point %d when sampling.", j);
        _Bool bumped = 0;  // did this jump find any different types; to reduce verbose output to relevant lines
        int jline = 0;  // line from this jump point
        while(ch<eof && (jline<JUMPLINES || j==nJumps-1)) {  // nJumps==1 implies sample all of input to eof; last jump to eof too
            const char *jlineStart = ch;
            if (sep==' ') while (ch<eof && *ch==' ') ch++;  // multiple sep=' ' at the jlineStart does not mean sep(!)
            // detect blank lines
            skip_white(&ch);
            if (ch>=eof || *ch==eol) {
              if (!skipEmptyLines && !fill) break;
              jlineStart = ch;  // to avoid 'Line finished early' below and get to the sampleLines++ block at the end of this while
            }
            jline++;
            int field=0;
            const char *fieldStart = ch;  // Needed outside loop for error messages below
            while (ch<eof && *ch!=eol && field<ncol) {
                // DTPRINT("<<%s>>(%d)", strlim(ch,20), quoteRule);
                fieldStart=ch;
                while (type[field]<=CT_STRING && fun[type[field]](&ch, trash)) {
                  if (finalByte && ch==eof) break;  // if very final field contains, for example, NA then finalByte=='A' and 'N' on its own should not cause bump to character
                  ch=fieldStart;
                  if (type[field]<CT_STRING) { type[field]++; bumped=true; }
                  else {
                    // the field could not be read with this quote rule, try again with next one
                    // Trying the next rule will only be successful if the number of fields is consistent with it
                    if (quoteRule >= 3) STOP("Even quoteRule 3 was insufficient!");
                    if (verbose)
                      DTPRINT("Bumping quote rule from %d to %d due to field %d on line %d of sampling jump %d starting <<%s>>\n",
                               quoteRule, quoteRule+1, field+1, jline, j, strlim(fieldStart,200));
                    quoteRule++;
                    bumped=true;
                    ch = jlineStart;  // Try whole line again, in case it's a hangover from previous field
                    field=0;
                    continue;
                  }
                }
                // DTPRINT("%d  (ch = %p)\n", type[field], ch);
                if (ch<eof && *ch!=eol) {ch++; field++;}
            }
            if (field<ncol-1 && !fill) {
                if (ch<eof && *ch!=eol) {
                    STOP("Internal error: line has finished early but not on an eol or eof (fill=false). Please report as bug.");
                } else if (ch>jlineStart) {
                    STOP("Line %d has too few fields when detecting types. Use fill=TRUE to pad with NA. Expecting %d fields but found %d: <<%s>>", jline, ncol, field+1, strlim(jlineStart,200));
                }
            }
            if (ch<eof) {
                if (*ch!=eol || field>=ncol) {   // the || >=ncol is for when a comma ends the line with eol straight after
                  if (field!=ncol) STOP("Internal error: Line has too many fields but field(%d)!=ncol(%d)", field, ncol);
                  STOP("Line %d from sampling jump %d starting <<%s>> has more than the expected %d fields. "
                       "Separator '%c' occurs at position %d which is character %d of the last field: <<%s>>. "
                       "Consider setting 'comment.char=' if there is a trailing comment to be ignored.",
                       jline, j, strlim(jlineStart,10), ncol, *ch, (int)(ch-jlineStart), (int)(ch-fieldStart),
                       strlim(fieldStart,200));
                }
                ch += eolLen;
            } else {
                // if very last field was quoted, check if it was completed with an ending quote ok.
                // not necessarily a problem (especially if we detected no quoting), but we test it and nice to have
                // a warning regardless of quoting rule just incase file has been inadvertently truncated
                // This warning is early at type skipping around stage before reading starts, so user can cancel early
                if (type[ncol-1]==CT_STRING && *fieldStart==quote && (*(ch-1)!=quote && finalByte!=quote)) {
                  if (quoteRule<2) STOP("Internal error: Last field of last field should select quote rule 2");
                  DTWARN("Last field of last line starts with a quote but is not finished with a quote before end of file: <<%s>>",
                         strlim(fieldStart, 200));
                }
            }
            // Two reasons:  1) to get the end of the very last good row before whitespace or footer before eof
            //               2) to check sample jumps don't overlap, otherwise double count and bad estimate
            lastRowEnd = ch;
            //DTPRINT("\n");
            int thisLineLen = (int)(ch-jlineStart);  // ch is now on start of next line so this includes eolLen already
            sampleLines++;
            sumLen += thisLineLen;
            sumLenSq += thisLineLen*thisLineLen;
            if (thisLineLen<minLen) minLen=thisLineLen;
            if (thisLineLen>maxLen) maxLen=thisLineLen;
        }
        if (verbose && (bumped || j==0 || j==nJumps-1)) {
          DTPRINT("  Type codes (jump %03d)    : ",j); printTypes(ncol);
          DTPRINT("  Quote rule %d\n", quoteRule);
        }
    }
    while (ch<eof && isspace(*ch)) ch++;
    if (ch<eof) {
      DTWARN("Found the last consistent line but text exists afterwards (discarded): <<%s>>", strlim(ch,200));
    }

    size_t estnrow=1, allocnrow=1;
    double meanLineLen=0;
    size_t bytesRead = 0;
    if (sampleLines<=1) {
      // column names only are present; e.g. fread("A\n")
    } else {
      bytesRead = (size_t)(lastRowEnd - pos);
      meanLineLen = (double)sumLen/sampleLines;
      estnrow = CEIL(bytesRead/meanLineLen);  // only used for progress meter and verbose line below
      double sd = sqrt( (sumLenSq - (sumLen*sumLen)/sampleLines)/(sampleLines-1) );
      allocnrow = clamp_szt((size_t)(bytesRead / fmax(meanLineLen - 2*sd, minLen)),
                            (size_t)(1.1*estnrow), 2*estnrow);
      // sd can be very close to 0.0 sometimes, so apply a +10% minimum
      // blank lines have length 1 so for fill=true apply a +100% maximum. It'll be grown if needed.
      if (verbose) {
        DTPRINT("  =====\n");
        DTPRINT("  Sampled %zd rows (handled \\n inside quoted fields) at %d jump points\n", sampleLines, nJumps);
        DTPRINT("  Bytes from first data row on line %d to the end of last row: %zd\n", row1Line, bytesRead);
        DTPRINT("  Line length: mean=%.2f sd=%.2f min=%d max=%d\n", meanLineLen, sd, minLen, maxLen);
        DTPRINT("  Estimated number of rows: %zd / %.2f = %zd\n", bytesRead, meanLineLen, estnrow);
        DTPRINT("  Initial alloc = %zd rows (%zd + %d%%) using bytes/max(mean-2*sd,min) clamped between [1.1*estn, 2.0*estn]\n",
                 allocnrow, estnrow, (int)(100.0*allocnrow/estnrow-100.0));
      }
      if (nJumps==1) {
        if (verbose) DTPRINT("  All rows were sampled since file is small so we know nrow=%zd exactly\n", sampleLines);
        estnrow = allocnrow = sampleLines;
      } else {
        if (sampleLines > allocnrow) STOP("Internal error: sampleLines(%zd) > allocnrow(%zd)", sampleLines, allocnrow);
      }
      if (nrowLimit < allocnrow) {
        if (verbose) DTPRINT("  Alloc limited to lower nrows=%zd passed in.\n", nrowLimit);
        estnrow = allocnrow = nrowLimit;
      }
      if (verbose) DTPRINT("  =====\n");
    }


    //*********************************************************************************************
    // [10] Apply colClasses, select, drop and integer64
    //*********************************************************************************************
    if (verbose) DTPRINT("[10] Apply user overrides on column types\n");
    ch = pos;
    oldType = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));
    if (!oldType) STOP("Unable to allocate %d bytes to check user overrides of column types", ncol);
    memcpy(oldType, type, ncol) ;
    if (!userOverride(type, colNames, colNamesAnchor, ncol)) { // colNames must not be changed but type[] can be
      if (verbose) DTPRINT("  Cancelled by user: userOverride() returned false.");
      freadCleanup();
      return 1;
    }
    int ndrop=0, nUserBumped=0;
    size_t rowSize1 = 0;
    size_t rowSize4 = 0;
    size_t rowSize8 = 0;
    int nStringCols = 0;
    int nNonStringCols = 0;
    for (int j=0; j<ncol; j++) {
      size[j] = typeSize[type[j]];
      rowSize1 += (size[j] & 1);  // only works if all sizes are powers of 2
      rowSize4 += (size[j] & 4);
      rowSize8 += (size[j] & 8);
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


    //*********************************************************************************************
    // [11] Allocate the result columns
    //*********************************************************************************************
    if (verbose) DTPRINT("[11] Allocate memory for the datatable\n");
    if (verbose) {
      DTPRINT("  Allocating %d column slots (%d - %d dropped) with %zd rows\n",
              ncol-ndrop, ncol, ndrop, allocnrow);
    }
    size_t DTbytes = allocateDT(type, size, ncol, ndrop, allocnrow);
    double tAlloc = wallclock();

    // Read ahead and drop behind each point as they move through (assuming it's on a per thread basis).
    // Considered it but when processing string columns the buffers point to offsets in the mmp'd pages
    // which are revisited when writing the finished buffer to DT. So, it isn't sequential.
    // if (fnam!=NULL) {
    //   #ifdef MADV_SEQUENTIAL  // not on Windows. PrefetchVirtualMemory from Windows 8+ ?
    //   int ret = madvise((void *)mmp, (size_t)fileSize, MADV_SEQUENTIAL);
    //   #endif
    // }


    //*********************************************************************************************
    // [12] Read the data
    //*********************************************************************************************
    if (verbose) DTPRINT("[12] Read the data\n");
    ch = pos;   // back to start of first data row
    int hasPrinted=0;  // the percentage last printed so it prints every 2% without many calls to wallclock()
    _Bool stopTeam=false, firstTime=true;  // _Bool for MT-safey (cannot ever read half written _Bool value)
    int nTypeBump=0, nTypeBumpCols=0;
    double tRead=0, tReread=0, tTot=0;  // overall timings outside the parallel region
    double thNextGoodLine=0, thRead=0, thPush=0;  // reductions of timings within the parallel region
    char *typeBumpMsg=NULL;  size_t typeBumpMsgSize=0;
    #define stopErrSize 1000
    char stopErr[stopErrSize+1]="";  // must be compile time size: the message is generated and we can't free before STOP
    size_t DTi = 0;   // the current row number in DT that we are writing to
    const char *prevJumpEnd = pos;  // the position after the last line the last thread processed (for checking)
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
      nJumps = (int)(bytesRead/chunkBytes);  // (int) rounds down
      if (nJumps==0) nJumps=1;
      else if (nJumps>nth) nJumps = nth*(1+(nJumps-1)/nth);
      chunkBytes = bytesRead / (size_t)nJumps;
    } else {
      nJumps = 1;
    }
    size_t initialBuffRows = allocnrow / (size_t)nJumps;

    // Catch initialBuffRows==0 when max_nrows is small, seg fault #2243
    // Rather than 10, maybe 1 would work too but then 1.5 grow factor * 1 would still be 1. This clamp
    // should only engage when max_nrows is supplied, and supplied small too, so doesn't matter too much.
    if (initialBuffRows < 10) initialBuffRows = 10;

    if (initialBuffRows > INT32_MAX) STOP("Buffer size %lld is too large\n", initialBuffRows);
    nth = imin(nJumps, nth);

    read:  // we'll return here to reread any columns with out-of-sample type exceptions
    #pragma omp parallel num_threads(nth)
    {
      int me = omp_get_thread_num();
      #pragma omp master
      nth = omp_get_num_threads();
      const char *thisJumpStart=NULL;  // The first good start-of-line after the jump point
      size_t myDTi = 0;  // which row in the final DT result I should start writing my chunk to
      size_t myNrow = 0; // the number of rows in my chunk

      // Allocate thread-private row-major myBuff
      // Do not reuse trash for myBuff0 as that might create write conflicts
      // between threads, causing slowdown of the process.
      size_t myBuffRows = initialBuffRows;  // Upon realloc, myBuffRows will increase to grown capacity
      void *myBuff8 = malloc(rowSize8 * myBuffRows);
      void *myBuff4 = malloc(rowSize4 * myBuffRows);
      void *myBuff1 = malloc(rowSize1 * myBuffRows);
      void *myBuff0 = malloc(8);  // for CT_DROP columns
      if ((rowSize8 && !myBuff8) ||
          (rowSize4 && !myBuff4) ||
          (rowSize1 && !myBuff1) || !myBuff0) stopTeam = true;

      ThreadLocalFreadParsingContext ctx = {
        .anchor = NULL,
        .buff8 = myBuff8,
        .buff4 = myBuff4,
        .buff1 = myBuff1,
        .rowSize8 = rowSize8,
        .rowSize4 = rowSize4,
        .rowSize1 = rowSize1,
        .DTi = 0,
        .nRows = allocnrow,
        .threadn = me,
        .quoteRule = quoteRule,
        .stopTeam = &stopTeam,
        #ifndef DTPY
        .nStringCols = nStringCols,
        .nNonStringCols = nNonStringCols
        #endif
      };
      prepareThreadContext(&ctx);

      #pragma omp for ordered schedule(dynamic) reduction(+:thNextGoodLine,thRead,thPush)
      for (int jump = 0; jump < nJumps; jump++) {
        if (stopTeam) continue;  // nothing left to do
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
          pushBuffer(&ctx);
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

        void *myBuff1Pos = myBuff1, *myBuff4Pos = myBuff4, *myBuff8Pos = myBuff8;
        void **allBuffPos[9];
        allBuffPos[0] = &myBuff0;
        allBuffPos[1] = &myBuff1Pos;
        allBuffPos[4] = &myBuff4Pos;
        allBuffPos[8] = &myBuff8Pos;

        while (tch<nextJump) {
          if (myNrow == myBuffRows) {
            // buffer full due to unusually short lines in this chunk vs the sample; e.g. #2070
            myBuffRows *= 1.5;
            #pragma omp atomic
            buffGrown++;
            long diff8 = (char*)myBuff8Pos - (char*)myBuff8;
            long diff4 = (char*)myBuff4Pos - (char*)myBuff4;
            long diff1 = (char*)myBuff1Pos - (char*)myBuff1;
            ctx.buff8 = myBuff8 = realloc(myBuff8, rowSize8 * myBuffRows);
            ctx.buff4 = myBuff4 = realloc(myBuff4, rowSize4 * myBuffRows);
            ctx.buff1 = myBuff1 = realloc(myBuff1, rowSize1 * myBuffRows);
            if ((rowSize8 && !myBuff8) ||
                (rowSize4 && !myBuff4) ||
                (rowSize1 && !myBuff1)) {
              stopTeam = true;
              break;
            }
            // restore myBuffXPos in case myBuffX was moved by realloc
            myBuff8Pos = (void*)((char*)myBuff8 + diff8);
            myBuff4Pos = (void*)((char*)myBuff4 + diff4);
            myBuff1Pos = (void*)((char*)myBuff1 + diff1);
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
                  (llint)(myDTi + myNrow));
                  // TODO - include a few (numbered) lines before and after in the message.
              }
              break;
            }
          }

          int j = 0;
          const char *fieldStart = tch;

          int finalFieldLen = 0;  // only used when finalByte
          char finalSep = '\0';   // only used when finalByte
          oneLastTimeIfFinalByte:

          while (j < ncol) {
            // DTPRINT("Field %d: '%.10s' as type %d  (tch=%p)\n", j+1, tch, type[j], tch);
            fieldStart = tch;
            int8_t joldType = type[j];   // fetch shared type once. Cannot read half-written byte.
            int8_t thisType = joldType;  // to know if it was bumped in (rare) out-of-sample type exceptions
            int8_t absType = (int8_t) abs(thisType);

            // always write to buffPos even when CT_DROP. It'll just overwrite on next non-CT_DROP
            while (absType < NUMTYPE) {
              // normally returns success=1, and myBuffPos is assigned inside *fun.
              void *target = thisType > 0? *(allBuffPos[size[j]]) : myBuff0;
              int ret = fun[absType](&tch, target);
              if (ret == 0) break;
              // guess is insufficient out-of-sample, type is changed to negative sign and then bumped. Continue to
              // check that the new type is sufficient for the rest of the column to be sure a single re-read will work.
              absType++;
              thisType = -absType;
              tch = fieldStart;
            }

            if (joldType == CT_STRING) {
              ((lenOff*) myBuff8Pos)->off += (size_t)(fieldStart - thisJumpStart);
            } else if (thisType != joldType             // rare out-of-sample type exception.
                       && (!finalByte || finalSep)) {   // don't bump the final field until we've replaced the finalByte (if any) test 894.0221 where final field is NA and finalByte=='A'
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
                    (int)(tch-fieldStart), fieldStart, (llint)(myDTi+myNrow));
                  typeBumpMsg = realloc(typeBumpMsg, typeBumpMsgSize + (size_t)len + 1);
                  strcpy(typeBumpMsg+typeBumpMsgSize, temp);
                  typeBumpMsgSize += (size_t)len;
                  nTypeBump++;
                  if (joldType>0) nTypeBumpCols++;
                  type[j] = thisType;
                } // else other thread bumped to a (negative) higher or equal type, so do nothing
              }
            }
            *((char**) allBuffPos[size[j]]) += size[j];
            j++;
            if (tch>=eof || *tch==eol) break;
            tch++;
          }

          // One extra branch at the end of each input line; it should be very well predicted and skipped vast majority of time.
          // All because cow mmap fileSize+1 doesn't work on Mac and Windows when fileSize%pageSize==0
          // The goal here is to handle the rare case of files that don't end with newline and are also exactly a multiple of page size, PR#2200.
          if (finalByte && tch==eof) {
            if (j<=0 || *eof!='\0' || !args.filename) {
              #pragma omp critical
              if (!stopTeam) {
                stopTeam = true;
                snprintf(stopErr, stopErrSize, "Internal error in final field: j<=0 || *eof!='\\0' || !args.filename");
                // Direct non-file input is already ended with '\0'. finalByte is only ever set for files and only when they end abruptly without final newline
              }
              break;
            }
            if (!finalSep) {
              // Just read final field for the first time and finalSep hasn't been set yet.
              // finalByte is true so we need to reread the final field
              // fieldStart will be correct even in very complicated cases where for example jump points are
              //   skipped because we delay and isolate this final field logic to this latest point when we really need it
              if (fieldStart>(char *)mmp) {
                // Vastly more common and expected branch: there is at least 1 byte before the very last field in the file (a separator or a newline)
                // Shift final field back 1 character in cow pages(s).
                // We do not allocate a copy for the last field because that would need *eof to be moved to the copy's end, but eof is shared across threads.
                finalFieldLen = (int)(tch-fieldStart);
                finalSep = fieldStart[-1];  // remember the byte we're shifting back over; either sep or newline
                memmove(_const_cast(fieldStart-1), fieldStart, finalFieldLen);  // cow page.
                *_const_cast(eof-1) = finalByte;
                // *eof=='\0' already, checked above
                tch = fieldStart-1;
              } else {
                // Very very rare, other than in edge case tests.
                // File is one single field (no column names, no separator) which does not end with a newline
                if (fileSize%4096==0 || nth>1) {  // TODO: portable way to discover relevant page size.
                  #pragma omp critical
                  if (!stopTeam) {
                    stopTeam = true;
                    snprintf(stopErr, stopErrSize,
                      nth==1 ? "File is very very very unusual. It appears to be one single field (no column names, no separators) which also has a length a multiple of 4096 and also does not end with a newline. Please add a new line at the end using for example 'echo >> file.txt'." : "Internal error: A single field file (!) somehow has nth>1");
                  }
                  break;
                }
                // otherwise there is one byte after eof which we can reliably write to in the very last cow page
                // We could do this branch for filesSize%4096 != 0 too, but we desire to run all tests through the alternative branch above (like test 893 which
                //   causes a type bump in the last field due to the finalByte)
                fileSize++;
                *_const_cast(eof++) = finalByte;  // here we eof++ which is ok as nth==1 checked above
                *_const_cast(eof) = '\0';
                tch = fieldStart;
                finalByte = '\0'; // so this branch doesn't run again. We dealt with it.
              }
              // reread final field
              j--;
              *((char**) allBuffPos[size[j]]) -= size[j];
              goto oneLastTimeIfFinalByte;
            } else if (nTypeBump) {
              // The very final field was just reread due to finalByte!='\0' and finalSep!='\0' so that was done by shifting it back.
              // But there are out-of-sample type exceptions so there will be a reread and we have to shift the final field forwards again and put
              //   back the finalSep before the final field.
              // If no reread then careful to leave field shifted back for when it is type character since lenOff points there.
              memmove(_const_cast(fieldStart+1), fieldStart, finalFieldLen); // shift final field forwards
              *_const_cast(fieldStart) = finalSep;  // put the finalField's preceeding sep or newline back again
              finalSep = '\0';  // important to reset in case whole file is reread due to out-of-sample type exceptions
              finalFieldLen = 0;
            }
          }

          if (j < ncol)  {
            // not enough columns observed
            if (!fill) {
              #pragma omp critical
              if (!stopTeam) {
                stopTeam = true;
                snprintf(stopErr, stopErrSize,
                  "Expecting %d cols but row %lld contains only %d cols (sep='%c'). " \
                  "Consider fill=true. <<%s>>",
                  ncol, (llint)myDTi, j, sep, strlim(tlineStart, 500));
              }
              break;
            }
            while (j<ncol) {
              switch (type[j]) {
              case CT_BOOL8:
                *(int8_t*)myBuff1Pos = NA_BOOL8;
                break;
              case CT_INT32_BARE:
              case CT_INT32_FULL:
                *(int32_t*)myBuff4Pos = NA_INT32;
                break;
              case CT_INT64:
                *(int64_t*)myBuff8Pos = NA_INT64;
                break;
              case CT_FLOAT64:
                *(double*)myBuff8Pos = NA_FLOAT64;
                break;
              case CT_STRING:
                ((lenOff*)myBuff8Pos)->len = blank_is_a_NAstring ? INT8_MIN : 0;
                ((lenOff*)myBuff8Pos)->off = 0;
                break;
              default:
                break;
              }
              *((char**) allBuffPos[size[j]]) += size[j];
              j++;
            }
          }
          if (tch<eof && *tch!=eol) {
            #pragma omp critical
            if (!stopTeam) {
              stopTeam = true;
              snprintf(stopErr, stopErrSize,
                "Too many fields on out-of-sample row %lld. Read all %d expected columns but more are present. <<%s>>",
                (llint)myDTi, ncol, strlim(tlineStart, 500));
            }
            break;
          }
          tch+=eolLen;
          myNrow++;
        }
        if (verbose) { tt1 = wallclock(); thRead += tt1 - tt0; tt0 = tt1; }
        ctx.anchor = thisJumpStart;
        ctx.nRows = myNrow;
        postprocessBuffer(&ctx);

        #pragma omp ordered
        {
          // stopTeam could be true if a previous thread already stopped while I was waiting my turn
          if (!stopTeam && prevJumpEnd != thisJumpStart) {
            snprintf(stopErr, stopErrSize,
              "Jump %d did not finish counting rows exactly where jump %d found its first good line start: "
              "prevEnd(%p)<<%s>> != thisStart(prevEnd%+d)<<%s>>",
              jump-1, jump, (const void*)prevJumpEnd, strlim(prevJumpEnd,50),
              (int)(thisJumpStart-prevJumpEnd), strlim(thisJumpStart,50));
            stopTeam=true;
          }
          myDTi = DTi;  // fetch shared DTi (where to write my results to the answer). The previous thread just told me.
          ctx.DTi = myDTi;
          if (myDTi >= nrowLimit) {
            // nrowLimit was supplied and a previous thread reached that limit while I was counting my rows
            stopTeam=true;
            myNrow = 0;
          } else {
            myNrow = umin(myNrow, nrowLimit - myDTi); // for the last jump that reaches nrowLimit
          }
          // tell next thread 2 things :
          prevJumpEnd = tch; // i) the \n I finished on so it can check (above) it started exactly on that \n good line start
          DTi += myNrow;     // ii) which row in the final result it should start writing to. As soon as I know myNrow.
          ctx.nRows = myNrow;
          orderBuffer(&ctx);
        }
        // END ORDERED.
        // Next thread can now start its ordered section and write its results to the final DT at the same time as me.
        // Ordered has to be last in some OpenMP implementations currently. Logically though, pushBuffer happens now.
      }
      // End for loop over all jump points

      // Push out all buffers one last time.
      if (myNrow) {
        double tt1 = verbose? wallclock() : 0;
        pushBuffer(&ctx);
        if (verbose) thRead += wallclock() - tt1;
      }
      // Each thread to free its own buffer.
      free(myBuff8); myBuff8 = NULL;
      free(myBuff4); myBuff4 = NULL;
      free(myBuff1); myBuff1 = NULL;
      free(myBuff0); myBuff0 = NULL;
      freeThreadContext(&ctx);
    }
    //-- end parallel ------------------


    //*********************************************************************************************
    // [13] Finalize the datatable
    //*********************************************************************************************
    if (hasPrinted && verbose) DTPRINT("\n");
    if (verbose) DTPRINT("[13] Finalizing the datatable\n");
    if (firstTime) {
      tReread = tRead = wallclock();
      tTot = tRead-t0;
      if (hasPrinted || verbose) {
        DTPRINT("\rRead %zd rows x %d columns from %s file in ", DTi, ncol-ndrop, filesize_to_str(fileSize));
        DTPRINT("%02d:%06.3f wall clock time\n", (int)tTot/60, fmod(tTot,60.0));
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
        DTPRINT("\rReread %zd rows x %d columns in ", DTi, nTypeBumpCols);
        DTPRINT("%02d:%06.3f\n", (int)(tReread-tRead)/60, fmod(tReread-tRead,60.0));
      }
    }
    if (stopTeam && stopErr[0]!='\0') STOP(stopErr); // else nrowLimit applied and stopped early normally
    if (DTi > allocnrow) {
      if (nrowLimit > allocnrow) STOP("Internal error: DTi(%zd) > allocnrow(%zd) but nrows=%zd (not limited)",
                                      DTi, allocnrow, nrowLimit);
      // for the last jump that fills nrow limit, then ansi is +=buffi which is >allocnrow and correct
    } else if (DTi == allocnrow) {
      if (verbose) DTPRINT("Read %zd rows. Exactly what was estimated and allocated up front\n", DTi);
    } else {
      allocnrow = DTi;
    }
    setFinalNrow(DTi);

    // However if some of the columns could not be read due to out-of-sample
    // type exceptions, we'll need to re-read the input file.
    if (firstTime && nTypeBump) {
      rowSize1 = rowSize4 = rowSize8 = 0;
      nStringCols = 0;
      nNonStringCols = 0;
      for (int j=0, resj=-1; j<ncol; j++) {
        if (type[j] == CT_DROP) continue;
        resj++;
        if (type[j]<0) {
          // column was bumped due to out-of-sample type exception
          type[j] = -type[j];
          size[j] = typeSize[type[j]];
          rowSize1 += (size[j] & 1);
          rowSize4 += (size[j] & 4);
          rowSize8 += (size[j] & 8);
          if (type[j] == CT_STRING) nStringCols++; else nNonStringCols++;
        } else if (type[j]>=1) {
          // we'll skip over non-bumped columns in the rerun, whilst still incrementing resi (hence not CT_DROP)
          // not -type[i] either because that would reprocess the contents of not-bumped columns wastefully
          type[j] = -CT_STRING;
          size[j] = 0;
        }
      }
      allocateDT(type, size, ncol, ncol - nStringCols - nNonStringCols, DTi);
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
      DTPRINT("%8.3fs (%3.0f%%) Column type detection using %zd sample rows\n",
        tColType-tLayout, 100.0*(tColType-tLayout)/tTot, sampleLines);
      DTPRINT("%8.3fs (%3.0f%%) Allocation of %zd rows x %d cols (%.3fGB)\n",
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
