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
  #include <stdio.h>     // snprintf
  #include <math.h>      // ceil, sqrt, isfinite
#endif
#include <stdbool.h>
#include "fread.h"
#include "freadLookups.h"

// On Windows variables of type `size_t` cannot be printed with "%zu" in the
// `snprintf()` function. For those variables we will cast them into
// `unsigned long long int` before printing; and this #define makes it
// slightly simpler.
#define llu   unsigned long long int


// Private globals to save passing all of them through to highly iterated field processors
static const char *eof;
static char sep;
static char whiteChar; // what to consider as whitespace to skip: ' ', '\t' or 0 means both (when sep!=' ' && sep!='\t')
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
static bool any_number_like_NAstrings=false;
static bool blank_is_a_NAstring=false;
static bool stripWhite=true;  // only applies to character columns; numeric fields always stripped
static bool skipEmptyLines=false, fill=false;

static double NA_FLOAT64;  // takes fread.h:NA_FLOAT64_VALUE

#define JUMPLINES 100    // at each of the 100 jumps how many lines to guess column types (10,000 sample lines)

// Private globals so they can be cleaned up both on error and on successful return
static void *mmp = NULL;
static size_t fileSize;
static int8_t *type = NULL, *size = NULL;
static lenOff *colNames = NULL;
static int8_t *oldType = NULL;
static freadMainArgs args;  // global for use by DTPRINT

const char typeName[NUMTYPE][10] = {"drop", "bool8", "bool8", "bool8", "bool8", "int32", "int64", "float64", "float64", "float64", "string"};
int8_t     typeSize[NUMTYPE]     = { 0,      1,       1,       1,       1,       4,       8,       8,         8,         8,         8      };

// NAN and INFINITY constants are float, so cast to double once up front.
static const double NAND = (double)NAN;
static const double INFD = (double)INFINITY;

typedef struct FieldParseContext {
  // Pointer to the current parsing location
  const char **ch;
  // Parse target buffers, indexed by size. A parser that reads values of byte
  // size `sz` will attempt to write that value into `targets[sz]`. Thus,
  // generally this is an array with elements 0, 1, 4, and 8 defined, while all
  // other pointers are NULL.
  void **targets;
  // String "anchor" for `Field()` parser -- the difference `ch - anchor` will
  // be written out as the string offset.
  const char *anchor;
} FieldParseContext;

// Forward declarations
static void Field(FieldParseContext *ctx);

#define ASSERT(cond, msg, ...) \
  if (!(cond)) STOP("Internal error in line %d of fread.c, please report on data.table GitHub:  " msg, __LINE__, __VA_ARGS__)



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
 * bring all global variables to a "clean slate". This function should always be
 * executed when fread() exits, either successfully or not. But if it doesn't
 * for some reason (e.g. unexpected error/bug) then it is called again on starting
 * with verbose message if it needed to clean anything up.
 */
bool freadCleanup(void)
{
  bool neededCleanup = (type || size || colNames || oldType || mmp);
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
      mmp = NULL;
      if (!ret) DTPRINT("System error %d unmapping view of file\n", GetLastError());
    #else
      int ret = munmap(mmp, fileSize);
      mmp = NULL;
      if (ret) DTPRINT("System errno %d unmapping file\n", errno);
    #endif
  }
  fileSize = 0;
  sep = whiteChar = quote = dec = finalByte = '\0';
  quoteRule = -1;
  any_number_like_NAstrings = false;
  blank_is_a_NAstring = false;
  stripWhite = true;
  skipEmptyLines = false;
  fill = false;
  // following are borrowed references: do not free
  eof = NULL;
  NAstrings = NULL;
  return neededCleanup;
}


#define CEIL(x)  ((size_t)(double)ceil(x))
static inline size_t umax(size_t a, size_t b) { return a > b ? a : b; }
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
  char *ptr = buf + 501 * flip;
  flip = 1 - flip;
  char *ch2 = ptr;
  if (limit>500) limit=500;
  size_t width = 0;
  while ((*ch>'\r' || (*ch!='\0' && *ch!='\r' && *ch!='\n')) && width++<limit) *ch2++ = *ch++;
  *ch2 = '\0';
  return ptr;
}


static void printTypes(int ncol) {
  // e.g. files with 10,000 columns, don't print all of it to verbose output.
  int tt=(ncol<=110?ncol:90); for (int i=0; i<tt; i++) DTPRINT("%d",type[i]);
  if (ncol>110) { DTPRINT("..."); for (int i=ncol-10; i<ncol; i++) DTPRINT("%d",type[i]); }
}


static inline void skip_white(const char **ptr) {
  // skip space so long as sep isn't space and skip tab so long as sep isn't tab
  const char *ch = *ptr;
  if (whiteChar == 0) {   // whiteChar==0 means skip both ' ' and '\t';  sep is neither ' ' nor '\t'.
    while (*ch == ' ' || *ch == '\t') ch++;
  } else {
    while (*ch == whiteChar) ch++;  // sep is ' ' or '\t' so just skip the other one.
  }
  *ptr = ch;
}


/**
 * Return True iff `ch` is a valid field terminator character: either a field
 * separator or a newline.
 */
static inline bool end_of_field(const char ch) {
  // \r is 13, \n is 10, and \0 is 0. The second part is optimized based on the
  // fact that the characters in the ASCII range 0..13 are very rare, so a
  // single check `ch<=13` is almost equivalent to checking whether `ch` is one
  // of \r, \n, \0. We cast to unsigned first because `char` type is signed by
  // default, and therefore characters in the range 0x80-0xFF are actually
  // treated as negatives.
  return ch==sep || ((uint8_t)ch<=13 && (ch=='\n' || ch=='\r' || ch=='\0'));
}


/**
 * eol() accepts a position and, if any of the following line endings, moves to the end of that sequence
 * and returns true. Repeated \\r are considered one. At most one \\n will be moved over.
 * 1. \\n        Unix
 * 2. \\r\\n     Windows
 * 3. \\r\\r\\n  R's download.file() in text mode doubling up \\r
 * 4. \\r        Old MacOS 9 format discontinued in 2002 but then #2347 was raised straight away when I tried not to support it
 * 5. \\n\\r     Acorn BBC (!) and RISC OS according to Wikipedia.
 * 6. \\r\\r\\r  Might as well, for completeness
 */
static inline bool eol(const char **ptr) {
  const char *ch = *ptr;
  while (*ch=='\r') ch++;  // commonly happens once on Windows for type 2
  if (*ch=='\n') {
    // 1,2,3 and 5 (one \n with any number of \r before and/or after)
    while (ch[1]=='\r') ch++;  // type 5. Could drop but we're only tepid here so keep for completeness and full generality.
    *ptr = ch;
    return true;
  }
  else if (ch>*ptr) {  // did we move over some \r above?
    // 4 and 6 (\r only with no \n before or after)
    *ptr = ch-1;  // move back onto the last \r
    return true;
  }
  return false;
}


static inline const char *end_NA_string(const char *fieldStart) {
  const char* const* nastr = NAstrings;
  const char *mostConsumed = fieldStart; // tests 1550* includes both 'na' and 'nan' in nastrings. Don't stop after 'na' if 'nan' can be consumed too.
  while (*nastr) {
    const char *ch1 = fieldStart;
    const char *ch2 = *nastr;
    while (*ch1==*ch2 && *ch2!='\0') { ch1++; ch2++; }
    if (*ch2=='\0' && ch1>mostConsumed) mostConsumed=ch1;
    nastr++;
  }
  return mostConsumed;
}

/**
 * Compute the number of fields on the current line (taking into account the
 * global `sep` and `quoteRule`), and move the parsing location to the
 * beginning of the next line.
 * Returns the number of fields on the current line, or -1 if the line cannot
 * be parsed using current settings.
 * This does not need to be particularly efficient; it's just used for format detection.
 */
static inline int countfields(const char **ptr)
{
  static lenOff trash;  // see comment on other trash declarations
  static void *targets[9];
  targets[8] = (void*) &trash;
  const char *ch = *ptr;
  if (sep==' ') while (*ch==' ') ch++;  // multiple sep==' ' at the start does not mean sep
  skip_white(&ch);
  if (eol(&ch)) {
    *ptr = ch+1;
    return 0;
  }
  int ncol = 1;
  FieldParseContext ctx = {
    .ch = &ch,
    .targets = targets,
    .anchor = NULL,
  };
  while (ch<eof) {
    Field(&ctx);
    // Field() leaves *ch resting on sep, \r, \n or *eof=='\0'
    if (sep==' ' && *ch==sep) {
      while (ch[1]==' ') ch++;
      if (ch[1]=='\r' || ch[1]=='\n' || ch[1]=='\0') {
        // reached end of line. Ignore padding spaces at the end of line.
        ch++;  // Move onto end of line character
      }
    }
    if (*ch==sep) {
      ch++;
      ncol++;
      continue;
    }
    if (eol(&ch)) { *ptr=ch+1; return ncol; }
    if (*ch!='\0') return -1;  // -1 means this line not valid for this sep and quote rule
    break;
  }
  if (ch==eof && finalByte && finalByte==sep && sep!=' ') ncol++;
  *ptr = ch;
  return ncol;
}


static inline bool nextGoodLine(const char **ptr, int ncol)  //  TODO: remove using Pasha's chunk-roll-on idea
{
  const char *ch = *ptr;
  // we may have landed inside quoted field containing embedded sep and/or embedded \n
  // find next \n and see if 5 good lines follow. If not try next \n, and so on, until we find the real \n
  // We don't know which line number this is, either, because we jumped straight to it. So return true/false for
  // the line number and error message to be worked out up there.
  int attempts=0;
  while (attempts++<30) {
    while (*ch!='\0' && *ch!='\n' && *ch!='\r') ch++;
    if (*ch=='\0') return false;
    eol(&ch);  // move to last byte of the line ending sequence
    ch++;      // move to first byte of next line
    int i=0;
    const char *ch2 = ch;
    while (i<5 && countfields(&ch2)==ncol) i++;
    if (i==5) break;
  }
  if (*ch!='\0' && attempts<30) { *ptr = ch; return true; }
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
static const char* filesize_to_str(size_t fsize)
{
  #define NSUFFIXES 4
  #define BUFFSIZE 100
  static char suffixes[NSUFFIXES] = {'T', 'G', 'M', 'K'};
  static char output[BUFFSIZE];
  static const char one_byte[] = "1 byte";
  llu lsize = (llu) fsize;
  for (int i = 0; i <= NSUFFIXES; i++) {
    int shift = (NSUFFIXES - i) * 10;
    if ((fsize >> shift) == 0) continue;
    int ndigits = 3;
    for (; ndigits >= 1; ndigits--) {
      if ((fsize >> (shift + 12 - ndigits * 3)) == 0) break;
    }
    if (ndigits == 0 || (fsize == (fsize >> shift << shift))) {
      if (i < NSUFFIXES) {
        snprintf(output, BUFFSIZE, "%llu%cB (%llu bytes)",
                 lsize >> shift, suffixes[i], lsize);
        return output;
      }
    } else {
      snprintf(output, BUFFSIZE, "%.*f%cB (%llu bytes)",
               ndigits, (double)fsize / (1 << shift), suffixes[i], lsize);
      return output;
    }
  }
  if (fsize == 1) return one_byte;
  snprintf(output, BUFFSIZE, "%llu bytes", lsize);
  return output;
}



//==============================================================================
// Field parsers
//
// This section contains functions each designed to parse values of a particular
// type from the input string. All these functions have the same signature:
//
//     void parser(FieldParseContext *ctx);
//
// Here the FieldParseContext structure contains the following fields:
//   .ch -- pointer to the current parsing location
//   .targets -- array of memory buffers where parsers store their results
//   + any other parser-specific fields (for example Field() uses .anchor)
//
// Each parser is expected to scan the input starting from position `ch`, and
// then perform one and only one of the following:
//
//   1. If the value can be successfully parsed, then it is stored into the
//      `ctx.targets[sizeof(value)]` buffer, and the parsing location `ctx.ch`
//      is advanced to point at the first unread byte in the input.
//   2. If the value cannot be parsed, then the parser must store an NA value
//      into the `ctx.targets[sizeof(value)]` buffer. The parsing location
//      `ctx.ch` should be left unmodified.
//
// Note that it is not the parser's job ot advance the `ctx.targets` pointer --
// this is left to the caller. The reason for this is because we have different
// parsing scenarios:
//   - in the "normal" case the value will be written and the target pointer
//     advanced;
//   - in the "trash" case the value will be written but no pointer will be
//     modified, and we allow the value to be overwritten by a subsequent call;
//   - in the "typebump" case the value will be written, but some _other_ target
//     pointer will be advanced. Thus, the value that was just written will be
//     eventually overwritten, while some other buffer will have a gap filled
//     with random data (we will reread that column anyways, so it's ok).
//
// In all cases the caller is responsible to ensure that the `targets[sz]`
// buffer is properly allocated and has enough space to write a single value of
// size `sz`.
//
//==============================================================================

static void Field(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  lenOff *target = (lenOff*) ctx->targets[sizeof(lenOff)];

  // need to skip_white first for the reason that a quoted field might have space before the
  // quote; e.g. test 1609. We need to skip the space(s) to then switch on quote or not.
  if (*ch==' ' && stripWhite) while(*++ch==' ');  // if sep==' ' the space would have been skipped already and we wouldn't be on space now.
  const char *fieldStart=ch;
  if (*ch!=quote || quoteRule==3) {
    // Most common case. Unambiguously not quoted. Simply search for sep|eol. If field contains sep|eol then it should have been quoted and we do not try to heal that.
    while(!end_of_field(*ch)) ch++;  // sep, \r, \n or \0 will end
    *(ctx->ch) = ch;
    int fieldLen = (int)(ch-fieldStart);
    if (stripWhite) {   // TODO:  do this if and the next one together once in bulk afterwards before push
      while(fieldLen>0 && ch[-1]==' ') { fieldLen--; ch--; }
      // this space can't be sep otherwise it would have stopped the field earlier inside end_of_field()
    }
    if ((fieldLen==0 && blank_is_a_NAstring) || (fieldLen && end_NA_string(fieldStart)==ch)) fieldLen=INT32_MIN;  // TODO - speed up by avoiding end_NA_string when there are none
    target->off = (int32_t)(fieldStart - ctx->anchor);
    target->len = fieldLen;
    return;
  }
  // else *ch==quote (we don't mind that quoted fields are a little slower e.g. no desire to save switch)
  //    the field is quoted and quotes are correctly escaped (quoteRule 0 and 1)
  // or the field is quoted but quotes are not escaped (quoteRule 2)
  // or the field is not quoted but the data contains a quote at the start (quoteRule 2 too)
  int eolCount = 0;
  fieldStart++;  // step over opening quote
  switch(quoteRule) {
  case 0:  // quoted with embedded quotes doubled; the final unescaped " must be followed by sep|eol
    while (*++ch) {
      if (*ch=='\n' && ++eolCount==100) return;  // TODO: expose this 100 to user to allow them to control limiting runaway fields
      if (*ch==quote) {
        if (ch[1]==quote) { ch++; continue; }
        break;  // found undoubled closing quote
      }
    }
    break;
  case 1:  // quoted with embedded quotes escaped; the final unescaped " must be followed by sep|eol
    while (*++ch) {
      if (*ch=='\n' && ++eolCount==100) return;
      if (*ch=='\\' && (ch[1]==quote || ch[1]=='\\')) { ch++; continue; }
      if (*ch==quote) break;
    }
    break;
  case 2:
    // (i) quoted (perhaps because the source system knows sep is present) but any quotes were not escaped at all,
    // so look for ", to define the end.   (There might not be any quotes present to worry about, anyway).
    // (ii) not-quoted but there is a quote at the beginning so it should have been; look for , at the end
    // If no eol are present inside quoted fields (i.e. rows are simple rows), then this should work ok e.g. test 1453
    // since we look for ", and the source system quoted when , is present, looking for ", should work well.
    // Under this rule, no eol may occur inside fields.
    {
      const char *ch2 = ch;
      while (*++ch && *ch!='\n' && *ch!='\r') {
        if (*ch==quote && end_of_field(ch[1])) {ch2=ch; break;}  // (*1) regular ", ending; leave *ch on closing quote
        if (*ch==sep) {
          // first sep in this field
          // if there is a ", afterwards but before the next \n, use that; the field was quoted and it's still case (i) above.
          // Otherwise break here at this first sep as it's case (ii) above (the data contains a quote at the start and no sep)
          ch2 = ch;
          while (*++ch2 && *ch2!='\n' && *ch2!='\r') {
            if (*ch2==quote && end_of_field(ch2[1])) {
              ch = ch2;                                          // (*2) move on to that first ", -- that's this field's ending
              break;
            }
          }
          break;
        }
      }
      if (ch!=ch2) fieldStart--;   // field ending is this sep|eol; neither (*1) or (*2) happened; opening quote wasn't really an opening quote
    }
    break;
  default:
    return;  // Internal error: undefined quote rule
  }
  target->len = (int32_t)(ch - fieldStart);
  target->off = (int32_t)(fieldStart - ctx->anchor);
  if (*ch==quote) {
    ch++;
    skip_white(&ch);
    *(ctx->ch) = ch;
  } else {
    *(ctx->ch) = ch;
    if (*ch=='\0' && quoteRule!=2) { target->off--; target->len++; }  // test 1324 where final field has open quote but not ending quote; include the open quote like quote rule 2
    if (stripWhite) while(target->len>0 && ch[-1]==' ') { target->len--; ch--; }  // test 1551.6; trailing whitespace in field [67,V37] == "\"\"A\"\" ST       "
  }
}


static void StrtoI32(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  int32_t *target = (int32_t*) ctx->targets[sizeof(int32_t)];

  bool neg = *ch=='-';
  ch += (neg || *ch=='+');
  const char *start = ch;  // to know if at least one digit is present
  // acc needs to be 64bit so that 5bn (still 10 digits but greater than 4bn) does not overflow. It could be
  //   signed but we use unsigned to be clear it will never be negative
  uint_fast64_t acc = 0;
  uint_fast8_t digit, sf=0; // significant figures = digits from the first non-zero onwards including trailing zeros
  // sep, \r, \n and *eof=='\0' all serve as valid terminators here by dint of being !=[0-9]
  // see init.c for checks of unsigned uint_fast8_t cast
  // optimizer should implement 10* as ((x<<2 + x)<<1) or (x<<3 + x<<1)

  /*if (loseLeadingZeroOption)*/ while (*ch=='0') ch++;
  while ( (digit=(uint_fast8_t)(*ch-'0'))<10 ) {
    acc = 10*acc + digit;
    ch++;
    sf++;
  }
  // INT32 range is NA==-2147483648(INT32_MIN) then symmetric [-2147483647,+2147483647] so we can just test INT32_MAX
  // The max (2147483647) happens to be 10 digits long, hence <=10.
  // Leading 0 (such as 001 and 099 but not 0, +0 or -0) will cause type bump to _full which has the
  // option to treat as integer or string with further cost.
  // if ( (acc && *start!='0' && acc<=INT32_MAX && (ch-start)<=10) ||
  //     (acc==0 && ch-start==1) ) {
  if ((sf || ch>start) && sf<=10 && acc<=INT32_MAX) {
    *target = neg ? -(int32_t)acc : (int32_t)acc;
    *(ctx->ch) = ch;
  } else {
    *target = NA_INT32;  // empty field ideally, contains NA and fall through to check if NA (in which case this write is important), or just plain invalid
  }
}


static void StrtoI64(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  int64_t *target = (int64_t*) ctx->targets[sizeof(int64_t)];

  bool neg = *ch=='-';
  ch += (neg || *ch=='+');
  const char *start = ch;
  uint_fast64_t acc = 0;  // important unsigned not signed here; we now need the full unsigned range
  uint_fast8_t digit, sf=0;
  while (*ch=='0') ch++;
  while ( (digit=(uint_fast8_t)(*ch-'0'))<10 ) {
    acc = 10*acc + digit;
    ch++;
    sf++;
  }
  // INT64 range is NA==-9223372036854775808(INT64_MIN) then symmetric [-9223372036854775807,+9223372036854775807].
  // A 20+ digit number is caught as too large via the field width check <=19, since leading zeros trigger character type not numeric
  // TODO Check tests exist that +9223372036854775808 and +9999999999999999999 are caught as too large. They are stll 19 wide
  //   and, fortunately, uint64 can hold 9999999999999999999 (19 9's) so that doesn't overflow uint64.
  //if ( (acc && *start!='0' && acc<=INT64_MAX && (ch-start)<=19) ||
  //     (acc==0 && ch-start==1) ) {
  if ((sf || ch>start) && sf<=19 && acc<=INT64_MAX) {
    *target = neg ? -(int64_t)acc : (int64_t)acc;
    *(ctx->ch) = ch;
  } else {
    *target = NA_INT64;
  }
}


// generate freadLookups.h
// TODO: review ERANGE checks and tests; that range outside [1.7e-308,1.7e+308] coerces to [0.0,Inf]
/*
f = "~/data.table/src/freadLookups.h"
cat("const long double pow10lookup[701] = {\n", file=f, append=FALSE)
for (i in (-350):(349)) cat("1.0E",i,"L,\n", sep="", file=f, append=TRUE)
cat("1.0E350L\n};\n", file=f, append=TRUE)
*/


// TODO  Add faster StrtoD_quick for small precision positive numerics such as prices without a sign or
//       an E (e.g. $.cents) which don't need I64, long double, or Inf/NAN. Could have more than two levels.

/**
 * Parse "usual" double literals, in the form
 *
 *   [+|-] (NNN|NNN.|.MMM|NNN.MMM) [(E|e) [+|-] EEE]
 *
 * where `NNN`, `MMM`, `EEE` are one or more decimal digits, representing the
 * whole part, fractional part, and the exponent respectively.
 *
 * Right now we do not parse floating numbers that would incur significant loss
 * of precision, for example `1.2439827340958723094785103` will not be parsed
 * as a double.
 */
static void parse_double_regular(FieldParseContext *ctx)
{
  //
  const char *ch = *(ctx->ch);
  double *target = (double*) ctx->targets[sizeof(double)];

  bool neg, Eneg;
  ch += (neg = *ch=='-') + (*ch=='+');

  const char *start = ch;
  uint_fast64_t acc = 0;  // holds NNN.MMM as NNNMMM
  int_fast32_t e = 0;     // width of MMM to adjust NNNMMM by dec location
  uint_fast8_t digit, sf=0;
  while (*ch=='0') ch++;

  const char *ch0 = ch;
  while ( (digit=(uint_fast8_t)(*ch-'0'))<10 ) {
    acc = 10*acc + digit;
    ch++;
  }
  sf = (uint_fast8_t)(ch - ch0);
  if (*ch==dec) {
    ch++;
    // Numbers like 0.00000000000000000000000000000000004 can be read without
    // loss of precision as 4e-35  (test 1817)
    if (sf==0 && *ch=='0') {
      ch0 = ch;
      while (*ch=='0') ch++;
      e -= (ch - ch0);
    }
    ch0 = ch;
    while ( (digit=(uint_fast8_t)(*ch-'0'))<10 ) {
      acc = 10*acc + digit;
      ch++;
    }
    e -= (ch - ch0);
    sf += (ch - ch0);
  }
  if (sf>18) goto fail;  // Too much precision for double. TODO: reduce to 15(?) and discard trailing 0's.
  if (*ch=='E' || *ch=='e') {
    if (ch==start) goto fail;  // something valid must be between [+|-] and E, character E alone is invalid.
    ch += 1/*E*/ + (Eneg = ch[1]=='-') + (ch[1]=='+');
    int E=0, max_digits=3;
    while ( max_digits && (digit=(uint_fast8_t)(*ch-'0'))<10 ) {
      E = 10*E + digit;
      ch++;
      max_digits--;
    }
    e += Eneg? -E : E;
  }
  e += 350; // lookup table is arranged from -350 (0) to +350 (700)
  if (e<0 || e>700 || ch==start) goto fail;

  *target = (double)((long double)acc * pow10lookup[e]);
  if (neg) *target = -*target;
  *(ctx->ch) = ch;
  return;

  fail:
    *target = NA_FLOAT64;
}



/**
 * Parses double values, but also understands various forms of NAN literals
 * (each can possibly be preceded with a `+` or `-` sign):
 *
 *   nan, inf, NaN, NAN, NaN%, NaNQ, NaNS, qNaN, sNaN, NaN12345, sNaN54321,
 *   1.#SNAN, 1.#QNAN, 1.#IND, 1.#INF, INF, Inf, Infinity,
 *   #DIV/0!, #VALUE!, #NULL!, #NAME?, #NUM!, #REF!, #N/A
 *
 */
static void parse_double_extended(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  double *target = (double*) ctx->targets[sizeof(double)];
  bool neg, quoted;
  ch += (quoted = (*ch=='"'));
  ch += (neg = (*ch=='-')) + (*ch=='+');

  if (ch[0]=='n' && ch[1]=='a' && ch[2]=='n' && (ch += 3)) goto return_nan;
  if (ch[0]=='i' && ch[1]=='n' && ch[2]=='f' && (ch += 3)) goto return_inf;
  if (ch[0]=='I' && ch[1]=='N' && ch[2]=='F' && (ch += 3)) goto return_inf;
  if (ch[0]=='I' && ch[1]=='n' && ch[2]=='f' && (ch += 3)) {
    if (ch[0]=='i' && ch[1]=='n' && ch[2]=='i' && ch[3]=='t' && ch[4]=='y') ch += 5;
    goto return_inf;
  }
  if (ch[0]=='N' && (ch[1]=='A' || ch[1]=='a') && ch[2]=='N' && (ch += 3)) {
    if (ch[-2]=='a' && (*ch=='%' || *ch=='Q' || *ch=='S')) ch++;
    while ((uint_fast8_t)(*ch-'0') < 10) ch++;
    goto return_nan;
  }
  if ((ch[0]=='q' || ch[0]=='s') && ch[1]=='N' && ch[2]=='a' && ch[3]=='N' && (ch += 4)) {
    while ((uint_fast8_t)(*ch-'0') < 10) ch++;
    goto return_nan;
  }
  if (ch[0]=='1' && ch[1]=='.' && ch[2]=='#') {
    if ((ch[3]=='S' || ch[3]=='Q') && ch[4]=='N' && ch[5]=='A' && ch[6]=='N' && (ch += 7)) goto return_nan;
    if (ch[3]=='I' && ch[4]=='N' && ch[5]=='D' && (ch += 6)) goto return_nan;
    if (ch[3]=='I' && ch[4]=='N' && ch[5]=='F' && (ch += 6)) goto return_inf;
  }
  if (ch[0]=='#') {  // Excel-specific "numbers"
    if (ch[1]=='D' && ch[2]=='I' && ch[3]=='V' && ch[4]=='/' && ch[5]=='0' && ch[6]=='!' && (ch += 7)) goto return_nan;
    if (ch[1]=='V' && ch[2]=='A' && ch[3]=='L' && ch[4]=='U' && ch[5]=='E' && ch[6]=='!' && (ch += 7)) goto return_nan;
    if (ch[1]=='N' && ch[2]=='U' && ch[3]=='L' && ch[4]=='L' && ch[5]=='!' && (ch += 6)) goto return_na;
    if (ch[1]=='N' && ch[2]=='A' && ch[3]=='M' && ch[4]=='E' && ch[5]=='?' && (ch += 6)) goto return_na;
    if (ch[1]=='N' && ch[2]=='U' && ch[3]=='M' && ch[4]=='!' && (ch += 5)) goto return_na;
    if (ch[1]=='R' && ch[2]=='E' && ch[3]=='F' && ch[4]=='!' && (ch += 5)) goto return_na;
    if (ch[1]=='N' && ch[2]=='/' && ch[3]=='A' && (ch += 4)) goto return_na;
  }
  parse_double_regular(ctx);
  return;

  return_inf:
    *target = neg? -INFD : INFD;
    goto ok;
  return_nan:
    *target = NAND;
    goto ok;
  return_na:
    *target = NA_FLOAT64;
  ok:
    if (quoted && *ch!='"') {
      *target = NA_FLOAT64;
    } else {
      *(ctx->ch) = ch + quoted;
    }
}



/**
 * Parser for hexadecimal doubles as used in Java.
 *
 * The numbers are in the following format:
 *
 *   [+|-] (0x|0X) (0.|1.) HexDigits (p|P) [+|-] DecExponent
 *
 * Thus the number has optional sign; followed by hex prefix `0x` or `0X`;
 * followed by hex significand which may be in the form of either `0.HHHHH...`
 * or `1.HHHHH...` where `H` are hex-digits (there can be no more than 13
 * digits; first form is used for subnormal numbers, second for normal ones);
 * followed by exponent indicator `p` or `P`; followed by optional exponent
 * sign; and lastly followed by the exponent which is a decimal number.
 *
 * This can be directly converted into IEEE-754 double representation:
 *
 *   <1 bit: sign> <11 bits: exp+1022> <52 bits: significand>
 *
 * This parser also recognizes literals "NaN" and "Infinity" which can be
 * produced by Java.
 *
 * @see http://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.10.2
 * @see https://en.wikipedia.org/wiki/IEEE_754-1985
 */
static void parse_double_hexadecimal(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  double *target = (double*) ctx->targets[sizeof(double)];
  uint64_t neg;
  bool Eneg, subnormal = 0;
  ch += (neg = (*ch=='-')) + (*ch=='+');

  if (ch[0]=='0' && (ch[1]=='x' || ch[1]=='X') &&
      (ch[2]=='1' || (subnormal = ch[2]=='0')) && ch[3]=='.') {
    ch += 4;
    uint64_t acc = 0;
    uint8_t digit;
    const char *ch0 = ch;
    while ((digit = hexdigits[(uint8_t)(*ch)]) < 16) {
      acc = (acc << 4) + digit;
      ch++;
    }
    size_t ndigits = (uint_fast8_t)(ch - ch0);
    if (ndigits > 13 || !(*ch=='p' || *ch=='P')) goto fail;
    acc <<= (13 - ndigits) * 4;
    ch += 1 + (Eneg = ch[1]=='-') + (ch[1]=='+');
    uint64_t E = 0;
    while ((digit = (uint8_t)(*ch-'0')) < 10) {
      E = 10*E + digit;
      ch++;
    }
    E = 1023 + (Eneg? -E : E) - subnormal;
    if (subnormal ? E : (E<1 || E>2046)) goto fail;

    *((uint64_t*)target) = (neg << 63) | (E << 52) | (acc);
    *(ctx->ch) = ch;
    return;
  }
  if (ch[0]=='N' && ch[1]=='a' && ch[2]=='N') {
    *target = NA_FLOAT64;
    *(ctx->ch) = ch + 3;
    return;
  }
  if (ch[0]=='I' && ch[1]=='n' && ch[2]=='f' && ch[3]=='i' &&
      ch[4]=='n' && ch[5]=='i' && ch[6]=='t' && ch[7]=='y') {
    *target = neg ? -INFD : INFD;
    *(ctx->ch) = ch + 8;
    return;
  }

  fail:
    *target = NA_FLOAT64;
}


/* Parse numbers 0 | 1 as boolean. */
static void parse_bool_numeric(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  int8_t *target = (int8_t*) ctx->targets[sizeof(int8_t)];
  uint8_t d = (uint8_t)(*ch - '0');  // '0'=>0, '1'=>1, everything else > 1
  if (d <= 1) {
    *target = (int8_t) d;
    *(ctx->ch) = ch + 1;
  } else {
    *target = NA_BOOL8;
  }
}

/* Parse uppercase TRUE | FALSE as boolean. */
static void parse_bool_uppercase(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  int8_t *target = (int8_t*) ctx->targets[sizeof(int8_t)];
  if (ch[0]=='T' && ch[1]=='R' && ch[2]=='U' && ch[3]=='E') {
    *target = 1;
    *(ctx->ch) = ch + 4;
  } else if (ch[0]=='F' && ch[1]=='A' && ch[2]=='L' && ch[3]=='S' && ch[4]=='E') {
    *target = 0;
    *(ctx->ch) = ch + 5;
  } else {
    *target = NA_BOOL8;
  }
}

/* Parse camelcase True | False as boolean. */
static void parse_bool_titlecase(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  int8_t *target = (int8_t*) ctx->targets[sizeof(int8_t)];
  if (ch[0]=='T' && ch[1]=='r' && ch[2]=='u' && ch[3]=='e') {
    *target = 1;
    *(ctx->ch) = ch + 4;
  } else if (ch[0]=='F' && ch[1]=='a' && ch[2]=='l' && ch[3]=='s' && ch[4]=='e') {
    *target = 0;
    *(ctx->ch) = ch + 5;
  } else {
    *target = NA_BOOL8;
  }
}

/* Parse lowercase true | false as boolean. */
static void parse_bool_lowercase(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  int8_t *target = (int8_t*) ctx->targets[sizeof(int8_t)];
  if (ch[0]=='t' && ch[1]=='r' && ch[2]=='u' && ch[3]=='e') {
    *target = 1;
    *(ctx->ch) = ch + 4;
  } else if (ch[0]=='f' && ch[1]=='a' && ch[2]=='l' && ch[3]=='s' && ch[4]=='e') {
    *target = 0;
    *(ctx->ch) = ch + 5;
  } else {
    *target = NA_BOOL8;
  }
}



typedef void (*reader_fun_t)(FieldParseContext *ctx);
static reader_fun_t fun[NUMTYPE] = {
  (reader_fun_t) &Field,
  (reader_fun_t) &parse_bool_numeric,
  (reader_fun_t) &parse_bool_uppercase,
  (reader_fun_t) &parse_bool_titlecase,
  (reader_fun_t) &parse_bool_lowercase,
  (reader_fun_t) &StrtoI32,
  (reader_fun_t) &StrtoI64,
  (reader_fun_t) &parse_double_regular,
  (reader_fun_t) &parse_double_extended,
  (reader_fun_t) &parse_double_hexadecimal,
  (reader_fun_t) &Field
};

static int disabled_parsers[NUMTYPE] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};


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
  bool verbose = args.verbose;
  bool warningsAreErrors = args.warningsAreErrors;

  if (freadCleanup() && verbose)
    DTPRINT("Previous fread() session was not cleaned up properly. Cleaned up ok at the beginning of this fread() call.\n");

  if (verbose) DTPRINT("[01] Check arguments\n");
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
  disabled_parsers[CT_BOOL8_N] = !args.logical01;
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
    DTPRINT("  0/1 column will be read as %s\n", args.logical01? "boolean" : "integer");
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
  const char *ch = NULL;

  //*********************************************************************************************
  // [2] Open and memory-map the input file, setting up the parsing context
  //     (sof, eof, ch).
  //*********************************************************************************************
  double tMap;  // moment when memory-map step has finished
  {
  if (verbose) DTPRINT("[02] Opening the file\n");
  mmp = NULL;
  finalByte = '\0';   // only ever !='\0' for files and when the file's final data line is ended abrubtly without last line ending
  if (args.input) {
    if (verbose) DTPRINT("  `input` argument is provided rather than a file name, interpreting as raw text to read\n");
    sof = args.input;
    fileSize = strlen(sof);
    eof = sof+fileSize;
    if (*eof!='\0') STOP("Internal error: last byte of character input isn't \\0");
  }
  else if (args.filename) {
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
  tMap = wallclock();
  }


  //*********************************************************************************************
  // [3] Check whether the file contains BOM (Byte Order Mark), and if yes
  //     strip it, modifying `sof`. If the last byte in file is 0x1A (Ctrl+Z)
  //     then skip it too, modifying `eof`.
  //
  //     Also, presence of BOM allows us to sometimes detect file's encoding.
  //     See: https://en.wikipedia.org/wiki/Byte_order_mark
  //     See: issues #1087, #1465 and #1612
  //*********************************************************************************************
  {
  if (verbose) DTPRINT("[03] Detect and skip BOM\n");
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
  }


  //*********************************************************************************************
  // [4] Terminate mmap with \0
  //*********************************************************************************************
  {
  if (verbose) DTPRINT("[04] Arrange mmap to be \\0 terminated\n");
  if (args.filename) {
    ch = eof-1; // eof was resting after the last byte and should never be dereferenced there (bus error when fileSize%4096==0)
    while (ch>=sof && (*ch==' ' || *ch=='\t' || *ch=='\r')) ch--;
    if (ch>=sof && *ch=='\n') {
      // found last \n in the file and there is no non-whitespace after it
      // ideal case, we'll use it to place \0 and no need to remember finalByte
      while (ch>sof && ch[-1]=='\r') ch--;  // move to the first of any preceeding \r too
      eof = ch;
    }
    else {
      // file ends abruptly without a final \n after the last line. In rare cases there might be a lone \r after the last line and if so we do this method too.
      const char *ch2 = ch;
      while (ch2>=sof && *ch2!='\n' && *ch2!='\r') ch2--;  // look for any \n or \r before the last line. If none found, it's a single line (likely column names only)
      if (ch2<sof) {
        // single line input. Use the simpler approach with 4096 restriction for this rare case.
        if (fileSize%4096==0) {  // TODO: portable way to discover relevant page size? 4096 is lowest common denominator though and should suffice
          STOP("File is very very unusual. It is one single line and the file's size is an exact multiple of 4096 bytes. Please append a newline at the end using for example 'echo >> %s'.", args.filename);
        }
        // otherwise there is one byte after eof which we can reliably write to in the very last cow page
        // We could do this routinely (not just for single line input) when fileSize%4096!=0 but we desire to run all tests through the harder branch
        // below that uses finalByte approach to test that logic. Like test 893 which causes a type bump in the last field due to the finalByte.
        fileSize++;
      }
      else {
        if (verbose) DTPRINT("  File ends abruptly with '%c'. This should be fine but if a problem does occur, please report that problem as a bug and workaround it by appending a newline to properly end the last record; e.g. 'echo >> %s'.\n", *ch, args.filename);
        eof--;
        if (*eof != '\r') finalByte = *eof;
        // finalByte could be ' ' or '\t' here because the last field may contain trailing whitespace which must be kept when stripWhite=FALSE
        // That would only be the case though if other non-whitespace occurred on the last line, though. Otherwise the last \n at the start of
        // of the last line would have been found and used.
        // Note that the character printed in the message is not necessarily the one saved to finalByte. But it is the one that the user would
        // see as the last printing character on their display.
      }
    }
    *_const_cast(eof) = '\0';  // cow page
  }
  // else char* input already guaranteed to end with \0. We do not modify direct char* input at all, ever.

  // We have now ensured the input ends on eof and that *eof=='\0' too.
  // If the file ended abruptly (rare), the last character was remembered in finalByte.
  // We have made most files which end properly with final end-of-line now abruptly end with a \0 instead (in the cow page).
  // This may seem counterintuitive but now we have consistency within the constraints of no mmap of fileSize+1.
  // In processors we don't need to test for ch<eof at all now, since \0 is terminal just as well as \r or \n.
  // When we need to, we now have two options: i) if (*ch && ...) (i.e. !='\0') which saves a compare to eof on every byte, or ii) ch<eof as usual
  // If UTF strings contain \0 we can branch in that rare case to test if ch==eof too if necessary. We have that option.
  // If a field does not end with sep or eol, it's only in that rare case do we then need to test if it is \0 or not.
  }


  //*********************************************************************************************
  // [5] Position to line `skipNrow+1` or to line containing `skipString`.
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
  const char *pos;  // Location where the actual data in the file begins
  int line = 1;     // Current line number
  {
  if (verbose) DTPRINT("[05] Skipping initial rows if needed\n");

  // line is for error and warning messages so considers raw \n whether inside quoted fields or not, just
  // like wc -l, head -n and tail -n
  ch = pos = sof;
  if (args.skipString) {
    ch = strstr(sof, args.skipString);  // as there is now a \0 at the end, this is safely bounded
    if (!ch) STOP("skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)",
                  args.skipString);
    while (ch>sof && ch[-1]!='\n') ch--;  // move to beginning of line
    pos = ch;
    ch = sof;
    while (ch<pos) line+=(*ch++=='\n');
    if (verbose) DTPRINT("Found skip='%s' on line %d. Taking this to be header row or first row of data.\n",
                         args.skipString, line);
    ch = pos;
  }
  // Skip the first `skipNrow` lines of input.
  else if (args.skipNrow>0) {
    while (ch<eof && line<=args.skipNrow) line+=(*ch++=='\n');
    if (ch>=eof) STOP("skip=%d but the input only has %d line%s", args.skipNrow, line, line>1?"s":"");
    pos = ch;
  }

  // skip blank input at the start
  const char *lineStart = ch;
  while (ch<eof && isspace(*ch)) {   // isspace matches ' ', \t, \n and \r
    if (*ch=='\n') { ch++; lineStart=ch; line++; } else ch++;
  }
  if (ch>=eof && !finalByte) STOP("Input is either empty, fully whitespace, or skip has been set after the last non-whitespace.");
  if (verbose) {
    if (lineStart>ch) DTPRINT("  Moved forward to first non-blank line (%d)\n", line);
    DTPRINT("  Positioned on line %d starting: <<%s>>\n", line, strlim(lineStart, 30));
  }
  ch = pos = lineStart;
  }


  //*********************************************************************************************
  // [6] Auto detect separator, quoting rule, first line and ncol, simply, using jump 0 only.
  //
  //     Always sample as if nrows= wasn't supplied. That's probably _why_
  //     user is setting nrow=0 to get the column names and types, without
  //     actually reading the data yet. Most likely to check consistency
  //     across a set of files.
  //*********************************************************************************************
  const char *firstJumpEnd=NULL; // remember where the winning jumpline from jump 0 ends, to know its size excluding header
  int ncol;  // Detected number of columns in the file
  {
  if (verbose) DTPRINT("[06] Detect separator, quoting rule, and ncolumns\n");

  int nseps;
  char seps[]=",|;\t ";  // default seps in order of preference. See ?fread.
  // using seps[] not *seps for writeability (http://stackoverflow.com/a/164258/403310)

  if (args.sep == '\0') {  // default is '\0' meaning 'auto'
    if (verbose) DTPRINT("  Detecting sep ...\n");
    nseps = (int) strlen(seps);
  } else if (args.sep == '\n') {
    // unusual requirement: user asked for lines as single string column
    seps[0] = 127;  // ASCII DEL: a character different from \r, \n and \0 that isn't in the data
    seps[1] = '\0';
    nseps = 1;
    if (verbose) DTPRINT("  sep='\\n' passed in meaning read lines as single character column\n");
  } else {
    seps[0] = args.sep;
    seps[1] = '\0';
    nseps = 1;
    if (verbose) DTPRINT("  Using supplied sep '%s'\n", args.sep=='\t' ? "\\t" : seps);
  }

  int topNumLines=0;        // the most number of lines with the same number of fields, so far
  int topNumFields=1;       // how many fields that was, to resolve ties
  char topSep=127;          // which sep that was, by default 127 (ascii del) means no sep i.e. single-column input (1 field)
  int topQuoteRule=0;       // which quote rule that was
  int topNmax=1;            // for that sep and quote rule, what was the max number of columns (just for fill=true)
                            //   (when fill=true, the max is usually the header row and is the longest but there are more
                            //    lines of fewer)

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
      bool updated=false;
      int nmax=0;

      i = -1;
      while (numLines[++i]) {
        if (numFields[i] > nmax) nmax=numFields[i];  // for fill=true to know max number of columns
        //if (args.verbose) DTPRINT("numLines[i]=%d, topNumLines=%d, numFields[i]=%d, topNumFields=%d\n",
        //                           numLines[i], topNumLines, numFields[i], topNumFields);
        if (numFields[i]>1 &&
            ((numLines[i]>topNumLines) ||   // most number of consistent ncol wins
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
        DTPRINT((unsigned)sep<32 ? "  sep=%#02x" : "  sep='%c'", sep);
        DTPRINT("  with %d lines of %d fields using quote rule %d\n", topNumLines, topNumFields, topQuoteRule);
      }
    }
  }
  if (!firstJumpEnd) STOP("Internal error: no sep won");

  quoteRule = topQuoteRule;
  sep = topSep;
  whiteChar = (sep==' ' ? '\t' : (sep=='\t' ? ' ' : 0));
  ch = pos;
  const char *prevStart = NULL;  // the start of the non-empty line before the first not-ignored row
  if (fill) {
    // start input from first populated line, already pos.
    ncol = topNmax;
  } else {
    // find the top line with the consistent number of fields.  There might be irregular banner lines above it.
    ncol = topNumFields;
    int thisLine=-1;
    while (ch<eof && ++thisLine<JUMPLINES) {
      const char *lastLineStart = ch;
      int tt = countfields(&ch);
      if (tt==ncol) { ch=pos=lastLineStart; line+=thisLine; break; }
      else prevStart = (tt>0 ? lastLineStart : NULL);
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
    DTPRINT("  fill=%s and the most number of columns found is %d\n", fill?"true":"false", ncol);
  }

  // Now check previous line which is being discarded and give helpful message to user
  if (prevStart && args.skipNrow==0 && args.skipString==NULL) {
    ch = prevStart;
    tt = countfields(&ch);
    if (tt==ncol) STOP("Internal error: row before first data row has the same number of fields but we're not using it.");
    if (tt>1) DTWARN("Starting data input on line %d <<%s>> with %d fields and discarding line %d <<%s>> before it because it has a different number of fields (%d).",
                     line, strlim(pos, 30), ncol, line-1, strlim(prevStart, 30), tt);
  }
  if (ch!=pos) STOP("Internal error. ch!=pos after checking the line before the first data row");
  }


  //*********************************************************************************************
  // [7] Detect column types, good nrow estimate and whether first row is column names
  //*********************************************************************************************
  size_t allocnrow;       // Number of rows in the allocated DataTable
  double meanLineLen;     // Average length (in bytes) of a single line in the input file
  int nJumps;             // How many jumps to use when pre-scanning the file
  size_t sampleLines;     // How many lines were sampled during the initial pre-scan
  size_t bytesRead;       // Bytes in the whole data section
  const char *lastRowEnd; // Pointer to the end of the data section
  {
  if (verbose) DTPRINT("[07] Detect column types, good nrow estimate and whether first row is column names\n");
  if (verbose && args.header!=NA_BOOL8) DTPRINT("  'header' changed by user from 'auto' to %s\n", args.header?"true":"false");

  type = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));
  size = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));  // TODO: remove size[] when we implement Pasha's idea to += size inside processor
  if (!type || !size) STOP("Failed to allocate %d x 2 bytes for type/size: %s", ncol, strerror(errno));

  int8_t type0 = 1;
  while (disabled_parsers[type0]) type0++;
  for (int j=0; j<ncol; j++) {
    // initialize with the lowest available type
    type[j] = type0;
    size[j] = typeSize[type0];
  }

  size_t jump0size=(size_t)(firstJumpEnd-pos);  // the size in bytes of the first JUMPLINES from the start (jump point 0)
  // how many places in the file to jump to and test types there (the very end is added as 11th or 101th)
  // not too many though so as not to slow down wide files; e.g. 10,000 columns.  But for such large files (50GB) it is
  // worth spending a few extra seconds sampling 10,000 rows to decrease a chance of costly reread even further.
  nJumps = 0;
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
    else DTPRINT("(%llu bytes from row 1 to eof) / (2 * %llu jump0size) == %llu\n",
                 (llu)sz, (llu)jump0size, (llu)(sz/(2*jump0size)));
  }

  sampleLines = 0;
  int row1Line = line;
  double sumLen=0.0, sumLenSq=0.0;
  int minLen=INT32_MAX, maxLen=-1;   // int_max so the first if(thisLen<minLen) is always true; similarly for max
  lastRowEnd = pos;
  bool firstDataRowAfterPotentialColumnNames = false;  // for test 1585.7
  bool lastSampleJumpOk = false;   // it won't be ok if its nextGoodLine returns false as testing in test 1768
  for (int j=0; j<nJumps; j++) {
    ch = (j == 0) ? pos :
         (j == nJumps-1) ? eof - (size_t)(0.5*jump0size) :
                           pos + (size_t)j*((size_t)(eof-pos)/(size_t)(nJumps-1));
    if (ch<lastRowEnd) ch=lastRowEnd;  // Overlap when apx 1,200 lines (just over 11*100) with short lines at the beginning and longer lines near the end, #2157
    if (ch>=eof) break;                // The 9th jump could reach the end in the same situation and that's ok. As long as the end is sampled is what we want.
    if (j>0 && !nextGoodLine(&ch, ncol)) {
      // skip this jump for sampling. Very unusual and in such unusual cases, we don't mind a slightly worse guess.
      continue;
    }
    if (j==nJumps-1) lastSampleJumpOk = true;
    bool bumped = 0;  // did this jump find any different types; to reduce verbose output to relevant lines
    int jline = 0;     // line from this jump point
    while(ch<eof && (jline<JUMPLINES || j==nJumps-1)) {  // nJumps==1 implies sample all of input to eof; last jump to eof too
      const char *jlineStart = ch;
      if (sep==' ') while (*ch==' ') ch++;  // multiple sep=' ' at the jlineStart does not mean sep(!)
      // detect blank lines ...
      skip_white(&ch);
      if (eol(&ch) || *ch=='\0') {
        if (!skipEmptyLines && !fill) break;
        ch += (*ch!='\0');
        if (!skipEmptyLines) sampleLines++;  // TODO: fall through more gracefully
        continue;
      }
      jline++;
      int field=0;
      const char *fieldStart = NULL; // Needed outside loop for error messages below
      ch--;
      int8_t previousLastColType = (sampleLines <= 1 ? 1 : type[ncol-1]);  // to revert any bump in last colum due to final field on final row due to finalByte
      while (field<ncol) {
        // DTPRINT("<<%s>>(%d)", strlim(ch,20), quoteRule);
        ch++;
        skip_white(&ch);
        fieldStart=ch;
        bool thisColumnNameWasString = false;
        if (firstDataRowAfterPotentialColumnNames) {
          // 2nd non-blank row is being read now.
          // 1st row's type is remembered and compared to second row to decide if 1st row is column names or not
          thisColumnNameWasString = (type[field]==CT_STRING);
          if (field==0 && args.header==NA_BOOL8 && !thisColumnNameWasString) {
            args.header=false;
            if (verbose) DTPRINT("  'header' determined to be false due to first field not being type string\n");
          }
          type[field] = type0;  // re-initialize for 2nd row onwards
        }
        // throw-away storage for processors to write to in this preamble.
        double trash; // double so that this storage is aligned. char trash[8] would not be aligned.
        void *targets[9] = {NULL, &trash, NULL, NULL, &trash, NULL, NULL, NULL, &trash};
        FieldParseContext fctx = {
          .ch = &ch,
          .targets = targets,
          .anchor = NULL,
        };
        while (type[field]<=CT_STRING) {
          fun[type[field]](&fctx);
          if (end_of_field(*ch)) break;
          skip_white(&ch);
          if (end_of_field(*ch)) break;
          ch = end_NA_string(fieldStart);
          if (end_of_field(*ch)) break;
          if (type[field]<CT_STRING) {
            ch = fieldStart;
            if (*ch==quote) {
              ch++;
              fun[type[field]](&fctx);
              if (*ch==quote && end_of_field(ch[1])) { ch++; break; }
            }
            type[field]++;
            while (disabled_parsers[type[field]]) type[field]++;
          } else {
            // the field could not be read with this quote rule, try again with next one
            // Trying the next rule will only be successful if the number of fields is consistent with it
            if (quoteRule >= 3) STOP("Even quoteRule 3 was insufficient!");
            if (verbose)
              DTPRINT("Bumping quote rule from %d to %d due to field %d on line %d of sampling jump %d starting <<%s>>\n",
                      quoteRule, quoteRule+1, field+1, jline, j, strlim(fieldStart,200));
            quoteRule++;
          }
          bumped=true;
          ch = fieldStart;
        }
        if (args.header==NA_BOOL8 && thisColumnNameWasString && type[field]<CT_STRING) {
          args.header=true;
          if (verbose) DTPRINT("  'header' determined to be true due to column %d having string on row 1 and a lower type (%s) on row 2\n", field+1, typeName[type[field]]);
        }
        if (*ch!=sep) break;
        if (sep==' ') {
          while (ch[1]==' ') ch++;
          if (ch[1]=='\r' || ch[1]=='\n' || ch[1]=='\0') { ch++; break; }
        }
        field++;
      }
      eol(&ch);
      if (ch==eof) {
        if (finalByte && type[ncol-1]!=previousLastColType) {
          // revert bump due to e.g. ,NA<eof> in the last field of last row where finalByte=='A' and N caused bump to character (test 894.0221)
          if (verbose) DTPRINT("  Reverted bump of final column from %d to %d on final field due to finalByte='%c'. This will trigger a reread. Finish the file properly with newline to avoid.\n",
                               previousLastColType, type[ncol-1], finalByte);
          type[ncol-1] = previousLastColType;
        }
        if ((finalByte==sep && sep!=' ') || (sep==' ' && finalByte!='\0' && finalByte!=' ')) field++;
      }
      if (field<ncol-1 && !fill) {
        if (*ch!='\n' && *ch!='\r' && *ch!='\0') {
          STOP("Internal error: line has finished early but not on an '\\n', '\\r' or '\\0' (fill=false). Please report as bug.");
        } else {
          STOP("Line %d has too few fields when detecting types. Use fill=TRUE to pad with NA. Expecting %d fields but found %d: <<%s>>%s",
               jline, ncol, field+1, strlim(jlineStart,200),
               nrowLimit<=(size_t)jline ? ". You have tried to request fewer rows with the nrows= argument but this will not help because the full sample is still taken for type consistency; set fill=TRUE instead." : "");
        }
      }
      if (field>=ncol || (*ch!='\n' && *ch!='\r' && *ch!='\0')) {   // >=ncol covers ==ncol. We do not expect >ncol to ever happen.
        STOP("Line %d from sampling jump %d starting <<%s>> has more than the expected %d fields. "
             "Separator '%c' occurs at position %d which is character %d of the last field: <<%s>>. "
             "Consider setting 'comment.char=' if there is a trailing comment to be ignored.",
             jline, j, strlim(jlineStart,10), ncol, *ch, (int)(ch-jlineStart+1), (int)(ch-fieldStart+1),
             strlim(fieldStart,200));
      }
      if (firstDataRowAfterPotentialColumnNames) {
        if (fill) for (int jj=field+1; jj<ncol; jj++) type[jj] = type0;
        firstDataRowAfterPotentialColumnNames = false;
      } else if (sampleLines==0) firstDataRowAfterPotentialColumnNames = true;  // To trigger 2nd row starting from type 1 again to compare to 1st row to decide if column names present
      ch += (*ch=='\n' || *ch=='\r');

      lastRowEnd = ch;
      int thisLineLen = (int)(ch-jlineStart);  // ch is now on start of next line so this includes line ending already
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
  if (lastSampleJumpOk) {
    while (ch<eof && isspace(*ch)) ch++;
    if (ch<eof)
      DTWARN("Found the last consistent line but text exists afterwards (discarded): <<%s>>", strlim(ch,200));
  } else {
    // nextGoodLine() was false for the last (extra) jump to check the end
    // must set lastRowEnd to eof accordingly otherwise it'll be left wherever the last good jump finished
    lastRowEnd = eof;
  }

  size_t estnrow=1;
  allocnrow = 1;
  meanLineLen = 0;
  bytesRead = 0;

  if (args.header==NA_BOOL8) {
    args.header=true;
    for (int j=0; j<ncol; j++)
      if (type[j]<CT_STRING) args.header=false;
    if (sampleLines<=1) {
      // e.g. fread("A,B\n")
      if (verbose) DTPRINT("  'header' determined to be %s because there are%s non-strings on the 1st and only row\n", args.header?"true":"false", args.header?" no":"");
    } else {
      if (verbose) DTPRINT("  'header' determined to be %s\n", args.header ?
                           "true because types of row 1 and row 2 are the same and all columns are string" :
                           "false because types of row 1 and row 2 are the same and at least one column is not string" );
    }
  }
  if (sampleLines<=1) {
    if (args.header==true) {
      for (int j=0; j<ncol; j++) type[j] = type0;
      // when column-names-only, 2nd row didn't happen, so initialize needs to be done now so that empty columns should be type logical
      // rather than the type of the column names (most likely string)
    }
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
      DTPRINT("  Sampled %llu rows (handled \\n inside quoted fields) at %d jump points\n", (llu)sampleLines, nJumps);
      DTPRINT("  Bytes from first data row on line %d to the end of last row: %llu\n", row1Line, (llu)bytesRead);
      DTPRINT("  Line length: mean=%.2f sd=%.2f min=%d max=%d\n", meanLineLen, sd, minLen, maxLen);
      DTPRINT("  Estimated number of rows: %llu / %.2f = %llu\n", (llu)bytesRead, meanLineLen, (llu)estnrow);
      DTPRINT("  Initial alloc = %llu rows (%llu + %d%%) using bytes/max(mean-2*sd,min) clamped between [1.1*estn, 2.0*estn]\n",
              (llu)allocnrow, (llu)estnrow, (int)(100.0*allocnrow/estnrow-100.0));
    }
    if (nJumps==1) {
      if (verbose) DTPRINT("  All rows were sampled since file is small so we know nrow=%llu exactly\n", (llu)sampleLines);
      estnrow = allocnrow = sampleLines;
    } else {
      if (sampleLines > allocnrow) STOP("Internal error: sampleLines(%llu) > allocnrow(%llu)", (llu)sampleLines, (llu)allocnrow);
    }
    if (nrowLimit < allocnrow) {
      if (verbose) DTPRINT("  Alloc limited to lower nrows=%llu passed in.\n", (llu)nrowLimit);
      estnrow = allocnrow = nrowLimit;
    }
    if (verbose) DTPRINT("  =====\n");
  }
  }

  //*********************************************************************************************
  // [8] Assign column names
  // Updates pos(ition) to rest after the column names (if any) at the start of the first data row
  //*********************************************************************************************
  double tLayout;  // Timer for assigning column names
  const char *colNamesAnchor = pos;
  {
  if (verbose) DTPRINT("[08] Assign column names\n");

  ch = pos;  // back to start of first row (likely column names)

  colNames = (lenOff*) calloc((size_t)ncol, sizeof(lenOff));
  if (!colNames) STOP("Unable to allocate %d*%d bytes for column name pointers: %s", ncol, sizeof(lenOff), strerror(errno));

  if (args.header==false) {
    // colNames was calloc'd so nothing to do; all len=off=0 already and default column names (V1, V2, etc) will be assigned
  } else {
    line++;
    if (sep==' ') while (*ch==' ') ch++;
    void *targets[9] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, colNames};
    FieldParseContext fctx = {
      .ch = &ch,
      .targets = targets,
      .anchor = colNamesAnchor,
    };
    ch--;
    for (int i=0; i<ncol; i++) {
      // Use Field() here as it handles quotes, leading space etc inside it
      ch++;
      Field(&fctx);  // stores the string length and offset as <uint,uint> in colNames[i]
      ((lenOff**) fctx.targets)[8]++;
      if (*ch!=sep) break;
      if (sep==' ') {
        while (ch[1]==' ') ch++;
        if (ch[1]=='\r' || ch[1]=='\n' || ch[1]=='\0') { ch++; break; }
      }
    }
    if (eol(&ch)) pos = ++ch;
    else if (*ch=='\0') pos = ch;
    else STOP("Internal error: reading colnames ending on '%c'", *ch);
    // now on first data row (row after column names)
    // when fill=TRUE and column names shorter (test 1635.2), leave calloc initialized lenOff.len==0
  }
  tLayout = wallclock();
  }


  //*********************************************************************************************
  // [9] Apply colClasses, select, drop and integer64
  //*********************************************************************************************
  double tColType;    // Timer for applying user column class overrides
  int ndrop;          // Number of columns dropped that will be dropped from the file being read
  int nStringCols;    // Number of string columns in the file
  int nNonStringCols; // Number of all other columns in the file
  size_t rowSize1;    // Total bytesize of all fields having sizeof==1
  size_t rowSize4;    // Total bytesize of all fields having sizeof==4
  size_t rowSize8;    // Total bytesize of all fields having sizeof==8
  {
  if (verbose) DTPRINT("[09] Apply user overrides on column types\n");
  ch = pos;
  oldType = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));
  if (!oldType) STOP("Unable to allocate %d bytes to check user overrides of column types", ncol);
  memcpy(oldType, type, (size_t)ncol) ;
  if (!userOverride(type, colNames, colNamesAnchor, ncol)) { // colNames must not be changed but type[] can be
    if (verbose) DTPRINT("  Cancelled by user: userOverride() returned false.");
    freadCleanup();
    return 1;
  }
  ndrop = 0;
  int nUserBumped=0;
  rowSize1 = 0;
  rowSize4 = 0;
  rowSize8 = 0;
  nStringCols = 0;
  nNonStringCols = 0;
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
    DTPRINT("  After %d type and %d drop user overrides : ", nUserBumped, ndrop);
    printTypes(ncol); DTPRINT("\n");
  }
  tColType = wallclock();
  }


  //*********************************************************************************************
  // [10] Allocate the result columns
  //*********************************************************************************************
  double tAlloc;  // Timer for allocating the DataTable
  size_t DTbytes; // Size of the allocated DataTable, in bytes
  {
  if (verbose) DTPRINT("[10] Allocate memory for the datatable\n");
  if (verbose) {
    DTPRINT("  Allocating %d column slots (%d - %d dropped) with %llu rows\n",
            ncol-ndrop, ncol, ndrop, (llu)allocnrow);
  }
  DTbytes = allocateDT(type, size, ncol, ndrop, allocnrow);
  tAlloc = wallclock();
  }


  //*********************************************************************************************
  // [11] Read the data
  //*********************************************************************************************
  int hasPrinted=0;  // the percentage last printed so it prints every 2% without many calls to wallclock()
  bool stopTeam=false, firstTime=true;  // bool for MT-safey (cannot ever read half written bool value)
  int nTypeBump=0, nTypeBumpCols=0;
  double tRead=0, tReread=0, tTot=0;  // overall timings outside the parallel region
  double thNextGoodLine=0, thRead=0, thPush=0;  // reductions of timings within the parallel region
  char *typeBumpMsg=NULL;  size_t typeBumpMsgSize=0;
  #define stopErrSize 1000
  char stopErr[stopErrSize+1]="";  // must be compile time size: the message is generated and we can't free before STOP
  size_t DTi = 0;   // the current row number in DT that we are writing to
  const char *prevJumpEnd = pos;  // the position after the last line the last thread processed (for checking)
  int buffGrown=0;
  // chunkBytes is the distance between each jump point; it decides the number of jumps
  // We may want each chunk to write to its own page of the final column, hence 1000*maxLen
  // For the 44GB file with 12875 columns, the max line len is 108,497. We may want each chunk to write to its
  // own page (4k) of the final column, hence 1000 rows of the smallest type (4 byte int) is just
  // under 4096 to leave space for R's header + malloc's header.
  size_t chunkBytes = umax((size_t)(1000*meanLineLen), 1ULL/*MB*/ *1024*1024);
  // Index of the first jump to read. May be modified if we ever need to restart
  // reading from the middle of the file.
  int jump0 = 0;
  // If we need to restart reading the file because we ran out of allocation
  // space, then this variable will tell how many new rows has to be allocated.
  size_t extraAllocRows = 0;

  if (nJumps/*from sampling*/>1) {
    // ensure data size is split into same sized chunks (no remainder in last chunk) and a multiple of nth
    // when nth==1 we still split by chunk for consistency (testing) and code sanity
    nJumps = (int)(bytesRead/chunkBytes);
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
  {
  if (verbose) {
    DTPRINT("[11] Read the data\n");
    DTPRINT("  jumps=[%d..%d), chunk_size=%llu, total_size=%llu\n", jump0, nJumps, (llu)chunkBytes, (llu)(lastRowEnd-pos));
  }
  ASSERT(allocnrow <= nrowLimit, "allocnrow(%llu) < nrowLimit(%llu)", (llu)allocnrow, (llu)nrowLimit);
  #pragma omp parallel num_threads(nth)
  {
    int me = omp_get_thread_num();
    #pragma omp master
    nth = omp_get_num_threads();
    const char *thisJumpStart=NULL;  // The first good start-of-line after the jump point
    size_t myNrow = 0; // the number of rows in my chunk
    size_t myBuffRows = initialBuffRows;  // Upon realloc, myBuffRows will increase to grown capacity

    // Allocate thread-private row-major `myBuff`s
    ThreadLocalFreadParsingContext ctx = {
      .anchor = NULL,
      .buff8 = malloc(rowSize8 * myBuffRows + 8),
      .buff4 = malloc(rowSize4 * myBuffRows + 4),
      .buff1 = malloc(rowSize1 * myBuffRows + 1),
      .rowSize8 = rowSize8,
      .rowSize4 = rowSize4,
      .rowSize1 = rowSize1,
      .DTi = 0,  // which row in the final DT result I should start writing my chunk to
      .nRows = allocnrow,
      .threadn = me,
      .quoteRule = quoteRule,
      .stopTeam = &stopTeam,
      #ifndef DTPY
      .nStringCols = nStringCols,
      .nNonStringCols = nNonStringCols
      #endif
    };
    if ((rowSize8 && !ctx.buff8) || (rowSize4 && !ctx.buff4) || (rowSize1 && !ctx.buff1)) {
      stopTeam = true;
    }
    prepareThreadContext(&ctx);

    #pragma omp for ordered schedule(dynamic) reduction(+:thNextGoodLine,thRead,thPush)
    for (int jump = jump0; jump < nJumps; jump++) {
      if (stopTeam) continue;  // must be continue and not break. We desire not to depend on (newer) omp cancel yet
      double tt0 = 0, tt1 = 0;
      if (verbose) { tt1 = tt0 = wallclock(); }

      if (myNrow) {
        // On the 2nd iteration onwards for this thread, push the data from the previous jump
        // Convoluted because the ordered section has to be last in some OpenMP implementations :
        // http://stackoverflow.com/questions/43540605/must-ordered-be-at-the-end
        // Hence why loop goes to nJumps+nth. Logically, this clause belongs after the ordered section.

        // Push buffer now to impl so that :
        //   i) lenoff.off can be "just" 32bit int from a local anchor rather than a 64bit offset from a global anchor
        //  ii) impl can do it in parallel if it wishes, and it can have an orphan critical directive if it wishes
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
            // TODO: Add ETA. Ok to call wallclock() here.
            progress(p, /*eta*/0);
            hasPrinted = p+2;  // update every 2%
          }
        }
        myNrow = 0;
      }

      const char *tch = pos + (size_t)jump*chunkBytes;
      const char *nextJump = jump<nJumps-1 ? tch+chunkBytes+1/*\n*/ : lastRowEnd;
      // +1 is for when nextJump happens to fall exactly on a \n. The
      // next thread will start one line later because nextGoodLine() starts by finding next eol.
      // Even when nextGoodLine goes away, we still want the +1 to avoid always running-on.
      // The correctness of this comment and logic here may have changed when eol and eolLen went away, but
      // the code must be working otherwise the thread-hand-over check (last end == this start) would be failing.
      if (jump>0 && !nextGoodLine(&tch, ncol)) {
        stopTeam=true;
        DTPRINT("No good line could be found from jump point %d\n",jump); // TODO: change to stopErr
        continue;
      }
      thisJumpStart=tch;
      if (verbose) { tt1 = wallclock(); thNextGoodLine += tt1 - tt0; tt0 = tt1; }

      void *targets[9] = {NULL, ctx.buff1, NULL, NULL, ctx.buff4, NULL, NULL, NULL, ctx.buff8};
      FieldParseContext fctx = {
        .ch = &tch,
        .targets = targets,
        .anchor = thisJumpStart,
      };

      while (tch<nextJump) {
        if (myNrow == myBuffRows) {
          // buffer full due to unusually short lines in this chunk vs the sample; e.g. #2070
          myBuffRows *= 1.5;
          #pragma omp atomic
          buffGrown++;
          ctx.buff8 = realloc(ctx.buff8, rowSize8 * myBuffRows + 8);
          ctx.buff4 = realloc(ctx.buff4, rowSize4 * myBuffRows + 4);
          ctx.buff1 = realloc(ctx.buff1, rowSize1 * myBuffRows + 1);
          if ((rowSize8 && !ctx.buff8) || (rowSize4 && !ctx.buff4) || (rowSize1 && !ctx.buff1)) {
            stopTeam = true;
            break;
          }
          // shift current buffer positions, since `myBuffX`s were probably moved by realloc
          fctx.targets[8] = (void*)((char*)ctx.buff8 + myNrow * rowSize8);
          fctx.targets[4] = (void*)((char*)ctx.buff4 + myNrow * rowSize4);
          fctx.targets[1] = (void*)((char*)ctx.buff1 + myNrow * rowSize1);
        }
        const char *tlineStart = tch;  // for error message
        const char *fieldStart = tch;
        int j = 0;

        int finalFieldLen = 0;  // only used when finalByte
        char finalSep = '\0';   // only used when finalByte. It is correct to be '\0' and not 127 (it's either set (!=0) or not (==0))
        oneLastTimeIfFinalByte:

        //*** START HOT ***//
        if (sep!=' ' && !any_number_like_NAstrings) {  // TODO:  can this 'if' be dropped somehow? Can numeric NAstrings be dealt with afterwards in one go as numeric comparison?
          // Try most common and fastest branch first: no whitespace, no quoted numeric, ",," means NA
          while (j < ncol) {
            // DTPRINT("Field %d: '%.10s' as type %d  (tch=%p)\n", j+1, tch, type[j], tch);
            fieldStart = tch;
            int8_t thisType = type[j];  // fetch shared type once. Cannot read half-written byte is one reason type's type is single byte to avoid atomic read here.
            int8_t thisSize = size[j];
            fun[abs(thisType)](&fctx);
            if (*tch!=sep) break;
            ((char **) targets)[thisSize] += thisSize;
            tch++;
            j++;
          }
          //*** END HOT. START TEPID ***//
          if (tch==tlineStart) {
            skip_white(&tch);
            if (*tch=='\0') break;  // empty last line
            if (eol(&tch) && skipEmptyLines) { tch++; continue; }
            tch = tlineStart;  // in case white space at the beginning may need to be including in field
          }
          else if (eol(&tch)) {
            int8_t thisSize = size[j];
            ((char **) targets)[thisSize] += thisSize;
            j++;
            if (j==ncol) { tch++; myNrow++; continue; }  // next line. Back up to while (tch<nextJump). Usually happens, fastest path
          }
          else {
            tch = fieldStart; // restart field as int processor could have moved to A in ",123A,", or we could be on \0 when we always reread last field regardless
          }
          // if *tch=='\0' then fall through below and reread final field if finalByte is set
        }
        //*** END TEPID. NOW COLD.

        // Either whitespace surrounds field in which case the processor will fault very quickly, it's numeric but quoted (quote will fault the non-string processor),
        // it contains an NA string, or there's an out-of-sample type bump needed.
        // In all those cases we're ok to be a bit slower. The rest of this line will be processed using the slower version.
        // (End-of-file) is also dealt with now, as could be the highly unusual line ending /n/r
        // This way (each line has new opportunity of the fast path) if only a little bit of the file is quoted (e.g. just when commas are present as fwrite does)
        // then a penalty isn't paid everywhere.
        // TODO: reduce(slowerBranch++). So we can see in verbose mode if this is happening too much.

        if (sep==' ') {
          while (*tch==' ') tch++;  // multiple sep=' ' at the tlineStart does not mean sep. We're at tLineStart because the fast branch above doesn't run when sep=' '
          fieldStart = tch;
        }
        if (fill || (*tch!='\n' && *tch!='\r')) while (j < ncol) {
          fieldStart = tch;
          int8_t joldType = type[j];
          int8_t thisType = joldType;  // to know if it was bumped in (rare) out-of-sample type exceptions
          int8_t absType = (int8_t)abs(thisType);

          while (absType < NUMTYPE) {
            tch = fieldStart;
            bool quoted = false;
            if (absType < CT_STRING) {
              skip_white(&tch);
              const char *afterSpace = tch;
              tch = end_NA_string(fieldStart);
              skip_white(&tch);
              if (!end_of_field(*tch)) tch = afterSpace; // else it is the field_end, we're on closing sep|eol and we'll let processor write appropriate NA as if field was empty
              if (*tch==quote) { quoted=true; tch++; }
            } // else Field() handles NA inside it unlike other processors e.g. ,, is interpretted as "" or NA depending on option read inside Field()
            fun[abs(thisType)](&fctx);
            if (quoted && *tch==quote) tch++;
            skip_white(&tch);
            if (end_of_field(*tch)) {
              if (sep==' ' && *tch==' ') {
                while (tch[1]==' ') tch++;  // multiple space considered one sep so move to last
                if (tch[1]=='\r' || tch[1]=='\n' || tch[1]=='\0') tch++;
              }
              break;
            }

            // guess is insufficient out-of-sample, type is changed to negative sign and then bumped. Continue to
            // check that the new type is sufficient for the rest of the column (and any other columns also in out-of-sample bump status) to be
            // sure a single re-read will definitely work.
            absType++;
            while (disabled_parsers[absType]) absType++;
            thisType = -absType;
            tch = fieldStart;
          }

          if (thisType != joldType             // rare out-of-sample type exception.
              && (!finalByte || finalSep)) {   // don't bump the final field until we've replaced the finalByte (if any) test 894.0221 where final field is NA and finalByte=='A'
            #pragma omp critical
            {
              joldType = type[j];  // fetch shared value again in case another thread bumped it while I was waiting.
              // Can't PRINT because we're likely not master. So accumulate message and print afterwards.
              if (thisType < joldType) {   // thisType<0 (type-exception)
                char temp[1001];
                int len = snprintf(temp, 1000,
                  "Column %d (\"%.*s\") bumped from '%s' to '%s' due to <<%.*s>> on row %llu\n",
                  j+1, colNames[j].len, colNamesAnchor + colNames[j].off,
                  typeName[abs(joldType)], typeName[abs(thisType)],
                  (int)(tch-fieldStart), fieldStart, (llu)(ctx.DTi+myNrow));
                typeBumpMsg = (char*) realloc(typeBumpMsg, typeBumpMsgSize + (size_t)len + 1);
                strcpy(typeBumpMsg+typeBumpMsgSize, temp);
                typeBumpMsgSize += (size_t)len;
                nTypeBump++;
                if (joldType>0) nTypeBumpCols++;
                type[j] = thisType;
              } // else another thread just bumped to a (negative) higher or equal type while I was waiting, so do nothing
            }
          }
          ((char**) targets)[size[j]] += size[j];
          j++;
          if (*tch==sep) { tch++; continue; }
          if (tch==eof && finalByte==sep && sep!=' ') { finalByte='\0'; continue; }
          if (fill && (*tch=='\n' || *tch=='\r' || *tch=='\0') && j<ncol) continue;  // reuse processors to write appropriate NA to target; saves maintenance of a type switch down here
          break;
        }

        // The goal here is to handle the rare case of files that don't end with newline AND are also exactly a multiple of page size, PR#2200.
        // All because mmap fileSize+1 doesn't work on Mac and Windows when fileSize%pageSize==0 (bus error on read and write)
        if (finalByte && tch==eof) {
          if (j<=0 || *eof!='\0' || !args.filename || fieldStart<=(char *)mmp) {
            #pragma omp critical
            if (!stopTeam) {
              stopTeam = true;
              snprintf(stopErr, stopErrSize, "Internal error in final field: j<=0 || *eof!='\\0' || !args.filename || fieldStart<=mmp");
              // Direct non-file input is already ended with '\0'. finalByte is only ever set for files and only when they end abruptly without final newline
            }
            break;
          }
          if (!finalSep) {
            // Just read final field for the first time and finalSep hasn't been set yet.
            // Shift final field back 1 character in cow pages(s) and reread it.
            // fieldStart will be correct even in very complicated cases where for example jump points are
            //   skipped because we delay and isolate this final field logic to this latest point when we really need it
            // We do not allocate a copy for the last field because that would need *eof to be moved to the copy's end, but eof is shared across threads.
            finalFieldLen = (int)(tch-fieldStart);
            finalSep = fieldStart[-1];  // remember the byte we're shifting back over; either sep or newline. fieldStart>mmp was checked above.
            memmove(_const_cast(fieldStart-1), fieldStart, (size_t)finalFieldLen);  // cow page.
            *_const_cast(eof-1) = finalByte;  // *eof=='\0' already
            tch = fieldStart-1;
            j--;
            ((char**) targets)[size[j]] -= size[j];
            goto oneLastTimeIfFinalByte;  // reread final field now that it has been shifted back one byte and the finalByte placed at the end
          } else if (nTypeBump) {
            // The very final field was just jiggled and reread due to finalByte!='\0' and finalSep!='\0'.
            // But there are out-of-sample type exceptions (nTypeBump>0) so there will be a reread next. So we have to shift the final field
            // forwards again and put back the finalSep before the final field.
            // If no reread is about to happen however (nTypeBump==0), careful to leave field shifted back since lenOff points there if it is string.
            memmove(_const_cast(fieldStart+1), fieldStart, (size_t)finalFieldLen); // shift final field forwards
            *_const_cast(fieldStart) = finalSep;  // put the finalField's preceeding sep or newline back again
            finalSep = '\0';  // important to reset ready for the whole file reread so that the final field gets shifted back again, a 2nd and final time
            finalFieldLen = 0;
          }
        }
        if (j < ncol)  {
          // not enough columns observed (including empty line). If fill==true, fields should already have been filled above due to continue inside while(j<ncol)
          #pragma omp critical
          if (!stopTeam) {
            stopTeam = true;
            snprintf(stopErr, stopErrSize,
              "Expecting %d cols but row %llu contains only %d cols (sep='%c'). " \
              "Consider fill=true. <<%s>>",
              ncol, (llu)ctx.DTi, j, sep, strlim(tlineStart, 500));
          }
          break;
        }
        if (!eol(&tch) && *tch!='\0') {
          #pragma omp critical
          if (!stopTeam) {
            stopTeam = true;
            snprintf(stopErr, stopErrSize,
              "Too many fields on out-of-sample row %llu. Read all %d expected columns but more are present. <<%s>>",
              (llu)ctx.DTi, ncol, strlim(tlineStart, 500));
          }
          break;
        }
        if (*tch!='\0') tch++;
        myNrow++;
      }
      if (verbose) { tt1 = wallclock(); thRead += tt1 - tt0; tt0 = tt1; }
      ctx.anchor = thisJumpStart;
      ctx.nRows = myNrow;
      postprocessBuffer(&ctx);

      #pragma omp ordered
      {
        // stopTeam could be true if a previous thread already stopped while I was waiting my turn
        if (!stopTeam && prevJumpEnd != thisJumpStart && jump > jump0) {
          snprintf(stopErr, stopErrSize,
            "Jump %d did not finish counting rows exactly where jump %d found its first good line start: "
            "prevEnd(%p)<<%s>> != thisStart(prevEnd%+d)<<%s>>",
            jump-1, jump, (const void*)prevJumpEnd, strlim(prevJumpEnd,50),
            (int)(thisJumpStart-prevJumpEnd), strlim(thisJumpStart,50));
          stopTeam=true;
        }
        ctx.DTi = DTi;  // fetch shared DTi (where to write my results to the answer). The previous thread just told me.
        if (ctx.DTi >= allocnrow) {  // a previous thread has already reached the `allocnrow` limit
          stopTeam = true;
          myNrow = 0;
        } else if (myNrow + ctx.DTi > allocnrow) {  // current thread has reached `allocnrow` limit
          if (allocnrow == nrowLimit) {
            // allocnrow is the same as nrowLimit, no need to reallocate the DT,
            // just truncate the rows in the current chunk.
            myNrow = nrowLimit - ctx.DTi;
          } else {
            // We reached `allocnrow` limit, but there are more data to read
            // left. In this case we arrange to terminate all threads but
            // remember the position where the previous thread has finished. We
            // will reallocate the DT and restart reading from the same point.
            jump0 = jump;
            extraAllocRows = (size_t)((double)(DTi+myNrow)*nJumps/(jump+1) * 1.2) - allocnrow;
            if (extraAllocRows < 1024) extraAllocRows = 1024;
            myNrow = 0;
            stopTeam = true;
          }
        }
                           // tell next thread (she not me) 2 things :
        prevJumpEnd = tch; // i) the \n I finished on so she can check (above) she started exactly on that \n good line start
        DTi += myNrow;     // ii) which row in the final result she should start writing to since now I know myNrow.
        ctx.nRows = myNrow;
        orderBuffer(&ctx);
      }
      // END ORDERED.
      // Next thread can now start her ordered section and write her results to the final DT at the same time as me.
      // Ordered has to be last in some OpenMP implementations currently. Logically though, pushBuffer happens now.
    }
    // End for loop over all jump points

    // Push out all buffers one last time.
    if (myNrow) {
      double tt1 = verbose? wallclock() : 0;
      pushBuffer(&ctx);
      if (verbose) thRead += wallclock() - tt1;
    }
    // Each thread to free their own buffer.
    free(ctx.buff8); ctx.buff8 = NULL;
    free(ctx.buff4); ctx.buff4 = NULL;
    free(ctx.buff1); ctx.buff1 = NULL;
    freeThreadContext(&ctx);
  }
  //-- end parallel ------------------
  }

  if (extraAllocRows) {
    allocnrow += extraAllocRows;
    if (allocnrow > nrowLimit) allocnrow = nrowLimit;
    if (verbose) DTPRINT("  Too few rows allocated. Allocating additional %llu rows (now nrows=%llu) and continue reading from jump point %d\n", (llu)extraAllocRows, (llu)allocnrow, jump0);
    allocateDT(type, size, ncol, ncol - nStringCols - nNonStringCols, allocnrow);
    extraAllocRows = 0;
    stopTeam = false;
    goto read;
  }


  //*********************************************************************************************
  // [12] Finalize the datatable
  //*********************************************************************************************
  {
  if (hasPrinted && verbose) DTPRINT("\n");
  if (verbose) DTPRINT("[12] Finalizing the datatable\n");
  if (firstTime) {
    tReread = tRead = wallclock();
    tTot = tRead-t0;
    if (hasPrinted || verbose) {
      DTPRINT("\rRead %llu rows x %d columns from %s file in ", (llu)DTi, ncol-ndrop, filesize_to_str(fileSize));
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
      DTPRINT("\rReread %llu rows x %d columns in ", (llu)DTi, nTypeBumpCols);
      DTPRINT("%02d:%06.3f\n", (int)(tReread-tRead)/60, fmod(tReread-tRead,60.0));
    }
  }
  if (stopTeam && stopErr[0]!='\0') STOP(stopErr); // else nrowLimit applied and stopped early normally
  if (DTi > allocnrow) {
    if (nrowLimit > allocnrow) STOP("Internal error: DTi(%llu) > allocnrow(%llu) but nrows=%llu (not limited)",
                                    (llu)DTi, (llu)allocnrow, (llu)nrowLimit);
    // for the last jump that fills nrow limit, then ansi is +=buffi which is >allocnrow and correct
  } else if (DTi == allocnrow) {
    if (verbose) DTPRINT("Read %llu rows. Exactly what was estimated and allocated up front\n", (llu)DTi);
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
    prevJumpEnd = pos;
    firstTime = false;
    nTypeBump = 0; // for test 1328.1. Otherwise the last field would get shifted forwards again.
    goto read;
  }
  if (verbose) {
    DTPRINT("=============================\n");
    if (tTot<0.000001) tTot=0.000001;  // to avoid nan% output in some trivially small tests where tot==0.000s
    DTPRINT("%8.3fs (%3.0f%%) Memory map %.3fGB file\n", tMap-t0, 100.0*(tMap-t0)/tTot, 1.0*fileSize/(1024*1024*1024));
    { DTPRINT("%8.3fs (%3.0f%%) sep=", tLayout-tMap, 100.0*(tLayout-tMap)/tTot);
      DTPRINT(sep=='\t' ? "'\\t'" : (sep=='\n' ? "'\\n'" : "'%c'"), sep);
      DTPRINT(" ncol=%d and header detection\n", ncol); }
    DTPRINT("%8.3fs (%3.0f%%) Column type detection using %llu sample rows\n",
            tColType-tLayout, 100.0*(tColType-tLayout)/tTot, (llu)sampleLines);
    DTPRINT("%8.3fs (%3.0f%%) Allocation of %llu rows x %d cols (%.3fGB)\n",
            tAlloc-tColType, 100.0*(tAlloc-tColType)/tTot, (llu)allocnrow, ncol, DTbytes/(1024.0*1024*1024));
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
  }

  return 1;
}
