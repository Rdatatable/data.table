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

// Private globals to save passing all of them through to highly iterated field processors
static const char *sof, *eof;
static char sep;
static char whiteChar; // what to consider as whitespace to skip: ' ', '\t' or 0 means both (when sep!=' ' && sep!='\t')
static char quote, dec;
static bool eol_one_r;  // only true very rarely for \r-only files

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

// Private globals so they can be cleaned up both on error and on successful return
static void *mmp = NULL;
static void *mmp_copy = NULL;
static size_t fileSize;
static int8_t *type = NULL, *tmpType = NULL, *size = NULL;
static lenOff *colNames = NULL;
static freadMainArgs args;  // global for use by DTPRINT

const char typeName[NUMTYPE][10] = {"drop", "bool8", "bool8", "bool8", "bool8", "int32", "int64", "float64", "float64", "float64", "int32", "float64", "string"};
int8_t     typeSize[NUMTYPE]     = { 0,      1,       1,       1,       1,       4,       8,       8,         8,         8,         4,       8       ,  8      };

// In AIX, NAN and INFINITY don't qualify as constant literals. Refer: PR #3043
// So we assign them through below init function.
static double NAND;
static double INFD;

// NAN and INFINITY constants are float, so cast to double once up front.
void init() {
  NAND = (double)NAN;
  INFD = (double)INFINITY;
}

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

// note -- because ASSERT is doing literal char array concatenation, it is
//   not possible to do translation of its messages without refactoring --
//   essentially that would come down to creating f() g() in the code after
//   macro expansion, which is not valid. These are internal errors, so just concede.
#define ASSERT(cond, msg, ...) \
  if (!(cond)) STOP(_("Internal error in line %d of fread.c, please report on data.table GitHub:  " msg), __LINE__, __VA_ARGS__) // # nocov



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
  bool neededCleanup = (type || tmpType || size || colNames || mmp || mmp_copy);
  free(type); type = NULL;
  free(tmpType); tmpType = NULL;
  free(size); size = NULL;
  free(colNames); colNames = NULL;
  if (mmp != NULL) {
    // Important to unmap as OS keeps internal reference open on file. Process is not exiting as
    // we're a .so/.dll here. If this was a process exiting we wouldn't need to unmap.
    //
    // Note that if there was an error unmapping the view of file, then we should not attempt
    // to call STOP() for 2 reasons: 1) freadCleanup() may have itself been called from STOP(),
    // and we would not want to overwrite the original error message; and 2) STOP() function
    // may call freadCleanup(), thus resulting in an infinite loop.
    #ifdef WIN32
      if (!UnmapViewOfFile(mmp))
        DTPRINT(_("System error %d unmapping view of file\n"), GetLastError());      // # nocov
    #else
      if (munmap(mmp, fileSize))
        DTPRINT(_("System errno %d unmapping file: %s\n"), errno, strerror(errno));  // # nocov
    #endif
    mmp = NULL;
  }
  free(mmp_copy); mmp_copy = NULL;
  fileSize = 0;
  sep = whiteChar = quote = dec = '\0';
  quoteRule = -1;
  any_number_like_NAstrings = false;
  blank_is_a_NAstring = false;
  stripWhite = true;
  skipEmptyLines = false;
  eol_one_r = false;
  fill = false;
  // following are borrowed references: do not free
  sof = eof = NULL;
  NAstrings = NULL;
  return neededCleanup;
}

#define CEIL(x)  ((uint64_t)(double)ceil(x))
static inline uint64_t umax(uint64_t a, uint64_t b) { return a > b ? a : b; }
static inline uint64_t umin(uint64_t a, uint64_t b) { return a < b ? a : b; }
static inline  int64_t imin( int64_t a,  int64_t b) { return a < b ? a : b; }

/** Return value of `x` clamped to the range [upper, lower] */
static inline int64_t clamp_szt(int64_t x, int64_t lower, int64_t upper) {
  return x < lower ? lower : x > upper? upper : x;
}


/**
 * Helper for error and warning messages to extract an input line starting at
 * `*ch` and until an end of line, but no longer than `limit` characters.
 * This function returns the string copied into an internal static buffer. Cannot
 * be called more than twice per single printf() invocation.
 * Parameter `limit` cannot exceed 500.
 * The data might contain % characters. Therefore, careful to ensure that if the msg
 * is constructed manually (using say snprintf) that warning(), stop()
 * and Rprintf() are all called as warning(_("%s"), msg) and not warning(msg).
 */
static const char* strlim(const char *ch, size_t limit) {
  static char buf[1002];
  static int flip = 0;
  char *ptr = buf + 501 * flip;
  flip = 1 - flip;
  char *ch2 = ptr;
  if (limit>500) limit=500;
  size_t width = 0;
  while ((*ch>'\r' || (*ch!='\0' && *ch!='\r' && *ch!='\n')) && width++<limit) {
    *ch2++ = *ch++;
  }
  *ch2 = '\0';
  return ptr;
}

static char *typeLetter = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

static char *typesAsString(int ncol) {
  int nLetters = strlen(typeLetter);
  if (NUMTYPE>nLetters) STOP(_("Internal error: NUMTYPE(%d) > nLetters(%d)"), NUMTYPE, nLetters); // # nocov
  static char str[101];
  int i=0;
  if (ncol<=100) {
    for (; i<ncol; i++) str[i] = typeLetter[abs(type[i])];  // abs for out-of-sample type bumps (negative)
  } else {
    for (; i<80; i++) str[i] = typeLetter[abs(type[i])];
    str[i++]='.'; str[i++]='.'; str[i++]='.';
    for (int j=ncol-10; j<ncol; j++) str[i++] = typeLetter[abs(type[j])];
  }
  str[i] = '\0';
  return str;
}


static inline void skip_white(const char **pch) {
  // skip space so long as sep isn't space and skip tab so long as sep isn't tab
  // always skip any \0 (NUL) that occur before end of file, #3400
  const char *ch = *pch;
  if (whiteChar==0) {   // whiteChar==0 means skip both ' ' and '\t';  sep is neither ' ' nor '\t'.
    while (*ch==' ' || *ch=='\t' || (*ch=='\0' && ch<eof)) ch++;
  } else {
    while (*ch==whiteChar || (*ch=='\0' && ch<eof)) ch++;  // sep is ' ' or '\t' so just skip the other one.
  }
  *pch = ch;
}


/**
 * eol() accepts a position and, if any of the following line endings, moves to the end of that sequence
 * and returns true. Repeated \\r around \n are considered one. At most one \\n will be moved over, though.
 * 1. \\n        Unix
 * 2. \\r\\n     Windows
 * 3. \\r\\r\\n  R's download.file() in text mode doubling up \\r
 * 4. \\r        Old MacOS 9 format discontinued in 2002 but then #2347 was raised straight away when I tried not to support it, #2371
 * 5. \\n\\r     Acorn BBC (!) and RISC OS according to Wikipedia.
 * 6. \\r\\r\\r  Not supported. So that blank lines in single column files generate NA ok, #2542.
 */
static inline bool eol(const char **pch) {
  const char *ch = *pch;
  // we call eol() when we expect to be on an eol(), so optimize as if we are on an eol
  while (*ch=='\r') ch++;  // commonly happens once on Windows for type 2
  if (*ch=='\n') {
    // 1,2,3 and 5 (one \n with any number of \r before and/or after)
    // most common branch as we only call eol() when we expect to be on eol
    while (ch[1]=='\r') ch++;  // type 5. Could drop but we're only tepid here so keep for completeness and full generality.
    *pch = ch;
    return true;
  }
  return eol_one_r && **pch=='\r';
}


/**
 * Return True iff `ch` is a valid field terminator character: either a field
 * separator or a newline.
 */
static inline bool end_of_field(const char *ch) {
  // \r is 13, \n is 10, and \0 is 0. The second part is optimized based on the
  // fact that the characters in the ASCII range 0..13 are very rare, so a
  // single check `ch<=13` is almost equivalent to checking whether `ch` is one
  // of \r, \n, \0. We cast to unsigned first because `char` type is signed by
  // default, and therefore characters in the range 0x80-0xFF are negative.
  // We use eol() because that looks at eol_one_r inside it w.r.t. \r
  // \0 (maybe more than one) before eof are part of field and do not end it; eol() returns false for \0 but the ch==eof will return true for the \0 at eof.
  return *ch==sep || ((uint8_t)*ch<=13 && (ch==eof || eol(&ch)));
}


static inline const char *end_NA_string(const char *start) {
  // start should be at the beginning of any potential NA string, after leading whitespace skipped by caller
  const char* const* nastr = NAstrings;
  const char *mostConsumed = start; // tests 1550* includes both 'na' and 'nan' in nastrings. Don't stop after 'na' if 'nan' can be consumed too.
  if (nastr) while (*nastr) {
    const char *ch1 = start;
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
static inline int countfields(const char **pch)
{
  static lenOff trash;  // see comment on other trash declarations
  static void *targets[9];
  targets[8] = (void*) &trash;
  const char *ch = *pch;
  if (sep==' ') while (*ch==' ') ch++;  // multiple sep==' ' at the start does not mean sep
  skip_white(&ch);
  if (eol(&ch) || ch==eof) {
    *pch = ch+1;
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
      if (ch[1]=='\r' || ch[1]=='\n' || (ch[1]=='\0' && ch+1==eof)) {
        // reached end of line. Ignore padding spaces at the end of line.
        ch++;  // Move onto end of line character
      }
    }
    if (*ch==sep) {
      ch++;
      ncol++;
      continue;
    }
    if (eol(&ch)) { *pch=ch+1; return ncol; }
    if (ch!=eof) return -1;  // -1 means this line not valid for this sep and quote rule
    break;
  }
  *pch = ch;
  return ncol;
}


static inline const char *nextGoodLine(const char *ch, int ncol)
{
  // We may have landed inside a quoted field containing embedded sep and/or embedded \n.
  // Find next \n and see if ncol fields are found there. If not, test the \n after that, etc.
  // If this doesn't return the true line start, no matter. The previous thread will run-on and
  // resolve it. A good guess is all we need here. Being wrong will just be a bit slower.
  // If there are no embedded newlines, all newlines are true, and this guess will never be wrong.
  while (*ch!='\n' && *ch!='\r' && (*ch!='\0' || ch<eof)) ch++;
  if (ch==eof) return eof;
  if (eol(&ch)) // move to last byte of the line ending sequence (e.g. \r\r\n would be +2).
    ch++;       // and then move to first byte of next line
  const char *simpleNext = ch;  // simply the first newline after the jump
  // if a better one can't be found, return this one (simpleNext). This will be the case when
  // fill=TRUE and the jump lands before 5 too-short lines, for example.
  int attempts=0;
  while (attempts++<5 && ch<eof) {
    const char *ch2 = ch;
    if (countfields(&ch2)==ncol) return ch;  // returns simpleNext here on first attempt, almost all the time
    while (*ch!='\n' && *ch!='\r' && (*ch!='\0' || ch<eof)) ch++;
    if (eol(&ch)) ch++;
  }
  return simpleNext;
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
  size_t lsize = fsize;
  for (int i = 0; i <= NSUFFIXES; i++) {
    int shift = (NSUFFIXES - i) * 10;
    if ((fsize >> shift) == 0) continue;
    int ndigits = 3;
    for (; ndigits >= 1; ndigits--) {
      if ((fsize >> (shift + 12 - ndigits * 3)) == 0) break;
    }
    if (ndigits == 0 || (fsize == (fsize >> shift << shift))) {
      if (i < NSUFFIXES) {
        snprintf(output, BUFFSIZE, "%"PRIu64"%cB (%"PRIu64" bytes)",
                 (uint64_t)(lsize >> shift), suffixes[i], (uint64_t)lsize);
        return output;
      }
    } else {
      snprintf(output, BUFFSIZE, "%.*f%cB (%"PRIu64" bytes)",
               ndigits, (double)fsize / (1LL << shift), suffixes[i], (uint64_t)lsize);
      return output;
    }
  }
  if (fsize == 1) return one_byte;
  snprintf(output, BUFFSIZE, "%"PRIu64" bytes", (uint64_t)lsize);
  return output;
}

void copyFile(size_t fileSize, const char *msg, bool verbose)  // only called in very very rare cases
{
  double tt = wallclock();
  mmp_copy = (char *)malloc((size_t)fileSize + 1/* extra \0 */);
  if (!mmp_copy) STOP(_("Unable to allocate %s of contiguous virtual RAM. %s allocation."), filesize_to_str(fileSize), msg);
  memcpy(mmp_copy, mmp, fileSize);
  sof = mmp_copy;
  eof = (char *)mmp_copy + fileSize;
  tt = wallclock()-tt;
  if (tt>0.5) DTPRINT(_("Avoidable %.3f seconds. %s time to copy.\n"), tt, msg);  // not warning as that could feasibly cause CRAN tests to fail, say, if test machine is heavily loaded
  if (verbose) DTPRINT(_("  File copy in RAM took %.3f seconds.\n"), tt);
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
  if ((*ch==' ' && stripWhite) || (*ch=='\0' && ch<eof))
    while(*++ch==' ' || (*ch=='\0' && ch<eof));  // if sep==' ' the space would have been skipped already and we wouldn't be on space now.
  const char *fieldStart=ch;
  if (*ch!=quote || quoteRule==3 || quote=='\0') {
    // Most common case. Unambiguously not quoted. Simply search for sep|eol. If field contains sep|eol then it should have been quoted and we do not try to heal that.
    while(!end_of_field(ch)) ch++;  // sep, \r, \n or eof will end
    *(ctx->ch) = ch;
    int fieldLen = (int)(ch-fieldStart);
    //if (stripWhite) {   // TODO:  do this if and the next one together once in bulk afterwards before push
      while(fieldLen>0 && ((ch[-1]==' ' && stripWhite) || ch[-1]=='\0')) { fieldLen--; ch--; }
      // this space can't be sep otherwise it would have stopped the field earlier inside end_of_field()
    //}
    if ((fieldLen==0 && blank_is_a_NAstring) || (fieldLen && end_NA_string(fieldStart)==ch)) fieldLen=INT32_MIN;  // TODO - speed up by avoiding end_NA_string when there are none
    target->off = (int32_t)(fieldStart - ctx->anchor);
    target->len = fieldLen;
    return;
  }
  // else *ch==quote (we don't mind that quoted fields are a little slower e.g. no desire to save switch)
  //    the field is quoted and quotes are correctly escaped (quoteRule 0 and 1)
  // or the field is quoted but quotes are not escaped (quoteRule 2)
  // or the field is not quoted but the data contains a quote at the start (quoteRule 2 too)
  fieldStart++;  // step over opening quote
  switch(quoteRule) {
  case 0:  // quoted with embedded quotes doubled; the final unescaped " must be followed by sep|eol
    while (*++ch || ch<eof) {
      if (*ch==quote) {
        if (ch[1]==quote) { ch++; continue; }
        break;  // found undoubled closing quote
      }
    }
    break;
  case 1:  // quoted with embedded quotes escaped; the final unescaped " must be followed by sep|eol
    while (*++ch || ch<eof) {
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
      while ((*++ch || ch<eof) && *ch!='\n' && *ch!='\r') {
        if (*ch==quote && end_of_field(ch+1)) {ch2=ch; break;}  // (*1) regular ", ending; leave *ch on closing quote
        if (*ch==sep) {
          // first sep in this field
          // if there is a ", afterwards but before the next \n, use that; the field was quoted and it's still case (i) above.
          // Otherwise break here at this first sep as it's case (ii) above (the data contains a quote at the start and no sep)
          ch2 = ch;
          while ((*++ch2 || ch2<eof) && *ch2!='\n' && *ch2!='\r') {
            if (*ch2==quote && end_of_field(ch2+1)) {
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
    return;  // # nocov Internal error: undefined quote rule
  }
  target->len = (int32_t)(ch - fieldStart);
  target->off = (int32_t)(fieldStart - ctx->anchor);
  if (*ch==quote) {   // quote=='\0' (user set quote="") would have returned earlier above in the same branch as quoteRule 3
    ch++;
    skip_white(&ch);
    *(ctx->ch) = ch;
  } else {
    *(ctx->ch) = ch;
    if (ch==eof && quoteRule!=2) { target->off--; target->len++; }   // test 1324 where final field has open quote but not ending quote; include the open quote like quote rule 2
    while(target->len>0 && ((ch[-1]==' ' && stripWhite) || ch[-1]=='\0')) { target->len--; ch--; }  // test 1551.6; trailing whitespace in field [67,V37] == "\"\"A\"\" ST       "
  }
}

static void str_to_i32_core(const char **pch, int32_t *target)
{
  const char *ch = *pch;

  if (*ch=='0' && args.keepLeadingZeros && (uint_fast8_t)(ch[1]-'0')<10) return;
  bool neg = *ch=='-';
  ch += (neg || *ch=='+');
  const char *start = ch;  // to know if at least one digit is present
  // acc needs to be 64bit so that 5bn (still 10 digits but greater than 4bn) does not overflow. It could be
  //   signed but we use unsigned to be clear it will never be negative
  uint_fast64_t acc = 0;
  uint_fast8_t digit;
  // sep, \r, \n and *eof=='\0' all serve as valid terminators here by dint of being !=[0-9]
  // see init.c for checks of unsigned uint_fast8_t cast
  // optimizer should implement 10* as ((x<<2 + x)<<1) or (x<<3 + x<<1)

  // number significant figures = digits from the first non-zero onwards including trailing zeros
  while (*ch=='0') ch++;
  uint_fast32_t sf = 0;
  while ( (digit=(uint_fast8_t)(ch[sf]-'0'))<10 ) {
    acc = 10*acc + digit;
    sf++;
  }
  ch += sf;
  // INT32 range is NA==-2147483648(INT32_MIN) then symmetric [-2147483647,+2147483647] so we can just test INT32_MAX
  // The max (2147483647) happens to be 10 digits long, hence <=10.
  // Leading 0 (such as 001 and 099 but not 0, +0 or -0) will cause type bump to _full which has the
  // option to treat as integer or string with further cost.
  // if ( (acc && *start!='0' && acc<=INT32_MAX && (ch-start)<=10) ||
  //     (acc==0 && ch-start==1) ) {
  if ((sf || ch>start) && sf<=10 && acc<=INT32_MAX) {
    *target = neg ? -(int32_t)acc : (int32_t)acc;
    *pch = ch;
  } else {
    *target = NA_INT32;  // empty field ideally, contains NA and fall through to check if NA (in which case this write is important), or just plain invalid
  }
}

static void StrtoI32(FieldParseContext *ctx)
{
  str_to_i32_core(ctx->ch, (int32_t*) ctx->targets[sizeof(int32_t)]);
}


static void StrtoI64(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  int64_t *target = (int64_t*) ctx->targets[sizeof(int64_t)];
  if (*ch=='0' && args.keepLeadingZeros && (uint_fast8_t)(ch[1]-'0')<10) return;
  bool neg = *ch=='-';
  ch += (neg || *ch=='+');
  const char *start = ch;
  while (*ch=='0') ch++;
  uint_fast64_t acc = 0;  // important unsigned not signed here; we now need the full unsigned range
  uint_fast8_t digit;
  uint_fast32_t sf = 0;
  while ( (digit=(uint_fast8_t)(ch[sf]-'0'))<10 ) {
    acc = 10*acc + digit;
    sf++;
  }
  ch += sf;
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
cat("const long double pow10lookup[601] = {\n", file=f, append=FALSE)
for (i in (-300):(299)) cat("1.0E",i,"L,\n", sep="", file=f, append=TRUE)
cat("1.0E300L\n};\n", file=f, append=TRUE)
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
static void parse_double_regular_core(const char **pch, double *target)
{
  #define FLOAT_MAX_DIGITS 18
  const char *ch = *pch;

  if (*ch=='0' && args.keepLeadingZeros && (uint_fast8_t)(ch[1]-'0')<10) return;
  bool neg, Eneg;
  ch += (neg = *ch=='-') + (*ch=='+');

  const char* start = ch; // beginning of the number, without the initial sign
  uint_fast64_t acc = 0;  // mantissa NNN.MMM as a single 64-bit integer NNNMMM
  int_fast32_t e = 0;     // the number's exponent. The value being parsed is
                          // equal to acc * pow(10,e)
  uint_fast8_t digit;     // temporary variable, holds last scanned digit.

  while (*ch=='0') ch++;  // Skip leading zeros
  // Read the first, integer part of the floating number (but no more than
  // FLOAT_MAX_DIGITS digits).
  int_fast32_t sflimit = FLOAT_MAX_DIGITS;
  while ((digit=(uint_fast8_t)(*ch-'0'))<10 && sflimit) {
    acc = 10*acc + digit;
    sflimit--;
    ch++;
  }

  // If maximum allowed number of digits were read, but more are present -- then
  // we will read and discard those extra digits, but only if they are followed
  // by a decimal point (otherwise it's a just big integer, which should be
  // treated as a string instead of losing precision).
  if (sflimit==0 && (uint_fast8_t)(*ch-'0')<10) {
    while ((uint_fast8_t)(*ch-'0')<10) {
      ch++;
      e++;
    }
    if (*ch!=dec && *ch!='e' && *ch!='E') goto fail;
  }

  // Read the fractional part of the number, if it's present
  if (*ch==dec) {
    ch++;  // skip the decimal point
    // If the integer part was 0, then leading zeros in the fractional part do
    // not count against the number's precision. In practice it means that
    // numbers like 0.00000000000000000000000000000000004 can be read without
    // loss of precision as 4e-35  (test 1817)
    if (*ch=='0' && acc==0) {
      int_fast32_t k = 0;
      while (ch[k]=='0') k++;
      ch += k;
      e = -k;
    }

    // Now read the significant digits in the fractional part of the number
    int_fast32_t k = 0;
    while ((digit=(uint_fast8_t)(ch[k]-'0'))<10 && sflimit) {
      acc = 10*acc + digit;
      k++;
      sflimit--;
    }
    ch += k;
    e -= k;

    // If more digits are present, skip them
    if (sflimit==0) {
      while ((uint_fast8_t)(*ch-'0')<10) ch++;
    }
    // Check that at least 1 digit was present in either the integer or
    // fractional part ("+1" here accounts for the decimal point char).
    if (ch == start + 1) goto fail;
  }
  // If there is no fractional part, then check that the integer part actually
  // exists (otherwise it's not a valid number)...
  else {
    if (ch == start) goto fail;
  }

  // Finally parse the "exponent" part of the number (if present)
  if (*ch=='E' || *ch=='e') {
    if (ch==start) goto fail;  // something valid must be between [+|-] and E, character E alone is invalid.
    ch += 1/*E*/ + (Eneg = ch[1]=='-') + (ch[1]=='+');
    int_fast32_t E = 0;
    if ((digit=(uint_fast8_t)(*ch-'0'))<10) {
      E = digit;
      ch++;
      if ((digit=(uint_fast8_t)(*ch-'0'))<10) {
        E = E*10 + digit;
        ch++;
        if ((digit=(uint_fast8_t)(*ch-'0'))<10) {
          E = E*10 + digit;
          ch++;
        }
      }
    } else {
      // Not a single digit after "E"? Invalid number
      goto fail;
    }
    e += Eneg? -E : E;
  }
  if (e<-350 || e>350) goto fail;

  long double r = (long double)acc;
  if (e < -300 || e > 300) {
    // Handle extra precision by pre-multiplying the result by pow(10, extra),
    // and then remove extra from e.
    // This avoids having to store very small or very large constants that may
    // fail to be encoded by the compiler, even though the values can actually
    // be stored correctly.
    int_fast8_t extra = e < 0 ? e + 300 : e - 300;
    r *= pow10lookup[extra + 300];
    e -= extra;
  }
  e += 300; // lookup table is arranged from -300 (0) to +300 (600)

  r *= pow10lookup[e];
  *target = (double)(neg? -r : r);
  *pch = ch;
  return;

  fail:
    *target = NA_FLOAT64;
}

static void parse_double_regular(FieldParseContext *ctx) {
  parse_double_regular_core(ctx->ch, (double*) ctx->targets[sizeof(double)]);
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
  init();
  ch += (quoted = (*ch==quote && quote));
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
    if (quoted && *ch!=quote) {
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
  init();
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

/*
f = 'src/freadLookups.h'
cat('const uint8_t cumDaysCycleYears[401] = {\n', file=f, append=TRUE)
t = format(as.double(difftime(as.Date(sprintf('%04d-01-01', 1600:1999)), .Date(0), units='days')))
rows = paste0(apply(matrix(t, ncol = 4L, byrow = TRUE), 1L, paste, collapse = ', '), ',\n')
cat(rows, sep='', file=f, append=TRUE)
cat(146097, '// total days in 400 years\n};\n', sep = '', file=f, append=TRUE)
*/
static void parse_iso8601_date_core(const char **pch, int32_t *target)
{
  const char *ch = *pch;

  int32_t year, month, day;

  str_to_i32_core(&ch, &year);

  // .Date(.Machine$integer.max*c(-1, 1)):
  //  -5877641-06-24 -- 5881580-07-11
  //  rather than fiddle with dates within those terminal years (unlikely
  //  to be showing up in data sets any time soon), just truncate towards 0
  if (year == NA_INT32 || year < -5877640 || year > 5881579 || *ch != '-')
    goto fail;

  // Multiples of 4, excluding 3/4 of centuries
  bool isLeapYear = year % 4 == 0 && (year % 100 != 0 || year/100 % 4 == 0);
  ch++;

  str_to_i32_core(&ch, &month);
  if (month == NA_INT32 || month < 1 || month > 12 || *ch != '-')
    goto fail;
  ch++;

  str_to_i32_core(&ch, &day);
  if (day == NA_INT32 || day < 1 ||
      (day > (isLeapYear ? leapYearDays[month-1] : normYearDays[month-1])))
    goto fail;

  *target =
    (year/400 - 4)*cumDaysCycleYears[400] + // days to beginning of 400-year cycle
    cumDaysCycleYears[year % 400] + // days to beginning of year within 400-year cycle
    (isLeapYear ? cumDaysCycleMonthsLeap[month-1] : cumDaysCycleMonthsNorm[month-1]) + // days to beginning of month within year
    day-1; // day within month (subtract 1: 1970-01-01 -> 0)

  *pch = ch;
  return;

  fail:
    *target = NA_FLOAT64;
}

static void parse_iso8601_date(FieldParseContext *ctx) {
  parse_iso8601_date_core(ctx->ch, (int32_t*) ctx->targets[sizeof(int32_t)]);
}

static void parse_iso8601_timestamp(FieldParseContext *ctx)
{
  const char *ch = *(ctx->ch);
  double *target = (double*) ctx->targets[sizeof(double)];

  int32_t date, hour=0, minute=0, tz_hour=0, tz_minute=0;
  double second=0;

  parse_iso8601_date_core(&ch, &date);
  if (date == NA_INT32)
    goto fail;
  if (*ch != ' ' && *ch != 'T')
    goto date_only;
    // allows date-only field in a column with UTC-marked datetimes to be parsed as UTC too; test 2150.13
  ch++;

  str_to_i32_core(&ch, &hour);
  if (hour == NA_INT32 || hour < 0 || hour > 23 || *ch != ':')
    goto fail;
  ch++;

  str_to_i32_core(&ch, &minute);
  if (minute == NA_INT32 || minute < 0 || minute > 59 || *ch != ':')
    goto fail;
  ch++;

  parse_double_regular_core(&ch, &second);
  if (second == NA_FLOAT64 || second < 0 || second >= 60)
    goto fail;

  if (*ch == 'Z') {
    ch++; // "Zulu time"=UTC
  } else {
    if (*ch == ' ')
      ch++;
    if (*ch == '+' || *ch == '-') {
      const char *start = ch; // facilitates distinguishing +04, +0004, +0000, +00:00
      // three recognized formats: [+-]AA:BB, [+-]AABB, and [+-]AA
      str_to_i32_core(&ch, &tz_hour);
      if (tz_hour == NA_INT32)
        goto fail;
      if (ch - start == 5 && tz_hour != 0) { // +AABB
        if (abs(tz_hour) > 2400)
          goto fail;
        tz_minute = tz_hour % 100;
        tz_hour /= 100;
      } else if (ch - start == 3) {
        if (abs(tz_hour) > 24)
          goto fail;
        if (*ch == ':') {
          ch++;
          str_to_i32_core(&ch, &tz_minute);
          if (tz_minute == NA_INT32)
            goto fail;
        }
      }
    } else {
      if (!args.noTZasUTC)
        goto fail;
      // if neither Z nor UTC offset is present, then it's local time and that's not directly supported yet; see news for v1.13.0
      // but user can specify that the unmarked datetimes are UTC by passing tz="UTC" 
      // if local time is UTC (env variable TZ is "" or "UTC", not unset) then local time is UTC, and that's caught by fread at R level too
    }
  }

  date_only:

  //Rprintf("date=%d\thour=%d\tz_hour=%d\tminute=%d\ttz_minute=%d\tsecond=%.1f\n", date, hour, tz_hour, minute, tz_minute, second);
  // cast upfront needed to prevent silent overflow
  *target = 86400*(double)date + 3600*(hour - tz_hour) + 60*(minute - tz_minute) + second;

  *(ctx->ch) = ch;
  return;

  fail:
    *target = NA_FLOAT64;
}

/* Parse numbers 0 | 1 as boolean and ,, as NA (fwrite's default) */
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

/* Parse uppercase TRUE | FALSE | NA as boolean (as written by default by R's write.csv */
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
  } else if (ch[0]=='N' && ch[1]=='A') {
    // the default in R's write.csv
    *target = NA_BOOL8;
    *(ctx->ch) = ch + 2;
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


/* How to register a new parser
 *  (1) Write the parser
 *  (2) Add it to fun array here
 *  (3) Extend disabled_parsers, typeName, and typeSize here as appropriate
 *  (4) Extend colType typdef in fread.h as appropriate
 *  (5) Extend typeSxp, typeRName, typeEnum in freadR.c as appropriate
 */
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
  (reader_fun_t) &parse_iso8601_date,
  (reader_fun_t) &parse_iso8601_timestamp,
  (reader_fun_t) &Field
};

static int disabled_parsers[NUMTYPE] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

static int detect_types( const char **pch, int8_t type[], int ncol, bool *bumped) {
  // used in sampling column types and whether column names are present
  // test at most ncol fields. If there are fewer fields, the data read step later
  // will error (if fill==false) when the line number is known, so we don't need to handle that here.
  // leave resting on the eol; caller will advance over that eol
  const char *ch = *pch;
  double trash; // double so that this throw-away storage is aligned. char trash[8] would not be aligned.
  void *targets[9] = {NULL, &trash, NULL, NULL, &trash, NULL, NULL, NULL, &trash};
  FieldParseContext fctx = {
    .ch = &ch,
    .targets = targets,
    .anchor = NULL,
  };
  if (sep==' ') while (*ch==' ') ch++;  // multiple sep=' ' at the beginning of a line does not mean sep
  skip_white(&ch);
  if (eol(&ch)) return 0;  // empty line
  int field=0;
  while (field<ncol) {
    // DTPRINT(_("<<%s>>(%d)"), strlim(ch,20), quoteRule);
    skip_white(&ch);
    const char *fieldStart = ch;
    while (tmpType[field]<=CT_STRING) {
      fun[tmpType[field]](&fctx);
      if (end_of_field(ch)) break;
      skip_white(&ch);
      if (end_of_field(ch)) break;
      ch = end_NA_string(fieldStart);  // fieldStart here is correct (already after skip_white above); fun[]() may have part processed field so not ch
      skip_white(&ch);
      if (end_of_field(ch)) break;
      ch = fieldStart;
      if (tmpType[field]==CT_STRING) {
        // Invalid string field. Do not bump quoteRule here because it is more likely jump is dirty. Only the
        // thread at headPos which has full lineage to sof may bump the quoteRule.
        break; // caller will detect this line hasn't finished properly
      }
      if (*ch==quote && quote) {  // && quote to exclude quote='\0' (user passed quote="")
        ch++;
        fun[tmpType[field]](&fctx);
        if (*ch==quote) {
          ch++;
          skip_white(&ch);
          if (end_of_field(ch)) break;
        }
      }
      ch = fieldStart;
      while (++tmpType[field]<CT_STRING && disabled_parsers[tmpType[field]]) {};
      *bumped = true;
    }
    field++;
    if (sep==' ' && *ch==sep) {
      while (ch[1]==' ') ch++;
      if (ch[1]=='\n' || ch[1]=='\r' || (ch[1]=='\0' && ch+1<eof)) ch++;  // space at the end of line does not mean sep
    }
    if (*ch!=sep || field==ncol) break;  // field==ncol is needed for 1753.2 where line ends with an extra comma but shouldn't, so shouldn't be moved over
    ch++;
  }
  *pch = ch;
  return field; // the number of fields so caller knows if ncol were read
}


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

  if (freadCleanup()) {
    DTWARN(_("Previous fread() session was not cleaned up properly. Cleaned up ok at the beginning of this fread() call.\n")); // # nocov
  }

  if (verbose) DTPRINT(_("[01] Check arguments\n"));
  int nth = args.nth;
  {
    int maxth = omp_get_max_threads();
    if (nth > maxth) nth = maxth;
    if (nth <= 0) nth += maxth;
    if (nth <= 0) nth = 1;
    if (verbose) DTPRINT(_("  Using %d threads (omp_get_max_threads()=%d, nth=%d)\n"), nth, maxth, args.nth);
  }

  uint64_t ui64 = NA_FLOAT64_I64;
  memcpy(&NA_FLOAT64, &ui64, 8);

  int64_t nrowLimit = args.nrowLimit;
  NAstrings = args.NAstrings;
  if (NAstrings==NULL) STOP(_("Internal error: NAstrings is itself NULL. When empty it should be pointer to NULL.")); // # nocov
  any_number_like_NAstrings = false;
  blank_is_a_NAstring = false;
  // if we know there are no nastrings which are numbers (like -999999) then in the number
  // field processors we can save an expensive step in checking the NAstrings. If the field parses as a number,
  // we then when any_number_like_nastrings==FALSE we know it can't be NA.
  const char * const* nastr = NAstrings;
  while (*nastr) {
    if (**nastr == '\0') {
      blank_is_a_NAstring = true;
      // if blank is the only one, as is the default, clear NAstrings so that doesn't have to be checked
      if (nastr==NAstrings && nastr+1==NULL) NAstrings=NULL;
      nastr++;
      continue;
    }
    const char *ch = *nastr;
    size_t nchar = strlen(ch);
    if (isspace(ch[0]) || isspace(ch[nchar-1]))
      STOP(_("freadMain: NAstring <<%s>> has whitespace at the beginning or end"), ch);
    if (strcmp(ch,"T")==0    || strcmp(ch,"F")==0 ||
        strcmp(ch,"TRUE")==0 || strcmp(ch,"FALSE")==0 ||
        strcmp(ch,"True")==0 || strcmp(ch,"False")==0 ||
        strcmp(ch,"1")==0    || strcmp(ch,"0")==0)
      STOP(_("freadMain: NAstring <<%s>> is recognized as type boolean, this is not permitted."), ch);
    char *end;
    errno = 0;
    (void)strtod(ch, &end);  // careful not to let "" get to here (see continue above) as strtod considers "" numeric
    if (errno==0 && (size_t)(end - ch) == nchar) any_number_like_NAstrings = true;
    nastr++;
  }
  disabled_parsers[CT_BOOL8_N] = !args.logical01;
  disabled_parsers[CT_ISO8601_DATE] = disabled_parsers[CT_ISO8601_TIME] = args.oldNoDateTime; // temporary new option in v1.13.0; see NEWS
  if (verbose) {
    if (*NAstrings == NULL) {
      DTPRINT(_("  No NAstrings provided.\n"));
    } else {
      DTPRINT(_("  NAstrings = ["));
      const char * const* s = NAstrings;
      while (*s++) DTPRINT(*s? "<<%s>>, " : "<<%s>>", s[-1]);
      DTPRINT(_("]\n"));
      if (any_number_like_NAstrings)
        DTPRINT(_("  One or more of the NAstrings looks like a number.\n"));
      else
        DTPRINT(_("  None of the NAstrings look like numbers.\n"));
    }
    if (args.skipNrow >= 0) DTPRINT(_("  skip num lines = %"PRId64"\n"), (int64_t)args.skipNrow);
    if (args.skipString) DTPRINT(_("  skip to string = <<%s>>\n"), args.skipString);
    DTPRINT(_("  show progress = %d\n"), args.showProgress);
    DTPRINT(_("  0/1 column will be read as %s\n"), args.logical01? "boolean" : "integer");
  }

  stripWhite = args.stripWhite;
  skipEmptyLines = args.skipEmptyLines;
  fill = args.fill;
  dec = args.dec;
  quote = args.quote;
  if (args.sep == quote && quote!='\0') STOP(_("sep == quote ('%c') is not allowed"), quote);
  if (dec=='\0') STOP(_("dec='' not allowed. Should be '.' or ','"));
  if (args.sep == dec) STOP(_("sep == dec ('%c') is not allowed"), dec);
  if (quote == dec) STOP(_("quote == dec ('%c') is not allowed"), dec);
  // since quote=='\0' when user passed quote="", the logic in this file uses '*ch==quote && quote' otherwise
  //   the ending \0 at eof could be treated as a quote (test xxx)

  // File parsing context: pointer to the start of file, and to the end of
  // the file. The `sof` pointer may be shifted in order to skip over
  // "irrelevant" parts: the BOM mark, the banner, the headers, the skipped
  // lines, etc. Similarly, `eof` may be adjusted to take out the footer of
  // the file.
  const char *ch = NULL;

  //*********************************************************************************************
  // [2] Open and memory-map the input file, setting up the parsing context
  //     (sof, eof, ch).
  //*********************************************************************************************
  double tMap;  // moment when memory-map step has finished
  {
  if (verbose) DTPRINT(_("[02] Opening the file\n"));
  mmp = NULL;
  if (args.input) {
    if (verbose) DTPRINT(_("  `input` argument is provided rather than a file name, interpreting as raw text to read\n"));
    sof = args.input;
    fileSize = strlen(sof);
    eof = sof+fileSize;
    if (*eof!='\0') STOP(_("Internal error: last byte of character input isn't \\0")); // # nocov
  }
  else if (args.filename) {
    if (verbose) DTPRINT(_("  Opening file %s\n"), args.filename);
    const char* fnam = args.filename;
    #ifndef WIN32
      int fd = open(fnam, O_RDONLY);
      if (fd==-1) STOP(_("file not found: %s"),fnam);
      struct stat stat_buf;
      if (fstat(fd, &stat_buf) == -1) {
        close(fd);                                                     // # nocov
        STOP(_("Opened file ok but couldn't obtain its size: %s"), fnam); // # nocov
      }
      fileSize = (size_t) stat_buf.st_size;
      if (fileSize == 0) {close(fd); STOP(_("File is empty: %s"), fnam);}
      if (verbose) DTPRINT(_("  File opened, size = %s.\n"), filesize_to_str(fileSize));

      // No MAP_POPULATE for faster nrows=10 and to make possible earlier progress bar in row count stage
      // Mac doesn't appear to support MAP_POPULATE anyway (failed on CRAN when I tried).
      // TO DO?: MAP_HUGETLB for Linux but seems to need admin to setup first. My Hugepagesize is 2MB (>>2KB, so promising)
      //         https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
      mmp = mmap(NULL, fileSize, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);  // COW for last page lastEOLreplaced
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
          if (GetLastError()==ERROR_FILE_NOT_FOUND) STOP(_("File not found: %s"),fnam);
          if (attempts<4) Sleep(250);  // 250ms
        }
        attempts++;
        // Looped retry to avoid ephemeral locks by system utilities as recommended here : http://support.microsoft.com/kb/316609
      }
      if (hFile==INVALID_HANDLE_VALUE) STOP(_("Unable to open file after %d attempts (error %d): %s"), attempts, GetLastError(), fnam);
      LARGE_INTEGER liFileSize;
      if (GetFileSizeEx(hFile,&liFileSize)==0) { CloseHandle(hFile); STOP(_("GetFileSizeEx failed (returned 0) on file: %s"), fnam); }
      fileSize = (size_t)liFileSize.QuadPart;
      if (fileSize<=0) { CloseHandle(hFile); STOP(_("File is empty: %s"), fnam); }
      if (verbose) DTPRINT(_("  File opened, size = %s.\n"), filesize_to_str(fileSize));
      HANDLE hMap=CreateFileMapping(hFile, NULL, PAGE_WRITECOPY, 0, 0, NULL);
      if (hMap==NULL) { CloseHandle(hFile); STOP(_("This is Windows, CreateFileMapping returned error %d for file %s"), GetLastError(), fnam); }
      mmp = MapViewOfFile(hMap,FILE_MAP_COPY,0,0,fileSize);  // fileSize must be <= hilo passed to CreateFileMapping above.
      CloseHandle(hMap);  // we don't need to keep the file open; the MapView keeps an internal reference;
      CloseHandle(hFile); //   see https://msdn.microsoft.com/en-us/library/windows/desktop/aa366537(v=vs.85).aspx
      if (mmp == NULL) {
    #endif
      int nbit = 8*sizeof(char *); // #nocov
      STOP(_("Opened %s file ok but could not memory map it. This is a %dbit process. %s."), filesize_to_str(fileSize), nbit, // # nocov
           nbit<=32 ? _("Please upgrade to 64bit") : _("There is probably not enough contiguous virtual memory available")); // # nocov
    }
    sof = (const char*) mmp;
    if (verbose) DTPRINT(_("  Memory mapped ok\n"));
  } else {
    STOP(_("Internal error: Neither `input` nor `filename` are given, nothing to read.")); // # nocov
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
  if (verbose) DTPRINT(_("[03] Detect and skip BOM\n"));
  if (fileSize >= 3 && memcmp(sof, "\xEF\xBB\xBF", 3) == 0) {
    sof += 3;
    // ienc = CE_UTF8;
    if (verbose) DTPRINT(_("  UTF-8 byte order mark EF BB BF found at the start of the file and skipped.\n"));
  }
  else if (fileSize >= 4 && memcmp(sof, "\x84\x31\x95\x33", 4) == 0) {
    sof += 4;
    // ienc = CE_GB18030;
    DTWARN(_("GB-18030 encoding detected, however fread() is unable to decode it. Some character fields may be garbled.\n"));
  }
  else if (fileSize >= 2 && sof[0] + sof[1] == '\xFE' + '\xFF') {  // either 0xFE 0xFF or 0xFF 0xFE
    STOP(_("File is encoded in UTF-16, this encoding is not supported by fread(). Please recode the file to UTF-8."));
  }
  if (eof>sof && (eof[-1]=='\x1A' || eof[-1]=='\0')) {
    char c = eof[-1];
    while (eof>sof && eof[-1]==c) eof--;
    if (verbose) DTPRINT(_("  Last byte(s) of input found to be %s and removed.\n"),
                         c ? "0x1A (Ctrl+Z)" : "0x00 (NUL)");
  }
  if (eof<=sof) STOP(_("Input is empty or only contains BOM or terminal control characters"));
  }


  //*********************************************************************************************
  // [4] Terminate mmap with \0
  //*********************************************************************************************
  if (verbose) DTPRINT(_("[04] Arrange mmap to be \\0 terminated\n"));

  // First, set 'eol_one_r' for use by eol() to know if \r-only line ending is allowed, #2371
  ch = sof;
  while (ch<eof && *ch!='\n') ch++;
  eol_one_r = (ch==eof);
  if (verbose) DTPRINT(eol_one_r ?
    _("  No \\n exists in the file at all, so single \\r (if any) will be taken as one line ending. This is unusual but will happen normally when there is no \\r either; e.g. a single line missing its end of line.\n") :
    _("  \\n has been found in the input and different lines can end with different line endings (e.g. mixed \\n and \\r\\n in one file). This is common and ideal.\n"));

  bool lastEOLreplaced = false;
  if (args.filename) {
    // eof is currently resting after the last byte of the file (so don't even read it there; bus error if fileSize%4096==0)
    ch = eof-1;
    if (eol_one_r) {
      while (ch>=sof && *ch!='\r') ch--;
    } else {
      while (ch>=sof && *ch!='\n') ch--;
      while (ch>sof && ch[-1]=='\r') ch--;  // the first of any preceeding \r to avoid a dangling \r
    }
    if (ch>=sof) {
      const char *lastNewLine = ch;  // the start of the final newline sequence.
      while (++ch<eof && isspace(*ch)) {};
      if (ch==eof) {
        // yes, just whitespace after last newline. Use last newline to put final \0
        eof = lastNewLine;
        lastEOLreplaced = true;  // only needed for blank lines at the end of single-column files
      }
    }
    if (!lastEOLreplaced) {
      // very unusual branch because properly formed csv will have final eol
      if (fileSize%4096!=0) {
        if (verbose) DTPRINT(_("  File ends abruptly with '%c'. Final end-of-line is missing. Using cow page to write 0 to the last byte.\n"), eof[-1]);
        // We could do this routinely (i.e. when there is a final newline too) but we desire to run all tests through the harder
        // branch above that replaces the final newline with \0 to test that logic (e.g. test 893 which causes a type bump in the last
        // field) since we rely on that logic to avoid the copy below when fileSize$4096==0 but there is a final eol ok.
        // TODO: portable way to discover relevant page size. 4096 is lowest common denominator, though, and should suffice.
      } else {
        const char *msg = _("This file is very unusual: it ends abruptly without a final newline, and also its size is a multiple of 4096 bytes. Please properly end the last row with a newline using for example 'echo >> file' to avoid this ");
        if (verbose) DTPRINT(_("  File ends abruptly with '%c'. Copying file in RAM. %s copy.\n"), eof[-1], msg);
        // In future, we may discover a way to mmap fileSize+1 on all OS when fileSize%4096==0, reliably. If and when, this clause can be updated with no code impact elsewhere.
        copyFile(fileSize, msg, verbose);
      }
    }
    *_const_cast(eof) = '\0';  // cow page
  }
  // else char* input already guaranteed to end with \0. We do not modify direct char* input at all, ever.
  // We have now ensured the input ends on eof and that *eof=='\0' too. Normally, lastEOLreplaced will be true.
  // We have made most files which end properly with final end-of-line now abruptly end with a \0 (in the cow page).
  // This may seem counterintuitive but now we have consistency within the constraints of no mmap of fileSize+1.
  // In field processors we don't need to test for ch<eof at all now, since \0 is terminal just as well as \r or \n.
  // When we need to, we now have two options: i) if (*ch && ...) (i.e. !='\0') which saves a compare to eof on every byte, or ii) ch<eof as usual
  // If UTF strings contain \0 we can branch in that rare case to test if ch==eof too if necessary. We have that option.
  // If a field does not end with sep or eol, it's only in that rare case do we then need to test if it is \0 or not.
  // fileSize is left unchanged at the actual file size. Use eof and sof from here on. Usually, eof-sof will be a few bytes less than fileSize.

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
  const char *pos = sof;   // Location where the actual data in the file begins
  int row1line = 1;        // The line number where the data starts. Normally row 1 is column names and row1line ends up == 2.
  {
  ch = pos;
  if (verbose) DTPRINT(_("[05] Skipping initial rows if needed\n"));

  // line is for error and warning messages so considers raw \n whether inside quoted fields or not, just
  // like wc -l, head -n and tail -n
  if (args.skipString) {
    ch = strstr(sof, args.skipString);  // as there is now a \0 at the end, this is safely bounded
    if (!ch) STOP(_("skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)"),
                  args.skipString);
    while (ch>sof && ch[-1]!='\n') ch--;  // move to beginning of line
    pos = ch;
    ch = sof;
    while (ch<pos) row1line+=(*ch++=='\n');
    if (verbose) DTPRINT(_("Found skip='%s' on line %"PRIu64". Taking this to be header row or first row of data.\n"),
                         args.skipString, (uint64_t)row1line);
    ch = pos;
  }
  else if (args.skipNrow >= 0) {
    // Skip the first `skipNrow` lines of input, including 0 to force the first line to be the start
    while (ch < eof && row1line <= args.skipNrow) {
      char c = *ch++;
      if (c == '\n' || c == '\r') {
        ch += (ch < eof && c + ch[0] == '\n' + '\r');
        row1line++;
      }
    }
    if (ch > sof && verbose) DTPRINT(_("  Skipped to line %"PRIu64" in the file"), (uint64_t)row1line);
    if (ch>=eof) STOP(_("skip=%"PRIu64" but the input only has %"PRIu64" line%s"), (uint64_t)args.skipNrow, (uint64_t)row1line, row1line>1?"s":"");
    pos = ch;
  }

  // skip blank input at the start
  const char *lineStart = ch;
  while (ch<eof && (isspace(*ch) || *ch=='\0')) {   // isspace matches ' ', \t, \n and \r;  \0 before eof should be skipped too
    if (*ch=='\n') { ch++; lineStart=ch; row1line++; } else ch++;
  }
  if (ch>=eof) STOP(_("Input is either empty, fully whitespace, or skip has been set after the last non-whitespace."));
  if (verbose) {
    if (lineStart>ch) DTPRINT(_("  Moved forward to first non-blank line (%d)\n"), row1line);
    DTPRINT(_("  Positioned on line %d starting: <<%s>>\n"), row1line, strlim(lineStart, 30));
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
  int ncol;  // Detected number of columns in the file
  const char *firstJumpEnd=NULL; // remember where the winning jumpline from jump 0 ends, to know its size excluding header
  const char *prevStart = NULL;  // the start of the non-empty line before the first not-ignored row (for warning message later, or taking as column names)
  int jumpLines = (int)umin(100,nrowLimit);   // how many lines from each jump point to use. If nrowLimit is supplied, nJumps is later set to 1 as well.
  {
  if (verbose) DTPRINT(_("[06] Detect separator, quoting rule, and ncolumns\n"));

  if (args.sep == '\n') {  // '\n' because '\0' is taken already to mean 'auto'
    // unusual
    if (verbose) DTPRINT(_("  sep='\\n' passed in meaning read lines as single character column\n"));
    sep = 127;     // ASCII DEL: a character different from \r, \n and \0 that isn't in the data
    whiteChar = 0;
    quoteRule = 3; // Ignore quoting
    ncol = 1;
    int thisLine=0;
    while (ch<eof && thisLine++<jumpLines) {
      countfields(&ch);
      if (eol(&ch)) ch++;
    }
    firstJumpEnd = ch;  // size of first 100 lines in bytes is used later for nrow estimate
    fill = true;        // so that blank lines are read as empty
    ch = pos;
  } else {
    int nseps;
    char seps[]=",|;\t ";  // default seps in order of preference. See ?fread.
                           // seps[] not *seps for writeability (http://stackoverflow.com/a/164258/403310)
    char topSep=127;       // which sep 'wins' top place (see top* below). By default 127 (ascii del) means no sep i.e. single-column input (1 field)
    if (args.sep == '\0') {
      if (verbose) DTPRINT(_("  Detecting sep automatically ...\n"));
      nseps = (int) strlen(seps);
    } else {
      seps[0] = args.sep;
      seps[1] = '\0';
      nseps = 1;
      //topSep = args.sep;
      if (verbose) DTPRINT(_("  Using supplied sep '%s'\n"), args.sep=='\t' ? "\\t" : seps);
    }
    int topNumLines=0;        // the most number of lines with the same number of fields, so far
    int topNumFields=1;       // how many fields that was, to resolve ties
    int topQuoteRule=-1;      // which quote rule that was
    int topSkip=0;            // how many rows to auto-skip
    const char *topStart=NULL;

    for (quoteRule=quote?0:3; quoteRule<4; quoteRule++) {
      // quote rule in order of preference.
      // when top is tied the first wins, so do all seps for the first quoteRule, then all seps for the second quoteRule, etc
      for (int s=0; s<nseps; s++) {
        sep = seps[s];
        whiteChar = (sep==' ' ? '\t' : (sep=='\t' ? ' ' : 0));  // 0 means both ' ' and '\t' to be skipped
        ch = pos;
        // if (verbose) DTPRINT(_("  Trying sep='%c' with quoteRule %d ...\n"), sep, quoteRule);

        if (fill) {
          if (quoteRule>1 && quote) continue;  // turn off self-healing quote rule when filling
          int firstRowNcol = countfields(&ch);
          int thisncol=0, maxncol=firstRowNcol, thisRow=0;
          while (ch<eof && ++thisRow<jumpLines) {   // TODO: rename 'jumpLines' to 'jumpRows'
            thisncol = countfields(&ch);
            if (thisncol<0) break;  // invalid line; e.g. improper quote
            if (thisncol>maxncol) maxncol=thisncol;
          }
          if (thisncol<0) continue; // if invalid line anywhere in first jumpRows, skip this sep and quoteRule
          if (firstRowNcol>1 && maxncol>topNumFields) {
            topNumFields = maxncol;
            topSep = sep;
            topQuoteRule = quoteRule;
            firstJumpEnd = ch;  // to know how many bytes jump 0 is, for nrow estimate later (a less-good estimate when fill=true since line lengths vary more)
            if (verbose) {
              DTPRINT((unsigned)sep<32 ? "  sep=%#02x" : "  sep='%c'", sep);
              DTPRINT(_("  with %d fields using quote rule %d\n"), topNumFields, quoteRule);
            }
          }
        } else {
          // standard regular same-ncol-per-row file, possibly with a meta header to be skipped
          const char *prevLineStart=ch, *lineStart=ch;
          int lastncol = countfields(&ch);
          if (lastncol<0) continue;  //   invalid file with this sep and quote rule, skip
          ASSERT(lastncol>0, "first non-empty row should always be at least one field; %c %d", sep, quoteRule); // # nocov
          const char *thisBlockStart=lineStart;
          const char *thisBlockPrevStart = NULL;
          int thisBlockLines=1, thisRow=0;
          while (ch<eof && ++thisRow<jumpLines) {
            prevLineStart=lineStart; lineStart=ch;
            int thisncol = countfields(&ch);
            if (thisncol==0 && skipEmptyLines) while ( ch<eof && thisncol==0 ) { prevLineStart=NULL; lineStart=ch; thisRow++; thisncol=countfields(&ch); }
            if (thisncol<0) break;  // invalid file with this sep and quote rule
            if (thisncol==lastncol) {
              // count the number of contiguous lines with the same number of fields
              thisBlockLines++;
              continue;
            }
            if (lastncol>1 && thisBlockLines>1) break;  // found and finished the first 2x2 (or bigger) block
            while (ch<eof && thisncol==0) {
              prevLineStart=NULL; lineStart=ch; thisRow++;
              thisncol = countfields(&ch);
            }
            if (thisncol>0) {
              lastncol = thisncol;
              thisBlockLines = 1;
              thisBlockPrevStart = prevLineStart;  // remember previous line start in case it has column names to be filled
              thisBlockStart = lineStart;
            }
          }
          if ((thisBlockLines>topNumLines && lastncol>1) ||  // more lines wins even with fewer fields, so long as number of fields >= 2
              (thisBlockLines==topNumLines &&
               lastncol>topNumFields &&                      // when number of lines is tied, choose the sep which separates it into more columns
               (quoteRule<2 || quoteRule<=topQuoteRule) &&   // for test 1834 where every line contains a correctly quoted field contain sep
               (topNumFields<=1 || sep!=' '))) {
            topNumLines = thisBlockLines;
            topNumFields = lastncol;
            topSep = sep;
            topQuoteRule = quoteRule;
            firstJumpEnd = ch;
            topStart = thisBlockStart;
            prevStart = thisBlockPrevStart; // only used when line prior to contiguous block has a wrong number of column names to be filled
            topSkip = thisRow-thisBlockLines;
            if (topSkip<0) topSkip=0;       // inelegant but will do for now to pass single row input such as test 890
            if (verbose) {
              DTPRINT((unsigned)sep<32 ? "  sep=%#02x" : "  sep='%c'", sep);
              DTPRINT(_("  with %d lines of %d fields using quote rule %d\n"), topNumLines, topNumFields, topQuoteRule);
            }
          }
        }
      }
    }
    if (!firstJumpEnd) {
      if (verbose) DTPRINT(_("  No sep and quote rule found a block of 2x2 or greater. Single column input.\n"));
      topNumFields = 1;
      ASSERT(topSep==127, "Single column input has topSep=%d", topSep);
      sep = topSep;
      // no self healing quote rules, as we don't have >1 field to disambiguate
      // choose quote rule 0 or 1 based on for which 100 rows gets furthest into file
      for (quoteRule=0; quoteRule<=1; quoteRule++) {
        int thisRow=0, thisncol=0;
        ch = pos;
        while (ch<eof && ++thisRow<jumpLines && (thisncol=countfields(&ch))>=0) {};
        if (thisncol<0) continue;  // invalid file; e.g. unescaped quote inside quoted field
        if (!firstJumpEnd || ch>firstJumpEnd) {
          firstJumpEnd = ch;
          topQuoteRule = quoteRule;
        }
      }
      if (!firstJumpEnd) STOP(_("Single column input contains invalid quotes. Self healing only effective when ncol>1"));
    }

    quoteRule = topQuoteRule;
    if (quoteRule>1 && quote) {
      DTWARN(_("Found and resolved improper quoting in first %d rows. If the fields are not quoted (e.g. field separator does not appear within any field), try quote=\"\" to avoid this warning."), jumpLines);
      // TODO: include line number and text in warning. Could loop again with the standard quote rule to find the line that fails.
    }
    sep = topSep;
    whiteChar = (sep==' ' ? '\t' : (sep=='\t' ? ' ' : 0));
    ncol = topNumFields;
    if (fill || sep==127) {
      // leave pos on the first populated line; that is start of data
      ch = pos;
    } else {
      // TODO: warning here if topSkip>0 that header is being auto-removed.
      ch = pos = topStart;
      row1line += topSkip;
    }
  }

  if (ncol<1 || row1line<1) STOP(_("Internal error: ncol==%d line==%d after detecting sep, ncol and first line"), ncol, row1line); // # nocov
  int tt = countfields(&ch);
  ch = pos; // move back to start of line since countfields() moved to next
  if (!fill && tt!=ncol) STOP(_("Internal error: first line has field count %d but expecting %d"), tt, ncol); // # nocov
  if (verbose) {
    DTPRINT(_("  Detected %d columns on line %d. This line is either column names or first data row. Line starts as: <<%s>>\n"),
            tt, row1line, strlim(pos, 30));
    DTPRINT(_("  Quote rule picked = %d\n"), quoteRule);
    DTPRINT(_("  fill=%s and the most number of columns found is %d\n"), fill?"true":"false", ncol);
  }

  if (ncol==1 && lastEOLreplaced && (eof[-1]=='\n' || eof[-1]=='\r')) {
    // Multiple newlines at the end are significant in the case of 1-column files only (multiple NA at the end)
    if (fileSize%4096==0) {
      const char *msg = _("This file is very unusual: it's one single column, ends with 2 or more end-of-line (representing several NA at the end), and is a multiple of 4096, too.");
      if (verbose) DTPRINT(_("  Copying file in RAM. %s\n"), msg);
      ASSERT(mmp_copy==NULL, "mmp has already been copied due to abrupt non-eol ending, so it does not end with 2 or more eol.", 1/*dummy arg for macro*/); // #nocov
      copyFile(fileSize, msg, verbose);
      pos = sof + (pos-(const char *)mmp);
      firstJumpEnd = sof + (firstJumpEnd-(const char *)mmp);
    } else {
      if (verbose) DTPRINT(_("  1-column file ends with 2 or more end-of-line. Restoring last eol using extra byte in cow page.\n"));
      eof++;
    }
    *_const_cast(eof-1) = eol_one_r ? '\r' : '\n';
    *_const_cast(eof) = '\0';
  }
  }

  //*********************************************************************************************
  // [7] Detect column types, good nrow estimate and whether first row is column names
  //*********************************************************************************************
  int nJumps;             // How many jumps to use when pre-scanning the file
  int64_t sampleLines;     // How many lines were sampled during the initial pre-scan
  bool autoFirstColName = false; // true when there's one less column name and then it's assumed that the first column is row names or index
  int64_t estnrow=1;
  int64_t allocnrow=0;     // Number of rows in the allocated DataTable
  double meanLineLen=0.0; // Average length (in bytes) of a single line in the input file
  size_t bytesRead=0;     // Bytes in the data section (i.e. excluding column names, header and footer, if any)
  {
  if (verbose) DTPRINT(_("[07] Detect column types, good nrow estimate and whether first row is column names\n"));
  if (verbose && args.header!=NA_BOOL8) DTPRINT(_("  'header' changed by user from 'auto' to %s\n"), args.header?"true":"false");

  type =    (int8_t *)malloc((size_t)ncol * sizeof(int8_t));
  tmpType = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));  // used i) in sampling to not stop on errors when bad jump point and ii) when accepting user overrides
  if (!type || !tmpType) STOP(_("Failed to allocate 2 x %d bytes for type and tmpType: %s"), ncol, strerror(errno));

  int8_t type0 = 1;
  while (disabled_parsers[type0]) type0++;
  for (int j=0; j<ncol; j++) {
    // initialize with the lowest available type
    tmpType[j] = type[j] = type0;
  }

  size_t jump0size=(size_t)(firstJumpEnd-pos);  // the size in bytes of the first 100 lines from the start (jump point 0)
  // how many places in the file to jump to and test types there (the very end is added as 11th or 101th)
  // not too many though so as not to slow down wide files; e.g. 10,000 columns.  But for such large files (50GB) it is
  // worth spending a few extra seconds sampling 10,000 rows to decrease a chance of costly reread even further.
  nJumps = 1;
  size_t sz = (size_t)(eof - pos);
  if (jump0size>0) {
    if (jump0size*100*2 < sz) nJumps=100;  // 100 jumps * 100 lines = 10,000 line sample
    else if (jump0size*10*2 < sz) nJumps=10;
    // *2 to get a good spacing. We don't want overlaps resulting in double counting.
  }
  if (verbose) {
    DTPRINT(_("  Number of sampling jump points = %d because "), nJumps);
    if (nrowLimit<INT64_MAX) DTPRINT(_("nrow limit (%"PRIu64") supplied\n"), (uint64_t)nrowLimit);
    else if (jump0size==0) DTPRINT(_("jump0size==0\n"));
    else DTPRINT(_("(%"PRIu64" bytes from row 1 to eof) / (2 * %"PRIu64" jump0size) == %"PRIu64"\n"),
                 (uint64_t)sz, (uint64_t)jump0size, (uint64_t)(sz/(2*jump0size)));
  }
  nJumps++; // the extra sample at the very end (up to eof) is sampled and format checked but not jumped to when reading
  if (nrowLimit<INT64_MAX) nJumps=1; // when nrowLimit supplied by user, no jumps (not even at the end) and single threaded

  sampleLines = 0;
  double sumLen=0.0, sumLenSq=0.0;
  int minLen=INT32_MAX, maxLen=-1;   // int_max so the first if(thisLen<minLen) is always true; similarly for max
  const char *lastRowEnd = pos;
  const char *firstRowStart = pos;
  for (int jump=0; jump<nJumps; jump++) {
    if (jump==0) {
      ch = pos;
      if (args.header!=false) {
        countfields(&ch); // skip first row for type guessing as it's probably column names
        row1line++;
      }
      firstRowStart = ch;
    } else {
      ch = (jump == nJumps-1) ? eof - (size_t)(0.5*jump0size) :  // to almost-surely sample the last line
                                pos + (size_t)jump*((size_t)(eof-pos)/(size_t)(nJumps-1));
      ch = nextGoodLine(ch, ncol);
    }
    if (ch<lastRowEnd) ch=lastRowEnd;  // Overlap when apx 1,200 lines (just over 11*100) with short lines at the beginning and longer lines near the end, #2157
    if (ch>=eof) break;                // The 9th jump could reach the end in the same situation and that's ok. As long as the end is sampled is what we want.
    bool bumped = false;  // did this jump find any different types; to reduce verbose output to relevant lines
    int jumpLine = 0;    // line from this jump point start

    while(ch<eof && jumpLine++<jumpLines) {
      const char *lineStart = ch;
      int thisNcol = detect_types(&ch, tmpType, ncol, &bumped);
      if (thisNcol==0 && skipEmptyLines) {
        if (eol(&ch)) ch++;
        continue;
      }
      if ( (thisNcol<ncol && ncol>1 && !fill) ||
           (!eol(&ch) && ch!=eof) ) {
        if (verbose) DTPRINT(_("  A line with too-%s fields (%d/%d) was found on line %d of sample jump %d. %s\n"),
                             thisNcol<ncol ? _("few") : _("many"), thisNcol, ncol, jumpLine, jump, jump>0 ? _("Most likely this jump landed awkwardly so type bumps here will be skipped.") : "");
        bumped = false;
        if (jump==0) lastRowEnd=eof;  // to prevent the end from being tested; e.g. a short file with blank line within first 100 like test 976
        break;
      }
      ch += (*ch=='\n' || *ch=='\r');   // eol() was called in the if() above which moved to the last of the eol sequence (if any)
      lastRowEnd = ch;
      int thisLineLen = (int)(ch-lineStart);  // ch is now on start of next line so this includes line ending already
      sampleLines++;
      sumLen += thisLineLen;
      sumLenSq += thisLineLen*thisLineLen;
      if (thisLineLen<minLen) minLen=thisLineLen;
      if (thisLineLen>maxLen) maxLen=thisLineLen;
      if (jump==0 && bumped) {
        // apply bumps after each line in the first jump from the start in case invalid line stopped early on is in the first 100 lines.
        // otherwise later jumps must complete fully before their bumps are appplied. Invalid lines in those are more likely to be due to bad jump start.
        memcpy(type, tmpType, (size_t)ncol);
        bumped = false;  // detect_types() only updates &bumped when it's true. So reset to false here.
      }
    }
    if (bumped) {
      // when jump>0, apply the bumps (if any) at the end of the successfully completed jump sample
      ASSERT(jump>0, "jump(%d)>0", jump);
      memcpy(type, tmpType, (size_t)ncol);
    }
    if (verbose && (bumped || jump==0 || jump==nJumps-1)) {
      DTPRINT(_("  Type codes (jump %03d)    : %s  Quote rule %d\n"), jump, typesAsString(ncol), quoteRule);
    }
  }

  ch = pos;
  if (args.header==NA_BOOL8) {
    for (int j=0; j<ncol; j++) tmpType[j]=type0;   // reuse tmpType
    bool bumped=false;
    detect_types(&ch, tmpType, ncol, &bumped);
    if (sampleLines>0) for (int j=0; j<ncol; j++) {
      if (tmpType[j]==CT_STRING && type[j]<CT_STRING) {
        // includes an all-blank column with a string at the top; e.g. test 1870.1 and 1870.2
        args.header=true;
        if (verbose) DTPRINT(_("  'header' determined to be true due to column %d containing a string on row 1 and a lower type (%s) in the rest of the %d sample rows\n"),
                             j+1, typeName[type[j]], sampleLines);
        break;
      }
    }
  }

  if (args.header==NA_BOOL8 && prevStart!=NULL) {
    // The first data row matches types in the row after that, and user didn't override default auto detection.
    // Maybe previous line (if there is one, prevStart!=NULL) contains column names but there are too few (which is why it didn't become the first data row).
    ch = prevStart;
    int tt = countfields(&ch);
    if (tt==ncol) STOP(_("Internal error: row before first data row has the same number of fields but we're not using it.")); // # nocov
    if (ch!=pos)  STOP(_("Internal error: ch!=pos after counting fields in the line before the first data row.")); // # nocov
    if (verbose) DTPRINT(_("Types in 1st data row match types in 2nd data row but previous row has %d fields. Taking previous row as column names."), tt);
    if (tt<ncol) {
      autoFirstColName = (tt==ncol-1);
      DTWARN(_("Detected %d column names but the data has %d columns (i.e. invalid file). Added %d extra default column name%s\n"), tt, ncol, ncol-tt,
             autoFirstColName ? _(" for the first column which is guessed to be row names or an index. Use setnames() afterwards if this guess is not correct, or fix the file write command that created the file to create a valid file.") : _("s at the end."));
    } else if (tt>ncol) {
      if (fill) STOP(_("Internal error: fill=true but there is a previous row which should already have been filled.")); // # nocov
      DTWARN(_("Detected %d column names but the data has %d columns. Filling rows automatically. Set fill=TRUE explicitly to avoid this warning.\n"), tt, ncol);
      fill = true;
      type =    (int8_t *)realloc(type,    (size_t)tt * sizeof(int8_t));
      tmpType = (int8_t *)realloc(tmpType, (size_t)tt * sizeof(int8_t));
      if (!type || !tmpType) STOP(_("Failed to realloc 2 x %d bytes for type and tmpType: %s"), tt, strerror(errno));
      for (int j=ncol; j<tt; j++) { tmpType[j] = type[j] = type0; }
      ncol = tt;
    }
    args.header = true;
    pos = prevStart;
    row1line--;
  }

  if (args.header==NA_BOOL8) {
    args.header = true;
    for (int j=0; j<ncol; j++) {
      if (tmpType[j]<CT_STRING) {
        args.header = false;
        row1line--;
        break;
      }
    }
    if (verbose) {
      if (sampleLines==0) {
        DTPRINT(_("  'header' determined to be %s because there are%s number fields in the first and only row\n"), args.header?"TRUE":"FALSE", args.header?_(" no"):"");
      } else {
        if (args.header)
          DTPRINT(_("  'header' determined to be true because all columns are type string and a better guess is not possible\n"));
        else
          DTPRINT(_("  'header' determined to be false because there are some number columns and those columns do not have a string field at the top of them\n"));
      }
    }
    if (args.header==false && nJumps<=2) sampleLines++; // all data rows may have been sampled, so increment sampleLines because it becomes the exact nrow allocation
  }

  if (args.header==false) {
    // now the header row is treated as data, apply any type bumps from it
    // if user manually set this to false, this runs redundantly
    bool bumped = false;
    for (int j=0; j<ncol; j++) {
      if (tmpType[j]>type[j]) {
        bumped = true;
        type[j] = tmpType[j];
      }
    }
    if (verbose && bumped) DTPRINT(_("  Type codes (first row)   : %s  Quote rule %d\n"), typesAsString(ncol), quoteRule);
  }

  estnrow=1;
  allocnrow=0;     // Number of rows in the allocated DataTable
  meanLineLen=0.0; // Average length (in bytes) of a single line in the input file
  bytesRead=0;     // Bytes in the data section (i.e. excluding column names, header and footer, if any)

  if (sampleLines <= jumpLines) {
    if (verbose) DTPRINT(_("  All rows were sampled since file is small so we know nrow=%"PRIu64" exactly\n"), (uint64_t)sampleLines);
    estnrow = allocnrow = sampleLines;
  } else {
    bytesRead = (size_t)(eof - firstRowStart);
    meanLineLen = (double)sumLen/sampleLines;
    estnrow = CEIL(bytesRead/meanLineLen);  // only used for progress meter and verbose line below
    double sd = sqrt( (sumLenSq - (sumLen*sumLen)/sampleLines)/(sampleLines-1) );
    allocnrow = clamp_szt((size_t)(bytesRead / fmax(meanLineLen - 2*sd, minLen)),
                          (size_t)(1.1*estnrow), 2*estnrow);
    // sd can be very close to 0.0 sometimes, so apply a +10% minimum
    // blank lines have length 1 so for fill=true apply a +100% maximum. It'll be grown if needed.
    if (verbose) {
      DTPRINT(_("  =====\n"));
      DTPRINT(_("  Sampled %"PRIu64" rows (handled \\n inside quoted fields) at %d jump points\n"), (uint64_t)sampleLines, nJumps);
      DTPRINT(_("  Bytes from first data row on line %d to the end of last row: %"PRIu64"\n"), row1line, (uint64_t)bytesRead);
      DTPRINT(_("  Line length: mean=%.2f sd=%.2f min=%d max=%d\n"), meanLineLen, sd, minLen, maxLen);
      DTPRINT(_("  Estimated number of rows: %"PRIu64" / %.2f = %"PRIu64"\n"), (uint64_t)bytesRead, meanLineLen, (uint64_t)estnrow);
      DTPRINT(_("  Initial alloc = %"PRIu64" rows (%"PRIu64" + %d%%) using bytes/max(mean-2*sd,min) clamped between [1.1*estn, 2.0*estn]\n"),
              (uint64_t)allocnrow, (uint64_t)estnrow, (int)(100.0*allocnrow/estnrow-100.0));
      DTPRINT(_("  =====\n"));
    } else {
      if (sampleLines > allocnrow) STOP(_("Internal error: sampleLines(%"PRIu64") > allocnrow(%"PRIu64")"), (uint64_t)sampleLines, (uint64_t)allocnrow); // # nocov
    }
  }
  if (nrowLimit < allocnrow) {
    if (verbose) DTPRINT(_("  Alloc limited to lower nrows=%"PRIu64" passed in.\n"), (uint64_t)nrowLimit);
    estnrow = allocnrow = nrowLimit;
  }
  }

  //*********************************************************************************************
  // [8] Assign column names
  // Updates pos(ition) to rest after the column names (if any) at the start of the first data row
  //*********************************************************************************************
  double tLayout;  // Timer for assigning column names
  const char *colNamesAnchor = pos;
  {
  if (verbose) DTPRINT(_("[08] Assign column names\n"));

  ch = pos;  // back to start of first row (column names if header==true)

  if (args.header==false) {
    colNames = NULL;  // userOverride will assign V1, V2, etc
  } else {
    colNames = (lenOff*) calloc((size_t)ncol, sizeof(lenOff));
    if (!colNames) STOP(_("Unable to allocate %d*%d bytes for column name pointers: %s"), ncol, sizeof(lenOff), strerror(errno));
    if (sep==' ') while (*ch==' ') ch++;
    void *targets[9] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, colNames + autoFirstColName};
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
    else STOP(_("Internal error: reading colnames ending on '%c'"), *ch); // # nocov
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
  if (verbose) DTPRINT(_("[09] Apply user overrides on column types\n"));
  ch = pos;
  memcpy(tmpType, type, (size_t)ncol) ;
  if (!userOverride(type, colNames, colNamesAnchor, ncol)) { // colNames must not be changed but type[] can be
    if (verbose) DTPRINT(_("  Cancelled by user: userOverride() returned false.")); // # nocov
    freadCleanup(); // # nocov
    return 1; // # nocov
  }
  ndrop = 0;
  int nUserBumped=0;
  rowSize1 = 0;
  rowSize4 = 0;
  rowSize8 = 0;
  size = (int8_t *)malloc((size_t)ncol * sizeof(int8_t));  // TODO: remove size[] when we implement Pasha's idea to += size inside processor
  if (!size) STOP(_("Failed to allocate %d bytes for size array: %s"), ncol, strerror(errno));
  nStringCols = 0;
  nNonStringCols = 0;
  for (int j=0; j<ncol; j++) {
    if (type[j]==CT_DROP) { size[j]=0; ndrop++; continue; }
    if (type[j]<tmpType[j]) {
      if (strcmp(typeName[tmpType[j]], typeName[type[j]]) != 0) {
        DTWARN(_("Attempt to override column %d <<%.*s>> of inherent type '%s' down to '%s' ignored. Only overrides to a higher type are currently supported. If this was intended, please coerce to the lower type afterwards."),
               j+1, colNames[j].len, colNamesAnchor+colNames[j].off, typeName[tmpType[j]], typeName[type[j]]);
      }
      type[j] = tmpType[j];
      // TODO: apply overrides to lower type afterwards and warn about the loss of accuracy then (if any); e.g. "4.0" would be fine to coerce to integer with no warning since
      // no loss of accuracy but must be read as double for now in case "4.3" occurs out of sample to know if warning about accuracy is needed afterwards.
    }
    nUserBumped += type[j]>tmpType[j];
    size[j] = typeSize[type[j]];
    rowSize1 += (size[j] & 1);  // only works if all sizes are powers of 2
    rowSize4 += (size[j] & 4);
    rowSize8 += (size[j] & 8);
    if (type[j] == CT_STRING) nStringCols++; else nNonStringCols++;
  }
  if (verbose) DTPRINT(_("  After %d type and %d drop user overrides : %s\n"), nUserBumped, ndrop, typesAsString(ncol));
  tColType = wallclock();
  }

  //*********************************************************************************************
  // [10] Allocate the result columns
  //*********************************************************************************************
  if (verbose) {
    DTPRINT(_("[10] Allocate memory for the datatable\n"));
    DTPRINT(_("  Allocating %d column slots (%d - %d dropped) with %"PRIu64" rows\n"),
            ncol-ndrop, ncol, ndrop, (uint64_t)allocnrow);
  }
  size_t DTbytes = allocateDT(type, size, ncol, ndrop, allocnrow);
  double tAlloc = wallclock();

  //*********************************************************************************************
  // [11] Read the data
  //*********************************************************************************************
  bool stopTeam=false, firstTime=true, restartTeam=false;  // bool for MT-safey (cannot ever read half written bool value badly)
  int nTypeBump=0, nTypeBumpCols=0;
  double tRead=0, tReread=0;
  double thRead=0, thPush=0;  // reductions of timings within the parallel region
  char *typeBumpMsg=NULL;  size_t typeBumpMsgSize=0;
  int typeCounts[NUMTYPE];  // used for verbose output; needs populating after first read and before reread (if any) -- see later comment
  #define internalErrSize 1000
  char internalErr[internalErrSize+1]="";  // must be compile time size: the message is generated and we can't free before STOP
  int64_t DTi = 0;                  // the current row number in DT that we are writing to
  const char *headPos = pos;       // the jump start corresponding to DTi
  int nSwept = 0;                  // count the number of dirty jumps that were swept
  const char *quoteRuleBumpedCh = NULL;   // in the very rare event of an out-of-sample quote rule bump, give a good warning message
  int64_t quoteRuleBumpedLine = -1;
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
  int64_t extraAllocRows = 0;

  if (nJumps/*from sampling*/>2) {
    // ensure data size is split into same sized chunks (no remainder in last chunk) and a multiple of nth
    // when nth==1 we still split by chunk for consistency (testing) and code sanity
    nJumps = (int)(bytesRead/chunkBytes);
    if (nJumps==0) nJumps=1;
    else if (nJumps>nth) nJumps = nth*(1+(nJumps-1)/nth);
    chunkBytes = bytesRead / (size_t)nJumps;
  } else {
    ASSERT(nJumps==1 /*when nrowLimit supplied*/ || nJumps==2 /*small files*/, "nJumps (%d) != 1|2", nJumps);
    nJumps=1;
  }
  int64_t initialBuffRows = (int64_t)allocnrow / nJumps;

  // Catch initialBuffRows==0 when max_nrows is small, seg fault #2243
  // Rather than 10, maybe 1 would work too but then 1.5 grow factor * 1 would still be 1. This clamp
  // should only engage when max_nrows is supplied, and supplied small too, so doesn't matter too much.
  if (initialBuffRows < 10) initialBuffRows = 10;

  if (initialBuffRows > INT32_MAX) STOP(_("Buffer size %"PRId64" is too large\n"), (int64_t)initialBuffRows);
  nth = imin(nJumps, nth);

  if (verbose) DTPRINT(_("[11] Read the data\n"));
  read:  // we'll return here to reread any columns with out-of-sample type exceptions, or dirty jumps
  restartTeam = false;
  if (verbose) DTPRINT(_("  jumps=[%d..%d), chunk_size=%"PRIu64", total_size=%"PRIu64"\n"),
                       jump0, nJumps, (uint64_t)chunkBytes, (uint64_t)(eof-pos));
  ASSERT(allocnrow <= nrowLimit, "allocnrow(%"PRIu64") <= nrowLimit(%"PRIu64")", (uint64_t)allocnrow, (uint64_t)nrowLimit);
  #pragma omp parallel num_threads(nth)
  {
    int me = omp_get_thread_num();
    bool myShowProgress = false;
    #pragma omp master
    {
      nth = omp_get_num_threads();
      if (me!=0) {
        // # nocov start
        snprintf(internalErr, internalErrSize, _("Internal error: Master thread is not thread 0 but thread %d.\n"), me);
        stopTeam = true;
        // # nocov end
      }
      myShowProgress = args.showProgress;
    }
    int64_t myNrow = 0; // the number of rows in my chunk
    int64_t myBuffRows = initialBuffRows;  // Upon realloc, myBuffRows will increase to grown capacity
    bool myStopEarly = false;      // true when an empty or too-short line is encountered when fill=false, or too-long row

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

    #pragma omp for ordered schedule(dynamic) reduction(+:thRead,thPush)
    for (int jump = jump0; jump < nJumps; jump++) {
      if (stopTeam) continue;  // must continue and not break. We desire not to depend on (relatively new) omp cancel directive, yet
      double tLast = 0.0;      // thread local wallclock time at last measuring point for verbose mode only.
      if (verbose) tLast = wallclock();
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
        myNrow = 0;
        if (verbose || myShowProgress) {
          double now = wallclock();
          thPush += now-tLast;
          tLast = now;
          if (myShowProgress && /*wait for all threads to process 2 jumps*/jump>=nth*2) {
            // Important for thread safety inside progess() that this is called not just from critical but that
            // it's the master thread too, hence me==0. OpenMP doesn't allow '#pragma omp master' here, but we
            // did check above that master's me==0.
            int ETA = (int)(((now-tAlloc)/jump) * (nJumps-jump));
            progress((int)(100.0*jump/nJumps), ETA);
          }
        }
      }

      const char *tch = jump==jump0 ? headPos : nextGoodLine(pos+(size_t)jump*chunkBytes, ncol);
      const char *thisJumpStart = tch;   // "this" for prev/this/next adjective used later, rather than a (mere) t prefix for thread-local.
      const char *tLineStart = tch;
      const char *nextJumpStart = jump<nJumps-1 ? nextGoodLine(pos+(size_t)(jump+1)*chunkBytes, ncol) : eof;

      void *targets[9] = {NULL, ctx.buff1, NULL, NULL, ctx.buff4, NULL, NULL, NULL, ctx.buff8};
      FieldParseContext fctx = {
        .ch = &tch,
        .targets = targets,
        .anchor = thisJumpStart,
      };

      while (tch<nextJumpStart && (nth>1 || DTi+myNrow<nrowLimit)) {  // setting nrowLimit sets nth to 1 to avoid bump or error on row after nrowLimit
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
        tLineStart = tch;  // for error message
        const char *fieldStart = tch;
        int j = 0;

        //*** START HOT ***//
        if (sep!=' ' && !any_number_like_NAstrings) {  // TODO:  can this 'if' be dropped somehow? Can numeric NAstrings be dealt with afterwards in one go as numeric comparison?
          // Try most common and fastest branch first: no whitespace, no quoted numeric, ",," means NA
          while (j < ncol) {
            // DTPRINT(_("Field %d: '%.10s' as type %d  (tch=%p)\n"), j+1, tch, type[j], tch);
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
          if (tch==tLineStart) {
            skip_white(&tch);       // skips \0 before eof
            if (*tch=='\0') break;  // empty last line
            if (eol(&tch) && skipEmptyLines) { tch++; continue; }
            tch = tLineStart;  // in case white space at the beginning may need to be including in field
          }
          else if (eol(&tch) && j<ncol) {   // j<ncol needed for #2523 (erroneous extra comma after last field)
            int8_t thisSize = size[j];
            ((char **) targets)[thisSize] += thisSize;
            j++;
            if (j==ncol) { tch++; myNrow++; continue; }  // next line. Back up to while (tch<nextJumpStart). Usually happens, fastest path
          }
          else {
            tch = fieldStart; // restart field as int processor could have moved to A in ",123A,"
          }
          // if *tch=='\0' then *eof in mind, fall through to below
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
          while (*tch==' ') tch++;  // multiple sep=' ' at the tLineStart does not mean sep. We're at tLineStart because the fast branch above doesn't run when sep=' '
          fieldStart = tch;
          skip_white(&tch);          // skips \0 before eof
          if (*tch=='\0') continue;  // tch==eof; empty last line
          if (eol(&tch) && skipEmptyLines) { tch++; continue; }
          tch = fieldStart;         // in case tabs at the beginning of the first field need to be included
        }
        bool checkedNumberOfFields = false;
        if (fill || ncol==1 || (*tch!='\n' && *tch!='\r')) while (j < ncol) {
          fieldStart = tch;
          int8_t joldType = type[j];
          int8_t thisType = joldType;  // to know if it was bumped in (rare) out-of-sample type exceptions
          int8_t absType = (int8_t)abs(thisType);

          while (absType < NUMTYPE) {
            tch = fieldStart;
            bool quoted = false;
            if (absType<CT_STRING && absType>CT_DROP/*Field() too*/) {
              skip_white(&tch);
              const char *afterSpace = tch;
              tch = end_NA_string(tch);
              skip_white(&tch);
              if (!end_of_field(tch)) tch = afterSpace; // else it is the field_end, we're on closing sep|eol and we'll let processor write appropriate NA as if field was empty
              if (*tch==quote && quote) { quoted=true; tch++; }
            } // else Field() handles NA inside it unlike other processors e.g. ,, is interpretted as "" or NA depending on option read inside Field()
            fun[abs(thisType)](&fctx);
            if (quoted) {   // quoted was only set to true with '&& quote' above (=> quote!='\0' now)
              if (*tch==quote) tch++;
              else goto typebump;
            }
            skip_white(&tch);
            if (end_of_field(tch)) {
              if (sep==' ' && *tch==' ') {
                while (tch[1]==' ') tch++;  // multiple space considered one sep so move to last
                if (tch[1]=='\r' || tch[1]=='\n' || tch+1==eof) tch++;
              }
              break;
            }

            // guess is insufficient out-of-sample, type is changed to negative sign and then bumped. Continue to
            // check that the new type is sufficient for the rest of the column (and any other columns also in out-of-sample bump status) to be
            // sure a single re-read will definitely work.
            typebump:
            while (++absType<CT_STRING && disabled_parsers[absType]) {};
            thisType = -absType;
            tch = fieldStart;
          }

          if (thisType != joldType) {             // rare out-of-sample type exception.
            if (!checkedNumberOfFields && !fill) {
              // check this line has the correct number of fields. If not, don't apply the bump from this invalid line. Instead fall through to myStopEarly below.
              const char *tt = fieldStart;
              int fieldsRemaining = countfields(&tt);
              if (j+fieldsRemaining != ncol) break;
              checkedNumberOfFields = true;
            }
            #pragma omp critical
            {
              joldType = type[j];  // fetch shared value again in case another thread bumped it while I was waiting.
              // Can't print because we're likely not master. So accumulate message and print afterwards.
              if (thisType < joldType) {   // thisType<0 (type-exception)
                if (verbose) {
                  char temp[1001];
                  int len = snprintf(temp, 1000,
                    _("Column %d (\"%.*s\") bumped from '%s' to '%s' due to <<%.*s>> on row %"PRIu64"\n"),
                    j+1, colNames[j].len, colNamesAnchor + colNames[j].off,
                    typeName[abs(joldType)], typeName[abs(thisType)],
                    (int)(tch-fieldStart), fieldStart, (uint64_t)(ctx.DTi+myNrow));
                  if (len > 1000) len = 1000;
                  if (len > 0) {
                    typeBumpMsg = (char*) realloc(typeBumpMsg, typeBumpMsgSize + (size_t)len + 1);
                    strcpy(typeBumpMsg+typeBumpMsgSize, temp);
                    typeBumpMsgSize += (size_t)len;
                  }
                }
                nTypeBump++;
                if (joldType>0) nTypeBumpCols++;
                type[j] = thisType;
              } // else another thread just bumped to a (negative) higher or equal type while I was waiting, so do nothing
            }
          }
          ((char**) targets)[size[j]] += size[j];
          j++;
          if (*tch==sep) { tch++; continue; }
          if (fill && (*tch=='\n' || *tch=='\r' || tch==eof) && j<ncol) continue;  // reuse processors to write appropriate NA to target; saves maintenance of a type switch down here
          break;
        }
        if (j<ncol || (!eol(&tch) && tch!=eof))  {
          // Too few or too many columns observed (but not empty lines when skipEmptyLines as they were found and skipped earlier above).
          // If fill==true, fields should already have been filled above due to continue inside while(j<ncol).
          myStopEarly = true;
          tch = tLineStart;
          break;
        }
        if (tch!=eof) tch++;
        myNrow++;
      }
      if (verbose) { double now = wallclock(); thRead += now-tLast; tLast = now; }
      ctx.anchor = thisJumpStart;
      ctx.nRows = myNrow;
      postprocessBuffer(&ctx);

      // if (tch>nextJumpStart) I know now that the next jump is dirty and should be swept. But, there might be earlier jumps
      // already waiting at ordered that are dirty too; i.e. it's likely that thisJumpStart!=headPos. Therefore, sweeping must
      // be triggered not here before ordered but when we're inside ordered and we're at headPos reliably.

      #pragma omp ordered
      {
        if (stopTeam) {             // A previous thread stopped while I was waiting my turn to enter ordered
          myNrow = 0;               // # nocov; discard my buffer
        }
        else if (headPos!=thisJumpStart) {
           // # nocov start
          snprintf(internalErr, internalErrSize, _("Internal error: invalid head position. jump=%d, headPos=%p, thisJumpStart=%p, sof=%p"), jump, (void*)headPos, (void*)thisJumpStart, (void*)sof);
          stopTeam = true;
          // # nocov end
        }
        else {
          ctx.DTi = DTi;  // fetch shared DTi (where to write my results to the answer). The previous thread just told me.
          if (ctx.DTi + myNrow > allocnrow) {
            // Guess for DT's nrow was insufficient. We cannot realloc DT now because other threads are pushing to DT now in
            // parallel. So, stop team, realloc and then restart reading from this jump.
            extraAllocRows = (int64_t)((double)(DTi+myNrow)*nJumps/(jump+1) * 1.2) - allocnrow;
            if (extraAllocRows < 1024) extraAllocRows = 1024;
            myNrow = 0;    // discard my buffer even though it was read correctly; this one jump will be reread wastefully in this rare case
            stopTeam = restartTeam = true;
            jump0 = jump;
          } else {
            // tell next thread 2 things :
            headPos = tch;  // i) advance headPos; the jump start up to which all rows have been pushed
            DTi += myNrow;  // ii) which row in the final result next thread should start writing to since now I know myNrow.
            ctx.nRows = myNrow;
            orderBuffer(&ctx);
            if (myStopEarly) {
              if (quoteRule<3) {
                quoteRule++;
                if (quoteRuleBumpedCh == NULL) {
                  // for warning message if the quote rule bump does in fact manage to heal it, e.g. test 1881
                  quoteRuleBumpedCh = tLineStart;
                  quoteRuleBumpedLine = row1line+DTi;
                }
                restartTeam = true;
                jump0 = jump;  // this jump will restart from headPos, not from its beginning, e.g. test 1453
              }
              stopTeam = true;
            } else if (headPos>nextJumpStart) {
              nSwept++;         // next jump landed awkwardly and will be reread from headPos; i.e. next jump is dirty and will be swept
              stopTeam = restartTeam = true;
              jump0 = jump+1;   // restart team from next jump. jump0 always starts from headPos
              // if too many jumps are dirty, scale down to single-threaded to save restarting team too much, wastefully. The file requires
              // a single threaded read anyway due to its tortuous complexity with so many embedded newlines so often
              if (nth>1 && nSwept>5 && (double)nSwept/jump > 0.10) nth=1;
            }
          }
        }
      }
      // END ORDERED.
      // Next thread can now start her ordered section and write her results to the final DT at the same time as me.
      // Ordered has to be last in some OpenMP implementations currently. Logically though, pushBuffer happens now.
    }
    // End for loop over all jump points

    // Push out all buffers one last time (only needed because of gomp ordered workaround above with push first in the loop)
    // If stopped early, this will happen once for thread at headPos (the only one left with myNrow>0)
    if (myNrow) {
      double now = verbose ? wallclock() : 0;
      pushBuffer(&ctx);
      if (verbose) thPush += wallclock() - now;
    }
    // Each thread to free their own buffer.
    free(ctx.buff8); ctx.buff8 = NULL;
    free(ctx.buff4); ctx.buff4 = NULL;
    free(ctx.buff1); ctx.buff1 = NULL;
    freeThreadContext(&ctx);
  }
  //-- end parallel ------------------

  if (stopTeam) {
    if (internalErr[0]!='\0') {
      STOP("%s", internalErr);  // # nocov
    }
    stopTeam = false;

    if (extraAllocRows) {
      allocnrow += extraAllocRows;
      if (allocnrow > nrowLimit) allocnrow = nrowLimit;
      if (verbose) DTPRINT(_("  Too few rows allocated. Allocating additional %"PRIu64" rows (now nrows=%"PRIu64") and continue reading from jump %d\n"),
                           (uint64_t)extraAllocRows, (uint64_t)allocnrow, jump0);
      allocateDT(type, size, ncol, ncol - nStringCols - nNonStringCols, allocnrow);
      extraAllocRows = 0;
      goto read;
    }
    if (restartTeam) {
      if (verbose) DTPRINT(_("  Restarting team from jump %d. nSwept==%d quoteRule==%d\n"), jump0, nSwept, quoteRule);
      ASSERT(nSwept>0 || quoteRuleBumpedCh!=NULL, "Internal error: team restart but nSwept==%d and quoteRuleBumpedCh==%p", nSwept, quoteRuleBumpedCh); // # nocov
      goto read;
    }
    // else nrowLimit applied and stopped early normally
  }

  // tell progress meter to finish up; e.g. write final newline
  // if there's a reread, the progress meter will start again from 0
  if (args.showProgress) progress(100, 0);

  if (firstTime) {
    tReread = tRead = wallclock();

    // if nTypeBump>0, not-bumped columns are about to be assigned parse type -CT_STRING for the reread, so we have to count
    // parse types now (for log). We can't count final column types afterwards because many parse types map to the same column type.
    for (int i=0; i<NUMTYPE; i++) typeCounts[i] = 0;
    for (int i=0; i<ncol; i++) typeCounts[ abs(type[i]) ]++;

    if (nTypeBump) {
      if (verbose) DTPRINT(_("  %d out-of-sample type bumps: %s\n"), nTypeBump, typesAsString(ncol));
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
      headPos = pos;
      jump0 = 0;
      firstTime = false;
      nSwept = 0;
      goto read;
    }
  } else {
    tReread = wallclock();
  }

  double tTot = tReread-t0;  // tReread==tRead when there was no reread
  if (verbose) DTPRINT(_("Read %"PRIu64" rows x %d columns from %s file in %02d:%06.3f wall clock time\n"),
       (uint64_t)DTi, ncol-ndrop, filesize_to_str(fileSize), (int)tTot/60, fmod(tTot,60.0));

  //*********************************************************************************************
  // [12] Finalize the datatable
  //*********************************************************************************************
  if (verbose) {
    DTPRINT(_("[12] Finalizing the datatable\n"));
    DTPRINT(_("  Type counts:\n"));
    for (int i=0; i<NUMTYPE; i++) if (typeCounts[i]) {
      DTPRINT(_("%10d : %-9s '%c'\n"), typeCounts[i], typeName[i], typeLetter[i]);
    }
  }
  setFinalNrow(DTi);

  if (headPos<eof && DTi<nrowLimit) {
    ch = headPos;
    while (ch<eof && isspace(*ch)) ch++;
    if (ch==eof) {
      // whitespace at the end of the file is always skipped ok
    } else {
      const char *skippedFooter = ch;
      // detect if it's a single line footer. Commonly the row count from SQL queries.
      while (ch<eof && *ch!='\n' && *ch!='\r') ch++;
      while (ch<eof && isspace(*ch)) ch++;
      if (ch==eof) {
        DTWARN(_("Discarded single-line footer: <<%s>>"), strlim(skippedFooter,500));
      }
      else {
        ch = headPos;
        int tt = countfields(&ch);
        DTWARN(_("Stopped early on line %"PRIu64". Expected %d fields but found %d. Consider fill=TRUE and comment.char=. First discarded non-empty line: <<%s>>"),
          (uint64_t)DTi+row1line, ncol, tt, strlim(skippedFooter,500));
      }
    }
  }
  if (quoteRuleBumpedCh!=NULL && quoteRuleBumpedCh<headPos) {
    DTWARN(_("Found and resolved improper quoting out-of-sample. First healed line %"PRIu64": <<%s>>. If the fields are not quoted (e.g. field separator does not appear within any field), try quote=\"\" to avoid this warning."), (uint64_t)quoteRuleBumpedLine, strlim(quoteRuleBumpedCh, 500));
  }

  if (verbose) {
    DTPRINT(_("=============================\n"));
    if (tTot<0.000001) tTot=0.000001;  // to avoid nan% output in some trivially small tests where tot==0.000s
    DTPRINT(_("%8.3fs (%3.0f%%) Memory map %.3fGB file\n"), tMap-t0, 100.0*(tMap-t0)/tTot, 1.0*fileSize/(1024*1024*1024));
    DTPRINT(_("%8.3fs (%3.0f%%) sep="), tLayout-tMap, 100.0*(tLayout-tMap)/tTot);
      DTPRINT(sep=='\t' ? "'\\t'" : (sep=='\n' ? "'\\n'" : "'%c'"), sep);
      DTPRINT(_(" ncol=%d and header detection\n"), ncol);
    DTPRINT(_("%8.3fs (%3.0f%%) Column type detection using %"PRIu64" sample rows\n"),
            tColType-tLayout, 100.0*(tColType-tLayout)/tTot, (uint64_t)sampleLines);
    DTPRINT(_("%8.3fs (%3.0f%%) Allocation of %"PRIu64" rows x %d cols (%.3fGB) of which %"PRIu64" (%3.0f%%) rows used\n"),
      tAlloc-tColType, 100.0*(tAlloc-tColType)/tTot, (uint64_t)allocnrow, ncol, DTbytes/(1024.0*1024*1024), (uint64_t)DTi, 100.0*DTi/allocnrow);
    thRead/=nth; thPush/=nth;
    double thWaiting = tReread-tAlloc-thRead-thPush;
    DTPRINT(_("%8.3fs (%3.0f%%) Reading %d chunks (%d swept) of %.3fMB (each chunk %d rows) using %d threads\n"),
            tReread-tAlloc, 100.0*(tReread-tAlloc)/tTot, nJumps, nSwept, (double)chunkBytes/(1024*1024), (int)(DTi/nJumps), nth);
    DTPRINT(_("   + %8.3fs (%3.0f%%) Parse to row-major thread buffers (grown %d times)\n"), thRead, 100.0*thRead/tTot, buffGrown);
    DTPRINT(_("   + %8.3fs (%3.0f%%) Transpose\n"), thPush, 100.0*thPush/tTot);
    DTPRINT(_("   + %8.3fs (%3.0f%%) Waiting\n"), thWaiting, 100.0*thWaiting/tTot);
    DTPRINT(_("%8.3fs (%3.0f%%) Rereading %d columns due to out-of-sample type exceptions\n"),
            tReread-tRead, 100.0*(tReread-tRead)/tTot, nTypeBumpCols);
    DTPRINT(_("%8.3fs        Total\n"), tTot);
    if (typeBumpMsg) {
      // if type bumps happened, it's useful to see them at the end after the timing 2 lines up showing the reread time
      // TODO - construct and output the copy and pastable colClasses argument so user can avoid the reread time if they are
      //        reading this file or files formatted like it many times (say in a production environment).
      DTPRINT("%s", typeBumpMsg);
      free(typeBumpMsg);  // local scope and only populated in verbose mode
    }
  }
  freadCleanup();
  return 1;
}
