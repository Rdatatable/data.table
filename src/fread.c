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
static char sep, eol, eol2;
static char antisep;
static int eolLen;
static char quote, dec;

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
static char *lineCopy = NULL;
static int8_t *type = NULL, *size = NULL;
static lenOff *colNames = NULL;
static int8_t *oldType = NULL;
static freadMainArgs args;  // global for use by DTPRINT

const char typeName[NUMTYPE][10] = {"drop", "bool8", "int32", "int32", "int64", "float64", "string"};
int8_t     typeSize[NUMTYPE]     = { 0,      1,       4,       4,       8,       8,         8      };
// size_t to prevent potential overflow of n*typeSize[i] (standard practice)

// NAN and INFINITY constants are float, so cast to double once up front.
static const double NAND = (double)NAN;
static const double INFD = (double)INFINITY;

// Forward declarations
static int Field(const char **this, void *target);
static int parse_string(const char **ptr, lenOff *target);
static int parse_string_continue(const char **ptr, lenOff *target);


/**
 * Free any resources / memory buffers allocated by the fread() function, and
 * bring all global variables to a "clean slate". This function must always be
 * executed when fread() exits, either successfully or not.
 */
void freadCleanup(void)
{
  // If typeOnStack is true, then `type` was `alloca`-ed, and therefore must not be freed!
  free(type); type = NULL;
  free(size); size = NULL;
  free(lineCopy); lineCopy = NULL;
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
  sep = antisep = eol = eol2 = quote = dec = '\0';
  eolLen = 0;
  quoteRule = -1;
  any_number_like_NAstrings = false;
  blank_is_a_NAstring = false;
  stripWhite = true;
  skipEmptyLines = false;
  fill = false;
  // following are borrowed references: do not free
  NAstrings = NULL;
}

#define ASSERT(test) do { \
  if (!(test)) \
    STOP("Assertion violation at line %d, please report at " \
         "https://github.com/Rdatatable/data.table/issues", __LINE__); \
} while(0)

#define CEIL(x)  ((size_t)(double)ceil(x))
static inline size_t umax(size_t a, size_t b) { return a > b ? a : b; }
static inline size_t umin(size_t a, size_t b) { return a < b ? a : b; }
static inline int imin(int a, int b) { return a < b ? a : b; }

/** Return value of `x` clamped to the range [upper, lower] */
static inline size_t clamp_szt(size_t x, size_t lower, size_t upper) {
  return x < lower ? lower : x > upper? upper : x;
}


// Helper for error and warning messages to extract next 10 chars or \n if occurs first
// Used exclusively together with "%.*s"
static int STRLIM(const char *ch, int limit, const char *eof) {
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
  if (antisep == 0) {
    while (*ch == ' ' || *ch == '\t') ch++;
  } else {
    while (*ch == antisep) ch++;
  }
  *this = ch;
}


static inline _Bool on_sep(const char **this) {
  const char *ch = *this;
  if (sep == ' ' && *ch == ' ') {
    while (ch[1] == ' ') ch++;  // move to last of this sequence of spaces
    if (ch[1] == eol) ch++;    // if that's followed by eol then move over
  }
  *this = ch;
  return (*ch == sep) || (*ch == eol);
}

static inline void next_sep(const char **this) {
  const char *ch = *this;
  while (*ch != sep && *ch != eol) ch++;
  on_sep(&ch); // to deal with multiple spaces when sep==' '
  *this = ch;
}

static inline _Bool is_NAstring(const char *fieldStart) {
  skip_white(&fieldStart);  // updates local fieldStart
  const char* const* nastr = NAstrings;
  while (*nastr) {
    const char *ch1 = fieldStart;
    const char *ch2 = *nastr;
    while (*ch1 == *ch2) { ch1++; ch2++; }  // not using strncmp due to eof not being '\0'
    if (*ch2=='\0') {
      skip_white(&ch1);
      if (*ch1==sep || *ch1==eol) return true;
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
 */
static int countfields(const char **ptr, const char **end, const char *soh, const char *eoh)
{
  static int64_t tmp;
  void *nowhere = (void *) &tmp;  // target for writing out parsed fields
  const char *ch = *ptr;
  const char *tend = *end;
  if (sep==' ') {
    while (*ch==' ') ch++;  // multiple sep==' ' at the start does not mean sep
  }
  skip_white(&ch);

  if (*ch == eol && ch[eolLen-1] == eol2) {
    *ptr = ch + eolLen;
    return 0;
  } else {
    int ncol = 0;
    while (1) {
      int res = parse_string(&ch, nowhere);
      if (res == 1) return -1;
      if (res == 2) {
        int linesCount = 0;
        while (res == 2 && linesCount++ < 100) {
          if (ch == tend) {
            if (eoh && tend != eoh) {
              ch = soh;
              tend = eoh;
            } else {
              return -1;
            }
          }
          res = parse_string_continue(&ch, nowhere);
        }
      }
      // Field() leaves *ch resting on sep or eol. Checked inside Field().
      ncol++;
      if (*ch==eol) { ch+=eolLen; break; }
      ch++;  // move over sep (which will already be last ' ' if sep=' ').
    }
    *ptr = ch;
    *end = tend;
    return ncol;
  }
}



static inline _Bool nextGoodLine(const char **this, int ncol, const char *eof)
{
  const char *ch = *this;
  // we may have landed inside quoted field containing embedded sep and/or embedded \n
  // find next \n and see if 5 good lines follow. If not try next \n, and so on, until we find the real \n
  // We don't know which line number this is, either, because we jumped straight to it. So return true/false for
  // the line number and error message to be worked out up there.
  int attempts=0;
  while (ch<eof && attempts++<30) {
    while (*ch != eol) ch++;
    ch += eolLen;
    int i = 0, thisNcol=0;
    const char *ch2 = ch;
    const char *end = eof;
    while (ch2<eof && i<5 && ( (thisNcol=countfields(&ch2, &end, NULL, NULL))==ncol || (thisNcol==0 && (skipEmptyLines || fill)))) i++;
    if (i==5 || ch2>=eof) break;
  }
  if (ch<eof && attempts<30) { *this = ch; return true; }
  return false;
}


double wallclock(void)
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


static int advance_sof_to(const char *newsof, const char **sof,
                           const char **eof, const char **soh, const char **eoh)
{
  intptr_t d = 0;
  if (*sof <= newsof && newsof < *eof) {
    d = newsof - *sof;
    *sof = newsof;
  } else if (newsof == *eof || newsof == *soh) {
    d = *eof - *sof;
    *sof = *soh;
    *eof = *eoh;
    *soh = *eoh = NULL;
  } else if (*soh < newsof && newsof <= *eoh) {
    d = (*eof - *sof) + (newsof - *soh);
    *sof = newsof;
    *eof = *eoh;
    *soh = *eoh = NULL;
  }
  if (d && args.verbose)
    DTPRINT("  Start-of-file pointer moved %zd bytes forward\n", d);
  return 1;
}


static int retreat_eof_to(const char *neweof, const char **sof,
                           const char **eof, const char **soh, const char **eoh)
{
  intptr_t d = 0;
  if (*soh < neweof && neweof <= *eoh) {
    d = *eoh - neweof;
    *eoh = neweof;
  } else if (neweof == *soh || neweof == *eof) {
    d = *eoh - *soh;
    *soh = *eoh = NULL;
  } else if (*sof <= neweof && neweof <= *eof) {
    d = (*eof - neweof) + (*eoh - *soh);
    *eof = neweof;
    *soh = *eoh = NULL;
  }
  if (d && args.verbose)
    DTPRINT("  End-of-file pointer moved %zd bytes backward\n", d);
  return 1;
}



//=================================================================================================
//
//   Field parsers
//
//=================================================================================================

static int Field(const char **this, void *target)
{
  const char *ch = *this;
  if (stripWhite) skip_white(&ch);  // before and after quoted field's quotes too (e.g. test 1609) but never inside quoted fields
  const char *fieldStart=ch;
  _Bool quoted = false;
  if (*ch!=quote || quoteRule==3) {
    // unambiguously not quoted. simply search for sep|eol. If field contains sep|eol then it must be quoted instead.
    while(*ch != sep && *ch != eol) ch++;
  } else {
    // the field is quoted and quotes are correctly escaped (quoteRule 0 and 1)
    // or the field is quoted but quotes are not escaped (quoteRule 2)
    // or the field is not quoted but the data contains a quote at the start (quoteRule 2 too)
    int eolCount = 0;
    quoted = true;
    fieldStart = ch+1; // step over opening quote
    switch(quoteRule) {
    case 0:  // quoted with embedded quotes doubled; the final unescaped " must be followed by sep|eol
      while (eolCount<100) {  // TODO: expose this 100 to user to allow them to increase
        ch++;
        eolCount += (*ch==eol);
        // 100 prevents runaway opening fields by limiting eols. Otherwise the whole file would be read in the sep and
        // quote rule testing step.
        if (*ch==quote) {
          if (ch[1] == quote) { ch++; continue; }
          break;  // found undoubled closing quote
        }
      }
      if (*ch != quote) return 1;
      break;
    case 1:  // quoted with embedded quotes escaped; the final unescaped " must be followed by sep|eol
      while (*(++ch) != quote && eolCount < 100) {
        eolCount += (*ch==eol);
        ch += (*ch=='\\');
      }
      if (*ch != quote) return 1;
      break;
    case 2:  // (i) quoted (perhaps because the source system knows sep is present) but any quotes were not escaped at all,
             // so look for ", to define the end.   (There might not be any quotes present to worry about, anyway).
             // (ii) not-quoted but there is a quote at the beginning so it should have been, look for , at the end
             // If no eol are present in the data (i.e. rows are rows), then this should work ok e.g. test 1453
             // since we look for ", and the source system quoted when , is present, looking for ", should work well.
             // No eol may occur inside fields, under this rule.
      {
        const char *ch2 = ch;
        while (*(++ch) != eol) {
          if (*ch==quote && (ch[1] == sep || ch[1] == eol)) {ch2=ch; break;}   // (*1) regular ", ending
          if (*ch==sep) {
            // first sep in this field
            // if there is a ", afterwards but before the next \n, use that; the field was quoted and it's still case (i) above.
            // Otherwise break here at this first sep as it's case (ii) above (the data contains a quote at the start and no sep)
            ch2 = ch;
            while (*(++ch2) != eol) {
              if (*ch2==quote && (ch2[1] == sep || ch2[1] == eol)) {
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
  ((lenOff *)target)->len = fieldLen;
  ((lenOff *)target)->off = (uint32_t)(fieldStart-*this);  // agnostic & thread-safe
  *this = ch; // Update caller's ch. This may be after fieldStart+fieldLen due to quotes and/or whitespace
  return 0;
}


static int parse_string(const char **ptr, lenOff *target)
{
  const char *ch = *ptr;
  if (stripWhite) skip_white(&ch);
  const char *start = ch;
  _Bool quoted = false;

  // consume the field: this section will move the pointer after the last
  // character within in the field. If the field was quoted then it will be the
  // quote character; otherwise it should be the separator.
  switch (quoteRule) {
    case 0: {
      // Rule 0: the field is either unquoted; or it is quoted and then the
      // internal quotes are doubled. The field may have embedded newlines.
      if (*ch == quote) {
        quoted = true;
        start = ch = ch + 1;
        while (*ch != eol) {
          if (*ch == quote) {
            if (ch[1] == quote) ch++;
            else break;
          }
          ch++;
        }
        if (*ch == eol) {
          target->len = (int32_t)(ch - start) + eolLen;
          target->off = (uint32_t)(start - *ptr);
          *ptr = ch + eolLen;
          return 2;
        }
      } else {
        while (*ch != sep && *ch != eol) ch++;
      }
      break;
    }

    case 1: {
      // Rule 1: the field is either unquoted; or it is quoted and then the
      // internal quotes are escaped with the backslash character. The field
      // is allowed to have embedded newlines.
      if (*ch == quote) {
        quoted = true;
        start = ch = ch + 1;
        while (*ch != eol && *ch != quote) {
          ch += 1 + (*ch == '\\' && ch[1] != eol);
        }
        if (*ch == eol) {
          target->len = (int32_t)(ch - start) + eolLen;
          target->off = (uint32_t)(start - *ptr);
          *ptr = ch + eolLen;
          return 2;
        }
      } else {
        while (*ch != sep && *ch != eol) ch++;
      }
      break;
    }

    case 2: {
      // Rule 2: the field is either unquoted (no quotes inside are allowed), or
      // it was quoted but any internal quotation marks were not escaped. This
      // is a "sloppy" rule: it does not allow to parse input unambiguously. We
      // will assume that a quoted field ends when we see a quote character
      // followed by a separator. This rule doesn't allow embedded newlines
      // inside fields.
      if (*ch == quote) {
        quoted = true;
        start = ch + 1;
        /*
        while (*ch != eol) {
          if (*ch == quote && (ch[1] == sep || ch[1] == eol)) break;
          ch++;
        }
        if (*ch != quote) {
          // Invalid field: finished at newline without the closing quote.
          return 1;
        }
        */
        const char *ch2 = ch;
        while (*(++ch) != eol) {
          if (*ch==quote && (ch[1] == sep || ch[1] == eol)) {ch2=ch; break;}   // (*1) regular ", ending
          if (*ch==sep) {
            // first sep in this field
            // if there is a ", afterwards but before the next \n, use that; the field was quoted and it's still case (i) above.
            // Otherwise break here at this first sep as it's case (ii) above (the data contains a quote at the start and no sep)
            ch2 = ch;
            while (*(++ch2) != eol) {
              if (*ch2==quote && (ch2[1] == sep || ch2[1] == eol)) {
                ch = ch2; // (*2) move on to that first ", -- that's this field's ending
                break;
              }
            }
            break;
          }
        }
        if (ch!=ch2) { start--; quoted=false; }
      } else {
        while (*ch != sep && *ch != eol) ch++;
      }
      break;
    }

    case 3: {
      // Rule 3: fields are not quoted, any quote characters are interpreted
      // literally. This rule always succeeds.
      while (*ch != sep && *ch != eol) {
        ch++;
      }
      break;
    }
  }

  int32_t fieldLength = (int)(ch - start);
  if (quoted) {
    ch++;
    if (stripWhite) skip_white(&ch);
  } else if (stripWhite) {
    // Remove trailing whitespace: note that we don't move ch pointer, merely
    // adjust the field length
    while (fieldLength && (start[fieldLength-1] == ' ' || start[fieldLength-1] == '\t'))
      fieldLength--;
  }
  if (!on_sep(&ch)) {
    return 1;  // Field ended unexpectedly: cannot happen under quoteRule 3
  }
  if (fieldLength == 0) {
    if (blank_is_a_NAstring) fieldLength = INT32_MIN;
  } else {
    if (is_NAstring(start)) fieldLength = INT32_MIN;
  }
  target->len = fieldLength;
  target->off = (uint32_t)(start - *ptr);
  *ptr = ch;
  return 0;
}


static int parse_string_continue(const char **ptr, lenOff *target)
{
  const char *ch = *ptr;
  ASSERT(quoteRule <= 1);
  if (quoteRule == 0) {
    while (*ch != eol) {
      if (*ch == quote) {
        if (ch[1] == quote) ch++;
        else break;
      }
      ch++;
    }
  } else {
    while (*ch != eol && *ch != quote) {
      ch += 1 + (*ch == '\\' && ch[1] != eol);
    }
  }
  if (*ch == eol) {
    target->len += (int32_t)(ch - *ptr + eolLen);
    *ptr = ch + eolLen;
    return 2;
  } else {
    ASSERT(*ch == quote);
    ch++;
    if (stripWhite) skip_white(&ch);
    if (!on_sep(&ch)) {
      return 1;
    }
    target->len += (int32_t)(ch - *ptr - 1);  // -1 removes closing quote
    *ptr = ch;
    return 0;
  }
}


static int StrtoI64(const char **this, void *target)
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
      return 0;
    }
    const char *start=ch;
    int sign=1;
    _Bool quoted = false;
    if (*ch == quote) { quoted=true; ch++; }
    if (*ch=='-' || *ch=='+') sign -= 2*(*ch++=='-');
    _Bool ok = '0'<=*ch && *ch<='9';  // a single - or + with no [0-9] is !ok and considered type character
    int64_t acc = 0;
    while ('0'<=*ch && *ch<='9' && acc<(INT64_MAX-10)/10) { // compiler should optimize last constant expression
      // Conveniently, INT64_MIN == -9223372036854775808 and INT64_MAX == +9223372036854775807
      // so the full valid range is now symetric [-INT64_MAX,+INT64_MAX] and NA==INT64_MIN==-INT64_MAX-1
      acc *= 10;
      acc += *ch-'0';
      ch++;
    }
    if (quoted) { if (*ch != quote) return 1; else ch++; }
    // TODO: if (!targetCol) return early?  Most of the time, not though.
    *(int64_t *)target = sign * acc;
    skip_white(&ch);
    ok = ok && on_sep(&ch);
    //DTPRINT("StrtoI64 field '%.*s' has len %d\n", lch-ch+1, ch, len);
    *this = ch;
    if (ok && !any_number_like_NAstrings) return 0;  // most common case, return
    _Bool na = is_NAstring(start);
    if (ok && !na) return 0;
    *(int64_t *)target = NA_INT64;
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
      acc = acc*10 + digit;     // optimizer has best chance here; e.g. (x<<2 + x)<<1 or x<<3 + x<<1
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
    if (*ch==quote) { quoted=true; ch++; }
    if (*ch=='-' || *ch=='+') sign -= 2*(*ch++=='-');
    _Bool ok = '0'<=*ch && *ch<='9';
    int acc = 0;
    while ('0'<=*ch && *ch<='9' && acc<(INT32_MAX-10)/10) {  // NA==INT_MIN==-2147483648==-INT_MAX(+2147483647)-1
      acc *= 10;
      acc += *ch-'0';
      ch++;
    }
    if (quoted) { if (*ch!=quote) return 1; else ch++; }
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
    if (*ch==quote) { quoted=true; ch++; }
    int sign=1;
    double d = NAND;
    const char *start=ch;
    if (*ch=='-' || *ch=='+') sign -= 2*(*ch++=='-');
    _Bool ok = ('0'<=*ch && *ch<='9') || *ch==dec;  // a single - or + with no [0-9] is !ok and considered type character
    if (!ok) {
      if      (*ch=='I' && *(ch+1)=='n' && *(ch+2)=='f') { ch+=3; d = sign*INFD; ok=true; }
      else if (*ch=='N' && *(ch+1)=='A' && *(ch+2)=='N') { ch+=3; d = NAND; ok=true; }
    } else {
      uint64_t acc = 0;
      while ('0'<=*ch && *ch<='9' && acc<(UINT64_MAX-10)/10) { // compiler should optimize last constant expression
        // UNIT64_MAX == 18446744073709551615
        acc *= 10;
        acc += (uint64_t)(*ch - '0');
        ch++;
      }
      const char *decCh = (*ch==dec) ? ++ch : NULL;
      while ('0'<=*ch && *ch<='9' && acc<(UINT64_MAX-10)/10) {
        acc *= 10;
        acc += (uint64_t)(*ch - '0');
        ch++;
      }
      int e = decCh ? -(int)(ch-decCh) : 0;
      if (decCh) while ('0'<=*ch && *ch<='9') ch++; // lose precision
      else       while ('0'<=*ch && *ch<='9') { e--; ch++; }  // lose precision but retain scale
      if (*ch=='E' || *ch=='e') {
        ch++;
        int esign=1;
        if (*ch=='-' || *ch=='+') esign -= 2*(*ch++=='-');
        int eacc = 0;
        while ('0'<=*ch && *ch<='9' && eacc<(INT32_MAX-10)/10) {
          eacc *= 10;
          eacc += *ch-'0';
          ch++;
        }
        e += esign * eacc;
      }
      d = (unsigned)(e + 350) <= 700 ? (double)(sign * (long double)acc * pow10lookup[350+e])
          : e < -350 ? 0 : sign * INFD;
    }
    if (quoted) { if (*ch!=quote) return 1; else ch++; }
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
    if (*ch==quote) { quoted=true; ch++; }
    if (quoted && *ch==quote) { ch++; if (on_sep(&ch)) {*this=ch; return 0;} else return 1; }  // empty quoted field ',"",'
    _Bool logical01 = false;  // expose to user and should default be true?
    if ( ((*ch=='0' || *ch=='1') && logical01) || (*ch=='N' && ch[1]=='A' && ch++)) {
        *target = (*ch=='1' ? true : (*ch=='0' ? false : NA_BOOL8));
        ch++;
    } else if (*ch=='T' || *ch == 't') {
        *target = 1;
        if ((ch[1]=='R' && ch[2]=='U' && ch[3]=='E') ||
            (ch[1]=='r' && ch[2]=='u' && ch[3]=='e')) ch += 4;
    } else if (*ch=='F' || *ch == 'f') {
        *target = 0;
        if ((ch[1] == 'A' && ch[2] == 'L' && ch[3] == 'S' && ch[4] == 'E') ||
            (ch[1] == 'a' && ch[2] == 'l' && ch[3] == 's' && ch[4] == 'e')) ch += 5;
    }
    if (quoted) { if (*ch!=quote) return 1; else ch++; }
    if (on_sep(&ch)) { *this=ch; return 0; }
    *target = NA_BOOL8;
    next_sep(&ch);
    *this=ch;
    return !is_NAstring(start);
}


typedef int (*reader_fun_t)(const char **ptr, void *target);
static reader_fun_t fun[NUMTYPE] = {
  (reader_fun_t) &parse_string,   // CT_DROP
  (reader_fun_t) &StrtoB,
  (reader_fun_t) &StrtoI32_bare,
  (reader_fun_t) &StrtoI32_full,
  (reader_fun_t) &StrtoI64,
  (reader_fun_t) &StrtoD,
  (reader_fun_t) &parse_string
};



//=================================================================================================
//
// Main fread() function that does all the job of reading a text/csv file.
//
// Returns 1 if it finishes successfully, and 0 otherwise.
//
//=================================================================================================
int freadMain(freadMainArgs _args)
{
    args = _args;  // assign to global for use by DTPRINT() in other functions
    double t0 = wallclock();

    //*********************************************************************************************
    // [1] Extract the arguments and check their validity
    //*********************************************************************************************
    _Bool verbose = args.verbose;
    _Bool warningsAreErrors = args.warningsAreErrors;
    if (verbose) DTPRINT("[1] Check arguments\n");

    if (mmp || colNames || oldType || lineCopy || type || size) {
      STOP("Internal error: Previous fread() session was not cleaned up properly");
    }

    int nth = (args.nth <= 0 || args.nth > omp_get_max_threads())
              ? omp_get_max_threads() : args.nth;
    if (verbose) {
      DTPRINT("  Num threads = %d %s\n", nth,
              nth < args.nth? "" : "(limited by omp_get_max_threads())");
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
        while (*s++) DTPRINT(*s? "\"%s\", " : "\"%s\"", s[-1]);
        DTPRINT("]\n");
        DTPRINT("  %s of the NAstrings look like numbers.\n",
                any_number_like_NAstrings ? "One or more" : "None");
      }
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
    // Additionally, we will sometimes need to switch to a different parsing
    // context in order to accommodate for the lack of newline on the last line
    // of file.
    const char *sof = NULL;
    const char *eof = NULL;
    // Convenience variable for iteration over the file.
    const char *ch = NULL, *end = NULL;


    //*********************************************************************************************
    // [2] Open and memory-map the input file, setting up the parsing context
    //     (sof, eof, ch).
    //*********************************************************************************************
    if (verbose) DTPRINT("[2] Opening the file\n");
    mmp = NULL;
    if (args.input) {
        sof = args.input;
        fileSize = strlen(sof);
        if (verbose) {
          eol = '\n';  // just a guess, so that STRLIM below work correctly
          DTPRINT("  Input is passed as raw text, starting <<%.*s>>\n",
                  STRLIM(sof, 20, sof+fileSize), sof);
        }
    } else
    if (args.filename) {
        if (verbose) DTPRINT("  Opening file %s\n", args.filename);
        const char *fnam = args.filename;
#ifndef WIN32
        int fd = open(fnam, O_RDONLY);
        if (fd==-1) STOP("file not found: %s",fnam);
        struct stat stat_buf;
        if (fstat(fd,&stat_buf) == -1) {close(fd); STOP("Opened file ok but couldn't obtain its size: %s", fnam);}
        fileSize = (size_t) stat_buf.st_size;
        if (fileSize == 0) {close(fd); STOP("File is empty: %s", fnam);}
        if (verbose) {
            DTPRINT("  File opened, size %.6f GB.\n", 1.0*fileSize/(1024*1024*1024));
            DTPRINT("  Memory mapping ... ");
        }

        // No MAP_POPULATE for faster nrows=10 and to make possible earlier progress bar in row count stage
        // Mac doesn't appear to support MAP_POPULATE anyway (failed on CRAN when I tried).
        // TO DO?: MAP_HUGETLB for Linux but seems to need admin to setup first. My Hugepagesize is 2MB (>>2KB, so promising)
        //         https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
        mmp = mmap(NULL, fileSize, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);  // COW pages. +1 for '\0' we'll add
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
            DTPRINT("  File opened, size %.6f GB.\n", (double)fileSize/(1024*1024*1024));
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
        if (verbose) DTPRINT("ok\n");  // to end 'Memory mapping ... '
    } else {
        STOP("Neither `input` nor `filename` are given, nothing to read.");
    }
    eof = sof + fileSize;
    double tMap = wallclock();


    //*********************************************************************************************
    // [3] Check whether the file contains BOM (Byte Order Mark), and if yes
    //     strip it, modifying `sof`. Also, presence of BOM allows us to
    //     reliably detect the file's encoding.
    //     See: https://en.wikipedia.org/wiki/Byte_order_mark
    //     See: issues #1087 and #1465
    //*********************************************************************************************
    if (verbose) DTPRINT("[3] Detect and skip BOM\n");
    if (fileSize >= 3 && memcmp(sof, "\xEF\xBB\xBF", 3) == 0) {
      sof += 3;
      // ienc = CE_UTF8;
      if (args.verbose) DTPRINT("  UTF-8 byte order mark EF BB BF found at the start of the file and skipped.\n");
    }
    else if (fileSize >= 4 && memcmp(sof, "\x84\x31\x95\x33", 4) == 0) {
      sof += 4;
      // ienc = CE_GB18030;
      if (args.verbose) DTPRINT("  GB-18030 byte order mark 84 31 95 33 found at the start of the file and skipped.\n");
      DTWARN("GB-18030 encoding detected, however fread() is unable to decode it. Some character fields may be garbled.\n");
    }
    else if (fileSize >= 2 && sof[0] + sof[1] == '\xFE' + '\xFF') {  // either 0xFE 0xFF or 0xFF 0xFE
      STOP("File is encoded in UTF-16, this encoding is not supported by fread(). Please recode the file to UTF-8.");
    }


    //*********************************************************************************************
    // [4] Auto detect end-of-line character(s)
    //
    //     This section initializes variables `eol`, `eol2` and `eolLen`. If
    //     `eolLen` is 1, then we set both `eol` and `eol2` to the same
    //     character, otherwise `eolLen` is 2 and we set `eol` and `eol2` to the
    //     first and the second line-separator characters respectively.
    //*********************************************************************************************
    if (verbose) DTPRINT("[4] Detect end-of-line character(s)\n");

    // Scan the file until the first newline character is found. By the end of
    // this loop `ch` will be pointing to such a character, or to eof if there
    // are no newlines in the file.
    ch = sof;
    while (ch<eof && *ch!='\n' && *ch!='\r') {
      char c = *ch++;
      // Skip quoted fields, because they may contain different types of
      // newlines than in the rest of the file.
      if (c == quote) {
        const char *ch0 = ch;
        int nn = 0;
        while (ch < eof && *ch != quote && nn < 10) {
          nn += (*ch == '\n') || (*ch == '\r');
          ch++;
        }
        if (*ch == quote) {
          // Skip over the closing quote
          ch++;
        } else {
          // If we skipped till the end of the file within a quoted field, or
          // if there were too many lines, then maybe it's not really a quoted
          // field at all -- jump back to the beginning of the field and re-scan
          // as if it wasn't quoted.
          ch = ch0;
        }
      }
    }

    // No newlines were found
    if (ch == eof) {
      if (verbose) {
        DTPRINT("  Input ends before any \\r or \\n observed. It will be "
                "treated as a single row and copied to temporary buffer.\n");
      }
      eol = eol2 = '\n';
      eolLen = 1;
      // Make copy of the input, so that it can have a proper eol character at the end.
      size_t sz = (size_t)(eof - sof + eolLen);
      lineCopy = (char*) malloc(sz);
      if (!lineCopy) STOP("Unable to allocate %zd bytes for temporary copy of the input", sz);
      memcpy(lineCopy, sof, sz - 1);
      lineCopy[sz - 1] = '\n';
      sof = lineCopy;
      eof = lineCopy + sz;
    }
    // Otherwise `ch` is pointing to the first newline character encountered
    else {
      eol = eol2 = *ch;
      eolLen = 1;
      if (eol=='\r') {
          if (ch+1<eof && *(ch+1)=='\n') {
              if (verbose) DTPRINT("  Detected eol as \\r\\n (CRLF) in that order, the Windows standard.\n");
              eol2='\n'; eolLen=2;
          } else {
              if (ch+1<eof && *(ch+1)=='\r')
                  STOP("Line ending is \\r\\r\\n. R's download.file() appears to add the extra \\r in text mode on Windows. Please download again in binary mode (mode='wb') which might be faster too. Alternatively, pass the URL directly to fread and it will download the file in binary mode for you.");
                  // NB: on Windows, download.file from file: seems to condense \r\r too. So
              if (verbose) DTPRINT("Detected eol as \\r only (no \\n or \\r afterwards). An old Mac 9 standard, discontinued in 2002 according to Wikipedia.\n");
          }
      } else {
          if (ch+1<eof && *(ch+1)=='\r') {
              DTWARN("Detected eol as \\n\\r, a highly unusual line ending. According to Wikipedia the Acorn BBC used this. If it is intended that the first column on the next row is a character column where the first character of the field value is \\r (why?) then the first column should start with a quote (i.e. 'protected'). Proceeding with attempt to read the file.\n");
              eol2='\r'; eolLen=2;
          } else if (verbose) DTPRINT("  Detected eol as \\n only (no \\r afterwards), the UNIX and Mac standard.\n");
      }
    }


    //*********************************************************************************************
    // [5] Temporarily hide the last line if it doesn't end with a newline.
    //
    //     Ensure file ends with eol so we don't need to check 'ch < eof &&
    //     *ch...' everywhere in deep loops just because the very last field in
    //     the file might finish abrubtly with no final \n.
    //
    //     If we do see that the file has no trailing newline, then we create
    //     the "hidden line" context by copying the last line of the file into a
    //     separate buffer (with newline characters appended at the end). We
    //     will need to be careful to switch to this context when appropriate.
    //
    //     After this section, it is guaranteed that `eof[-eolLen] == eol` and
    //     `eoh` is either NULL or `eoh[-eolLen] == eol`. On the other hand, all
    //     subsequent sections will have to take into account the fact that the
    //     input may have been split into 2 parts.
    //*********************************************************************************************
    if (verbose) DTPRINT("[5] Check for missing newline at the end of input\n");

    // "Hidden line" context: start-of-hidden section and end-of-hidden section.
    const char *soh = NULL;
    const char *eoh = NULL;

    _Bool trailing_newline_added = false;
    if (!(eof[-eolLen] == eol && eof[-1] == eol2)) {
      const char *oldeof = eof;
      while (eof[-eolLen] != eol || eof[-1] != eol2) eof--;
      size_t sz = (size_t)(oldeof - eof + eolLen);
      size_t sz0 = sz - (size_t)eolLen;
      lineCopy = (char*) malloc(sz);
      if (!lineCopy) STOP("Unable to allocate %zd bytes for a temporary buffer", sz);
      memcpy(lineCopy, eof, sz0);
      lineCopy[sz - 1] = eol2;
      lineCopy[sz0] = eol;
      soh = lineCopy;
      eoh = lineCopy + sz;
      if (verbose)
        DTPRINT("  Last character in the file is not a newline, so EOF is "
                "temporarily moved %zd bytes backwards\n", sz0);
      ASSERT(eoh[-eolLen] == eol && eoh[-1] == eol2);
      ASSERT(*eof == *soh && oldeof[-1] == eoh[-eolLen-1]);
      trailing_newline_added = true;
    }
    ASSERT(eof[-eolLen] == eol && eof[-1] == eol2);


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

    if (args.skipString) {
      // TODO: unsafe! there might be no \0 at the end of the file
      ch = strstr(sof, args.skipString);
      if (!ch) {
        STOP("skip='%s' not found in input (it is case sensitive and literal; "
             "i.e., no patterns, wildcards or regexps)", args.skipString);
      }
      // Move to beginning of line. We ignore complications arising from
      // possibility to end up inside a quoted field. Presumably, if the user
      // supplied explicit option `skipString` he knows what he is doing.
      if (soh && ch >= eof) {
        // The `skipString` was found within the last "hidden" line of the file.
        // This means the entire first section of the file can be skipped, and
        // only the "hidden" section remains. Thus we "unhide" it replacing the
        // "main" section.
        advance_sof_to(soh, &sof, &eof, &soh, &eoh);
        if (verbose) {
          DTPRINT("  Found skip='%s' on the last line of the input. Skipping all "
                  "lines but the last", args.skipString);
        }
      } else {
        // Scan backwards to find the beginning of the current line
        while (ch > sof && ch[-1] != eol2) ch--;
        const char *start = ch;
        ch = sof;
        while (ch < start) {
          line += (*ch++ == eol && (eolLen == 1 || *ch++ == eol2));
        }
        if (verbose) {
          DTPRINT("  Found skip='%s' on line %d. The file will be scanned from "
                  "that line onwards.\n", args.skipString, line);
        }
        advance_sof_to(start, &sof, &eof, &soh, &eoh);
      }
    } else

    // Skip the first `skipNrow` lines of input.
    if (args.skipNrow) {
      ch = sof; end = eof;
      while ((ch < end || (soh && (end != eoh) && (ch = soh) && (end = eoh)))
             && line <= args.skipNrow)
      {
        line += (*ch++ == eol && (eolLen == 1 || *ch++ == eol2));
      }
      if (line > args.skipNrow) {
        advance_sof_to(ch, &sof, &eof, &soh, &eoh);
        if (verbose) DTPRINT("  Skipped %d line(s) of input.\n", line);
      } else {
        STOP("skip=%d but the input has only %d line(s)\n", args.skipNrow, line-1);
      }
    }

    // Additionally, skip any blank lines at the start
    const char *lineStart = sof;
    ch = sof; end = eof;
    while ((ch < end || (soh && (end != eoh) && (ch = soh) && (end = eoh)))
           && isspace(*ch))  // isspace() matches ' ', \t, \n and \r
    {
      if (*ch++ == eol && (eolLen == 1 || *ch++ == eol2)) {
        lineStart = ch;
        line++;
      }
    }
    if (ch >= end) {
      if (args.skipNrow || args.skipString)
        STOP("All input has been skipped: the remainder of the file has nothing but whitespace.\n");
      else
        STOP("Input is empty or contains only Whitespace.\n");
    }
    if (verbose) {
      if (lineStart != sof) {
        DTPRINT("  Moved forward to first non-blank line (%d)\n", line);
      }
      DTPRINT("  Positioned on line %d starting: <<%.*s>>\n",
              line, STRLIM(lineStart, 30, eof), lineStart);
    }
    advance_sof_to(lineStart, &sof, &eof, &soh, &eoh);


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
      antisep = (sep == ' ')? '\t' : (sep == '\t')? ' ' : 0;
      for (quoteRule=0; quoteRule<4; quoteRule++) {  // quote rule in order of preference
        // if (verbose) DTPRINT("  Trying sep='%c' with quoteRule %d ...\n", sep, quoteRule);
        // DTPRINT("  sof=%p, eof=%p, soh=%p, eoh=%p\n", sof, eof, soh, eoh);
        for (int i=0; i<=JUMPLINES; i++) { numFields[i]=0; numLines[i]=0; } // clear VLAs
        int i=-1; // The slot we're counting the currently contiguous consistent ncol
        int thisLine=0, lastncol=-1;
        ch = sof; end = eof;
        while ((ch < end || (soh && (end != eoh) && (ch = soh) && (end = eoh)))
               && thisLine++ < JUMPLINES)
        {
          // Compute num columns and move `ch` to the start of next line
          // DTPRINT("    ch=%p end=%p -> ", ch, end);
          int thisncol = countfields(&ch, &end, soh, eoh);
          // DTPRINT("ncol=%d  (ch=%p, end=%p)\n", thisncol, ch, end);
          if (thisncol < 0) {
            // invalid file with this sep and quote rule; abort
            numFields[0] = -1;
            break;
          }
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
    // the size in bytes of the first JUMPLINES from the start (jump point 0)
    size_t jump0size = (sof <= firstJumpEnd && firstJumpEnd <= eof)
                        ? (size_t)(firstJumpEnd - sof)
                        : (size_t)(eof - sof) + (size_t)(firstJumpEnd - soh);
    // DTPRINT("sof=%p eof=%p soh=%p eoh=%p fjend=%p\n", sof, eof, soh, eoh, firstJumpEnd);
    ASSERT(jump0size >= 0 && jump0size <= fileSize + (size_t)eolLen);
    quoteRule = topQuoteRule;
    sep = topSep;
    antisep = (sep == ' ')? '\t' : (sep == '\t')? ' ' : 0;

    // Find the first line with the consistent number of fields.  There might
    // be irregular header lines above it.
    int ncol;
    // Save the `sof` pointer before moving it. We might need to come back to it
    // later when reporting an error in next section.
    const char *headerPtr = sof;
    if (fill) {
      // start input from first populated line; do not alter sof.
      ncol = topNmax;
    } else {
      ncol = topNumFields;
      int thisLine = -1;
      ch = sof; end = eof;
      while ((ch < eof || (soh && (end != eoh) && (end=eoh) && (ch=soh)))
             && ++thisLine < JUMPLINES)
      {
        const char *ch2 = ch;   // lineStart
        int cols = countfields(&ch, &end, soh, eoh);  // advances ch to next line
        if (cols == ncol) {
          advance_sof_to(ch2, &sof, &eof, &soh, &eoh);
          line += thisLine;
          break;
        }
      }
    }
    // For standard regular separated files, we're now on the first byte of the file.

    ASSERT(ncol >= 1 && line >= 1);
    ch = sof;
    end = eof;
    int tt = countfields(&ch, &end, soh, eoh);
    if (verbose) {
      DTPRINT("  Detected %d columns on line %d. This line is either column "
              "names or first data row. Line starts as: <<%.*s>>\n",
               tt, line, STRLIM(sof, 30, eof), sof);
      DTPRINT("  Quote rule picked = %d\n", quoteRule);
      if (fill) DTPRINT("  fill=true and the most number of columns found is %d\n", ncol);
    }
    ASSERT(fill || tt == ncol);


    //*********************************************************************************************
    // [8] Detect and assign column names (if present)
    //
    //     This section also moves the `sof` pointer to point at the first row
    //     of data ("removing" the column names).
    //*********************************************************************************************
    if (verbose) DTPRINT("[8] Determine column names\n");
    // throw-away storage for processors to write to in this preamble.
    // Saves deep 'if (target)' inside processors.
    double trash_val;
    double *trash_dbl = &trash_val;
    void *trash_any = (void*) &trash_val;
    lenOff *trash_lenoff = (lenOff*) &trash_val;

    const char *colNamesAnchor = sof;
    colNames = calloc((size_t)ncol, sizeof(lenOff));
    if (!colNames) STOP("Unable to allocate %d*%d bytes for column name pointers: %s", ncol, sizeof(lenOff), strerror(errno));
    _Bool allchar=true;
    ch = sof; // move back to start of line since countfields() moved to next
    if (sep==' ') {
      while (*ch==' ') ch++;
    }
    ch--;  // so we can ++ at the beginning inside loop.
    for (int field=0; field<tt; field++) {
      const char *this = ++ch;
      // DTPRINT("Field %d <<%.*s>>\n", field, STRLIM(ch, 20, eof), ch);
      skip_white(&ch);
      if (allchar && !on_sep(&ch) && !StrtoD(&ch,trash_dbl)) allchar=false;  // don't stop early as we want to check all columns to eol here
      // considered looking for one isalpha present but we want 1E9 to be considered a value not a column name
      ch = this;  // rewind to the start of this field
      Field(&ch, trash_lenoff);  // StrtoD does not consume quoted fields according to the quote rule, so redo with Field()
      // countfields() above already validated the line so no need to check again now.
    }
    if (*ch != eol)
      STOP("Read %d expected fields in the header row (fill=%d) but finished on <<%.*s>>'",tt,fill,STRLIM(ch,30,eof),ch);
    // already checked above that tt==ncol unless fill=TRUE
    // when fill=TRUE and column names shorter (test 1635.2), leave calloc initialized lenOff.len==0
    if (verbose && args.header!=NA_BOOL8) DTPRINT("  'header' changed by user from 'auto' to %s\n", args.header?"true":"false");
    if (args.header==false || (args.header==NA_BOOL8 && !allchar)) {
        if (verbose && args.header==NA_BOOL8) DTPRINT("  Some fields on line %d are not type character. Treating as a data row and using default column names.\n", line);
        // colNames was calloc'd so nothing to do; all len=off=0 already
        ch = sof;  // back to start of first row. Treat as first data row, no column names present.
        end = eof;
        // now check previous line which is being discarded and give helpful msg to user ...
        if (ch>headerPtr && args.skipNrow==0) {
          ch -= (eolLen+1);
          if (ch<headerPtr) ch=headerPtr;  // for when headerPtr[0]=='\n'
          while (ch>headerPtr && *ch!=eol2) ch--;
          if (ch>headerPtr) ch++;
          const char *prevStart = ch;
          int tmp = countfields(&ch, &end, soh, eoh);
          if (tmp==ncol) STOP("Internal error: row before first data row has the same number of fields but we're not using it.");
          if (tmp>1) DTWARN("Starting data input on line %d <<%.*s>> with %d fields and discarding line %d <<%.*s>> before it because it has a different number of fields (%d).", line, STRLIM(sof, 30, eof), sof, ncol, line-1, STRLIM(prevStart, 30, eof), prevStart, tmp);
        }
        if (ch!=sof) STOP("Internal error. ch!=sof after prevBlank check");
    } else {
        if (verbose && args.header==NA_BOOL8) {
          DTPRINT("  All the fields on line %d are character fields. Treating as the column names.\n", line);
        }
        ch = sof;
        line++;
        if (sep==' ') {
          while (*ch==' ') ch++;
        }
        ch--;
        for (int i=0; i<ncol; i++) {
            // Use Field() here as it's already designed to handle quotes, leading space etc.
            const char *start = ++ch;
            Field(&ch, colNames+i);  // stores the string length and offset as <uint,uint> in colnames[i]
            colNames[i].off += (size_t)(start-colNamesAnchor);
            if (*ch==eol) break;   // already checked number of fields previously above
        }
        if (*ch != eol) STOP("Internal error: reading colnames did not end on eol");
        advance_sof_to(ch + eolLen, &sof, &eof, &soh, &eoh);
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
      type[j] = CT_BOOL8;
      size[j] = typeSize[CT_BOOL8];
    }

    int nJumps = 0;
    // how many places in the file to jump to and test types there (the very end is added as 11th or 101th)
    // not too many though so as not to slow down wide files; e.g. 10,000 columns.  But for such large files (50GB) it is
    // worth spending a few extra seconds sampling 10,000 rows to decrease a chance of costly reread even further.
    size_t sz = (size_t)(eof - sof + (eoh ? eoh - soh : 0));
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
    const char *lastRowEnd = sof;
    for (int j=0; j<nJumps; j++) {
        ch = (j == 0) ? sof :
             (j == nJumps-1) ? eof - (size_t)(0.5*jump0size) :
                               sof + (size_t)j*(sz/(size_t)(nJumps-1));
        end = eof;
        if (j>0 && !nextGoodLine(&ch, ncol, end))
          STOP("Could not find first good line start after jump point %d when sampling.", j);
        _Bool bumped = 0;  // did this jump find any different types; to reduce verbose output to relevant lines
        int jline = 0;  // line from this jump point
        while((ch < end || (soh && (end != eoh) && (end=eoh) && (ch=soh))) &&
              (jline<JUMPLINES || j==nJumps-1)) {  // nJumps==1 implies sample all of input to eof; last jump to eof too
          const char *jlineStart = ch;
          if (sep==' ') {
            while (*ch == ' ') ch++;  // multiple sep=' ' at the jlineStart does not mean sep(!)
          }
          // detect blank lines
          skip_white(&ch);
          if (*ch == eol) {
            if (!skipEmptyLines && !fill) break;
            jlineStart = ch;  // to avoid 'Line finished early' below and get to the sampleLines++ block at the end of this while
          }
          jline++;
          // DTPRINT("  Line %d: <<%.*s>>  (ch=%p)\n", jline, STRLIM(ch,80,end), ch, (const void*)ch);
          int field=0;
          const char *fieldStart = ch;  // Needed outside loop for error messages below
          while (*ch != eol && field < ncol) {
            // DTPRINT("<<%.*s>>(%d)", STRLIM(ch,20,end), ch, quoteRule);
            fieldStart=ch;
            int res;
            while (type[field]<=CT_STRING && (res = fun[type[field]](&ch, trash_any))) {
              // DTPRINT("=");
              int neols = 0;
              while (res == 2 && neols++ < 100) {
                // DTPRINT("~(%p)", ch);
                if (ch == end) {
                  if (eoh && end != eoh) { ch = soh; end = eoh; }
                  else { res = 1; break; }
                }
                res = parse_string_continue(&ch, trash_any);
              }
              if (res == 0) break;
              ch = fieldStart;
              if (type[field] < CT_STRING) {
                type[field]++;
                bumped = true;
              } else {
                // the field could not be read with this quote rule, try again with next one
                // Trying the next rule will only be successful if the number of fields is consistent with it
                ASSERT(quoteRule < 3);
                if (verbose)
                  DTPRINT("Bumping quote rule from %d to %d due to field %d on line %d of sampling jump %d starting <<%.*s>>\n",
                           quoteRule, quoteRule+1, field+1, jline, j, STRLIM(fieldStart, 200, end), fieldStart);
                quoteRule++;
                bumped=true;
                ch = jlineStart;  // Try whole line again, in case it's a hangover from previous field
                field=0;
                continue;
              }
            }
            // DTPRINT("%d  (ch = %p)\n", type[field], ch);
            if (*ch == eol && ch[eolLen-1] == eol2) {
              break;
            } else {
              // skip over the field separator
              if (*ch != sep) STOP("Unexpected: field ended without a separator present: <<%.*s>>",
                                   STRLIM(fieldStart,30,end),fieldStart);
              ch++;
              field++;
            }
          }
          if (field<ncol-1 && !fill) {
              if (ch<end && *ch!=eol) {
                  STOP("Internal error: line has finished early but not on an eol or eof (fill=false). Please report as bug.");
              } else if (ch>jlineStart) {
                  STOP("Line %d has too few fields when detecting types. Use fill=TRUE to pad with NA. Expecting %d fields but found %d: <<%.*s>>", jline, ncol, field+1, STRLIM(jlineStart,200,end), jlineStart);
              }
          }
          ASSERT(ch < end);
          if (*ch!=eol || field>=ncol) {   // the || >=ncol is for when a comma ends the line with eol straight after
            if (field!=ncol) STOP("Internal error: Line has too many fields but field(%d)!=ncol(%d)", field, ncol);
            STOP("Line %d from sampling jump %d starting <<%.*s>> has more than the expected %d fields. " \
                 "Separator %d occurs at position %d which is character %d of the last field: <<%.*s>>. " \
                 "Consider setting 'comment.char=' if there is a trailing comment to be ignored.",
                jline, j, STRLIM(jlineStart,10,end), jlineStart, ncol, ncol, (int)(ch-jlineStart), (int)(ch-fieldStart),
                STRLIM(fieldStart,200,end), fieldStart);
          }
          // if very last field was quoted, check if it was completed with an ending quote ok.
          // not necessarily a problem (especially if we detected no quoting), but we test it and nice to have
          // a warning regardless of quoting rule just in case file has been inadvertently truncated.
          // The warning is only issued if the file didn't have the newline on the last line.
          // This warning is early at type skipping around stage before reading starts, so user can cancel early
          if (type[ncol-1]==CT_STRING && *fieldStart==quote && *(ch-1)!=quote && trailing_newline_added) {
            if (quoteRule<2) STOP("Internal error: Last field of last line should select quote rule 2");
            DTWARN("Last field of last line starts with a quote but is not finished with a quote before end of file: <<%.*s>>",
                    STRLIM(fieldStart, 200, end), fieldStart);
          }
          ch += eolLen;
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
    while ((ch < end || (soh && (end != eoh) && (end=eoh) && (ch=soh)))
           && isspace(*ch)) ch++;
    if (ch < end) {
      DTWARN("Found the last consistent line but text exists afterwards (discarded): <<%.*s>>", STRLIM(ch,200,end), ch);
    }
    retreat_eof_to(lastRowEnd, &sof, &eof, &soh, &eoh);

    size_t estnrow=1, allocnrow=1;
    size_t bytesRead = (size_t)(eof - sof) + (size_t)(eoh? eoh - soh : 0);
    double meanLineLen=0;
    if (sampleLines == 0) {
      if (verbose) DTPRINT("  sampleLines=0: only column names are present\n");
    } else {
      meanLineLen = (double)sumLen/sampleLines;
      estnrow = CEIL(bytesRead/meanLineLen);  // only used for progress meter and verbose line below
      double sd = sqrt( (sumLenSq - (sumLen*sumLen)/sampleLines)/(sampleLines-1) );
      allocnrow = clamp_szt((size_t)(bytesRead / fmax(meanLineLen - 2*sd, minLen)),
                            (size_t)(1.1*estnrow), 2*estnrow);
      // sd can be very close to 0.0 sometimes, so apply a +10% minimum
      // blank lines have length 1 so for fill=true apply a +100% maximum. It'll be grown if needed.
      if (verbose) {
        DTPRINT("  =====\n");
        DTPRINT("  Sampled %zd rows (handled \\n inside quoted fields) at %d jump point(s)\n", sampleLines, nJumps);
        DTPRINT("  Bytes from first data row on line %d to the end of last row: %zd\n", row1Line, bytesRead);
        DTPRINT("  Line length: mean=%.2f sd=%.2f min=%d max=%d\n", meanLineLen, sd, minLen, maxLen);
        DTPRINT("  Estimated nrow: %zd / %.2f = %zd\n", bytesRead, meanLineLen, estnrow);
        DTPRINT("  Initial alloc = %zd rows (%zd + %d%%) using bytes/max(mean-2*sd,min) clamped between [1.1*estn, 2.0*estn]\n",
                 allocnrow, estnrow, (int)(100.0*allocnrow/estnrow-100.0));
      }
      if (nJumps==1) {
        estnrow = allocnrow = sampleLines + (soh != NULL);
        if (verbose) DTPRINT("  All rows were sampled since file is small so we know nrow=%zd exactly\n", estnrow);
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
    ch = sof;
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
      DTPRINT("  After %d type and %d drop user overrides : ", nUserBumped, ndrop);
      printTypes(ncol); DTPRINT("\n");
    }
    double tColType = wallclock();


    //*********************************************************************************************
    // [11]  Allocate the result columns
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
    ch = sof;   // back to start of first data row
    int hasPrinted=0;  // the percentage last printed so it prints every 2% without many calls to wallclock()
    _Bool stopTeam=false, firstTime=true;  // _Bool for MT-safey (cannot ever read half written _Bool value)
    int nTypeBump=0, nTypeBumpCols=0;
    double tRead=0, tReread=0, tTot=0;  // overall timings outside the parallel region
    double thNextGoodLine=0, thRead=0, thPush=0;  // reductions of timings within the parallel region
    char *typeBumpMsg=NULL;  size_t typeBumpMsgSize=0;
    #define stopErrSize 1000
    char stopErr[stopErrSize+1]="";  // must be compile time size: the message is generated and we can't free before STOP
    size_t DTi = 0;   // the current row number in DT that we are writing to
    const char *prevJumpEnd;  // the position after the last line the last thread processed (for checking)
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
    if (verbose) {
      DTPRINT("  njumps=%d and chunkBytes=%zd\n", nJumps, chunkBytes);
    }
    size_t initialBuffRows = allocnrow / (size_t)nJumps;
    if (initialBuffRows > INT32_MAX) STOP("Buffer size %lld is too large\n", initialBuffRows);
    nth = imin(nJumps, nth);

    read:  // we'll return here to reread any columns with out-of-sample type exceptions
    prevJumpEnd = sof;
    #pragma omp parallel num_threads(nth)
    {
      int me = omp_get_thread_num();
      #pragma omp master
      nth = omp_get_num_threads();
      const char *thisJumpStart = NULL;  // The first good start-of-line after the jump point
      size_t myDTi = 0;  // which row in the final DT result I should start writing my chunk to
      size_t myNrow = 0; // the number of rows in my chunk

      // Allocate thread-private row-major myBuffs
      // Do not reuse &trash for myBuff0 as that might create write conflicts
      // between threads, causing slowdown of the process.
      size_t myBuffRows = initialBuffRows;  // Upon realloc, myBuffRows will increase to grown capacity
      void *myBuff8 = malloc(rowSize8 * myBuffRows);
      void *myBuff4 = malloc(rowSize4 * myBuffRows);
      void *myBuff1 = malloc(rowSize1 * myBuffRows);
      void *myBuff0 = malloc(8);  // for CT_DROP columns
      if ((rowSize8 && !myBuff8) ||
          (rowSize4 && !myBuff4) ||
          (rowSize1 && !myBuff1) || !myBuff0) stopTeam = true;

      #pragma omp for ordered schedule(dynamic) reduction(+:thNextGoodLine,thRead,thPush)
      for (int jump=0; jump<nJumps+nth; jump++) {
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
          pushBuffer(myBuff8, myBuff4, myBuff1, /*anchor=*/thisJumpStart,
                     (int)myNrow, myDTi, (int)rowSize8, (int)rowSize4, (int)rowSize1,
                     nStringCols, nNonStringCols);
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
        if (jump>=nJumps || stopTeam) continue;  // nothing left to do. This jump was the dummy extra one.

        const char *tch = sof + (size_t)jump * chunkBytes;
        const char *nextJump = jump<nJumps-1 ? tch+chunkBytes+eolLen : eof;
        // +eolLen is for when nextJump happens to fall exactly on a line start (or on eol2 on Windows). The
        // next thread will start one line later because nextGoodLine() starts by finding next eol
        // Easier to imagine eolLen==1 and tch<=nextJump in the while() below
        if (jump>0 && !nextGoodLine(&tch, ncol, nextJump)) {
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

        const char *fake_anchor = thisJumpStart;
        while (tch<nextJump) {
          if (myNrow == myBuffRows) {
            // buffer full due to unusually short lines in this chunk vs the sample; e.g. #2070
            myBuffRows *= 1.5;
            #pragma omp atomic
            buffGrown++;
            long diff8 = (char*)myBuff8Pos - (char*)myBuff8;
            long diff4 = (char*)myBuff4Pos - (char*)myBuff4;
            long diff1 = (char*)myBuff1Pos - (char*)myBuff1;
            myBuff8 = realloc(myBuff8, rowSize8 * myBuffRows);
            myBuff4 = realloc(myBuff4, rowSize4 * myBuffRows);
            myBuff1 = realloc(myBuff1, rowSize1 * myBuffRows);
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
          if (sep==' ') while (*tch==' ') tch++;  // multiple sep=' ' at the tlineStart does not mean sep(!)
          skip_white(&tch);  // solely for blank lines otherwise could leave to field processors which handle leading white
          if (*tch == eol && tch[eolLen-1] == eol2) {
            if (skipEmptyLines) {
              tch += eolLen;
              continue;
            }
            else if (!fill) {
              #pragma omp critical
              if (!stopTeam) {
                stopTeam = true;
                snprintf(stopErr, stopErrSize,
                  "Row %zd is empty. It is outside the sample rows. "
                  "Set fill=true to treat it as an NA row, or blank.lines.skip=true to skip it",
                  myDTi + myNrow);
                  // TODO - include a few (numbered) lines before and after in the message.
              }
              break;
            }
          }

          int j = 0;
          _Bool at_line_end = false; // set to true if the loop ends at a line end
          while (j < ncol) {
            // DTPRINT("Field %d: '%.10s' as type %d  (tch=%p)\n", j+1, tch, type[j], tch);
            const char *fieldStart = tch;
            int8_t joldType = type[j];   // fetch shared type once. Cannot read half-written byte.
            int8_t thisType = joldType;  // to know if it was bumped in (rare) out-of-sample type exceptions
            int8_t absType = (int8_t) abs(thisType);

            // always write to buffPos even when CT_DROP. It'll just overwrite on next non-CT_DROP
            while (absType < NUMTYPE) {
              // normally returns success=1, and myBuffPos is assigned inside *fun.
              void *target = thisType > 0? *(allBuffPos[size[j]]) : myBuff0;
              int ret = fun[absType](&tch, target);
              if (ret == 0) break;
              while (ret == 2) {
                // DTPRINT("...cont: tch=%p, end=%p\n");
                if (tch == eof) {
                  if (eoh) { tch = soh; nextJump = eoh; }
                  else break;
                }
                ret = parse_string_continue(&tch, target);
              }
              if (ret == 0) break;
              // guess is insufficient out-of-sample, type is changed to negative sign and then bumped. Continue to
              // check that the new type is sufficient for the rest of the column to be sure a single re-read will work.
              absType++;
              thisType = -absType;
              tch = fieldStart;
            }

            if (joldType == CT_STRING) {
              ((lenOff*) myBuff8Pos)->off += (size_t)(fieldStart - fake_anchor);
            } else if (thisType != joldType) {  // rare out-of-sample type exception
              #pragma omp critical
              {
                joldType = type[j];  // fetch shared value again in case another thread bumped it while I was waiting.
                // Can't PRINT because we're likely not master. So accumulate message and print afterwards.
                if (thisType < joldType) {   // thisType<0 (type-exception)
                  char temp[1001];
                  int len = snprintf(temp, 1000,
                    "Column %d (\"%.*s\") bumped from '%s' to '%s' due to <<%.*s>> on row %zd\n",
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
            *((char**) allBuffPos[size[j]]) += size[j];
            j++;
            if (*tch==eol && tch[eolLen-1] == eol2) {
              tch += eolLen;
              if (tch == eof && soh) {
                fake_anchor += soh - tch;
                tch = soh;
                nextJump = eoh;
              }
              at_line_end = true;
              break;
            }
            tch++;
          }

          if (j < ncol)  {
            // not enough columns observed
            if (!fill) {
              #pragma omp critical
              if (!stopTeam) {
                stopTeam = true;
                snprintf(stopErr, stopErrSize,
                  "Expecting %d cols but row %zd contains only %d cols (sep='%c'). " \
                  "Consider fill=true. <<%.*s>>",
                  ncol, myDTi, j, sep, STRLIM(tlineStart, 500, nextJump), tlineStart);
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
          if (!at_line_end) {
            #pragma omp critical
            if (!stopTeam) {
              // DTPRINT("tch=%p (=lineStart+%d), *tch=%02x\n", tch, tch-tlineStart, *tch);
              stopTeam = true;
              snprintf(stopErr, stopErrSize,
                "Too many fields on out-of-sample row %zd. Read all %d expected columns but more are present. <<%.*s>>",
                myDTi, ncol, STRLIM(tlineStart, 500, nextJump), tlineStart);
            }
            break;
          }
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
              jump-1, jump, (const void*)prevJumpEnd, STRLIM(prevJumpEnd,50,nextJump), prevJumpEnd,
              (int)(thisJumpStart-prevJumpEnd), STRLIM(thisJumpStart,50,nextJump), thisJumpStart);
            stopTeam=true;
          }
          myDTi = DTi;  // fetch shared DTi (where to write my results to the answer). The previous thread just told me.
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
        }
        // END ORDERED.
        // Next thread can now start its ordered section and write its results to the final DT at the same time as me.
        // Ordered has to be last in some OpenMP implementations currently. Logically though, pushBuffer happens now.
      }
      // Each thread to free its own buffer.
      free(myBuff8); myBuff8 = NULL;
      free(myBuff4); myBuff4 = NULL;
      free(myBuff1); myBuff1 = NULL;
      free(myBuff0); myBuff0 = NULL;
    }
    // end parallel
    if (firstTime) {
      tReread = tRead = wallclock();
      tTot = tRead-t0;
      if (hasPrinted || verbose) {
        DTPRINT("\rRead %zd rows x %d columns from %.3fGB file in ", DTi, ncol-ndrop, 1.0*fileSize/(1024*1024*1024));
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
    if (firstTime && nTypeBump) {
      rowSize1 = rowSize4 = rowSize8 = 0;
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
          size[j] = typeSize[newType];
          rowSize1 += (size[j] & 1);
          rowSize4 += (size[j] & 4);
          rowSize8 += (size[j] & 8);
          if (type[j] == CT_STRING) nStringCols++; else nNonStringCols++;
        } else if (type[j]>=1) {
          // we'll skip over non-bumped columns in the rerun, whilst still incrementing resi (hence not CT_DROP)
          // not -type[i] either because that would reprocess the contents of not-bumped columns wastefully
          type[j] = -CT_STRING;
        }
      }
      // reread from the beginning
      DTi = 0;
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
