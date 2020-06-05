// For translations (#4402) we need positional specifiers (%n$), a non-C99 POSIX extension.
// On Linux and Mac, standard snprintf supports positional specifiers.
// On Windows, we tried many things but just couldn't achieve it. This may be why R uses
// a third party library, trio, on Windows. But R does not expose trio for use by packages.
// So ...
// Rather than require compile flags (such as _XOPEN_SOURCE or POSIX_C_SOURCE), or require
// linking to particular Windows libraries which may be fragile over time depending on
// user's environments, we use the standard C99 features here. To do so, we simulate
// positionals via format massage. Just on Windows, all snprintf calls are replaced with
// this dt_win_snprintf via a #define in data.table.h. The goal of this massage is to be
// as light and minimal as possible.
// In C is it is impossible, portably, to reorder a va_list (sadly).
// In C you must past the correct type to va_arg(), so even to navigate va_list you
// must parse and rely on fmt. But we don't want to reimplement all the types and modifiers.
// Hence, reordering the specifiers, passing the va_list to the library, and then
// putting the output strings into the desired order afterwards.
// NB: must be thread-safe

#include "data.table.h"
#include <stdarg.h>
#undef snprintf // on Windows, just in this file, we do want to use the C library's snprintf

int dt_win_snprintf(char *dest, size_t n, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  const char *ch = strstr(fmt, "%1$");
  if (ch==NULL) {
    // no positionals present, just pass on to the C library vsnprintf as-is
    int ans = vsnprintf(dest, n, fmt, ap);
    va_end(ap);
    return ans;
  }
  // Standards say that if one specifier uses position, they all must. Good.
  // We will not allow repeats though; must be a permutation.
  // As in C, there are few checks; wrong/mismatching positionals will be a crash.
  // This is for messages/errors, so time should not be spent on a fast solution.
  char *buff = (char *)malloc(n);  // not R_alloc as we need to be thread-safe
  if (!buff) error("Unable to allocate %d bytes for buffer in dt_win_snprintf", n);
  int pos=1;
  // Use dest as temp to write the reordered specifiers
  char *ch2=dest;
  #define NDELIM 2
  const char delim[NDELIM+1] = "\x7f\x7f"; // tokenize using 2 DELs
  while (ch!=NULL) {  // ch is resting on start of %pos$ in fmt
    // Find end of %[parameter][flags][width][.precision][length]type
    // https://en.wikipedia.org/wiki/Printf_format_string#Syntax
    const char *start = strchr(ch, '$')+1; // look for $ since pos could be > 9 or potentially > 99
    const char *end = strpbrk(start,"diufFeEgGxXoscpaA"); // last character of specifier
    *ch2++ = '%';
    strncpy(ch2, start, end-start+1); // write the specifer in order without the n$ part
    ch2 += end-start+1;
    strcpy(ch2, delim); // includes '\0'
    ch2 += NDELIM;      // now resting on the '\0'
    char posstr[15];  // 15 to avoid C compiler warnings
    snprintf(posstr, 15, "%%%d$", ++pos);  // snprintf was #undef above, so this is the C library one
    ch = strstr(fmt, posstr);
  }
  int narg = pos-1;
  vsnprintf(buff, n, dest, ap); // dest used as tmp here, holds reordered specifiers same order as arglist
  // All the hard formatting work and va_arg type navigation has now been done by the C library
  // Now we just need to put the string results for each argument back into the desired positions
  // First create lookups so we can loop through fmt once replacing the specifiers as they appear
  const char *arg[narg];
  int len[narg];
  ch = buff;
  for (int i=0; i<narg; ++i) {
    arg[i] = ch;
    const char *end = strstr(ch, delim);
    len[i] = end-ch;
    ch = end+NDELIM;
  }
  ch = fmt;
  ch2 = dest;
  while (*ch!='\0') {
    if (*ch!='%')   {*ch2++=*ch++;      continue; }  // copy non-specifier to the result as-is
    if (ch[1]=='%') {*ch2++='%'; ch+=2; continue; }  // interpret %% as a single %
    if (ch[1]<'1' || ch[1]>'9') error("When positional %n$ is used, all specifiers must include positional");
    int pos = atoi(ch+1);
    ch = strpbrk(ch,"diufFeEgGxXoscpaA")+1; // move to the end of the specifier
    strncpy(ch2, arg[pos-1], len[pos-1]);   // write the result of the appropriate argument
    ch2 += len[pos-1];
  }
  *ch2='\0';
  free(buff);
  va_end(ap);
  return ch2-dest;
}

SEXP test_dt_win_snprintf()
{
  char buff[50];
  dt_win_snprintf(buff, 50, "No pos %d%%%d ok", 42, -84);
  if (strcmp(buff, "No pos 42%-84 ok"))   error("dt_win_snprintf test 1 failed: %s", buff);
  dt_win_snprintf(buff, 50, "With pos %1$d%%%2$d ok", 42, -84);
  if (strcmp(buff, "With pos 42%-84 ok")) error("dt_win_snprintf test 2 failed: %s", buff);
  dt_win_snprintf(buff, 50, "With pos %2$d%%%1$d ok", 42, -84);
  if (strcmp(buff, "With pos -84%42 ok")) error("dt_win_snprintf test 3 failed: %s", buff);
  dt_win_snprintf(buff, 50, "%3$s %1$d %4$10s %2$03d$", -99, 12, "hello%2$d", "short");
  if (strcmp(buff, "hello%2$d -99      short 012$")) error("dt_win_snprintf test 4 failed: %s", buff);
  return R_NilValue;
}

