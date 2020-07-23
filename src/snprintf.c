// For translations (#4402) we need positional specifiers (%n$), a non-C99 POSIX extension.
// On Linux and Mac, standard snprintf supports positional specifiers.
// On Windows, we tried many things but just couldn't achieve linking to _sprintf_p. Even
// if we managed that on AppVeyor we may have fragility in the future on Windows given
// varying Windows versions, compile environments/flags, and dll libraries. This may be
// why R uses a third party library, trio, on Windows. But R does not expose trio for use
// by packages.
// So, in this file we use standard C99 features to support %n$. We simulate
// positionals via format massage. Just on Windows, all snprintf calls are replaced with
// this dt_win_snprintf via a #define in data.table.h. The goal of this massage is to be
// as light and minimal as possible.
// In C it is not possible, portably, to reorder a va_list (sadly).
// In C you must pass the correct type to va_arg(). So even to navigate va_list you
// must parse and rely on fmt. But we don't want to reimplement all the types and modifiers.
// Hence, reordering the specifiers, passing the va_list to the library, and then
// putting the output strings into the desired order afterwards.
// NB: must be thread-safe

#include "data.table.h"
#include <stdarg.h>
#include <ctype.h>  // isdigit
#undef snprintf // on Windows, just in this file, we do want to use the C library's snprintf

int dt_win_snprintf(char *dest, const size_t n, const char *fmt, ...)
{
  if (n<1) return 0;
  va_list ap;
  va_start(ap, fmt);
  const char *strp[99]={NULL};
  int         strl[99]={0};
  int narg=0;
  // are any positional specifiers present?
  // previously used strstr(fmt, "%1$") here but that could match to %%1$ and then
  // what if there's another %1$ as well as the %%1$. Hence a more complicated
  // loop here with more robust checks as well to catch mistakes in fmt
  bool posSpec=false, nonPosSpec=false;
  int specAlloc=0; // total characters of specifiers for alloc
  const char *ch = fmt;
  while (*ch!='\0') {
    if (*ch!='%')   {ch++; continue;}
    if (ch[1]=='%') {ch+=2; continue; }  // %% means literal %
    // Find end of %[parameter][flags][width][.precision][length]type
    // https://en.wikipedia.org/wiki/Printf_format_string#Syntax
    // These letters do not appear in flags or length modifiers, just type
    const char *end = strpbrk(ch,"diufFeEgGxXoscpaA");
    if (!end) {
      // an error() call is not thread-safe; placing error in dest is better than a crash. This way
      // we have a better chance of the user reporting the strange error and we'll see it's a fmt issue
      // in the message itself.
      snprintf(dest, n, "0 %-5s does not end with recognized type letter", ch);
      return -1;
    }
    const char *d = ch+1;
    if (*d=='-') d++; // to give helpful outside-range message for %-1$ too
    while (isdigit(*d)) d++;
    if (*d=='$') {
      posSpec=true;
      int pos = atoi(ch+1);
      if (pos<1 || pos>99) {
        // up to 99 supported here; should not need more than 99 in a message
        snprintf(dest, n, "1 %.*s outside range [1,99]", (int)(d-ch+1), ch);
        return -1;
      }
      if (pos>narg) narg=pos;
      if (strp[pos-1]) {
        // no dups allowed because it's reasonable to not support dups, but this wrapper
        // could not cope with the same argument formatted differently; e.g. "%1$d %1$5d"
        snprintf(dest, n, "2 %%%d$ appears twice", pos);
        return -1;
      }
      strp[pos-1] = strchr(ch, '$')+1;
      strl[pos-1] = end-strp[pos-1]+1;
      specAlloc += strl[pos-1]+1; // +1 for leading '%'
    } else {
      nonPosSpec=true;
    }
    ch = end+1;
  }
  if (posSpec && nonPosSpec) {
    // Standards state that if one specifier uses position, they all must; good.
    snprintf(dest, n, "3 some %%n$ but not all");
    return -1;
  }
  if (!posSpec) {
    // no positionals present, just pass on to the C library vsnprintf as-is
    int ans = vsnprintf(dest, n, fmt, ap);
    va_end(ap);
    return ans;
  }
  #define NDELIM 2
  const char delim[NDELIM+1] = "\x7f\x7f"; // tokenize temporary using 2 DELs
  specAlloc += narg*NDELIM + 1;  // +1 for final '\0'
  char *spec = (char *)malloc(specAlloc);  // not R_alloc as we need to be thread-safe
  if (!spec) {
    // # nocov start
    snprintf(dest, n, "4 %d byte spec alloc failed", (int)specAlloc);
    return -1;
    // # nocov end
  }
  char *ch2 = spec;
  for (int i=0; i<narg; ++i) {
    if (!strp[i] || strl[i]<1) {
      // if %n$ is present, then %[1:n]$ must all be present
      snprintf(dest, n, "5 %%%d$ missing", i+1);
      free(spec);
      return -1;
    }
    *ch2++ = '%';
    strncpy(ch2, strp[i], strl[i]); // write the reordered specifers without the n$ part
    ch2 += strl[i];
    strcpy(ch2, delim); // includes '\0'
    ch2 += NDELIM;      // now resting on the '\0'    
  }
  char *buff = malloc(n); // for the result of the specifiers
  if (!buff) {
    // # nocov start
    snprintf(dest, n, "6 %d byte buff alloc failed", (int)n);
    free(spec);
    return -1;
    // # nocov end
  }
  // now spec contains the specifiers (minus their $n parts) in the same oder as ap
  int res = vsnprintf(buff, n, spec, ap); // C library does all the (non-positional) hard work here
  va_end(ap);
  if (res>=n) {
    // n wasn't big enough to hold result; test 9 covers this unlikely event
    // C99 standard states that vsnprintf returns the size that would be big enough
    char *new = realloc(buff, res+1);
    if (!new) {
      // # nocov start
      snprintf(dest, n, "7 %d byte buff realloc failed", (int)res+1);
      free(spec);
      free(buff);
      return -1;
      // # nocov end
    }
    buff = new;
    va_start(ap, fmt); // to use ap again must reset it; #4545
    int newres = vsnprintf(buff, res+1, spec, ap);  // try again; test 9
    va_end(ap);
    if (newres!=res) {
      // # nocov start
      snprintf(dest, n, "8 %d %d second vsnprintf", newres, res);
      free(spec);
      free(buff);
      return -1;
      // # nocov end
    }
  } else if (res<1) { // negative is error, cover 0 as error too here
    // # nocov start
    snprintf(dest, n, "9 %d clib error", res);
    free(spec);
    free(buff);
    return -1;
    // # nocov end
  }
  // now we just need to put the string results for each arg back into the desired positions
  // create lookups so we can loop through fmt once replacing the specifiers as they appear
  ch = buff;
  for (int i=0; i<narg; ++i) {
    strp[i] = ch;
    const char *end = strstr(ch, delim);
    strl[i] = end-ch;
    ch = end+NDELIM;
  }
  ch = fmt;
  ch2 = dest;
  int nc=0;  // as per C99 standard: how many chars would be written if output isn't curtailed at n-1
             // since we are replacing snprintf with dt_win_snprintf it has to conform to same result
  while (*ch!='\0') {
    const int space = nc>=n-1 ? 0 : n-1-nc; // space remaining
    if (*ch!='%')   { if (space) *ch2++=*ch; ch++; nc++; continue; }  // copy non-specifier to the result as-is
    if (ch[1]=='%') { if (space) *ch2++='%'; ch+=2; nc++; continue; }  // interpret %% as a single %
    const int pos = atoi(ch+1);  // valid position already checked above
    nc += strl[pos-1];
    const int nWrite = MIN(strl[pos-1], space); // potentially write half of this field to fill up n
    strncpy(ch2, strp[pos-1], nWrite);
    ch2 += nWrite;
    ch = strpbrk(ch,"diufFeEgGxXoscpaA")+1; // move to the end of the specifier; valid checked earlier
  }
  *ch2='\0';
  free(spec);
  free(buff);
  return nc;
}

SEXP test_dt_win_snprintf()
{
  char buff[50];

  dt_win_snprintf(buff, 50, "No pos %d%%%d ok", 42, -84);
  if (strcmp(buff, "No pos 42%-84 ok"))                                             error("dt_win_snprintf test 1 failed: %s", buff);

  dt_win_snprintf(buff, 50, "With pos %1$d%%%2$d ok", 42, -84);
  if (strcmp(buff, "With pos 42%-84 ok"))                                           error("dt_win_snprintf test 2 failed: %s", buff);

  dt_win_snprintf(buff, 50, "With pos %2$d%%%1$d ok", 42, -84);
  if (strcmp(buff, "With pos -84%42 ok"))                                           error("dt_win_snprintf test 3 failed: %s", buff);

  dt_win_snprintf(buff, 50, "%3$s %1$d %4$10s %2$03d$", -99, 12, "hello%2$d", "short");
  if (strcmp(buff, "hello%2$d -99      short 012$"))                                error("dt_win_snprintf test 4 failed: %s", buff);

  dt_win_snprintf(buff, 50, "%1$d %s", 9, "foo");
  if (strcmp(buff, "3 some %n$ but not all"))                                       error("dt_win_snprintf test 5 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%%1$foo%d", 9);  // The %1$f is not a specifier because % is doubled
  if (strcmp(buff, "%1$foo9"))                                                      error("dt_win_snprintf test 6 failed: %s", buff);
  
  dt_win_snprintf(buff, 40, "long format string more than n==%d chopped", 40); // regular library (no %n$) chops to 39 chars + '/0'
  if (strlen(buff)!=39 || strcmp(buff, "long format string more than n==40 chop"))  error("dt_win_snprintf test 7 failed: %s", buff);
  
  dt_win_snprintf(buff, 40, "long %3$s %2$s more than n==%1$d chopped", 40, "string", "format"); // same with dt_win_snprintf
  if (strlen(buff)!=39 || strcmp(buff, "long format string more than n==40 chop"))  error("dt_win_snprintf test 8 failed: %s", buff);
  
  int res = dt_win_snprintf(buff, 10, "%4$d%2$d%3$d%5$d%1$d", 111, 222, 33, 44, 555); // fmt longer than n
  if (strlen(buff)!=9 || strcmp(buff, "442223355"))                                 error("dt_win_snprintf test 9 failed: %s", buff);
  if (res!=13) /* should return what would have been written if not chopped */      error("dt_win_snprintf test 10 failed: %d", res);
  
  dt_win_snprintf(buff, 39, "%l", 3);
  if (strlen(buff)!=38 || strcmp(buff, "0 %l    does not end with recognized t"))   error("dt_win_snprintf test 11 failed: %s", buff);
  
  dt_win_snprintf(buff, 19, "%l", 3);
  if (strlen(buff)!=18 || strcmp(buff, "0 %l    does not e"))                       error("dt_win_snprintf test 12 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%1$d == %0$d", 1, 2);
  if (strcmp(buff, "1 %0$ outside range [1,99]"))                                   error("dt_win_snprintf test 13 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%1$d == %$d", 1, 2);
  if (strcmp(buff, "1 %$ outside range [1,99]"))                                    error("dt_win_snprintf test 14 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%1$d == %100$d", 1, 2);
  if (strcmp(buff, "1 %100$ outside range [1,99]"))                                 error("dt_win_snprintf test 15 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%1$d == %-1$d", 1, 2);
  if (strcmp(buff, "1 %-1$ outside range [1,99]"))                                  error("dt_win_snprintf test 16 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%1$d == %3$d", 1, 2, 3);
  if (strcmp(buff, "5 %2$ missing"))                                                error("dt_win_snprintf test 17 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%1$d == %1$d", 42);
  if (strcmp(buff, "2 %1$ appears twice"))                                          error("dt_win_snprintf test 18 failed: %s", buff);
  
  dt_win_snprintf(buff, 50, "%1$d + %3$d - %2$d == %3$d", 1, 1, 2);
  if (strcmp(buff, "2 %3$ appears twice"))                                          error("dt_win_snprintf test 19 failed: %s", buff);
  
  return R_NilValue;
}

