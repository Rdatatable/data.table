
// It seems that latest min64-w64 compiler needs some help to avoid warnings; #4064
// It appears that its inttypes.h defines PRId64 to be I64 on Windows but then warns that
// I64 is not C99 too. Which is correct, but we know, and we don't want any warnings, and
// we can't control settings on CRAN. So here we try and isolate ourselves from all scenarios.
// There is a lot online about this, but these were a few that were most useful:
// https://stackoverflow.com/questions/23718110/error-unknown-conversion-type-character-l-in-format-scanning-long-long
// https://gitlab.freedesktop.org/nacho.garglez/cerbero/commit/4ec6bfdc1db1e4bc8d4278f53ad295af1efe89fb
// https://github.com/GNOME/glib/commit/98a0ab929d8c59ee27e5f470f11d077bb6a56749#diff-969b60ad3d206fd45c208e266ccfed38
// https://sourceforge.net/p/mingw-w64/wiki2/gnu%20printf/
// Warning that __USE_MINGW_ANSI_STDIO could be deprecated:
// https://mingw-w64-public.narkive.com/aQDJHqCv/how-to-printf-64-bit-integer-in-64-bit-compiler-in-strict-iso-mode-with-all-warnings-enabled
// and then actual deprecation (with complaints) earlier in 2019 here:
// https://osdn.net/projects/mingw/lists/archive/users/2019-January/000202.html
// But I can't find _mingw.h. I can find mingw.h but not advice within it:
// https://github.com/git/git/blob/master/compat/mingw.h#L448
// The deprecation about I64 doesn't seem to take into account that lld throws the warning in MinGW:
//   warning: unknown conversion type character 'l' in format [-Wformat=]
// So let's see if the approach below works for now.
// If we need to make changes in future, we can do it here in one place.

#ifndef DT_STDIO_H
#define DT_STDIO_H
#if defined(__MINGW32__) || (defined __MINGW64__)
  #define __USE_MINGW_ANSI_STDIO 1
  #include <stdio.h>
  #define PRId64 "lld"
  #define PRIu64 "llu"
#else
  #include <stdio.h>
  #include <inttypes.h>
  // let inttypes.h decide: lld on Linux/C99; I64 on Windows Visual Studio for example
#endif
#endif
