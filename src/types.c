#include <Rdefines.h>

// find end of a string, used to append verbose message or warnings
char *end(char *start) {
  return strchr(start, 0);
}
