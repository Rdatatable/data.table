#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
#define Pl_(n, String1, StringPlural) dngettext("data.table", String1, StringPlural, n)
#define Pl_Extract(String1, StringPlural) (String1 "\0" StringPlural) // Helps extract both strings
#else
#define _(String) (String)
#define Pl_(n, String1, StringPlural) ((n) == 1 ? (String1) : (StringPlural))
#define Pl_Extract(String1, StringPlural) (String1 "\0" StringPlural) // Consistent behavior
#endif
