#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
// NB: flip argument order to match that of R's ngettext()
#define Pl_(n, String1, StringPlural) dngettext("data.table", String1, StringPlural, n)
#else
#define _(String) (String)
#define Pl_(n, String1, StringPlural) ((n) == 1 ? (String1) : (StringPlural))
#endif
