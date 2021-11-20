#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
#else
#define _(String) (String)
#endif
