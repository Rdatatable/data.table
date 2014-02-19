#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

// .Calls
SEXP setattrib();
SEXP bmerge();
SEXP assign();
SEXP dogroups();
SEXP copy();
SEXP shallowwrapper();
SEXP alloccolwrapper();
SEXP selfrefokwrapper();
SEXP truelength();
SEXP settruelength();
SEXP setcharvec();
SEXP setcolorder();
SEXP chmatchwrapper();
SEXP duplist();
SEXP readfile();
SEXP reorder();
SEXP rbindlist();
SEXP vecseq();
SEXP copyattr();
SEXP setlistelt();
SEXP setnamed();
SEXP address();
SEXP copyNamedInList();
SEXP fmelt();
SEXP fcast();
SEXP uniqlist();
SEXP uniqlengths();
SEXP fastradixdouble();
SEXP fastradixint();
SEXP setrev();
SEXP forder();
SEXP fsorted();
SEXP gstart();
SEXP gend();
SEXP gsum();

// .Externals
SEXP fastmean();

static const
R_CallMethodDef callMethods[] = {
{"Csetattrib", (DL_FUNC) &setattrib, -1},
{"Cbmerge", (DL_FUNC) &bmerge, -1},
{"Cassign", (DL_FUNC) &assign, -1},
{"Cdogroups", (DL_FUNC) &dogroups, -1},
{"Ccopy", (DL_FUNC) &copy, -1},
{"Cshallowwrapper", (DL_FUNC) &shallowwrapper, -1},
{"Calloccolwrapper", (DL_FUNC) &alloccolwrapper, -1},
{"Cselfrefokwrapper", (DL_FUNC) &selfrefokwrapper, -1},
{"Ctruelength", (DL_FUNC) &truelength, -1},
{"Csettruelength", (DL_FUNC) &settruelength, -1},
{"Csetcharvec", (DL_FUNC) &setcharvec, -1},
{"Csetcolorder", (DL_FUNC) &setcolorder, -1},
{"Cchmatchwrapper", (DL_FUNC) &chmatchwrapper, -1},
{"Cduplist", (DL_FUNC) &duplist, -1},
{"Creadfile", (DL_FUNC) &readfile, -1},
{"Creorder", (DL_FUNC) &reorder, -1},
{"Crbindlist", (DL_FUNC) &rbindlist, -1},
{"Cvecseq", (DL_FUNC) &vecseq, -1},
{"Ccopyattr", (DL_FUNC) &copyattr, -1},
{"Csetlistelt", (DL_FUNC) &setlistelt, -1},
{"Csetnamed", (DL_FUNC) &setnamed, -1},
{"Caddress", (DL_FUNC) &address, -1},
{"CcopyNamedInList", (DL_FUNC) &copyNamedInList, -1},
{"Cfmelt", (DL_FUNC) &fmelt, -1}, 
{"Cfcast", (DL_FUNC) &fcast, -1}, 
{"Cuniqlist", (DL_FUNC) &uniqlist, -1},
{"Cuniqlengths", (DL_FUNC) &uniqlengths, -1},
{"Cfastradixdouble", (DL_FUNC) &fastradixdouble, -1}, 
{"Cfastradixint", (DL_FUNC) &fastradixint, -1},
{"Csetrev", (DL_FUNC) &setrev, -1},
{"Cforder", (DL_FUNC) &forder, -1},
{"Cfsorted", (DL_FUNC) &fsorted, -1},
{"Cgstart", (DL_FUNC) &gstart, -1},
{"Cgend", (DL_FUNC) &gend, -1},
{"Cgsum", (DL_FUNC) &gsum, -1},
{NULL, NULL, 0}
};


static const
R_ExternalMethodDef externalMethods[] = {
{"Cfastmean", (DL_FUNC) &fastmean, -1},
{NULL, NULL, 0}
};

void setSizes();

void attribute_visible R_init_datatable(DllInfo *info)
// relies on pkg/src/Makevars to mv data.table.so to datatable.so
{
    R_registerRoutines(info, NULL, callMethods, NULL, externalMethods);
    R_useDynamicSymbols(info, FALSE);
    setSizes();
    if (NA_INTEGER != INT_MIN) error("NA_INTEGER [%d] != INT_MIN [%d]. Please report to datatable-help.", NA_INTEGER, INT_MIN);
    if (NA_INTEGER != NA_LOGICAL) error("NA_INTEGER [%d] != NA_LOGICAL [%d]. Please report to datatable-help.", NA_INTEGER, NA_LOGICAL);
    if (sizeof(int) != 4) error("sizeof(int) is not 4 but %d. Please report to datatable-help.", sizeof(int));
    if (sizeof(double) != 8) error("sizeof(double) is not 8 but %d. Please report to datatable-help.", sizeof(double));  // 8 on both 32bit and 64bit.
    if (sizeof(int *) != 4 && sizeof(int *) != 8) error("sizeof(pointer) is not 4 or 8 but %d. Please report to datatable-help.", sizeof(int *));
    if (sizeof(SEXP) != sizeof(int *)) error("sizeof(SEXP) [%d] != sizeof(pointer) [%d]. Please report to datatable-help.", sizeof(SEXP), sizeof(int *));

    // According to IEEE (http://en.wikipedia.org/wiki/IEEE_754-1985#Zero) we can rely on 0.0 being all 0 bits.
    // But check here anyway just to be sure, just in case this answer is right (http://stackoverflow.com/a/2952680/403310).
    int i = 314;
    memset(&i, 0, sizeof(int));
    if (i != 0) error("memset0 != (int)0.  Please report to data.table-help");
    unsigned int ui = 314;
    memset(&ui, 0, sizeof(unsigned int));
    if (ui != 0) error("memset0 != (unsigned int)0.  Please report to data.table-help");
    double d = 3.14;
    memset(&d, 0, sizeof(double));
    if (d != 0.0) error("memset0 != (double)0.0.  Please report to data.table-help");
    long double ld = 3.14;
    memset(&ld, 0, sizeof(long double));
    if (ld != 0.0) error("memset0 != (long double)0.0.  Please report to data.table-help");
}


