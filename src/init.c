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
SEXP setcharvec();
SEXP setcolorder();
SEXP chmatchwrapper();
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
SEXP gmean();
SEXP isOrderedSubset();
SEXP pointWrapper();
SEXP setNumericRounding();
SEXP getNumericRounding();
SEXP binary();
SEXP chmatch2();
SEXP subsetDT();
SEXP subsetVector();
SEXP convertNegativeIdx();

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
{"Csetcharvec", (DL_FUNC) &setcharvec, -1},
{"Csetcolorder", (DL_FUNC) &setcolorder, -1},
{"Cchmatchwrapper", (DL_FUNC) &chmatchwrapper, -1},
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
{"Cgmean", (DL_FUNC) &gmean, -1},
{"CisOrderedSubset", (DL_FUNC) &isOrderedSubset, -1},
{"CpointWrapper", (DL_FUNC) &pointWrapper, -1},
{"CsetNumericRounding", (DL_FUNC) &setNumericRounding, -1},
{"CgetNumericRounding", (DL_FUNC) &getNumericRounding, -1},
{"Cbinary", (DL_FUNC) &binary, -1},
{"Cchmatch2", (DL_FUNC) &chmatch2, -1},
{"CsubsetDT", (DL_FUNC) &subsetDT, -1},
{"CsubsetVector", (DL_FUNC) &subsetVector, -1},
{"CconvertNegativeIdx", (DL_FUNC) &convertNegativeIdx, -1},
{NULL, NULL, 0}
};


static const
R_ExternalMethodDef externalMethods[] = {
{"Cfastmean", (DL_FUNC) &fastmean, -1},
{NULL, NULL, 0}
};

void setSizes();
SEXP char_integer64;

void attribute_visible R_init_datatable(DllInfo *info)
// relies on pkg/src/Makevars to mv data.table.so to datatable.so
{
    R_registerRoutines(info, NULL, callMethods, NULL, externalMethods);
    R_useDynamicSymbols(info, FALSE);
    setSizes();
    const char *msg = "... failed. Please forward this message to maintainer('data.table') or datatable-help.";
    if (NA_INTEGER != INT_MIN) error("Checking NA_INTEGER [%d] == INT_MIN [%d] %s", NA_INTEGER, INT_MIN, msg);
    if (NA_INTEGER != NA_LOGICAL) error("Checking NA_INTEGER [%d] == NA_LOGICAL [%d] %s", NA_INTEGER, NA_LOGICAL, msg);
    if (sizeof(int) != 4) error("Checking sizeof(int) [%d] is 4 %s", sizeof(int), msg);
    if (sizeof(double) != 8) error("Checking sizeof(double) [%d] is 8 %s", sizeof(double), msg);  // 8 on both 32bit and 64bit.
    if (sizeof(long long) != 8) error("Checking sizeof(long long) [%d] is 8 %s", sizeof(long long), msg);
    if (sizeof(char *) != 4 && sizeof(char *) != 8) error("Checking sizeof(pointer) [%d] is 4 or 8 %s", sizeof(char *), msg);
    if (sizeof(SEXP) != sizeof(char *)) error("Checking sizeof(SEXP) [%d] == sizeof(pointer) [%d] %s", sizeof(SEXP), sizeof(char *), msg);
    
    SEXP tmp = PROTECT(allocVector(INTSXP,2));
    if (LENGTH(tmp)!=2) error("Checking LENGTH(allocVector(INTSXP,2)) [%d] is 2 %s", LENGTH(tmp), msg);
    if (TRUELENGTH(tmp)!=0) error("Checking TRUELENGTH(allocVector(INTSXP,2)) [%d] is 0 %s", TRUELENGTH(tmp), msg);
    UNPROTECT(1);

    // According to IEEE (http://en.wikipedia.org/wiki/IEEE_754-1985#Zero) we can rely on 0.0 being all 0 bits.
    // But check here anyway just to be sure, just in case this answer is right (http://stackoverflow.com/a/2952680/403310).
    int i = 314;
    memset(&i, 0, sizeof(int));
    if (i != 0) error("Checking memset(&i,0,sizeof(int)); i == (int)0 %s", msg);
    unsigned int ui = 314;
    memset(&ui, 0, sizeof(unsigned int));
    if (ui != 0) error("Checking memset(&ui, 0, sizeof(unsigned int)); ui == (unsigned int)0 %s", msg);
    double d = 3.14;
    memset(&d, 0, sizeof(double));
    if (d != 0.0) error("Checking memset(&d, 0, sizeof(double)); d == (double)0.0 %s", msg);
    long double ld = 3.14;
    memset(&ld, 0, sizeof(long double));
    if (ld != 0.0) error("Checking memset(&ld, 0, sizeof(long double)); ld == (long double)0.0 %s", msg);
    
    setNumericRounding(ScalarInteger(2));
    
    char_integer64 = mkChar("integer64");  // for speed, similar to R_*Symbol.
}


