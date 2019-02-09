#include "data.table.h"
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
SEXP freadR();
SEXP fwriteR();
SEXP reorder();
SEXP rbindlist();
SEXP vecseq();
SEXP copyattr();
SEXP setlistelt();
SEXP setmutable();
SEXP address();
SEXP copyNamedInList();
SEXP expandAltRep();
SEXP fmelt();
SEXP fcast();
SEXP uniqlist();
SEXP uniqlengths();
SEXP setrev();
SEXP forder();
SEXP fsorted();
SEXP gforce();
SEXP gsum();
SEXP gmean();
SEXP gmin();
SEXP gmax();
SEXP isOrderedSubset();
SEXP pointWrapper();
SEXP setNumericRounding();
SEXP getNumericRounding();
SEXP binary();
SEXP chmatch2();
SEXP subsetDT();
SEXP subsetVector();
SEXP convertNegAndZeroIdx();
SEXP frank();
SEXP dt_na();
SEXP lookup();
SEXP overlaps();
SEXP whichwrapper();
SEXP shift();
SEXP transpose();
SEXP anyNA();
SEXP isReallyReal();
SEXP setlevels();
SEXP rleid();
SEXP gmedian();
SEXP gtail();
SEXP ghead();
SEXP glast();
SEXP gfirst();
SEXP gnthvalue();
SEXP dim();
SEXP gvar();
SEXP gsd();
SEXP gprod();
SEXP nestedid();
SEXP setDTthreads();
SEXP getDTthreads_R();
SEXP nqRecreateIndices();
SEXP fsort();
SEXP inrange();
SEXP between();
SEXP hasOpenMP();
SEXP uniqueNlogical();
SEXP frollfunR();
SEXP dllVersion();

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
{"CfreadR", (DL_FUNC) &freadR, -1},
{"CfwriteR", (DL_FUNC) &fwriteR, -1},
{"Creorder", (DL_FUNC) &reorder, -1},
{"Crbindlist", (DL_FUNC) &rbindlist, -1},
{"Cvecseq", (DL_FUNC) &vecseq, -1},
{"Ccopyattr", (DL_FUNC) &copyattr, -1},
{"Csetlistelt", (DL_FUNC) &setlistelt, -1},
{"Csetmutable", (DL_FUNC) &setmutable, -1},
{"Caddress", (DL_FUNC) &address, -1},
{"CcopyNamedInList", (DL_FUNC) &copyNamedInList, -1},
{"CexpandAltRep", (DL_FUNC) &expandAltRep, -1},
{"Cfmelt", (DL_FUNC) &fmelt, -1},
{"Cfcast", (DL_FUNC) &fcast, -1},
{"Cuniqlist", (DL_FUNC) &uniqlist, -1},
{"Cuniqlengths", (DL_FUNC) &uniqlengths, -1},
{"Csetrev", (DL_FUNC) &setrev, -1},
{"Cforder", (DL_FUNC) &forder, -1},
{"Cfsorted", (DL_FUNC) &fsorted, -1},
{"Cgforce", (DL_FUNC) &gforce, -1},
{"Cgsum", (DL_FUNC) &gsum, -1},
{"Cgmean", (DL_FUNC) &gmean, -1},
{"Cgmin", (DL_FUNC) &gmin, -1},
{"Cgmax", (DL_FUNC) &gmax, -1},
{"CisOrderedSubset", (DL_FUNC) &isOrderedSubset, -1},
{"CpointWrapper", (DL_FUNC) &pointWrapper, -1},
{"CsetNumericRounding", (DL_FUNC) &setNumericRounding, -1},
{"CgetNumericRounding", (DL_FUNC) &getNumericRounding, -1},
{"Cbinary", (DL_FUNC) &binary, -1},
{"Cchmatch2", (DL_FUNC) &chmatch2, -1},
{"CsubsetDT", (DL_FUNC) &subsetDT, -1},
{"CsubsetVector", (DL_FUNC) &subsetVector, -1},
{"CconvertNegAndZeroIdx", (DL_FUNC) &convertNegAndZeroIdx, -1},
{"Cfrank", (DL_FUNC) &frank, -1},
{"Cdt_na", (DL_FUNC) &dt_na, -1},
{"Clookup", (DL_FUNC) &lookup, -1},
{"Coverlaps", (DL_FUNC) &overlaps, -1},
{"Cwhichwrapper", (DL_FUNC) &whichwrapper, -1},
{"Cshift", (DL_FUNC) &shift, -1},
{"Ctranspose", (DL_FUNC) &transpose, -1},
{"CanyNA", (DL_FUNC) &anyNA, -1},
{"CisReallyReal", (DL_FUNC) &isReallyReal, -1},
{"Csetlevels", (DL_FUNC) &setlevels, -1},
{"Crleid", (DL_FUNC) &rleid, -1},
{"Cgmedian", (DL_FUNC) &gmedian, -1},
{"Cgtail", (DL_FUNC) &gtail, -1},
{"Cghead", (DL_FUNC) &ghead, -1},
{"Cglast", (DL_FUNC) &glast, -1},
{"Cgfirst", (DL_FUNC) &gfirst, -1},
{"Cgnthvalue", (DL_FUNC) &gnthvalue, -1},
{"Cdim", (DL_FUNC) &dim, -1},
{"Cgvar", (DL_FUNC) &gvar, -1},
{"Cgsd", (DL_FUNC) &gsd, -1},
{"Cgprod", (DL_FUNC) &gprod, -1},
{"Cnestedid", (DL_FUNC) &nestedid, -1},
{"CsetDTthreads", (DL_FUNC) &setDTthreads, -1},
{"CgetDTthreads", (DL_FUNC) &getDTthreads_R, -1},
{"CnqRecreateIndices", (DL_FUNC) &nqRecreateIndices, -1},
{"Cfsort", (DL_FUNC) &fsort, -1},
{"Cinrange", (DL_FUNC) &inrange, -1},
{"Cbetween", (DL_FUNC) &between, -1},
{"ChasOpenMP", (DL_FUNC) &hasOpenMP, -1},
{"CuniqueNlogical", (DL_FUNC) &uniqueNlogical, -1},
{"CfrollfunR", (DL_FUNC) &frollfunR, -1},
{"CdllVersion", (DL_FUNC) &dllVersion, -1},
{NULL, NULL, 0}
};


static const
R_ExternalMethodDef externalMethods[] = {
{"Cfastmean", (DL_FUNC) &fastmean, -1},
{NULL, NULL, 0}
};

void attribute_visible R_init_datatable(DllInfo *info)
// relies on pkg/src/Makevars to mv data.table.so to datatable.so
{
  R_registerRoutines(info, NULL, callMethods, NULL, externalMethods);
  R_useDynamicSymbols(info, FALSE);
  setSizes();
  const char *msg = "... failed. Please forward this message to maintainer('data.table').";
  if ((int)NA_INTEGER != (int)INT_MIN) error("Checking NA_INTEGER [%d] == INT_MIN [%d] %s", NA_INTEGER, INT_MIN, msg);
  if ((int)NA_INTEGER != (int)NA_LOGICAL) error("Checking NA_INTEGER [%d] == NA_LOGICAL [%d] %s", NA_INTEGER, NA_LOGICAL, msg);
  if (sizeof(int) != 4) error("Checking sizeof(int) [%d] is 4 %s", sizeof(int), msg);
  if (sizeof(double) != 8) error("Checking sizeof(double) [%d] is 8 %s", sizeof(double), msg);     // 8 on both 32bit and 64bit
  // alignof not available in C99: if (alignof(double) != 8) error("Checking alignof(double) [%d] is 8 %s", alignof(double), msg);  // 8 on both 32bit and 64bit
  if (sizeof(long long) != 8) error("Checking sizeof(long long) [%d] is 8 %s", sizeof(long long), msg);
  if (sizeof(char *) != 4 && sizeof(char *) != 8) error("Checking sizeof(pointer) [%d] is 4 or 8 %s", sizeof(char *), msg);
  if (sizeof(SEXP) != sizeof(char *)) error("Checking sizeof(SEXP) [%d] == sizeof(pointer) [%d] %s", sizeof(SEXP), sizeof(char *), msg);
  if (sizeof(uint64_t) != 8) error("Checking sizeof(uint64_t) [%d] is 8 %s", sizeof(uint64_t), msg);
  if (sizeof(int64_t) != 8) error("Checking sizeof(int64_t) [%d] is 8 %s", sizeof(int64_t), msg);
  if (sizeof(signed char) != 1) error("Checking sizeof(signed char) [%d] is 1 %s", sizeof(signed char), msg);
  if (sizeof(int8_t) != 1) error("Checking sizeof(int8_t) [%d] is 1 %s", sizeof(int8_t), msg);
  if (sizeof(uint8_t) != 1) error("Checking sizeof(uint8_t) [%d] is 1 %s", sizeof(uint8_t), msg);
  if (sizeof(int16_t) != 2) error("Checking sizeof(int16_t) [%d] is 2 %s", sizeof(int16_t), msg);
  if (sizeof(uint16_t) != 2) error("Checking sizeof(uint16_t) [%d] is 2 %s", sizeof(uint16_t), msg);

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

  // Check unsigned cast used in fread.c. This isn't overflow/underflow, just cast.
  if ((uint_fast8_t)('0'-'/') != 1) error("The ascii character '/' is not just before '0'");
  if ((uint_fast8_t)('/'-'0') < 10) error("The C expression (uint_fast8_t)('/'-'0')<10 is true. Should be false.");
  if ((uint_fast8_t)(':'-'9') != 1) error("The ascii character ':' is not just after '9'");
  if ((uint_fast8_t)('9'-':') < 10) error("The C expression (uint_fast8_t)('9'-':')<10 is true. Should be false.");

  // Variables rather than #define for NA_INT64 to ensure correct usage; i.e. not casted
  NA_INT64_LL = LLONG_MIN;
  NA_INT64_D = LLtoD(NA_INT64_LL);
  if (NA_INT64_LL != DtoLL(NA_INT64_D)) error("Conversion of NA_INT64 via double failed %lld!=%lld", NA_INT64_LL, DtoLL(NA_INT64_D));
  // LLONG_MIN when punned to double is the sign bit set and then all zeros in exponent and significand i.e. -0.0
  //   That's why we must never test for NA_INT64_D using == in double type. Must always DtoLL and compare long long types.
  //   Assigning NA_INT64_D to a REAL is ok however.
  if (NA_INT64_D != 0.0)  error("NA_INT64_D (negative -0.0) is not == 0.0.");
  if (NA_INT64_D != -0.0) error("NA_INT64_D (negative -0.0) is not ==-0.0.");
  if (ISNAN(NA_INT64_D)) error("ISNAN(NA_INT64_D) is TRUE but should not be");
  if (isnan(NA_INT64_D)) error("isnan(NA_INT64_D) is TRUE but should not be");

  setNumericRounding(PROTECT(ScalarInteger(0))); // #1642, #1728, #1463, #485
  UNPROTECT(1);

  // create needed strings in advance for speed, same techique as R_*Symbol
  // Following R-exts 5.9.4; paragraph and example starting "Using install ..."
  // either use PRINTNAME(install()) or R_PreserveObject(mkChar()) here.
  char_integer64 = PRINTNAME(install("integer64"));
  char_ITime =     PRINTNAME(install("ITime"));
  char_Date =      PRINTNAME(install("Date"));   // used for IDate too since IDate inherits from Date
  char_POSIXct =   PRINTNAME(install("POSIXct"));
  char_nanotime =  PRINTNAME(install("nanotime"));
  char_starts =    PRINTNAME(sym_starts = install("starts"));
  char_lens =      PRINTNAME(install("lens"));
  char_indices =   PRINTNAME(install("indices"));
  char_allLen1 =   PRINTNAME(install("allLen1"));
  char_allGrp1 =   PRINTNAME(install("allGrp1"));

  if (TYPEOF(char_integer64) != CHARSXP) {
    // checking one is enough in case of any R-devel changes
    error("PRINTNAME(install(\"integer64\")) has returned %s not %s",
      type2char(TYPEOF(char_integer64)), type2char(CHARSXP));
  }

  // create commonly used symbols, same as R_*Symbol but internal to DT
  // Not really for speed but to avoid leak in situations like setAttrib(DT, install(), allocVector()) where
  // the allocVector() can happen first and then the install() could gc and free it before it is protected
  // within setAttrib. Thanks to Bill Dunlap finding and reporting. Using these symbols instead of install()
  // avoids the gc without needing an extra PROTECT and immediate UNPROTECT after the setAttrib which would
  // look odd (and devs in future might be tempted to remove them). Avoiding passing install() to API calls
  // keeps the code neat and readable. Also see grep's added to CRAN_Release.cmd to find such calls.
  sym_sorted  = install("sorted");
  sym_index   = install("index");
  sym_BY      = install(".BY");
  sym_maxgrpn = install("maxgrpn");

  avoid_openmp_hang_within_fork();
}

inline bool INHERITS(SEXP x, SEXP char_) {
  // Thread safe inherits() by pre-calling install() above in init first then
  // passing those char_* in here for simple and fast non-API pointer compare.
  // The thread-safety aspect here is only currently actually needed for list columns in
  // fwrite() where the class of the cell's vector is tested; the class of the column
  // itself is pre-stored by fwrite (for example in isInteger64[] and isITime[]).
  // Thread safe in the limited sense of correct and intended usage :
  // i) no API call such as install() or mkChar() must be passed in.
  // ii) no attrib writes must be possible in other threads.
  SEXP klass;
  if (isString(klass = getAttrib(x, R_ClassSymbol))) {
    for (int i=0; i<LENGTH(klass); i++) {
      if (STRING_ELT(klass, i) == char_) return true;
    }
  }
  return false;
}

inline long long DtoLL(double x) {
  // Type punning such as
  //     *(long long *)&REAL(column)[i]
  // is undefined by C standards. This may have been the cause of 1.10.2 failing on 31 Jan 2017
  // under clang 3.9.1 -O3 and solaris-sparc but not solaris-x86 or gcc.
  // There is now a grep in CRAN_Release.cmd; use this union method instead.
  // int64_t may help rather than 'long long' (TODO: replace all long long with int64_t)
  // The two types must be the same size. That is checked in R_init_datatable (above)
  // where sizeof(int64_t)==sizeof(double)==8 is checked.
  // Endianness should not matter because whether big or little, endianness is the same
  // inside this process, and the two types are the same size.
  union {double d; int64_t i64;} u;  // not static, inline instead
  u.d = x;
  return (long long)u.i64;
}

inline double LLtoD(long long x) {
  union {double d; int64_t i64;} u;
  u.i64 = (int64_t)x;
  return u.d;
}


SEXP hasOpenMP() {
  // Just for use by onAttach to avoid an RPRINTF from C level which isn't suppressable by CRAN
  // There is now a 'grep' in CRAN_Release.cmd to detect any use of RPRINTF in init.c, which is
  // why RPRINTF is capitalized in this comment to avoid that grep.
  // TODO: perhaps .Platform or .Machine in R itself could contain whether OpenMP is available.
  #ifdef _OPENMP
  return ScalarLogical(TRUE);
  #else
  return ScalarLogical(FALSE);
  #endif
}

SEXP dllVersion() {
  // .onLoad calls this and checks the same as packageVersion() to ensure no R/C version mismatch, #3056
  return(ScalarString(mkChar("1.12.1")));
}

