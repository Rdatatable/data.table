#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT void setattrib();
#endif

void setattrib(SEXP x, SEXP name, SEXP value)
{
    // For internal use by setkey for example, setting attributes by reference
    // Note the lower case 'a' is required to avoid conflict with R's internal setAttrib
    setAttrib(x, install(CHAR(STRING_ELT(name,0))), value);
}

