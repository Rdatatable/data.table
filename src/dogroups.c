#include <R.h>
#define USE_RINTERNALS  // to do: test the speed increase of this (didn't have a good test case yet), and test code works with and without it. It seems to turn INTEGER() into a macro rather than a function call.
#include <Rinternals.h>
#include <Rdefines.h>
//#include <sys/mman.h>
#include <fcntl.h>

#ifdef BUILD_DLL
#define EXPORT __declspec(dllexport)
EXPORT SEXP dogroups();
#endif

SEXP dogroups(SEXP dt, SEXP SS, SEXP dtcols, SEXP dtcolsizes, SEXP starts, SEXP lens, SEXP jexp, SEXP env)
{
    int mode;
    R_len_t i, j, n;
    SEXP names, ans;
    if (TYPEOF(starts) != INTSXP) error("starts not integer");
    if (TYPEOF(lens) != INTSXP) error("lens not integer");
    if (INTEGER(starts)[0]<1 || INTEGER(lens)[0]<1) error("starts[1]<1 or lens[1]<1");
    if(!isEnvironment(env)) error("’env’ should be an environment");
    n = length(starts);  // the number of groups
    PROTECT(ans = allocVector(VECSXP, n));
	names = getAttrib(SS, R_NamesSymbol);
	// only the the subset of column names that the j expression uses exist in SS, or if
	// the j uses SS, then all the columns of dt (detected in the calling R). 
	for(i = 0; i < length(SS); i++) {
		switch (TYPEOF(VECTOR_ELT(SS, i))) {
			case INTSXP :
			case REALSXP :
			case LGLSXP :
    			break;
    		default:
    			error("only integer,double and logical vectors currently tested");
		}
	  	defineVar(install(CHAR(STRING_ELT(names, i))), VECTOR_ELT(SS, i), env);
    }
	for(i = 0; i < n; i++) {
		for (j=0; j<length(SS); j++) {
			memcpy(	(char *)DATAPTR(VECTOR_ELT(SS,j)),
					(char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]))+(INTEGER(starts)[i]-1)*INTEGER(dtcolsizes)[j],
					INTEGER(lens)[i]*INTEGER(dtcolsizes)[j]);
			SETLENGTH(VECTOR_ELT(SS,j),INTEGER(lens)[i]);
		}
       	SET_VECTOR_ELT(ans, i, eval(jexp, env));
    }
    UNPROTECT(1);
    return(ans);
}



