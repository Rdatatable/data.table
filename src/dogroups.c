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

SEXP dogroups(SEXP dt, SEXP SD, SEXP dtcols, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env)
{
  R_len_t i, j, k, n, rownum;
  SEXP names, ans, sizes;
	if (TYPEOF(order) != INTSXP) error("order not integer");
  if (TYPEOF(starts) != INTSXP) error("starts not integer");
  if (TYPEOF(lens) != INTSXP) error("lens not integer");
  if (INTEGER(starts)[0]<1 || INTEGER(lens)[0]<1) error("starts[1]<1 or lens[1]<1");
  if(!isEnvironment(env)) error("’env’ should be an environment");
  n = length(starts);  // the number of groups
  PROTECT(ans = allocVector(VECSXP, n));
	names = getAttrib(SD, R_NamesSymbol);
	// only the subset of column names that the j expression uses exist in SD, or if
	// the j uses .SD (or .SDF), then all the columns of dt (detected in the calling R). 
  PROTECT(sizes = allocVector(INTSXP, length(SD)));
	for(i = 0; i < length(SD); i++) {
		switch (TYPEOF(VECTOR_ELT(SD, i))) {
			case INTSXP :
			case LGLSXP :
				INTEGER(sizes)[i] = sizeof(int);
				break;
			case REALSXP :
				INTEGER(sizes)[i] = sizeof(double);
  			break;
			default:
				error("only integer,double and logical vectors currently tested");
		}
  	defineVar(install(CHAR(STRING_ELT(names, i))), VECTOR_ELT(SD, i), env);
  }
	defineVar(install(".SD"), SD, env);
	// By installing SD directly inside itself, R finds this symbol more quickly (if used). This then allows the
	// env's parent to be the one that called [.data.table, rather than the local scope of
	// [.data.table, for robustness and efficiency. If it were local scope of [.data.table then user's j
	// may inadvertently see interal variables names (as it used to in v1.2) and we had to name them to avoid
	// conflict e.g. "o__".  That is no longer required.
	for(i = 0; i < n; i++) {
		if (length(order)==0) {
			for (j=0; j<length(SD); j++) {
				memcpy(	(char *)DATAPTR(VECTOR_ELT(SD,j)),
								(char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1))+(INTEGER(starts)[i]-1)*INTEGER(sizes)[j],
								INTEGER(lens)[i]*INTEGER(sizes)[j]);
			}
		} else {
			for (k=0; k<INTEGER(lens)[i]; k++) {
				rownum = INTEGER(order)[ INTEGER(starts)[i] -1 + k ] -1;
				for (j=0; j<length(SD); j++) {	
					memcpy( (char *)DATAPTR(VECTOR_ELT(SD,j)) + k*INTEGER(sizes)[j],
								  (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1)) + rownum*INTEGER(sizes)[j],
									INTEGER(sizes)[j]);
				}
			}			
		}
		for (j=0; j<length(SD); j++) {  SETLENGTH(VECTOR_ELT(SD,j),INTEGER(lens)[i]); }
    SET_VECTOR_ELT(ans, i, eval(jexp, env));
	}
	UNPROTECT(2);
	return(ans);
}



