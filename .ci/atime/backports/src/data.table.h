#include <Rversion.h>
#if R_VERSION >= R_Version(4, 6, 0)
void SETLENGTH(SEXP x, R_xlen_t n);
R_xlen_t TRUELENGTH(SEXP x);
void SET_TRUELENGTH(SEXP x, R_xlen_t n);
void SET_GROWABLE_BIT(SEXP);
int LEVELS(SEXP);
int NAMED(SEXP);
#define REFCNT(x) NAMED(x)
SEXP ATTRIB(SEXP);
void SET_ATTRIB(SEXP, SEXP);
int OBJECT(SEXP);
void SET_OBJECT(SEXP, int);
#define isFrame(x) isDataFrame(x)
#define GetOption(x, none) GetOption1(x)
#undef findVar // Rf_ mapping remains
#define findVar(sym, env) R_getVar(sym, env, FALSE)
#define STRING_PTR(x) ((SEXP *)STRING_PTR_RO(x))
int IS_S4_OBJECT(SEXP);
void SET_S4_OBJECT(SEXP);
void UNSET_S4_OBJECT(SEXP);
void SET_TYPEOF(SEXP, int);
#define VECTOR_ELT(x, i) VECTOR_ELT_(x, i)
SEXP VECTOR_ELT_(SEXP, R_xlen_t);
#define VECTOR_PTR(x) ((SEXP*)DATAPTR_RO(x))
#define DATAPTR(x) ((void*)DATAPTR_RO(x))
#endif
