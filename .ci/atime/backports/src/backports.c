#include "data.table.h"
#if R_VERSION >= R_Version(4, 6, 0)
#define NAMED_BITS 16
struct sxpinfo_struct {
  SEXPTYPE type      :  TYPE_BITS; // in Rinternals.h
  unsigned int scalar:  1;
  unsigned int obj   :  1;
  unsigned int alt   :  1;
  unsigned int gp    : 16;
  unsigned int mark  :  1;
  unsigned int debug :  1;
  unsigned int trace :  1;
  unsigned int spare :  1;
  unsigned int gcgen :  1;
  unsigned int gccls :  3;
  unsigned int named : NAMED_BITS;
  unsigned int extra : 32 - NAMED_BITS;
};

struct vecsxp_struct {
  R_xlen_t length;
  R_xlen_t truelength;
};

typedef struct VECTOR_SEXPREC {
  struct sxpinfo_struct sxpinfo;
  SEXP attrib;
  SEXP gengc_next_node, gengc_prev_node;
  struct vecsxp_struct vecsxp;
} *VECSEXP;

void SETLENGTH(SEXP x, R_xlen_t n) {
  ((VECSEXP)x)->vecsxp.length = n;
}
R_xlen_t TRUELENGTH(SEXP x) {
  return ((VECSEXP)x)->vecsxp.truelength;
}
void SET_TRUELENGTH(SEXP x, R_xlen_t n) {
  ((VECSEXP)x)->vecsxp.truelength = n;
}
void SET_GROWABLE_BIT(SEXP x) {
  ((VECSEXP)x)->sxpinfo.gp |= 0x20;
}
int LEVELS(SEXP x) {
  return ((VECSEXP)x)->sxpinfo.gp;
}
int NAMED(SEXP x) {
  return ((VECSEXP)x)->sxpinfo.named;
}
int OBJECT(SEXP x) {
  return ((VECSEXP)x)->sxpinfo.obj;
}
void SET_OBJECT(SEXP x, int o) {
  ((VECSEXP)x)->sxpinfo.obj = o;
}
SEXP ATTRIB(SEXP x) {
  return ((VECSEXP)x)->attrib;
}
void SET_ATTRIB(SEXP x, SEXP att) {
  ((VECSEXP)x)->attrib = att;
}
#define S4_OBJECT (1<<4)
int IS_S4_OBJECT(SEXP x) {
  return ((VECSEXP)x)->sxpinfo.gp & S4_OBJECT;
}
void SET_S4_OBJECT(SEXP x) {
  ((VECSEXP)x)->sxpinfo.gp |= S4_OBJECT;
}
void UNSET_S4_OBJECT(SEXP x) {
  ((VECSEXP)x)->sxpinfo.gp &= ~S4_OBJECT;
}
void SET_TYPEOF(SEXP x, int type) {
  ((VECSEXP)x)->sxpinfo.type = type;
}
SEXP VECTOR_ELT_(SEXP x, R_xlen_t i) {
  return ALTREP(x) ? (VECTOR_ELT)(x, i) : ((SEXP*)DATAPTR_RO(x))[i];
}
#endif
