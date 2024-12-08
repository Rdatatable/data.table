#include <R.h>
#include <Rinternals.h>

// NOTE: All of this is copied from Defn.h: https://github.com/wch/r-source/blob/28de75af0541f93832c5899139b969d290bf422e/src/include/Defn.h
// We intend to gradually remove the need for this header file

// Writable vector/string pointer
#define SEXPPTR(x) ((SEXP *)DATAPTR(x))

#ifndef NAMED_BITS
# define NAMED_BITS 16
#endif

#ifndef SEXPREC_HEADER

// https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L123
struct sxpinfo_struct {
  SEXPTYPE type      :  TYPE_BITS;
  /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
   * -> warning: `type' is narrower than values
   *              of its type
   * when SEXPTYPE was an enum */
  unsigned int scalar:  1;
  unsigned int obj   :  1;
  unsigned int alt   :  1;
  unsigned int gp    : 16;
  unsigned int mark  :  1;
  unsigned int debug :  1;
  unsigned int trace :  1;  /* functions and memory tracing */
  unsigned int spare :  1;  /* used on closures and when REFCNT is defined */
  unsigned int gcgen :  1;  /* old generation number */
  unsigned int gccls :  3;  /* node class */
  unsigned int named : NAMED_BITS;
  unsigned int extra : 32 - NAMED_BITS; /* used for immediate bindings */
}; /*		    Tot: 64 */
  
// https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L190  
#define SEXPREC_HEADER           \
  struct sxpinfo_struct sxpinfo; \
  struct SEXPREC *attrib;        \
  struct SEXPREC *gengc_next_node, *gengc_prev_node

// https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L143
struct vecsxp_struct {
  R_xlen_t	length;
  R_xlen_t	truelength;
};
  
// https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L214
typedef struct VECTOR_SEXPREC {
  SEXPREC_HEADER;
  struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;
  
#endif
  
// https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L197  
typedef struct {
  SEXPREC_HEADER;
} SEXPREC_partial;

// (SET_)TRULENGTH: https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L386
#define STDVEC_TRUELENGTH(x) (((VECSEXP) (x))->vecsxp.truelength)
// No method to set ALTREP_TRUELENGTH (gives error): https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L388
#define SET_TRULEN(x, v) (STDVEC_TRUELENGTH(x)=(v))
// ALTREP_TRUELENGTH is 0: https://github.com/wch/r-source/blob/48f06c1071fea6a6e7e365ad3d745217268e2175/src/main/altrep.c#L345
#define TRULEN(x) (ALTREP(x) ? 0 : STDVEC_TRUELENGTH(x))
  
// SETLENGTH: https://github.com/wch/r-source/blob/05bb18266d49e87f2477120ecb0ab1440f4e9b40/src/include/Defn.h#L385
#define STDVEC_LENGTH(x) (((VECSEXP) (x))->vecsxp.length)
#define SETSCAL(x, v) ((((SEXPREC_partial *)(x))->sxpinfo.scalar) = (v))
#define SET_STDVEC_LENGTH(x,v) do {		     \
  SEXP __x__ = (x);			                   \
  R_xlen_t __v__ = (v);			               \
  STDVEC_LENGTH(__x__) = __v__;		         \
  SETSCAL(__x__, __v__ == 1 ? 1 : 0);	     \
} while (0)                                
// https://github.com/wch/r-source/blob/05bb18266d49e87f2477120ecb0ab1440f4e9b40/src/main/memory.c#L4072  
#define SET_LEN SET_STDVEC_LENGTH

// LEVELS: https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L228    
#define LEVLS(x)	(((SEXPREC_partial *)(x))->sxpinfo.gp)

// SET_GROWABLE_BIT: https://github.com/wch/r-source/blob/2640a203d13473f95c9c7508eb2976fefb5c931c/src/include/Defn.h#L374
#define GROWBLE_MASK ((unsigned short)(1<<5))
#define SET_GROWBLE_BIT(x) (LEVLS(x) |= GROWBLE_MASK)
