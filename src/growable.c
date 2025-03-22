#include "data.table.h"

#ifndef USE_GROWABLE_ALTREP

SEXP growable_allocate(SEXPTYPE type, R_xlen_t size, R_xlen_t max_size) {
  SEXP ret = PROTECT(allocVector(type, max_size));
  SET_TRUELENGTH(ret, max_size);
#if R_VERSION >= R_Version(3, 4, 0)
  SET_GROWABLE_BIT(ret);
#endif // otherwise perceived memory use will end up higher
  SETLENGTH(ret, size);
  UNPROTECT(1);
  return ret;
}

R_xlen_t growable_max_size(SEXP x) {
  return TRUELENGTH(x);
}

void growable_resize(SEXP x, R_xlen_t newsize) {
  R_xlen_t max_size;
  if (newsize > (max_size = growable_max_size(x))) internal_error(
    __func__, "newsize=%g > max_size=%g",
    (double)newsize, (double)max_size
  );
  SETLENGTH(x, newsize);
}

Rboolean is_growable(SEXP x) {
  return isVector(x) && TRUELENGTH(x) >= XLENGTH(x)
#if R_VERSION >= R_Version(3, 4, 0)
    && IS_GROWABLE(x)
#endif
  ;
}

// Assuming no ALTREP columns
SEXP make_growable(SEXP x) {
  if (TRUELENGTH(x) < XLENGTH(x)) SET_TRUELENGTH(x, XLENGTH(x));
  SET_GROWABLE_BIT(x);
  return x;
}

#else

#include <R_ext/Altrep.h>

static R_altrep_class_t dta_grow_string, dta_grow_integer, dta_grow_logical, dta_grow_real, dta_grow_complex, dta_grow_raw, dta_grow_list;
static Rcomplex NA_COMPLEX = { 0, };

/*
ALTREP class layout:
data1 = underlying vector
data2 = its current length stored as a length-1 REALSXP
Unless we implement an Unserialize method, this can be changed any time.
Classes have been released on CRAN with a Serialized_state/Unserialize pair will have to stay as they have been defined in order to keep *.rds files readable.
*/

static R_xlen_t altall_Length(SEXP x) {
  return (R_xlen_t)REAL(R_altrep_data2(x))[0];
}

#define make_inspect_method(classname) \
  static Rboolean alt##classname##_Inspect( \
    SEXP x, int pre, int deep, int pvec, \
    void (*inspect_subtree)(SEXP x, int pre, int deep, int pvec) \
  ) { \
    (void)pre; (void)deep; (void)pvec; (void)inspect_subtree; \
    Rprintf("data.table::growable" #classname "_v0(truelength=%g) ", (double)XLENGTH(R_altrep_data1(x))); \
    return FALSE; \
  }
make_inspect_method(string)
make_inspect_method(integer)
make_inspect_method(logical)
make_inspect_method(real)
make_inspect_method(complex)
make_inspect_method(raw)
make_inspect_method(list)
#undef make_inspect_method

#define make_dataptr_method(class, accessor) \
  static void * alt##class##_Dataptr(SEXP x, Rboolean writable) { \
    (void)writable; \
    return (void*)accessor(R_altrep_data1(x)); \
  }
make_dataptr_method(string, STRING_PTR_RO)
make_dataptr_method(integer, INTEGER)
make_dataptr_method(logical, LOGICAL)
make_dataptr_method(real, REAL)
make_dataptr_method(complex, COMPLEX)
make_dataptr_method(raw, RAW)
make_dataptr_method(list, DATAPTR_RO) // VECTOR_PTR_RO to appear in R-4.5
#undef make_dataptr_method

static const void * altall_Dataptr_or_null(SEXP x) { return DATAPTR_RO(x); }

// lots of boilerplate, but R calling *_ELT one by one would be far too slow
#define make_extract_subset_method(class, type, accessor, NA) \
  static SEXP alt##class##_Extract_subset(SEXP x, SEXP indx, SEXP call) { \
    (void)call; \
    indx = PROTECT(coerceVector(indx, REALSXP)); \
    double * ii = REAL(indx); \
    R_xlen_t rlen = XLENGTH(indx), mylen = XLENGTH(x); \
    SEXP ret = PROTECT(allocVector(TYPEOF(x), rlen)); \
    type *rdata = accessor(ret), *mydata = accessor(x); \
    for (R_xlen_t i = 0; i < rlen; ++i) \
      rdata[i] = (ii[i] >= 1 && ii[i] <= mylen) ? mydata[(R_xlen_t)ii[i]-1] : NA; \
    UNPROTECT(2); \
    return ret; \
  }
make_extract_subset_method(integer, int, INTEGER, NA_INTEGER)
make_extract_subset_method(logical, int, LOGICAL, NA_LOGICAL)
make_extract_subset_method(real, double, REAL, NA_REAL)
make_extract_subset_method(complex, Rcomplex, COMPLEX, NA_COMPLEX)
make_extract_subset_method(raw, Rbyte, RAW, 0)
// not implementing the string and list methods because those do require the write barrier and are thus no better than calling *_ELT one by one
#undef make_extract_subset_method

#define make_elt_method(class, accessor) \
  static SEXP alt##class##_Elt(SEXP x, R_xlen_t i) { \
    return accessor(R_altrep_data1(x), i); \
  }
make_elt_method(string, STRING_ELT)
make_elt_method(list, VECTOR_ELT)
#undef make_elt_method

#define make_set_elt_method(class, accessor) \
  static void alt##class##_Set_elt(SEXP x, R_xlen_t i, SEXP v) { \
    accessor(R_altrep_data1(x), i, v); \
  }
make_set_elt_method(string, SET_STRING_ELT)
make_set_elt_method(list, SET_VECTOR_ELT)
#undef make_set_elt_method

// liked the Extract_subset methods? say hello to Get_region
#define make_get_region_method(class, type, accessor) \
  static R_xlen_t alt##class##_Get_region( \
    SEXP x, R_xlen_t i, R_xlen_t n, type * buf \
  ) { \
    R_xlen_t j = 0, mylen = XLENGTH(x); \
    type * data = accessor(x); \
    for (; j < n && i < mylen; ++i, ++j) buf[j] = data[i]; \
    return j; \
  }
make_get_region_method(integer, int, INTEGER)
make_get_region_method(logical, int, LOGICAL)
make_get_region_method(real, double, REAL)
make_get_region_method(complex, Rcomplex, COMPLEX)
make_get_region_method(raw, Rbyte, RAW)
#undef make_get_region_method

void register_altrep_classes(DllInfo * info) {
  // Used by the altcomplex_Extract_subset method
  NA_COMPLEX = (Rcomplex){ .r = NA_REAL, .i = NA_REAL };

  dta_grow_string = R_make_altstring_class("growable_string_v0", "data.table", info);
  R_set_altrep_Length_method(dta_grow_string, altall_Length);
  R_set_altrep_Inspect_method(dta_grow_string, altstring_Inspect);
  R_set_altvec_Dataptr_method(dta_grow_string, altstring_Dataptr);
  R_set_altvec_Dataptr_or_null_method(dta_grow_string, altall_Dataptr_or_null);
  R_set_altstring_Elt_method(dta_grow_string, altstring_Elt);
  R_set_altstring_Set_elt_method(dta_grow_string, altstring_Set_elt);
  dta_grow_integer = R_make_altinteger_class("growable_integer_v0", "data.table", info);
  R_set_altrep_Length_method(dta_grow_integer, altall_Length);
  R_set_altrep_Inspect_method(dta_grow_integer, altinteger_Inspect);
  R_set_altvec_Dataptr_method(dta_grow_integer, altinteger_Dataptr);
  R_set_altvec_Dataptr_or_null_method(dta_grow_integer, altall_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(dta_grow_integer, altinteger_Extract_subset);
  R_set_altinteger_Get_region_method(dta_grow_integer, altinteger_Get_region);
  dta_grow_logical = R_make_altlogical_class("growable_logical_v0", "data.table", info);
  R_set_altrep_Length_method(dta_grow_logical, altall_Length);
  R_set_altrep_Inspect_method(dta_grow_logical, altlogical_Inspect);
  R_set_altvec_Dataptr_method(dta_grow_logical, altlogical_Dataptr);
  R_set_altvec_Dataptr_or_null_method(dta_grow_logical, altall_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(dta_grow_logical, altlogical_Extract_subset);
  R_set_altlogical_Get_region_method(dta_grow_logical, altlogical_Get_region);
  dta_grow_real = R_make_altreal_class("growable_real_v0", "data.table", info);
  R_set_altrep_Length_method(dta_grow_real, altall_Length);
  R_set_altrep_Inspect_method(dta_grow_real, altreal_Inspect);
  R_set_altvec_Dataptr_method(dta_grow_real, altreal_Dataptr);
  R_set_altvec_Dataptr_or_null_method(dta_grow_real, altall_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(dta_grow_real, altreal_Extract_subset);
  R_set_altreal_Get_region_method(dta_grow_real, altreal_Get_region);
  dta_grow_complex = R_make_altcomplex_class("growable_complex_v0", "data.table", info);
  R_set_altrep_Length_method(dta_grow_complex, altall_Length);
  R_set_altrep_Inspect_method(dta_grow_complex, altcomplex_Inspect);
  R_set_altvec_Dataptr_method(dta_grow_complex, altcomplex_Dataptr);
  R_set_altvec_Dataptr_or_null_method(dta_grow_complex, altall_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(dta_grow_complex, altcomplex_Extract_subset);
  R_set_altcomplex_Get_region_method(dta_grow_complex, altcomplex_Get_region);
  dta_grow_raw = R_make_altraw_class("growable_raw_v0", "data.table", info);
  R_set_altrep_Length_method(dta_grow_raw, altall_Length);
  R_set_altrep_Inspect_method(dta_grow_raw, altraw_Inspect);
  R_set_altvec_Dataptr_method(dta_grow_raw, altraw_Dataptr);
  R_set_altvec_Dataptr_or_null_method(dta_grow_raw, altall_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(dta_grow_raw, altraw_Extract_subset);
  R_set_altraw_Get_region_method(dta_grow_raw, altraw_Get_region);
  dta_grow_list = R_make_altlist_class("growable_list_v0", "data.table", info);
  R_set_altrep_Length_method(dta_grow_list, altall_Length);
  R_set_altrep_Inspect_method(dta_grow_list, altlist_Inspect);
  R_set_altvec_Dataptr_method(dta_grow_list, altlist_Dataptr);
  R_set_altvec_Dataptr_or_null_method(dta_grow_list, altall_Dataptr_or_null);
  R_set_altlist_Elt_method(dta_grow_list, altlist_Elt);
  R_set_altlist_Set_elt_method(dta_grow_list, altlist_Set_elt);
}

static R_altrep_class_t type2class(SEXPTYPE type) {
  switch(type) {
  case STRSXP:
    return dta_grow_string;
  case INTSXP:
    return dta_grow_integer;
  case LGLSXP:
    return dta_grow_logical;
  case REALSXP:
    return dta_grow_real;
  case CPLXSXP:
    return dta_grow_complex;
  case RAWSXP:
    return dta_grow_raw;
  case VECSXP:
  case EXPRSXP:
    return dta_grow_list;
  default:
    internal_error(__func__, "Can't create a growable vector of type '%s'", type2char(type));
  }
}

SEXP growable_allocate(SEXPTYPE type, R_xlen_t size, R_xlen_t max_size) {
  SEXP ret = PROTECT(R_new_altrep(type2class(type), R_NilValue, R_NilValue));
  R_set_altrep_data1(ret, allocVector(type, max_size));
  R_set_altrep_data2(ret, ScalarReal(size));
  UNPROTECT(1);
  return ret;
}

R_xlen_t growable_max_size(SEXP x) {
  return XLENGTH(R_altrep_data1(x));
}

void growable_resize(SEXP x, R_xlen_t newsize) {
  R_xlen_t max_size;
  if (newsize > (max_size = growable_max_size(x))) internal_error(
    __func__, "newsize=%g > max_size=%g",
    (double)newsize, (double)max_size
  );
  REAL(R_altrep_data2(x))[0] = newsize;
}

Rboolean is_growable(SEXP x) {
  switch(TYPEOF(x)) {
  case STRSXP:
  case INTSXP:
  case LGLSXP:
  case REALSXP:
  case CPLXSXP:
  case RAWSXP:
  case VECSXP:
    return R_altrep_inherits(x, type2class(TYPEOF(x)));
  default:
    return FALSE;
  }
}

SEXP make_growable(SEXP x) {
  SEXP ret = PROTECT(R_new_altrep(type2class(TYPEOF(x)), R_NilValue, R_NilValue));
  R_set_altrep_data1(ret, x);
  R_set_altrep_data2(ret, ScalarReal(XLENGTH(x)));
  SHALLOW_DUPLICATE_ATTRIB(ret, x);
  UNPROTECT(1);
  return ret;
}

#endif
