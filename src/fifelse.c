#include "data.table.h"

SEXP fifelseR(SEXP l, SEXP a, SEXP b)
{
  // l is the test
  // a is what to do in case l is TRUE
  // b is what to do in case l is FALSE
  // maybe add something in case l is neither TRUE or FALSE ? for now it will default to NA

  // Check if test is logical
  if(!isLogical(l)) error("Argument 'test' must be logical.");

  const int64_t len0 = xlength(l);
  const int64_t len1 = xlength(a);
  const int64_t len2 = xlength(b);

  SEXPTYPE ta = TYPEOF(a);
  SEXPTYPE tb = TYPEOF(b);

  int nprotect = 0;

  // Check if same type and do en-listing of singleton
  if(ta != tb)
  {
    if(ta == VECSXP && (tb == INTSXP || tb == REALSXP || tb == LGLSXP || tb == CPLXSXP || tb == STRSXP))
    {
      if(len2 == 1)
      {
        SEXP tmp = PROTECT(allocVector(VECSXP,1)); nprotect++;
        SET_VECTOR_ELT(tmp, 0, b);
        b = tmp;
        tb = VECSXP;
      }
    } else if(tb == VECSXP && (ta == INTSXP || ta == REALSXP || ta == LGLSXP || ta == CPLXSXP || ta == STRSXP)){
      if(len1 == 1)
      {
        SEXP tmp = PROTECT(allocVector(VECSXP,1)); nprotect++;
        SET_VECTOR_ELT(tmp, 0, a);
        a = tmp;
        ta = VECSXP;
      }
    } else {
      error("'yes' is of type %s but 'no' is of type %s. Please make sure candidate replacements are of the same type.", type2char(ta),type2char(tb));
    }
  }

  SEXP class_type = PROTECT(getAttrib(a,R_ClassSymbol)); nprotect++;

  // Check if same class
  if(!R_compute_identical(class_type,getAttrib(b,R_ClassSymbol),0))
    error("'yes' has different class than 'no'. Please make sure that candidate replacements have the same class.");

  const int atest = isFactor(a);
  SEXP alevels = R_NilValue, blevels = R_NilValue, result = R_NilValue;

  // Check if factor
  if(atest)
  {
    alevels = PROTECT(getAttrib(a,R_LevelsSymbol)); nprotect++;
    blevels = PROTECT(getAttrib(b,R_LevelsSymbol)); nprotect++;
  }

  /*
   Variable 'adj' is used to seperate case where l (test),
   a (yes) and b (no) are of different length.
   */
  unsigned int adj = 0;

  // Check here the length of the different input variables.
  if(len1 != len2)
  {
    if(len1 < len2)
    {
      if(len1 != 1)    error("Length of 'yes' must be 1 or equal to length of 'test' (%d).", len0);
      if(len2 != len0) error("Length of 'no' must be 1 or equal to length of 'test' (%d).", len0);
      adj = 1;
    } else {
      if(len2 != 1)    error("Length of 'no' must be 1 or equal to length of 'test' (%d).", len0);
      if(len1 != len0) error("Length of 'yes' must be 1 or equal to length of 'test' (%d).", len0);
      adj = 2;
    }
  } else {
    if(len0 != len1)
    {
      if(len1 != 1) error("Length of 'yes' and 'no' must be 1 or equal to length of 'test' (%d).", len0);
      adj = 3;
    }
  }

  int *pl = LOGICAL(l);
  switch(ta)
  {
    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type INTSXP (integer)
     */
  case INTSXP :
    result = PROTECT(allocVector(INTSXP, len0)); nprotect++;
    int *int_pres = INTEGER(result);
    int *int_pa   = INTEGER(a);
    int *int_pb   = INTEGER(b);
    switch(adj)
    {
    case 0:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : int_pres[i] = int_pb[i];  break;
          case 1  : int_pres[i] = int_pa[i];  break;
          default : int_pres[i] = NA_INTEGER; break;
        }
      }
      break;

    case 1:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : int_pres[i] = int_pb[i];  break;
          case 1  : int_pres[i] = int_pa[0];  break;
          default : int_pres[i] = NA_INTEGER; break;
        }
      }
      break;

    case 2:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : int_pres[i] = int_pb[0];  break;
          case 1  : int_pres[i] = int_pa[i];  break;
          default : int_pres[i] = NA_INTEGER; break;
        }
      }
      break;

    case 3:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : int_pres[i] = int_pb[0];  break;
          case 1  : int_pres[i] = int_pa[0];  break;
          default : int_pres[i] = NA_INTEGER; break;
        }
      }
      break;
    }
    break;

    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type REALSXP (double)
     */
  case REALSXP :
    result = PROTECT(allocVector(REALSXP, len0)); nprotect++;
    double *double_pres = REAL(result);
    double *double_pa   = REAL(a);
    double *double_pb   = REAL(b);
    switch(adj)
    {
    case 0:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : double_pres[i] = double_pb[i];  break;
          case 1  : double_pres[i] = double_pa[i];  break;
          default : double_pres[i] = NA_REAL; break;
        }
      }
      break;

    case 1:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : double_pres[i] = double_pb[i];  break;
          case 1  : double_pres[i] = double_pa[0];  break;
          default : double_pres[i] = NA_REAL; break;
        }
      }
      break;

    case 2:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : double_pres[i] = double_pb[0];  break;
          case 1  : double_pres[i] = double_pa[i];  break;
          default : double_pres[i] = NA_REAL; break;
        }
      }
      break;

    case 3:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : double_pres[i] = double_pb[0];  break;
          case 1  : double_pres[i] = double_pa[0];  break;
          default : double_pres[i] = NA_REAL; break;
        }
      }
      break;
    }
    break;

    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type LGLSXP (logical)
     */
  case LGLSXP :
    result = PROTECT(allocVector(LGLSXP, len0)); nprotect++;
    int *lg_pres = LOGICAL(result);
    int *lg_pa   = LOGICAL(a);
    int *lg_pb   = LOGICAL(b);
    switch(adj)
    {
    case 0:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : lg_pres[i] = lg_pb[i];  break;
          case 1  : lg_pres[i] = lg_pa[i];  break;
          default : lg_pres[i] = NA_LOGICAL; break;
        }
      }
      break;

    case 1:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : lg_pres[i] = lg_pb[i];  break;
          case 1  : lg_pres[i] = lg_pa[0];  break;
          default : lg_pres[i] = NA_LOGICAL; break;
        }
      }
      break;

    case 2:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : lg_pres[i] = lg_pb[0];  break;
          case 1  : lg_pres[i] = lg_pa[i];  break;
          default : lg_pres[i] = NA_LOGICAL; break;
        }
      }
      break;

    case 3:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : lg_pres[i] = lg_pb[0];  break;
          case 1  : lg_pres[i] = lg_pa[0];  break;
          default : lg_pres[i] = NA_LOGICAL; break;
        }
      }
      break;
    }
    break;

    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type STRSXP (character)
     */
  case STRSXP :
    result = PROTECT(allocVector(STRSXP, len0)); nprotect++;
    const SEXP *pa = STRING_PTR(a);
    const SEXP *pb = STRING_PTR(b);
    switch(adj)
    {
    case 0:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_STRING_ELT(result, i, pb[i]);  break;
          case 1  : SET_STRING_ELT(result, i, pa[i]);  break;
          default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;

    case 1:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_STRING_ELT(result, i, pb[i]);  break;
          case 1  : SET_STRING_ELT(result, i, pa[0]);  break;
          default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;

    case 2:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_STRING_ELT(result, i, pb[0]);  break;
          case 1  : SET_STRING_ELT(result, i, pa[i]);  break;
          default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;

    case 3:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_STRING_ELT(result, i, pb[0]);  break;
          case 1  : SET_STRING_ELT(result, i, pa[0]);  break;
          default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;
    }
    break;

    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type CPLXSXP (complex)
     */
  case CPLXSXP :
    result = PROTECT(allocVector(CPLXSXP, len0)); nprotect++;
    Rcomplex *cp_pres = COMPLEX(result);
    Rcomplex *cp_pa   = COMPLEX(a);
    Rcomplex *cp_pb   = COMPLEX(b);
    switch(adj)
    {
    case 0:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : cp_pres[i] = cp_pb[i]; break;
          case 1  : cp_pres[i] = cp_pa[i]; break;
          default : cp_pres[i] = NA_CPLX;  break;
        }
      }
      break;

    case 1:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : cp_pres[i] = cp_pb[i]; break;
          case 1  : cp_pres[i] = cp_pa[0]; break;
          default : cp_pres[i] = NA_CPLX;  break;
        }
      }
      break;

    case 2:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : cp_pres[i] = cp_pb[0]; break;
          case 1  : cp_pres[i] = cp_pa[i]; break;
          default : cp_pres[i] = NA_CPLX;  break;
        }
      }
      break;

    case 3:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : cp_pres[i] = cp_pb[0]; break;
          case 1  : cp_pres[i] = cp_pa[0]; break;
          default : cp_pres[i] = NA_CPLX;  break;
        }
      }
      break;
    }
    break;

    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type VECSXP (list)
     */
  case VECSXP :
    result = PROTECT(allocVector(VECSXP, len0)); nprotect++;
    SEXP nms = PROTECT(allocVector(STRSXP, len0)); nprotect++; // create list names
    // Get list names from a and b
    SEXP nmsa = PROTECT(getAttrib(a,R_NamesSymbol)); nprotect++;
    SEXP nmsb = PROTECT(getAttrib(b,R_NamesSymbol)); nprotect++;
    SEXP na_list = PROTECT(allocVector(INTSXP, 1)); nprotect++;
    INTEGER(na_list)[0] = NA_INTEGER;
    const int testa = !isNull(nmsa);
    const int testb = !isNull(nmsb);
    switch(adj)
    {
    case 0:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_VECTOR_ELT(result, i, VECTOR_ELT(b, i)); if(testb)SET_STRING_ELT(nms, i, STRING_ELT(nmsb,i)); break;
          case 1  : SET_VECTOR_ELT(result, i, VECTOR_ELT(a, i)); if(testa)SET_STRING_ELT(nms, i, STRING_ELT(nmsa,i)); break;
          default : SET_VECTOR_ELT(result, i, na_list); break;
        }
      }
      break;

    case 1:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_VECTOR_ELT(result, i, VECTOR_ELT(b, i)); if(testb)SET_STRING_ELT(nms, i, STRING_ELT(nmsb,i)); break;
          case 1  : SET_VECTOR_ELT(result, i, VECTOR_ELT(a, 0)); if(testa)SET_STRING_ELT(nms, i, STRING_ELT(nmsa,0)); break;
          default : SET_VECTOR_ELT(result, i, na_list); break;
        }
      }
      break;

    case 2:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_VECTOR_ELT(result, i, VECTOR_ELT(b, 0)); if(testb)SET_STRING_ELT(nms, i, STRING_ELT(nmsb,0)); break; //some speed up can be done here
          case 1  : SET_VECTOR_ELT(result, i, VECTOR_ELT(a, i)); if(testa)SET_STRING_ELT(nms, i, STRING_ELT(nmsa,i)); break;
          default : SET_VECTOR_ELT(result, i, na_list); break;
        }
      }
      break;

    case 3:
      for(int64_t i=0; i<len0; ++i)
      {
        switch(pl[i])
        {
          case 0  : SET_VECTOR_ELT(result, i, VECTOR_ELT(b, 0)); if(testb)SET_STRING_ELT(nms, i, STRING_ELT(nmsb,0)); break;
          case 1  : SET_VECTOR_ELT(result, i, VECTOR_ELT(a, 0)); if(testa)SET_STRING_ELT(nms, i, STRING_ELT(nmsa,0)); break;
          default : SET_VECTOR_ELT(result, i, na_list); break;
        }
      }
      break;
    }
    if(testa || testb) setAttrib(result, R_NamesSymbol, nms);
    break;

  default: error("Type %s is not supported.",type2char(ta)); break;
  }

  // Check class type and adjust (including factors)
  if(!isNull(class_type))
  {
    if(atest)
    {
      if(!R_ExternalPtrAddr(alevels))
      {
        setAttrib(result, R_LevelsSymbol, blevels);
      } else {
        setAttrib(result, R_LevelsSymbol, alevels);
      }
      setAttrib(result, R_ClassSymbol, mkString("factor"));
    } else {
      copyMostAttrib(a, result);
    }
  }

  UNPROTECT(nprotect);
  return result;
}

