#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>
#include<stdint.h>

SEXP fifelseR(SEXP l, SEXP a, SEXP b)
{
  // l is the test
  // a is what to do in case l is TRUE
  // b is what to do in case l is FALSE
  // maybe add something in case l is neither TRUE or FALSE ? for now it will default to NA
  
  // Check if test is logical
  if(!isLogical(l)) error("Argument 'test' must be logical.");
  
  SEXPTYPE ta = TYPEOF(a); 
  
  // Check if same type
  if(ta != TYPEOF(b))
    error("Item 'yes' is of type %s but item 'no' is of type %s. Please make sure they are of the same type", type2char(ta),type2char(TYPEOF(b)));
  
  SEXP class_type = PROTECT(getAttrib(a,R_ClassSymbol));
  unsigned int stack_size = 1;
  
  // Check if same class
  if(!R_compute_identical(class_type,getAttrib(b,R_ClassSymbol),0))
    error("Item 'yes' has different class than item 'no'. Please make sure that they have same class type.");
  
  // Check if factor
  if(isFactor(a))
  {
    a = PROTECT(asCharacterFactor(a));
    b = PROTECT(asCharacterFactor(b));
    stack_size = stack_size + 2;
    ta = STRSXP;
  }
  /*Jan Gorecki : Afair this will make factor class always slower than character, would be nice to have it optimised where possible*/
  
  const uint64_t len0 = LENGTH(l);
  const uint64_t len1 = LENGTH(a);
  const uint64_t len2 = LENGTH(b);
  uint64_t i;
  
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
      if(len1 != 1)    error("Length of 'yes' must be 1 or equal to length of 'test'.");
      if(len2 != len0) error("Length of 'no' must be 1 or equal to length of 'test'.");
      adj = 1;
    } else {
      if(len2 != 1)    error("Length of 'no' must be 1 or equal to length of 'test'.");
      if(len1 != len0) error("Length of 'yes' must be 1 or equal to length of 'test'.");
      adj = 2;
    }
  } else {
    if(len0 != len1)
    {
      if(len1 != 1) error("Length of 'yes' and 'no' must be 1 or equal to length of 'test'.");
      adj = 3;
    }
  }
  
  SEXP result = R_NilValue;
  int *pl = LOGICAL(l);
  
  int *int_pres;
  int *int_pa;
  int *int_pb;
  
  double *double_pres;
  double *double_pa;
  double *double_pb;
  
  switch(ta)
  {
    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type INTSXP (integer)
     */
  case INTSXP :
    result = PROTECT(allocVector(INTSXP, len0));
    stack_size++;
    int_pres = INTEGER(result);
    int_pa   = INTEGER(a);
    int_pb   = INTEGER(b);
    switch(adj)
    {
    case 0:
      for(i = 0; i < len0; i++)
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
      for(i = 0; i < len0; i++)
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
      for(i = 0; i < len0; i++)
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
      for(i = 0; i < len0; i++)
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
    result = PROTECT(allocVector(REALSXP, len0));
    stack_size++;
    double_pres = REAL(result);
    double_pa   = REAL(a);
    double_pb   = REAL(b);
    switch(adj)
    {
    case 0:
      for(i = 0; i < len0; i++)
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
      for(i = 0; i < len0; i++)
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
      for(i = 0; i < len0; i++)
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
      for(i = 0; i < len0; i++)
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
    result = PROTECT(allocVector(LGLSXP, len0));
    stack_size++;
    int_pres = LOGICAL(result);
    int_pa   = LOGICAL(a);
    int_pb   = LOGICAL(b);
    switch(adj)
    {
    case 0:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : int_pres[i] = int_pb[i];  break;
        case 1  : int_pres[i] = int_pa[i];  break;
        default : int_pres[i] = NA_LOGICAL; break;
        }
      }
      break;
      
    case 1:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : int_pres[i] = int_pb[i];  break;
        case 1  : int_pres[i] = int_pa[0];  break;
        default : int_pres[i] = NA_LOGICAL; break;
        }
      }
      break;
      
    case 2:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : int_pres[i] = int_pb[0];  break;
        case 1  : int_pres[i] = int_pa[i];  break;
        default : int_pres[i] = NA_LOGICAL; break;
        }
      }
      break;
      
    case 3:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : int_pres[i] = int_pb[0];  break;
        case 1  : int_pres[i] = int_pa[0];  break;
        default : int_pres[i] = NA_LOGICAL; break;
        }
      }
      break;
    }
    break;
    
    /*
     This part is for dealing with a (yes) and b (no)
     in case they are both of type STRSXP (logical)
     */
  case STRSXP :
    result = PROTECT(allocVector(STRSXP, len0));
    stack_size++;
    switch(adj)
    {
    case 0:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : SET_STRING_ELT(result, i, STRING_ELT(b, i));  break;
        case 1  : SET_STRING_ELT(result, i, STRING_ELT(a, i));  break;
        default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;
      
    case 1:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : SET_STRING_ELT(result, i, STRING_ELT(b, i));  break;
        case 1  : SET_STRING_ELT(result, i, STRING_ELT(a, 0));  break;
        default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;
      
    case 2:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : SET_STRING_ELT(result, i, STRING_ELT(b, 0));  break;
        case 1  : SET_STRING_ELT(result, i, STRING_ELT(a, i));  break;
        default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;
      
    case 3:
      for(i = 0; i < len0; i++)
      {
        switch(pl[i])
        {
        case 0  : SET_STRING_ELT(result, i, STRING_ELT(b, 0));  break;
        case 1  : SET_STRING_ELT(result, i, STRING_ELT(a, 0));  break;
        default : SET_STRING_ELT(result, i, NA_STRING); break;
        }
      }
      break;
    }
    break;
    
  default: error("Type %s is not supported.",type2char(ta)); break;
  }
  
  // Check if class type is Date and adjust
  if(!isNull(class_type) && stack_size < 3) copyMostAttrib(a, result); // issue here for factor with NA value
    //if( CHAR(STRING_ELT(class_type,0)) == CHAR(STRING_ELT(Rf_mkString("Date"),0)) )
      //setAttrib(result, R_ClassSymbol, mkString("Date"));
    
  UNPROTECT(stack_size);
  return result;
}
