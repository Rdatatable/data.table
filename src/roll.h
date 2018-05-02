#include <stdio.h>
#include <stdint.h>
//#include <stdlib.h>
#include <stdbool.h>
#include <math.h> // for isnan function
//#include <float.h> // for DBL_MAX, maybe not needed?

//#ifdef DTPY
//#include "py_roll.h" // github.com/h2oai/datatable/blob/master/c/*roll.h
//#else
//#include "rollR.h" // maybe we do not need that?
//#endif

/*
#define NA_BOOL8         INT8_MIN
#define NA_INT32         INT32_MIN
#define NA_INT64         INT64_MIN
*/
//#define NA_FLOAT64_I64   0x7FF00000000007A2

// taken from R core for NA and NaN tests

/* gcc had problems with static const on AIX and Solaris
   Solaris was for gcc 3.1 and 3.2 under -O2 32-bit on 64-bit kernel */
/*
#ifdef _AIX
#define CONST
#elif defined(sparc) && defined (__GNUC__) && __GNUC__ == 3
#define CONST
#else
#define CONST const
#endif
*/

//#ifdef WORDS_BIGENDIAN
//static CONST int hw = 0;
//static CONST int lw = 1;
//#else  /* !WORDS_BIGENDIAN */
//static CONST int hw = 1;
//static CONST int lw = 0;
//#endif /* WORDS_BIGENDIAN */

/*
typedef union
{
    double value;
    unsigned int word[2];
} ieee_double;
*/
// NaN or NA
//#define ISNAN(x) (isnan(x)!=0)

// #define ISFINITE R_FINITE
// https://github.com/wch/r-source/blob/trunk/src/main/arithmetic.c#L149
// require R_PosInf, R_NegInf, we do not need it yet

// not yet independent from R
#include "data.table.h"
#include <Rdefines.h>
