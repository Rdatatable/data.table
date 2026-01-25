/* Ensure _RO accessors are used for assignment to 'const' variables */
@@
type T;
const T *variable;
expression E;
@@
- variable = REAL(E)
+ variable = REAL_RO(E)

@@
type T;
const T *variable;
expression E;
@@
- variable = INTEGER(E)
+ variable = INTEGER_RO(E)

@@
type T;
const T *variable;
expression E;
@@
- variable = COMPLEX(E)
+ variable = COMPLEX_RO(E)

@@
type T;
const T *variable;
expression E;
@@
- variable = RAW(E)
+ variable = RAW_RO(E)

@@
type T;
const T *variable;
expression E;
@@
- variable = LOGICAL(E)
+ variable = LOGICAL_RO(E)

/* Just use _RO accessors directly instead of 'const' casting the writeable one */
@@
expression E;
@@
-(const int*) INTEGER(E)
+INTEGER_RO(E)

@@
expression E;
@@
-(const double*) REAL(E)
+REAL_RO(E)

@@
expression E;
@@
-(const int*) LOGICAL(E)
+LOGICAL_RO(E)

@@
expression E;
type T;
@@
-(const T*) RAW(E)
+RAW_RO(E)

@@
expression E;
type T;
@@
-(const T*) COMPLEX(E)
+COMPLEX_RO(E)

@@
expression E;
type T;
@@
-(const T*) STRING_PTR(E)
+STRING_PTR_RO(E)
