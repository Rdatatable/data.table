/* Ensure _RO accessors are used for assignment to 'const' variables */
@@
type T;
const T *variable;
expression E;
@@
  variable =
- RAW(E)
+ RAW_RO(E)

@@
type T;
const T *variable;
expression E;
@@
  variable =
- LOGICAL(E)
+ LOGICAL_RO(E)

@@
type T;
const T *variable;
expression E;
@@
  variable =
- INTEGER(E)
+ INTEGER_RO(E)

@@
type T;
const T *variable;
expression E;
@@
  variable =
- REAL(E)
+ REAL_RO(E)

@@
type T;
const T *variable;
expression E;
@@
  variable =
- COMPLEX(E)
+ COMPLEX_RO(E)

@@
type T;
const T *variable;
expression E;
@@
  variable =
- STRING_PTR(E)
+ STRING_PTR_RO(E)

/* Just use _RO accessors directly instead of 'const' casting the writeable one */
@@
expression E;
type T;
@@
-(const T*) RAW(E)
+RAW_RO(E)

@@
expression E;
@@
-(const int*) LOGICAL(E)
+LOGICAL_RO(E)

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
