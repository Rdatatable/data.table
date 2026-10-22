@@
identifier acc = {RAW, RAW_RO, LOGICAL, LOGICAL_RO, INTEGER, INTEGER_RO, REAL, REAL_RO, COMPLEX, COMPLEX_RO, STRING_PTR_RO};
idexpression x;
expression I, e;
@@
(
  for (...;...;...) { <+... when != x = e
*   acc(x)[I]
  ...+> }
|
  while (...) { <+... when != x = e
*   acc(x)[I]
  ...+> }
|
  do { <+... when != x = e
*   acc(x)[I]
  ...+> } while (...);
)
