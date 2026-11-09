/* Hoist accessor calls on loop-invariant variables out of loops */
@accessed@
identifier acc = {RAW, RAW_RO, LOGICAL, LOGICAL_RO, INTEGER, INTEGER_RO, REAL, REAL_RO, COMPLEX, COMPLEX_RO, STRING_PTR_RO};
idexpression x;
@@
  acc(x)
@@
identifier acc = {RAW, RAW_RO, LOGICAL, LOGICAL_RO, INTEGER, INTEGER_RO, REAL, REAL_RO, COMPLEX, COMPLEX_RO, STRING_PTR_RO};
idexpression accessed.x;
expression I, e;
constant C;
statement S;
@@
(
  for (...;...;x = e) S
|
  for (...;...;...) {
    ... when any
    x = e
    ... when any
  }
|
  while (...) {
    ... when any
    x = e
    ... when any
  }
|
  do {
    ... when any
    x = e
    ... when any
  } while (...);
|
  for (...;...;...) {
    ... when any
(
    acc(x)[C]
|
*   acc(x)[I]
)
    ... when any
  }
|
  while (...) {
    ... when any
(
    acc(x)[C]
|
*   acc(x)[I]
)
    ... when any
  }
|
  do {
    ... when any
(
    acc(x)[C]
|
*   acc(x)[I]
)
    ... when any
  } while (...);
|
  for (...; <+... acc(x)[C] ...+>; ...) S
|
  while (<+... acc(x)[C] ...+>) S
|
  for (...;
       <+...
*      acc(x)[I]
       ...+>;
       ...) S
|
  while (
       <+...
*      acc(x)[I]
       ...+>) S
)
