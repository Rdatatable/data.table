@malloc_return_value_cast expression@
type T;
expression E;
@@
- (T)
  malloc(E)

- (T)
  calloc(_, E)

- (T)
  realloc(_, E)
