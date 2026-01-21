@malloc_return_value_cast expression@
type T;
expression E;
@@
- (T)
  malloc(E)

@calloc_realloc_return_value_cast expression@
type T;
expression E1, E2;
identifier alloc =~ "^(c|re)alloc$";
@@
- (T)
  alloc(E1, E2)
