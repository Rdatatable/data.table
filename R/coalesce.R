coalesce <- function(x, ...) {
  if (!anyNA(x) || missing(..1)) return(x)
  
  count_na = sum(is.na(x))
  
  values = list(...)
  
  nn = length(x)
  # can be lengths() in R 3.2.0
  lengths = vapply_1i(values, length)
  if (any(idx <- lengths > 1L & lengths < nn)) {
    first_idx = which(idx)
    stop("Replacement ", first_idx, " has length ", lengths[first_idx], 
         " but length(x) is ", lx, ". Only singletons will be recycled")
  }
  
  x_type = typeof(x)
  x_factor = inherits(x, 'factor')
  
  types = vapply_1c(values, typeof)
  if (!all(idx <- x_type == types)) {
    first_idx = which(!idx)
    stop("Replacement ", first_idx, " has internal type ", types[first_idx],
         " but typeof(x) is '", x_type, "'. Please coerce before coalescing.")
  }
  if (x_type == 'integer' && !all(idx <- x_factor == vapply_1b(values, inherits, 'factor'))) {
    first_idx = which(!idx)
    stop("Replacement ", first_idx, ' is ', if (x_factor) 'not ' else '',
         "a factor, but x is", if (x_factor) "." else "not.",
         " Please ensure type consistency on your inputs.")
  }
  
  .Call(Ccoalesce, x, values, nn, count_na)
}