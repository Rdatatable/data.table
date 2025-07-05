# 647 fast droplevels.data.table method
fdroplevels = function(x, exclude = if (anyNA(levels(x))) NULL else NA, ...) {
  stopifnot(inherits(x, "factor"))
  lev = which(tabulate(x, nlevels(x)) & (!match(levels(x), exclude, 0L)))
  ans = match(as.integer(x), lev)
  setattr(ans, 'levels', levels(x)[lev])
  setattr(ans, 'class', class(x))
  ans
}

droplevels.data.table = function(x, except=NULL, exclude, ...){
  x = copy(x)
  if (missing(exclude)) exclude = NULL
  setdroplevels(x, except, exclude)[]
}

setdroplevels = function(x, except=NULL, exclude=NULL) {
  if (!nrow(x)) return(invisible(x))
  ix = vapply_1b(x, is.factor)
  if (!is.null(except)) {
    stopifnot(is.numeric(except), except >= 1L, except <= length(x))
    ix[except] = FALSE
  }
  if (!any(ix)) return(invisible(x))
  for (nx in names(ix)[ix]) {
    set(x, i=NULL, j=nx, value=fdroplevels(x[[nx]], exclude=exclude))
  }
  invisible(x)
}
