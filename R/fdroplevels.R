# 647 fast droplevels.data.table method
fdroplevels = function(x, exclude = if (anyNA(levels(x))) NULL else NA, ...) {
  stopifnot(inherits(x, "factor"))
  lev = which(tabulate(x, length(levels(x))) & (!match(levels(x), exclude, 0L)))
  ans = match(as.integer(x), lev)
  setattr(ans, 'levels', levels(x)[lev])
  setattr(ans, 'class', class(x))
  return(ans)
}

droplevels.data.table = function(x, except = NULL, exclude, in.place = FALSE, ...){
    stopifnot(is.logical(in.place))
    if (nrow(x)==0L) return(x)
    ix = vapply(x, is.factor, NA)
    if(!is.null(except)){
        stopifnot(is.numeric(except), except <= length(x))
        ix[except] = FALSE
    }
    if(!sum(ix)) return(x)
    if(!in.place) x = copy(x)
    for(nx in names(ix)[ix==TRUE]){
        if (missing(exclude)) set(x, i = NULL, j = nx, value = fdroplevels(x[[nx]]))
        else set(x, i = NULL, j = nx, value = fdroplevels(x[[nx]], exclude = exclude))
    }
    return(x)
}
