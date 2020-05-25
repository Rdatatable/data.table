# `i` argument could get with=FALSE #4485
wither = function(with) {
  w = list(i=NA, j=NA)
  if (!is.logical(with)) stop("'with' must be logical")
  lw = length(with)
  if (lw!=1L && lw!=2L) stop("'with' must be length 1 or 2")
  nw = names(with)
  if (!is.null(nw)) {
    nw = unique(nw)
    if (length(nw)!=lw) stop("'with' names must be unique")
    if (any(!nw%chin%c("i","j"))) stop("'with' names must be 'i' and/or 'j'")
    w[["i"]] = with["i"]
    w[["j"]] = with["j"]
  } else {
    if (lw==1L) {
      #w[["j"]] = with ## we don't do this line for backward compatibility, new 'with' optimization kicks in only for length 2 'with' or named 'with', been escaped already thus # nocov
    } else {
      w[["i"]] = with[1L]
      w[["j"]] = with[2L]
    }
  }
  w
}
hasNot = function(x) x%iscall%"!" || (x%iscall%"-" && length(x)==2L)
with_j_valid = function(subj, env, verbose) {
  bang = subj %iscall% "!"
  j = eval(subj[[2L]], env)
  j_ok = (is.logical(j) && bang) || (is.numeric(j) && !bang)
  if (!j_ok) {
    msg = "Your 'j' argument is"
    if (is.logical(j)) msg = paste(msg, "logical but has been used with unary '-', please change it to use '!' instead.")
    else if (is.numeric(j)) msg = paste(msg, "numeric but has been used with '!', please change it to use unary '-' instead.")
    else if (is.character(j)) msg = paste(msg, "character, please change it to use '!names(x) %in% j' instead.")
    else msg = paste(msg, "neither logical, numeric or character, please change it to be one of those.")
    stop(paste(msg, "Or simply not use new 'with' optimization by providing scalar unnamed logical. If you believe your use case is valid you can test it with 'options(datatable.with=2)', which might be default in future."))
  } else if (verbose) {
    cat("with[j]=FALSE used together with negation operator but the use case is valid: unary minus for numeric or bang for logical; not need to emit message\n")
  }
  j_ok
}
with_i = function(i, len, verbose) {
  if (!is.numeric(i) && !is.logical(i)) stop("'i' must be numeric or logical when with=FALSE")
  if (is.logical(i)) {
    if (length(i)!=len) stop("'i' logical must be length of rows for with=FALSE, it is not recycled") ## because scalar TRUE has special meaning
    i = which(i)
  } else {
    if (!is.integer(i)) i = as.integer(i)
    i = .Call(CconvertNegAndZeroIdx, i, len, TRUE)
  }
  if (verbose)
    cat("with[i]=FALSE optimization\n")
  i
}
with_j = function(j, len, x, verbose) {
  if (!is.numeric(j) && !is.logical(j) && !is.character(j)) stop("'j' must be numeric, logical or character when with=FALSE")
  if (anyNA(j)) stop("'j' must be non-NA")
  if (is.logical(j)) {
    if (length(j)!=len) stop("'j' logical must be length of columns for with=FALSE, it is not recycled")
    j = which(j)
  } else if (is.character(j)) {
    j = chmatch(j, names(x))
    if (anyNA(j)) stop("'j' must be existing columns")
  } else {
    if (!is.integer(j)) j = as.integer(j)
    j = .Call(CconvertNegAndZeroIdx, j, len, FALSE)
  }
  if (verbose)
    cat("with[j]=FALSE optimization\n")
  j
}
