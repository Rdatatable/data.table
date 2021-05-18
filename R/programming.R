is.AsIs = function(x) {
  inherits(x, "AsIs")
}
rm.AsIs = function(x) {
  cl = oldClass(x)
  oldClass(x) = cl[cl!="AsIs"]
  x
}
list2lang = function(x) {
  if (!is.list(x))
    stop("'x' must be a list")
  if (is.AsIs(x))
    return(rm.AsIs(x))
  asis = vapply(x, is.AsIs, FALSE)
  char = vapply(x, is.character, FALSE)
  to.name = !asis & char
  if (any(to.name)) { ## turns "my_name" character scalar into `my_name` symbol, for convenience
    if (any(non.scalar.char <- vapply(x[to.name], length, 0L)!=1L)) {
      stop("Character objects provided in the input are not scalar objects, if you need them as character vector rather than a name, then wrap each into 'I' call: ",
           paste(names(non.scalar.char)[non.scalar.char], collapse=", "))
    }
    x[to.name] = lapply(x[to.name], as.name)
  }
  if (isTRUE(getOption("datatable.enlist", TRUE))) { ## recursively enlist for nested lists, see note section in substitute2 manual
    islt = vapply(x, is.list, FALSE)
    to.enlist = !asis & islt
    if (any(to.enlist)) {
      x[to.enlist] = lapply(x[to.enlist], enlist)
    }
  }
  if (any(asis)) {
    x[asis] = lapply(x[asis], rm.AsIs)
  }
  x
}
enlist = function(x) {
  if (!is.list(x))
    stop("'x' must be a list")
  if (is.AsIs(x))
    return(rm.AsIs(x))
  as.call(c(quote(list), list2lang(x)))
}

substitute2 = function(expr, env) {
  if (missing(expr))
    return(substitute())
  if (missing(env)) {
    stop("'env' must not be missing")
  } else if (is.null(env)) {
    # null is fine, will be escaped few lines below
  } else if (is.environment(env)) {
    env = as.list(env, all.names=TRUE, sorted=TRUE)
  } else if (!is.list(env)) {
    stop("'env' must be a list or an environment")
  }
  if (!length(env)) {
    return(substitute(expr))
  }
  env.names = names(env)
  if (is.null(env.names)) {
    stop("'env' argument does not have names")
  } else if (!all(nzchar(env.names))) {
    stop("'env' argument has zero char names")
  } else if (anyNA(env.names)) {
    stop("'env' argument has NA names")
  } else if (anyDuplicated(env.names)) {
    stop("'env' argument has duplicated names")
  }
  # character to name/symbol, and list to list call
  env = list2lang(env)
  # R substitute
  expr.sub = eval(substitute(
    substitute(.expr, env),
    env = list(.expr = substitute(expr))
  ))
  if (missing(expr.sub))
    return(substitute()) ## nested emptiness
  # substitute call argument names
  .Call(Csubstitute_call_arg_namesR, expr.sub, env)
}
