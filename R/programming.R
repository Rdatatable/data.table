is.AsIs = function(x) {
  inherits(x, "AsIs")
}
rm.AsIs = function(x) {
  cl = oldClass(x)
  oldClass(x) = cl[cl!="AsIs"]
  x
}

substitute2 = function(expr, env) {
  if (missing(env)) {
    stop("'env' must not be missing")
  } else if (is.null(env)) {
    # null is fine, will be escaped few lines below
  } else if (is.environment(env)) {
    env = as.list(env, all.names=TRUE) ## todo: try to use environment rather than list
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
  if (!is.AsIs(env)) {
    asis = vapply(env, is.AsIs, FALSE)
    char = vapply(env, is.character, FALSE)
    to.name = !asis & char
    if (any(to.name)) { ## turns "my_name" character scalar into `my_name` symbol, for convenience
      if (any(non.scalar.char <- vapply(env[to.name], length, 0L)!=1L)) {
        stop("Character objects provided in 'env' are not scalar objects, if you need them as character vector rather than a name, then use wrap it into 'I' call: ",
             paste(names(non.scalar.char)[non.scalar.char], collapse=", "))
      }
      env[to.name] = lapply(env[to.name], as.name)
    }
    if (any(asis)) {
      env[asis] = lapply(env[asis], rm.AsIs)
    }
  }
  # R substitute
  expr.sub = eval(substitute(
    substitute(.expr, env),
    env = list(.expr = substitute(expr))
  ))
  # call arg names substitute
  .Call(Csubstitute_call_arg_namesR, expr.sub, env)
}
