is.AsIs = function(x) {
  inherits(x, "AsIs")
}

substitute2 = function(expr, env, char.as.name=!is.AsIs(env), sub.names=TRUE) {
  if (missing(env)) {
    stop("TODO")
  } else if (is.environment(env)) {
    env = as.list(env, all.names=TRUE) ## todo: try to use environment rather than list
  } else if (!is.list(env)) {
    stop("'env' must be a list of an environment")
  }
  env.names = names(env)
  if (is.null(env.names)) {
    stop("'env' argument does not have names")
  } else if (!all(nzchar(env.names))) {
    stop("'env' argument has an zero char names")
  }
  if (isTRUE(char.as.name)) {
    asis = vapply(env, is.AsIs, FALSE)
    char = vapply(env, is.character, FALSE)
    to.name = !asis & char
    if (any(to.name)) {
      if (any(non.scalar.char <- vapply(env[to.name], length, 0L)!=1L)) {
        stop("'char.as.name' was used but the following character objects provided in 'env' are not scalar, if you need them as character vector rather a name, then use 'I' function: ",
             paste(names(non.scalar.char)[non.scalar.char], collapse=", "))
      }
      env[to.name] = lapply(env[to.name], as.name)
    }
  }
  # R substitute
  expr.sub = eval(substitute(
    substitute(.expr, env),
    env = list(.expr = substitute(expr))
  ))
  # new arg names substitute
  if (isTRUE(sub.names)) {
    #cat("entering substitute_call_arg_namesR\n")
    .Call(Csubstitute_call_arg_namesR, expr.sub, env)
  } else {
    expr.sub
  }
}
