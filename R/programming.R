substitute2 = function(expr, env, char.as.name=FALSE, sub.names=TRUE) {
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
  } else if (any(!nzchar(env.names))) {
    stop("'env' argument has an zero char names")
  }
  if (isTRUE(char.as.name)) {
    char = vapply(env, is.character, FALSE)
    if (any(char)) {
      if (any(non.scalar.char <- lengths(env[char])!=1L)) {
        stop("'char.as.name' was used but the following character elements provided in 'env' are not scalar: ",
             paste(names(non.scalar.char)[non.scalar.char], collapse=", "))
      }
      env[char] = lapply(env[char], as.name)
    }
  }
  expr.sub = eval(substitute(substitute(expr, env)))
  if (isTRUE(sub.names)) {
    .Call(Csubstitute_call_arg_namesR, expr.sub, env)
  } else {
    expr.sub
  }
}
