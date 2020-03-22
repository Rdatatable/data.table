is.AsIs = function(x) {
  inherits(x, "AsIs")
}

substitute2 = function(expr, env) {
  if (missing(env)) {
    stop("TODO, as of now 'env' should not be missing")
  } else if (is.environment(env)) {
    env = as.list(env, all.names=TRUE) ## todo: try to use environment rather than list
  } else if (!is.list(env)) {
    stop("'env' must be a list or an environment")
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
        stop("'char.as.name' was used but the following character objects provided in 'env' are not scalar objects, if you need them as character vector rather a name, then use 'I' function: ",
             paste(names(non.scalar.char)[non.scalar.char], collapse=", "))
      }
      env[to.name] = lapply(env[to.name], as.name)
    }
    if (any(asis)) {
      rm.AsIs = function(x) { ## removes any AsIs class
        cl = oldClass(x)
        oldClass(x) = cl[cl!="AsIs"]
        x
      }
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
