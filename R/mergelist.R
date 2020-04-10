cbindlist = function(x, copy=TRUE) {
  ans = .Call(Ccbindlist, x, copy)
  setDT(ans)
  ans
}

mergelist = function(x, on, how) {
  # note that 'mult' is fixed to "first"
  # expand or subset each DT by joining to complete columns set specified in 'on'
  # now all DT share the same order and row count so we can safely stack them by cbind
  stopifnot(is.list(x))
  if (!length(x)) return(x)
  stopifnot(vapply(x, is.data.table, FALSE))
  if (length(x)<2L) return(copy(x))
  stopifnot(is.character(on), vapply(x, function(x) all(on %chin% names(x)), FALSE))
  stopifnot(is.character(how), length(how)==1L, how %chin% c("inner","left","right","full"))
  nomatch = NA
  if (how=="full") { # full outer join
    i = lapply(x, function(x) x[, on, with=FALSE])
    i = unique(rbindlist(i))
  } else if (how=="inner") { # inner join
    i = lapply(x, function(x) x[, on, with=FALSE])
    i = Reduce(fintersect, i)
    nomatch = NULL
  } else if (how=="left") { # left outer join
    i = x[[1L]][, unique(.SD), .SDcols=on]
  } else if (how=="right") { # right outer join
    i = x[[length(x)]][, unique(.SD), .SDcols=on]
  }
  expand = function(x, i, on, nomatch) x[i, on=on, nomatch=nomatch, mult="first", .SD, .SDcols=setdiff(names(x), on)]
  cbindlist(c(list(i), lapply(x, expand, i=i, on=on, nomatch=nomatch)), copy=FALSE)
}
