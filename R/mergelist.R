cbindlist = function(l, copy=TRUE) {
  ans = .Call(Ccbindlist, l, copy)
  setDT(ans)
  ans
}

# when 'on' is missing then use keys, used only for inner and full join
onkeys = function(x, y) {
  if (is.null(x) && !is.null(y)) y
  else if (!is.null(x) && is.null(y)) x
  else if (!is.null(x) && !is.null(y)) {if (length(x)>=length(y)) intersect(y, x) else intersect(x, y)} ## align order to shorter vector
  else NULL
}
someCols = function(x, cols, drop=character(), keep=character()) {
  keep = colnamesInt(x, keep)
  drop = colnamesInt(x, drop)
  cols = colnamesInt(x, cols)
  union(keep, setdiff(cols, drop))
}
hasindex = function(x, by, retGrp=FALSE) {
  index = attr(x, "index", TRUE)
  if (is.null(index)) return(FALSE)
  idx_name = paste0("__",by,collapse="")
  idx = attr(index, idx_name, TRUE)
  if (is.null(idx)) return(FALSE)
  if (!retGrp) return(TRUE)
  return(!is.null(attr(idx, "starts", TRUE)))
}

# fdistinct applies mult='first|last'
# for mult='first' it is unique(x, by=on)[, cols, with=FALSE]
# it may not copy when copy=FALSE and x is unique by 'on'
fdistinct = function(x, on=key(x), mult=c("first","last"), cols=seq_along(x), copy=TRUE) {
  if (!perhaps.data.table(x))
    stop("'x' must be data.table type object")
  if (!is.character(on) || !length(on) || anyNA(on) || any(!on%chin%names(x)))
    stop("'on' must be character column names of 'x' argument")
  mult = match.arg(mult)
  if (is.null(cols))
    cols = seq_along(x)
  else if (!(is.character(cols) || is.integer(cols)) || !length(cols) || anyNA(cols))
    stop("'cols' must be non-zero length, non-NA, integer or character columns of 'x' argument")
  if (!is.logical(copy) || length(copy)!=1L || is.na(copy))
    stop("'copy' must be TRUE or FALSE")
  ## do not compute sort=F for mult="first" if index (sort=T) already available, sort=T is needed only for mult="last"
  ## this short circuit will work after #4386 because it requires retGrp=T
  sort = hasindex(x, by=on, retGrp=TRUE) || mult!="first"
  o = forderv(x, by=on, sort=sort, retGrp=TRUE)
  if (attr(o, "maxgrpn", TRUE) <= 1L) {
    ans = .shallow(x, someCols(x, cols, keep=on), retain.key=TRUE)
    if (copy) ans = copy(ans)
    return(ans)
  }
  f = attr(o, "starts", exact=TRUE)
  if (mult=="last") {
    if (!sort) stop("internal error: sort must be TRUE when computing mult='last'") # nocov
    f = c(f[-1L]-1L, nrow(x)) ## last of each group
  }
  if (length(o)) f = o[f]
  if (sort && length(o <- forderv(f))) f = f[o] ## this rolls back to original order
  .Call(CsubsetDT, x, f, someCols(x, cols, keep=on))
}

# extra layer over bmerge to provide ready to use row index (or NULL for 1:nrow)
# NULL to avoid extra copies in downstream code, it turned out that avoiding copies precisely is costly and enormously complicates code
# could be re-used in [.data.table to simplify merges there
# we don't use semi join in mergelist so is not tested, anti join is used in full outer join
dtmatch = function(x, i, on, nomatch, mult, verbose, allow.cartesian, semi=FALSE, anti=FALSE, void=FALSE) {
  nrow = function(x) if (is.null(ans<-base::nrow(x))) stop("nrow called on a list? not good") else ans
  inner = identical(nomatch, 0L)
  if (void && mult!="error")
    stop("internal error: void must be used with mult='error'") # nocov
  if ((semi || anti) && !inner)
    stop("internal error: semi and anti joins must be used with nomatch=0L") # nocov
  icols = colnamesInt(i, on, check_dups=TRUE)
  xcols = colnamesInt(x, on, check_dups=TRUE)
  ans = bmerge(i, x, icols, xcols, roll=0, rollends=c(FALSE, TRUE), nomatch=nomatch, mult=mult, ops=1L, verbose=verbose)
  if (void) ## void=T is only for the case when we want raise error for mult='error', and that would happen in above line
    return(invisible(NULL))

  if (DEVEL<-FALSE) { ## some of those adds an overhead, keep it like this for documentation
    # nocov start
    if (!identical(ans$indices, integer()))
      stop("internal error: bmerge()$indices should be integer()")
    if (!isTRUE(ans$allGrp1))
      stop("internal error: bmerge()$allGrp1 should be TRUE")
    if (mult!="all" && !ans$allLen1)
      stop("internal error: bmerge()$allLen1 should be TRUE for mult!='all'")
    if (mult!="all" && is.na(nomatch) && any(ans$lens!=1L))
      stop("internal error: bmerge()$lens should be all 1L for mult!='all' && nomatch==NA")
    if (mult!="all" && identical(nomatch, 0L) && any(!ans$lens%in%c(0L,1L)))
      stop("internal error: bmerge()$lens should be all 0L or 1L for mult!='all' && nomatch==0L")
    # nocov end
  }

  ## semi and anti short-circuit
  if (semi || anti) {
    ## we subset i rather than x, thus assign to irows, not to xrows
    irows = which(if (semi) ans$lens!=0L else ans$lens==0L)
    return(list(ans = ans, irows = irows))
  }

  ## xrows, join-to
  xrows = if (ans$allLen1) ans$starts else vecseq(ans$starts, ans$lens, NULL)
  if (mult=="all" && inner && ans$allLen1) xrows = xrows[xrows != 0L]
  if (mult!="all" && inner) xrows = xrows[ans$lens > 0L]
  len.x = length(xrows)

  ## irows, join-from
  irows = if (!(ans$allLen1 && (!inner || length(xrows)==length(ans$starts)))) seqexp(ans$lens)
  len.i = if (is.null(irows)) nrow(i) else length(irows)

  if (length(ans$xo) && length(xrows))
    xrows = ans$xo[xrows]
  len.x = length(xrows)

  if (len.i!=len.x) {
    if (interactive()) {message("dtmatch out len.i != len.x"); browser()}
    stop("dtmatch out len.i != len.x")
  }
  return(list(ans = ans, irows = irows, xrows = xrows))
}

# atomic join between two tables
mergepair = function(lhs, rhs, on, how, mult, lhs.cols=names(lhs), rhs.cols=names(rhs), copy=TRUE, allow.cartesian=FALSE, verbose=FALSE) {
  if (is.null(on)) {
    if (how=="left") on = key(rhs)
    else if (how=="right") on = key(lhs)
    else on = onkeys(key(lhs), key(rhs))
    if (is.null(on))
      stop("'on' is missing and necessary keys were not present")
  }
  if (how!="right") { ## join-to, join-from
    jnfm = lhs; fm.cols = lhs.cols; jnto = rhs; to.cols = rhs.cols
  } else {
    jnfm = rhs; fm.cols = rhs.cols; jnto = lhs; to.cols = lhs.cols
  }
  nomatch = if (how=="inner") 0L else NA_integer_

  if (mult!="all" && (how=="inner" || how=="full")) { ## for inner|full join we apply mult on both tables, bmerge do only 'i' table
    if (mult=="first" || mult=="last") {
      jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE) ## might not copy when already unique by 'on'
    } else if (mult=="error") {
      ## we do this branch only to raise error from bmerge, we cannot use forder to just find duplicates because those duplicates might not have matching rows in another table
      dtmatch(x=jnfm, i=jnto, on=on, nomatch=nomatch, mult=mult, verbose=verbose, allow.cartesian=allow.cartesian, void=TRUE)
    }
  }

  ## binary merge
  ans = dtmatch(x=jnto, i=jnfm, on=on, nomatch=nomatch, mult=mult, verbose=verbose, allow.cartesian=allow.cartesian)

  ## make i side
  cp.i = !is.null(ans$irows)
  out.i = if (!cp.i)
    .shallow(jnfm, cols=someCols(jnfm, fm.cols, keep=on), retain.key=TRUE)
  else
    .Call(CsubsetDT, jnfm, ans$irows, someCols(jnfm, fm.cols, keep=on))

  ## make x side
  cp.x = !is.null(ans$xrows)
  out.x = if (!cp.x)
    .shallow(jnto, cols=someCols(jnto, to.cols, drop=on))
  else
    .Call(CsubsetDT, jnto, ans$xrows, someCols(jnto, to.cols, drop=on))

  if (how!="full") {
    if (!cp.i && isTRUE(copy))
      out.i = copy(out.i)
    if (!cp.x && !is.na(copy))
      out.x = copy(out.x)

    ## stack i and x
    if (how=="right") {
      out = .Call(Ccbindlist, list(.shallow(out.i, cols=on), out.x, .shallow(out.i, cols=someCols(jnfm, fm.cols, drop=on))), FALSE) ## arrange columns: i.on, x.cols, i.cols
    } else {
      out = .Call(Ccbindlist, list(out.i, out.x), FALSE)
    }
  } else { # how=="full"
    ## we made left join already, proceed to right join, actually just swap tbls and inner
    jnfm = rhs; fm.cols = rhs.cols; jnto = lhs; to.cols = lhs.cols
    if (mult=="first" || mult=="last")
      jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE)
    ## mult=="error" check not needed anymore, both sides has been already checked

    ## binary merge anti join
    bns = dtmatch(x=jnto, i=jnfm, on=on, nomatch=0L, mult=mult, verbose=verbose, allow.cartesian=allow.cartesian, anti=TRUE)

    ## make anti join side
    cp.r = !is.null(bns$irows)
    out.r = if (!cp.r)
      .shallow(jnfm, cols=someCols(jnfm, fm.cols, keep=on))
    else
      .Call(CsubsetDT, jnfm, bns$irows, someCols(jnfm, fm.cols, keep=on))

    if (!nrow(out.r)) { ## short circuit to avoid rbindlist to empty sets
      if (!cp.i && isTRUE(copy))
        out.i = copy(out.i)
      if (!cp.x && !is.na(copy))
        out.x = copy(out.x)
      out = .Call(Ccbindlist, list(out.i, out.x), FALSE)
    } else { ## all might still not be copied, rbindlist will do
      out.l = .Call(Ccbindlist, list(out.i, out.x), FALSE)
      ## we could replace rbindlist for new alloc of nrow(out.l)+nrow(out.r) and then memcpy into it
      out = rbindlist(list(out.l, out.r), use.names=TRUE, fill=TRUE)
    }
  }
  out
}

# TODO:
# rework dtmatch: rm allow.cartesian?
# missing on, key of join-to
# vectorized length(l)-1L: on, how, mult
# copy!=TRUE unit tests, mergelist and mergepair
mergelist = function(l, on, cols, how=c("inner","left","right","full"), mult=c("all","first","last","error"), copy=TRUE) {
  verbose = getOption("datatable.verbose")
  allow.cartesian = TRUE
  if (verbose) p = proc.time()[[3L]]
  if (!is.list(l) || is.data.frame(l))
    stop("'l' must be a list")
  how = match.arg(how)
  if (!is.logical(copy) || length(copy)!=1)
    stop("'copy' must be logical of length 1")
  if (is.na(copy))
    stop("'copy' NA not yet implemented")
  mult = match.arg(mult)
  if (!isTRUE(copy) && (
    how!="left" ||
    !(mult=="first" || mult=="last" || mult=="error")
  ))
    stop("copy!=TRUE is possible only for how='left' and mult='first|last|error'")
  if (any(!vapply(l, perhaps.data.table, FALSE)))
    stop("Every element of 'l' list must be data.table type object")
  n = length(l)
  if (n<2L) {
    if (isTRUE(copy)) l = copy(l)
    if (verbose) cat(sprintf("mergelist: took %.3fs\n", n, proc.time()[[3L]]-p))
    return(l)
  }
  if (missing(on) || is.null(on)) {
    haskeyv = vapply(l, haskey, TRUE)
    ## TODO: first LHS tables does not need to be keyed
    if (any(!haskeyv))
      stop("'on' is missing but not all tables are keyed")
    on = NULL
  } else if (!is.character(on) || !length(on)) {
    stop("'on' must be non zero length character")
  } else if (any(vapply(l, function(x, on) any(!on %chin% names(x)), on=on, FALSE))) {
    stop("'on' argument specify columns to join that are not present in every table") ## TODO this should not be needed!! we can merge those columns later on, i.e. denormalizing snowflake
  }
  if (missing(cols) || is.null(cols)) {
    cols = vector("list", n)
  } else {
    if (!is.list(cols))
      stop("'cols' must be a list")
    if (length(cols) != n)
      stop("'cols' must be same length as 'l'")
    skip = vapply(cols, is.null, FALSE)
    if (any(!vapply(cols[!skip], is.character, FALSE)))
      stop("'cols' must be a list of character vectors, or eventually NULLs")
    if (any(mapply(function(x, icols) any(!icols %chin% names(x)), l[!skip], cols[!skip])))
      stop("'cols' specify columns not present in corresponding table")
  }

  stopifnot(length(l) == 2L) ## dev
  l.mem = lapply(l, vapply, address, "")
  out = l[[1L]]
  out.cols = cols[[1L]]
  for (join.i in seq_len(n-1L)) {
    rhs.i = join.i + 1L
    out = mergepair(
      lhs = out, rhs = l[[rhs.i]],
      on = on, how = how, mult = mult,
      #on = on[[join.i]], how = how[[join.i]], mult = mult[[join.i]], ## vectorized params TODO
      lhs.cols = out.cols, rhs.cols = cols[[rhs.i]],
      copy = NA, ## avoid any copies inside, will copy once below
      allow.cartesian = allow.cartesian, verbose = verbose
    )
    out.cols = copy(names(out))
  }
  out.mem = vapply(out, address, "")
  setDT(out)
  if (!is.na(copy)) {
    cp = names(out.mem)[out.mem %chin% unlist(if (copy) l.mem else l.mem[-1L], recursive=FALSE)]
    if (length(cp)) {
      if (verbose) cat(sprintf("mergelist: copy %d columns\n", length(cp)))
      set(out, NULL, cp, copy(unclass(out)[cp]))
    }
  }
  if (verbose) cat(sprintf("mergelist: merging %d tables, took %.3fs\n", proc.time()[[3L]]-p))
  out
}

seqexp = function(x) .Call(Cseqexp, x)
perhaps.data.table = function(x) .Call(CperhapsDataTableR, x)
