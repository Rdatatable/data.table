cbindlist = function(l, copy=TRUE) {
  ans = .Call(Ccbindlist, l, copy)
  if (anyDuplicated(names(ans))) { ## invalidate key and index
    setattr(ans, "sorted", NULL)
    setattr(ans, "index", integer())
  }
  setDT(ans)
  ans
}

# when 'on' is missing then use keys, used only for inner and full join
onkeys = function(x, y) {
  if (is.null(x) && !is.null(y)) y
  else if (!is.null(x) && is.null(y)) x
  else if (!is.null(x) && !is.null(y)) {
    if (length(x)>=length(y)) intersect(y, x) ## align order to shorter|rhs key
    else intersect(x, y)
  } else NULL # nocov ## internal error is being called later in mergepair
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
# for mult='first' it is unique(x, by=on)[, c(on, cols), with=FALSE]
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
  if (!isTRUEorFALSE(copy))
    stop("'copy' must be TRUE or FALSE")
  ## do not compute sort=F for mult="first" if index (sort=T) already available, sort=T is needed only for mult="last"
  ## this short circuit will work after #4386 because it requires retGrp=T
  sort = mult!="first" || hasindex(x, by=on, retGrp=TRUE)
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

# extra layer over bmerge to provide ready to use row indices (or NULL for 1:nrow)
# NULL to avoid extra copies in downstream code, it turned out that avoiding copies precisely is costly and enormously complicates code, need #4409 and/or handle 1:nrow in subsetDT
# we don't use semi join in mergelist so is not tested, anti join is used in full outer join
dtmatch = function(x, i, on, nomatch, mult, verbose, join.many, semi=FALSE, anti=FALSE, void=FALSE) {
  inner = identical(nomatch, 0L)
  if (void && mult!="error")
    stop("internal error: void must be used with mult='error'") # nocov
  if (semi || anti) {
    if (semi && anti)
      stop("internal error: semi and anti cannot be used together") # nocov
    if (!inner)
      stop("internal error: semi and anti joins must be used with nomatch=0L") # nocov
  }
  icols = colnamesInt(i, on, check_dups=TRUE)
  xcols = colnamesInt(x, on, check_dups=TRUE)
  ans = bmerge(i, x, icols, xcols, roll=0, rollends=c(FALSE, TRUE), nomatch=nomatch, mult=mult, ops=rep.int(1L, length(on)), verbose=verbose)
  if (void) { ## void=T is only for the case when we want raise error for mult='error', and that would happen in above line
    return(invisible(NULL))
  } else if (semi || anti) { ## semi and anti short-circuit
    irows = which(if (semi) ans$lens!=0L else ans$lens==0L) ## we will subset i rather than x, thus assign to irows, not to xrows
    if (length(irows)==length(ans$lens)) irows = NULL
    return(list(ans=ans, irows=irows))
  } else if (mult=="all" && !ans$allLen1 && !join.many && ## join.many, like allow.cartesian, check
    !(length(ans$starts)==1L && ans$lens==nrow(x)) && ## special case of scalar i match to const duplicated x, not handled by anyDuplicate: data.table(x=c(1L,1L))[data.table(x=1L), on="x"]
    anyDuplicated(ans$starts, incomparables=c(0L,NA_integer_))
  )
    stop("Joining resulted in many-to-many join. Perform quality check on your data, use mult!='all', or set 'datatable.join.many' option to TRUE to allow rows explosion.")

  ## xrows, join-to
  xrows = if (ans$allLen1) ans$starts else vecseq(ans$starts, ans$lens, NULL)
  if (inner && ans$allLen1) xrows = xrows[as.logical(ans$lens)]
  len.x = length(xrows) ## as of now cannot optimize to NULL, search for #4409 here

  ## irows, join-from
  irows = if (!(ans$allLen1 && (!inner || len.x==length(ans$starts)))) seqexp(ans$lens)
  len.i = if (is.null(irows)) nrow(i) else length(irows)

  if (length(ans$xo) && length(xrows))
    xrows = ans$xo[xrows]
  len.x = length(xrows)

  if (len.i!=len.x)
    stop("internal error: dtmatch out len.i != len.x") # nocov

  return(list(ans=ans, irows=irows, xrows=xrows))
}

# atomic join between two tables
mergepair = function(lhs, rhs, on, how, mult, lhs.cols=names(lhs), rhs.cols=names(rhs), copy=TRUE, join.many=TRUE, verbose=FALSE) {
  if (is.null(on)) {
    if (how=="left") on = key(rhs)
    else if (how=="right") on = key(lhs)
    else on = onkeys(key(lhs), key(rhs))
    if (is.null(on))
      stop("internal error: 'on' is missing, keys were probably dropped in sequence of merges, please submit reproducible example to issue tracker") # nocov
  }
  if (any(bad.on <- !on %chin% names(lhs)))
    stop(sprintf("'on' argument specify columns to join [%s] that are not present in LHS table [%s]", paste(on[bad.on], collapse=", "), paste(names(lhs), collapse=", ")))
  if (any(bad.on <- !on %chin% names(rhs)))
    stop(sprintf("'on' argument specify columns to join [%s] that are not present in RHS table [%s]", paste(on[bad.on], collapse=", "), paste(names(rhs), collapse=", ")))
  if (how!="right") { ## join-to, join-from
    jnfm = lhs; fm.cols = lhs.cols; jnto = rhs; to.cols = rhs.cols
  } else {
    jnfm = rhs; fm.cols = rhs.cols; jnto = lhs; to.cols = lhs.cols
  }
  nomatch = if (how=="inner") 0L else NA_integer_

  cp.i = FALSE ## copy marker of out.i
  if (mult!="all" && (how=="inner" || how=="full")) { ## for inner|full join we apply mult on both tables, bmerge do only 'i' table
    if (mult=="first" || mult=="last") {
      jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE) ## might not copy when already unique by 'on'
      cp.i = nrow(jnfm)!=nrow(lhs) ## nrow(lhs) bc how='inner|full' so jnfm=lhs
    } else if (mult=="error") { ## we do this branch only to raise error from bmerge, we cannot use forder to just find duplicates because those duplicates might not have matching rows in another table
      dtmatch(x=jnfm, i=jnto, on=on, nomatch=nomatch, mult=mult, verbose=verbose, join.many=join.many, void=TRUE)
    }
  }

  ## binary merge
  ans = dtmatch(x=jnto, i=jnfm, on=on, nomatch=nomatch, mult=mult, verbose=verbose, join.many=join.many)

  ## make i side
  out.i = if (is.null(ans$irows))
    .shallow(jnfm, cols=someCols(jnfm, fm.cols, keep=on), retain.key=TRUE)
  else
    .Call(CsubsetDT, jnfm, ans$irows, someCols(jnfm, fm.cols, keep=on))
  cp.i = cp.i || !is.null(ans$irows)

  ## make x side
  out.x = if (is.null(ans$xrows)) ## as of now xrows cannot be NULL #4409 thus nocov below
    stop("internal error: dtmatch()$xrows returned NULL, #4409 been resolved but related code has not been updated? please report to issue tracker") #.shallow(jnto, cols=someCols(jnto, to.cols, drop=on), retain.key=TRUE) # nocov ## as of now nocov does not make difference r-lib/covr#279
  else
    .Call(CsubsetDT, jnto, ans$xrows, someCols(jnto, to.cols, drop=on))
  cp.x = !is.null(ans$xrows)

  ## ensure no duplicated column names in merge results
  if (any(dup.i<-names(out.i) %chin% names(out.x)))
    stop("merge result has duplicated column names, use 'cols' argument or rename columns in 'l' tables, duplicated column(s): ", paste(names(out.i)[dup.i], collapse=", "))

  if (how!="full") {
    if (!cp.i && copy) out.i = copy(out.i)
    #if (!cp.x && copy) out.x = copy(out.x) ## as of now cp.x always TRUE, search for #4409 here
    out = .Call(Ccbindlist, list(out.i, out.x), FALSE)
    if (how=="right") setcolorder(out, neworder=c(on, names(out.x))) ## arrange columns: i.on, x.cols, i.cols
  } else { # how=="full"
    ## we made left join above, proceed to right join, actually just swap tbls and antijoin
    jnfm = rhs; fm.cols = rhs.cols; jnto = lhs; to.cols = lhs.cols
    cp.r = FALSE
    if (mult=="first" || mult=="last") {
      jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE)
      cp.r = nrow(jnfm)!=nrow(rhs) ## nrow(rhs) bc jnfm=rhs
    } ## mult=="error" check not needed anymore, both sides will be checked

    ## binary merge anti join
    bns = dtmatch(x=jnto, i=jnfm, on=on, nomatch=0L, mult=mult, verbose=verbose, join.many=join.many, anti=TRUE)

    ## make anti join side
    out.r = if (is.null(bns$irows))
      .shallow(jnfm, cols=someCols(jnfm, fm.cols, keep=on), retain.key=TRUE) ## retain.key is used only in the edge case when !nrow(out.i)
    else
      .Call(CsubsetDT, jnfm, bns$irows, someCols(jnfm, fm.cols, keep=on))
    cp.r = cp.r || !is.null(bns$irows)

    ## short circuit to avoid rbindlist to empty sets and retains keys
    if (!nrow(out.r)) { ## possibly also !nrow(out.i)
      if (!cp.i && copy) out.i = copy(out.i)
      #if (!cp.x && copy) out.x = copy(out.x) ## as of now cp.x always TRUE, search for #4409 here
      out = .Call(Ccbindlist, list(out.i, out.x), FALSE)
    } else if (!nrow(out.i)) { ## but not !nrow(out.r)
      if (!cp.r && copy) out.r = copy(out.r)
      if (length(add<-setdiff(names(out.i), names(out.r)))) { ## add missing columns of proper types NA
        neworder = copy(names(out.i)) #set(out.r, NULL, add, lapply(unclass(out.i)[add], `[`, 1L)) ## 291.04 overalloc exceed fail during set()
        out.i = lapply(unclass(out.i)[add], `[`, seq_len(nrow(out.r))) ## we can remove that once cbindlist will recycle, note that we need out.r not to be copied
        out.r = .Call(Ccbindlist, list(out.r, out.i), FALSE)
        setcolorder(out.r, neworder=neworder)
      }
      out = out.r
    } else { ## all might have not been copied yet, rbindlist will copy
      out.l = .Call(Ccbindlist, list(out.i, out.x), FALSE)
      out = rbindlist(list(out.l, out.r), use.names=TRUE, fill=TRUE) ## we could replace rbindlist for new alloc of nrow(out.l)+nrow(out.r) and then memcpy into it, we dont need overalloc here
    }
  }
  setDT(out) ## no need to overalloc so would be better as list, but bmerge wants DT, and just set class will not do, selfrefok will warn, thus setDT
}

mergelist = function(l, on, cols, how=c("left","inner","full","right"), mult=c("error","all","first","last"), copy=TRUE, join.many=getOption("datatable.join.many")) {
  verbose = getOption("datatable.verbose")
  if (verbose)
    p = proc.time()[[3L]]
  if (!is.list(l) || is.data.frame(l))
    stop("'l' must be a list")
  if (any(!vapply(l, is.data.table, FALSE)))
    stop("Every element of 'l' list must be data.table objects")
  if (any(!vapply(l, length, 0L)))
    stop("Tables in 'l' argument must be non-zero columns tables")
  if (!isTRUEorFALSE(copy))
    stop("'copy' must be TRUE or FALSE")
  n = length(l)
  if (is.list(mult))
    stop("'mult' list not yet implemented")
  mult = match.arg(mult)
  if (!copy && !(mult=="first" || mult=="last" || mult=="error"))
    stop("copy=FALSE works only for mult='first|last|error'")
  if (n<2L) {
    out = if (!n) as.data.table(l) else l[[1L]]
    if (copy) out = copy(out)
    if (verbose)
      cat(sprintf("mergelist: merging %d table(s), took %.3fs\n", n, proc.time()[[3L]]-p))
    return(out)
  }
  if (!(is.list(join.many) && length(join.many)==n-1L && all(vapply(join.many, isTRUEorFALSE, NA)))
      && !isTRUEorFALSE(join.many))
    stop("'join.many' must be TRUE or FALSE, or a list of such which length must be length(l)-1L")
  if (!is.list(join.many))
    join.many = replicate(n-1L, join.many, simplify=FALSE)
  if (is.list(how))
    stop("'how' list not yet implemented")
  how = match.arg(how)
  if (missing(on) || is.null(on)) {
    haskeyv = vapply(l, haskey, TRUE)
    if (!( ## list valid keys below
      (how=="left" && all(haskeyv[-1L])) ||
      (how=="right" && all(haskeyv[-n])) ||
      ((how=="inner" || how=="full") && any(haskeyv[1:2]) && all(haskeyv[-(1:2)]))
    ))
      stop("'on' is missing and necessary keys are not present")
    on = vector("list", n-1L)
  } else {
    if (!(is.list(on) && length(on)==n-1L && all(vapply(on, is.character, NA)) && all(vapply(on, length, 0L)) && all(!vapply(on, anyDuplicated, 0L)) && !anyNA(on, recursive=TRUE))
        && !(is.character(on) && length(on) && !anyDuplicated(on) && !anyNA(on))) {
      stop("'on' must be non-zero length, non-NA, non-duplicated, character, or a list of such which length must be length(l)-1L")
    } else if (any(bad.on <- !unlist(on, use.names=FALSE) %chin% unique(unlist(lapply(l, names))))) {
      stop("'on' argument specify columns to join that are not present in any table: ", paste(unlist(on, use.names=FALSE)[bad.on], collapse=", "))
    }
    if (is.character(on))
      on = replicate(n-1L, on, simplify=FALSE)
  }
  if (any(vapply(l, function(x) anyDuplicated(names(x)), 0L)))
    stop("Some of the tables in 'l' have duplicated column names")
  if (missing(cols) || is.null(cols)) {
    cols = vector("list", n)
  } else {
    if (!is.list(cols))
      stop("'cols' must be a list")
    if (length(cols) != n)
      stop("'cols' must be same length as 'l'")
    skip = vapply(cols, is.null, FALSE)
    if (any(!vapply(cols[!skip], function(x) is.character(x) && !anyNA(x) && !anyDuplicated(x), FALSE)))
      stop("'cols' must be a list of non-zero length, non-NA, non-duplicated, character vectors, or eventually NULLs (all columns)")
    if (any(mapply(function(x, icols) any(!icols %chin% names(x)), l[!skip], cols[!skip])))
      stop("'cols' specify columns not present in corresponding table")
  }

  l.mem = lapply(l, vapply, address, "")
  out = l[[1L]]
  out.cols = cols[[1L]]
  for (join.i in seq_len(n-1L)) {
    rhs.i = join.i + 1L
    out = mergepair(
      lhs = out, rhs = l[[rhs.i]],
      on = on[[join.i]],
      how = how, mult = mult,
      #how = how[[join.i]], mult = mult[[join.i]], ## not yet implemented
      lhs.cols = out.cols, rhs.cols = cols[[rhs.i]],
      copy = FALSE, ## avoid any copies inside, will copy once below
      join.many = join.many[[join.i]],
      verbose = verbose
    )
    out.cols = copy(names(out))
  }
  out.mem = vapply(out, address, "")
  if (copy)
    .Call(CcopyCols, out, colnamesInt(out, names(out.mem)[out.mem %chin% unique(unlist(l.mem, recursive=FALSE))]))
  if (verbose)
    cat(sprintf("mergelist: merging %d tables, took %.3fs\n", n, proc.time()[[3L]]-p))
  out
}

seqexp = function(x) .Call(Cseqexp, x)
perhaps.data.table = function(x) .Call(CperhapsDataTableR, x)
