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
someCols = function(x, cols, drop=character(), keep=character(), retain.order=FALSE) {
  keep = colnamesInt(x, keep)
  drop = colnamesInt(x, drop)
  cols = colnamesInt(x, cols)
  ans = union(keep, setdiff(cols, drop))
  if (!retain.order) return(ans)
  intersect(colnamesInt(x, NULL), ans)
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
    stopf("'x' must be data.table")
  if (!is.character(on) || !length(on) || anyNA(on) || !all(on %chin% names(x)))
    stopf("'on' must be character column names of 'x' argument")
  mult = match.arg(mult)
  if (is.null(cols))
    cols = seq_along(x)
  else if (!(is.character(cols) || is.integer(cols)) || !length(cols) || anyNA(cols))
    stopf("'cols' must be non-zero length, non-NA, integer or character columns of 'x' argument")
  if (!isTRUEorFALSE(copy))
    stopf("'%s' must be TRUE or FALSE", "copy")
  ## do not compute sort=F for mult="first" if index (sort=T) already available, sort=T is needed only for mult="last"
  ## this short circuit will work after #4386 because it requires retGrp=T
  #### sort = mult!="first" || hasindex(x, by=on, retGrp=TRUE)
  sort = TRUE ## above line does not work for the moment, test 302.02
  o = forderv(x, by=on, sort=sort, retGrp=TRUE)
  if (attr(o, "maxgrpn", TRUE) <= 1L) {
    ans = .shallow(x, someCols(x, cols, keep=on), retain.key=TRUE)
    if (copy) ans = copy(ans)
    return(ans)
  }
  f = attr(o, "starts", exact=TRUE)
  if (mult=="last") {
    if (!sort) internal_error("sort must be TRUE when computing mult='last'") # nocov
    f = c(f[-1L]-1L, nrow(x)) ## last of each group
  }
  if (length(o)) f = o[f]
  if (sort && length(o <- forderv(f))) f = f[o] ## this rolls back to original order
  .Call(CsubsetDT, x, f, someCols(x, cols, keep=on))
}

# extra layer over bmerge to provide ready to use row indices (or NULL for 1:nrow)
# NULL to avoid extra copies in downstream code, it turned out that avoiding copies precisely is costly and enormously complicates code, need #4409 and/or handle 1:nrow in subsetDT
dtmerge = function(x, i, on, how, mult, join.many, void=FALSE, verbose) {
  nomatch = switch(how, "inner"=, "semi"=, "anti"=, "cross"= 0L, "left"=, "right"=, "full"= NA_integer_)
  nomatch0 = identical(nomatch, 0L)
  if (is.null(mult))
    mult = switch(how, "semi"=, "anti"= "last", "cross"= "all", "inner"=, "left"=, "right"=, "full"= "error")
  if (void && mult!="error")
    internal_error("void must be used with mult='error'") # nocov
  if (how=="cross") { ## short-circuit bmerge results only for cross join
    if (length(on) || mult!="all" || !join.many)
      stopf("cross join must be used with zero-length on, mult='all', join.many=TRUE")
    if (void)
      internal_error("cross join must be used with void=FALSE") # nocov
    ans = list(allLen1=FALSE, starts=rep.int(1L, nrow(i)), lens=rep.int(nrow(x), nrow(i)), xo=integer())
  } else {
    if (!length(on))
      stopf("'on' must be non-zero length character vector")
    if (mult=="all" && (how=="semi" || how=="anti"))
      stopf("semi and anti joins must be used with mult!='all'")
    icols = colnamesInt(i, on, check_dups=TRUE)
    xcols = colnamesInt(x, on, check_dups=TRUE)
    ans = bmerge(i, x, icols, xcols, roll=0, rollends=c(FALSE, TRUE), nomatch=nomatch, mult=mult, ops=rep.int(1L, length(on)), verbose=verbose)
    if (void) { ## void=T is only for the case when we want raise error for mult='error', and that would happen in above line
      return(invisible(NULL))
    } else if (how=="semi" || how=="anti") { ## semi and anti short-circuit
      irows = which(if (how=="semi") ans$lens!=0L else ans$lens==0L) ## we will subset i rather than x, thus assign to irows, not to xrows
      if (length(irows)==length(ans$lens)) irows = NULL
      return(list(ans=ans, irows=irows))
    } else if (mult=="all" && !ans$allLen1 && !join.many && ## join.many, like allow.cartesian, check
      !(length(ans$starts)==1L && ans$lens==nrow(x)) && ## special case of scalar i match to const duplicated x, not handled by anyDuplicate: data.table(x=c(1L,1L))[data.table(x=1L), on="x"]
      anyDuplicated(ans$starts, incomparables=c(0L,NA_integer_))
    )
      stopf("Joining resulted in many-to-many join. Perform quality check on your data, use mult!='all', or set 'datatable.join.many' option to TRUE to allow rows explosion.")
  }

  ## xrows, join-to
  xrows = if (ans$allLen1) ans$starts else vecseq(ans$starts, ans$lens, NULL)
  if (nomatch0 && ans$allLen1) xrows = xrows[as.logical(ans$lens)]
  len.x = length(xrows) ## as of now cannot optimize to NULL, search for #4409 here

  ## irows, join-from
  irows = if (!(ans$allLen1 && (!nomatch0 || len.x==length(ans$starts)))) seqexp(ans$lens)
  len.i = if (is.null(irows)) nrow(i) else length(irows)

  if (length(ans$xo) && length(xrows))
    xrows = ans$xo[xrows]
  len.x = length(xrows)

  if (len.i!=len.x)
    internal_error("dtmerge out len.i != len.x") # nocov

  return(list(ans=ans, irows=irows, xrows=xrows))
}

# atomic join between two tables
mergepair = function(lhs, rhs, on, how, mult, lhs.cols=names(lhs), rhs.cols=names(rhs), copy=TRUE, join.many=TRUE, verbose=FALSE) {
  semianti = how=="semi" || how=="anti"
  innerfull = how=="inner" || how=="full"
  {
    if (how!="cross") {
      if (is.null(on)) {
        if (how=="left" || semianti) on = key(rhs)
        else if (how=="right") on = key(lhs)
        else if (innerfull) on = onkeys(key(lhs), key(rhs))
        if (is.null(on))
          stopf("'on' is missing and necessary key is not present")
      }
      if (any(bad.on <- !on %chin% names(lhs)))
        stopf("'on' argument specify columns to join [%s] that are not present in LHS table [%s]", brackify(on[bad.on]), brackify(names(lhs)))
      if (any(bad.on <- !on %chin% names(rhs)))
        stopf("'on' argument specify columns to join [%s] that are not present in RHS table [%s]", brackify(on[bad.on]), brackify(names(rhs)))
    } else if (is.null(on)) {
      on = character() ## cross join only
    }
  } ## on
  {
    if (how!="right") {
      jnfm = lhs; fm.cols = lhs.cols; jnto = rhs; to.cols = rhs.cols
    } else {
      jnfm = rhs; fm.cols = rhs.cols; jnto = lhs; to.cols = lhs.cols
    }
  } ## join-to and join-from tables and columns (right outer join swap)

  ## ensure symmetric join for inner|full join, apply mult on both tables, bmerge do only 'x' table
  cp.i = FALSE ## copy marker of out.i
  if ((innerfull) && !is.null(mult) && (mult=="first" || mult=="last")) {
    jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE) ## might not copy when already unique by 'on'
    cp.i = nrow(jnfm)!=nrow(lhs) ## nrow(lhs) bc how='inner|full' so jnfm=lhs
  } else if (how=="inner" && (is.null(mult) || mult=="error")) { ## we do this branch only to raise error from bmerge, we cannot use forder to just find duplicates because those duplicates might not have matching rows in another table, full join checks mult='error' during two non-void bmerges
    dtmerge(x=jnfm, i=jnto, on=on, how=how, mult=mult, verbose=verbose, join.many=join.many, void=TRUE)
  }

  ## binary merge
  ans = dtmerge(x=jnto, i=jnfm, on=on, how=how, mult=mult, verbose=verbose, join.many=join.many)

  ## make i side
  out.i = if (is.null(ans$irows))
    .shallow(jnfm, cols=someCols(jnfm, fm.cols, keep=on, retain.order=semianti), retain.key=TRUE)
  else
    .Call(CsubsetDT, jnfm, ans$irows, someCols(jnfm, fm.cols, keep=on, retain.order=semianti))
  cp.i = cp.i || !is.null(ans$irows)

  ## make x side
  if (semianti) {
    out.x = list(); cp.x = TRUE
  } else {
    out.x = if (is.null(ans$xrows)) ## as of now xrows cannot be NULL #4409 thus nocov below
      internal_error("dtmerge()$xrows returned NULL, #4409 been resolved but related code has not been updated?") #.shallow(jnto, cols=someCols(jnto, to.cols, drop=on), retain.key=TRUE) # nocov ## as of now nocov does not make difference r-lib/covr#279
    else
      .Call(CsubsetDT, jnto, ans$xrows, someCols(jnto, to.cols, drop=on))
    cp.x = !is.null(ans$xrows)
    ## ensure no duplicated column names in merge results
    if (any(dup.i<-names(out.i) %chin% names(out.x)))
      stopf("merge result has duplicated column names, use 'cols' argument or rename columns in 'l' tables, duplicated column(s): %s", brackify(names(out.i)[dup.i]))
  }

  ## stack i and x
  if (how!="full") {
    if (!cp.i && copy) out.i = copy(out.i)
    #if (!cp.x && copy) out.x = copy(out.x) ## as of now cp.x always TRUE, search for #4409 here
    out = .Call(Ccbindlist, list(out.i, out.x), FALSE)
    if (how=="right") setcolorder(out, neworder=c(on, names(out.x))) ## arrange columns: i.on, x.cols, i.cols
  } else { # how=="full"
    ## we made left join side above, proceed to right join side, so swap tbls
    jnfm = rhs; fm.cols = rhs.cols; jnto = lhs; to.cols = lhs.cols

    cp.r = FALSE
    if (!is.null(mult) && (mult=="first" || mult=="last")) {
      jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE)
      cp.r = nrow(jnfm)!=nrow(rhs) ## nrow(rhs) bc jnfm=rhs
    } ## mult=="error" check was made on one side already, below we do on the second side, test 101.43

    ## binary merge anti join
    bns = dtmerge(x=jnto, i=jnfm, on=on, how="anti", mult=if (!is.null(mult) && mult!="all") mult, verbose=verbose, join.many=join.many)

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
        out.i = lapply(unclass(out.i)[add], `[`, seq_len(nrow(out.r))) ## could eventually remove this when cbindlist recycle 0 rows up, note that we need out.r not to be copied
        out.r = .Call(Ccbindlist, list(out.r, out.i), FALSE)
        setcolorder(out.r, neworder=neworder)
      }
      out = out.r
    } else { ## all might have not been copied yet, rbindlist will copy
      out.l = .Call(Ccbindlist, list(out.i, out.x), FALSE)
      out = rbindlist(list(out.l, out.r), use.names=TRUE, fill=TRUE)
    }
  }
  setDT(out)
}

# Previously, we had a custom C implementation here, which is ~2x faster,
#   but this is fast enough we don't bother maintaining a new routine.
#   Hopefully in the future rep() can recognize the ALTREP and use that, too.
seqexp = function(x) rep(seq_along(x), x)
perhaps.data.table = function(x) .Call(CperhapsDataTableR, x)
