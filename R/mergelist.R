cbindlist_impl_ = function(l, copy) {
  ans = .Call(Ccbindlist, l, copy)
  if (anyDuplicated(names(ans))) { ## invalidate key and index
    setattr(ans, "sorted", NULL)
    setattr(ans, "index", NULL)
  }
  setDT(ans)
  ans
}

cbindlist = function(l) cbindlist_impl_(l, copy=TRUE)
setcbindlist = function(l) cbindlist_impl_(l, copy=FALSE)

# when 'on' is missing then use keys, used only for inner and full join
onkeys = function(x, y) {
  if (is.null(x) && !is.null(y)) return(y)
  if (!is.null(x) && is.null(y)) return(x)
  if (!is.null(x) && !is.null(y)) {
    if (length(x) >= length(y))
      return(intersect(y, x)) ## align order to shorter|rhs key
    else
      return(intersect(x, y))
  }
  NULL # nocov. Internal error is being called later in mergepair
}

# column index selection helper
someCols = function(x, cols, drop=character(), keep=character(), retain.order=FALSE) {
  keep = colnamesInt(x, keep)
  drop = colnamesInt(x, drop)
  cols = colnamesInt(x, cols)
  ans = union(keep, setdiff(cols, drop))
  if (!retain.order) return(ans)
  sort(ans)
}

hasindex = function(x, by, retGrp=FALSE) {
  index = attr(x, "index", TRUE)
  if (is.null(index)) return(FALSE)
  idx_name = paste0("__", by, collapse="")
  idx = attr(index, idx_name, TRUE)
  if (is.null(idx)) return(FALSE)
  if (!retGrp) return(TRUE)
  !is.null(attr(idx, "starts", TRUE))
}

# fdistinct applies mult='first|last'
# for mult='first' it is unique(x, by=on)[, c(on, cols), with=FALSE]
# it may not copy when copy=FALSE and x is unique by 'on'
fdistinct = function(x, on=key(x), mult=c("first", "last"), cols=seq_along(x), copy=TRUE) {
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
  if (mult == "last") {
    if (!sort) internal_error("sort must be TRUE when computing mult='last'") # nocov
    f = c(f[-1L] - 1L, nrow(x)) ## last of each group
  }
  if (length(o)) f = o[f]
  if (sort && length(o <- forderv(f))) f = f[o] ## this rolls back to original order
  .Call(CsubsetDT, x, f, someCols(x, cols, keep=on))
}

# extra layer over bmerge to provide ready to use row indices (or NULL for 1:nrow)
# NULL to avoid extra copies in downstream code, it turned out that avoiding copies precisely is costly and enormously complicates code, need #4409 and/or handle 1:nrow in subsetDT
dtmerge = function(x, i, on, how, mult, join.many, void=FALSE, verbose) {
  nomatch = switch(how,
                   inner=, semi=, anti=, cross= 0L,
                   left=, right=, full=NA_integer_)
  nomatch0 = identical(nomatch, 0L)
  if (is.null(mult))
    mult = switch(how,
                  semi=, anti="last",
                  cross="all",
                  inner=, left=, right=, full="error")
  if (void && mult != "error")
    internal_error("'void' must be used with mult='error'") # nocov
  if (how == "cross") { ## short-circuit bmerge results only for cross join
    if (length(on) || mult != "all" || !join.many)
      stopf("cross join must be used with zero-length on, mult='all', join.many=TRUE")
    if (void)
      internal_error("cross join must be used with void=FALSE") # nocov
    ans = list(allLen1=FALSE, starts=rep.int(1L, nrow(i)), lens=rep.int(nrow(x), nrow(i)), xo=integer())
  } else {
    if (!length(on))
      stopf("'on' must be non-zero length character vector")
    if (mult == "all" && (how == "semi" || how == "anti"))
      stopf("semi and anti joins must be used with mult!='all'")
    icols = colnamesInt(i, on, check_dups=TRUE)
    xcols = colnamesInt(x, on, check_dups=TRUE)
    ans = bmerge(i, x, icols, xcols, roll=0, rollends=c(FALSE, TRUE), nomatch=nomatch, mult=mult, ops=rep.int(1L, length(on)), verbose=verbose)
    if (void) { ## void=T is only for the case when we want raise error for mult='error', and that would happen in above line
      return(invisible(NULL))
    } else if (how == "semi" || how == "anti") { ## semi and anti short-circuit
      ## we will subset i rather than x, thus assign to irows, not to xrows
      if (how == "semi")
        irows = which(ans$lens != 0L)
      else
        irows = which(ans$lens == 0L)
      if (length(irows) == length(ans$lens)) irows = NULL
      return(list(ans=ans, irows=irows))
    } else if (mult == "all" && !ans$allLen1 && !join.many && ## join.many, like allow.cartesian, check
      !(length(ans$starts) == 1L && ans$lens == nrow(x)) && ## special case of scalar i match to const duplicated x, not handled by anyDuplicate: data.table(x=c(1L,1L))[data.table(x=1L), on="x"]
      anyDuplicated(ans$starts, incomparables=c(0L, NA_integer_))
    )
      stopf("Joining resulted in many-to-many join. Perform quality check on your data, use mult!='all', or set 'datatable.join.many' option to TRUE to allow rows explosion.")
  }

  ## xrows, join-to
  xrows = if (ans$allLen1) ans$starts else vecseq(ans$starts, ans$lens, NULL)
  if (nomatch0 && ans$allLen1) xrows = xrows[as.logical(ans$lens)]
  len.x = length(xrows) ## as of now cannot optimize to NULL, search for #4409 here

  ## irows, join-from
  irows = if (!(ans$allLen1 && (!nomatch0 || len.x == length(ans$starts)))) seqexp(ans$lens)
  len.i = if (is.null(irows)) nrow(i) else length(irows)

  if (length(ans$xo) && length(xrows))
    xrows = ans$xo[xrows]
  len.x = length(xrows)

  if (len.i != len.x)
    internal_error("dtmerge out len.i != len.x") # nocov

  list(ans=ans, irows=irows, xrows=xrows)
}

# atomic join between two tables
mergepair = function(lhs, rhs, on, how, mult, lhs.cols=names(lhs), rhs.cols=names(rhs), copy=TRUE, join.many=TRUE, verbose=FALSE) {
  semi_or_anti = how == "semi" || how == "anti"
  inner_or_full = how == "inner" || how == "full"

  if (how != "cross") {
    if (is.null(on)) {
      if (how == "left" || semi_or_anti) on = key(rhs)
      else if (how == "right") on = key(lhs)
      else if (inner_or_full) on = onkeys(key(lhs), key(rhs))
      if (is.null(on))
        stopf("'on' is missing and necessary key is not present")
    }
    if (any(bad.on <- !on %chin% names(lhs)))
      stopf("'on' argument specifies columns to join [%s] that are not present in %s table [%s]", brackify(on[bad.on]), "LHS", brackify(names(lhs)))
    if (any(bad.on <- !on %chin% names(rhs)))
      stopf("'on' argument specifies columns to join [%s] that are not present in %s table [%s]", brackify(on[bad.on]), "RHS", brackify(names(rhs)))
  } else if (is.null(on)) {
    on = character() ## cross join only
  }

  ## join-to and join-from tables and columns (right outer join-->swap)
  if (how != "right") {
    join_from = lhs; from_cols = lhs.cols; join_to = rhs; to_cols = rhs.cols
  } else {
    join_from = rhs; from_cols = rhs.cols; join_to = lhs; to_cols = lhs.cols
  }

  ## ensure symmetric join for inner|full join, apply mult on both tables, bmerge do only 'x' table
  copy_i = FALSE ## copy marker of out.i
  if (inner_or_full && !is.null(mult) && (mult == "first" || mult == "last")) {
    join_from = fdistinct(join_from, on=on, mult=mult, cols=from_cols, copy=FALSE) ## might not copy when already unique by 'on'
    copy_i = nrow(join_from) != nrow(lhs) ## nrow(lhs) bc how='inner|full' so join_from=lhs
  } else if (how == "inner" && (is.null(mult) || mult == "error")) { ## we do this branch only to raise error from bmerge, we cannot use forder to just find duplicates because those duplicates might not have matching rows in another table, full join checks mult='error' during two non-void bmerges
    dtmerge(x=join_from, i=join_to, on=on, how=how, mult=mult, verbose=verbose, join.many=join.many, void=TRUE)
  }

  ## binary merge
  ans = dtmerge(x=join_to, i=join_from, on=on, how=how, mult=mult, verbose=verbose, join.many=join.many)

  ## make i side; avoid subsetting if possible
  cols_i = someCols(join_from, from_cols, keep=on, retain.order=semi_or_anti)
  if (is.null(ans$irows)) {
    out.i = .shallow(join_from, cols=cols_i, retain.key=TRUE)
  } else {
    out.i = .Call(CsubsetDT, join_from, ans$irows, cols_i)
    copy_i = TRUE
  }

  ## make x side
  copy_x = TRUE
  if (semi_or_anti) {
    out.x = list()
  } else {
    if (is.null(ans$xrows)) ## as of now xrows cannot be NULL #4409 thus nocov below
      internal_error("dtmerge()$xrows returned NULL, #4409 been resolved but related code has not been updated?") # nocov
    out.x = .Call(CsubsetDT, join_to, ans$xrows, someCols(join_to, to_cols, drop=on))
    copy_x = TRUE
    ## ensure no duplicated column names in merge results
    if (any(dup.i <- names(out.i) %chin% names(out.x)))
      stopf("merge result has duplicated column names [%s], use 'cols' argument or rename columns in 'l' tables", brackify(names(out.i)[dup.i]))
  }

  ## stack i and x
  if (how != "full") {
    if (!copy_i && copy) out.i = copy(out.i)
    #if (!copy_x && copy) out.x = copy(out.x) ## as of now copy_x always TRUE, search for #4409 here
    out = .Call(Ccbindlist, list(out.i, out.x), FALSE)
    if (how == "right") setcolorder(out, neworder=c(on, names(out.x))) ## arrange columns: i.on, x.cols, i.cols
  } else { # how=="full"
    ## we made left join side above, proceed to right join side, so swap tbls
    join_from = rhs; from_cols = rhs.cols; join_to = lhs; to_cols = lhs.cols

    copy_r = FALSE
    if (!is.null(mult) && (mult == "first" || mult == "last")) {
      join_from = fdistinct(join_from, on=on, mult=mult, cols=from_cols, copy=FALSE)
      copy_r = nrow(join_from) != nrow(rhs) ## nrow(rhs) bc join_from=rhs
    } ## mult=="error" check was made on one side already, below we do on the second side, test 101.43

    ## binary merge anti join; only need to keep 'irows'
    mult = if (!is.null(mult) && mult != "all") mult
    supplement_rows = dtmerge(x=join_to, i=join_from, on=on, how="anti", mult=mult, verbose=verbose, join.many=join.many)$irows

    ## make anti join side
    cols_r = someCols(join_from, from_cols, keep=on)
    if (is.null(supplement_rows)) {
      out.r = .shallow(join_from, cols=cols_r, retain.key=TRUE) ## retain.key is used only in the edge case when !nrow(out.i)
    } else {
      out.r = .Call(CsubsetDT, join_from, supplement_rows, cols_r)
      copy_r = TRUE
    }

    ## short circuit to avoid rbindlist to empty sets and retains keys
    if (!nrow(out.r)) { ## possibly also !nrow(out.i)
      if (!copy_i && copy) out.i = copy(out.i)
      #if (!copy_x && copy) out.x = copy(out.x) ## as of now copy_x always TRUE, search for #4409 here
      out = .Call(Ccbindlist, list(out.i, out.x), FALSE)
    } else if (!nrow(out.i)) { ## but not !nrow(out.r)
      if (!copy_r && copy) out.r = copy(out.r)
      if (length(add <- setdiff(names(out.i), names(out.r)))) { ## add missing columns of proper types NA
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
