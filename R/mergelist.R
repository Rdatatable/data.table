cbindlist = function(l, copy=TRUE) {
  .Call(Ccbindlist, l, copy)
}

someCols = function(x, cols, drop=character(), keep=character()) {
  keep = colnamesInt(x, keep)
  drop = colnamesInt(x, drop)
  cols = colnamesInt(x, cols)
  union(keep, setdiff(cols, drop))
}

imatch = function(x, i, on, nomatch, mult, verbose, allow.cartesian=FALSE, notjoin=FALSE) {
  devel = isTRUE(getOption("datatable.mergelist.devel",TRUE))
  ans = bmerge(
    i, x,
    icols = colnamesInt(i, on, check_dups=TRUE),
    xcols = colnamesInt(x, on, check_dups=TRUE),
    roll = 0, rollends = c(FALSE, TRUE),
    nomatch = nomatch,
    mult = mult,
    ops=1L, verbose=verbose
  )
  if (devel) {
    # nocov start
    if (!identical(ans$indices, integer()))
      stop("internal error: bmerge()$indices should be integer()")
    if (!isTRUE(ans$allGrp1))
      stop("internal error: bmerge()$allGrp1 should be TRUE")
    if (mult!="all" && any(!ans$allLen1))
      stop("internal error: bmerge()$allLen1 should be all TRUE for mult!='all'")
    if (mult!="all" && is.na(nomatch) && any(ans$lens!=1L))
      stop("internal error: bmerge()$lens should be all 1L for mult!='all' && nomatch==NA")
    if (mult!="all" && identical(nomatch, 0L) && any(!ans$lens%in%c(0L,1L)))
      stop("internal error: bmerge()$lens should be all 0L or 1L for mult!='all' && nomatch==0L")
    if (!identical(ans$starts[ans$starts!=0L], ans$starts[ans$lens>0L]))
      stop("internal error: bmerge()$starts should be same for filtering via starts!=0 and lens>0L")
    # nocov end
  }

  xo = ans$xo           ## x order
  allLen1 = ans$allLen1 ## if duplicated matches occured, always TRUE for mult!="all"
  f__ = ans$starts      ## when x sorted, it marks the starts of each new group
  len__ = ans$lens      ## when no match then nomatch=0 => 0; nomatch=NA => 1, can be 2+ when multiple matches, useful to supply this into new seqexp function

  allGrp1 = ans$allGrp1   ## this is always TRUE here but in data.table.R is FALSE
  indices__ = ans$indices ## always integer() for equi join AFAIU

  if (mult!="all" && is.na(nomatch)) {
    # for mult!="all" and nomatch==NA whole branch could be skipped
    # irows has same length as nrow of i
    irows = f__
  } else if (allLen1) {
    # no duplicate matches
    irows = if (identical(nomatch,0L)) f__[as.logical(len__)] else f__
  } else if (mult=="all") {
    # some duplicate matches
    # anyDuplicated is likely to be expensive, we should manage to get that info from somewhere else
    # rework allow.cartesian #4383
    if (allow.cartesian || notjoin ||
        !anyDuplicated(f__, incomparables = c(0L, NA_integer_))) {
      irows = vecseq(f__, len__, NULL) # #742. If 'i' has no duplicates, ignore
    } else {
      irows = vecseq(f__, len__, as.double(nrow(x)+nrow(i)))
    } # rows in i might not match to x so old max(nrow(x),nrow(i)) wasn't enough. But this limit now only applies when there are duplicates present so the reason now for nrow(x)+nrow(i) is just to nail it down and be bigger than max(nrow(x),nrow(i)).
  } else {
    stop("internal error: !(mult!='all' && is.na(nomatch)) && !allLen1 && mult!='all'") # nocov
  }

  ans$f__ = f__
  if (length(xo) && length(irows))
    irows = xo[irows]
  ans$irows = irows
  ans
}

# how=left && mult!="all" can !copy
# copy=T will copy LHS and RHS
# copy=F will not copy LHS always, and RHS whenever possible - cbindlist case (RHS[on] == LHS[on])
# copy=NA will not copy LHS but copy RHS always
# how=right is sub-efficient, whenever possible left should be preferred
# TODO:
# full
# review retain.key in .shallow
# rework imatch (rm notjoin, allow.cartesian?)
mergelist = function(l, on, cols, how=c("inner","left","right","full"), mult=c("all","first","last","error"), copy=TRUE) {
  verbose = getOption("datatable.verbose")
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
    (mult!="first" && mult!="last" && mult!="error")
  ))
    stop("copy=FALSE is possible only for how='left' and mult='first|last|error'")
  if (any(!vapply(l, perhaps.data.table, FALSE)))
    stop("Every element of 'l' list must be data.table type object")
  n = length(l)
  if (n<2L) {
    if (copy) l = copy(l)
    if (verbose) cat(sprintf("mergelist: took %.3fs\n", proc.time()[[3L]]-p))
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
    stop("'on' argument specify columns to join that are not present in every table")
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

  ## hepler function, selfjoin mult='first|last', used in right outer join (and full outer join)
  right.nomult = function(x, on, mult, xcols) {
    stopifnot(mult=="first" || mult=="last")
    o = forderv(x, by=on, sort=mult!="first", retGrp=TRUE)
    if (attr(o, "maxgrpn", TRUE) > 1L) {
      f = attr(o, "starts", exact=TRUE)
      if (mult=="first") { ## sort==FALSE
        if (length(o)) f = o[f]
      } else { ## sort==TRUE
        f = c(f[-1L]-1L, nrow(x)) ## last of each group
        if (length(o)) f = o[f]
        if (length(o <- forderv(f))) f = f[o] ## this rolls back to original order
      }
      .Call(CsubsetDT, x, f, someCols(x, xcols, keep=on))
    } else x
  }
  # dev area
  stopifnot(length(l) == 2L) ## swap here because bmerge expects right outer and we prefer left outer by default
  x = l[[2L]]; i = l[[1L]]
  xcols = cols[[2L]]; icols = cols[[1L]]
  #mergepair = function(lhs, rhs, on, how, mult, lhs.cols, rhs.icols, verbose) {}
  devel = isTRUE(getOption("datatable.mergelist.devel",TRUE)); allow.cartesian = TRUE; nomatch = if (how=="inner") 0L else NA_integer_
  if (how=="left" || how=="inner") {
    if (is.null(on)) on = key(x)
  } else if (how=="full") {
    if (is.null(on)) {
      if (!identical(key(x), key(i))) stop("full outer join requires matching keys of 'x' and 'i', or explicitly provided 'on' argument")
      on = key(x)
    }
  } else {
    if (is.null(on)) on = key(i)
    tmp = x; x = i; i = tmp ## swap x, i and xcols, icols
    tmp = xcols; xcols = icols; icols = tmp; rm(tmp)
    if (mult=="error") { ## we do this branch only to raise error from bmerge
      imatch(x = x, i = i, on = on, nomatch = nomatch, mult = mult, verbose = verbose, allow.cartesian = allow.cartesian)
    } else if (mult=="first" || mult=="last") {
      if (devel) org_i = copy(i)
      i = right.nomult(i, on, mult, icols)
    }
  }
  ans = imatch(x = x, i = i, on = on,
               nomatch = nomatch,
               mult = mult, verbose = verbose,
               allow.cartesian = allow.cartesian)

  if (ans$allLen1 && (how!="inner" ||
    (how=="inner" && length(ans$irows)==nrow(i))
  )) { ## no duplicate matches
    out_i = .shallow(i, cols=icols, retain.key=TRUE, unlock=TRUE)
    cpi = FALSE ## short circuit, do not copy via CsubsetDT
  } else { ## multiple matches, so mult=="all"
    if (how!="inner" && mult!="all") stop("internal error: how!='inner'' && mult!='all' but multiple matches returned") # nocov
    out_i = .Call(CsubsetDT, i, seqexp(ans$lens), someCols(i, icols, keep=on))
    cpi = TRUE
  }
  if (!cpi && (how=="inner" || ## we still need to copy because we dont want copiness to be depends on the data, and how='inner' has to copy always
    (copy && how!="full") ## how='full' will copy later on in rbindlist
  )) {
    out_i = copy(out_i)
    cpi = TRUE
  }

  ## TODO: short circuit on x, !copy for how=left, useful only when `on` values are exactly the same in x and i, so cbindlist would be enough rather than mergelist
  if (!length(ans$xo) && FALSE) { ## condition `!length(ans$xo)` is not enough
    out_x = .shallow(x, cols=someCols(x, xcols, drop=on), retain.key=TRUE, unlock=TRUE)
    cpx = FALSE
  } else {
    out_x = .Call(CsubsetDT, x, ans$irows, someCols(x, xcols, drop=on))
    cpx = TRUE
  }
  if (!cpi && (how=="inner" || ## we still need to copy because we dont want copiness to be depends on the data, and how='inner' has to copy always
      (copy && how!="full") ## how='full' will copy later on in rbindlist
  )) {
    out_x = copy(out_x)
    cpx = TRUE
  }
  ## TODO: add copy=NA, for how=left only, when we want enforce copy of RHS (x) always, but keep LHS (i) not copied

  if (how!="full") { ## not for 'full' because we need to ensure it is copied, there are 2 levels of short circuit and we need to copy only once, so we handle out (out_l) in how=full branch
    if (how=="right") {
      out = cbindlist(list(.shallow(out_i, cols=on), out_x, .shallow(out_i, cols=someCols(i, icols, drop=on))), copy=FALSE) ## arrange columns: i.on, x.cols, i.cols
    } else {
      out = cbindlist(list(out_i, out_x), copy=FALSE)
    }
  } else { # how=="full"
    if (mult=="error") {
      ## normally we would need to call imatch(x=x, i=i) just to raise error on mult, but we already made that for how=left, so mult matches are not there anymore
      ## double check, as now we also change nomatch=NA to 0L for notjoin
      if (devel) imatch(x = i, i = x, on = on, nomatch = 0L, mult = mult, verbose = verbose, allow.cartesian = allow.cartesian)
    } else if (mult=="first" || mult=="last") { ## this is to ensure no duplicates in i (x before swap)
      if (devel) org_x = copy(x)
      x = right.nomult(x, on, mult, xcols)
    }
    ans2 = imatch(
      x = i, i = x, on = on, ## ans is for how=left, ans2 here is for how=right, thus swap
      nomatch = 0L, ## notjoin, thus nomatch=0L
      mult = mult, verbose = verbose,
      allow.cartesian = allow.cartesian,
      notjoin=TRUE # only avoid cartesian product error message
    )
    ni = which(ans2$f__==0L)
    ## devel testing reference
    #m1 = x[!i, which=TRUE, on=on, mult=mult, allow.cartesian=allow.cartesian]
    #m2 = xx[!i, which=TRUE, on=on, mult=mult, allow.cartesian=allow.cartesian]
    cpr = FALSE # copied r flag
    if (!length(ni)) { ## short circuit to avoid rbindlist to empty sets
      if (!cpi) {
        out_i = copy(out_i)
        cpi = TRUE
      }
      if (!cpx) {
        out_x = copy(out_x)
        cpx = TRUE
      }
      out = cbindlist(list(out_i, out_x), copy=FALSE)
      out_r = copy(NULL)
      cpr = TRUE ## we havn't even materialize right side here
    } else {
      if (length(ni)==nrow(x)) { ## short circuit, do not copy via CsubsetDT
        out_r = .shallow(x, cols=someCols(x, xcols, keep=on), retain.key=FALSE, unlock=TRUE) ## subset columns
      } else {
        out_r = .Call(CsubsetDT, x, ni, someCols(x, xcols, keep=on))
        cpr = TRUE
      }
      out_l = cbindlist(list(out_i, out_x), copy=FALSE)
      out = rbindlist(list(out_l, out_r), use.names=TRUE, fill=TRUE)
      cpi = cpx = cpr = TRUE ## all are being copied in rbindlist
    }
  }
  if (devel) {
    adr_i = vapply(i, address, "")
    adr_x = vapply(x, address, "")
    adr_x = adr_x[!names(adr_x) %chin% on]
    adr_o = vapply(out, address, "")
    if (how!="full") {
      org = x[i, on=on, mult=mult, nomatch=nomatch, allow.cartesian=allow.cartesian]
      ref = if (ans$allLen1 && how!="inner") {
        cbind(i[,on,with=FALSE], x[ans$irows, !on, with=FALSE], i[, !on, with=FALSE])
      } else {
        ii = seqexp(ans$lens)
        cbind(i[ii,on,with=FALSE], x[ans$irows, !on, with=FALSE], i[ii, !on, with=FALSE])
      }
      stopifnot(all.equal.data.table(org, ref))
      stopifnot(all.equal.data.table(ignore.col.order=TRUE, org, out))
      #if (!isTRUE(all.equal.data.table(ignore.col.order=TRUE, org, out))) browser()
      if (copy) stopifnot(cpi, cpx)
      if (cpi) stopifnot(!adr_i %chin% adr_o)
      if (cpx) stopifnot(!adr_x %chin% adr_o)
      if (!cpi) stopifnot(adr_i %chin% adr_o)
      if (!cpx) stopifnot(adr_x %chin% adr_o)
    } else { # how: full
      org = merge.data.table(i, x, by=on, all=TRUE, sort=FALSE, allow.cartesian=allow.cartesian, mult=mult)
      #stopifnot(isTRUE(all.equal.data.table(org, out, check.attributes=FALSE)))
      if (!isTRUE(all.equal.data.table(ignore.col.order=TRUE, org, out))) browser()
      adr_r = vapply(out_r, address, "")
      stopifnot(copy, cpi, cpx, cpr)
      if (cpi) stopifnot(!adr_i %chin% adr_o)
      if (cpx) stopifnot(!adr_x %chin% adr_o)
      if (cpr) stopifnot(!adr_r %chin% adr_o)
    }
  }
  if (verbose) cat(sprintf("mergelist: took %.3fs\n", proc.time()[[3L]]-p))
  out
}

seqexp = function(x) .Call(Cseqexp, x)
perhaps.data.table = function(x) .Call(CperhapsDataTableR, x)
