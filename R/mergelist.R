cbindlist = function(l, copy=TRUE) {
  .Call(Ccbindlist, l, copy)
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
  ## do no compute sort=F for mult="first" if index (sort=T) already available, sort=T is needed for mult="last", this short circuit will work after #4386 because it requires retGrp=T
  sort = hasindex(x, by=on, retGrp=TRUE) || mult!="first"
  o = forderv(x, by=on, sort=sort, retGrp=TRUE)
  if (attr(o, "maxgrpn", TRUE) <= 1L) {
    ans = .shallow(x, someCols(x, cols, keep=on))
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

# extra layer between mergepair and bmerge
imatch = function(x, i, on, nomatch, mult, verbose, allow.cartesian) {
  icols = colnamesInt(i, on, check_dups=TRUE)
  xcols = colnamesInt(x, on, check_dups=TRUE)
  ans = bmerge(i, x, icols, xcols, roll = 0, rollends = c(FALSE, TRUE), nomatch = nomatch, mult = mult, ops=1L, verbose=verbose)
  # nocov start ## this is going to be removed because it is expensive
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
  xo = ans$xo           ## x order
  allLen1 = ans$allLen1 ## if duplicated matches occured, always TRUE for mult!="all"
  f__ = ans$starts      ## when x sorted, it marks the starts of each new group
  len__ = ans$lens      ## when no match then nomatch=0 => 0; nomatch=NA => 1, can be 2+ when multiple matches, useful to supply this into new seqexp function
  allGrp1 = ans$allGrp1   ## this is always TRUE here but in data.table.R is FALSE
  indices__ = ans$indices ## always integer() for equi join AFAIU
  if (mult!="all" && is.na(nomatch)) {
    ## for mult!="all" and nomatch==NA whole branch could be skipped
    ## irows has same length as nrow of i
    irows = f__
  } else if (allLen1) {
    ## no duplicate matches
    irows = if (identical(nomatch,0L)) f__[as.logical(len__)] else f__
  } else if (mult=="all") {
    ## some duplicate matches
    ## anyDuplicated is likely to be expensive, we should manage to get that info from somewhere else
    ## rework allow.cartesian #4383
    if (allow.cartesian ||
        !anyDuplicated(f__, incomparables = c(0L, NA_integer_))) {
      irows = vecseq(f__, len__, NULL) # #742. If 'i' has no duplicates, ignore
    } else {
      irows = vecseq(f__, len__, as.double(nrow(x)+nrow(i)))
    }
  } else {
    stop("internal error: !(mult!='all' && is.na(nomatch)) && !allLen1 && mult!='all'") # nocov
  }
  if (length(xo) && length(irows))
    irows = xo[irows]
  ans$irows = irows
  ans
}

# atomic join between two tables
mergepair = function(lhs, rhs, on, how, mult, lhs.cols, rhs.cols, copy, verbose) {
  if (is.null(on)) {
    if (how=="left") on = key(rhs)
    else if (how=="right") on = key(lhs)
    else on = onkeys(key(lhs), key(rhs))
    if (is.null(on))
      stop("'on' is missing and necessary keys were not present")
  }
  if (how!="right") { ## join-to, join-from
    jnfm = lhs; fm.cols = lhs.cols
    jnto = rhs; to.cols = rhs.cols
  } else {
    jnfm = rhs; fm.cols = rhs.cols
    jnto = lhs; to.cols = lhs.cols
  }
  allow.cartesian = TRUE
  nomatch = if (how=="inner") 0L else NA_integer_
  if (mult!="all" && (how=="inner" || how=="full")) { ## for inner|full join we apply mult on both tables, bmerge do only 'i' table
    if (mult=="first" || mult=="last") {
      jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE) ## might not copy when already unique by 'on'
    } else if (mult=="error") {
      ## we do this branch only to raise error from bmerge, we cannot use forder to just find duplicates because those duplicates might not have matching rows in another table
      imatch(x = jnfm, i = jnto, on = on, nomatch = nomatch, mult = mult, verbose = verbose, allow.cartesian = allow.cartesian)
      ## TODO test we actually need this branch
    }
  }
  ans = imatch(x = jnto, i = jnfm, on = on,
               nomatch = nomatch,
               mult = mult, verbose = verbose,
               allow.cartesian = allow.cartesian)

  if (ans$allLen1 && (how!="inner" || (
    how=="inner" && length(ans$irows)==nrow(jnfm)
    ))) { ## no duplicate matches
    out_i = .shallow(jnfm, cols=fm.cols, retain.key=TRUE, unlock=TRUE)
    cpi = FALSE ## short circuit, do not copy via CsubsetDT
  } else { ## multiple matches, so mult=="all"
    if (how!="inner" && mult!="all") stop("internal error: how!='inner'' && mult!='all' but multiple matches returned") # nocov
    out_i = .Call(CsubsetDT, jnfm, seqexp(ans$lens), someCols(jnfm, fm.cols, keep=on))
    cpi = TRUE
  }
  if (!cpi && (how=="inner" || ( ## we still need to copy because we dont want copiness to be depends on the data, and how='inner' has to copy always
    copy && how!="full" ## how='full' will copy later on in rbindlist
  ))) {
    out_i = copy(out_i)
    cpi = TRUE
  }

  ## TODO: short circuit on x, !copy for how=left, useful only when `on` values are exactly the same in x and i, so cbindlist would be enough rather than mergelist
  if (!length(ans$xo) && FALSE) { ## condition `!length(ans$xo)` is not enough
    out_x = .shallow(jnto, cols=someCols(jnto, to.cols, drop=on), retain.key=TRUE, unlock=TRUE)
    cpx = FALSE
  } else {
    out_x = .Call(CsubsetDT, jnto, ans$irows, someCols(jnto, to.cols, drop=on))
    cpx = TRUE
  }
  if (!cpi && (how=="inner" || ( ## we still need to copy because we dont want copiness to be depends on the data, and how='inner' has to copy always
    copy && how!="full" ## how='full' will copy later on in rbindlist
  ))) {
    out_x = copy(out_x)
    cpx = TRUE
  }
  ## TODO: add copy=NA, for how=left only, when we want enforce copy of RHS (x) always, but keep LHS (i) not copied

  if (how!="full") { ## not for 'full' because we need to ensure it is copied, there are 2 levels of short circuit and we need to copy only once, so we handle out (out_l) in how=full branch
    if (how=="right") {
      out = cbindlist(list(.shallow(out_i, cols=on), out_x, .shallow(out_i, cols=someCols(jnfm, fm.cols, drop=on))), copy=FALSE) ## arrange columns: i.on, x.cols, i.cols
    } else {
      out = cbindlist(list(out_i, out_x), copy=FALSE)
    }
  } else { # how=="full"
    ## we made left join already, proceed to right join
    jnfm = rhs; fm.cols = rhs.cols
    jnto = lhs; to.cols = lhs.cols
    if (mult!="all") {
      if (mult=="first" || mult=="last") {
        jnfm = fdistinct(jnfm, on=on, mult=mult, cols=fm.cols, copy=FALSE)
      } else if (mult=="error") {
        ## we do this branch only to raise error from bmerge, we cannot use forder to just find duplicates because those duplicates might not have matching rows in another table
        imatch(x = jnfm, i = jnto, on = on, nomatch = nomatch, mult = mult, verbose = verbose, allow.cartesian = allow.cartesian)
        ## TODO test we actually need this branch
      }
    }
    bns = imatch(x = jnto, i = jnfm, on = on,
                 nomatch = 0L, ## notjoin
                 mult = mult, verbose = verbose,
                 allow.cartesian = allow.cartesian)
    ni = which(bns$starts==0L) ## not join
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
      cpr = TRUE ## we havn't even materialize right side here
    } else {
      if (length(ni)==nrow(jnfm)) { ## short circuit, do not copy via CsubsetDT
        out_r = .shallow(jnfm, cols=someCols(jnfm, fm.cols, keep=on), retain.key=FALSE, unlock=FALSE)
      } else {
        out_r = .Call(CsubsetDT, jnfm, ni, someCols(jnfm, fm.cols, keep=on))
        cpr = TRUE
      }
      out_l = cbindlist(list(out_i, out_x), copy=FALSE)
      out = rbindlist(list(out_l, out_r), use.names=TRUE, fill=TRUE)
      cpi = cpx = cpr = TRUE ## all are being copied in rbindlist
    }
  }
  out
}

# TODO:
# review retain.key in .shallow
# rework imatch: rm allow.cartesian?
# missing on, key of join-to
# vectorized length(l)-1L: on, how, mult
# copy=NA
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
  for (jnto.i in seq_len(n)[-1L]) {
    out = mergepair(
      lhs = out, rhs = l[[jnto.i]], on = on, how = how, mult = mult,
      lhs.cols = out.cols, rhs.cols = cols[[jnto.i]], copy = FALSE, verbose = verbose
    )
    out.cols = copy(names(out))
  }
  out.mem = vapply(out, address, "")
  ## TODO
  if (isTRUE(copy) && FALSE) {
    if (any(out.mem %chin% unlist(l.mem, recursive=FALSE)))
      stop("Some columns have not been copied")
  } else if (FALSE) {
    copied = out.mem[intersect(names(out.mem), names(l.mem[[1L]]))]
    if (any(!copied %chin% l.mem[[1L]]))
      stop("copy!=TRUE but not all columns from first list elements has been copied")
    if (!is.na(copy) && any(out.mem %chin% unlist(l.mem[-1L], recursive=FALSE)))
      stop("copy=FALSE but some of tables in list, excluding first one, has been copied")
  }
  if (verbose) cat(sprintf("mergelist: merging %d tables, took %.3fs\n", proc.time()[[3L]]-p))
  out
}

seqexp = function(x) .Call(Cseqexp, x)
perhaps.data.table = function(x) .Call(CperhapsDataTableR, x)
