## those two helpers does not quote argument names in errors because frollapply has them in uppercase
partial2adaptive = function(x, n, align, adaptive) {
  if (align=="center")
    stopf("'partial' cannot be used together with align='center'")
  if (is.list(x)) {
    if (!is.data.frame(x) && length(unique(vapply(x, length, 0L)))!=1L) ## froll
      stopf("'partial' does not support variable length of columns in x")
    else if (all(vapply(x, is.data.frame, FALSE)) && length(unique(vapply(x, nrow, 0L)))!=1L) ## frollapply by.column=F, single DT already wrapped into list
      stopf("'partial' does not support variable nrow of data.tables in x")
  }
  if (!adaptive) {
    if (is.list(n))
      stopf("n must be integer, list is accepted for adaptive TRUE")
    else if (!is.numeric(n))
      stopf("n must be integer vector")
  } else if (!(is.numeric(n) || (is.list(n) && all(vapply(n, is.numeric, FALSE))))) {
    stopf("n must be integer vector or list of integer vectors")
  }
  len = if (is.list(x)) {
    if (is.data.frame(x[[1L]])) ## frollapply by.column
      nrow(x[[1L]])
    else
      length(x[[1L]]) ## froll, this will work for both x list and x dt on input
  } else length(x)
  verbose = getOption("datatable.verbose")
  if (!adaptive) {
    n = as.list(n) ## test 6006.032
    if (verbose)
      cat("partial2adaptive: froll partial=TRUE trimming n and redirecting to adaptive=TRUE\n")
    trimn = function(n, len, align) {
      n = min(n, len)
      if (align=="right")
        c(seq_len(n), rep(n, len-n))
      else
        c(rep(n, len-n), rev(seq_len(n)))
    }
    sapply(n, len, align, FUN=trimn, simplify=FALSE)
  } else {
    if (!is.list(n)) n = list(n)
    if (length(unique(vapply(n, length, 0L)))!=1L)
      stopf("adaptive windows provided in n must not to have different lengths")
    if (length(n[[1L]]) != len)
      stopf("length of vectors in x must match to length of adaptive window in n")
    if (verbose)
      cat("partial2adaptive: froll adaptive=TRUE and partial=TRUE trimming n\n")
    triman = function(n, align) {
      if (align=="right")
        pmin(n, seq_along(n))
      else
        pmin(n, rev(seq_along(n)))
    }
    sapply(n, align, FUN=triman, simplify=FALSE)
  }
}
make.roll.names = function(x.len, n.len, n, x.nm, n.nm, fun, adaptive) {
  if (is.null(n.nm)) {
    if (!adaptive) {
      if (!is.numeric(n))
        stopf("internal error: misuse of make.names, n must be numeric for !adaptive") ## nocov
      n.nm = paste0("roll", fun, as.character(as.integer(n)))
    } else {
      n.nm = paste0("aroll", fun, seq_len(n.len))
    }
  } else if (!length(n.nm) && !adaptive)
    stopf("internal error: misuse of make.names, non-null length 0 n is not possible for !adaptive") ## nocov
  if (is.null(x.nm)) {
    x.nm = paste0("V", seq_len(x.len))
  }
  ans = if (length(x.nm)) { ## is.list(x) && !is.data.frame(x)
    if (length(n.nm)) { ## !adaptive || is.list(n)
      paste(rep(x.nm, each=length(n.nm)), n.nm, sep="_")
    } else { ## adaptive && is.numeric(n)
      x.nm
    }
  } else { ## (by.column && is.atomic(x)) || (!by.column && is.data.frame(x))
    if (length(n.nm)) { ## !adaptive || is.list(n)
      n.nm
    } else { ## adaptive && is.numeric(n)
      NULL
    }
  }
  if (!is.null(ans) && length(ans) != x.len*n.len)
    stopf("internal error: make.names generated names of wrong length") ## nocov
  ans
}

## reference implementation for C frolladapt, will be removed after testing
aw = function(x, n, partial=FALSE) {
  for (i in seq_along(x)[-1L])
    if (x[i] <= x[i-1L]) stop("not sorted or duplicates")
  ans = rep.int(NA_integer_, length(x))
  x0 = x[1]
  i = 1
  j = 1
  while (i <= length(x)) {
    lhs = x[i]
    rhs = x[j]
    an = i-j+1L                 ## window we are currently looking at in this iteration
    #cat(sprintf("i=%d, j=%d\n", i, j))
    #browser()
    if (an > n)
      stop("internal error: an > n, should not increment i in the first place")
    else if (an == n) {         ## an is same size as n, so we either have no gaps or will need to shrink an by j++
      if (lhs == rhs+n-1L) {    ## no gaps - or a k gaps and a k dups?
        ans[i] = n              ## could skip if pre-fill
        i = i+1L
        j = j+1L
      } else if (lhs > rhs+n-1L) { ## need to shrink an
        j = j+1L
      } else {
        stop("internal error: no sorted, should be been detected by now")
      }
    }
    else if (an < n) {          ## there are some gaps
      if (lhs == rhs+n-1L) {    ## gap and rhs matches the bound, so increment i and j
        ans[i] = an
        i = i+1L
        j = j+1L
      } else if (lhs > rhs+n-1L) { ## need to shrink an
        ans[i] = an                ## likely to be overwritten by smaller an if shrinking continues because i is not incremented
        j = j+1L
      } else if (lhs < rhs+n-1L) {
        ans[i] = if (!partial && lhs-x0<n) NA_integer_ else an ## for i==j an=1L
        i = i+1L
      }
    }
  }
  ans
}
## irregularly spaced time series, helper for creating adaptive window size
frolladapt = function(x, n, partial=FALSE, give.names=FALSE, .validate=FALSE) {
  x = unclass(x)
  if (!is.numeric(x))
    stopf("Index vector 'x' must of numeric type")
  if (!is.integer(x))
    x = as.integer(x)
  if (!is.numeric(n)) {
    stopf("Window size 'n' must be integer")
  } else {
    nms = names(n) ## only for give.names
    if (!is.integer(n)) {
      if (!isRealReallyInt(n))
        stopf("Window size 'n' must be integer")
      n = as.integer(n)
    }
  }
  if (length(n) < 1L || anyNA(n))
    stopf("Argument 'n' must be non-zero length and must not have NAs")
  if (!isTRUEorFALSE(partial))
    stopf("Argument 'partial' must be TRUE or FALSE")
  if (!isTRUEorFALSE(give.names))
    stopf("Argument 'give.names' must be TRUE or FALSE")

  if (length(n) == 1L) {
    ans = .Call(Cfrolladapt, x, n, partial)
  } else {
    ans = lapply(n, function(.n) .Call(Cfrolladapt, x, .n, partial))
    if (give.names) {
      if (is.null(nms))
        nms = paste0("n", as.character(n))
      setattr(ans, "names", nms)
    }
  }
  if (!.validate) return(ans)

  ## validation1
  d = as.data.table(list(id=x))
  set(d,, "from", x-n+1L)
  an = d[d, on=.(id <= id, id >= from), .N, by=.EACHI]$N
  if (!partial) {
    first = x[1L]+n-1L
    incomplete = which(x < first)
    an[incomplete] = rep.int(NA_integer_, length(incomplete))
  }
  an1 = an
  ok1 = all.equal(an1, ans)
  ## validation2
  an2 = aw(x, n, partial)
  ok2 = all.equal(an2, ans)

  if (isTRUE(ok1) && isTRUE(ok2)) ans else {
    warning("results does not match")
    list(ans=ans, an1=an1, an2=an2)
  }
}

froll = function(fun, x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA=NA) {
  stopifnot(!missing(fun), is.character(fun), length(fun)==1L, !is.na(fun))
  if (!missing(hasNA)) {
    if (!is.na(has.nf))
      stopf("hasNA is deprecated, use has.nf instead")
    warning("hasNA is deprecated, use has.nf instead")
    has.nf = hasNA
  } # remove check on next major release
  algo = match.arg(algo)
  align = match.arg(align)
  if (isTRUE(give.names)) {
    orig = list(n=n, adaptive=adaptive)
    xnam = if (is.list(x)) names(x) else character()
    nnam = if (isTRUE(adaptive)) {
      if (is.list(n)) names(n) else character()
    } else names(n)
    nx = if (is.list(x)) length(x) else 1L
    nn = if (isTRUE(adaptive)) {
      if (is.list(n)) length(n) else 1L
    } else length(n)
  }
  if (isTRUE(partial)) {
    n = partial2adaptive(x, n, align, adaptive)
    adaptive = TRUE
  } ## support for partial added in #5441
  leftadaptive = isTRUE(adaptive) && align=="left"
  if (leftadaptive) {
    verbose = getOption("datatable.verbose")
    rev2 = function(x) if (is.list(x)) sapply(x, rev, simplify=FALSE) else rev(x)
    if (verbose)
      cat("froll: adaptive=TRUE && align='left' pre-processing for align='right'\n")
    x = rev2(x)
    n = rev2(n)
    align = "right"
  } ## support for left adaptive added in #5441
  ans = .Call(CfrollfunR, fun, x, n, fill, algo, align, na.rm, has.nf, adaptive)
  if (leftadaptive) {
    if (verbose)
      cat("froll: adaptive=TRUE && align='left' post-processing from align='right'\n")
    ans = rev2(ans)
  }
  if (isTRUE(give.names) && is.list(ans)) {
    nms = make.roll.names(x.len=nx, n.len=nn, n=orig$n, x.nm=xnam, n.nm=nnam, fun=fun, adaptive=orig$adaptive)
    setattr(ans, "names", nms)
  }
  ans
}

frollmean = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA) {
  froll(fun="mean", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
frollsum = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA) {
  froll(fun="sum", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
frollmax = function(x, n, fill=NA, algo=c("fast","exact"), align=c("right","left","center"), na.rm=FALSE, has.nf=NA, adaptive=FALSE, partial=FALSE, give.names=FALSE, hasNA) {
  froll(fun="max", x=x, n=n, fill=fill, algo=algo, align=align, na.rm=na.rm, has.nf=has.nf, adaptive=adaptive, partial=partial, hasNA=hasNA, give.names=give.names)
}
