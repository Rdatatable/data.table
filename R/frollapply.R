## ansmask is to handle leading values from fill to match type of the ans
simplifylist = function(x, fill, ansmask) {
  l = lengths(x)
  ul = unique(l)
  if (length(ul)!=1L) ## different lenghts
    return(x)
  t = vapply(x, typeof, "", USE.NAMES=FALSE)
  ut = unique(t)
  if (length(ut)==2L) {
    all.ut = ut
    t = vapply(x[ansmask], typeof, "", USE.NAMES=FALSE)
    ut = unique(t)
    if (length(ut)!=1L)
      return(x) ## different typeof even excluding fill, a FUN was not type stable
    if (!(ut=="integer"||ut=="logical"||ut=="double"||ut=="complex"||ut=="character"||ut=="raw"))
      return(x) ## ans is not atomic
    if (identical(fill, NA)) { ## different typeof, try to handle fill=NA logical type
      filli = which(!ansmask)
      ans1 = x[[which.first(ansmask)]]
      x[filli] = rep_len(list(ans1[NA]), length(filli)) ## this will recycle to length of ans1
    } else if (all(c("integer","double") %in% all.ut)) { ## typeof numeric and int, fill is coerced to the type FUN
      filli = which(!ansmask)
      cast = if (ut=="double") as.numeric else as.integer
      x[filli] = rep_len(list(cast(fill)), length(filli))
    } else { ## length == 2L but no easy way to match type
      return(x)
    }
  } else if (length(ut)>2L) { ## unique typeof length > 2L
    return(x)
  }
  if (ut=="integer"||ut=="logical"||ut=="double"||ut=="complex"||ut=="character"||ut=="raw") {
    if (ul==1L) ## length 1
      return(unlist(x, recursive=FALSE, use.names=FALSE))
    else ## length 2+
      return(rbindlist(lapply(x, as.list)))
  } else if (ut=="list") {
    if (all(vapply(x, is.data.frame, FALSE, USE.NAMES=FALSE))) ## list(data.table(...), data.table(...))
      return(rbindlist(x))
    ll = lapply(x, lengths) ## length of each column of each x
    ull = unique(ll)
    if (length(ull)==1L) ## list(list(1:2, 1:2), list(2:3, 2:3))
      return(rbindlist(x))
    lu = function(x) length(unique(x))
    if (all(vapply(ull, lu, 0L, USE.NAMES=FALSE)==1L)) ## within each x column lengths the same, each could be DF: list(list(1, 2), list(1:2, 2:3))
      return(rbindlist(x))
  } ## else NULL, closure, special, builtin, environment, S4, ...
  x
}

## parallel's fork serializes results so we need setalloccol
fixselfref = function(x) {
  if (length(x) && is.data.table(x[[1L]])) { ## calling is.data.table many times always may be too much, so first we check only first element
    dtmask = vapply(x, is.data.table, FALSE, USE.NAMES=FALSE) ## most likely all, but we cannot be sure that function is type stable
    x[dtmask] = lapply(x[dtmask], setalloccol)
  }
  x
}

all.atomic = function(x) all(vapply(x, is.atomic, FALSE, USE.NAMES=FALSE))
all.data.frame = function(x) all(vapply(x, is.data.frame, FALSE, USE.NAMES=FALSE))
all.list = function(x) all(vapply(x, is.list, FALSE, USE.NAMES=FALSE))
equal.lengths = function(x) length(unique(lengths(x)))<=1L

frollapply = function(X, N, FUN, ..., by.column=TRUE, fill=NA, align=c("right","left","center"), adaptive=FALSE, partial=FALSE, give.names=FALSE, simplify=TRUE, x, n) {
  if (!missing(x)) {
    warningf("'x' is deprecated in frollapply, use 'X' instead")
    X = x
  }
  if (!missing(n)) {
    warningf("'n' is deprecated in frollapply, use 'N' instead")
    N = n
  }
  if (!isTRUEorFALSE(by.column))
    stopf("'by.column' must be TRUE or FALSE")
  if (!isTRUEorFALSE(adaptive))
    stopf("'adaptive' must be TRUE or FALSE")
  if (!isTRUEorFALSE(partial))
    stopf("'partial' must be TRUE or FALSE")
  if (!isTRUEorFALSE(give.names))
    stopf("'give.names' must be TRUE or FALSE")
  if (!isTRUEorFALSE(simplify) && !is.function(simplify))
    stopf("'simplify' must be TRUE or FALSE or a function")

  align = match.arg(align)
  FUN = match.fun(FUN)
  verbose = getOption("datatable.verbose")
  if (give.names)
    orig = list(N=N, adaptive=adaptive)

  ## by.column, x validation, x preprocess
  if (by.column) {
    if (is.atomic(X)) {
      xvec = FALSE ## marker about form of input, used to unpack answer to vector
      len = length(X) ## count of observations for deepest loop
      nx = as.integer(as.logical(len)) ## top level loop for vectorized x
      X = list(X)
      xnam = character() ## used for give.names
    } else if (is.list(X) && all.atomic(X)) {
      xvec = TRUE
      nx = length(X)
      len = lengths(X)
      xnam = names(X)
    } else
      stopf("frollapply by.column=TRUE requires 'X' argument to be atomic or a list of those")
  } else {
    list.df = FALSE
    if (is.data.frame(X)) {
      xvec = FALSE
      len = nrow(X)
      nx = as.integer(as.logical(len))
      X = list(X)
      xnam = character()
    } else if (is.list(X)) {
      if (all.atomic(X)) { ## handles frollapply(.(col1, col2), ...)
        if (!equal.lengths(X))
          stopf("frollapply by.column=FALSE, when provided a list in 'X' then all vectors must have equal lengths, like data.frame")
        list.df = TRUE
        xvec = FALSE
        len = if (length(X)) length(X[[1L]]) else 0L
        nx = as.integer(as.logical(len))
        X = list(X)
        xnam = character()
      } else if (all.data.frame(X)) {
        if (!all(vapply(X, all.atomic, FALSE, USE.NAMES=FALSE)))
          stopf("frollapply by.column=FALSE got vectorized input in 'X', list of data.frames/data.tables, but not all columns of data.frames/data.tables are atomic")
        xvec = TRUE
        len = vapply(X, nrow, 0L, USE.NAMES=FALSE)
        nx = length(X)
        xnam = names(X)
      } else if (all.list(X)) { ## vectorized input does not support lists as that would be ambiguous
        stopf("frollapply by.column=FALSE supports vectorized input in 'X' as a list of data.frames/data.tables, not a list of lists. Turn nested lists into data.frames/data.table and retry.")
      } else { ## mixed types
        stopf("frollapply by.column=FALSE got list in 'X' but it is not valid one. If intent is to pass a list as non-vectorized input, but a single object to apply function to, then the list must have all its vectors atomic. For a vectorized input, passing multiple objects to apply function to, it must be a list of data.frames/data.tables.")
      }
    } else
      stopf("frollapply by.column=FALSE requires 'X' argument to be a data.table/data.frame or a list of equal length vectors. For vectorized input can be a list of data.frames/data.tables, but not a list of lists. All columns/vectors must be atomic.")
  }
  ## adaptive, n validation, n preprocess
  if (!length(N))
    stopf("'N' must be non 0 length")
  if (!adaptive) {
    if (is.list(N))
      stopf("'N' must be integer, list is accepted for adaptive TRUE")
    else if (!is.numeric(N))
      stopf("'N' must be integer vector")
    nnam = names(N) ## used for give.names
    if (!is.integer(N))
      N = as.integer(N)
    nn = length(N) ## top level loop for vectorized n
  } else {
    if (length(unique(len)) > 1L) ## vectorized x requires same nrow for adaptive
      stopf("adaptive rolling function can only process 'X' having equal length of elements; If you want to call rolling function on list having variable length of elements call it for each field separately")
    if (is.numeric(N)) {
      if (length(N) != len[1L])
        stopf("length of integer vector(s) provided as list to 'N' argument must be equal to number of observations provided in 'X'")
      if (!is.integer(N))
        N = as.integer(N)
      nn = 1L
      N = list(N)
      nnam = character()
    } else if (is.list(N)) {
      if (length(N[[1L]])!=len[1L])
        stopf("length of integer vector(s) provided as list to 'N' argument must be equal to number of observations provided in 'X'")
      if (!equal.lengths(N))
        stopf("adaptive windows provided in 'N' must not to have different lengths")
      if (!all(vapply(N, is.numeric, FALSE, USE.NAMES=FALSE)))
        stopf("n must be integer vector or list of integer vectors")
      if (!all(vapply(N, is.integer, FALSE, USE.NAMES=FALSE)))
        N = lapply(N, as.integer)
      nn = length(N)
      nnam = names(N)
    } else
      stopf("'N' must be integer vector or list of integer vectors")
  }
  ## partial
  if (partial) {
    N = partial2adaptive(X, N, align, adaptive)
    adaptive = TRUE
  }
  ## left adaptive preprocess x and n
  if (adaptive) {
    if (align=="center")
      stopf("using adaptive TRUE and align 'center' is not implemented")
    leftadaptive = align=="left"
  } else leftadaptive = FALSE
  if (leftadaptive) {
    if (verbose)
      cat("frollapply: adaptive=TRUE && align='left' pre-processing for align='right'\n")
    if (by.column) {
      X = lapply(X, rev)
    } else {
      rev.d = function(d) {
        l = lapply(d, rev)
        if (is.data.table(d)) setDT(l) else if (is.data.frame(d)) setDF(l) else l
      }
      X = lapply(X, rev.d)
    }
    N = lapply(N, rev)
    align = "right"
  }

  ## prepare functions so we don't need to branch inside the loops, makes code in loops cleaner as well
  ## only tight has to be optimized
  if (!adaptive) {
    cpy = copy
    ansMask = function(len, n) {
      mask = rep(TRUE, len)
      mask[seq_len(n-1L)] = FALSE
      mask
    }
    if (by.column) {
      allocWindow = function(x, n) x[seq_len(n)]
      tight = function(i, dest, src, n) FUN(.Call(CmemcpyVector, dest, src, i, n), ...)
    } else {
      if (!list.df) {
        allocWindow = function(x, n) x[seq_len(n), , drop=FALSE]
      } else {
        allocWindow = function(x, n) lapply(x, `[`, seq_len(n))
      }
      tight = function(i, dest, src, n) FUN(.Call(CmemcpyDT, dest, src, i, n), ...)
    }
  } else {
    has.growable = base::getRversion() >= "3.4.0"
    cpy = if (has.growable) function(x) .Call(Csetgrowable, copy(x)) else copy
    ansMask = function(len, n) {
      mask = seq_len(len) >= n
      mask[is.na(mask)] = FALSE ## test 6010.206
      mask
    }
    if (by.column) {
      allocWindow = function(x, n) x[seq_len(max(n, na.rm=TRUE))]
      if (has.growable) {
        tight = function(i, dest, src, n) FUN(.Call(CmemcpyVectoradaptive, dest, src, i, n), ...)
      } else {
        tight = function(i, dest, src, n) FUN(src[(i-n[i]+1L):i], ...)
      }
    } else {
      if (!list.df) {
        allocWindow = function(x, n) x[seq_len(max(n, na.rm=TRUE)), , drop=FALSE]
      } else {
        allocWindow = function(x, n) lapply(x, `[`, seq_len(max(n)))
      }
      if (has.growable) {
        tight = function(i, dest, src, n) FUN(.Call(CmemcpyDTadaptive, dest, src, i, n), ...)
      } else {
        if (!list.df) {
          tight = function(i, dest, src, n) FUN(src[(i-n[i]+1L):i, , drop=FALSE], ...)
        } else {
          tight = function(i, dest, src, n) FUN(lapply(src, `[`, (i-n[i]+1L):i), ...)
        }
      }
    }
  }
  ## prepare templates for errors and warnings
  msg.collect = "frollapply calling parallel::mccollect to collect results from forked processes raised %s.\n%s"
  msg.simplify = if (is.function(simplify))
    "frollapply completed successfully but raised %s when attempting to simplify results using user specified function in 'simplify' argument. Be sure to provide 'fill' argument matching the type and shape of results returned by the your function. Use simplify=FALSE to obtain a list instead.\n%s"
  else if (isTRUE(simplify))
    "frollapply completed successfully but raised %s when attempting to simplify results using our internal 'simplifylist' function. Be sure to provide 'fill' argument matching the type and shape of results returned by the your function. Use simplify=FALSE to obtain a list instead. If you believe your results could be automatically simplified please submit your use case as new issue in our issue tracker.\n%s"

  DTths = getDTthreads(FALSE)
  use.fork = .Platform$OS.type!="windows" && DTths > 1L
  if (verbose) {
    if (use.fork) cat("frollapply running on multiple CPU threads using parallel::mcparallel\n")
    else cat("frollapply running on single CPU thread\n")
  }
  ans = vector("list", nx*nn)
  ## vectorized x
  for (i in seq_len(nx)) {
    thisx = X[[i]]
    thislen = len[i]
    if (!thislen)
      next
    ## vectorized n
    for (j in seq_len(nn)) {
      thisn = N[[j]]
      w = allocWindow(thisx, thisn) ## prepare window, handles adaptive
      ansmask = ansMask(thislen, thisn)
      ansi = which(ansmask)
      if (use.fork) { ## !windows && getDTthreads()>1L
        ths = min(DTths, length(ansi))
        ii = split(ansi, sort(rep_len(seq_len(ths), length(ansi)))) ## assign row indexes to threads
        jobs = vector("integer", ths)
        for (th in seq_len(ths)) {
          jobs[th] = parallel::mcparallel({
            setDTthreads(1L)       ## disable nested parallelism
            lapply(ii[[th]],       ## loops over indexes for that thread
                   FUN = tight,    ## handles adaptive and by.column
                   dest = cpy(w),  ## allocate own window for each thread
                   src = thisx,    ## full input
                   n = thisn)      ## scalar or in adaptive case a vector
          })[["pid"]]
        }
      } else { ## windows || getDTthreads()==1L
        h = list(err=NULL, warn=NULL) ## pretty printing errors/warnings
        oldDTthreads = setDTthreads(1L) ## for consistency, anyway window size is unlikely to be big enough to benefit any parallelism
        withCallingHandlers(
          tryCatch(
            thisans <- lapply(ansi, FUN = tight, dest = cpy(w), src = thisx, n = thisn),
            error = function(e) h$err <<- conditionMessage(e)
          ), warning = function(w) {h$warn <<- c(h$warn, conditionMessage(w)); invokeRestart("muffleWarning")}
        )
        setDTthreads(oldDTthreads)
        if (!is.null(h$warn))
          warningf("frollapply received a warning(s) when evaluating FUN:\n%s", paste(unique(h$warn), collapse="\n"))
        if (!is.null(h$err))
          stopf("frollapply received an error(s) when evaluating FUN:\n%s", h$err)
      }
      ## align
      if (leftadaptive) {
        ansmask = rev(ansmask)
        ansi = which(ansmask)
      } else if (align!="right") { ## must be non-adaptive bc adaptive don't support align=center
        ansmask = shift(ansmask, if (align=="left") 1L-thisn else -floor(thisn/2L), fill=FALSE, type="shift")
        ansi = which(ansmask)
      }
      ## fill
      thisansi = (i-1L)*nn+j
      ans[[thisansi]] = vector("list", thislen)
      filli = which(!ansmask)
      ans[[thisansi]][filli] = rep_len(list(fill), length(filli))
      ## collect results
      if (length(ansi)) {
        if (use.fork) {
          fork.res = tryCatch(
            parallel::mccollect(jobs),
            error = function(e) stopf(msg.collect, "an error", e[["message"]]),
            warning = function(w) warningf(msg.collect, "a warning", w[["message"]])
          )
          ## check for any errors in FUN, warnings are silently ignored
          fork.err = vapply(fork.res, inherits, FALSE, "try-error", USE.NAMES=FALSE)
          if (any(fork.err))
            stopf("frollapply received an error(s) when evaluating FUN:\n%s",
                  paste(unique(vapply(fork.res[fork.err], function(err) attr(err,"condition",TRUE)[["message"]], "", USE.NAMES=FALSE)), collapse="\n"))
          thisans = unlist(fork.res, recursive=FALSE, use.names=FALSE)
          ## fix selfref after serializing data.table from forked process
          thisans = fixselfref(thisans)
        } ## thisans is already created from !use.fork, don't need error check, unlist or fixselfref
        if (leftadaptive)
          thisans = rev(thisans)
        ans[[thisansi]][ansi] = thisans
      }
      ## simplify
      if (is.function(simplify)) {
        ans[[thisansi]] = tryCatch(
          simplify(ans[[thisansi]]),
          error = function(e) stopf(msg.simplify, "an error", e[["message"]]),
          warning = function(w) warningf(msg.simplify, "a warning", w[["message"]])
        )
      } else if (isTRUE(simplify)) {
        ans[[thisansi]] = tryCatch(
          simplifylist(ans[[thisansi]], fill, ansmask),
          error = function(e) stopf(msg.simplify, "an error", e[["message"]]),
          warning = function(w) warningf(msg.simplify, "a warning", w[["message"]])
        )
      }
    }
  }

  ## preparing output format
  if (length(ans)) {
    if (!xvec && length(ans)==1L) {
      ans = ans[[1L]] ## unpack atomic input
    } else if (give.names) {
      nms = make.roll.names(x.len=nx, n.len=nn, n=orig$N, x.nm=xnam, n.nm=nnam, fun="apply", adaptive=orig$adaptive)
      setattr(ans, "names", nms)
    }
  }
  ans
}
