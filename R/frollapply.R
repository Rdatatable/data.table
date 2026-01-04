all_atomic = function(x) all(vapply_1b(x, is.atomic, use.names=FALSE))
all_data.frame = function(x) all(vapply_1b(x, is.data.frame, use.names=FALSE))
all_list = function(x) all(vapply_1b(x, is.list, use.names=FALSE))
all_types = function(x) vapply_1c(x, typeof, use.names=FALSE)
all_names = function(x) lapply(x, names)
len_unq = function(x) length(unique(x))
any_NA_neg = function(x) anyNA(x) || any(x < 0L)
any_NA_names = function(x) anyNA(names(x))
all_NULL_names = function(x) all(vapply_1b(x, function(y) is.null(names(y)), use.names=FALSE))
equal.lengths = function(x) len_unq(lengths(x)) <= 1L
equal.nrows = function(x) len_unq(lapply(x, nrow)) <= 1L
equal.dims = function(x) len_unq(lapply(x, dim)) <= 1L
equal.types = function(x) len_unq(all_types(x)) <= 1L
equal.names = function(x) len_unq(all_names(x)) <= 1L

## ansmask is to handle leading values from fill to match type of the ans
simplifylist = function(x, fill, ansmask) {
  all.t = all_types(x)
  all.ut = unique(all.t)
  if (length(all.ut) > 1L) {
    ans.ut = unique(all_types(x[ansmask]))
    ## coerce int to double when varies within answer results: median #7313
    if (
      length(ans.ut) == 2L &&                   ## simplifylist(list(NA, 1, 1L, TRUE), NA, ansmask=c(F,T,T,T))
      all(c("double","integer") %in% ans.ut) && ## simplifylist(list(NA, 1, TRUE), NA, ansmask=c(F,T,T))
      equal.lengths(x[ansmask])                 ## simplifylist(list(NA, 1, 1:2), NA, ansmask=c(F,T,T))
    ) {
      x[ansmask & all.t=="integer"] = lapply(x[ansmask & all.t=="integer"], as.numeric) ## coerce integer to double
      ans.ut = "double"
    }
    ## coerce fill to answers type and length
    if (
      length(ans.ut) == 1L &&      ## simplifylist(list(NA, 1, TRUE), NA, ansmask=c(F,T,T))
      equal.lengths(x[ansmask]) && ## simplifylist(list(NA, 1L, 1:2), NA, ansmask=c(F,T,T))
      is.atomic(fill)              ## simplifylist(list(list(NA), list(1), list(2)), list(NA), ansmask=c(F,T,T))
    ) {
      fill.t = typeof(fill)
      ans1 = x[[which.first(ansmask)]] ## first ans from full window, all ans same type by now
      ## coerce fill to type
      if (identical(fill, NA)) {
        if (ans.ut == "list") {
          fill = lapply(ans1, `[`, NA) ## we want list(NA) rather than list(NULL), this also propagates names
        } else {
          fill = setNames(ans1[NA], names(ans1))
        }
        fill.t = ans.ut
      } else if (
        fill.t != ans.ut &&                  ## simplifylist(list(-1, 1, 2), -1, ansmask=c(F,T,T))
        fill.t %in% c("double","integer") && ## simplifylist(list(NULL, 1, 2), NULL, ansmask=c(F,T,T))
        ans.ut %in% c("double","integer")    ## simplifylist(list(1, "a", "b"), 1, ansmask=c(F,T,T))
      ) { ## fill=-2, ans=1L
        if (fill.t == "integer" && ans.ut == "double") {
          fill = as.double(fill)
        } else if (fill.t == "double" && ans.ut == "integer") {
          fill = as.integer(fill)
        } else {
          internal_error("coerce fill type reached a branch of unexpected fill type and answer type") # nocov
        }
        fill.t = ans.ut
      }
      ## name fill if all ans have same names
      if (
        ans.ut != "list" &&
        length(fill) == length(ans1) &&
        is.null(names(fill)) &&
        !is.null(names(ans1)) &&                                             ## simplifylist(list(NA, c(1,2), c(1,2)), NA, ansmask=c(F,T,T))
        len_unq(vapply_1b(x[ansmask], any_NA_names, use.names=FALSE)) <= 1L && ## simplifylist(list(NA, c(a=1,b=2), setNames(c(1, 2), c(NA,"b"))), NA, ansmask=c(F,T,T))
        equal.names(x[ansmask])                                              ## simplifylist(list(NA, c(a=1,b=2), c(x=1,y=2)), NA, ansmask=c(F,T,T))
      ) {
        fill = setNames(fill, names(ans1))
      }
      ## recycle fill
      filli = which(!ansmask)
      x[filli] = rep_len(list(fill), length(filli))
    }
    all.ut = unique(all_types(x))
  }
  if (
    !is.null(names(fill)) &&
    all_NULL_names(x[ansmask]) &&
    equal.lengths(x)
  ) {
    nm = names(fill)
    x[ansmask] = lapply(x[ansmask], `names<-`, nm)
  }
  if (length(all.ut) == 1L) {
    if (all.ut %in% c("integer","logical","double","complex","character","raw")) {
      if (identical(unique(lengths(x)), 1L)) { ## all length 1
        return(unlist(x, recursive=FALSE, use.names=FALSE))
      } else if (
        equal.lengths(x) &&
        equal.names(x)
      ) { ## length 2+ and equal
        return(rbindlist(lapply(x, as.list)))
      }
    } else if (identical(all.ut,"list")) {
      if (
        all_data.frame(x) && ## simplifylist(list(NA, list(a=1L, b=2L), data.table(a=1L, b=2L)), NA, ansmask=c(F,T,T))
        equal.dims(x) &&     ## simplifylist(list(NA, data.table(a=1L, b=2L), data.table(a=1L)), NA, ansmask=c(F,T,T))
        equal.types(x) &&    ## simplifylist(list(NA, data.table(a=1L, b=2L), data.table(a=1L, b="b")), NA, ansmask=c(F,T,T))
        equal.names(x)       ## simplifylist(list(NA, data.table(a=1L, b=2L), data.table(x=1L, y=2L)), NA, ansmask=c(F,T,T))
      ) {
        return(rbindlist(x))
      } else if (
        equal.lengths(x) &&
        len_unq(lapply(x, lengths, use.names=FALSE)) <= 1L && ## nested lengths
        len_unq(lapply(lapply(x, unlist, recursive=FALSE, use.names=FALSE), typeof)) <= 1L &&
        equal.names(x)
      ) { ## same length lists: list(list(1:2, 1:2), list(2:3, 2:3))
        return(rbindlist(x))  ## simplifylist(list(NA, list(1:2, 1:2), list(2:3, 2:3)), NA, ansmask=c(F,T,T))
      }
    }
  }
  ## not simplified, return as is, see #7317
  # NULL, closure, special, builtin, environment, S4, ...
  x
}

## parallel's fork serializes results so we need setalloccol
fixselfref = function(x) {
  if (length(x) && is.data.table(x[[1L]])) { ## calling is.data.table many times always may be too much, so first we check only first element
    dtmask = vapply_1b(x, is.data.table, use.names=FALSE) ## most likely all, but we cannot be sure that function is type stable
    x[dtmask] = lapply(x[dtmask], setalloccol)
  }
  x
}

frollapply = function(X, N, FUN, ..., by.column=TRUE, fill=NA, align=c("right","left","center"), adaptive=FALSE, partial=FALSE, give.names=FALSE, simplify=TRUE, x, n) {
  if (!missing(x)) {
    stopf("'x' is deprecated in frollapply, use 'X' instead")
  }
  if (!missing(n)) {
    stopf("'n' is deprecated in frollapply, use 'N' instead")
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
  if (!length(X))
    return(vector(mode=typeof(X), length=0L))
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
    } else if (is.list(X) && all_atomic(X)) {
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
      if (all_atomic(X)) { ## handles frollapply(.(col1, col2), ...)
        if (!equal.lengths(X))
          stopf("frollapply by.column=FALSE, when provided a list in 'X' then all vectors must have equal lengths, like data.frame")
        list.df = TRUE
        xvec = FALSE
        len = if (length(X)) length(X[[1L]]) else 0L
        nx = as.integer(as.logical(len))
        X = list(X)
        xnam = character()
      } else if (all_data.frame(X)) {
        if (!all(vapply_1b(X, all_atomic, use.names=FALSE)))
          stopf("frollapply by.column=FALSE got vectorized input in 'X', list of data.frames/data.tables, but not all columns of data.frames/data.tables are atomic")
        xvec = TRUE
        len = vapply_1i(X, nrow, use.names=FALSE)
        nx = length(X)
        xnam = names(X)
      } else if (all_list(X)) { ## vectorized input does not support lists as that would be ambiguous
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
      stopf("'N' must be an integer, list is accepted for adaptive TRUE")
    else if (!is.numeric(N))
      stopf("'N' must be an integer")
    nnam = names(N) ## used for give.names
    if (!is.integer(N))
      N = as.integer(N)
    if (any_NA_neg(N))
      stopf("'N' must be non-negative integer values (>= 0)")
    nn = length(N) ## top level loop for vectorized n
  } else {
    if (length(unique(len)) > 1L) ## vectorized x requires same nrow for adaptive
      stopf("adaptive rolling function can only process 'X' having equal length of elements; If you want to call rolling function on list having variable length of elements call it for each field separately")
    if (is.numeric(N)) {
      if (length(N) != len[1L])
        stopf("length of integer vector(s) provided as list to 'N' argument must be equal to number of observations provided in 'X'")
      if (!is.integer(N))
        N = as.integer(N)
      if (any_NA_neg(N))
        stopf("'N' must be non-negative integer values (>= 0)")
      nn = 1L
      N = list(N)
      nnam = character()
    } else if (is.list(N)) {
      if (length(N[[1L]])!=len[1L])
        stopf("length of integer vector(s) provided as list to 'N' argument must be equal to number of observations provided in 'X'")
      if (!equal.lengths(N))
        stopf("adaptive windows provided in 'N' must not to have different lengths")
      if (!all(vapply_1b(N, is.numeric, use.names=FALSE)))
        stopf("'N' must be an integer vector or list of integer vectors")
      if (!all(vapply_1b(N, is.integer, use.names=FALSE)))
        N = lapply(N, as.integer)
      if (any(vapply_1b(N, any_NA_neg, use.names=FALSE)))
        stopf("'N' must be non-negative integer values (>= 0)")
      nn = length(N)
      nnam = names(N)
    } else
      stopf("'N' must be an integer vector or list of integer vectors")
  }
  ## partial
  if (partial) {
    N = partial2adaptive(X, N, align, adaptive)
    if (!is.list(N))
      N = list(N)
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
      catf("frollapply: adaptive=TRUE && align='left' pre-processing for align='right'\n")
    if (by.column) {
      X = lapply(X, rev)
    } else {
      rev.d = function(d) {
        l = lapply(d, rev)
        if (is.data.table(d)) setDT(l) else if (is.data.frame(d)) setDF(l) else l
      }
      X = lapply(X, rev.d)
    }
    stopifnot(is.list(N)) ## internal
    N = lapply(N, rev)
    align = "right"
  }

  ## prepare functions so we don't need to branch inside the loops, makes code in loops cleaner as well
  ## only tight has to be optimized
  if (!adaptive) {
    cpy = copy
    ansMask = function(len, n) {
      mask = rep(TRUE, len)
      if (n) ## handle n==0
        mask[seq_len(n-1L)] = FALSE
      mask
    }
    tight0 = function(i, dest, src, n) FUN(dest, ...) ## skip memcpy when n==0
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
    cpy = function(x) .Call(CcopyAsGrowable, x)
    ansMask = function(len, n) {
      mask = seq_len(len) >= n
      mask[is.na(mask)] = FALSE ## test 6010.206
      mask
    }
    if (by.column) {
      allocWindow = function(x, n) cpy(x[seq_len(max(n, na.rm=TRUE))])
      tight = function(i, dest, src, n) FUN(.Call(CmemcpyVectoradaptive, dest, src, i, n), ...) # CmemcpyVectoradaptive handles k[i]==0
    } else {
      if (!list.df) {
        allocWindow = function(x, n) cpy(x[seq_len(max(n, na.rm=TRUE)), , drop=FALSE])
      } else {
        allocWindow = function(x, n) cpy(lapply(x, `[`, seq_len(max(n))))
      }
      tight = function(i, dest, src, n) FUN(.Call(CmemcpyDTadaptive, dest, src, i, n), ...) # CmemcpyDTadaptive handles k[i]==0
    }
  }
  ## prepare templates for errors and warnings
  err.collect = gettext("frollapply calling parallel::mccollect to collect results from forked processes raised an error.\n%s")
  warn.collect = gettext("frollapply internal call to parallel::mccollect raised a warning, FUN warnings should have been suppressed by parallel.\n%s")
  if (is.function(simplify)) {
    err.simplify = gettext("frollapply completed successfully but raised an error when attempting to simplify results using user specified function in 'simplify' argument. Be sure to provide 'fill' argument matching the type and shape of results returned by the your function. Use simplify=FALSE to obtain a list instead.\n%s")
    warn.simplify = gettext("frollapply completed successfully but raised a warning when attempting to simplify results using user specified function in 'simplify' argument. Be sure to provide 'fill' argument matching the type and shape of results returned by the your function. Use simplify=FALSE to obtain a list instead.\n%s")
  } else if (isTRUE(simplify)) {
    err.simplify = gettext("frollapply completed successfully but raised an error when attempting to simplify results using our internal 'simplifylist' function. Be sure to provide 'fill' argument matching the type and shape of results returned by the your function. Use simplify=FALSE to obtain a list instead. If you believe your results could be automatically simplified please submit your use case as new issue in our issue tracker.\n%s")
    warn.simplify = gettext("frollapply completed successfully but raised a warning when attempting to simplify results using our internal 'simplifylist' function. Be sure to provide 'fill' argument matching the type and shape of results returned by the your function. Use simplify=FALSE to obtain a list instead. If you believe your results could be automatically simplified please submit your use case as new issue in our issue tracker.\n%s")
  }

  DTths0 = getDTthreads(FALSE)
  use.fork0 = .Platform$OS.type!="windows" && DTths0 > 1L
  if (verbose && !use.fork0)
    catf("frollapply running on single CPU thread\n")
  ans = vector("list", nx*nn)
  ## vectorized x

  for (i in seq_len(nx)) {
    thisx = X[[i]]
    thislen = len[i]
    if (thislen) {
      if (!use.fork0) {
        use.fork = use.fork0
      } else {
        # throttle
        DTths = getDTthreadsC(thislen, TRUE)
        use.fork = DTths > 1L
        if (verbose) {
          if (DTths < DTths0)
            catf("frollapply run on %d CPU threads throttled to %d threads, input length %d\n", DTths0, DTths, thislen)
          else
            catf("frollapply running on %d CPU threads\n", DTths)
        }
      }
    }
    ## vectorized n
    for (j in seq_len(nn)) {
      if (!thislen) {
        ans[[(i-1L)*nn+j]] = vector(mode=typeof(thisx), length=0L)
        next
      }
      thisn = N[[j]]
      w = allocWindow(thisx, thisn) ## prepare window, handles adaptive
      ansmask = ansMask(thislen, thisn)
      ansi = which(ansmask)
      tightFUN = if (adaptive || thisn) { ## handle n==0 for !adaptive, for !adaptive thisn should be scalar
        tight
      }  else {
        tight0
      }
      if (use.fork) { ## !windows && getDTthreads()>1L, and then throttle using getDTthreadsC(thislen, TRUE)
        ths = min(DTths, length(ansi))
        ii = split(ansi, sort(rep_len(seq_len(ths), length(ansi)))) ## assign row indexes to threads
        jobs = vector("integer", ths)
        for (th in seq_len(ths)) {
          jobs[th] = parallel::mcparallel({
            #catf("%d\n", 4, "")
            # nocov start ## fork processes seem not to be tracked by codecov, at least when parallel not in suggests
            setDTthreads(1L)       ## disable nested parallelism
            lapply(ii[[th]],       ## loops over indexes for that thread
                   FUN = tightFUN, ## handles adaptive and by.column
                   dest = cpy(w),  ## allocate own window for each thread, if we would not copy here, then copy would be handled later on by fork's copy-on-write
                   src = thisx,    ## full input
                   n = thisn)      ## scalar or in adaptive case a vector
            # nocov end
          })[["pid"]]
        }
        if (length(ansi)) {
          fork.res = withCallingHandlers( ## collect results early to minimize time when user could raise SIGINT
            tryCatch(
              parallel::mccollect(jobs),
              error = function(e) stopf(err.collect, e[["message"]]),
              warning = function(w) {
                warningf(warn.collect, w[["message"]]) # nocov
              }
            ),
            interrupt = function(e) {
              # nocov start
              suspendInterrupts({
                lapply(jobs[.Call(Cis_direct_child, jobs)], function(pid) try(tools::pskill(pid), silent = TRUE))
                parallel::mccollect(jobs)
              })
              # nocov end
              # Let the interrupt continue without invoking restarts
            }
          )
          ## check for any errors in FUN, warnings are silently ignored
          fork.err = vapply_1b(fork.res, inherits, "try-error", use.names = FALSE)
          if (any(fork.err)) {
            stopf(
              "frollapply received an error(s) when evaluating FUN:\n%s",
              attr(fork.res[fork.err][[1L]], "condition", TRUE)[["message"]] ## print only first error for consistency to single threaded code
            )
          }
          thisans = unlist(fork.res, recursive = FALSE, use.names = FALSE)
          ## fix selfref after serializing data.table from forked process
          thisans = fixselfref(thisans)
        }
      } else { ## windows || getDTthreads()==1L
        h = list2env(list(warning=NULL, error=NULL)) ## pretty printing errors/warnings
        oldDTthreads = setDTthreads(1L) ## for consistency, anyway window size is unlikely to be big enough to benefit any parallelism
        withCallingHandlers(
          tryCatch(
            thisans <- lapply(ansi, FUN = tightFUN, dest = cpy(w), src = thisx, n = thisn),
            error = function(e) h$err = conditionMessage(e)
          ), warning = function(w) {
            #h$warn = c(h$warn, conditionMessage(w)) ## warnings are suppressed for consistency to parallel processing code
            invokeRestart("muffleWarning")
          }
        )
        setDTthreads(oldDTthreads)
        #if (!is.null(h$warn)) ## warnings are suppressed for consistency to parallel processing code
        #  warningf("frollapply received a warning(s) when evaluating FUN:\n%s", paste(unique(h$warn), collapse="\n"))
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
      if (length(ansi)) {
        if (leftadaptive)
          thisans = rev(thisans)
        ans[[thisansi]][ansi] = thisans
      }
      ## simplify
      if (is.function(simplify)) {
        ans[[thisansi]] = tryCatch(
          simplify(ans[[thisansi]]),
          error = function(e) stopf(err.simplify, e[["message"]]),
          warning = function(w) warningf(warn.simplify, w[["message"]])
        )
      } else if (isTRUE(simplify)) {
        ans[[thisansi]] = tryCatch(
          simplifylist(ans[[thisansi]], fill, ansmask),
          error = function(e) stopf(err.simplify, e[["message"]]),
          warning = function(w) warningf(warn.simplify, w[["message"]])
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
