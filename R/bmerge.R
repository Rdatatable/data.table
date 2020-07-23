
bmerge = function(i, x, icols, xcols, roll, rollends, nomatch, mult, ops, verbose)
{
  callersi = i
  i = shallow(i)
  # Just before the call to bmerge() in [.data.table there is a shallow() copy of i to prevent coercions here
  # by bmerge changing the type of the user's input object by reference. We now shallow copy i again. If we then
  # coerce a column in i only, we are just changing the temporary coercion used for the merge operation. If we
  # set callersi too then we are keeping that coerced i column in the merge result returned to user.
  # The type of the i column is always returned (i.e. just i set not callersi too), other than:
  #   i) to convert int-as-double to int, useful for ad hoc joins when the L postfix is often forgotten.
  #  ii) to coerce i.factor to character when joining to x.character
  # So those are the only two uses of callersi below.
  # Careful to only use plonk syntax (full column) on i and x from now on, otherwise user's i and x would
  # change. This is why shallow() is very importantly internal only, currently.

  # Using .SD in j to join could fail due to being locked and set() being used here, #1926
  .Call(C_unlock, i)
  x = shallow(x)
  .Call(C_unlock, x)
  if (.Call(C_islocked, callersi)) {
    .Call(C_unlock, callersi)
    on.exit(.Call(C_lock, callersi))
  }

  supported = c(ORDERING_TYPES, "factor", "integer64")

  getClass = function(x) {
    ans = typeof(x)
    if      (ans=="integer") { if (is.factor(x))             ans = "factor"    }
    else if (ans=="double")  { if (inherits(x, "integer64")) ans = "integer64" }
    # do not call isReallyReal(x) yet because i) if both types are double we don't need to coerce even if one or both sides
    # are int-as-double, and ii) to save calling it until we really need it
    ans
  }

  if (nrow(i)) for (a in seq_along(icols)) {
    # - check that join columns have compatible types
    # - do type coercions if necessary on just the shallow local copies for the purpose of join
    # - handle factor columns appropriately
    # Note that if i is keyed, if this coerces i's key gets dropped by set()
    ic = icols[a]
    xc = xcols[a]
    xclass = getClass(x[[xc]])
    iclass = getClass(i[[ic]])
    if (!xclass %chin% supported) stop("x.", names(x)[xc]," is type ", xclass, " which is not supported by data.table join")
    if (!iclass %chin% supported) stop("i.", names(i)[ic]," is type ", iclass, " which is not supported by data.table join")
    if (xclass=="factor" || iclass=="factor") {
      if (roll!=0.0 && a==length(icols))
        stop("Attempting roll join on factor column when joining x.",names(x)[xc]," to i.",names(i)[ic],". Only integer, double or character columns may be roll joined.")
      if (xclass=="factor" && iclass=="factor") {
        if (verbose) cat("Matching i.",names(i)[ic]," factor levels to x.",names(x)[xc]," factor levels.\n",sep="")
        set(i, j=ic, value=chmatch(levels(i[[ic]]), levels(x[[xc]]), nomatch=0L)[i[[ic]]])  # nomatch=0L otherwise a level that is missing would match to NA values
        next
      } else {
        if (xclass=="character") {
          if (verbose) cat("Coercing factor column i.",names(i)[ic]," to type character to match type of x.",names(x)[xc],".\n",sep="")
          set(i, j=ic, value=val<-as.character(i[[ic]]))
          set(callersi, j=ic, value=val)  # factor in i joining to character in x will return character and not keep x's factor; e.g. for antaresRead #3581
          next
        } else if (iclass=="character") {
          if (verbose) cat("Matching character column i.",names(i)[ic]," to factor levels in x.",names(x)[xc],".\n",sep="")
          newvalue = chmatch(i[[ic]], levels(x[[xc]]), nomatch=0L)
          if (anyNA(i[[ic]])) newvalue[is.na(i[[ic]])] = NA_integer_  # NA_character_ should match to NA in factor, #3809
          set(i, j=ic, value=newvalue)
          next
        }
      }
      stop("Incompatible join types: x.", names(x)[xc], " (",xclass,") and i.", names(i)[ic], " (",iclass,"). Factor columns must join to factor or character columns.")
    }
    if (xclass == iclass) {
      if (verbose) cat("i.",names(i)[ic]," has same type (",xclass,") as x.",names(x)[xc],". No coercion needed.\n", sep="")
      next
    }
    if (xclass=="character" || iclass=="character" ||
        xclass=="logical" || iclass=="logical" ||
        xclass=="factor" || iclass=="factor") {
      if (anyNA(i[[ic]]) && allNA(i[[ic]])) {
        if (verbose) cat("Coercing all-NA i.",names(i)[ic]," (",iclass,") to type ",xclass," to match type of x.",names(x)[xc],".\n",sep="")
        set(i, j=ic, value=match.fun(paste0("as.", xclass))(i[[ic]]))
        next
      }
      else if (anyNA(x[[xc]]) && allNA(x[[xc]])) {
        if (verbose) cat("Coercing all-NA x.",names(x)[xc]," (",xclass,") to type ",iclass," to match type of i.",names(i)[ic],".\n",sep="")
        set(x, j=xc, value=match.fun(paste0("as.", iclass))(x[[xc]]))
        next
      }
      stop("Incompatible join types: x.", names(x)[xc], " (",xclass,") and i.", names(i)[ic], " (",iclass,")")
    }
    if (xclass=="integer64" || iclass=="integer64") {
      nm = paste0(c("i.","x."), c(names(i)[ic], names(x)[xc]))
      if (xclass=="integer64") { w=i; wc=ic; wclass=iclass; } else { w=x; wc=xc; wclass=xclass; nm=rev(nm) }  # w is which to coerce
      if (wclass=="integer" || (wclass=="double" && !isReallyReal(w[[wc]]))) {
        if (verbose) cat("Coercing ",wclass," column ", nm[1L], if(wclass=="double")" (which contains no fractions)"," to type integer64 to match type of ", nm[2L],".\n",sep="")
        set(w, j=wc, value=bit64::as.integer64(w[[wc]]))
      } else stop("Incompatible join types: ", nm[2L], " is type integer64 but ", nm[1L], " is type double and contains fractions")
    } else {
      # just integer and double left
      if (iclass=="double") {
        if (!isReallyReal(i[[ic]])) {
          # common case of ad hoc user-typed integers missing L postfix joining to correct integer keys
          # we've always coerced to int and returned int, for convenience.
          if (verbose) cat("Coercing double column i.",names(i)[ic]," (which contains no fractions) to type integer to match type of x.",names(x)[xc],".\n",sep="")
          val = as.integer(i[[ic]])
          if (!is.null(attributes(i[[ic]]))) attributes(val) = attributes(i[[ic]])  # to retain Date for example; 3679
          set(i, j=ic, value=val)
          set(callersi, j=ic, value=val)       # change the shallow copy of i up in [.data.table to reflect in the result, too.
        } else {
          if (verbose) cat("Coercing integer column x.",names(x)[xc]," to type double to match type of i.",names(i)[ic]," which contains fractions.\n",sep="")
          set(x, j=xc, value=as.double(x[[xc]]))
        }
      } else {
        if (verbose) cat("Coercing integer column i.",names(i)[ic]," to type double for join to match type of x.",names(x)[xc],".\n",sep="")
        set(i, j=ic, value=as.double(i[[ic]]))
      }
    }
  }

  ## after all modifications of i, check if i has a proper key on all icols
  io = identical(icols, head(chmatch(key(i), names(i)), length(icols)))

  ## after all modifications of x, check if x has a proper key on all xcols.
  ## If not, calculate the order. Also for non-equi joins, the order must be calculated.
  non_equi = which.first(ops != 1L) # 1 is "==" operator
  if (is.na(non_equi)) {
    # equi join. use existing key (#1825) or existing secondary index (#1439)
    if (identical(xcols, head(chmatch(key(x), names(x)), length(xcols)))) {
      xo = integer(0L)
      if (verbose) cat("on= matches existing key, using key\n")
    } else {
      xo = NULL
      if (isTRUE(getOption("datatable.use.index"))) {
        xo = getindex(x, names(x)[xcols])
        if (verbose && !is.null(xo)) cat("on= matches existing index, using index\n")
      }
      if (is.null(xo)) {
        if (verbose) {last.started.at=proc.time(); flush.console()}
        xo = forderv(x, by = xcols)
        if (verbose) {cat("Calculated ad hoc index in",timetaken(last.started.at),"\n"); flush.console()}
        # TODO: use setindex() instead, so it's cached for future reuse
      }
    }
    ## these variables are only needed for non-equi joins. Set them to default.
    nqgrp = integer(0L)
    nqmaxgrp = 1L
  } else {
    # non-equi operators present.. investigate groups..
    nqgrp = integer(0L)
    nqmaxgrp = 1L
    if (verbose) cat("Non-equi join operators detected ... \n")
    if (roll != FALSE) stop("roll is not implemented for non-equi joins yet.")
    if (verbose) {last.started.at=proc.time();cat("  forder took ... ");flush.console()}
    # TODO: could check/reuse secondary indices, but we need 'starts' attribute as well!
    xo = forderv(x, xcols, retGrp=TRUE)
    if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
    xg = attr(xo, 'starts', exact=TRUE)
    resetcols = head(xcols, non_equi-1L)
    if (length(resetcols)) {
      # TODO: can we get around having to reorder twice here?
      # or at least reuse previous order?
      if (verbose) {last.started.at=proc.time();cat("  Generating group lengths ... ");flush.console()}
      resetlen = attr(forderv(x, resetcols, retGrp=TRUE), 'starts', exact=TRUE)
      resetlen = .Call(Cuniqlengths, resetlen, nrow(x))
      if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console()}
    } else resetlen = integer(0L)
    if (verbose) {last.started.at=proc.time();cat("  Generating non-equi group ids ... ");flush.console()}
    nqgrp = .Call(Cnestedid, x, xcols[non_equi:length(xcols)], xo, xg, resetlen, mult)
    if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console()}
    if (length(nqgrp)) nqmaxgrp = max(nqgrp) # fix for #1986, when 'x' is 0-row table max(.) returns -Inf.
    if (nqmaxgrp > 1L) { # got some non-equi join work to do
      if ("_nqgrp_" %in% names(x)) stop("Column name '_nqgrp_' is reserved for non-equi joins.")
      if (verbose) {last.started.at=proc.time();cat("  Recomputing forder with non-equi ids ... ");flush.console()}
      set(nqx<-shallow(x), j="_nqgrp_", value=nqgrp)
      xo = forderv(nqx, c(ncol(nqx), xcols))
      if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console()}
    } else nqgrp = integer(0L)
    if (verbose) cat("  Found", nqmaxgrp, "non-equi group(s) ...\n")
  }

  if (verbose) {last.started.at=proc.time();cat("Starting bmerge ...\n");flush.console()}
  ans = .Call(Cbmerge, i, x, as.integer(icols), as.integer(xcols), io, xo, roll, rollends, nomatch, mult, ops, nqgrp, nqmaxgrp)
  if (verbose) {cat("bmerge done in",timetaken(last.started.at),"\n"); flush.console()}
  # TO DO: xo could be moved inside Cbmerge

  ans$xo = xo  # for further use by [.data.table
  return(ans)
}

