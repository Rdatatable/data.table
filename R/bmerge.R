
bmerge <- function(i, x, icols, xcols, roll, rollends, nomatch, mult, ops, verbose)
{
  callersi = i
  i = shallow(i)  # important: i is already passed in as a shallow copy from [.data.table because [.data.table
                       # builds the answer from (potentially) coerced i, other than factor columns for which the
                       # original levels in i are retained in the final answer.
  x = shallow(x)       # if any coercions are done for x, we must be careful not to change the caller's view of x.
  # careful to only plonk syntax (full column) on i/x from now on (otherwise user's i and x would change);
  #   this is why shallow() is very importantly internal only, currently.

  #  DELETE ... resetifactor = NULL     # Keep track of any factor to factor/character join cols (only time we keep orig)

  getClass <- function(x) {
    ans = typeof(x)
    if      (ans=="integer") { if (is.factor(x))             ans = "factor"    }
    else if (ans=="double")  { if (inherits(x, "integer64")) ans = "integer64" }
    # do not call isReallyReal(x) yet because if both types are double, we don't need to coerce even if one or both sides are int-as-double
    ans
  }

  ## function or coercing a vector to a new type. Depending on the source and target type,
  ## different coercion strategies are used.
  coerceClass <- function(x, to){
    sourceType <- getClass(x)
    ## reallyDouble and intAsDouble are the same here:
    if(sourceType %chin% c("reallyDouble", "intAsDouble")) sourceType <- "double"
    if(sourceType == to) return(x) ## nothing to be done
    if(!to %chin% c("logical", "integer", "double", "character", "factor", "integer64"))
      stop("Invalid 'to' argument: ", to)
    if(sourceType == "other")
      stop("type coercion not supported for this type: ", paste0(class(x), collapse = ","))
    if(sourceType %chin% c("logical", "integer", "double") &
       to         %chin% c("logical", "integer", "double")){
      ## for these classes, we can do mode coercion to retain other class attributes, e.g. IDate
      ## identical types have been caught above
      out       <- x
      mode(out) <- to
    }
    else {
      ## we need as.'to'() conversion
      converter <- match.fun(paste0("as.", to))
      out       <- converter(x)
    }
    return(out)
  }

  ## Establish a lookup table with coercion strategies for each pair of types.
  ## Coercion strategies can be one of the following:
  ##------------------|-----------------------------------------------------
  ## y (yes):         | no coercion necessary, types match
  ##------------------|-----------------------------------------------------
  ## e (error):       | throw an error because of incompatible types
  ##------------------|-----------------------------------------------------
  ## ci (coercion i): | coerce the column in i to the type of the column in x
  ##------------------|-----------------------------------------------------
  ## ciw:             | same as above, but with warning.
  ##------------------|-----------------------------------------------------
  ## cx (coercion x): | coerce the column in x to the type of the column in i
  ##------------------|-----------------------------------------------------
  ## cxw:             | same as above, but with warning.
  ##------------------|-----------------------------------------------------
  ## rows mark the column type in i, columns the column type in x
  ## possible types are: logical, integer, intAsDouble, reallyDouble, character, factor, integer64

  supported = c("logical", "integer", "double", "character", "factor", "integer64")
  for (a in seq_along(icols)) {
    # This loop does the following:
    # - check that join columns have compatible types
    # - do type coercions if necessary
    # - special support for joining factor columns
    # Note that if i is keyed, if this coerces, i's key gets dropped and the key may not be retained
    ic = icols[a]
    xc = xcols[a]
    xclass = getClass(x[[xc]])
    iclass = getClass(i[[ic]])
    if (!xclass %chin% supported) stop("x.", names(x)[xc]," is type ", xclass, " which is not supported by data.table join")
    if (!iclass %chin% supported) stop("i.", names(i)[ic]," is type ", iclass, " which is not supported by data.table join")
    if (xclass=="factor" || iclass=="factor") {
      if (roll!=0.0 && a==length(icols))
        stop("Attempting roll join on factor column when joining x.",names(x)[xc]," to i.",names(i)[ic],". Only integer, double or character colums may be roll joined.")
      if (xclass=="factor" && iclass=="factor") {
        # Levels are matched to levels here, then bmerge.c continues as-if integer
        # Retain original levels of i's factor column (important when NAs, see tests 687 and 688).  Related fixes: #499 and #945
        # delete ... resetifactor = c(resetifactor,lc)
        #            val = origi[[lc]] # note: using 'origi' here because set(..., value = .) always copies '.', we need a way to avoid it in internal cases.
        #            lx = levels(x[[xc]])
        #            li = levels(val)
        if (verbose) cat("Matching i.",names(i)[ic]," factor levels to x.",names(x)[xc]," factor levels.\n",sep="")
        set(i, j=ic, value=chmatch(levels(i[[ic]]), levels(x[[xc]]), nomatch=0L)[i[[ic]]])
        next
        #            levels(newfactor) = lx
        #            class(newfactor) = "factor"
        #            set(i, j=lc, value=newfactor)
      } else {
        if (xclass=="character") {
          if (verbose) cat("Coercing factor column i.",names(i)[ic]," to character to match type of x.",names(x)[xc],".\n",sep="")
          val = as.character(i[[ic]])
          set(i, j=ic, value=val)
          set(callersi, j=ic, value=val)
          next
        } else if (iclass=="character") {
          if (verbose) cat("Matching character column i.",names(i)[ic]," to factor levels in x.",names(x)[xc],".\n",sep="")
          set(i, j=ic, value=chmatch(i[[ic]], levels(x[[xc]]), nomatch=0L))
          next
        }
      }
    }
    if (xclass == iclass) {
      if (verbose) cat("i.",names(i)[ic],"has same type (",xclass,") as x.",names(x)[xc],". No coercion needed.")
      next
    }
    if (xclass=="character" || iclass=="character" ||
        xclass=="logical" || iclass=="logical" ||
        xclass=="factor" || iclass=="factor") {
      stop("Incompatible join types: x.", names(x)[xc], " (",xclass,") and i.", names(i)[ic], " (",iclass,")")
    }
    if (xclass=="integer64" || iclass=="integer64") {
      if (xclass=="integer64") { w=i; wc=ic; wclass=iclass; } else { w=x; wc=xc; wclass=xclass; }  # w is which to coerce
      if (wclass=="integer" || (wclass=="double" && !isReallyReal(w[[wc]]))) {
        val = as.integer64(w[[wc]])
        set(w, j=wc, value=val)
        if (wclass==iclass) set(callersi, j=wc, value=val)
      } else stop("cannot coerce reallyreal to integer64")
    } else {
      # just integer and double left
      if (iclass=="double") {
        if (!isReallyReal(i[[ic]])) {
          # common case of ad hoc user-typed integers missing L postfix joining to correct integer keys
          # we've always coerced to int and returned int, for convenience.
          if (verbose) cat("Coercing double column i.",names(i)[ic]," which contains integers to type integer to match type of x.",names(x)[xc],".\n",sep="")
          val = as.integer(i[[ic]])
          set(callersi, j=ic, value=val)       # change the shallow copy of i up in [.data.table to reflect in the result
          set(i, j=ic, value=val)  # change local shallow copy too to apply in the Cmerge call below
        }
        else stop("Cannot coerce double to int")
      } else {
        val = as.double(i[[ic]])
        set(i, j=ic, value=val)
        set(callersi, j=ic, value=val)
      }
    }
  }
  # browser()

#      stop(sprintf("Incompatible types: %s (%s) and %s (%s)",
#                   paste0("x.", xcnam), myXtype, paste0("i.", icnam), myItype))

#      if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.",
#                                myItype, paste0("i.'", icnam, "'"), myXtype, paste0("x.'", xcnam, "'"))); flush.console()}

#        warning(sprintf("Coercing %s column %s to %s to match type of %s.",
#                        myItype, paste0("i.'", icnam, "'"), myXtype, paste0("x.'", xcnam, "'")))


  ## after all modifications of i, check if i has a proper key on all icols
  io <- identical(icols, head(chmatch(key(i), names(i)), length(icols)))

  ## after all modifications of x, check if x has a proper key on all xcols.
  ## If not, calculate the order. Also for non-equi joins, the order must be calculated.
  non_equi = which.first(ops != 1L) # 1 is "==" operator
  if (is.na(non_equi)) {
    # equi join. use existing key (#1825) or existing secondary index (#1439)
    if (identical(xcols, head(chmatch(key(x), names(x)), length(xcols)))) {
      xo = integer(0L)
      if (verbose) cat("on= matches existing key, using key\n")
    } else {
      xo <- NULL
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
    ## these vaiables are only needed for non-equi joins. Set them to default.
    nqgrp <- integer(0)
    nqmaxgrp <- 1L
  } else {
    # non-equi operators present.. investigate groups..
    nqgrp <- integer(0)
    nqmaxgrp <- 1L
    if (verbose) cat("Non-equi join operators detected ... \n")
    if (roll != FALSE) stop("roll is not implemented for non-equi joins yet.")
    if (verbose) {last.started.at=proc.time();cat("  forder took ... ");flush.console()}
    # TODO: could check/reuse secondary indices, but we need 'starts' attribute as well!
    xo = forderv(x, xcols, retGrp=TRUE)
    if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
    xg = attr(xo, 'starts')
    resetcols = head(xcols, non_equi-1L)
    if (length(resetcols)) {
      # TODO: can we get around having to reorder twice here?
      # or at least reuse previous order?
      if (verbose) {last.started.at=proc.time();cat("  Generating group lengths ... ");flush.console()}
      resetlen = attr(forderv(x, resetcols, retGrp=TRUE), 'starts')
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

  if (verbose) {last.started.at=proc.time();cat("Starting bmerge ...");flush.console()}
  ans = .Call(Cbmerge, i, x, as.integer(icols), as.integer(xcols), io, xo, roll, rollends, nomatch, mult, ops, nqgrp, nqmaxgrp)
  if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console()}
  # TO DO: xo could be moved inside Cbmerge

  # delete ... in the caller's shallow copy,  see comment at the top of this function for usage
  # We want to leave the coercions to i in place otherwise, since the caller depends on that to build the result
  #if (length(resetifactor)) {
  #  for (ii in resetifactor)
  #    set(i,j=ii,value=origi[[ii]])
  #  if (haskey(origi))
  #    setattr(i, 'sorted', key(origi))
  #}

  ## add xo for further use
  ans$xo <- xo
  return(ans)
}

