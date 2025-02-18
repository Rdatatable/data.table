

mergeType = function(x) {
  ans = typeof(x)
  if      (ans=="integer") { if (is.factor(x))             ans = "factor"    }
  else if (ans=="double")  { if (inherits(x, "integer64")) ans = "integer64" }
  # do not call fitsInInt*(x) yet because i) if both types are double we don't need to coerce even if one or both sides
  # are int-as-double, and ii) to save calling it until we really need it
  ans
}

cast_with_attrs = function(x, cast_fun) {
  ans = cast_fun(x)
  # do not copy attributes when coercing factor (to character)
  if (!is.factor(x) && !is.null(attributes(x))) attributes(ans) = attributes(x)
  ans
}

coerce_col = function(dt, col, from_type, to_type, from_name, to_name, from_detail="", to_detail="", verbose) {
  if (verbose) catf(
    "Coercing %s column %s%s to type %s to match type of %s%s.\n",
    from_type, from_name, from_detail, to_type, to_name, to_detail
  )
  cast_fun = switch(to_type, integer64 = bit64::as.integer64, match.fun(paste0("as.", to_type)))
  set(dt, j=col, value=cast_with_attrs(dt[[col]], cast_fun))
}

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

  if (nrow(i)) for (a in seq_along(icols)) {
    # - check that join columns have compatible types
    # - do type coercions if necessary on just the shallow local copies for the purpose of join
    # - handle factor columns appropriately
    # Note that if i is keyed, if this coerces i's key gets dropped by set()
    icol = icols[a]
    xcol = xcols[a]
    x_merge_type = mergeType(x[[xcol]])
    i_merge_type = mergeType(i[[icol]])
    xname = paste0("x.", names(x)[xcol])
    iname = paste0("i.", names(i)[icol])
    if (!x_merge_type %chin% supported) stopf("%s is type %s which is not supported by data.table join", xname, x_merge_type)
    if (!i_merge_type %chin% supported) stopf("%s is type %s which is not supported by data.table join", iname, i_merge_type)
    if (x_merge_type=="factor" || i_merge_type=="factor") {
      if (roll!=0.0 && a==length(icols))
        stopf("Attempting roll join on factor column when joining %s to %s. Only integer, double or character columns may be roll joined.", xname, iname)
      if (x_merge_type=="factor" && i_merge_type=="factor") {
        if (verbose) catf("Matching %s factor levels to %s factor levels.\n", iname, xname)
        set(i, j=icol, value=chmatch(levels(i[[icol]]), levels(x[[xcol]]), nomatch=0L)[i[[icol]]])  # nomatch=0L otherwise a level that is missing would match to NA values
        next
      } else {
        if (x_merge_type=="character") {
          coerce_col(i, icol, "factor", "character", iname, xname, verbose=verbose)
          set(callersi, j=icol, value=i[[icol]])  # factor in i joining to character in x will return character and not keep x's factor; e.g. for antaresRead #3581
          next
        } else if (i_merge_type=="character") {
          if (verbose) catf("Matching character column %s to factor levels in %s.\n", iname, xname)
          newvalue = chmatch(i[[icol]], levels(x[[xcol]]), nomatch=0L)
          if (anyNA(i[[icol]])) newvalue[is.na(i[[icol]])] = NA_integer_  # NA_character_ should match to NA in factor, #3809
          set(i, j=icol, value=newvalue)
          next
        }
      }
      stopf("Incompatible join types: %s (%s) and %s (%s). Factor columns must join to factor or character columns.", xname, x_merge_type, iname, i_merge_type)
    }
    # we check factors first to cater for the case when trying to do rolling joins on factors
    if (x_merge_type == i_merge_type) {
      if (verbose) catf("%s has same type (%s) as %s. No coercion needed.\n", iname, x_merge_type, xname)
      next
    }
    cfl = c("character", "logical", "factor")
    if (x_merge_type %chin% cfl || i_merge_type %chin% cfl) {
      if (anyNA(i[[icol]]) && allNA(i[[icol]])) {
        coerce_col(i, icol, i_merge_type, x_merge_type, iname, xname, from_detail=gettext(" (all-NA)"), verbose=verbose)
        next
      }
      if (anyNA(x[[xcol]]) && allNA(x[[xcol]])) {
        coerce_col(x, xcol, x_merge_type, i_merge_type, xname, iname, from_detail=gettext(" (all-NA)"), verbose=verbose)
        next
      }
      stopf("Incompatible join types: %s (%s) and %s (%s)", xname, x_merge_type, iname, i_merge_type)
    }
    if (x_merge_type=="integer64" || i_merge_type=="integer64") {
      nm = c(iname, xname)
      if (x_merge_type=="integer64") { w=i; wc=icol; wclass=i_merge_type; } else { w=x; wc=xcol; wclass=x_merge_type; nm=rev(nm) }  # w is which to coerce
      if (wclass=="integer" || (wclass=="double" && fitsInInt64(w[[wc]]))) {
        from_detail = if (wclass == "double") gettext(" (which has integer64 representation, e.g. no fractions)") else ""
        coerce_col(w, wc, wclass, "integer64", nm[1L], nm[2L], from_detail, verbose=verbose)
      } else stopf("Incompatible join types: %s is type integer64 but %s is type double and cannot be coerced to integer64 (e.g. has fractions)", nm[2L], nm[1L])
    } else {
      # just integer and double left
      ic_idx = which(icol == icols) # check if on is joined on multiple conditions, #6602
      if (i_merge_type=="double") {
        coerce_x = FALSE
        if (fitsInInt32(i[[icol]])) {
          coerce_x = TRUE
          # common case of ad hoc user-typed integers missing L postfix joining to correct integer keys
          # we've always coerced to int and returned int, for convenience.
          if (length(ic_idx)>1L) {
            xc_idx = xcols[ic_idx]
            for (xb in xc_idx[which(vapply_1c(.shallow(x, xc_idx), mergeType) == "double")]) {
              if (!fitsInInt32(x[[xb]])) {
                coerce_x = FALSE
                break
              }
            }
          }
          if (coerce_x) {
            from_detail = gettext(" (which contains no fractions)")
            coerce_col(i, icol, "double", "integer", iname, xname, from_detail, verbose=verbose)
            set(callersi, j=icol, value=i[[icol]])       # change the shallow copy of i up in [.data.table to reflect in the result, too.
            if (length(ic_idx)>1L) {
              xc_idx = xcols[ic_idx]
              for (xb in xc_idx[which(vapply_1c(.shallow(x, xc_idx), mergeType) == "double")]) {
                coerce_col(x, xb, "double", "integer", paste0("x.", names(x)[xb]), xname, from_detail, verbose=verbose)
              }
            }
          }
        }
        if (!coerce_x) {
          coerce_col(x, xcol, "integer", "double", xname, iname, to_detail=gettext(" (which contains fractions)"), verbose=verbose)
        }
      } else {
        coerce_col(i, icol, "integer", "double", iname, xname, from_detail=gettext(" (for join)"), verbose=verbose)
        if (length(ic_idx)>1L) {
          xc_idx = xcols[ic_idx]
          for (xb in xc_idx[which(vapply_1c(.shallow(x, xc_idx), mergeType) == "integer")]) {
            coerce_col(x, xb, "integer", "double", paste0("x.", names(x)[xb]), xname, verbose=verbose)
          }
        }
      }
    }
  }

  ## after all modifications of x, check if x has a proper key on all xcols.
  ## If not, calculate the order. Also for non-equi joins, the order must be calculated.
  non_equi = which.first(ops != 1L) # 1 is "==" operator
  if (is.na(non_equi)) {
    # equi join. use existing key (#1825) or existing secondary index (#1439)
    if (identical(xcols, head(chmatch(key(x), names(x)), length(xcols)))) {
      xo = integer(0L)
      if (verbose) catf("on= matches existing key, using key\n")
    } else {
      xo = NULL
      if (isTRUE(getOption("datatable.use.index"))) {
        xo = getindex(x, names(x)[xcols])
        if (verbose && !is.null(xo)) catf("on= matches existing index, using index\n")
      }
      if (is.null(xo)) {
        if (verbose) {last.started.at=proc.time(); flush.console()}
        xo = forderv(x, by = xcols)
        if (verbose) {catf("Calculated ad hoc index in %s\n", timetaken(last.started.at)); flush.console()}
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
    if (verbose) catf("Non-equi join operators detected ... \n")
    if (roll != FALSE) stopf("roll is not implemented for non-equi joins yet.")
    if (verbose) {last.started.at=proc.time();catf("  forder took ... ");flush.console()}
    # TODO: could check/reuse secondary indices, but we need 'starts' attribute as well!
    xo = forderv(x, xcols, retGrp=TRUE)
    if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()} # notranslate
    xg = attr(xo, 'starts', exact=TRUE)
    resetcols = head(xcols, non_equi-1L)
    if (length(resetcols)) {
      # TODO: can we get around having to reorder twice here?
      # or at least reuse previous order?
      if (verbose) {last.started.at=proc.time();catf("  Generating group lengths ... ");flush.console()}
      resetlen = attr(forderv(x, resetcols, retGrp=TRUE), 'starts', exact=TRUE)
      resetlen = .Call(Cuniqlengths, resetlen, nrow(x))
      if (verbose) {catf("done in %s\n",timetaken(last.started.at)); flush.console()}
    } else resetlen = integer(0L)
    if (verbose) {last.started.at=proc.time();catf("  Generating non-equi group ids ... ");flush.console()}
    nqgrp = .Call(Cnestedid, x, xcols[non_equi:length(xcols)], xo, xg, resetlen, mult)
    if (verbose) {catf("done in %s\n",timetaken(last.started.at)); flush.console()}
    if (length(nqgrp)) nqmaxgrp = max(nqgrp) # fix for #1986, when 'x' is 0-row table max(.) returns -Inf.
    if (nqmaxgrp > 1L) { # got some non-equi join work to do
      if ("_nqgrp_" %in% names(x)) stopf("Column name '_nqgrp_' is reserved for non-equi joins.")
      if (verbose) {last.started.at=proc.time();catf("  Recomputing forder with non-equi ids ... ");flush.console()}
      set(nqx<-shallow(x), j="_nqgrp_", value=nqgrp)
      xo = forderv(nqx, c(ncol(nqx), xcols))
      if (verbose) {catf("done in %s\n",timetaken(last.started.at)); flush.console()}
    } else nqgrp = integer(0L)
    if (verbose)
      catf(ngettext(nqmaxgrp, "  Found %d non-equi group ...\n", "  Found %d non-equi groups ...\n"), nqmaxgrp, domain=NA)
  }

  if (verbose) {last.started.at=proc.time();catf("Starting bmerge ...\n");flush.console()}
  ans = .Call(Cbmerge, i, x, as.integer(icols), as.integer(xcols), xo, roll, rollends, nomatch, mult, ops, nqgrp, nqmaxgrp)
  if (verbose) {catf("bmerge done in %s\n",timetaken(last.started.at)); flush.console()}
  # TO DO: xo could be moved inside Cbmerge

  ans$xo = xo  # for further use by [.data.table
  ans
}
