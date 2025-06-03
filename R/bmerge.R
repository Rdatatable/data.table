mergeType = function(x) {
  ans = typeof(x)
  if      (ans=="integer") { if (is.factor(x))             ans = "factor"    }
  else if (ans=="double")  { if (inherits(x, "integer64")) ans = "integer64" }
  ans
}

cast_with_attrs = function(x, cast_fun) {
  ans = cast_fun(x)
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
  .Call(C_unlock, i)
  x = shallow(x)
  .Call(C_unlock, x)
  if (.Call(C_islocked, callersi)) {
    .Call(C_unlock, callersi)
    on.exit(.Call(C_lock, callersi))
  }

  supported = c(ORDERING_TYPES, "factor", "integer64")

  if (nrow(i)) for (a in seq_along(icols)) {
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
        set(i, j=icol, value=chmatch(levels(i[[icol]]), levels(x[[xcol]]), nomatch=0L)[i[[icol]]])
        next
      } else {
        if (x_merge_type=="character") {
          coerce_col(i, icol, "factor", "character", iname, xname, verbose=verbose)
          set(callersi, j=icol, value=i[[icol]])
          next
        } else if (i_merge_type=="character") {
          if (verbose) catf("Matching character column %s to factor levels in %s.\n", iname, xname)
          newvalue = chmatch(i[[icol]], levels(x[[xcol]]), nomatch=0L)
          if (anyNA(i[[icol]])) newvalue[is.na(i[[icol]])] = NA_integer_
          set(i, j=icol, value=newvalue)
          next
        }
      }
      # Incompatible factor join: Factor vs (Not Factor and Not Character)
      # The 'message' attribute must match the *old* error for direct calls to bmerge (e.g., DT[otherDT])
      condition_message <- sprintf(
        "Incompatible join types: %s (%s) and %s (%s). Factor columns must join to factor or character columns.", # Exact match for tests like 2044.24
        xname, x_merge_type,
        iname, i_merge_type
      )

      condition <- structure(
        list(
          message = condition_message,
          c_bmerge_x_arg_bare_col_name = names(x)[xcol],
          c_bmerge_x_arg_type          = x_merge_type,
          c_bmerge_i_arg_bare_col_name = names(i)[icol],
          c_bmerge_i_arg_type          = i_merge_type
        ),
        class = c("bmerge_incompatible_type_error", "data.table_error", "error", "condition")
      )
      stop(condition)
    }

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
      # This 'stopf' might have been the one originally hit by the failing tests.
      # Our custom error above now preempts this for incompatible factor joins.
      stopf("Incompatible join types: %s (%s) and %s (%s)", xname, x_merge_type, iname, i_merge_type)
    }
    if (x_merge_type=="integer64" || i_merge_type=="integer64") {
      nm = c(iname, xname)
      if (x_merge_type=="integer64") { w=i; wc=icol; wclass=i_merge_type; } else { w=x; wc=xcol; wclass=x_merge_type; nm=rev(nm) }
      if (wclass=="integer" || (wclass=="double" && fitsInInt64(w[[wc]]))) {
        from_detail = if (wclass == "double") gettext(" (which has integer64 representation, e.g. no fractions)") else ""
        coerce_col(w, wc, wclass, "integer64", nm[1L], nm[2L], from_detail, verbose=verbose)
      } else stopf("Incompatible join types: %s is type integer64 but %s is type double and cannot be coerced to integer64 (e.g. has fractions)", nm[2L], nm[1L])
    } else {
      ic_idx = which(icol == icols)
      if (i_merge_type=="double") {
        coerce_x = FALSE
        if (fitsInInt32(i[[icol]])) {
          coerce_x = TRUE
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
            set(callersi, j=icol, value=i[[icol]])
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

  non_equi = which.first(ops != 1L)
  if (is.na(non_equi)) {
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
      }
    }
    nqgrp = integer(0L)
    nqmaxgrp = 1L
  } else {
    nqgrp = integer(0L)
    nqmaxgrp = 1L
    if (verbose) catf("Non-equi join operators detected ... \n")
    if (roll != FALSE) stopf("roll is not implemented for non-equi joins yet.")
    if (verbose) {last.started.at=proc.time();catf("  forder took ... ");flush.console()}
    xo = forderv(x, xcols, retGrp=TRUE)
    if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
    xg = attr(xo, 'starts', exact=TRUE)
    resetcols = head(xcols, non_equi-1L)
    if (length(resetcols)) {
      if (verbose) {last.started.at=proc.time();catf("  Generating group lengths ... ");flush.console()}
      resetlen = attr(forderv(x, resetcols, retGrp=TRUE), 'starts', exact=TRUE)
      resetlen = .Call(Cuniqlengths, resetlen, nrow(x))
      if (verbose) {catf("done in %s\n",timetaken(last.started.at)); flush.console()}
    } else resetlen = integer(0L)
    if (verbose) {last.started.at=proc.time();catf("  Generating non-equi group ids ... ");flush.console()}
    nqgrp = .Call(Cnestedid, x, xcols[non_equi:length(xcols)], xo, xg, resetlen, mult)
    if (verbose) {catf("done in %s\n",timetaken(last.started.at)); flush.console()}
    if (length(nqgrp)) nqmaxgrp = max(nqgrp)
    if (nqmaxgrp > 1L) {
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

  ans$xo = xo
  ans
}