# setdiff for data.tables, internal at the moment #547, used in not-join
setdiff_ = function(x, y, by.x=seq_along(x), by.y=seq_along(y), use.names=FALSE) {
  if (!is.data.table(x) || !is.data.table(y)) stop("x and y must both be data.tables")
  # !ncol redundant since all 0-column data.tables have 0 rows
  if (!nrow(x)) return(x)
  by.x = colnamesInt(x, by.x, check_dups=TRUE)
  if (!nrow(y)) return(unique(x, by=by.x))
  by.y = colnamesInt(y, by.y, check_dups=TRUE)
  if (length(by.x) != length(by.y)) stop("length(by.x) != length(by.y)")
  # factor in x should've factor/character in y, and viceversa
  for (a in seq_along(by.x)) {
    lc = by.y[a]
    rc = by.x[a]
    icnam = names(y)[lc]
    xcnam = names(x)[rc]
    if ( is.character(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
      stop("When x's column ('",xcnam,"') is character, the corresponding column in y ('",icnam,"') should be factor or character, but found incompatible type '",typeof(y[[lc]]),"'.")
    } else if ( is.factor(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
      stop("When x's column ('",xcnam,"') is factor, the corresponding column in y ('",icnam,"') should be character or factor, but found incompatible type '",typeof(y[[lc]]),"'.")
    } else if ( (is.integer(x[[rc]]) || is.double(x[[rc]])) && (is.logical(y[[lc]]) || is.character(y[[lc]])) ) {
      stop("When x's column ('",xcnam,"') is integer or numeric, the corresponding column in y ('",icnam,"') can not be character or logical types, but found incompatible type '",typeof(y[[lc]]),"'.")
    }
  }
  ux = unique(shallow(x, by.x))
  uy = unique(shallow(y, by.y))
  ix = duplicated(rbind(uy, ux, use.names=use.names, fill=FALSE))[-seq_len(nrow(uy))]
  .Call(CsubsetDT, ux, which_(ix, FALSE), seq_along(ux)) # more memory efficient version of which(!ix)
}

# set operators ----

funique = function(x) {
  stopifnot(is.data.table(x))
  dup = duplicated(x)
  if (any(dup)) .Call(CsubsetDT, x, which_(dup, FALSE), seq_along(x)) else x
}

.set_ops_arg_check = function(x, y, all, .seqn = FALSE, block_list = TRUE) {
  if (!is.logical(all) || length(all) != 1L) stop("argument 'all' should be logical of length one")
  if (!is.data.table(x) || !is.data.table(y)) stop("x and y must both be data.tables")
  if (!identical(sort(names(x)), sort(names(y)))) stop("x and y must have the same column names")
  if (!identical(names(x), names(y))) stop("x and y must have the same column order")
  bad_types = c("raw", "complex", if (block_list) "list")
  found = bad_types %chin% c(vapply_1c(x, typeof), vapply_1c(y, typeof))
  if (any(found)) stop("unsupported column type", if (sum(found) > 1L) "s" else "",
                       " found in x or y: ", brackify(bad_types[found]))
  super = function(x) {
    # allow character->factor and integer->numeric because from v1.12.4 i's type is retained by joins, #3820
    ans = class(x)[1L]
    switch(ans, factor="character", integer="numeric", ans)
  }
  if (!identical(sx<-sapply(x, super), sy<-sapply(y, super))) {
    w = which.first(sx!=sy)
    stop("Item ",w," of x is '",class(x[[w]])[1L],"' but the corresponding item of y is '", class(y[[w]])[1L], "'.")
  }
  if (.seqn && ".seqn" %chin% names(x)) stop("None of the datasets should contain a column named '.seqn'")
}

fintersect = function(x, y, all=FALSE) {
  .set_ops_arg_check(x, y, all, .seqn = TRUE)
  if (!nrow(x) || !nrow(y)) return(x[0L])
  if (all) {
    x = shallow(x)[, ".seqn" := rowidv(x)]
    y = shallow(y)[, ".seqn" := rowidv(y)]
    jn.on = c(".seqn",setdiff(names(x),".seqn"))
    x[y, .SD, .SDcols=setdiff(names(x),".seqn"), nomatch=NULL, on=jn.on]
  } else {
    z = funique(y)  # fixes #3034. When .. prefix in i= is implemented (TODO), this can be x[funique(..y), on=, multi=]
    x[z, nomatch=NULL, on=names(x), mult="first"]
  }
}

fsetdiff = function(x, y, all=FALSE) {
  .set_ops_arg_check(x, y, all, .seqn = TRUE)
  if (!nrow(x)) return(x)
  if (!nrow(y)) return(if (!all) funique(x) else x)
  if (all) {
    x = shallow(x)[, ".seqn" := rowidv(x)]
    y = shallow(y)[, ".seqn" := rowidv(y)]
    jn.on = c(".seqn",setdiff(names(x),".seqn"))
    x[!y, .SD, .SDcols=setdiff(names(x),".seqn"), on=jn.on]
  } else {
    funique(x[!y, on=names(x)])
  }
}

funion = function(x, y, all=FALSE) {
  .set_ops_arg_check(x, y, all, block_list = !all)
  ans = rbindlist(list(x, y))
  if (!all) ans = funique(ans)
  ans
}

fsetequal = function(x, y, all=TRUE) {
  .set_ops_arg_check(x, y, all)
  if (!all) {
    x = funique(x)
    y = funique(y)
  }
  isTRUE(all.equal.data.table(x, y, check.attributes = FALSE, ignore.row.order = TRUE))
}

# all.equal ----

all.equal.data.table = function(target, current, trim.levels=TRUE, check.attributes=TRUE, ignore.col.order=FALSE, ignore.row.order=FALSE, tolerance=sqrt(.Machine$double.eps), ...) {
  stopifnot(is.logical(trim.levels), is.logical(check.attributes), is.logical(ignore.col.order), is.logical(ignore.row.order), is.numeric(tolerance), is.data.table(target))

  if (!is.data.table(current)) {
    if (check.attributes) return(paste0('target is data.table, current is ', data.class(current)))
    try({current = as.data.table(current)}, silent = TRUE)
    if (!is.data.table(current)) return('target is data.table but current is not and failed to be coerced to it')
  }

  msg = character(0L)
  # init checks that detect high level all.equal
  if (nrow(current) != nrow(target)) msg = "Different number of rows"
  if (ncol(current) != ncol(target)) msg = c(msg, "Different number of columns")
  diff.colnames = !identical(sort(names(target)), sort(names(current)))
  diff.colorder = !identical(names(target), names(current))
  if (check.attributes && diff.colnames) msg = c(msg, "Different column names")
  if (!diff.colnames && !ignore.col.order && diff.colorder) msg = c(msg, "Different column order")

  if (length(msg)) return(msg) # skip check.attributes and further heavy processing

  # ignore.col.order
  if (ignore.col.order && diff.colorder) current = setcolorder(shallow(current), names(target))

  # Always check modes equal, like base::all.equal
  targetModes = vapply_1c(target, mode)
  currentModes = vapply_1c(current,  mode)
  if (any( d<-(targetModes!=currentModes) )) {
    w = head(which(d),3L)
    return(paste0("Datasets have different column modes. First 3: ",paste(
     paste0(names(targetModes)[w],"(",paste(targetModes[w],currentModes[w],sep="!="),")")
            ,collapse=" ")))
  }

  if (check.attributes) {
    squashClass = function(x) if (is.object(x)) paste(class(x),collapse=";") else mode(x)
    # else mode() is so that integer==numeric, like base all.equal does.
    targetTypes = vapply_1c(target, squashClass)
    currentTypes = vapply_1c(current, squashClass)
    if (length(targetTypes) != length(currentTypes))
      stop("Internal error: ncol(current)==ncol(target) was checked above") # nocov
    if (any( d<-(targetTypes != currentTypes))) {
      w = head(which(d),3L)
      return(paste0("Datasets have different column classes. First 3: ",paste(
     paste0(names(targetTypes)[w],"(",paste(targetTypes[w],currentTypes[w],sep="!="),")")
            ,collapse=" ")))
    }

    # check key
    k1 = key(target)
    k2 = key(current)
    if (!identical(k1, k2)) {
      return(sprintf("Datasets has different keys. 'target'%s. 'current'%s.",
               if(length(k1)) paste0(": ", paste(k1, collapse=", ")) else " has no key",
               if(length(k2)) paste0(": ", paste(k2, collapse=", ")) else " has no key"))
    }
    # check index
    i1 = indices(target)
    i2 = indices(current)
    if (!identical(i1, i2)) {
      return(sprintf("Datasets has different indexes. 'target'%s. 'current'%s.",
               if(length(i1)) paste0(": ", paste(i1, collapse=", ")) else " has no index",
               if(length(i2)) paste0(": ", paste(i2, collapse=", ")) else " has no index"))
    }

    # Trim any extra row.names attributes that came from some inheritance
    # Trim ".internal.selfref" as long as there is no `all.equal.externalptr` method
    exclude.attrs = function(x, attrs = c("row.names",".internal.selfref")) x[!names(x) %chin% attrs]
    a1 = exclude.attrs(attributes(target))
    a2 = exclude.attrs(attributes(current))
    if (length(a1) != length(a2)) return(sprintf("Datasets has different number of (non-excluded) attributes: target %s, current %s", length(a1), length(a2)))
    if (!identical(nm1 <- sort(names(a1)), nm2 <- sort(names(a2)))) return(sprintf("Datasets has attributes with different names: %s", paste(setdiff(union(names(a1), names(a2)), intersect(names(a1), names(a2))), collapse=", ")))
    attrs.r = all.equal(a1[nm1], a2[nm2], ..., check.attributes = check.attributes)
    if (is.character(attrs.r)) return(paste("Attributes: <", attrs.r, ">")) # skip further heavy processing
  }

  if (ignore.row.order) {
    if (".seqn" %chin% names(target))
      stop("None of the datasets to compare should contain a column named '.seqn'")
    bad.type = setNames(c("raw","complex","list") %chin% c(vapply_1c(current, typeof), vapply_1c(target, typeof)), c("raw","complex","list"))
    if (any(bad.type))
      stop("Datasets to compare with 'ignore.row.order' must not have unsupported column types: ", brackify(names(bad.type)[bad.type]))
    if (between(tolerance, 0, sqrt(.Machine$double.eps), incbounds=FALSE)) {
      warning("Argument 'tolerance' was forced to lowest accepted value `sqrt(.Machine$double.eps)` from provided ", format(tolerance, scientific=FALSE))
      tolerance = sqrt(.Machine$double.eps)
    }
    target_dup = as.logical(anyDuplicated(target))
    current_dup = as.logical(anyDuplicated(current))
    tolerance.msg = if (identical(tolerance, 0)) ", be aware you are using `tolerance=0` which may result into visually equal data" else ""
    if (target_dup || current_dup) {
      # handling 'tolerance' for duplicate rows - those `msg` will be returned only when equality with tolerance will fail
      if (any(vapply_1c(target,typeof)=="double") && !identical(tolerance, 0)) {
        if (target_dup && !current_dup) msg = c(msg, "Dataset 'target' has duplicate rows while 'current' doesn't")
        else if (!target_dup && current_dup) msg = c(msg, "Dataset 'current' has duplicate rows while 'target' doesn't")
        else { # both
          if (!identical(tolerance, sqrt(.Machine$double.eps))) # non-default will raise error
            stop("Duplicate rows in datasets, numeric columns and ignore.row.order cannot be used with non 0 tolerance argument")
          msg = c(msg, "Both datasets have duplicate rows, they also have numeric columns, together with ignore.row.order this force 'tolerance' argument to 0")
          tolerance = 0
        }
      } else { # no numeric columns or tolerance==0L
        if (target_dup && !current_dup)
          return(sprintf("Dataset 'target' has duplicate rows while 'current' doesn't%s", tolerance.msg))
        if (!target_dup && current_dup)
          return(sprintf("Dataset 'current' has duplicate rows while 'target' doesn't%s", tolerance.msg))
      }
    }
    # handling 'tolerance' for factor cols - those `msg` will be returned only when equality with tolerance will fail
    if (any(vapply_1b(target,is.factor)) && !identical(tolerance, 0)) {
      if (!identical(tolerance, sqrt(.Machine$double.eps))) # non-default will raise error
        stop("Factor columns and ignore.row.order cannot be used with non 0 tolerance argument")
      msg = c(msg, "Using factor columns together together with ignore.row.order, this force 'tolerance' argument to 0")
      tolerance = 0
    }
    jn.on = copy(names(target)) # default, possible altered later on
    dbl.cols = vapply_1c(target,typeof)=="double"
    if (!identical(tolerance, 0)) {
      if (!any(dbl.cols)) { # dbl.cols handles (removed) "all character columns" (char.cols) case as well
        tolerance = 0
      } else {
        jn.on = jn.on[c(which(!dbl.cols), which(dbl.cols))] # double column must be last for rolling join
      }
    }
    if (target_dup && current_dup) {
      target = shallow(target)[, ".seqn" := rowidv(target)]
      current = shallow(current)[, ".seqn" := rowidv(current)]
      jn.on = c(".seqn", jn.on)
    }
    # roll join to support 'tolerance' argument, conditional to retain support for factor when tolerance=0
    ans = if (identical(tolerance, 0)) target[current, nomatch=NA, which=TRUE, on=jn.on] else {
      ans1 = target[current, roll=tolerance, rollends=TRUE, which=TRUE, on=jn.on]
      ans2 = target[current, roll=-tolerance, rollends=TRUE, which=TRUE, on=jn.on]
      pmin(ans1, ans2, na.rm=TRUE)
    }
    if (any_na(as_list(ans))) {
      msg = c(msg, sprintf("Dataset 'current' has rows not present in 'target'%s%s", if (target_dup || current_dup) " or present in different quantity" else "", tolerance.msg))
      return(msg)
    }
    # rolling join other way around
    ans = if (identical(tolerance, 0)) current[target, nomatch=NA, which=TRUE, on=jn.on] else {
      ans1 = current[target, roll=tolerance, rollends=TRUE, which=TRUE, on=jn.on]
      ans2 = current[target, roll=-tolerance, rollends=TRUE, which=TRUE, on=jn.on]
      pmin(ans1, ans2, na.rm=TRUE)
    }
    if (any_na(as_list(ans))) {
      msg = c(msg, sprintf("Dataset 'target' has rows not present in 'current'%s%s", if (target_dup || current_dup) " or present in different quantity" else "", tolerance.msg))
      return(msg)
    }
  } else {
    for (i in seq_along(target)) {
      # trim.levels moved here
      x = target[[i]]
      y = current[[i]]
      if (xor(is.factor(x),is.factor(y)))
        stop("Internal error: factor type mismatch should have been caught earlier") # nocov
      cols.r = TRUE
      if (is.factor(x)) {
        if (!identical(levels(x),levels(y))) {
          if (trim.levels) {
            # do this regardless of check.attributes (that's more about classes, checked above)
            x = factor(x)
            y = factor(y)
            if (!identical(levels(x),levels(y)))
            cols.r = "Levels not identical even after refactoring since trim.levels is TRUE"
          } else {
            cols.r = "Levels not identical. No attempt to refactor because trim.levels is FALSE"
          }
        } else {
          cols.r = all.equal(x, y, check.attributes=check.attributes)
          # the check.attributes here refers to everything other than the levels, which are always
          # dealt with according to trim.levels
        }
      } else {
        cols.r = all.equal(unclass(x), unclass(y), tolerance=tolerance, ..., check.attributes=check.attributes)
        # classes were explicitly checked earlier above, so ignore classes here.
      }
      if (!isTRUE(cols.r)) return(paste0("Column '", names(target)[i], "': ", paste(cols.r,collapse=" ")))
    }
  }
  TRUE
}

