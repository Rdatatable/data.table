# setdiff for data.tables, internal at the moment #547, used in not-join
setdiff_ = function(x, y, by.x=seq_along(x), by.y=seq_along(y), use.names=FALSE) {
  if (!is.data.table(x) || !is.data.table(y)) stopf("x and y must both be data.tables")
  # !ncol redundant since all 0-column data.tables have 0 rows
  if (!nrow(x)) return(x)
  by.x = colnamesInt(x, by.x, check_dups=TRUE)
  if (!nrow(y)) return(unique(x, by=by.x))
  by.y = colnamesInt(y, by.y, check_dups=TRUE)
  if (length(by.x) != length(by.y)) stopf("length(by.x) != length(by.y)", domain=NA)
  # factor in x should've factor/character in y, and vice-versa
  for (a in seq_along(by.x)) {
    lc = by.y[a]
    rc = by.x[a]
    icnam = names(y)[lc]
    xcnam = names(x)[rc]
    if ( is.character(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
      stopf("When x's column ('%s') is character, the corresponding column in y ('%s') should be factor or character, but found incompatible type '%s'.", xcnam, icnam, typeof(y[[lc]]))
    } else if ( is.factor(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
      stopf("When x's column ('%s') is factor, the corresponding column in y ('%s') should be character or factor, but found incompatible type '%s'.", xcnam, icnam, typeof(y[[lc]]))
    } else if ( (is.integer(x[[rc]]) || is.double(x[[rc]])) && (is.logical(y[[lc]]) || is.character(y[[lc]])) ) {
      stopf("When x's column ('%s') is integer or numeric, the corresponding column in y ('%s') can not be character or logical types, but found incompatible type '%s'.", xcnam, icnam, typeof(y[[lc]]))
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
  if (!is.logical(all) || length(all) != 1L) stopf("argument 'all' should be logical of length one")
  if (!is.data.table(x) || !is.data.table(y)) stopf("x and y must both be data.tables")
  if (!identical(sort(names(x)), sort(names(y)))) stopf("x and y must have the same column names")
  if (!identical(names(x), names(y))) stopf("x and y must have the same column order")
  bad_types = c("raw", "complex", if (block_list) "list")
  found = bad_types %chin% c(vapply_1c(x, typeof), vapply_1c(y, typeof))
  if (any(found))
    stopf(ngettext(sum(found), "unsupported column type found in x or y: %s", "unsupported column types found in x or y: %s"),
          brackify(bad_types[found]), domain=NA)
  super = function(x) {
    # allow character->factor and integer->numeric because from v1.12.4 i's type is retained by joins, #3820
    ans = class1(x)
    switch(ans, factor="character", integer="numeric", ans)
  }
  if (!identical(sx<-sapply(x, super), sy<-sapply(y, super))) {
    w = which.first(sx!=sy)
    stopf("Item %d of x is '%s' but the corresponding item of y is '%s'.", w, class1(x[[w]]), class1(y[[w]]))
  }
  if (.seqn && ".seqn" %chin% names(x)) stopf("None of the datasets should contain a column named '.seqn'")
}

fintersect = function(x, y, all=FALSE) {
  .set_ops_arg_check(x, y, all, .seqn = TRUE)
  if (!nrow(x) || !nrow(y)) return(x[0L])
  if (all) {
    .seqn_id = NULL  # to avoid 'no visible binding for global variable' note from R CMD check
    x = shallow(x)[, ".seqn" := rowidv(.seqn_id), env=list(.seqn_id=x)]
    y = shallow(y)[, ".seqn" := rowidv(.seqn_id), env=list(.seqn_id=y)]
    jn.on = c(".seqn",setdiff(names(y),".seqn"))
    # fixes #4716 by preserving order of 1st (uses y[x] join) argument instead of 2nd (uses x[y] join)
    y[x, .SD, .SDcols=setdiff(names(y),".seqn"), nomatch=NULL, on=jn.on]
  } else {
    z = funique(x)  # fixes #3034. When .. prefix in i= is implemented (TODO), this can be x[funique(..y), on=, multi=]
    y[z, nomatch=NULL, on=names(y), mult="first"]
  }
}

fsetdiff = function(x, y, all=FALSE) {
  .set_ops_arg_check(x, y, all, .seqn = TRUE)
  if (!nrow(x)) return(x)
  if (!nrow(y)) return(if (!all) funique(x) else x)
  if (all) {
    .seqn_id = NULL  # to avoid 'no visible binding for global variable' note from R CMD check
    x = shallow(x)[, ".seqn" := rowidv(.seqn_id), env=list(.seqn_id=x)]
    y = shallow(y)[, ".seqn" := rowidv(.seqn_id), env=list(.seqn_id=y)]
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
      internal_error("ncol(current)==ncol(target) was checked above") # nocov
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
      return(sprintf(
        "%s. 'target': %s. 'current': %s.",
        gettext("Datasets have different keys"),
        if(length(k1)) brackify(k1) else gettextf("has no key"),
        if(length(k2)) brackify(k2) else gettextf("has no key")
      ))
    }
    # check index
    i1 = indices(target)
    i2 = indices(current)
    if (!identical(i1, i2)) {
      return(sprintf(
        "%s. 'target': %s. 'current': %s.",
        gettext("Datasets have different indices"),
        if(length(i1)) brackify(i1) else gettextf("has no index"),
        if(length(i2)) brackify(i2) else gettextf("has no index")
      ))
    }

    # Trim any extra row.names attributes that came from some inheritance
    # Trim ".internal.selfref" as long as there is no `all.equal.externalptr` method
    exclude.attrs = function(x, attrs = c("row.names",".internal.selfref")) x[!names(x) %chin% attrs]
    a1 = exclude.attrs(attributes(target))
    a2 = exclude.attrs(attributes(current))
    if (length(a1) != length(a2)) return(sprintf("Datasets has different number of (non-excluded) attributes: target %s, current %s", length(a1), length(a2)))
    if (!identical(nm1 <- sort(names(a1)), nm2 <- sort(names(a2)))) return(sprintf("Datasets has attributes with different names: %s", brackify(setdiff(union(names(a1), names(a2)), intersect(names(a1), names(a2))))))
    attrs.r = all.equal(a1[nm1], a2[nm2], ..., check.attributes = check.attributes)
    if (is.character(attrs.r)) return(paste("Attributes: <", attrs.r, ">")) # skip further heavy processing
  }

  if (ignore.row.order) {
    if (".seqn" %chin% names(target))
      stopf("None of the datasets to compare should contain a column named '.seqn'")
    bad.type = setNames(c("raw","complex","list") %chin% c(vapply_1c(current, typeof), vapply_1c(target, typeof)), c("raw","complex","list"))
    if (any(bad.type))
      stopf("Datasets to compare with 'ignore.row.order' must not have unsupported column types: %s", brackify(names(bad.type)[bad.type]))
    if (between(tolerance, 0.0, sqrt(.Machine$double.eps), incbounds=FALSE)) {
      warningf("Argument 'tolerance' was forced to lowest accepted value `sqrt(.Machine$double.eps)` from provided %s", format(tolerance, scientific=FALSE))
      tolerance = sqrt(.Machine$double.eps)
    }
    target_dup = as.logical(anyDuplicated(target))
    current_dup = as.logical(anyDuplicated(current))
    tolerance.msg = if (identical(tolerance, 0.0)) ", be aware you are using `tolerance=0` which may result into visually equal data" else ""
    if (target_dup || current_dup) {
      # handling 'tolerance' for duplicate rows - those `msg` will be returned only when equality with tolerance will fail
      if (any(vapply_1c(target, typeof) == "double") && !identical(tolerance, 0.0)) {
        if (target_dup && !current_dup) msg = c(msg, "Dataset 'target' has duplicate rows while 'current' doesn't")
        else if (!target_dup && current_dup) msg = c(msg, "Dataset 'current' has duplicate rows while 'target' doesn't")
        else { # both
          if (!identical(tolerance, sqrt(.Machine$double.eps))) # non-default will raise error
            stopf("Duplicate rows in datasets, numeric columns and ignore.row.order cannot be used with non 0 tolerance argument")
          msg = c(msg, "Both datasets have duplicate rows, they also have numeric columns, together with ignore.row.order this force 'tolerance' argument to 0")
          tolerance = 0.0
        }
      } else { # no numeric columns or tolerance==0
        if (target_dup && !current_dup)
          return(sprintf("Dataset 'target' has duplicate rows while 'current' doesn't%s", tolerance.msg))
        if (!target_dup && current_dup)
          return(sprintf("Dataset 'current' has duplicate rows while 'target' doesn't%s", tolerance.msg))
      }
    }
    # handling 'tolerance' for factor cols - those `msg` will be returned only when equality with tolerance will fail
    if (any(vapply_1b(target, is.factor)) && !identical(tolerance, 0.0)) {
      if (!identical(tolerance, sqrt(.Machine$double.eps))) # non-default will raise error
        stopf("Factor columns and ignore.row.order cannot be used with non 0 tolerance argument")
      msg = c(msg, "Using factor columns together together with ignore.row.order, this force 'tolerance' argument to 0")
      tolerance = 0.0
    }
    jn.on = copy(names(target)) # default, possible altered later on
    dbl.cols = vapply_1c(target, typeof)=="double"
    if (!identical(tolerance, 0.0)) {
      if (!any(dbl.cols)) { # dbl.cols handles (removed) "all character columns" (char.cols) case as well
        tolerance = 0.0
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
    ans = if (identical(tolerance, 0.0)) target[current, nomatch=NA, which=TRUE, on=jn.on] else {
      ans1 = target[current, roll=tolerance, rollends=TRUE, which=TRUE, on=jn.on]
      ans2 = target[current, roll=-tolerance, rollends=TRUE, which=TRUE, on=jn.on]
      pmin(ans1, ans2, na.rm=TRUE)
    }
    if (any_na(as_list(ans))) {
      msg = c(msg, sprintf("Dataset 'current' has rows not present in 'target'%s%s", if (target_dup || current_dup) " or present in different quantity" else "", tolerance.msg))
      return(msg)
    }
    # rolling join other way around
    ans = if (identical(tolerance, 0.0)) current[target, nomatch=NA, which=TRUE, on=jn.on] else {
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
      if (XOR(is.factor(x), is.factor(y)))
        internal_error("factor type mismatch should have been caught earlier") # nocov
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
        # for test 1710.5 and #4543, we want to (1) make sure we dispatch to
        #   any existing all.equal methods for x while also (2) treating class(x)/class(y)
        #   as attributes as regards check.attributes argument
        cols.r = all.equal(x, y, tolerance=tolerance, ..., check.attributes=check.attributes)
        if (!isTRUE(cols.r) && !check.attributes && isTRUE(all.equal(unclass(x), unclass(y), tolerance=tolerance, ..., check.attributes=FALSE)))
          cols.r = TRUE
      }
      if (!isTRUE(cols.r)) return(paste0("Column '", names(target)[i], "': ", paste(cols.r,collapse=" ")))
    }
  }
  TRUE
}
