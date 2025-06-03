merge.data.table = function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
               all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian=getOption("datatable.allow.cartesian"), incomparables=NULL, ...) {
  if (!sort %in% c(TRUE, FALSE))
    stopf("Argument 'sort' should be logical TRUE/FALSE")
  if (!no.dups %in% c(TRUE, FALSE))
    stopf("Argument 'no.dups' should be logical TRUE/FALSE")
  class_x = class(x)
  if (!is.data.table(y)) {
    y = as.data.table(y)
    if (missing(by) && missing(by.x)) {
      by = key(x)
    }
  }
  x0 = length(x) == 0L
  y0 = length(y) == 0L
  if (x0 || y0) {
    if (x0 && y0)
      warningf("Neither of the input data.tables to join have columns.")
    else if (x0)
      warningf("Input data.table '%s' has no columns.", "x")
    else
      warningf("Input data.table '%s' has no columns.", "y")
  }
  check_duplicate_names(x)
  check_duplicate_names(y)

  nm_x = names(x)
  nm_y = names(y)

  ## set up 'by'/'by.x'/'by.y'
  if ((!is.null(by.x) || !is.null(by.y)) && length(by.x) != length(by.y))
    stopf("`by.x` and `by.y` must be of same length.")
  if (!missing(by) && !missing(by.x))
    warningf("Supplied both `by` and `by.x`/`by.y`. `by` argument will be ignored.")
  if (!is.null(by.x)) {
    if (length(by.x) == 0L || !is.character(by.x) || !is.character(by.y))
      stopf("A non-empty vector of column names is required for `by.x` and `by.y`.")
    if (!all(idx <- by.x %chin% nm_x)) {
      stopf("The following columns listed in `%s` are missing from %s: %s", "by.x", "x", brackify(by.x[!idx]))
    }
    if (!all(idx <- by.y %chin% nm_y)) {
      stopf("The following columns listed in `%s` are missing from %s: %s", "by.y", "y", brackify(by.y[!idx]))
    }
    by = by.x
    names(by) = by.y
  } else {
    if (is.null(by))
      by = intersect(key(x), key(y))
    if (!length(by))
      by = key(x)
    if (!length(by))
      by = intersect(nm_x, nm_y)
    if (length(by) == 0L || !is.character(by))
      stopf("A non-empty vector of column names for `by` is required.")
    if (!all(idx <- by %in% nm_x)) {
      stopf("The following columns listed in `%s` are missing from %s: %s", "by", "x", brackify(by[!idx]))
    }
    if (!all(idx <- by %in% nm_y)) {
      stopf("The following columns listed in `%s` are missing from %s: %s", "by", "y", brackify(by[!idx]))
    }
    by = unname(by)
    by.x = by.y = by
  }

  if (length(list(...))) {
    ell = as.list(substitute(list(...)))[-1L]
    for (n in setdiff(names(ell), "")) warningf("Unknown argument '%s' has been passed.", n)
    unnamed_n = length(ell) - sum(nzchar(names(ell)))
    if (unnamed_n)
      warningf("Passed %d unknown and unnamed arguments.", unnamed_n)
  }

  start = setdiff(nm_x, by.x)
  end = setdiff(nm_y, by.y)
  dupnames = intersect(start, end)
  if (length(dupnames)) {
    start[chmatch(dupnames, start, 0L)] = paste0(dupnames, suffixes[1L])
    end[chmatch(dupnames, end, 0L)] = paste0(dupnames, suffixes[2L])
  }

  dupkeyx = intersect(by.x, end)
  if (no.dups && length(dupkeyx)) {
    end[chmatch(dupkeyx, end, 0L)] = paste0(dupkeyx, suffixes[2L])
  }

  if (!is.null(incomparables)) {
    "%fin%" = function(x, table) if (is.character(x) && is.character(table)) x %chin% table else x %in% table
    xind = rowSums(x[, lapply(.SD, function(x) !(x %fin% incomparables)), .SDcols=by.x]) == length(by)
    yind = rowSums(y[, lapply(.SD, function(x) !(x %fin% incomparables)), .SDcols=by.y]) == length(by)
    x = x[xind]
    y = y[yind]
  }

  # Enhanced error handling during join
  user_x_name = deparse1(substitute(x))
  user_y_name = deparse1(substitute(y))
  dt = tryCatch({
    y[x, nomatch=if (all.x) NA else NULL, on=by, allow.cartesian=allow.cartesian]
  }, bmerge_incompatible_type_error = function(e) {
    col_from_user_x <- e$bmerge_i_arg_details$col_name
    type_from_user_x <- e$bmerge_i_arg_details$type_str

    col_from_user_y <- e$bmerge_x_arg_details$col_name
    type_from_user_y <- e$bmerge_x_arg_details$type_str

    by_col_name <- col_from_user_x

    new_msg <- sprintf(
      "When merging data.table '%s' (argument 'x') and data.table '%s' (argument 'y') on column '%s':\n  Incompatible join types. Factor columns must join to factor or character columns.\n  Type in '%s' (column '%s'%s): %s\n  Type in '%s' (column '%s'%s): %s",
      user_x_name,
      user_y_name,
      by_col_name,
      user_x_name, by_col_name, if (suffixes[1] != "") paste0(" becoming '", by_col_name, suffixes[1], "'") else "", type_from_user_x,
      user_y_name, by_col_name, if (suffixes[2] != "") paste0(" becoming '", by_col_name, suffixes[2], "'") else "", type_from_user_y
    )

    stop(errorCondition(message = new_msg, class = c("datatable_merge_error", "error", "condition"), call = NULL))
  })

  if (all.y && nrow(y)) {
    missingyidx = y[!x, which=TRUE, on=by, allow.cartesian=allow.cartesian]
    if (length(missingyidx)) dt = rbind(dt, y[missingyidx], use.names=FALSE, fill=TRUE, ignore.attr=TRUE)
  }
  newend = setdiff(nm_y, by.y)
  setcolorder(dt, c(by.y, setdiff(names(dt), c(by.y, newend)), newend))
  setnames(dt, c(by.x, start, end))
  if (nrow(dt) > 0L) {
    setkeyv(dt, if (sort) by.x else NULL)
  }

  resultdupnames = names(dt)[duplicated(names(dt))]
  if (length(resultdupnames)) {
    warningf("column names %s are duplicated in the result", brackify(resultdupnames))
  }

  setattr(dt, "class", class_x)
  dt
}
