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
    if (!length(by))   # was is.null() before PR#5183  changed to !length()
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

  # warn about unused arguments #2587
  .maybe_warn_merge_dots(...)

  # with i. prefix in v1.9.3, this goes away. Left here for now ...
  ## sidestep the auto-increment column number feature-leading-to-bug by
  ## ensuring no names end in ".1", see unit test
  ## "merge and auto-increment columns in y[x]" in test-data.frame.like.R
  start = setdiff(nm_x, by.x)
  end = setdiff(nm_y, by.y)
  dupnames = intersect(start, end)
  if (length(dupnames)) {
    start[chmatch(dupnames, start, 0L)] = paste0(dupnames, suffixes[1L])
    end[chmatch(dupnames, end, 0L)] = paste0(dupnames, suffixes[2L])
  }
  # If no.dups = TRUE we also need to added the suffix to columns in y
  # that share a name with by.x
  dupkeyx = intersect(by.x, end)
  if (no.dups && length(dupkeyx)) {
    end[chmatch(dupkeyx, end, 0L)] = paste0(dupkeyx, suffixes[2L])
  }

  # implement incomparables argument #2587
  if (!is.null(incomparables)) {
    # %fin% to be replaced when #5232 is implemented/closed
    "%fin%" = function(x, table) if (is.character(x) && is.character(table)) x %chin% table else x %in% table
    xind = rowSums(x[, lapply(.SD, function(x) !(x %fin% incomparables)), .SDcols=by.x]) == length(by)
    yind = rowSums(y[, lapply(.SD, function(x) !(x %fin% incomparables)), .SDcols=by.y]) == length(by)
    # subset both so later steps still work
    x = x[xind]
    y = y[yind]
  }
  dt = y[x, nomatch=if (all.x) NA else NULL, on=by, allow.cartesian=allow.cartesian]   # includes JIS columns (with a i. prefix if conflict with x names)

  if (all.y && nrow(y)) {  # If y does not have any rows, no need to proceed
    # Perhaps not very commonly used, so not a huge deal that the join is redone here.
    missingyidx = y[!x, which=TRUE, on=by, allow.cartesian=allow.cartesian]
    if (length(missingyidx)) dt = rbind(dt, y[missingyidx], use.names=FALSE, fill=TRUE, ignore.attr=TRUE)
  }
  # X[Y] syntax puts JIS i columns at the end, merge likes them alongside i.
  newend = setdiff(nm_y, by.y)
  # fix for #1290, make sure by.y order is set properly before naming
  setcolorder(dt, c(by.y, setdiff(names(dt), c(by.y, newend)), newend))
  setnames(dt, c(by.x, start, end))
  if (nrow(dt) > 0L) {
    setkeyv(dt, if (sort) by.x else NULL)
  }

  # Throw warning if there are duplicate column names in 'dt' (i.e. if
  # `suffixes=c("","")`, to match behaviour in base:::merge.data.frame)
  resultdupnames = names(dt)[duplicated(names(dt))]
  if (length(resultdupnames)) {
    warningf("column names %s are duplicated in the result", brackify(resultdupnames))
  }

  # retain custom classes of first argument that resulted in dispatch to this method, #1378
  setattr(dt, "class", class_x)
  dt
}

.maybe_warn_merge_dots <- function(...) {
  # TODO(R >= 3.5.0): use ...length()
  n_dots <- length(dots <- list(...))
  if (!n_dots) return(invisible())

  nm <- names(dots)
  if (is.null(nm)) {
    warningf(ngettext(n_dots, "merge.data.table() received %d unnamed argument in '...' which will be ignored.",
                              "merge.data.table() received %d unnamed arguments in '...' which will be ignored."),
              n_dots)
  } else {
    named_idx = nzchar(nm)
    if (all(named_idx)) {
      warningf(ngettext(n_dots, "merge.data.table() received %d unknown keyword argument which will be ignored: %s",
                                "merge.data.table() received %d unknown keyword arguments which will be ignored: %s"),
                n_dots, brackify(nm))
    } else {
      n_named <- sum(named_idx)
      unnamed_clause <- sprintf(ngettext(n_dots - n_named, "%d unnamed argument in '...'", "%d unnamed arguments in '...'"), n_dots - n_named)
      named_clause <- sprintf(ngettext(n_named, "%d unknown keyword argument", "%d unknown keyword arguments"), n_named)
      warningf("merge.data.table() received %s and %s, all of which will be ignored: %s", unnamed_clause, named_clause, brackify(nm[named_idx]))
    }
  }
}
