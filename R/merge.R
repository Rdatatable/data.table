merge.data.table = function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
                            all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE,
                            allow.cartesian = getOption("datatable.allow.cartesian"), 
                            incomparables = NULL, ...) {
  if (!is.logical(sort)) stopf("Argument 'sort' should be logical TRUE/FALSE")
  if (!is.logical(no.dups)) stopf("Argument 'no.dups' should be logical TRUE/FALSE")
  
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
    if (x0 && y0) {
      warningf("Neither of the input data.tables to join have columns.")
    } else if (x0) {
      warningf("Input data.table '%s' has no columns.", "x")
    } else {
      warningf("Input data.table '%s' has no columns.", "y")
    }
  }
  
  check_duplicate_names(x)
  check_duplicate_names(y)

  nm_x = names(x)
  nm_y = names(y)

  # Setup 'by', 'by.x', 'by.y'
  if ((!is.null(by.x) || !is.null(by.y)) && length(by.x) != length(by.y)) {
    stopf("by.x and by.y must be of the same length.")
  }
  
  if (!missing(by) && !missing(by.x)) {
    warningf("Supplied both 'by' and 'by.x/by.y'. 'by' argument will be ignored.")
  }
  
  if (!is.null(by.x)) {
    if (length(by.x) == 0L || !is.character(by.x) || !is.character(by.y)) {
      stopf("A non-empty vector of column names is required for by.x and by.y.")
    }
    if (!all(by.x %chin% nm_x)) {
      stopf("Elements listed in by.x must be valid column names in x.")
    }
    if (!all(by.y %chin% nm_y)) {
      stopf("Elements listed in by.y must be valid column names in y.")
    }
    by = by.x
    names(by) = by.y
  } else {
    if (is.null(by)) by = intersect(key(x), key(y))
    if (!length(by)) by = key(x)
    if (!length(by)) by = intersect(nm_x, nm_y)
    if (length(by) == 0L || !is.character(by)) {
      stopf("A non-empty vector of column names for 'by' is required.")
    }

    # Updated Error Handling Section
    missing_in_x = setdiff(by, nm_x)
    missing_in_y = setdiff(by, nm_y)
    if (length(missing_in_x) > 0 || length(missing_in_y) > 0) {
      error_msg = "Columns listed in 'by' must be valid column names in both data.tables.\n"
      if (length(missing_in_x) > 0) {
        error_msg = paste0(error_msg, sprintf("? Missing in x: %s\n", toString(missing_in_x)))
      }
      if (length(missing_in_y) > 0) {
        error_msg = paste0(error_msg, sprintf("? Missing in y: %s", toString(missing_in_y)))
      }
      stopf(error_msg)
    }

    by = unname(by)
    by.x = by.y = by
  }

  # Warn about unused arguments
  if (length(list(...))) {
    ell = as.list(substitute(list(...)))[-1L]
    for (n in setdiff(names(ell), "")) warningf("Unknown argument '%s' has been passed.", n)
    unnamed_n = length(ell) - sum(nzchar(names(ell)))
    if (unnamed_n) warningf("Passed %d unknown and unnamed arguments.", unnamed_n)
  }

  # Handle duplicate column names
  start = setdiff(nm_x, by.x)
  end = setdiff(nm_y, by.y)
  dupnames = intersect(start, end)
  if (length(dupnames)) {
    start[chmatch(dupnames, start, 0L)] = paste0(dupnames, suffixes[1L])
    end[chmatch(dupnames, end, 0L)] = paste0(dupnames, suffixes[2L])
  }
  
  # Handle incomparables argument
  if (!is.null(incomparables)) {
    "%fin%" = function(x, table) if (is.character(x) && is.character(table)) x %chin% table else x %in% table
    xind = rowSums(x[, lapply(.SD, function(x) !(x %fin% incomparables)), .SDcols = by.x]) == length(by)
    yind = rowSums(y[, lapply(.SD, function(x) !(x %fin% incomparables)), .SDcols = by.y]) == length(by)
    x = x[xind]
    y = y[yind]
  }
  
  dt = y[x, nomatch = if (all.x) NA else NULL, on = by, allow.cartesian = allow.cartesian]
    
  if (all.y && nrow(y)) {
    missingyidx = y[!x, which = TRUE, on = by, allow.cartesian = allow.cartesian]
    if (length(missingyidx)) dt = rbind(dt, y[missingyidx], use.names = FALSE, fill = TRUE, ignore.attr = TRUE)
  }
  
  # Reorder columns
  newend = setdiff(nm_y, by.y)
  setcolorder(dt, c(by.y, setdiff(names(dt), c(by.y, newend)), newend))
  setnames(dt, c(by.x, start, end))
  
  if (nrow(dt) > 0L) {
    setkeyv(dt, if (sort) by.x else NULL)
  }
  
  # Warn about duplicate column names in result
  resultdupnames = names(dt)[duplicated(names(dt))]
  if (length(resultdupnames)) {
    warningf("Column names %s are duplicated in the result", toString(resultdupnames))
  }

  # Retain custom classes
  setattr(dt, "class", class_x)
  dt
}
