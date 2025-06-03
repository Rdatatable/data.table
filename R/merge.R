merge.data.table = function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
               all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian=getOption("datatable.allow.cartesian"), incomparables=NULL, ...) {

  # NO user_x_name / user_y_name here to maintain original variable environment for tests

  if (!sort %in% c(TRUE, FALSE))
    stopf("Argument 'sort' should be logical TRUE/FALSE")
  if (!no.dups %in% c(TRUE, FALSE))
    stopf("Argument 'no.dups' should be logical TRUE/FALSE")
  class_x = class(x)
  if (!is.data.table(y)) {
    y = as.data.table(y)
    if (missing(by) && missing(by.x)) {
      by = key(x) # Original logic
    }
  }
  x0 = length(x) == 0L
  y0 = length(y) == 0L
  if (x0 || y0) {
    if (x0 && y0)
      warningf("Neither of the input data.tables to join have columns.")
    else if (x0)
      warningf("Input data.table '%s' has no columns.", "x") # Original: literal "x"
    else
      warningf("Input data.table '%s' has no columns.", "y") # Original: literal "y"
  }
  check_duplicate_names(x)
  check_duplicate_names(y)

  nm_x = names(x) # Original logic
  nm_y = names(y) # Original logic

  ## set up 'by'/'by.x'/'by.y' - RETAIN ORIGINAL LOGIC EXACTLY
  if ((!is.null(by.x) || !is.null(by.y)) && length(by.x) != length(by.y))
    stopf("`by.x` and `by.y` must be of same length.")
  if (!missing(by) && !missing(by.x))
    warningf("Supplied both `by` and `by.x`/`by.y`. `by` argument will be ignored.")
  if (!is.null(by.x)) {
    if (length(by.x) == 0L || !is.character(by.x) || !is.character(by.y))
      stopf("A non-empty vector of column names is required for `by.x` and `by.y`.")
    if (!all(idx <- by.x %chin% nm_x)) {
      stopf("The following columns listed in `%s` are missing from %s: %s", "by.x", "x", brackify(by.x[!idx])) # Original: literal "x"
    }
    if (!all(idx <- by.y %chin% nm_y)) {
      stopf("The following columns listed in `%s` are missing from %s: %s", "by.y", "y", brackify(by.y[!idx])) # Original: literal "y"
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
      stopf("The following columns listed in `%s` are missing from %s: %s", "by", "x", brackify(by[!idx])) # Original: literal "x"
    }
    if (!all(idx <- by %in% nm_y)) {
      stopf("The following columns listed in `%s` are missing from %s: %s", "by", "y", brackify(by[!idx])) # Original: literal "y"
    }
    by = unname(by)
    by.x = by.y = by
  }

  # warn about unused arguments #2587
  if (length(list(...))) {
    ell = as.list(substitute(list(...)))[-1L]
    for (n in setdiff(names(ell), "")) warningf("Unknown argument '%s' has been passed.", n)
    unnamed_n = length(ell) - sum(nzchar(names(ell)))
    if (unnamed_n)
      warningf("Passed %d unknown and unnamed arguments.", unnamed_n)
  }

  start = setdiff(nm_x, by.x) # Original logic
  end = setdiff(nm_y, by.y)   # Original logic
  dupnames = intersect(start, end)
  if (length(dupnames)) {
    start[chmatch(dupnames, start, 0L)] = paste0(dupnames, suffixes[1L])
    end[chmatch(dupnames, end, 0L)] = paste0(dupnames, suffixes[2L])
  }
  dupkeyx = intersect(by.x, end)
  if (no.dups && length(dupkeyx)) {
    end[chmatch(dupkeyx, end, 0L)] = paste0(dupkeyx, suffixes[2L])
  }

  # implement incomparables argument #2587
  if (!is.null(incomparables)) {
    # %fin% to be replaced when #5232 is implemented/closed
    "%fin%" = function(x_val, table_val) if (is.character(x_val) && is.character(table_val)) x_val %chin% table_val else x_val %in% table_val
    xind = rowSums(x[, lapply(.SD, function(x_col_val) !(x_col_val %fin% incomparables)), .SDcols=by.x]) == length(by.x) # Original by.x usage
    yind = rowSums(y[, lapply(.SD, function(y_col_val) !(y_col_val %fin% incomparables)), .SDcols=by.y]) == length(by.y) # Original by.y usage
    # subset both so later steps still work
    x = x[xind] # Original modification of x
    y = y[yind] # Original modification of y
  }

  # ----- MINIMAL MODIFICATION: tryCatch around the main join call -----
  dt <- tryCatch({
      # Original join call, using 'by' as constructed by original logic above
      y[x, nomatch=if (all.x) NA else NULL, on=by, allow.cartesian=allow.cartesian]
    },
    bmerge_incompatible_type_error = function(e) {
      # Get table names *locally* only when this specific error occurs.
      # These x_arg_name and y_arg_name refer to the 'x' and 'y' arguments of merge.data.table
      x_arg_name <- deparse1(substitute(x, env = parent.frame(2))) # Go up to merge.data.table's frame
      y_arg_name <- deparse1(substitute(y, env = parent.frame(2))) # Go up to merge.data.table's frame

      # Ensure arg names are single, non-empty strings
      final_x_arg_name <- if (is.null(x_arg_name) || length(x_arg_name) != 1L || !nzchar(x_arg_name[1L])) "arg 'x'" else x_arg_name[1L]
      final_y_arg_name <- if (is.null(y_arg_name) || length(y_arg_name) != 1L || !nzchar(y_arg_name[1L])) "arg 'y'" else y_arg_name[1L]

      # Extract column/type details from error object 'e'
      # Assumes attributes on 'e' are valid single character strings from R/bmerge.R
      # Mapping: merge's 'x' is bmerge's 'i'; merge's 'y' is bmerge's 'x'
      col_from_x_arg <- e$c_bmerge_i_arg_bare_col_name
      type_from_x_arg <- e$c_bmerge_i_arg_type
      col_from_y_arg <- e$c_bmerge_x_arg_bare_col_name
      type_from_y_arg <- e$c_bmerge_x_arg_type

      # Construct the concise error message
      msg <- sprintf(
        "Incompatible join types for merge: table '%s' column '%s' (%s) and table '%s' column '%s' (%s). Factor columns must join to factor or character columns.",
        final_x_arg_name, col_from_x_arg, type_from_x_arg,
        final_y_arg_name, col_from_y_arg, type_from_y_arg
      )
      
      stop(errorCondition(message = msg, class = c("datatable_merge_type_error", "data.table_error", "error", "condition"), call = NULL))
    }
  )
  # ----- END MINIMAL MODIFICATION -----

  if (all.y && nrow(y)) {  # Original logic
    missingyidx = y[!x, which=TRUE, on=by, allow.cartesian=allow.cartesian]
    if (length(missingyidx)) dt = rbind(dt, y[missingyidx], use.names=FALSE, fill=TRUE, ignore.attr=TRUE)
  }
  
  newend = setdiff(nm_y, by.y) # Original logic
  setcolorder(dt, c(by.y, setdiff(names(dt), c(by.y, newend)), newend)) # Original logic
  setnames(dt, c(by.x, start, end)) # Original logic
  if (nrow(dt) > 0L) {
    setkeyv(dt, if (sort) by.x else NULL) # Original logic
  }

  resultdupnames = names(dt)[duplicated(names(dt))]
  if (length(resultdupnames)) {
    warningf("column names %s are duplicated in the result", brackify(resultdupnames))
  }

  setattr(dt, "class", class_x)
  dt
}