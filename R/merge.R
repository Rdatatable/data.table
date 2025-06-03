merge.data.table = function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
               all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian=getOption("datatable.allow.cartesian"), incomparables=NULL, ...) {

  
  user_x_name <- deparse1(substitute(x))
  user_y_name <- deparse1(substitute(y))

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
      warningf("Input data.table '%s' (argument 'x') has no columns.", user_x_name)
    else
      warningf("Input data.table '%s' (argument 'y') has no columns.", user_y_name)
  }
  check_duplicate_names(x)
  check_duplicate_names(y)

  nm_x = names(x)
  nm_y = names(y)

  ## set up 'by'/'by.x'/'by.y'
  # The 'by' variable created here is crucial as it becomes the 'on' argument for y[x, on=by, ...]
  if ((!is.null(by.x) || !is.null(by.y)) && length(by.x) != length(by.y))
    stopf("`by.x` and `by.y` must be of same length.")
  if (!missing(by) && !missing(by.x))
    warningf("Supplied both `by` and `by.x`/`by.y`. `by` argument will be ignored.")
  if (!is.null(by.x)) { # by.x and by.y are provided
    if (length(by.x) == 0L || !is.character(by.x) || !is.character(by.y))
      stopf("A non-empty vector of column names is required for `by.x` and `by.y`.")
    # ----- MODIFIED: Use specific table names in stopf -----
    if (!all(idx <- by.x %chin% nm_x)) {
      stopf("The following columns listed in `%s` are missing from data.table '%s' (argument 'x'): %s", "by.x", user_x_name, brackify(by.x[!idx]))
    }
    if (!all(idx <- by.y %chin% nm_y)) {
      stopf("The following columns listed in `%s` are missing from data.table '%s' (argument 'y'): %s", "by.y", user_y_name, brackify(by.y[!idx]))
    }
    # ----- END MODIFICATION -----
    by = by.x             # 'by' takes values from by.x (cols of table 'x' arg)
    names(by) = by.y      # names of 'by' are cols from by.y (cols of table 'y' arg)
                          # So `by` for `on=by` will be c(col_from_y = "col_from_x", ...)
  } else { # by is provided or inferred
    if (is.null(by))
      by = intersect(key(x), key(y))
    if (!length(by))   # was is.null() before PR#5183  changed to !length()
      by = key(x)
    if (!length(by))
      by = intersect(nm_x, nm_y)
    if (length(by) == 0L || !is.character(by))
      stopf("A non-empty vector of column names for `by` is required.")
    if (!all(idx <- by %in% nm_x)) {
      stopf("The following columns listed in `%s` are missing from data.table '%s' (argument 'x'): %s", "by", user_x_name, brackify(by[!idx]))
    }
    if (!all(idx <- by %in% nm_y)) {
      stopf("The following columns listed in `%s` are missing from data.table '%s' (argument 'y'): %s", "by", user_y_name, brackify(by[!idx]))
    }
    by = unname(by)       # 'by' becomes a character vector of common column names for `on=by`
    by.x = by.y = by      # by.x and by.y are set for later use in renaming/ordering
  }
  if (length(list(...))) {
    ell = as.list(substitute(list(...)))[-1L]
    for (n in setdiff(names(ell), "")) warningf("Unknown argument '%s' has been passed.", n)
    unnamed_n = length(ell) - sum(nzchar(names(ell)))
    if (unnamed_n)
      warningf("Passed %d unknown and unnamed arguments.", unnamed_n)
  }
  start = setdiff(nm_x, by.x) # by.x here refers to key cols from original x's perspective or common names
  end = setdiff(nm_y, by.y)   # by.y here refers to key cols from original y's perspective or common names
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
    "%fin%" = function(x_val, table_val) if (is.character(x_val) && is.character(table_val)) x_val %chin% table_val else x_val %in% table_val
    xind = rowSums(x[, lapply(.SD, function(col_val_x) !(col_val_x %fin% incomparables)), .SDcols=by.x]) == length(by.x) # Use by.x for columns from table x
    yind = rowSums(y[, lapply(.SD, function(col_val_y) !(col_val_y %fin% incomparables)), .SDcols=by.y]) == length(by.y) # Use by.y for columns from table y
    x = x[xind]
    y = y[yind]
  }
  dt <- tryCatch({
      y[x, nomatch = if (all.x) NA else NULL, on = by, allow.cartesian = allow.cartesian]
    },
    bmerge_incompatible_type_error = function(e) {
            safe_attr_get <- function(obj, attr_name, placeholder) {
        val <- obj[[attr_name]] # Use [[ to get NULL if attribute doesn't exist
        if (is.null(val) || length(val) != 1L || is.na(val) || !nzchar(as.character(val)[1L])) {
          # Provide a warning to console to aid debugging if attributes are not as expected
          warning(paste0(
            "Developer warning: Problem retrieving attribute '", attr_name,
            "' from bmerge_incompatible_type_error object. Was: ",
            paste(capture.output(dput(val)), collapse=" ")
          ))
          return(placeholder)
        }
        return(as.character(val)[1L])
      }

      # In merge(USER_X, USER_Y), the internal join is y[x, ...]:
      # - USER_X (1st arg to merge) corresponds to 'i' argument in bmerge.R (source of error `e$c_bmerge_i_arg_*`).
      # - USER_Y (2nd arg to merge) corresponds to 'x' argument in bmerge.R (source of error `e$c_bmerge_x_arg_*`).

      col_from_user_x_table <- safe_attr_get(e, "c_bmerge_i_arg_bare_col_name", "[unknown column from 'x' arg]")
      type_from_user_x_table <- safe_attr_get(e, "c_bmerge_i_arg_type", "[unknown type for 'x' arg]")

      col_from_user_y_table <- safe_attr_get(e, "c_bmerge_x_arg_bare_col_name", "[unknown column from 'y' arg]")
      type_from_user_y_table <- safe_attr_get(e, "c_bmerge_x_arg_type", "[unknown type for 'y' arg]")

      # Ensure user_x_name and user_y_name are valid strings for sprintf
      final_user_x_name <- if (is.null(user_x_name) || length(user_x_name) != 1L || !nzchar(user_x_name[1L])) "x" else user_x_name[1L]
      final_user_y_name <- if (is.null(user_y_name) || length(user_y_name) != 1L || !nzchar(user_y_name[1L])) "y" else user_y_name[1L]
      msg <- sprintf(
        "When merging data.table '%s' (argument 'x') and data.table '%s' (argument 'y'):\n  Incompatible join types. Factor columns must join to factor or character columns.\n  Column '%s' from '%s' (arg 'x') has type: %s.\n  Column '%s' from '%s' (arg 'y') has type: %s.",
        final_user_x_name,
        final_user_y_name,
        col_from_user_x_table, final_user_x_name, type_from_user_x_table,
        col_from_user_y_table, final_user_y_name, type_from_user_y_table
      )
      stop(errorCondition(message = msg, class = c("datatable_merge_type_error", "data.table_error", "error", "condition"), call = NULL))
    }
  )


  if (all.y && nrow(y)) {  
    # are most likely to be caught in the primary join attempt.
    missingyidx = y[!x, which=TRUE, on=by, allow.cartesian=allow.cartesian]
    if (length(missingyidx)) dt = rbind(dt, y[missingyidx], use.names=FALSE, fill=TRUE, ignore.attr=TRUE)
  }
    newend = setdiff(nm_y, by.y)
  setcolorder(dt, c(by.y, setdiff(names(dt), c(by.y, newend)), newend))

  setnames(dt, c(by.x, start, end))
  if (nrow(dt) > 0L) {
    setkeyv(dt, if (sort) by.x else NULL) # Uses final key column names
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