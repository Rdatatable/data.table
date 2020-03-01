merge.data.table = function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
               all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian=getOption("datatable.allow.cartesian"), ...) {
  if (!sort %in% c(TRUE, FALSE))
    stop("Argument 'sort' should be logical TRUE/FALSE")
  if (!no.dups %in% c(TRUE, FALSE))
    stop("Argument 'no.dups' should be logical TRUE/FALSE")
  class_x = class(x)
  if (!is.data.table(y)) {
    y = as.data.table(y)
    if (missing(by) && missing(by.x)) {
      by = key(x)
    }
  }
  if ((x0 <- length(x)==0L) | (y0 <- length(y)==0L)) warning("You are trying to join data.tables where ", if(x0 & y0) "'x' and 'y' arguments are" else if(x0 & !y0) "'x' argument is" else if(!x0 & y0) "'y' argument is", " 0 columns data.table.")
  if (any(duplicated(names(x)))) stop("x has some duplicated column name(s): ",paste(names(x)[duplicated(names(x))],collapse=","),". Please remove or rename the duplicate(s) and try again.")
  if (any(duplicated(names(y)))) stop("y has some duplicated column name(s): ",paste(names(y)[duplicated(names(y))],collapse=","),". Please remove or rename the duplicate(s) and try again.")

  ## set up 'by'/'by.x'/'by.y'
  if ( (!is.null(by.x) || !is.null(by.y)) && length(by.x)!=length(by.y) )
    stop("`by.x` and `by.y` must be of same length.")
  if (!missing(by) && !missing(by.x))
    warning("Supplied both `by` and `by.x/by.y`. `by` argument will be ignored.")
  if (!is.null(by.x)) {
    if (length(by.x) == 0L || !is.character(by.x) || !is.character(by.y))
      stop("A non-empty vector of column names is required for `by.x` and `by.y`.")
    if (!all(by.x %chin% names(x)))
      stop("Elements listed in `by.x` must be valid column names in x.")
    if (!all(by.y %chin% names(y)))
      stop("Elements listed in `by.y` must be valid column names in y.")
    by = by.x
    names(by) = by.y
  } else {
    if (is.null(by))
      by = intersect(key(x), key(y))
    if (is.null(by))
      by = key(x)
    if (is.null(by))
      by = intersect(names(x), names(y))
    if (length(by) == 0L || !is.character(by))
      stop("A non-empty vector of column names for `by` is required.")
    if (!all(by %chin% intersect(colnames(x), colnames(y))))
      stop("Elements listed in `by` must be valid column names in x and y")
    by = unname(by)
    by.x = by.y = by
  }
  # with i. prefix in v1.9.3, this goes away. Left here for now ...
  ## sidestep the auto-increment column number feature-leading-to-bug by
  ## ensuring no names end in ".1", see unit test
  ## "merge and auto-increment columns in y[x]" in test-data.frame.like.R
  start = setdiff(names(x), by.x)
  end = setdiff(names(y), by.y)
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

  dt = y[x, nomatch=if (all.x) NA else NULL, on=by, allow.cartesian=allow.cartesian]   # includes JIS columns (with a i. prefix if conflict with x names)

  if (all.y && nrow(y)) {  # If y does not have any rows, no need to proceed
    # Perhaps not very commonly used, so not a huge deal that the join is redone here.
    missingyidx = y[!x, which=TRUE, on=by, allow.cartesian=allow.cartesian]
    if (length(missingyidx)) {
      yy = y[missingyidx]
      othercolsx = setdiff(names(x), by)
      if (length(othercolsx)) {
        tmp = rep.int(NA_integer_, length(missingyidx))
        # TO DO: use set() here instead..
        yy = cbind(yy, x[tmp, othercolsx, with = FALSE])
      }
      # empty data.tables (nrow =0, ncol>0) doesn't skip names anymore in new rbindlist
      # takes care of #24 without having to save names. This is how it should be, IMHO.
      dt = rbind(dt, yy, use.names=FALSE)
    }
  }
  # X[Y] syntax puts JIS i columns at the end, merge likes them alongside i.
  newend = setdiff(names(y), by.y)
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
    warning("column names ", paste0("'", resultdupnames, "'", collapse=", "),
            " are duplicated in the result")
  }

  # retain custom classes of first argument that resulted in dispatch to this method, #1378
  setattr(dt, "class", class_x)
  dt
}
