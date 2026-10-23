rowwiseDT = function(...) {
  x = substitute(list(...))[-1L]
  if (is.null(nms <- names(x)))
    stopf("Must provide at least one column (use `name=`). See ?rowwiseDT for details")
  if (length(key_pos <- which(nms == "key" & nzchar(x))) > 0L) {
    if (length(key_pos) > 1L)
      stopf("key specified more than once")
    if (length(key <- eval(x[[key_pos]], parent.frame())) == 1L)
      key = cols_from_csv(key)
    x = x[-key_pos]
    nms = nms[-key_pos]
  } else {
    key_pos = 0L
  }
  header_pos = which(nzchar(nms))
  if (any(nzchar(x[header_pos])))
    stopf("Named arguments must be empty")
  if (!identical(header_pos, seq_along(header_pos)))
    stopf("Header must be the first N arguments")
  if (key_pos && !(key_pos - 1L) %in% c(0L, length(header_pos), length(x)))
    stopf("key specification not allowed in between names or data, only boundaries")
  header = nms[header_pos]
  ncols = length(header)
  body = lapply(x[-header_pos], eval, envir = parent.frame())
  nrows = length(body) %/% ncols
  if (length(body) != nrows * ncols)
    stopf("There are %d columns but the number of cells is %d, which is not an integer multiple of the columns", ncols, length(body))
  is_problematic = vapply_1b(body, function(v) !(is.atomic(v) || is.null(v) || typeof(v) == "list"))
  if (any(is_problematic)) {
    idx = which(is_problematic)[1L]
    col_idx = (idx - 1L) %% ncols + 1L
    col_name = header[col_idx]
    obj_type = class1(body[[idx]])
    stopf("Column '%s' is type '%s'. Non-atomic, non-list objects must be wrapped in list(), e.g., list(f) instead of f", col_name, obj_type)
  }
  # make all the non-scalar elements to a list
  needs_list = lengths(body) != 1L
  body[needs_list] = lapply(body[needs_list], list)
  body = split(body, rep(seq_len(nrows), each = ncols))
  ans = rbindlist(body)
  setnames(ans, header)
  if (key_pos)
    setkeyv(ans, key)
  ans
}
