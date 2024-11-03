rowwiseDT = function(...) {
  x = substitute(list(...))[-1L]
  if (is.null(nms <- names(x)))
    stopf("Must provide at least one column (use `name=`). See ?rowwiseDT for details")
  header_pos = which(nzchar(nms))
  if (any(nzchar(x[header_pos])))
    stopf("Named arguments must be empty")
  if (!identical(header_pos, seq_along(header_pos)))
    stopf("Header must be the first N arguments")
  header = nms[header_pos]
  ncols = length(header)
  body = lapply(x[-header_pos], eval, envir = parent.frame())
  nrows = length(body) %/% ncols
  if (length(body) != nrows * ncols)
    stopf("There are %d columns but the number of cells is %d, which is not an integer multiple of the columns", ncols, length(body))
  # make all the non-scalar elements to a list
  needs_list = lengths(body) != 1L
  body[needs_list] = lapply(body[needs_list], list)
  body = split(body, rep(seq_len(nrows), each = ncols))
  ans = rbindlist(body)
  setnames(ans, header)
  ans
}
