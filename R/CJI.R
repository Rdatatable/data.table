setindex <- function(dt, ..., verbose = getOption("datatable.verbose")){
  cols <- data.table:::getdots()
  setindexv(dt, cols, verbose = verbose)
}

setindexv <- function(dt, cols, verbose = getOption("datatable.verbose")){
  # explode to all combinations
  X <- seq_along(cols)
  l <- lapply(X, function(x) combn(length(X), x, simplify = FALSE))
  cols_idx <- unlist(l, recursive = FALSE)
  cols_combn <- lapply(cols_idx, function(idx) cols[idx])
  cols_idx_char <- lapply(cols_idx, function(x) paste(x,collapse="_"))
  names(cols_combn) <- unlist(cols_idx_char)
  # do list of data.table each combination of keys
  lapply(cols_combn, function(cols){
    res = dt[, cols, with=FALSE]
    res[, i:=1:nrow(dt)]
    setkeyv(res, cols, verbose = verbose)
  })
}

CJI <- function(index, ..., nomatch = 0){
  values <- as.list(substitute(list(...)))[-1L]
  idx_skip <- sapply(values, isTRUE, USE.NAMES = FALSE)
  index[[paste(which(!idx_skip),collapse="_")]][do.call(what = CJ, args = values[!idx_skip]), nomatch = nomatch][,i]
}
