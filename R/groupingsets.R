rollup = function(x, ...) {
  UseMethod("rollup")
}
rollup.data.table = function(x, j, by, .SDcols, id = FALSE, ...) {
  # input data type basic validation
  if (!is.data.table(x))
    stop("Argument 'x' must be a data.table object")
  if (!is.character(by))
    stop("Argument 'by' must be a character vector of column names used in grouping.")
  if (!is.logical(id))
    stop("Argument 'id' must be a logical scalar.")
  # generate grouping sets for rollup
  sets = lapply(length(by):0L, function(i) by[0L:i])
  # redirect to workhorse function
  jj = substitute(j)
  groupingsets.data.table(x, by=by, sets=sets, .SDcols=.SDcols, id=id, jj=jj)
}

cube = function(x, ...) {
  UseMethod("cube")
}
cube.data.table = function(x, j, by, .SDcols, id = FALSE, ...) {
  # input data type basic validation
  if (!is.data.table(x))
    stop("Argument 'x' must be a data.table object")
  if (!is.character(by))
    stop("Argument 'by' must be a character vector of column names used in grouping.")
  if (!is.logical(id))
    stop("Argument 'id' must be a logical scalar.")
  # generate grouping sets for cube - power set: http://stackoverflow.com/a/32187892/2490497
  n = length(by)
  keepBool = sapply(2L^(seq_len(n)-1L), function(k) rep(c(FALSE, TRUE), times=k, each=((2L^n)/(2L*k))))
  sets = lapply((2L^n):1L, function(j) by[keepBool[j, ]])
  # redirect to workhorse function
  jj = substitute(j)
  groupingsets.data.table(x, by=by, sets=sets, .SDcols=.SDcols, id=id, jj=jj)
}

groupingsets = function(x, ...) {
  UseMethod("groupingsets")
}
groupingsets.data.table = function(x, j, by, sets, .SDcols, id = FALSE, jj, ...) {
  # input data type basic validation
  if (!is.data.table(x))
    stop("Argument 'x' must be a data.table object")
  if (ncol(x) < 1L)
    stop("Argument 'x' is a 0-column data.table; no measure to apply grouping over.")
  if (anyDuplicated(names(x)) > 0L)
    stop("Input data.table must not contain duplicate column names.")
  if (!is.character(by))
    stop("Argument 'by' must be a character vector of column names used in grouping.")
  if (anyDuplicated(by) > 0L)
    stop("Argument 'by' must have unique column names for grouping.")
  if (!is.list(sets) || !all(sapply(sets, is.character)))
    stop("Argument 'sets' must be a list of character vectors.")
  if (!is.logical(id))
    stop("Argument 'id' must be a logical scalar.")
  # logic constraints validation
  if (!all((sets.all.by <- unique(unlist(sets))) %chin% by))
    stop("All columns used in 'sets' argument must be in 'by' too. Columns used in 'sets' but not present in 'by': ", brackify(setdiff(sets.all.by, by)))
  if (id && "grouping" %chin% names(x))
    stop("When using `id=TRUE` the 'x' data.table must not have a column named 'grouping'.")
  if (any(sapply(sets, anyDuplicated)))
    stop("Character vectors in 'sets' list must not have duplicated column names within a single grouping set.")
  if (length(sets) > 1L && (idx<-anyDuplicated(lapply(sets, sort))))
    warning("'sets' contains a duplicate (i.e., equivalent up to sorting) element at index ", idx, "; as such, there will be duplicate rows in the output -- note that grouping by A,B and B,A will produce the same aggregations. Use `sets=unique(lapply(sets, sort))` to eliminate duplicates.")
  # input arguments handling
  jj = if (!missing(jj)) jj else substitute(j)
  av = all.vars(jj, TRUE)
  if (":=" %chin% av)
    stop("Expression passed to grouping sets function must not update by reference. Use ':=' on results of your grouping function.")
  if (missing(.SDcols))
    .SDcols = if (".SD" %chin% av) setdiff(names(x), by) else NULL
  # 0 rows template data.table to keep colorder and type
  empty = if (length(.SDcols)) x[0L, eval(jj), by, .SDcols=.SDcols] else x[0L, eval(jj), by]
  if (id && "grouping" %chin% names(empty)) # `j` could have been evaluated to `grouping` field
    stop("When using `id=TRUE` the 'j' expression must not evaluate to a column named 'grouping'.")
  if (anyDuplicated(names(empty)) > 0L)
    stop("There exists duplicated column names in the results, ensure the column passed/evaluated in `j` and those in `by` are not overlapping.")
  # adding grouping column to template - aggregation level identifier
  if (id) {
    set(empty, j = "grouping", value = integer())
    setcolorder(empty, c("grouping", by, setdiff(names(empty), c("grouping", by))))
  }
  # workaround for rbindlist fill=TRUE on integer64 #1459
  int64.cols = vapply_1b(empty, inherits, "integer64")
  int64.cols = names(int64.cols)[int64.cols]
  if (length(int64.cols) && !requireNamespace("bit64", quietly=TRUE))
    stop("Using integer64 class columns require to have 'bit64' package installed.") # nocov
  int64.by.cols = intersect(int64.cols, by)
  # aggregate function called for each grouping set
  aggregate.set = function(by.set) {
    r = if (length(.SDcols)) x[, eval(jj), by.set, .SDcols=.SDcols] else x[, eval(jj), by.set]
    if (id) {
      # integer bit mask of aggregation levels: http://www.postgresql.org/docs/9.5/static/functions-aggregate.html#FUNCTIONS-GROUPING-TABLE
      # 3267: strtoi("", base = 2L) output apparently unstable across platforms
      i_str = paste(c("1", "0")[by %chin% by.set + 1L], collapse="")
      set(r, j = "grouping", value = if (nzchar(i_str)) strtoi(i_str, base=2L) else 0L)
    }
    if (length(int64.by.cols)) {
      # workaround for rbindlist fill=TRUE on integer64 #1459
      missing.int64.by.cols = setdiff(int64.by.cols, by.set)
      if (length(missing.int64.by.cols)) r[, (missing.int64.by.cols) := bit64::as.integer64(NA)]
    }
    r
  }
  # actually processing everything here
  rbindlist(c(
    list(empty), # 0 rows template for colorder and type
    lapply(sets, aggregate.set) # all aggregations
  ), use.names=TRUE, fill=TRUE)
}
