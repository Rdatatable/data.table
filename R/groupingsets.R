rollup = function(x, ...) {
  UseMethod("rollup")
}
rollup.data.table = function(x, j, by, .SDcols, id = FALSE, total.label = NULL, ...) {
  # input data type basic validation
  if (!is.data.table(x))
    stopf("Argument 'x' must be a data.table object")
  if (!is.character(by))
    stopf("Argument 'by' must be a character vector of column names used in grouping.")
  if (!is.logical(id))
    stopf("Argument 'id' must be a logical scalar.")
  # generate grouping sets for rollup
  sets = lapply(length(by):0L, function(i) by[0L:i])
  # redirect to workhorse function
  jj = substitute(j)
  groupingsets.data.table(x, by=by, sets=sets, .SDcols=.SDcols, id=id, jj=jj, total.label=total.label)
}

cube = function(x, ...) {
  UseMethod("cube")
}
cube.data.table = function(x, j, by, .SDcols, id = FALSE, total.label = NULL, ...) {
  # input data type basic validation
  if (!is.data.table(x))
    stopf("Argument 'x' must be a data.table object")
  if (!is.character(by))
    stopf("Argument 'by' must be a character vector of column names used in grouping.")
  if (!is.logical(id))
    stopf("Argument 'id' must be a logical scalar.")
  if (missing(j))
    stopf("Argument 'j' is required")
  # generate grouping sets for cube - power set: http://stackoverflow.com/a/32187892/2490497
  n = length(by)
  keepBool = sapply(2L^(seq_len(n)-1L), function(k) rep(c(FALSE, TRUE), times=k, each=((2L^n)/(2L*k))))
  sets = lapply((2L^n):1L, function(jj) by[keepBool[jj, ]])
  # redirect to workhorse function
  jj = substitute(j)
  groupingsets.data.table(x, by=by, sets=sets, .SDcols=.SDcols, id=id, jj=jj, total.label=total.label)
}

groupingsets = function(x, ...) {
  UseMethod("groupingsets")
}
groupingsets.data.table = function(x, j, by, sets, .SDcols, id = FALSE, jj, total.label = NULL, ...) {
  # input data type basic validation
  if (!is.data.table(x))
    stopf("Argument 'x' must be a data.table object")
  if (ncol(x) < 1L)
    stopf("Argument 'x' is a 0-column data.table; no measure to apply grouping over.")
  if (anyDuplicated(names(x)) > 0L)
    stopf("Input data.table must not contain duplicate column names.")
  if (!is.character(by))
    stopf("Argument 'by' must be a character vector of column names used in grouping.")
  if (anyDuplicated(by) > 0L)
    stopf("Argument 'by' must have unique column names for grouping.")
  if (!is.list(sets) || !all(vapply_1b(sets, is.character)))
    stopf("Argument 'sets' must be a list of character vectors.")
  if (!is.logical(id))
    stopf("Argument 'id' must be a logical scalar.")
  if (!(is.null(total.label) ||
        (is.character(total.label) && length(total.label) == 1L) ||
        (is.list(total.label) && all(vapply_1b(total.label, is.character)) &&
         all(vapply_1i(total.label, length) == 1L) && !is.null(names(total.label)))))
    stopf("Argument 'total.label', if not NULL, must be a character string (character vector of length 1) or a named list of character strings.")
  if (is.list(total.label) && !is.null(names(total.label)) &&
      ("" %chin% names(total.label) || any(is.na(names(total.label)))))
    stopf("When argument 'total.label' is a list, all of the list elements must be named.")
  if (is.list(total.label) && anyDuplicated(names(total.label)))
    stopf("When argument 'total.label' is a list, the element names must not contain duplicates.")
  # logic constraints validation
  if (!all((sets.all.by <- unique(unlist(sets))) %chin% by))
    stopf("All columns used in 'sets' argument must be in 'by' too. Columns used in 'sets' but not present in 'by': %s", brackify(setdiff(sets.all.by, by)))
  if (id && "grouping" %chin% names(x))
    stopf("When using `id=TRUE` the 'x' data.table must not have a column named 'grouping'.")
  if (any(vapply_1i(sets, anyDuplicated)))  # anyDuplicated returns index of first duplicate, otherwise 0L
    stopf("Character vectors in 'sets' list must not have duplicated column names within a single grouping set.")
  if (length(sets) > 1L && (idx<-anyDuplicated(lapply(sets, sort))))
    warningf("'sets' contains a duplicate (i.e., equivalent up to sorting) element at index %d; as such, there will be duplicate rows in the output -- note that grouping by A,B and B,A will produce the same aggregations. Use `sets=unique(lapply(sets, sort))` to eliminate duplicates.", idx)
  if (is.list(total.label) && !all(names(total.label) %chin% by))
    stopf("When argument 'total.label' is a list, all element names must be in 'by' too. Element names in 'total.label' but not present in 'by': %s",
          brackify(setdiff(names(total.label), by)))
  # input arguments handling
  jj = if (!missing(jj)) jj else substitute(j)
  av = all.vars(jj, TRUE)
  if (":=" %chin% av)
    stopf("Expression passed to grouping sets function must not update by reference. Use ':=' on results of your grouping function.")
  if (missing(.SDcols))
    .SDcols = if (".SD" %chin% av) setdiff(names(x), by) else NULL
  if (length(names(by))) by = unname(by)
  # 0 rows template data.table to keep colorder and type
  empty = if (length(.SDcols)) x[0L, eval(jj), by, .SDcols=.SDcols] else x[0L, eval(jj), by]
  if (id && "grouping" %chin% names(empty)) # `j` could have been evaluated to `grouping` field
    stopf("When using `id=TRUE` the 'j' expression must not evaluate to a column named 'grouping'.")
  if (anyDuplicated(names(empty)) > 0L)
    stopf("There exists duplicated column names in the results, ensure the column passed/evaluated in `j` and those in `by` are not overlapping.")
  # adding grouping column to template - aggregation level identifier
  if (id) {
    set(empty, j = "grouping", value = integer())
    setcolorder(empty, c("grouping", by, setdiff(names(empty), c("grouping", by))))
  }
  # Define variables related to total.label
  if (!is.null(total.label)) {
    total.vars = intersect(by, unique(unlist(lapply(sets, function(u) setdiff(by, u)))))
    total.vars.char.fac = total.vars[vapply_1b(x[, total.vars, with=FALSE],
                                               function(u) is.character(u) || is.factor(u))]
    if (is.list(total.label) &&
        !all(vapply_1b(x[, names(total.label), with=FALSE], function(u) is.character(u) || is.factor(u))))
      warningf("'total.label' can only be applied to variables of type character or factor but was specified explicitly (in a named list) for the following variables which are neither character nor factor: %s",
               brackify(names(total.label)[!vapply_1b(x[, names(total.label), with=FALSE],
                                                      function(u) is.character(u) || is.factor(u))]))
    if (is.character(total.label) && (length(total.vars) > length(total.vars.char.fac)))
      warningf("'total.label' can only be applied to variables of type character or factor, so it was not used for the following variables, even though they have 'total' rows in the output: %s",
               brackify(setdiff(total.vars, total.vars.char.fac)))
    if (is.character(total.label)) {
      total.label.use = as.list(rep(total.label, length(total.vars.char.fac)))
      names(total.label.use) = total.vars.char.fac
    }
    if (is.list(total.label))
      total.label.use = total.label[intersect(names(total.label), total.vars.char.fac)]
    total.label.use.in.x = vapply_1b(names(total.label.use), function(u) total.label.use[[u]] %in% x[[u]])
    if (any(total.label.use.in.x))
      warningf("The 'total.label' value was already in the data for these variables: %s",
               brackify(names(total.label.use)[total.label.use.in.x]))
  }
  # workaround for rbindlist fill=TRUE on integer64 #1459
  int64.cols = vapply_1b(empty, inherits, "integer64")
  int64.cols = names(int64.cols)[int64.cols]
  if (length(int64.cols) && !requireNamespace("bit64", quietly=TRUE))
    stopf("Using integer64 class columns require to have 'bit64' package installed.") # nocov
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
    if (!is.null(total.label))
      for (total.var in intersect(setdiff(by, by.set), names(total.label.use))) {
        r[, (total.var) := total.label.use[[total.var]]]
      }
    r
  }
  # actually processing everything here
  rbindlist(c(
    list(empty), # 0 rows template for colorder and type
    lapply(sets, aggregate.set) # all aggregations
  ), use.names=TRUE, fill=TRUE)
}
