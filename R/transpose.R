transpose = function(l, fill=NA, ignore.empty=FALSE, keep.names=NULL, make.names=NULL) {
  if (!is.null(make.names)) {
    stopifnot(length(make.names)==1L)
    if (is.character(make.names)) {
      m = chmatch(make.names, names(l))
      if (is.na(m))
        stopf("make.names='%s' not found in names of input", make.names)
      make.names = m
    } else {
      make.names = as.integer(make.names)
      if (is.na(make.names) || make.names<1L || make.names>length(l))
        stopf("make.names=%d is out of range [1,ncol=%d]", make.names, length(l))
    }
    colnames = as.character(l[[make.names]])
    l = if (is.data.table(l)) l[,-make.names,with=FALSE] else l[-make.names]
  }
  ans = .Call(Ctranspose, l, fill, ignore.empty, keep.names)
  if (!is.null(make.names)) setattr(ans, "names", c(keep.names, colnames))
  else if (is.data.frame(l))  # including data.table but not plain list
    setattr(ans, "names", c(keep.names, paste0("V", seq_len(length(ans)-length(keep.names)))))
  if (is.data.table(l)) setDT(ans)
  else if (is.data.frame(l)) setDF(ans)
  ans[]
}

tstrsplit = function(x, ..., fill=NA, type.convert=FALSE, keep, names=FALSE) {
  if (!isTRUEorFALSE(names) && !is.character(names))
    stopf("'names' must be TRUE/FALSE or a character vector.")
  ans = transpose(strsplit(as.character(x), ...), fill=fill, ignore.empty=FALSE)
  if (!missing(keep)) {
    keep = suppressWarnings(as.integer(keep))
    chk = min(keep) >= min(1L, length(ans)) & max(keep) <= length(ans)
    if (!isTRUE(chk) || !length(keep))
      stopf("'keep' should contain integer values between %d and %d.", min(1L, length(ans)), length(ans))
  } else {
    keep = seq_along(ans)
  }
  if (isFALSE(type.convert))
    ans = ans[keep]
  # Implementing #1094, but default FALSE
  else if (isTRUE(type.convert))
    ans = lapply(ans[keep], type.convert, as.is=TRUE)
  # Implementing and extending #5094
  else if (is.function(type.convert))
    ans = lapply(ans[keep], type.convert)
  else if (is.list(type.convert)) {
    if (all(vapply(type.convert, is.function, NA)) && (length(keep) == length(type.convert) || length(type.convert) == 1L))
      ans = mapply(function(idx, fun) fun(ans[[idx]]), keep, type.convert, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    else {
      n = length(type.convert)
      if(!n) stopf("The argument 'type.convert' does not support empty list.")
      is_named = nzchar(names(type.convert))
      all_is_named = length(is_named) && all(is_named)                  # because all(is_named)=TRUE if is_named=NULL <-- names(type.convert)=NULL
      last_item = paste(deparse(substitute(type.convert)[[n + 1L]], width.cutoff=500L), collapse=" ")
      if (!all_is_named) {
        if (!(sum(!is_named) == 1L && !is_named[n] && is.function(type.convert[[n]])))
          stopf("When the argument 'type.convert' contains an unnamed element, it is expected to be the last element and should be a function. More than one unnamed element is not allowed unless all elements are functions with length equal to %d (the length of the transpose list or 'keep' argument if it is specified).", length(keep))
        else {
          fothers = type.convert[[n]]         
          type.convert = type.convert[-n]
        }
      }
      indxs = unlist(type.convert, recursive=FALSE, use.names=FALSE)
      bad_indxs = setdiff(indxs, keep)
      if (!is.numeric(indxs) || anyNA(indxs) || anyDuplicated(indxs))
        stopf("When the argument 'type.convert' contains transpose list indices, it should be a named list of non-missing integer values (with no duplicate) except the last element that should be unnamed if it is a function.")
      if (length(bad_indxs))
        stopf("When the argument 'type.convert' contains transpose list indices, they should be integer values contained in the argument 'keep' (if it is specified) or be between %d and %d (if it is not). But '%s' is/are not contained in '%s'.", 1L, length(keep), toString(bad_indxs), toString(keep))
      if (exists("fothers", inherits=FALSE)) {
        others = setdiff(keep, indxs)
        if (length(others))
          ans[others] = lapply(ans[others], fothers)
        else
          warningf("In the argument 'type.convert', '%s' was ignored because all elements in the transpose list or elements corrisponding to indices specified in the 'keep' argument have already been converted.", last_item)
      }
      for (fn in names(type.convert)) {
        idx = type.convert[[fn]]
        ans[idx] = lapply(ans[idx], function(x) match.fun(fn)(x))
      }
      ans = ans[keep]
    }
  } else
    stopf("The argument 'type.convert' should be TRUE/FALSE, a function, a list of functions, or a named list of pairs 'fun=indices' with optionally one unnamed element (a function) but an object of type '%s' was provided.", typeof(type.convert))
  if (isFALSE(names)) return(ans)
  else if (isTRUE(names)) names = paste0("V", seq_along(ans))
  if (length(names) != length(ans)) {
    str = if (missing(keep)) "ans" else "keep"
    stopf("length(names) (= %d) is not equal to length(%s) (= %d).", length(names), str, length(ans))
  }
  setattr(ans, 'names', names)
  ans
}

