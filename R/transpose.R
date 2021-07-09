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
    if (!isTRUE(chk)) # handles NA case too
      stopf("'keep' should contain integer values between %d and %d.", min(1L, length(ans)), length(ans))
    ans = ans[keep]
  }
  # Implementing #1094, but default FALSE
  if(type.convert) ans = lapply(ans, type.convert, as.is = TRUE)
  if (isFALSE(names)) return(ans)
  else if (isTRUE(names)) names = paste0("V", seq_along(ans))
  if (length(names) != length(ans)) {
    str = if (missing(keep)) "ans" else "keep"
    stopf("length(names) (= %d) is not equal to length(%s) (= %d).", length(names), str, length(ans))
  }
  setattr(ans, 'names', names)
  ans
}

