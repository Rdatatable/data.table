SJ = function(...) {
  JDT = as.data.table(list(...))
  setkey(JDT)
}
# S for Sorted, usually used in i to sort the i table

# TO DO?: Use the CJ list() replication method for SJ (inside as.data.table.list?, #2109) too to avoid alloc.col

CJ <- function(..., sorted = TRUE, unique = FALSE)
{
  # Pass in a list of unique values, e.g. ids and dates
  # Cross Join will then produce a join table with the combination of all values (cross product).
  # The last vector is varied the quickest in the table, so dates should be last for roll for example
  l = list(...)
  emptyList <- FALSE ## fix for #2511
  if(any(vapply_1i(l, length) == 0L)){
    ## at least one column is empty The whole thing will be empty in the end
    emptyList <- TRUE
    l <- lapply(l, "[", 0L)
  }
  if (unique && !emptyList) l = lapply(l, unique)

  dups = FALSE # fix for #1513
  ncol = length(l)
  if (ncol==1L && !emptyList) {
    if (sorted && length(o <- forderv(l[[1L]]))) out = list(l[[1L]][o])
    else out = list(l[[1L]])
    nrow = length(l[[1L]])
  } else if (ncol > 1L && !emptyList) {
    # using rep.int instead of rep speeds things up considerably (but attributes are dropped).
    n = vapply_1i(l, length) #lengths(l) will work from R 3.2.0 (also above)
    nrow = prod(n)
    if (nrow > .Machine$integer.max) {
      stop("Cross product of elements provided to CJ() would result in ",nrow," rows which exceeds .Machine$integer.max == ",.Machine$integer.max)
    }
    
    # apply sorting
    if (sorted && any(idx <- vapply_1b(l, is.list))) stop("'sorted' is TRUE but element ", which(idx), " is a list, which can't be sorted; try setting sorted = FALSE")
    if (sorted) l = lapply(l, function(li) {
      # fix for #1513
      if (length(o <- forderv(li, retGrp=TRUE))) li = li[o]
      if (!dups) dups <<- attr(o, 'maxgrpn') > 1L
      return(li)
    })
    
    # standard [ method destroys attributes, so below
    #   will keep attributes only for classes with methods that impose so
    attrib = lapply(l, attributes)
    out = .Call(Ccj, l)
    if (!is.null(attrib)) for (jj in 1:ncol) if (!is.null(attrib[[jj]])) attributes(out[[jj]]) = attrib[[jj]]
  # ncol == 0 || emptyList
  } else {out = l; nrow = length(l[[1L]])}
  setattr(out, "row.names", .set_row_names(nrow))
  setattr(out, "class", c("data.table", "data.frame"))
  if (getOption("datatable.CJ.names", TRUE)) {  # added as FALSE in v1.11.6 with NEWS item saying TRUE in v1.12.0. TODO: remove in v1.13.0
    vnames = name_dots(...)$vnames
  } else {
    if (is.null(vnames <- names(out))) vnames = paste0("V", seq_len(ncol))
    else if (any(tt <- vnames=="")) vnames[tt] = paste0("V", which(tt))
  }
  setattr(out, "names", vnames)

  alloc.col(out)  # a tiny bit wasteful to over-allocate a fixed join table (column slots only), doing it anyway for consistency, and it's possible a user may wish to use SJ directly outside a join and would expect consistent over-allocation.
  if (sorted) {
    if (!dups) setattr(out, 'sorted', names(out))
    else setkey(out) # fix #1513
  }
  out
}
