# data.table originally defined first(x) and last(x) with no arguments just for the single
# first/last observation. Over time n= has been added since xts::last has n so now it makes
# sense to support n. The difference to head/tail is the default n=1 vs n=6, and
# that first/last are not generic for speed by group.

first = function(x, n=1L, na.rm=FALSE, ...) {
  .firstlast(x, n=n, na.rm=na.rm, first=TRUE, ...)
}

last = function(x, n=1L, na.rm=FALSE, ...) {
  .firstlast(x, n=n, na.rm=na.rm, first=FALSE, ...)
}

.firstlast = function(x, n=1L, na.rm=FALSE, first=TRUE, ...) {
  if (inherits(x, "xts")) {
    if (isTRUE(getOption("datatable.verbose", FALSE)))
      catf("using %s\n", if (first) "xts::first" else "xts::last")
    return((if (first) xts::first else xts::last)(x, n=n, na.rm=na.rm, ...))
  }
  stopifnot(isTRUEorFALSE(na.rm) || identical(na.rm,"row"))
  stopifnot(is.numeric(n), length(n)==1L, n>=0L)
  n = as.integer(n)
  if (is.data.frame(x)) {
    if (!nrow(x)) return(x)
    if (identical(na.rm, "row")) {   # any NA on the row removes that row
      nna = which_(.Call(Cdt_na, x, seq_along(x)), bool=FALSE)
      # very similar to na.omit.data.table
      # TODO: n and first/last could be passed to Cdt_na and it could stop after finding n (it already does that in gsumm.c when gforce optimized)
      nna = .firstlastVector(nna, n=n, first=first, na.rm=FALSE)
      ans = .Call(CsubsetDT, x, nna, seq_along(x))  # works on DF too
    } else {
      ans = lapply(x, .firstlastVector, n=n, first=first, na.rm=na.rm)
      if (na.rm) {
        l = vapply_1i(ans, length)
        m = max(l)
        for (i in which(l<m)) {
          ans[[i]] = c(ans[[i]], rep(NA, m-l[i]))
        }
        # any row.names won't align to the values now in the result so don't retain them
      }
    }
    if (is.data.table(x)) setDT(ans) else setDF(ans)
    setattr(ans, "class", class(x))
    if (!isTRUE(na.rm) && length(rn<-attr(x,"row.names")))
      setattr(ans, "row.names", if (isFALSE(na.rm)) .firstlastVector(rn, n=n, first=first, na.rm=FALSE)
                                               else rn[nna])
    return(ans)
  }
  if (!length(x))
    return(x)
  if (!is.vector(x)) {
    if (!isFALSE(na.rm))
      stopf("na.rm=TRUE|'row' is not currently supported for '%s'", class(x)[1L])
    return((if (first) utils::head else utils::tail)(x, n=n, ...))  # e.g. matrix
  }
  return(.firstlastVector(x, n=n, first=first, na.rm=!isFALSE(na.rm)))  # !isFALSE to convert 'row' to TRUE
}

.firstlastVector = function(x, n, first, na.rm) {
  if (!length(x)) return(x)
  if (n==0L) return(x[0L])
  ans = if (na.rm) {
    nna = which_(if (is.list(x)) vapply_1b(x,function(y){is.null(y)||(length(y)==1L&&is.na(y))})
                            else is.na(x), bool=FALSE)   # TODO: again, n and first/last could be passed to C here
    if (!length(nna)) x[0L]
    else {y=min(n,length(nna)); x[nna[if (first) seq.int(1L,y) else seq.int(length(nna)-y+1L,length(nna))]]}
  } else {
    y=min(n,length(x)); x[if (first) seq.int(1L,y) else seq.int(length(x)-y+1L,length(x))] 
  }
  if (n>1L || na.rm)   # n!=length(ans)
    .Call("Csettruelength", ans, length(ans))
  # for dogroups.c to know that shorter results (including when na.rm results in a length-1) should be padded with NA to match the length of longer items
  # head and tail with na.rm=TRUE are by their nature returning a vector and therefore shouldn't be recycled when length-1; test 2240.81
  # TODO: new function pad() could be provided so user can do things like DT[, .(pad(na.omit(B)), pad(na.omit(C))), by=grp]
  #         to avoid the error 'Supplied 2 items for column 1 of group 1 which has 3 rows ...'
  #       and/or pad= could be added to [.data.table to allow padding all results
  # Since gforce_dynamic optimizes head/tail it knows to pad and that's optimized. However, default last(x) and first(x) (i.e. n=1 na.rm=FALSE) are
  # single-valued like mean,median etc and are recycled in the same way. This is consistent with n=1 na.rm=FALSE already not being treated as
  # gforce_dynamic in gsumm.c either. n=1 na.rm=TRUE returns empty when all-NA so is still a vector result not recycled when length-1.
  ans
}

