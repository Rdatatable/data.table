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
  .headtail = if (first) utils::head else utils::tail
  if (is.data.frame(x)) {
    if (!nrow(x)) return(x)
    if (identical(na.rm, "row")) {   # any NA on the row removes that row
      nna = which_(.Call(Cdt_na, x, seq_along(x)), bool=FALSE)
      # from na.omit.data.table without calling na.omit which would subset all non-NA rows
      # TODO: n and first/last could be passed to Cdt_na and it could stop after finding n
      nna = .headtail(nna, n=n)
      if (length(nna) < min(n,nrow(x))) {
        # to match optimized na.rm=TRUE behavior; e.g. when .SD is one column
        # TODO: extra argument all.na=NA|NULL (or pad.na=) could control this
        pad = rep.int(NA, min(n,nrow(x))-length(nna))
        # returning min(n,nrow(x)) is what optimized one-column does because GForce needs to be deterministic by group
        # currently; i.e. number of items per group doesn't depend on how many NA there are
        nna = if (first) c(nna, pad) else c(pad, nna)
      }
      return(x[nna,,drop=FALSE])
    }
    if (isTRUE(na.rm)) {  #  select the first/last non-NA within each column
      ans = lapply(x, .narmVector, n=n, first=first)
      l = sapply(ans, length)
      m = max(l)
      for (i in which(l<m)) {  # pad with NA
        ans[[i]] = if (first) c(ans[[i]], rep(NA, m-l[i]))
                   else       c(rep(NA, m-l[i]), ans[[i]])
      }
      if (is.data.table(x)) setDT(ans) else setDF(ans)
      setattr(ans, "class", class(x))
      return(ans)
    }
    # na.rm=FALSE
    return(.headtail(x, n=n, ...))
  }
  if (!length(x))
    return(x)
  if (is.vector(x) && !isFALSE(na.rm))
    return(.narmVector(x, n=n, first=first))
  if (!isFALSE(na.rm))
    stopf("na.rm=TRUE|'row' is not currently supported for '%s'", class(x)[1L])
  .headtail(x, n=n, ...)  
  # TODO when n=1, return(x[length(x)]) would save method dispatch overhead
  # TODO and previous version had lx = length(x); if (!lx) x else x[[lx]].  So empty input returned empty
}

.narmVector = function(x, n, first) {
  nna = which_(is.na(x), bool=FALSE)   # TODO: again, n and first/last could be passed to C here
  if (!length(nna)) x[NA_integer_]
  else if (n==1L)   x[nna[if (first) 1L else length(nna)]]
  else              x[(if (first) utils::head else utils::tail)(nna, n)]  # TODO: avoid dispatch here and do ourselves since just a vector
}

