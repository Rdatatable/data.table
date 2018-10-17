setnames <- function(x,old,new,skip_absent=FALSE) {
  # Sets by reference, maintains truelength, no copy of table at all.
  # But also more convenient than names(DT)[i]="newname"  because we can also do setnames(DT,"oldname","newname")
  # without an onerous match() ourselves. old can be positions, too, but we encourage by name for robustness.
  if (!is.data.frame(x)) stop("x is not a data.table or data.frame")
  if (length(names(x)) != length(x)) stop("dt is length ",length(dt)," but its names are length ",length(names(x)))
  if (missing(new)) {
    # for setnames(DT,new); e.g., setnames(DT,c("A","B")) where ncol(DT)==2
    if (!is.character(old)) stop("Passed a vector of type '",typeof(old),"'. Needs to be type 'character'.")
    if (length(old) != ncol(x)) stop("Can't assign ",length(old)," names to a ",ncol(x)," column data.table")
    nx <- names(x)
    # note that duplicate names are permitted to be created in this usage only
    if (anyNA(nx)) {
      # if x somehow has some NA names, which() needs help to return them, #2475
      w = which((nx != old) | (is.na(nx) & !is.na(old)))
    } else {
      w = which(nx != old)
    }
    if (!length(w)) return(invisible(x))  # no changes
    new = old[w]
    i = w
  } else {
    if (missing(old)) stop("When 'new' is provided, 'old' must be provided too")
    if (!is.character(new)) stop("'new' is not a character vector")
    if (is.numeric(old)) {
      if (length(sgn <- unique(sign(old))) != 1L)
        stop("Items of 'old' is numeric but has both +ve and -ve indices.")
      tt = abs(old)<1L | abs(old)>length(x) | is.na(old)
      if (any(tt)) stop("Items of 'old' either NA or outside range [1,",length(x),"]: ",paste(old[tt],collapse=","))
      i = if (sgn == 1L) as.integer(old) else seq_along(x)[as.integer(old)]
      if (any(duplicated(i))) stop("Some duplicates exist in 'old': ",paste(i[duplicated(i)],collapse=","))
    } else {
      if (!is.character(old)) stop("'old' is type ",typeof(old)," but should be integer, double or character")
      if (any(duplicated(old))) stop("Some duplicates exist in 'old': ", paste(old[duplicated(old)],collapse=","))
      i = chmatch(old,names(x))
      ########################
      if (skip_absent == TRUE){ w <- old %chin% names(x); old = old[w]; new = new[w]; i = i[w] }
      ########################
      if (anyNA(i)) stop("Items of 'old' not found in column names: ",paste(old[is.na(i)],collapse=","))
      if (any(tt<-!is.na(chmatch(old,names(x)[-i])))) stop("Some items of 'old' are duplicated (ambiguous) in column names: ",paste(old[tt],collapse=","))
    }
    if (length(new)!=length(i)) stop("'old' is length ",length(i)," but 'new' is length ",length(new))
  }
  # update the key if the column name being change is in the key
  m = chmatch(names(x)[i], key(x))
  w = which(!is.na(m))
  if (length(w))
    .Call(Csetcharvec, attr(x,"sorted"), m[w], new[w])

  # update secondary keys
  idx = attr(x,"index")
  for (k in names(attributes(idx))) {
    tt = strsplit(k,split="__")[[1L]][-1L]
    m = chmatch(names(x)[i], tt)
    w = which(!is.na(m))
    if (length(w)) {
      tt[m[w]] = new[w]
      newk = paste0("__",tt,collapse="")
      setattr(idx, newk, attr(idx, k))
      setattr(idx, k, NULL)
    }
  }

  .Call(Csetcharvec, attr(x,"names"), as.integer(i), new)
  invisible(x)
}
