
bmerge <- function(i, x, leftcols, rightcols, xo, roll, rollends, nomatch, mult, ops, nqgrp, nqmaxgrp, verbose)
{
  # TO DO: rename leftcols to icols, rightcols to xcols
  # TO DO: xo could be moved inside Cbmerge
  # bmerge moved to be separate function now that list() doesn't copy in R
  # types of i join columns are promoted to match x's types (with warning or verbose)

  # Important that i is already passed in as a shallow copy, due to these coercions for factors.
  # i.e. bmerge(i<-shallow(i),...)
  # The caller ([.data.table) then uses the coerced columns to build the output

  # careful to only plonk syntax (full column) on i from now on (otherwise i would change)
  # TO DO: enforce via .internal.shallow attribute and expose shallow() to users
  # This is why shallow() is very importantly internal only, currently.

  origi = shallow(i)      # Needed for factor to factor/character joins, to recover the original levels
                          # Otherwise, types of i join columns are anyways promoted to match x's
                          # types (with warning or verbose)
  resetifactor = NULL     # Keep track of any factor to factor/character join cols (only time we keep orig)
  for (a in seq_along(leftcols)) {
    # This loop is simply to support joining factor columns
    # Note that if i is keyed, if this coerces, i's key gets dropped and the key may not be retained
    lc = leftcols[a]   # i   # TO DO: rename left and right to i and x
    rc = rightcols[a]  # x
    icnam = names(i)[lc]
    xcnam = names(x)[rc]
    if (is.character(x[[rc]])) {
      if (is.character(i[[lc]])) next
      if (!is.factor(i[[lc]]))
        stop("x.'",xcnam,"' is a character column being joined to i.'",icnam,"' which is type '",typeof(i[[lc]]),"'. Character columns must join to factor or character columns.")
      if (verbose) cat("Coercing factor column i.'",icnam,"' to character to match type of x.'",xcnam,"'.\n",sep="")
      set(i,j=lc,value=as.character(i[[lc]]))
      # no longer copies all of i, thanks to shallow() and :=/set
      next
    }
    if (is.factor(x[[rc]])) {
      if (is.character(i[[lc]])) {
        if (verbose) cat("Coercing character column i.'",icnam,"' to factor to match type of x.'",xcnam,"'. If possible please change x.'",xcnam,"' to character. Character columns are now preferred in joins.\n",sep="")
        set(origi, j=lc, value=factor(origi[[lc]])) # note the use of 'origi' here - see #499 and #945
        # TO DO: we need a way to avoid copying 'value' for internal purposes
        # that would allow setting: set(i, j=lc, value=origi[[lc]]) without resulting in a copy.
        # until then using 'val <- origi[[lc]]' below to avoid another copy.
      } else {
        if (!is.factor(i[[lc]]))
          stop("x.'",xcnam,"' is a factor column being joined to i.'",icnam,"' which is type '",typeof(i[[lc]]),"'. Factor columns must join to factor or character columns.")
      }
      # Retain original levels of i's factor columns in factor to factor joins (important when NAs,
      # see tests 687 and 688).
      # Moved it outside of 'else' to fix #499 and #945.
      resetifactor = c(resetifactor,lc)
      if (roll!=0.0 && a==length(leftcols)) stop("Attempting roll join on factor column x.",names(x)[rc],". Only integer, double or character colums may be roll joined.")   # because the chmatch on next line returns <strike>NA</strike> <new>0</new> for missing chars in x (rather than some integer greater than existing). Note roll!=0.0 is ok in this 0 special floating point case e.g. as.double(FALSE)==0.0 is ok, and "nearest"!=0.0 is also true.
      val = origi[[lc]] # note: using 'origi' here because set(..., value = .) always copies '.', we need a way to avoid it in internal cases.
      lx = levels(x[[rc]])
      li = levels(val)
      newfactor = chmatch(li, lx, nomatch=0L)[val] # fix for #945, a hacky solution for now.
      levels(newfactor) = lx
      class(newfactor) = "factor"
      set(i, j=lc, value=newfactor)
      # COMMENT BELOW IS NOT TRUE ANYMORE... had to change nomatch to 0L to take care of case where 'NA' occurs as a separate value... See #945.
      # <OUTDATED> NAs can be produced by this level match, in which case the C code (it knows integer value NA)
      # can skip over the lookup. It's therefore important we pass NA rather than 0 to the C code.
    }
    # Fix for #1108.
    # TODO: clean this code up...
    # NOTE: bit64::is.double(int64) returns FALSE.. but base::is.double returns TRUE
    is.int64 <- function(x) inherits(x, 'integer64')
    is.strictlydouble <- function(x) !is.int64(x) && is.double(x)
    if (is.integer(x[[rc]]) && (base::is.double(i[[lc]]) || is.logical(i[[lc]]))) {
      # TO DO: add warning if reallyreal about loss of precision
      # or could coerce in binary search on the fly, at cost
      if (verbose) cat("Coercing ", typeof(i[[lc]])," column i.'",icnam,"' to integer to match type of x.'",xcnam,"'. Please avoid coercion for efficiency.\n",sep="")
      newval = i[[lc]]
      if (is.int64(newval))
        newval = as.integer(newval)
      else mode(newval) = "integer"  # retains column attributes (such as IDateTime class)
      set(i, j=lc, value=newval)
    } else if (is.int64(x[[rc]]) && (is.integer(i[[lc]]) || is.logical(i[[lc]]) || is.strictlydouble(i[[lc]]) )) {
      if (verbose) cat("Coercing ",typeof(i[[lc]])," column i.'",icnam,"' to double to match type of x.'",xcnam,"'. Please avoid coercion for efficiency.\n",sep="")
      newval = bit64::as.integer64(i[[lc]])
      set(i, j=lc, value=newval)
    } else if (is.strictlydouble(x[[rc]]) && (is.integer(i[[lc]]) || is.logical(i[[lc]]) || is.int64(i[[lc]]) )) {
      if (verbose) cat("Coercing ",typeof(i[[lc]])," column i.'",icnam,"' to double to match type of x.'",xcnam,"'. Please avoid coercion for efficiency.\n",sep="")
      newval = i[[lc]]
      if (is.int64(newval))
        newval = as.numeric(newval)
      else mode(newval) = "double"
      set(i, j=lc, value=newval)
    }
  }
  ## after all modifications of i, check if i has a proper key on all leftcols
  io <- identical(leftcols, head(chmatch(key(i), names(i)), length(leftcols)))
  if (verbose) {last.started.at=proc.time();cat("Starting bmerge ...");flush.console()}
  ans = .Call(Cbmerge, i, x, as.integer(leftcols), as.integer(rightcols), io, xo, roll, rollends, nomatch, mult, ops, nqgrp, nqmaxgrp)
  if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console()}

  # in the caller's shallow copy,  see comment at the top of this function for usage
  # We want to leave the coercions to i in place otherwise, since the caller depends on that to build the result
  if (length(resetifactor)) {
    for (ii in resetifactor)
      set(i,j=ii,value=origi[[ii]])
    if (haskey(origi))
      setattr(i, 'sorted', key(origi))
  }
  return(ans)
}

