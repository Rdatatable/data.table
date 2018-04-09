
bmerge <- function(i, x, leftcols, rightcols, io, xo, roll, rollends, nomatch, mult, ops, nqgrp, nqmaxgrp, verbose)
{
  # TO DO: rename leftcols to icols, rightcols to xcols
  # NB: io is currently just TRUE or FALSE for whether i is keyed
  # TO DO: io and xo could be moved inside Cbmerge
  # bmerge moved to be separate function now that list() doesn't copy in R
  # types of i join columns are promoted to match x's types (with warning or verbose)

  # Important that i is already passed in as a shallow copy, due to these coercions for factors.
  # i.e. bmerge(i<-shallow(i),...)
  # The caller ([.data.table) then uses the coerced columns to build the output
  # careful to only plonk syntax (full column) on i from now on (otherwise i would change)
  # TO DO: enforce via .internal.shallow attribute and expose shallow() to users
  # This is why shallow() is very importantly internal only, currently.
  
  # In contrast, x is not passed as shallow, but is shallow copied inside bmerge. 
  # This allows for coercion of column types for the join, 
  # but the calling function only sees the original column types of x
  # careful to only plonk syntax (full column) on x from now on (otherwise x would change)
  
  x     = shallow(x)
  origi = shallow(i)      # Needed for factor to factor/character joins, to recover the original levels
                          # Otherwise, types of i join columns are anyways promoted to match x's
                          # types (with warning or verbose)
  resetifactor = NULL     # Keep track of any factor to factor/character join cols (only time we keep orig)
  
  ## function to determine type of columns. 
  ## Important not to use typeof() since integer and factor have same type (integer)
  ## important to test for is.double instead of is.numeric because of bug with IDate (test 346)
  getClass <- function(x){
    if(is.logical(x))                 out <- "logical"
    else if(is.integer(x))            out <- "integer"
    else if(base::is.double(x)){ ## base::is.double(integer64) is TRUE, while bit64::is.double(integer64) is FALSE!
      if(inherits(x, "integer64"))    out <- "integer64" ## is.numeric returns TRUE on integer64 columns
      ## distinguish integer values with storage mode double from real numeric 
      ## values that can't be coerced to integer without loss of precision
      else if(isReallyReal(x))        out <- "realDouble"
      else out <- "double"}
    else if(is.character(x))          out <- "character"
    else if(is.factor(x))             out <- "factor"
    else out <- "other" ## whatever it is, it is not a standard type
    if(length(out) > 1) out <- "other"
    return(out)
  }
  ## The following column types throw an error when joined together. Column in x is on the left of ==
  typeErrorClasses = c("logical==integer", "logical==double", "logical==realDouble", "logical==character", "logical==factor", "logical==integer64",
                       "integer==character", "integer==factor",
                       "double==character", "double==factor",
                       "realDouble==character", "realDouble==factor",
                       "character==logical", "character==integer", "character==double", "character==realDouble", "character==integer64",
                       "factor==logical", "factor==integer", "factor==double", "factor==realDouble", "factor==integer64",
                       "integer64==character", "integer64==factor")
  ## The following column types need no specific treatment when joined together
  typeNoTreatment = c("logical==logical", "integer==integer", 
                      "double==double", "double==realDouble",
                      "realDouble==double", "realDouble==realDouble",
                      "character==character", ## factor == factor needs treatment to consolidate levels
                      "integer64==integer64"
                      )
  ## The following column types need simple coercion of the column in i by setting the mode(i[[lc]]) <- "newclass", 
  ## where newclass is the class on the left of == 
  typeModeCoercionI = c("integer==logical", "integer==double", ## not for realDouble!!
                        "double==logical", "double==integer",
                        "realDouble==logical", "realDouble==integer")
  ## The following column types need simple coercion of the column in x by setting the mode(x[[rc]]) <- "newclass", 
  ## where newclass is the class on the rigth of == 
  typeModeCoercionX = c("integer==realDouble")
  ## The following column types need coercion of the column in i by calling as.newclass(i[[lc]]), 
  ## where newclass is the class on the left of ==
  typeCastCoercionI = c("integer==integer64", "double==integer64", "realDouble==integer64",
                        "integer64==logical", "integer64==integer", "integer64==double",
                        "character==factor")
  ## The following column types need coercion of the column in x by calling as.newclass(x[[rc]]), 
  ## where newclass is the class on the right of ==
  typeCastCoercionX = c("integer64==realDouble")
  
  for (a in seq_along(leftcols)) {
    # This loop does the following:
    # - check that join columns have compatible types
    # - do type coercions if necessary
    # - special support for joining factor columns
    # Note that if i is keyed, if this coerces, i's key gets dropped and the key may not be retained
    lc = leftcols[a]   # i   # TO DO: rename left and right to i and x
    rc = rightcols[a]  # x
    icnam = names(i)[lc]
    xcnam = names(x)[rc]
    myXclass = getClass(x[[rc]])
    myIclass = getClass(i[[lc]])
    myXtype = if(myXclass == "realDouble") "double" else myXclass
    myItype = if(myIclass == "realDouble") "double" else myIclass
    joinTypeIdentifier = paste0(myXclass, "==", myIclass)
    if(joinTypeIdentifier %chin% typeNoTreatment){
      next
    } else if(joinTypeIdentifier %chin% typeErrorClasses){
      stop(sprintf("Incompatible types: %s (%s) and %s (%s)", 
                   paste0("x.", xcnam), myXtype, paste0("i.", icnam), myItype))
    } else if(joinTypeIdentifier %chin% typeModeCoercionI){
      ## coerce i[[lc]] to same class as x[[rc]] by mode() approach
      if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.", 
                            myItype, paste0("i.'", icnam, "'"), myXtype, paste0("x.'", xcnam, "'"))); flush.console()}
      newval = i[[lc]]
      mode(newval) = myXtype  # retains column attributes (such as IDateTime class)
      set(i, j=lc, value=newval)
    } else if(joinTypeIdentifier %chin% typeModeCoercionX){
      ## coerce x[[rc]] to same class as i[[lc]] by mode() approach
      if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.", 
                            myXtype, paste0("x.'", xcnam, "'"), myItype, paste0("i.'", icnam, "'"))); flush.console()}
      newval = x[[rc]]
      mode(newval) = myItype  # retains column attributes (such as IDateTime class)
      set(x, j=rc, value=newval)
    } else if(joinTypeIdentifier %chin% typeCastCoercionI){
      ## coerce i[[lc]] to same class as x[[rc]] by as.newclass() approach
      converter <- match.fun(paste0("as.", myXtype))
      if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.", 
                            myItype, paste0("i.'", icnam, "'"), myXtype, paste0("x.'", xcnam, "'"))); flush.console()}
      newval = i[[lc]]
      newval = converter(newval)
      set(i, j=lc, value=newval)
    } else if(joinTypeIdentifier %chin% typeCastCoercionX){
      ## coerce x[[rc]] to same class as i[[lc]] by as.newclass() approach
      converter <- match.fun(paste0("as.", myItype))
      if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.", 
                            myXtype, paste0("x.'", xcnam, "'"), myItype, paste0("i.'", icnam, "'"))); flush.console()}
      newval = x[[rc]]
      newval = converter(newval)
      set(x, j=rc, value=newval)
    } else if(joinTypeIdentifier %chin% c("factor==factor", "factor==character")){
      if (myItype == "character") {
        if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.", 
                              myItype, paste0("i.'", icnam, "'"), myXtype, paste0("x.'", xcnam, "'"))); flush.console()}
        set(origi, j=lc, value=factor(origi[[lc]])) # note the use of 'origi' here - see #499 and #945
        # TO DO: we need a way to avoid copying 'value' for internal purposes
        # that would allow setting: set(i, j=lc, value=origi[[lc]]) without resulting in a copy.
        # until then using 'val <- origi[[lc]]' below to avoid another copy.
      }
      # levels of factors have to be treated properly when coercing
      # Retain original levels of i's factor columns in factor to factor joins (important when NAs,
      # see tests 687 and 688).
      # Moved it outside of 'else' to fix #499 and #945.
      resetifactor = c(resetifactor,lc)
      if (roll!=0.0 && a==length(leftcols)) stop("Attempting roll join on factor column x.",xcnam,". Only integer, double or character colums may be roll joined.")   # because the chmatch on next line returns <strike>NA</strike> <new>0</new> for missing chars in x (rather than some integer greater than existing). Note roll!=0.0 is ok in this 0 special floating point case e.g. as.double(FALSE)==0.0 is ok, and "nearest"!=0.0 is also true.
      val = origi[[lc]] # note: using 'origi' here because set(..., value = .) always copies '.', we need a way to avoid it in internal cases.
      lx = levels(x[[rc]])
      li = levels(val)
      newfactor = chmatch(li, lx, nomatch=0L)[val] # fix for #945, a hacky solution for now.
      levels(newfactor) = lx
      class(newfactor) = "factor"
      set(i, j=lc, value=newfactor)      
    } else if(myIclass == "other" || myXclass == "other"){
      ## at least one column has a non-standard class, e.g. POSIXct.
      ## join will work if 
      ## - both columns have exactly the same class
      ## - typeof(x[[rc]]) == typeof(i[[lc]]) with a warning
      if(all(class(x[[rc]]) == class(i[[lc]]))){
        next
      } else if(typeof(x[[rc]]) == typeof(i[[lc]])){
        warning(sprintf("Joining on columns of different class: %s (%s) and %s (%s). Join works since both columns are of the same type: %s",
                        paste0("x.'", xcnam, "'"), paste0(class(x[[rc]]), collapse = ","), paste0("i.'", icnam, "'"), paste0(class(i[[lc]]), collapse = ","), typeof(x[[rc]])))
        ## nothing needs to be done, Cbmerge will work because of the same types.
      } else {
        stop(sprintf("Incompatible types: %s (%s) and %s (%s)", 
                     paste0("x.'", xcnam, "'"), paste0(class(x[[rc]]), collapse = ","), paste0("i.'", icnam, "'"), paste0(class(i[[lc]]), collapse = ",")))
      }
    } else {
      stop("Internal error: data.table's bmerge doesn't know how to handle joins of type ", joinTypeIdentifier, ". Please report the bug to the developers")
    }
  }
  if (verbose) {last.started.at=proc.time();cat("Starting bmerge ...");flush.console()}
  ans = .Call(Cbmerge, i, x, as.integer(leftcols), as.integer(rightcols), io<-haskey(i), xo, roll, rollends, nomatch, mult, ops, nqgrp, nqmaxgrp)
  # NB: io<-haskey(i) necessary for test 579 where the := above change the factor to character and remove i's key
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

