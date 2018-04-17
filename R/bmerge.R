
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
      else if(isReallyReal(x))        out <- "reallyDouble"
      else                            out <- "intAsDouble"
    }
    else if(is.character(x))          out <- "character"
    else if(is.factor(x))             out <- "factor"
    else                              out <- "other"
    return(out)
  }
  
  ## function or coercing a vector to a new type. Depending on the source and target type,
  ## different coercion strategies are used.
  coerceClass <- function(x, to){
    sourceType <- getClass(x)
    ## reallyDouble and intAsDouble are the same here:
    if(sourceType %chin% c("reallyDouble", "intAsDouble")) sourceType <- "double"
    if(sourceType == to) return(x) ## nothing to be done
    if(!to %chin% c("logical", "integer", "double", "character", "factor", "integer64")) 
      stop("Invalid 'to' argument: ", to)
    if(sourceType == "other")
      stop("type coercion not supported for this type: ", paste0(class(x), collapse = ","))
    if(sourceType %chin% c("logical", "integer", "double") &
       to         %chin% c("logical", "integer", "double")){
      ## for these classes, we can do mode coercion to retain other class attributes, e.g. IDate
      ## identical types have been caught above
      out       <- x
      mode(out) <- to
    }
    else {
      ## we need as.'to'() conversion
      converter <- match.fun(paste0("as.", to))
      out       <- converter(x)
    }
    return(out)
  }
  
  ## Establish a lookup table with coercion strategies for each pair of types.
  ## Coercion strategies can be one of the following:
  ##------------------|-----------------------------------------------------
  ## y (yes):         | no coercion necessary, types match
  ##------------------|-----------------------------------------------------
  ## e (error):       | throw an error because of incompatible types
  ##------------------|-----------------------------------------------------
  ## ci (coercion i): | coerce the column in i to the type of the column in x
  ##------------------|-----------------------------------------------------
  ## ciw:             | same as above, but with warning.
  ##------------------|-----------------------------------------------------
  ## cx (coercion x): | coerce the column in x to the type of the column in i
  ##------------------|-----------------------------------------------------
  ## cxw:             | same as above, but with warning.
  ##------------------|-----------------------------------------------------
  ## rows mark the column type in i, columns the column type in x
  ## possible types are: logical, integer, intAsDouble, reallyDouble, character, factor, integer64
  allTypes <- c("logical", "integer", "intAsDouble", "reallyDouble", "character", "factor", "integer64", "other")
  ct <- matrix(data = NA_character_, nrow = length(allTypes), ncol = length(allTypes), 
               dimnames = list(allTypes, allTypes))
  
  ct["logical",      "logical"] = "y"
  ct["integer",      "logical"] = "e"
  ct["intAsDouble",  "logical"] = "e"
  ct["reallyDouble", "logical"] = "e"
  ct["character",    "logical"] = "e"
  ct["factor",       "logical"] = "e"
  ct["integer64",    "logical"] = "e"
  ct["other",        "logical"] = "e"
  
  ct["logical",      "integer"] = "ci"
  ct["integer",      "integer"] = "y"
  ct["intAsDouble",  "integer"] = "ci"
  ct["reallyDouble", "integer"] = "cx"
  ct["character",    "integer"] = "e"
  ct["factor",       "integer"] = "e"
  ct["integer64",    "integer"] = "ci"
  ct["other",        "integer"] = "e"
  
  ct["logical",      "intAsDouble"] = "ci"
  ct["integer",      "intAsDouble"] = "ci"
  ct["intAsDouble",  "intAsDouble"] = "y"
  ct["reallyDouble", "intAsDouble"] = "y"
  ct["character",    "intAsDouble"] = "e"
  ct["factor",       "intAsDouble"] = "e"
  ct["integer64",    "intAsDouble"] = "ci"
  ct["other",        "intAsDouble"] = "e"
  
  ct["logical",      "reallyDouble"] = "ci"
  ct["integer",      "reallyDouble"] = "ci"
  ct["intAsDouble",  "reallyDouble"] = "y"
  ct["reallyDouble", "reallyDouble"] = "y"
  ct["character",    "reallyDouble"] = "e"
  ct["factor",       "reallyDouble"] = "e"
  ct["integer64",    "reallyDouble"] = "ci"
  ct["other",        "reallyDouble"] = "e"
  
  ct["logical",      "character"] = "e"
  ct["integer",      "character"] = "e"
  ct["intAsDouble",  "character"] = "e"
  ct["reallyDouble", "character"] = "e"
  ct["character",    "character"] = "y"
  ct["factor",       "character"] = "ci"
  ct["integer64",    "character"] = "e"
  ct["other",        "character"] = "e"
  
  ct["logical",      "factor"] = "e"
  ct["integer",      "factor"] = "e"
  ct["intAsDouble",  "factor"] = "e"
  ct["reallyDouble", "factor"] = "e"
  ct["character",    "factor"] = "ci"
  ct["factor",       "factor"] = "y"
  ct["integer64",    "factor"] = "e"
  ct["other",        "factor"] = "e"
  
  ct["logical",      "integer64"] = "ci"
  ct["integer",      "integer64"] = "ci"
  ct["intAsDouble",  "integer64"] = "ci"
  ct["reallyDouble", "integer64"] = "cx"
  ct["character",    "integer64"] = "e"
  ct["factor",       "integer64"] = "e"
  ct["integer64",    "integer64"] = "y"
  ct["other",        "integer64"] = "e"
  
  ct["logical",      "other"] = "e"
  ct["integer",      "other"] = "e"
  ct["intAsDouble",  "other"] = "e"
  ct["reallyDouble", "other"] = "e"
  ct["character",    "other"] = "e"
  ct["factor",       "other"] = "e"
  ct["integer64",    "other"] = "e"
  ct["other",        "other"] = "e"
  
  if(anyNA(ct)) stop("Type coercion table ct incomplete.")

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
    myXtype  = myXclass
    if(myXtype %chin% c("intAsDouble", "reallyDouble")) myXtype = "double"
    if(myXtype == "other") myXtype = paste0(class(x[[rc]]), collapse = ",")
    myItype  = myIclass
    if(myItype %chin% c("intAsDouble", "reallyDouble")) myItype = "double"
    if(myItype == "other") myItype = paste0(class(i[[lc]]), collapse = ",")
    coercionStrategy <- ct[myIclass, myXclass]

    if(coercionStrategy == "y"){
      ## nothing to be done, but no 'next' since the factor related stuff below needs to be executed
    } 
    else if(coercionStrategy == "e"){
      stop(sprintf("Incompatible types: %s (%s) and %s (%s)", 
                   paste0("x.", xcnam), myXtype, paste0("i.", icnam), myItype))
    } 
    else if(coercionStrategy %chin% c("ci", "ciw")){
      ## coerce i[[lc]] to same class as x[[rc]]
      if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.", 
                                myItype, paste0("i.'", icnam, "'"), myXtype, paste0("x.'", xcnam, "'"))); flush.console()}
      if(coercionStrategy == "ciw"){
        warning(sprintf("Coercing %s column %s to %s to match type of %s.", 
                        myItype, paste0("i.'", icnam, "'"), myXtype, paste0("x.'", xcnam, "'")))
      }
      if(myXtype == "factor"){
        ## special treatment due to factor levels, see #499 and #945
        newval = coerceClass(origi[[lc]], to = myXtype) ## will do mode() coercion if possible to retain attributes
        set(origi, j=lc, value=newval)
        # TO DO: we need a way to avoid copying 'value' for internal purposes
        # that would allow setting: set(i, j=lc, value=origi[[lc]]) without resulting in a copy.
        # until then using 'val <- origi[[lc]]' below to avoid another copy.
      } else {
        newval = coerceClass(i[[lc]], to = myXtype) ## will do mode() coercion if possible to retain attributes
        set(i, j=lc, value=newval)
      }
      myItype = myXtype
    } 
    else if(coercionStrategy %chin% c("cx", "cxw")){
      ## coerce x[[rc]] to same class as i[[lc]]
      if (verbose) {cat(sprintf("Coercing %s column %s to %s to match type of %s.", 
                            myXtype, paste0("x.'", xcnam, "'"), myItype, paste0("i.'", icnam, "'"))); flush.console()}
      if(coercionStrategy == "cxw"){
        warning(sprintf("Coercing %s column %s to %s to match type of %s.", 
                        myXtype, paste0("x.'", xcnam, "'"), myItype, paste0("i.'", icnam, "'")))
      }
      newval = coerceClass(x[[rc]], to = myItype) ## will do mode() coercion if possible to retain attributes
      set(x, j=rc, value=newval)
      myXtype = myItype
    } 
    else stop("Internal error in bmerge: unknown type coercion strategy.")
    
    ## now take care about factor columns.
    if(myXtype == "factor"){
      if(myItype != "factor") stop("Internal error in bmerge: at this point, myItype should be factor.")
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

