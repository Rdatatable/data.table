## This file contains 
##   check_for_seq()  and  convert_to_col_number_and_check_valid()
## for allowing  .SDcols='b':'d'  and/or  by='g':'k'
##
## Todo:  Test against  unquoted column names
##        can NOT yet handle nested functions,
##           such as  c('b':'d')  or  do.call(paste, list("V", c(3, 7)))



check_for_seq <- function(input, x) {
## If input is of the form
##      from:to
## this function will interpret from and to to reference columns
## from, to can be integers or column names
## they do NOT need to be of the same type
##
## If the input is NOT of the above form, the input is returned unchanged
##

  arg <- as.list(substitute(input))

  ## if input is not of the expected form, return it unchanged
  if (!(arg[[1]] == ':' & length(arg) == 3)) 
    return(input)

  from <- convert_to_col_number_and_check_valid ( arg[[2]], x, showWarnings=TRUE )
  to   <- convert_to_col_number_and_check_valid ( arg[[3]], x, showWarnings=TRUE )

  return( names(x)[from:to] )

}


convert_to_col_number_and_check_valid <- function(cn, x, showWarnings=TRUE) {
## returns the column number of x corresponding to cn
## while performing several checks to ensure validity
##
## cn : a column name or column number
##      if it is numeric, then we simply check to ensure it is within seq(x)
##      if it is factor,  it is coerced to character with optional warning
##      otherwise, it is presumed a character and any coercian will be done by `==`
##                 in the line  which(cn == names(x))


  ## x must have valid names
  if (is.null(names(x)))
    stop ("x must have valid names. They are names(x) is NULL")

  ## if input is.name, we need to deparse it
  if (is.name(cn))
    cn <- deparse(cn)

  ## factors are too ambiguous. Do we interpret the character value 
  if (is.factor(cn)) {
    if (isTRUE(showWarnings))
      warning("input is a factor and will be coerced to character")
    cn <- as.character(cn)
  }

  ## match to col index number
  if (is.numeric(cn))
    col <- cn
  else 
    col <- which(cn == names(x))


  ## ERROR CHECK: confirm there is one and exactly one match
  ## --------------- ##
    extra_error_msg <- "\n\nNOTE: using 'b':'d' is a new feature.\nIf you feel this is error should not have occured, please report it."

    if (! length(col))  ## No Match
      stop ("'", cn, "' is not a name of a column of the data.table.", extra_error_msg)
    if (length(col) > 1)  ## More than one match
      stop ("'", cn, "' matches more than one (", length(col), ") columns of the data.table.\nConsider using make.names()", extra_error_msg)
    if (!any(col == seq(x))) ## outside range of x
      stop ("'", cn, "' is byeond the range of the seq(x) which is from (1:", length(x), ").", extra_error_msg)
  ## --------------- ##

  return(col)
}
