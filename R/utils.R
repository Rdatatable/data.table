# all non-exported / unused internal (utility) functions

# which.first
which.first <- function(x)
{
    if (!is.logical(x)) {
        stop("x not boolean")
    }
    match(TRUE, x)
}

# which.last
which.last <- function(x)
{
    if (!is.logical(x)) {
        stop("x not boolean")
    }
    length(x) - match(TRUE, rev(x)) +1
}

# trim
trim <- function(x) {
    # Removes whitespace at the beginning and end of strings
    # Assigning to x[] to retain the original dimensions, rownames and colnames
    x[] = gsub(" +$", "", x)
    x[] = gsub("^ +", "", x)
    x
}

# take (I don't see it being used anywhere)
take <- function(x, n=1)
{
    # returns the head of head, without the last n observations
    # convenient when inlining expressions
    # NROW, for vectors, returns the vector length, but we cater for non-table like lists also here
    # TO DO: allow taking along any dimension e.g. all but last column, rather than all but last row.
    if (is.list(x) && !is.data.frame(x)  && !is.data.table(x)) l = length(x)
    else l = NROW(x)
    if (l < n) stop("Cannot take ",n," from ",l)
    head(x, l-n)
}
# TODO: Implement take as UseMethod. Specific methods for each type.

# plus
"%+%" <- function(x,y)
UseMethod("%+%")

"%+%.default" <- function(x,y) paste(paste(x,collapse=","),paste(y,collapse=","),sep="")
# we often construct warning msgs with a msg followed by several items of a vector, so %+% is for convenience
