which.last = function(x)
{
    if (!is.logical(x)) {
        stop("x not boolean")
    }
    length(x) - match(TRUE, rev(x)) +1
}
