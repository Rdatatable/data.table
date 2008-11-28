which.first = function(x)
{
    if (!is.logical(x)) {
        stop("x not boolean")
    }
    match(TRUE, x)
}
