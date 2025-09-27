"print.data.table" <-
function (x, digits = NULL, quote = FALSE, right = TRUE, ...)
{
    if (length(x) == 0) {
        cat("NULL data table\n")
    } else {
        print(as.matrix(x), digits=digits, quote=quote, right=right, ...)
        #mm = do.call("cbind", lapply(x, format, digits=digits))
        #print(mm, ..., quote = quote, right = right)
    }
    invisible(x)
}

