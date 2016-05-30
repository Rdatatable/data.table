fwrite <- function(x, file.path, append=FALSE, quote="auto",
                   sep=",", eol=if (.Platform$OS.type=="windows") "\r\n" else "\n",
                   na="", col.names=TRUE, qmethod="double", verbose=FALSE, turbo=TRUE) {

    isLOGICAL <- function(x) isTRUE(x) || identical(FALSE, x)  # it seems there is no isFALSE in R?
    if (identical(quote,"auto")) quote=FALSE # TODO to implement auto at C level per field/column
    # validate arguments
    stopifnot(is.data.frame(x), ncol(x) > 0L, isLOGICAL(quote), 
        length(sep) == 1L && class(sep) == "character" && nchar(sep) == 1L, 
        length(eol) == 1L && class(eol) == "character", 
        length(qmethod) == 1L && qmethod %in% c("double", "escape"), 
        isLOGICAL(col.names), isLOGICAL(append), isLOGICAL(verbose), 
        isLOGICAL(turbo))
    if (append && missing(col.names) && file.exists(file.path)) 
    col.names = FALSE  # test 1658.16 checks this

    # handle paths like "~/foo/bar"
    file.path <- path.expand(file.path)

    .Call(Cwritefile, x, file.path, sep, eol, na, quote, qmethod == "escape", append, col.names, verbose, turbo)
    invisible()
}

