fwrite <- function(x, file="", append=FALSE, quote="auto",
                   sep=",", eol=if (.Platform$OS.type=="windows") "\r\n" else "\n",
                   na="", col.names=TRUE, qmethod="double", verbose=FALSE, turbo=TRUE) {

    isLOGICAL <- function(x) isTRUE(x) || identical(FALSE, x)  # it seems there is no isFALSE in R?
    na = as.character(na[1L]) # fix for #1725
    if (identical(quote,"auto")) quote=FALSE # TODO to implement auto at C level per field/column
    # validate arguments
    stopifnot(is.data.frame(x), ncol(x) > 0L, isLOGICAL(quote), 
        length(sep) == 1L && class(sep) == "character" && nchar(sep) == 1L, 
        length(eol) == 1L && class(eol) == "character", 
        length(qmethod) == 1L && qmethod %in% c("double", "escape"), 
        isLOGICAL(col.names), isLOGICAL(append), isLOGICAL(verbose), 
        length(na) == 1L, #1725, handles NULL or character(0) input
        isLOGICAL(turbo),
        is.character(file) && length(file)==1 && !is.na(file))
    file <- path.expand(file)  # "~/foo/bar"
    if (append && missing(col.names) && (file=="" || file.exists(file)))
        col.names = FALSE  # test 1658.16 checks this
    if (verbose) old=setDTthreads(1)  # verbose messages (Rprintf) are not thread safe
    .Call(Cwritefile, x, file, sep, eol, na, quote, qmethod == "escape", append, col.names, verbose, turbo)
    if (verbose) setDTthreads(old)
    invisible()
}

traceAccuracy = function() .Call(CtraceAccuracy)

