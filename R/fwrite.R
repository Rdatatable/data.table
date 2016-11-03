fwrite <- function(x, file="", append=FALSE, quote="auto",
                   sep=",", eol=if (.Platform$OS.type=="windows") "\r\n" else "\n",
                   na="", dec=".", row.names=FALSE, col.names=TRUE,
                   qmethod=c("double","escape"), verbose=FALSE, ..turbo=TRUE) {
    isLOGICAL = function(x) isTRUE(x) || identical(FALSE, x)  # it seems there is no isFALSE in R?
    na = as.character(na[1L]) # fix for #1725
    if (missing(qmethod)) qmethod = qmethod[1L]
    # write.csv default is 'double' so fwrite follows suit. write.table's default is 'escape'
    # validate arguments
    stopifnot(is.data.frame(x), ncol(x) > 0L,
        identical(quote,"auto") || identical(quote,FALSE) || identical(quote,TRUE),
        length(sep) == 1L && class(sep) == "character" && nchar(sep) == 1L, 
        length(eol) == 1L && class(eol) == "character", 
        length(qmethod) == 1L && qmethod %in% c("double", "escape"), 
        isLOGICAL(col.names), isLOGICAL(append), isLOGICAL(verbose), isLOGICAL(row.names),
        length(na) == 1L, #1725, handles NULL or character(0) input
        isLOGICAL(..turbo),
        is.character(file) && length(file)==1 && !is.na(file),
        is.character(dec) && identical(nchar(dec),1L),
        dec != sep)
    file <- path.expand(file)  # "~/foo/bar"
    if (append && missing(col.names) && (file=="" || file.exists(file)))
        col.names = FALSE  # test 1658.16 checks this
    if (!..turbo) warning("The ..turbo=FALSE option will be removed in future. Please report any problems with ..turbo=TRUE.")
    if (identical(quote,"auto")) quote=NA  # logical NA
    if (verbose || file=="") old=setDTthreads(1)  # console output isn't thread safe    
    .Call(Cwritefile, x, file, sep, eol, na, dec, quote, qmethod=="escape", append, row.names, col.names, verbose, ..turbo)
    if (verbose) setDTthreads(old)
    invisible()
}

genLookups = function() invisible(.Call(CgenLookups))

