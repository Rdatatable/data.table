fwrite <- function(x, file="", append=FALSE, quote="auto",
                   sep=",", sep2=c("","|",""), eol=if (.Platform$OS.type=="windows") "\r\n" else "\n",
                   na="", dec=".", row.names=FALSE, col.names=TRUE,
                   qmethod=c("double","escape"),
                   logicalAsInt=FALSE, dateTimeAs = c("ISO","squash","epoch","write.csv"),
                   buffMB=8, nThread=getDTthreads(),
                   showProgress=interactive(),
                   verbose=getOption("datatable.verbose")) {
    isLOGICAL = function(x) isTRUE(x) || identical(FALSE, x)  # it seems there is no isFALSE in R?
    na = as.character(na[1L]) # fix for #1725
    if (missing(qmethod)) qmethod = qmethod[1L]
    if (missing(dateTimeAs)) dateTimeAs = dateTimeAs[1L]
    else if (length(dateTimeAs)>1) stop("dateTimeAs must be a single string")
    dateTimeAs = chmatch(dateTimeAs, c("ISO","squash","epoch","write.csv"))-1L
    if (is.na(dateTimeAs)) stop("dateTimeAs must be 'ISO','squash','epoch' or 'write.csv'")
    buffMB = as.integer(buffMB)
    nThread = as.integer(nThread)
    # write.csv default is 'double' so fwrite follows suit. write.table's default is 'escape'
    # validate arguments
    stopifnot(is.list(x), ncol(x) > 0L,
        identical(quote,"auto") || identical(quote,FALSE) || identical(quote,TRUE),
        is.character(sep) && length(sep)==1L && nchar(sep) == 1L,
        is.character(sep2) && length(sep2)==3L && nchar(sep2[2L])==1L,
        is.character(dec) && length(dec)==1L && nchar(dec) == 1L,
        dec != sep,  # sep2!=dec and sep2!=sep checked at C level when we know if list columns are present 
        is.character(eol) && length(eol)==1L,
        length(qmethod) == 1L && qmethod %in% c("double", "escape"),
        isLOGICAL(col.names), isLOGICAL(append), isLOGICAL(row.names),
        isLOGICAL(verbose), isLOGICAL(showProgress), isLOGICAL(logicalAsInt),
        length(na) == 1L, #1725, handles NULL or character(0) input
        is.character(file) && length(file)==1 && !is.na(file),
        length(buffMB)==1 && !is.na(buffMB) && 1<=buffMB && buffMB<=1024,
        length(nThread)==1 && !is.na(nThread) && nThread>=1
        )
    file <- path.expand(file)  # "~/foo/bar"
    if (append && missing(col.names) && (file=="" || file.exists(file)))
        col.names = FALSE  # test 1658.16 checks this
    if (identical(quote,"auto")) quote=NA  # logical NA
    if (file=="") {
        # console output (Rprintf) isn't thread safe.
        # Perhaps more so on Windows (as experienced) than Linux
        nThread=1L
        showProgress=FALSE
    }
    .Call(Cwritefile, x, file, sep, sep2, eol, na, dec, quote, qmethod=="escape", append,
                      row.names, col.names, logicalAsInt, dateTimeAs, buffMB, nThread,
                      showProgress, verbose)
    invisible()
}

genLookups = function() invisible(.Call(CgenLookups))

