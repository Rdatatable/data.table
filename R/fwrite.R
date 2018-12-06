fwrite <- function(x, file="", append=FALSE, quote="auto",
           sep=",", sep2=c("","|",""), eol=if (.Platform$OS.type=="windows") "\r\n" else "\n",
           na="", dec=".", row.names=FALSE, col.names=TRUE,
           qmethod=c("double","escape"),
           logical01=getOption("datatable.logical01", FALSE), # due to change to TRUE; see NEWS
           logicalAsInt=logical01,
           dateTimeAs = c("ISO","squash","epoch","write.csv"),
           buffMB=8, nThread=getDTthreads(verbose),
           showProgress=getOption("datatable.showProgress", interactive()),
           verbose=getOption("datatable.verbose", FALSE)) {
  isLOGICAL = function(x) isTRUE(x) || identical(FALSE, x)  # it seems there is no isFALSE in R?
  na = as.character(na[1L]) # fix for #1725
  if (missing(qmethod)) qmethod = qmethod[1L]
  if (missing(dateTimeAs)) { dateTimeAs = dateTimeAs[1L] }
  else if (length(dateTimeAs)>1L) stop("dateTimeAs must be a single string")
  dateTimeAs = chmatch(dateTimeAs, c("ISO","squash","epoch","write.csv"))-1L
  if (is.na(dateTimeAs)) stop("dateTimeAs must be 'ISO','squash','epoch' or 'write.csv'")
  if (!missing(logical01) && !missing(logicalAsInt))
    stop("logicalAsInt has been renamed logical01. Use logical01 only, not both.")
  if (!missing(logicalAsInt)) {
    # TODO: warning("logicalAsInt has been renamed logical01 for consistency with fread. It will work fine but please change to logical01 at your convenience so we can remove logicalAsInt in future.")
    logical01 = logicalAsInt
    logicalAsInt=NULL
  }
  buffMB = as.integer(buffMB)
  nThread = as.integer(nThread)
  # write.csv default is 'double' so fwrite follows suit. write.table's default is 'escape'
  # validate arguments
  if (is.matrix(x)) { # coerce to data.table if input object is matrix
    message("x being coerced from class: matrix to data.table")
    x <- as.data.table(x)
  }
  stopifnot(is.list(x),
    identical(quote,"auto") || identical(quote,FALSE) || identical(quote,TRUE),
    is.character(sep) && length(sep)==1L && nchar(sep) == 1L,
    is.character(sep2) && length(sep2)==3L && nchar(sep2[2L])==1L,
    is.character(dec) && length(dec)==1L && nchar(dec) == 1L,
    dec != sep,  # sep2!=dec and sep2!=sep checked at C level when we know if list columns are present
    is.character(eol) && length(eol)==1L,
    length(qmethod) == 1L && qmethod %chin% c("double", "escape"),
    isLOGICAL(col.names), isLOGICAL(append), isLOGICAL(row.names),
    isLOGICAL(verbose), isLOGICAL(showProgress), isLOGICAL(logical01),
    length(na) == 1L, #1725, handles NULL or character(0) input
    is.character(file) && length(file)==1L && !is.na(file),
    length(buffMB)==1L && !is.na(buffMB) && 1L<=buffMB && buffMB<=1024,
    length(nThread)==1L && !is.na(nThread) && nThread>=1L
    )
  file <- path.expand(file)  # "~/foo/bar"
  if (append && missing(col.names) && (file=="" || file.exists(file)))
    col.names = FALSE  # test 1658.16 checks this
  if (identical(quote,"auto")) quote=NA  # logical NA
  if (file=="") {
    # console output which it seems isn't thread safe on Windows even when one-batch-at-a-time
    nThread = 1L
    showProgress = FALSE
    eol = "\n"  # Rprintf() is used at C level which knows inside it to output \r\n on Windows. Otherwise extra \r is output.
  }
  if (NCOL(x)==0L && file!="") {
    if (file.exists(file)) {
      warning("Input has no columns; doing nothing.",
              if (!append)
                paste("\nIf you intended to overwrite the file at",
                      file, "with an empty one, please use file.remove first."))
      return(invisible())
    } else {
      warning("Input has no columns; creating an empty file at '", file, "' and exiting.")
      file.create(file)
      return(invisible())
    }
  }
  file <- enc2native(file) # CfwriteR cannot handle UTF-8 if that is not the native encoding, see #3078.
  .Call(CfwriteR, x, file, sep, sep2, eol, na, dec, quote, qmethod=="escape", append,
          row.names, col.names, logical01, dateTimeAs, buffMB, nThread,
          showProgress, verbose)
  invisible()
}

