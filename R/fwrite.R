fwrite = function(x, file="", append=FALSE, quote="auto",
           sep=getOption("datatable.fwrite.sep", ","),
           sep2=c("","|",""), eol=if (.Platform$OS.type=="windows") "\r\n" else "\n",
           na="", dec=".", row.names=FALSE, col.names=TRUE,
           qmethod=c("double","escape"),
           logical01=getOption("datatable.logical01", FALSE), # due to change to TRUE; see NEWS
           logicalAsInt=logical01,
           scipen=getOption('scipen', 0L),
           dateTimeAs = c("ISO","squash","epoch","write.csv"),
           buffMB=8, nThread=getDTthreads(verbose),
           showProgress=getOption("datatable.showProgress", interactive()),
           compress = c("auto", "none", "gzip"),
           yaml = FALSE,
           bom = FALSE,
           verbose=getOption("datatable.verbose", FALSE),
           encoding = "") {
  na = as.character(na[1L]) # fix for #1725
  if (length(encoding) != 1L || !encoding %chin% c("", "UTF-8", "native")) {
    stopf("Argument 'encoding' must be '', 'UTF-8' or 'native'.")
  }
  if (missing(qmethod)) qmethod = qmethod[1L]
  if (missing(compress)) compress = compress[1L]
  if (missing(dateTimeAs)) { dateTimeAs = dateTimeAs[1L] }
  else if (length(dateTimeAs)>1L) stopf("dateTimeAs must be a single string")
  dateTimeAs = chmatch(dateTimeAs, c("ISO","squash","epoch","write.csv"))-1L
  if (is.na(dateTimeAs)) stopf("dateTimeAs must be 'ISO','squash','epoch' or 'write.csv'")
  if (!missing(logical01) && !missing(logicalAsInt))
    stopf("logicalAsInt has been renamed logical01. Use logical01 only, not both.")
  if (!missing(logicalAsInt)) {
    # TODO: warningf("logicalAsInt has been renamed logical01 for consistency with fread. It will work fine but please change to logical01 at your convenience so we can remove logicalAsInt in future.")
    logical01 = logicalAsInt
    logicalAsInt=NULL
  }
  scipen = if (is.numeric(scipen)) as.integer(scipen) else 0L
  buffMB = as.integer(buffMB)
  nThread = as.integer(nThread)
  # write.csv default is 'double' so fwrite follows suit. write.table's default is 'escape'
  # validate arguments
  if (is.matrix(x)) { # coerce to data.table if input object is matrix
    messagef("x being coerced from class: matrix to data.table")
    x = as.data.table(x)
  }
  stopifnot(is.list(x),
    identical(quote,"auto") || isTRUEorFALSE(quote),
    is.character(sep) && length(sep)==1L && (nchar(sep) == 1L || sep == ""),
    is.character(sep2) && length(sep2)==3L && nchar(sep2[2L])==1L,
    is.character(dec) && length(dec)==1L && nchar(dec) == 1L,
    dec != sep,  # sep2!=dec and sep2!=sep checked at C level when we know if list columns are present
    is.character(eol) && length(eol)==1L,
    length(qmethod) == 1L && qmethod %chin% c("double", "escape"),
    length(compress) == 1L && compress %chin% c("auto", "none", "gzip"),
    isTRUEorFALSE(col.names), isTRUEorFALSE(append), isTRUEorFALSE(row.names),
    isTRUEorFALSE(verbose), isTRUEorFALSE(showProgress), isTRUEorFALSE(logical01),
    isTRUEorFALSE(bom),
    length(na) == 1L, #1725, handles NULL or character(0) input
    is.character(file) && length(file)==1L && !is.na(file),
    length(buffMB)==1L && !is.na(buffMB) && 1L<=buffMB && buffMB<=1024L,
    length(nThread)==1L && !is.na(nThread) && nThread>=1L
    )

  is_gzip = compress == "gzip" || (compress == "auto" && grepl("\\.gz$", file))

  file = path.expand(file)  # "~/foo/bar"
  if (append && (file=="" || file.exists(file))) {
    if (missing(col.names)) col.names = FALSE
    if (verbose) catf("Appending to existing file so setting bom=FALSE and yaml=FALSE\n")
    bom = FALSE
    yaml = FALSE
  }
  if (identical(quote,"auto")) quote=NA  # logical NA
  if (file=="") {
    # console output which it seems isn't thread safe on Windows even when one-batch-at-a-time
    nThread = 1L
    showProgress = FALSE
    eol = "\n"  # Rprintf() is used at C level which knows inside it to output \r\n on Windows. Otherwise extra \r is output.
  }
  if (NCOL(x)==0L && file!="") {
    if (file.exists(file)) {
      suggested <- if (append) "" else gettextf("\nIf you intended to overwrite the file at %s with an empty one, please use file.remove first.", file)
      warningf("Input has no columns; doing nothing.%s", suggested)
      return(invisible())
    } else {
      warningf("Input has no columns; creating an empty file at '%s' and exiting.", file)
      file.create(file)
      return(invisible())
    }
  }
  yaml = if (!yaml) "" else {
    if (!requireNamespace('yaml', quietly=TRUE))
      stopf("'data.table' relies on the package 'yaml' to write the file header; please add this to your library with install.packages('yaml') and try again.") # nocov
    schema_vec = sapply(x, class)
    # multi-class objects reduced to first class
    if (is.list(schema_vec)) schema_vec = sapply(schema_vec, `[`, 1L)
    # as.vector strips names
    schema_vec = list(name=names(schema_vec), type=as.vector(schema_vec))
    yaml_header = list(
      source = sprintf('R[v%s.%s]::data.table[v%s]::fwrite',
                       R.version$major, R.version$minor, format(tryCatch(utils::packageVersion('data.table'), error=function(e) 'DEV'))),
      creation_time_utc = format(Sys.time(), tz='UTC'),
      schema = list(
        fields = lapply(
          seq_along(x),
          function(i) list(name=schema_vec$name[i], type=schema_vec$type[i])
        )
      ),
      header=col.names, sep=sep, sep2=sep2, eol=eol, na.strings=na,
      dec=dec, qmethod=qmethod, logical01=logical01
    )
    paste0('---', eol, yaml::as.yaml(yaml_header, line.sep=eol), '---', eol) # NB: as.yaml adds trailing newline
  }
  file = enc2native(file) # CfwriteR cannot handle UTF-8 if that is not the native encoding, see #3078.
  .Call(CfwriteR, x, file, sep, sep2, eol, na, dec, quote, qmethod=="escape", append,
        row.names, col.names, logical01, scipen, dateTimeAs, buffMB, nThread,
        showProgress, is_gzip, bom, yaml, verbose, encoding)
  invisible()
}

