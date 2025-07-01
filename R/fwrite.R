fwrite = function(x, file="", append=FALSE, quote="auto",
           sep=getOption("datatable.fwrite.sep", ","),
           sep2=c("","|",""), eol=if (.Platform$OS.type=="windows") "\r\n" else "\n",
           na="", dec=".", row.names=FALSE, col.names=TRUE,
           qmethod=c("double","escape"),
           logical01=getOption("datatable.logical01", FALSE),
           scipen=getOption('scipen', 0L),
           dateTimeAs = c("ISO","squash","epoch","write.csv"),
           buffMB=8L, nThread=getDTthreads(verbose),
           showProgress=getOption("datatable.showProgress", interactive()),
           compress = c("auto", "none", "gzip"),
           compressLevel = 6L,
           yaml = FALSE,
           bom = FALSE,
           verbose=getOption("datatable.verbose", FALSE),
           encoding = "") {
  na = as.character(na[1L]) # fix for #1725
  if (length(encoding) != 1L || !encoding %chin% c("", "UTF-8", "native")) {
    stopf("Argument 'encoding' must be '', 'UTF-8' or 'native'.")
  }
  qmethod = match.arg(qmethod)
  compress = match.arg(compress)
  dateTimeAs = match.arg(dateTimeAs)
  dateTimeAs = chmatch(dateTimeAs, c("ISO", "squash", "epoch", "write.csv")) - 1L
  scipen = if (is.numeric(scipen)) as.integer(scipen) else 0L
  buffMB = as.integer(buffMB)
  nThread = as.integer(nThread)
  compressLevel = as.integer(compressLevel)
  # write.csv default is 'double' so fwrite follows suit. write.table's default is 'escape'
  # validate arguments
  if (is.matrix(x)) { # coerce to data.table if input object is matrix
    messagef("x being coerced from class: matrix to data.table")
    # keep row.names for matrix input #5315
    if (row.names && !is.null(rownames(x))) {
      row.names = FALSE
      x = as.data.table(x, keep.rownames="")
    } else {
      x = as.data.table(x)
    }
  }
  stopifnot(
    is.list(x),
    identical(quote,"auto") || isTRUEorFALSE(quote),
    is.character(sep) && length(sep)==1L && (nchar(sep) == 1L || identical(sep, "")),
    is.character(sep2) && length(sep2)==3L && nchar(sep2[2L])==1L,
    is.character(dec) && length(dec)==1L && nchar(dec) == 1L,
    dec != sep,  # sep2!=dec and sep2!=sep checked at C level when we know if list columns are present
    is.character(eol) && length(eol)==1L,
    length(qmethod) == 1L && qmethod %chin% c("double", "escape"),
    length(compress) == 1L && compress %chin% c("auto", "none", "gzip"),
    length(compressLevel) == 1L && 0L <= compressLevel && compressLevel <= 9L,
    isTRUEorFALSE(col.names), isTRUEorFALSE(append), isTRUEorFALSE(row.names),
    isTRUEorFALSE(verbose), isTRUEorFALSE(showProgress), isTRUEorFALSE(logical01),
    isTRUEorFALSE(bom),
    length(na) == 1L, #1725, handles NULL or character(0) input
    is.character(file) && length(file)==1L && !is.na(file),
    length(buffMB)==1L && !is.na(buffMB) && 1L<=buffMB && buffMB<=1024L,
    length(nThread)==1L && !is.na(nThread) && nThread>=1L
  )

  is_gzip = compress == "gzip" || (compress == "auto" && endsWithAny(file, ".gz"))

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
      suggested = if (append) "" else gettextf("\nIf you intended to overwrite the file at %s with an empty one, please use file.remove first.", file)
      warningf("Input has no columns; doing nothing.%s", suggested)
      return(invisible())
    } else {
      warningf("Input has no columns; creating an empty file at '%s' and exiting.", file)
      file.create(file)
      return(invisible())
    }
  }
  # nocov start. See test 17 in other.Rraw, not tested in the main suite.
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
  # nocov end
  file = enc2native(file) # CfwriteR cannot handle UTF-8 if that is not the native encoding, see #3078.
  # pre-encode any strings or factor levels to avoid translateChar trying to allocate from OpenMP threads
  if (encoding %chin% c("UTF-8", "native")) {
    enc = switch(encoding, "UTF-8" = enc2utf8, "native" = enc2native)
    x = lapply(x, function(x) {
      if (is.character(x)) x = enc(x)
      if (is.factor(x)) levels(x) = enc(levels(x))
      x
    })
  }
  .Call(CfwriteR, x, file, sep, sep2, eol, na, dec, quote, qmethod=="escape", append,
        row.names, col.names, logical01, scipen, dateTimeAs, buffMB, nThread,
        showProgress, is_gzip, compressLevel, bom, yaml, verbose, encoding)
  invisible()
}

haszlib = function() .Call(Cdt_has_zlib)
