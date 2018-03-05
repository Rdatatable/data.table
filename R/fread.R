
fread <- function(input="",file,sep="auto",sep2="auto",dec=".",quote="\"",nrows=Inf,header="auto",na.strings=getOption("datatable.na.strings","NA"),stringsAsFactors=FALSE,verbose=getOption("datatable.verbose"),skip="__auto__",select=NULL,drop=NULL,colClasses=NULL,integer64=getOption("datatable.integer64"), col.names, check.names=FALSE, encoding="unknown", strip.white=TRUE, fill=FALSE, blank.lines.skip=FALSE, key=NULL, showProgress=interactive(),data.table=getOption("datatable.fread.datatable"),nThread=getDTthreads(),logical01=TRUE,autostart=NA)
{
  if (is.null(sep)) sep="\n"         # C level knows that \n means \r\n on Windows, for example
  else {
    stopifnot( length(sep)==1L, !is.na(sep), is.character(sep) )
    if (sep=="") sep="\n"             # meaning readLines behaviour. The 3 values (NULL, "" or "\n") are equivalent.
    else if (sep=="auto") sep=""      # sep=="" at C level means auto sep
    else stopifnot( nchar(sep)==1L )  # otherwise an actual character to use as sep
  }
  stopifnot( is.character(dec), length(dec)==1L, nchar(dec)==1L )
  # handle encoding, #563
  if (length(encoding) != 1L || !encoding %in% c("unknown", "UTF-8", "Latin-1")) {
    stop("Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.")
  }
  isTrueFalse = function(x) isTRUE(x) || identical(FALSE, x)
  isTrueFalseNA = function(x) isTRUE(x) || identical(FALSE, x) || identical(NA, x)
  stopifnot( isTrueFalse(strip.white), isTrueFalse(blank.lines.skip), isTrueFalse(fill), isTrueFalse(showProgress),
             isTrueFalse(stringsAsFactors), isTrueFalse(verbose), isTrueFalse(check.names), isTrueFalse(logical01) )
  stopifnot( is.numeric(nrows), length(nrows)==1L )
  if (is.na(nrows) || nrows<0) nrows=Inf   # accept -1 to mean Inf, as read.table does
  if (identical(header,"auto")) header=NA
  stopifnot(isTrueFalseNA(header))
  stopifnot(is.numeric(nThread) && length(nThread)==1L)
  nThread=as.integer(nThread)
  stopifnot(nThread>=1L)
  if (!missing(file)) {
    if (!identical(input, "")) stop("You can provide 'input=' or 'file=', not both.")
    if (!file.exists(file)) stop("File '",file,"' does not exist.")
    if (isTRUE(file.info(file)$isdir)) stop("File '",file,"' is a directory. Not yet implemented.") # dir.exists() requires R v3.2+, #989
    input = file
  } else {
    if (!is.character(input) || length(input)!=1L) {
      stop("'input' must be a single character string containing a file name, a system command containing at least one space, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or, the input data itself containing at least one \\n or \\r")
    }
    if ( input == "" || length(grep('\\n|\\r', input)) ) {
      # input is data itself containing at least one \n or \r
    } else if (file.exists(input)) {
      if (isTRUE(file.info(input)$isdir)) stop("File '",input,"' is a directory. Not yet implemented.")
    } else {
      if (substring(input,1L,1L)==" ") {
        stop("Input argument is not a file name and contains no \\n or \\r, but starts with a space. Please remove the leading space.")
      }
      # either a download or a system command, both to temp file
      tmpFile = tempfile()
      on.exit(unlink(tmpFile), add=TRUE)
      str6 = substring(input,1,6)   # avoid grepl() for #2531
      str7 = substring(input,1,7)
      str8 = substring(input,1,8)
      if (str7=="ftps://" || str8=="https://") {
        if (!requireNamespace("curl", quietly = TRUE))
            stop("Input URL requires https:// connection for which fread() requires 'curl' package, but cannot be found. Please install curl using 'install.packages('curl')'.")
        curl::curl_download(input, tmpFile, mode="wb", quiet = !showProgress)
      }
      else if (str6=="ftp://" || str7== "http://" || str7=="file://") {
        method = if (str7=="file://") "auto" else getOption("download.file.method", default="auto")
        # force "auto" when file:// to ensure we don't use an invalid option (e.g. wget), #1668
        download.file(input, tmpFile, method=method, mode="wb", quiet=!showProgress)
        # In text mode on Windows-only, R doubles up \r to make \r\r\n line endings. mode="wb" avoids that. See ?connections:"CRLF"
      }
      else if (length(grep(' ', input))) {
        (if (.Platform$OS.type == "unix") system else shell)(paste('(', input, ') > ', tmpFile, sep=""))
      }
      else stop("File '",input,"' does not exist; getwd()=='", getwd(), "'",
                ". Include correct full path, or one or more spaces to consider the input a system command.")
      input = tmpFile  # the file name
    }
  }
  if (!missing(autostart)) warning("'autostart' is now deprecated and ignored. Consider skip='string' or skip=n");
  if (is.logical(colClasses)) {
    if (!all(is.na(colClasses))) stop("colClasses is type 'logical' which is ok if all NA but it has some TRUE or FALSE values in it which is not allowed. Please consider the drop= or select= argument instead. See ?fread.")
    colClasses = NULL
  }
  if (!is.null(colClasses) && is.atomic(colClasses)) {
    if (!is.character(colClasses)) stop("colClasses is not type list or character vector")
    if (!length(colClasses)) stop("colClasses is character vector ok but has 0 length")
    if (!is.null(names(colClasses))) {   # names are column names; convert to list approach
      colClasses = tapply(names(colClasses), colClasses, c, simplify=FALSE)
    }
  }
  stopifnot(length(skip)==1L, !is.na(skip), is.character(skip) || is.numeric(skip))
  if (skip=="__auto__") skip=-1L   # skip="string" so long as "string" is not "__auto__". Best conveys to user something is automatic there (than -1 or NA).
  if (is.double(skip)) skip = as.integer(skip)
  warnings2errors = getOption("warn") >= 2
  ans = .Call(CfreadR,input,sep,dec,quote,header,nrows,skip,na.strings,strip.white,blank.lines.skip,
              fill,showProgress,nThread,verbose,warnings2errors,logical01,select,drop,colClasses,integer64,encoding)
  nr = length(ans[[1L]])
  if ((!"bit64" %chin% loadedNamespaces()) && any(sapply(ans,inherits,"integer64"))) require_bit64()
  setattr(ans,"row.names",.set_row_names(nr))

  if (isTRUE(data.table)) {
    setattr(ans, "class", c("data.table", "data.frame"))
    alloc.col(ans)
  } else {
    setattr(ans, "class", "data.frame")
  }
  # #1027, make.unique -> make.names as spotted by @DavidArenberg
  if (check.names) {
    setattr(ans, 'names', make.names(names(ans), unique=TRUE))
  }
  cols = NULL
  if (stringsAsFactors)
    cols = which(vapply(ans, is.character, TRUE))
  else if (length(colClasses)) {
    if (is.list(colClasses) && "factor" %in% names(colClasses))
      cols = colClasses[["factor"]]
    else if (is.character(colClasses) && "factor" %chin% colClasses)
      cols = which(colClasses=="factor")
  }
  setfactor(ans, cols, verbose)
  # 2007: is.missing is not correct since default value of select is NULL
  if (!is.null(select)) {
    # fix for #1445
    if (is.numeric(select)) {
      reorder = if (length(o <- forderv(select))) o else seq_along(select)
    } else {
      reorder = select[select %chin% names(ans)]
      # any missing columns are warning about in fread.c and skipped
    }
    setcolorder(ans, reorder)
  }
  # FR #768
  if (!missing(col.names))
    setnames(ans, col.names) # setnames checks and errors automatically
  if (!is.null(key) && data.table) {
    if (!is.character(key))
      stop("key argument of data.table() must be character")
    if (length(key) == 1L) {
      key = strsplit(key, split = ",")[[1L]]
    }
    setkeyv(ans, key)
  }
  ans
}

# for internal use only. Used in `fread` and `data.table` for 'stringsAsFactors' argument
setfactor <- function(x, cols, verbose) {
  # simplified but faster version of `factor()` for internal use.
  as_factor <- function(x) {
    lev = forderv(x, retGrp = TRUE, na.last = NA)
    # get levels, also take care of all sorted condition
    lev = if (length(lev)) x[lev[attributes(lev)$starts]] else x[attributes(lev)$starts]
    ans = chmatch(x, lev)
    setattr(ans, 'levels', lev)
    setattr(ans, 'class', 'factor')
  }
  if (length(cols)) {
    if (verbose) cat("Converting column(s) [", paste(names(x)[cols], collapse = ", "), "] from 'char' to 'factor'\n", sep = "")
    for (j in cols) set(x, j = j, value = as_factor(.subset2(x, j)))
  }
  invisible(x)
}
