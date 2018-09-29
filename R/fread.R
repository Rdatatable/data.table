
fread <- function(input="",file=NULL,text=NULL,cmd=NULL,sep="auto",sep2="auto",dec=".",quote="\"",nrows=Inf,header="auto",na.strings=getOption("datatable.na.strings","NA"),stringsAsFactors=FALSE,verbose=getOption("datatable.verbose",FALSE),skip="__auto__",select=NULL,drop=NULL,colClasses=NULL,integer64=getOption("datatable.integer64","integer64"), col.names, check.names=FALSE, encoding="unknown", strip.white=TRUE, fill=FALSE, blank.lines.skip=FALSE, key=NULL, index=NULL, showProgress=getOption("datatable.showProgress",interactive()), data.table=getOption("datatable.fread.datatable",TRUE), nThread=getDTthreads(verbose), logical01=getOption("datatable.logical01", FALSE), autostart=NA)
{
  if (missing(input)+is.null(file)+is.null(text)+is.null(cmd) < 3L) stop("Used more than one of the arguments input=, file=, text= and cmd=.")
  input_has_vars = length(all.vars(substitute(input)))>0L  # see news for v1.11.6
  if (is.null(sep)) sep="\n"         # C level knows that \n means \r\n on Windows, for example
  else {
    stopifnot( length(sep)==1L, !is.na(sep), is.character(sep) )
    if (sep=="") { sep="\n" }         # meaning readLines behaviour. The 3 values (NULL, "" or "\n") are equivalent.
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
  if (!is.null(text)) {
    if (!is.character(text)) stop("'text=' is type ", typeof(text), " but must be character.")
    if (!length(text)) return(data.table())
    if (length(text) > 1L) {
      cat(text, file=(tmpFile<-tempfile()), sep="\n")  # avoid paste0() which could create a new very long single string in R's memory
      file = tmpFile
      on.exit(unlink(tmpFile), add=TRUE)
    } else {
      # avoid creating a tempfile() for single strings, which can be done a lot; e.g. in the test suite.
      input = text
    }
  }
  else if (is.null(cmd)) {
    if (!is.character(input) || length(input)!=1L) {
      stop("input= must be a single character string containing a file name, a system command containing at least one space, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or, the input data itself containing at least one \\n or \\r")
    }
    if (input=="" || length(grep('\\n|\\r', input))) {
      # input is data itself containing at least one \n or \r
    } else {
      if (substring(input,1L,1L)==" ") {
        stop("input= contains no \\n or \\r, but starts with a space. Please remove the leading space, or use text=, file= or cmd=")
      }
      str6 = substring(input,1L,6L)   # avoid grepl() for #2531
      str7 = substring(input,1L,7L)
      str8 = substring(input,1L,8L)
      ext2 = substring(input,nchar(input)-2L,nchar(input))   # last 3 characters ".gz"
      ext3 = substring(input,nchar(input)-3L,nchar(input))   # last 4 characters ".bz2"
      if (str7=="ftps://" || str8=="https://") {
        # nocov start
        if (!requireNamespace("curl", quietly = TRUE))
          stop("Input URL requires https:// connection for which fread() requires 'curl' package, but cannot be found. Please install curl using 'install.packages('curl')'.")
        curl::curl_download(input, tmpFile<-tempfile(), mode="wb", quiet = !showProgress)
        file = tmpFile
        on.exit(unlink(tmpFile), add=TRUE)
        # nocov end
      }
      else if (str6=="ftp://" || str7== "http://" || str7=="file://") {
        # nocov start
        method = if (str7=="file://") "internal" else getOption("download.file.method", default="auto")
        # force "auto" when file:// to ensure we don't use an invalid option (e.g. wget), #1668
        download.file(input, tmpFile<-tempfile(), method=method, mode="wb", quiet=!showProgress)
        # In text mode on Windows-only, R doubles up \r to make \r\r\n line endings. mode="wb" avoids that. See ?connections:"CRLF"
        file = tmpFile
        on.exit(unlink(tmpFile), add=TRUE)
        # nocov end
      }
      else if (ext2==".gz" || ext3==".bz2") {
        if (!requireNamespace("R.utils", quietly = TRUE))
          stop("To read gz and bz2 files directly, fread() requires 'R.utils' package, but cannot be found. Please install R.utils using 'install.packages('R.utils')'.")
        if (ext2==".gz") { ext="gz";  FUN=gzfile; }
        else             { ext="bz2"; FUN=bzfile; }
        R.utils::decompressFile(input, tmpFile<-tempfile(), ext=ext, FUN=FUN, remove=FALSE)
        file = tmpFile
        on.exit(unlink(tmpFile), add=TRUE)
      }
      else if (length(grep(' ', input)) && !file.exists(input)) {  # file name or path containing spaces is not a command
        cmd = input
        if (input_has_vars && getOption("datatable.fread.input.cmd.message", TRUE)) {
          message("Taking input= as a system command ('",cmd,"') and a variable has been used in the expression passed to `input=`. Please use fread(cmd=...). There is a security concern if you are creating an app, and the app could have a malicious user, and the app is not running in a secure envionment; e.g. the app is running as root. Please read item 5 in the NEWS file for v1.11.6 for more information and for the option to suppress this message.")
        }
      }
      else {
        file = input
      }
    }
  }
  if (!is.null(cmd)) {
    (if (.Platform$OS.type == "unix") system else shell)(paste0('(', cmd, ') > ', tmpFile<-tempfile()))
    file = tmpFile
    on.exit(unlink(tmpFile), add=TRUE)
  }
  if (!is.null(file)) {
    file_info = file.info(file)
    if (is.na(file_info$size)) stop("File '",file,"' does not exist or is non-readable. getwd()=='", getwd(), "'")
    if (isTRUE(file_info$isdir)) stop("File '",file,"' is a directory. Not yet implemented.") # dir.exists() requires R v3.2+, #989
    if (!file_info$size) {
      warning(sprintf("File '%s' has size 0. Returning a NULL %s.", file, if (data.table) 'data.table' else 'data.frame'))
      return(if (data.table) data.table(NULL) else data.frame(NULL))
    }
    input = file
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
  stopifnot(is.null(na.strings) || is.character(na.strings))
  tt = grep("^\\s+$", na.strings)
  if (length(tt)) {
    msg = paste0('na.strings[', tt[1L], ']=="',na.strings[tt[1L]],'" consists only of whitespace, ignoring. ')
    if (strip.white) {
      if (any(na.strings=="")) {
        warning(msg, 'strip.white==TRUE (default) and "" is present in na.strings, so any number of spaces in string columns will already be read as <NA>.')
      } else {
        warning(msg, 'Since strip.white=TRUE (default), use na.strings="" to specify that any number of spaces in a string column should be read as <NA>.')
      }
      na.strings = na.strings[-tt]
    } else {
      stop(msg, 'But strip.white=FALSE. Use strip.white=TRUE (default) together with na.strings="" to turn any number of spaces in string columns into <NA>')
    }
    # whitespace at the beginning or end of na.strings is checked at C level and is an error there; test 1804
  }
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
      reorder = frank(select)
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
      stop("key argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)")
    if (length(key) == 1L) {
      key = strsplit(key, split = ",", fixed = TRUE)[[1L]]
    }
    setkeyv(ans, key)
  }
  if (!is.null(index) && data.table) {
    if (!all(sapply(index, is.character)))
      stop("index argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)")
    if (is.list(index)) {
      to_split = sapply(index, length) == 1L
      if (any(to_split))
        index[to_split] = sapply(index[to_split], strsplit, split = ",", fixed = TRUE)
    } else {
      if (length(index) == 1L) {
        # setindexv accepts lists, so no [[1]]
        index = strsplit(index, split = ",", fixed = TRUE)
      }
    }
    setindexv(ans, index)
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
