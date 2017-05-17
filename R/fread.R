
fread <- function(input="",file,sep="auto",sep2="auto",dec=".",quote="\"",nrows=Inf,header="auto",na.strings="NA",stringsAsFactors=FALSE,verbose=getOption("datatable.verbose"),autostart=NA,skip=0,select=NULL,drop=NULL,colClasses=NULL,integer64=getOption("datatable.integer64"), col.names, check.names=FALSE, encoding="unknown", strip.white=TRUE, fill=FALSE, blank.lines.skip=FALSE, key=NULL, showProgress=interactive(),data.table=getOption("datatable.fread.datatable"),nThread=getDTthreads())
{
    stopifnot( is.character(sep), length(sep)==1, sep=="auto" || nchar(sep)==1 )
    if (sep == "auto") sep=""
    stopifnot( is.character(dec), length(dec)==1, nchar(dec)==1 )
    # handle encoding, #563
    if (length(encoding) != 1L || !encoding %in% c("unknown", "UTF-8", "Latin-1")) {
        stop("Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.")
    }
    isTrueFalse = function(x) isTRUE(x) || identical(FALSE, x)
    isTrueFalseNA = function(x) isTRUE(x) || identical(FALSE, x) || identical(NA, x)
    stopifnot( isTrueFalse(strip.white), isTrueFalse(blank.lines.skip), isTrueFalse(fill), isTrueFalse(showProgress),
               isTrueFalse(stringsAsFactors), isTrueFalse(verbose), isTrueFalse(check.names) )
    stopifnot( is.numeric(nrows), length(nrows)==1 )
    if (is.na(nrows) || nrows<0) nrows=Inf   # accept -1 to mean Inf, as read.table does
    if (identical(header,"auto")) header=NA
    stopifnot(isTrueFalseNA(header))
    stopifnot(length(skip)==1)
    stopifnot(is.numeric(nThread) && length(nThread)==1)
    nThread=as.integer(nThread)
    stopifnot(nThread>=1)
    if (!missing(file)) {
        if (!identical(input, "")) stop("You can provide 'input' or 'file', not both.")
        if (!file.exists(file)) stop(sprintf("Provided file '%s' does not exists.", file))
        input = file
    }
    if (!missing(autostart)) warning("'autostart' is now deprecated and ignored. Consider skip='string' or skip=n");
    is_url <- function(x) grepl("^(http|ftp)s?://", x)
    is_secureurl <- function(x) grepl("^(http|ftp)s://", x)
    is_file <- function(x) grepl("^file://", x)
    if (!is.character(input) || length(input)!=1L) {
        stop("'input' must be a single character string containing a file name, a command, full path to a file, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or the input data itself")
    } else if (is_url(input) || is_file(input)) {
        tt = tempfile()
        on.exit(unlink(tt), add = TRUE)
        # In text mode on Windows-only, R doubles up \r to make \r\r\n line endings. mode="wb" avoids that. See ?connections:"CRLF"
        if (!is_secureurl(input)) {
            #1668 - force "auto" when is_file to
            #  ensure we don't use an invalid option, e.g. wget
            method <- if (is_file(input)) "auto" else
                getOption("download.file.method", default = "auto")
            download.file(input, tt, method = method,
                          mode = "wb", quiet = !showProgress)
        } else {
            if (!requireNamespace("curl", quietly = TRUE))
                stop("Input URL requires https:// connection for which fread() requires 'curl' package, but cannot be found. Please install the package using 'install.packages()'.")
            curl::curl_download(input, tt, mode = "wb", quiet = !showProgress)
        }
        input = tt
    } else if (input == "" || length(grep('\\n|\\r', input)) > 0) {
        # text input
    } else if (isTRUE(file.info(input)$isdir)) { # fix for #989, dir.exists() requires v3.2+
        stop("'input' can not be a directory name, but must be a single character string containing a file name, a command, full path to a file, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or the input data itself.")
    } else if (!file.exists(input)) {
        if (!length(grep(' ', input))) {
            stop("File '",input,"' does not exist; getwd()=='", getwd(), "'",
              ". Include correct full path, or one or more spaces to consider the input a system command.")
        }
        tt = tempfile()
        on.exit(unlink(tt), add = TRUE)
        if (.Platform$OS.type == "unix") {
            if (file.exists('/dev/shm') && file.info('/dev/shm')$isdir) {
                tt = tempfile(tmpdir = '/dev/shm')
            }
            system(paste('(', input, ') > ', tt, sep=""))
        } else {
            shell(paste('(', input, ') > ', tt, sep=""))
        }
        input = tt
    }
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
    if (is.numeric(skip)) skip = as.integer(skip)
    warnings2errors = getOption("warn") >= 2
    ans = .Call(CfreadR,input,sep,dec,quote,header,nrows,skip,na.strings,strip.white,blank.lines.skip,
                        fill,showProgress,nThread,verbose,warnings2errors,select,drop,colClasses,integer64,encoding)
    nr = length(ans[[1]])
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
    if (!missing(select)) {
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
