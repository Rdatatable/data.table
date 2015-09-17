
fread <- function(input="",sep="auto",sep2="auto",nrows=-1L,header="auto",na.strings="NA",stringsAsFactors=FALSE,verbose=getOption("datatable.verbose"),autostart=1L,skip=0L,select=NULL,drop=NULL,colClasses=NULL,integer64=getOption("datatable.integer64"),dec=if (sep!=".") "." else ",", col.names, check.names=FALSE, encoding="unknown", strip.white=TRUE, showProgress=getOption("datatable.showProgress"),data.table=getOption("datatable.fread.datatable")) {
    if (!is.character(dec) || length(dec)!=1L || nchar(dec)!=1) stop("dec must be a single character e.g. '.' or ','")
    # handle encoding, #563
    if (!encoding %in% c("unknown", "UTF-8", "Latin-1")) {
        stop("Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.")
    }
    if (!strip.white %in% c(TRUE, FALSE)) {
        stop("Argument 'strip.white' must be logical TRUE/FALSE")
    }
    if (getOption("datatable.fread.dec.experiment") && Sys.localeconv()["decimal_point"] != dec) {
        oldlocale = Sys.getlocale("LC_NUMERIC")
        if (verbose) cat("dec='",dec,"' but current locale ('",oldlocale,"') has dec='",Sys.localeconv()["decimal_point"],"'. Attempting to change locale to one that has the desired decimal point.\n",sep="")
        on.exit(Sys.setlocale("LC_NUMERIC", oldlocale))
        if (dec==".") {
            tt <- Sys.setlocale("LC_NUMERIC", "C")
            if (!identical(tt,"C")) stop("It is supposed to be guaranteed that Sys.setlocale('LC_NUMERIC', 'C') will always work!")
        }
        else suppressWarnings(tt <- Sys.setlocale("LC_NUMERIC", ""))   # hope get lucky with system locale; i.e. France in France
        if (Sys.localeconv()["decimal_point"] != dec) {
            if (verbose) cat("Changing to system locale ('",tt,"') did not provide the desired dec. Now trying any provided in getOption('datatable.fread.dec.locale')\n", sep="")
            for (i in getOption("datatable.fread.dec.locale")) {
                if (i=="") {
                    if (verbose) cat("Ignoring ''\n")
                    next
                } else {
                    if (verbose) cat("Trying '",i,"'\n", sep="")
                }
                suppressWarnings(tt <- Sys.setlocale("LC_NUMERIC", i))
                cmd = paste("Sys.setlocale('LC_NUMERIC','",i,"')",sep="")
                if (is.null(tt)) stop(cmd," returned NULL (locale information is unavailable). See ?Sys.setlocale.")
                if (tt=="") {
                    if (verbose) cat(cmd, ' returned ""; i.e., this locale name is not valid on your system. It was provided by you in getOption("datatable.fread.dec.locale"). See ?Sys.setlocale and ?fread.')
                    next
                }
                if (toupper(tt)!=toupper(i)) {
                    warning(cmd, " returned '",tt,"' != '",i,"' (not NULL not '' and allowing for case differences). This may not be a problem but please report.")
                } 
                if (Sys.localeconv()["decimal_point"] == dec) break
                if (verbose) cat("Successfully changed locale but it provides dec='",Sys.localeconv()["decimal_point"],"' not the desired dec", sep="")
            }
        }
        if (Sys.localeconv()["decimal_point"] != dec) {
            stop('Unable to change to a locale which provides the desired dec. You will need to add a valid locale name to getOption("datatable.fread.dec.locale"). See the long paragraph in ?fread.', if(verbose)'' else ' Run again with verbose=TRUE to inspect.')   # see issue #502
        }
        if (verbose) cat("This R session's locale is now '",tt,"' which provides the desired decimal point for reading numerics in the file - success! The locale will be restored to what it was ('",oldlocale,") even if the function fails for other reasons.\n")
    }

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
            download.file(input, tt, mode = "wb", quiet = !showProgress)
        } else {
            if (!requireNamespace("curl", quietly = TRUE))
                stop("Input URL requires https:// connection for which fread() requires 'curl' package, but cannot be found. Please install the package using 'install.packages()'.")
            curl::curl_download(input, tt, mode = "wb", quiet = !showProgress)
        }
        input = tt
    } else if (input == "" || length(grep('\\n|\\r', input)) > 0) {
        # text input
    } else if (!file.exists(input)) {
        if (length(grep(' ', input)) == 0) stop("File '",input,"' does not exist. Include one or more spaces to consider the input a system command.")
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
    if (identical(header,"auto")) header=NA
    if (identical(sep,"auto")) sep=NULL
    if (is.atomic(colClasses) && !is.null(names(colClasses))) colClasses = tapply(names(colClasses),colClasses,c,simplify=FALSE)
    ans = .Call(Creadfile,input,sep,as.integer(nrows),header,na.strings,verbose,as.integer(autostart),skip,select,drop,colClasses,integer64,dec,encoding,strip.white,as.integer(showProgress))
    nr = length(ans[[1]])
    if ( integer64=="integer64" && !exists("print.integer64") && any(sapply(ans,inherits,"integer64")) )
        warning("Some columns have been read as type 'integer64' but package bit64 isn't loaded. Those columns will display as strange looking floating point data. There is no need to reload the data. Just require(bit64) to obtain the integer64 print method and print the data again.")
    setattr(ans,"row.names",.set_row_names(nr))

    if (isTRUE(data.table)) {
        setattr(ans, "class", c("data.table", "data.frame"))
        alloc.col(ans)
    } else {
        setattr(ans, "class", "data.frame")
    }
    if (isTRUE(as.logical(check.names))) {
        setattr(ans, 'names', make.unique(names(ans)))
    }
    as_factor <- function(x) {
        lev = forderv(x, retGrp = TRUE)
        # get levels, also take care of all sorted condition
        if (length(lev)) lev = x[lev[attributes(lev)$starts]]
        else lev = x[attributes(lev)$starts]
        ans = chmatch(x, lev)
        setattr(ans, 'levels', lev)
        setattr(ans, 'class', 'factor')
    }
    if (isTRUE(as.logical(stringsAsFactors))) {
        cols = which(vapply(ans, is.character, TRUE))
        if (length(cols)) {
            if (verbose) cat("Converting column(s) [", paste(names(ans)[cols], collapse = ", "), "] from 'char' to 'factor'\n", sep = "")
            for (j in cols) 
                set(ans, j = j, value = as_factor(.subset2(ans, j)))
        }
    }
    # FR #768
    if (!missing(col.names))
        setnames(ans, col.names) # setnames checks and errors automatically
    ans
}
