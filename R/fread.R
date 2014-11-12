
fread <- function(input="",sep="auto",sep2="auto",nrows=-1L,header="auto",na.strings="NA",stringsAsFactors=FALSE,verbose=getOption("datatable.verbose"),autostart=1L,skip=0L,select=NULL,drop=NULL,colClasses=NULL,integer64=getOption("datatable.integer64"),dec=".",showProgress=getOption("datatable.showProgress"),data.table=getOption("datatable.fread.datatable")) {
    if (!is.character(dec) || length(dec)!=1L || nchar(dec)!=1) stop("dec must be a single character e.g. '.' or ','")
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
                    warning(cmd, ' returned ""; i.e., this locale name is not valid on your system. It was provided by you in getOption("datatable.fread.dec.locale"). See ?Sys.setlocale and ?fread.')
                    next
                }
                if (toupper(tt)!=toupper(i)) {
                    warning(cmd, " returned '",tt,"' != '",i,"' (not NULL not '' and allowing for case differences). This may not be a problem but please report.")
                } 
                if (Sys.localeconv()["decimal_point"] == dec) break
                if (verbose) cat("Successfully changed locale but it provides dec='",Sys.localeconv()["decimal_point"],"' not the desired dec", sep="")
            }
        }
        if (Sys.localeconv()["decimal_point"] != dec)
            (if (sep==".") warning else stop)(if(verbose)'' else 'Run again with verbose=TRUE to inspect... ','Unable to change to a locale which provides the desired dec. You will need to add a valid locale name to getOption("datatable.fread.dec.locale"). See the long paragraph in ?fread.')
            # the warning/stop switch is for #502
        if (verbose) cat("This R session's locale is now '",tt,"' which provides the desired decimal point for reading numerics in the file - success! The locale will be restored to what it was ('",oldlocale,") even if the function fails for other reasons.\n")
    }
    if (!is.character(input) || length(input)!=1) {
        stop("'input' must be a single character string containing a file name, a command, full path to a file, a URL starting 'http://' or 'file://', or the input data itself")
    } else if (substring(input,1,7) %chin% c("http://","https:/","file://")) {
        tt = tempfile()
        on.exit(unlink(tt), add=TRUE)
        download.file(input, tt, mode="wb", quiet=!showProgress)
        # In text mode on Windows-only, R doubles up \r to make \r\r\n line endings. mode="wb" avoids that. See ?connections:"CRLF"
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
    ans = .Call(Creadfile,input,sep,as.integer(nrows),header,na.strings,verbose,as.integer(autostart),skip,select,drop,colClasses,integer64,dec,as.integer(showProgress))
    nr = length(ans[[1]])
    if ( integer64=="integer64" && !exists("print.integer64") && any(sapply(ans,inherits,"integer64")) )
        warning("Some columns have been read as type 'integer64' but package bit64 isn't loaded. Those columns will display as strange looking floating point data. There is no need to reload the data. Just require(bit64) to obtain the integer64 print method and print the data again.")
    setattr(ans,"row.names",.set_row_names(nr))
    if (isTRUE(data.table)) {
        setattr(ans,"class",c("data.table","data.frame"))
        return(alloc.col(ans))
    } else {
        setattr(ans,"class","data.frame")
        return(ans)
    }
}


