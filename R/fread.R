
fread = function(fnam="test.csv",sep="auto",sep2="auto",nrows=-1,header="auto",na.strings="NA",stringsAsFactors=FALSE,verbose=FALSE) {
    if (!is.character(fnam) || length(fnam)!=1) stop("fnam must be a single character string containing a file name, full path to a file, or URL starting 'http://', 'https://' or 'file://'")
    if (substring(fnam,1,7) %chin% c("http://","https:/","file://")) {
        tt = tempfile()
        on.exit(unlink(tt), add=TRUE)
        download.file(fnam, tt)
        fnam = tt
    } else fnam = path.expand(fnam)
    if (identical(header,"auto")) header=NA
    else if (!identical(header,TRUE) && !identical(header,FALSE)) stop("'header' must be 'auto', TRUE or FALSE")
    if (!is.null(na.strings) && !is.character(na.strings)) stop("'na.strings' is type '",typeof(na.strings),"'. Must be a character vector.")
    if (!is.numeric(nrows) || length(nrows)!=1) stop("'nrows' must be a length 1 vector of type numeric or integer")
    ans = .Call(Creadfile,fnam,as.integer(nrows),header,na.strings,verbose,file.info(fnam)$size)
    nr = length(ans[[1]])
    setattr(ans,"row.names",.set_row_names(nr))
    setattr(ans,"class",c("data.table","data.frame"))
    if ( !exists("print.integer64") && any(sapply(ans,inherits,"integer64")) )
        warning("Some columns have been loaded as type 'integer64' but package bit64 isn't loaded. Those columns will display as strange looking floating point data. There is no need to reload the data. Just require(bit64) to obtain the integer64 print method and print the data again.")
    alloc.col(ans)
}


