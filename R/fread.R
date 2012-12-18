
fread = function(input="test.csv",sep="auto",sep2="auto",nrows=-1,header="auto",na.strings="NA",stringsAsFactors=FALSE,verbose=FALSE,autostart=30) {
    if (!is.character(input) || length(input)!=1) stop("'input' must be a single character string containing a file name, full path to a file, a URL starting 'http://' or 'file://', or the input data itself")
    if (substring(input,1,7) %chin% c("http://","https:/","file://")) {
        tt = tempfile()
        on.exit(unlink(tt), add=TRUE)
        download.file(input, tt)
        input = tt
    } else input = path.expand(input)
    if (identical(header,"auto")) header=NA
    else if (!identical(header,TRUE) && !identical(header,FALSE)) stop("'header' must be 'auto', TRUE or FALSE")
    if (!is.null(na.strings) && !is.character(na.strings)) stop("'na.strings' is type '",typeof(na.strings),"'. Must be a character vector.")
    if (!is.numeric(nrows) || length(nrows)!=1) stop("'nrows' must be a length 1 vector of type numeric or integer")
    if (!is.numeric(autostart) || length(autostart)!=1 || autostart<1) stop("'autostart' must be a length 1 vector of type numeric or integer, >=1")
    ans = .Call(Creadfile,input,as.integer(nrows),header,na.strings,verbose,as.integer(autostart))
    nr = length(ans[[1]])
    setattr(ans,"row.names",.set_row_names(nr))
    setattr(ans,"class",c("data.table","data.frame"))
    if ( !exists("print.integer64") && any(sapply(ans,inherits,"integer64")) )
        warning("Some columns have been loaded as type 'integer64' but package bit64 isn't loaded. Those columns will display as strange looking floating point data. There is no need to reload the data. Just require(bit64) to obtain the integer64 print method and print the data again.")
    alloc.col(ans)
}


