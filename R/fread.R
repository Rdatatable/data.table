
fread = function(input="test.csv",sep="auto",sep2="auto",nrows=-1,header="auto",na.strings="NA",stringsAsFactors=FALSE,verbose=FALSE,autostart=30L,select=NULL,colClasses=NULL,integer64=getOption("datatable.integer64")) {
    if (!is.character(input) || length(input)!=1) stop("'input' must be a single character string containing a file name, full path to a file, a URL starting 'http://' or 'file://', or the input data itself")
    if (substring(input,1,7) %chin% c("http://","https:/","file://")) {
        tt = tempfile()
        on.exit(unlink(tt), add=TRUE)
        download.file(input, tt)
        input = tt
    } else input = path.expand(input)
    if (identical(header,"auto")) header=NA
    if (identical(sep,"auto")) sep=NULL
    if (is.atomic(colClasses) && !is.null(names(colClasses))) colClasses = tapply(names(colClasses),colClasses,c,simplify=FALSE)
    ans = .Call(Creadfile,input,sep,as.integer(nrows),header,na.strings,verbose,as.integer(autostart),select,colClasses,integer64)
    nr = length(ans[[1]])
    setattr(ans,"row.names",.set_row_names(nr))
    setattr(ans,"class",c("data.table","data.frame"))
    if ( integer64=="integer64" && !exists("print.integer64") && any(sapply(ans,inherits,"integer64")) )
        warning("Some columns have been read as type 'integer64' but package bit64 isn't loaded. Those columns will display as strange looking floating point data. There is no need to reload the data. Just require(bit64) to obtain the integer64 print method and print the data again.")
    alloc.col(ans)
}


