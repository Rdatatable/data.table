
read = function(fnam="test.csv",sep="auto",sep2="auto",header="auto",na.strings=NULL,stringsAsFactors=FALSE,verbose=TRUE) {
    if (!is.character(fnam) || length(fnam)!=1) stop("fnam must be a single character string containing a file name, full path to a file, or URL starting 'http://', 'https://' or 'file://'")
    if (substring(fnam,1,7) %chin% c("http://","https:/","file://")) {
        tt = tempfile()
        on.exit(unlink(tt), add=TRUE)
        download.file(fnam, tt)
        fnam = tt
    }
    if (identical(header,"auto")) header=NA
    else if (!identical(header,TRUE) && !identical(header,FALSE)) stop("'header' must be 'auto', TRUE or FALSE")
    ans = .Call(Creadfile,fnam,header,verbose,file.info(fnam)$size)
    nr = length(ans[[1]])
    setattr(ans,"row.names",.set_row_names(nr))
    setattr(ans,"class",c("data.table","data.frame"))
    setattr(ans,"names",paste("V",1:length(ans),sep=""))
    # if (read any integer64, then require(bit64) else cat("integer64 have been read, you'll need bit64 to make those columns appear correctly, which isn't installed")
    ans
}


