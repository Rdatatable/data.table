
read = function(fnam="test.csv",sep="auto",sep2="auto",header="auto",na.strings=NULL,stringsAsFactors=FALSE,verbose=TRUE) {
    if (!is.character(fnam) || length(fnam)!=1) stop("fnam must be a single character string containing a file name, full path to a file, or URL starting 'http://', 'https://' or 'file://'")
    header=TRUE
    if (substring(fnam,1,7) %chin% c("http://","https:/","file://")) {
        tt = tempfile()
        on.exit(unlink(tt), add=TRUE)
        download.file(fnam, tt)
        fnam = tt
    }
    .Call(Cfinddelim,fnam)
    h = read.table(fnam,header=header,sep=",",stringsAsFactors=FALSE,nrows=10)
    type = sapply(h,typeof)
    f = c("integer"="%d","double"="%lg","character"="%[^,\n]")[type]
    f = paste(paste(f,collapse=","),sep="")
    types = lapply(sapply(h,typeof,USE.NAMES=FALSE),vector,length=0L)
    
    h = sapply(readLines(fnam,n=11),nchar)   # This could be even faster version of wc -l in C just for top 10 rows
    if (header) {
        hrowsize = h[1]+1
        hsize = sum(h[2:11]) + 10
    } else {
        hrowsize = 0
        hsize = sum(h[1:10]) + 10
    }
    estn = as.integer(1.05 * 10 * (file.info(fnam)$size - hrowsize) / hsize)
    if (verbose) {
        cat("Allocating nrows =",estn,"\n")
        cat("Format =",gsub("[\n]","\\\\n",f),"\n")
    }
    ans = .Call(Creadfile,fnam,f,types,as.integer(header),estn,verbose)
    nr = length(ans[[1]])
    setattr(ans,"row.names",.set_row_names(nr))
    setattr(ans,"class",c("data.table","data.frame"))
    setattr(ans,"names",paste("V",1:length(ans),sep=""))
    
    # if (read any integer64, then require(bit64) else cat("integer64 have been read, you'll need bit64 to make those columns appear correctly, which isn't installed")
    ans
}


