
read = function(x="test.csv",primary=",",secondary="",header=TRUE,verbose=TRUE) {
    h = read.table(x,header=header,sep=",",stringsAsFactors=FALSE,nrows=10)
    type = sapply(h,typeof)
    f = c("integer"="%d","double"="%lf","character"="%[^,]")[type]
    if (last(type)=="character") f[length(f)] = "%[^,\n]"
    f = paste(paste(f,collapse=","),"\n",sep="")
    types = lapply(sapply(h,typeof,USE.NAMES=FALSE),vector,length=0L)
    
    h = sapply(readLines(x,n=11),nchar)   # This could be even faster version of wc -l in C just for top 10 rows
    if (header) {
        hrowsize = h[1]+1
        hsize = sum(h[2:11]) + 10
    } else {
        hrowsize = 0
        hsize = sum(h[1:10]) + 10
    }
    estn = 10 * (file.info(x)$size - hrowsize) / hsize
    if (verbose) {
        cat("Estimated nrows =",estn,"\n")
        cat("Format =",gsub("[\n][]]","\\\\n]",f))  # matching the ] is to exclude the final \n
    }
    ans = .Call(Creadfile,x,f,types,as.integer(header),as.integer(1.05*estn),verbose)
    nr = length(ans[[1]])
    setattr(ans,"row.names",.set_row_names(nr))
    setattr(ans,"class",c("data.table","data.frame"))
    setattr(ans,"names",paste("V",1:length(ans),sep=""))
    ans
}


