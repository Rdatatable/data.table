
read = function(fnam="test.csv",primary=",") {   #,secondary=" ")
    ans = .Call("readfile",fnam,PACKAGE="data.table")
    nr = length(ans[[1]])
    setattr(ans,"row.names",.set_row_names(nr))
    setattr(ans,"class",c("data.table","data.frame"))
    names(ans) = paste("V",1:length(ans),sep="")
    ans
}

if (FALSE) {
# test speedup so far
n=1e7
test = data.table(a=sample(1:1000,n,replace=TRUE), b=sample(1:1000,n,replace=TRUE))
write.table(test,"test.csv",sep=",",row.names=FALSE,col.names=FALSE)

system.time(xx <- read.csv("test.csv",header=FALSE))
system.time(yy <- read("test.csv"))
identical(yy$V1,xx$V1)  # TRUE
identical(yy$V2,xx$V2)  # TRUE

# with n=1e6 I got :
#> system.time(xx <- read.csv("test.csv",header=FALSE))
#   user  system elapsed 
#  5.388   0.092   5.486 
#> system.time(yy <- read("test.csv"))
#   user  system elapsed 
#  1.276   0.052   1.329    
#
# That's only a 7.5MB file on disk, so perhaps a larger file, and more
# columns (because that would be no more calls to fscanf), would
# be a greater speedup. Especially the BED format (columns 11 and 12)
# seems well matched for data.table's features with list() columns.
# Also see comments in readfile.c
}

# TO DO: 
#    use read.table on first 10 rows to get column names and types
#    estimate number of rows using file size and info from first 10 rows
#    allocate estimated rows + 5% in C, once. Even a memcpy afterwards
#    to the precise number of rows would be faster than incrementally
#    allocated memory as the scan progresses, or just leave the vectors
#    slightly over allocated until they are copied anyway by something else.
#    add the secondary separator for list() columns with vector cell
#    allocation in C. Really, not difficult at all.

