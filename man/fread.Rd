\name{fread}
\alias{fread}
\title{ Fast and friendly file finagler }
\description{
   Similar to \code{read.table} but much faster and more convenient. All controls such as delimiter, colClasses and nrows are automatically detected. Dual-delimited files such as BED are automatically detected: columns 11 and 12 are each read directly into list columns where each cell is itself an integer vector (no need for strsplit). bit64::integer64 types are also detected and read directly without needing to read as character then convert.
   
   This help file is in development and may document features that haven't been implemented yet.
}
\usage{
fread(fnam, sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE, verbose=FALSE)
}
\arguments{
  \item{fnam}{ The file name to read, as a length 1 character string or a URL starting http:// or file://. }
  \item{sep}{ The separator between columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n]. }
  \item{sep2}{ The separator \emph{within} columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n<sep>]. }
  \item{nrows}{ The number of rows to read, by default -1 means all. Unlike \code{read.table}, it doesn't help speed to set this to the number of rows in the file (or an estimate), since the number of rows is automatically estimated and is already fast. Only set \code{nrows} if you require the first 10 rows, for example, or are reading the file in chunks. }
  \item{header}{ Does the first line contain column names? Defaults according to a comparison of field types in rows 1 vs 2.}
  \item{na.strings}{ A character vector of strings to convert to \code{NA_character_}. By default, ",," is read as a blank string (\code{""}) and ",NA," is read as \code{NA_character_}. Typical values might be \code{na.strings=NULL} or perhaps \code{na.strings=c("NA","N/A","")}. }
  \item{stringsAsFactors}{ Convert all character columns to factors? }
  \item{verbose}{ Be chatty? }
}
\details{

character columns can be single quoted (') or double quoted ("), or not quoted at all. Spaces and other whitepace (anything other than sep and newline) may appear in an unquoted character field, provided the field doesn't contain sep itself. Therefore quoting character values is only required if sep itself appears in the string value. Quoting may also be used to signify that numeric data should be read as text (or the column type can be overriden using colClasses). This is automatically detected and no arguments are needed to control it. If a character value starts " it should end with "; it may then include sep and '. If a character value starts ' it should end with '; it may then include sep and ". Just like the R parser. This is on a per field basis; i.e., an entire column need not follow consistent quoting. Just a few cells (perhaps just those that contain sep) may only be quoted.

There is no line length limit, not even a very large one. Since we are encouraging \code{list} columns (i.e. using \code{sep2}), this has the potential to encourage longer line lengths. So the approach of scanning each line into a buffer first and then rescanning that buffer is not used. A single pass approach is used.

The filename extension (such as .csv) is irrelevant for "auto" \code{sep} and \code{sep2}. Separator detection is entirely driven by the file contents. This can be useful when loading a large set of different files, which may not be named consistently, or may not have the extension .csv despite being csv. Some datasets have been collected over many years, one file per day for example. Sometimes the file name format has changed at some point in the past or even the format of the file itself. So the idea is that you can loop \code{read} through a set of files and as long as each file is regular and delimited, \code{read} can read them all.

There is no need for a \code{skip} argument. Once the format and number of fields is detected from lines 30-39, the first row in the file containing a conformant number of fields is deemed to be the header row. Thus, human readable banner rows are automatically skipped. This feature can be useful if you are loading many files which may not all have consistently sized banners.

These features are for fostering friendliness. Facilitated by a fair farthingsworth of (far from fatuitous) finagling. The fread function's first letter is f; for fast, friendly, file or finagle.

}
\value{
    A data.table.
}
\references{
Background :\cr
\url{http://cran.r-project.org/doc/manuals/R-data.html}\cr
\url{http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r}\cr
\url{www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html}\cr
\url{https://stat.ethz.ch/pipermail/r-help/2007-August/138315.html}\cr
\url{http://www.cerebralmastication.com/2009/11/loading-big-data-into-r/}\cr
\url{http://stackoverflow.com/questions/9061736/faster-than-scan-with-rcpp}\cr
\url{http://stackoverflow.com/questions/415515/how-can-i-read-and-manipulate-csv-file-data-in-c}\cr
\url{http://stackoverflow.com/questions/9352887/strategies-for-reading-in-csv-files-in-pieces}\cr
\url{http://stackoverflow.com/questions/11782084/reading-in-large-text-files-in-r}

finagler = "to get or achieve by guile or manipulation" \url{http://dictionary.reference.com/browse/finagler}
}
\seealso{ \code{\link[utils]{read.csv}}, \code{\link[base]{url}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
\dontrun{

# Demo speedup
n=1e6
DT = data.table( a=sample(1:1000,n,replace=TRUE),
                 b=sample(1:1000,n,replace=TRUE),
                 c=rnorm(n),
                 d=sample(c("foo","bar","baz","qux","quux"),n,replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000,n,replace=TRUE) )
DT[2,b:=NA_integer_]
DT[4,c:=NA_real_]
DT[3,d:=NA_character_]
DT[5,d:=""]
DT[2,e:=+Inf]
DT[3,e:=-Inf]

write.table(DT,"test.csv",sep=",",row.names=FALSE,quote=FALSE)
cat("File size (MB):",round(file.info("test.csv")$size/1024^2),"\n")    # 50 MB (1e6 rows x 6 columns)

system.time(DF1 <- read.csv("test.csv",stringsAsFactors=FALSE))         # 60 sec (first time in fresh R session)
system.time(DF1 <- read.csv("test.csv",stringsAsFactors=FALSE))         # 30 sec (immediate repeat is faster, varies)

system.time(DF2 <- read.table("test.csv",header=TRUE,sep=",",quote="",  # 10 sec (consistently)
    stringsAsFactors=FALSE,comment.char="",nrows=n,                     # ( All known tricks and known
    colClasses=c("integer","integer","numeric",                         #   nrows, see references )
                 "character","numeric","integer")))

require(data.table)
system.time(DT <- fread("test.csv"))                                    #  5 sec (faster and friendlier)

require(sqldf)
system.time(SQLDF <- read.csv.sql("test.csv",dbname=NULL))              # 20 sec (friendly too, good defaults)

require(ff)
system.time(FFDF <- read.csv.ffdf(file="test.csv",nrows=n))             # 20 sec (friendly too, good defaults)

identical(DF1,DF2)                                                      # TRUE
all.equal(as.data.table(DF1), DT)                                       # TRUE
identical(DF1,within(SQLDF,{b<-as.integer(b);c<-as.numeric(c)}))        # TRUE
identical(DF1,within(as.data.frame(FFDF),d<-as.character(d)))           # TRUE

# Scaling up ...
l = vector("list",10)
for (i in 1:10) l[[i]] = DT
DTbig = rbindlist(l)
tables()
write.table(DTbig,"testbig.csv",sep=",",row.names=FALSE,quote=FALSE)    # 500MB (10 million rows x 6 columns)

system.time(DF <- read.table("testbig.csv",header=TRUE,sep=",",         # 100-200 sec (varies)  
    quote="",stringsAsFactors=FALSE,comment.char="",nrows=1e7,                     
    colClasses=c("integer","integer","numeric",
                 "character","numeric","integer")))
                 
system.time(DT <- fread("testbig.csv"))                                 # 50 sec (consistently)
all(mapply(all.equal, DF, DT))                                          # TRUE


# Real data example (Airline data)
# http://stat-computing.org/dataexpo/2009/the-data.html

download.file("http://stat-computing.org/dataexpo/2009/2008.csv.bz2",
              destfile="2008.csv.bz2")                                  # 109MB (compressed)
system("bunzip2 2008.csv.bz2")                                          # 658MB (7,009,729 rows x 29 columns)
system.time(DT <- fread("2008.csv"))                                    # 90 secs
table(sapply(DT,class))                                                 # 4 character and 25 integer columns


# Reads URLs directly :
fread("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")

# Reads text input directly (TO DO) :
# read("...")

}
}
\keyword{ data }

