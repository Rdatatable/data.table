\name{fread}
\alias{fread}
\title{ Fast and friendly file finagler }
\description{
   Similar to \code{read.table} but much faster and more convenient. All controls such as delimiter, colClasses and nrows are automatically detected. Dual-delimited files such as BED are automatically detected: columns 11 and 12 are each read directly into list columns where each cell is itself an integer vector (no need for strsplit). bit64::integer64 types are also detected and read directly without needing to read as character then convert.
   
   This help file is in development and may document features that haven't been implemented yet.
}
\usage{
fread(input, sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE, verbose=FALSE, autostart=30)
}
\arguments{
  \item{input}{ Either the file name to read (containing no \\n character) or the input itself as a string (containing at least one \\n), see examples. In both cases, a length 1 character string. A filename may be a URL starting http:// or file://. }
  \item{sep}{ The separator between columns. Defaults to the first character on line \code{datastart} not in the set [a-zA-Z0-9"+-./\]. }
  \item{sep2}{ The separator \emph{within} columns. Defaults to the first character on line \code{datastart} not in the set [a-zA-Z0-9"+-./\<sep>]. }
  \item{nrows}{ The number of rows to read, by default -1 means all. Unlike \code{read.table}, it doesn't help speed to set this to the number of rows in the file (or an estimate), since the number of rows is automatically estimated and is already fast. Only set \code{nrows} if you require the first 10 rows, for example, or are reading the file in chunks. }
  \item{header}{ Does the first data line contain column names? Defaults according to a comparison of field types in data line 1 vs 2.}
  \item{na.strings}{ A character vector of strings to convert to \code{NA_character_}. By default, ",," is read as a blank string (\code{""}) and ",NA," is read as \code{NA_character_}. Typical values might be \code{na.strings=NULL} or perhaps \code{na.strings=c("NA","N/A","")}. }
  \item{stringsAsFactors}{ Convert all character columns to factors? }
  \item{verbose}{ Be chatty? }
  \item{autostart}{ Any line number within the region of machine readable delimited text, by default 30 (or the last line if the file is shorter). If this line is empty (e.g. short files with trailing blank lines) then the last non empty line before that is found. This line is then used to auto detect \code{sep}, \code{sep2}, the number of columns and guess at the column types (although more rows are used to make a better guess, see Details). We invisage it to be extremely unlikely that \code{autostart} should ever need to be changed. }
}
\details{

Once the separator is detected, the number of columns is determined. Then the file is searched backwards from \code{autostart} until a row is found that doesn't have that number of columns, or the start of file is reached. Thus, the first data row is found and any human readable banners are automatically skipped, if present. Rather like in Excel when you click a cell inside a rectangular region of populated cells with empty rows above, then select AutoFilter. The table region is determined by Excel searching outwards from that cell (in our case, line 30) to find the table edges. The time to do this is utterly negligible, but is reported by \code{verbose=TRUE}, just in case.

Once a good guess is made, the file may of course still contain data of a different type. In this case, the type is bumped and existing data coerced. Unlike read.table when colClasses is supplied; it creates NA in this case, silently.

character columns can be quoted ("), or not quoted at all. Spaces and other whitepace (anything other than sep and newline) may appear in an unquoted character field, provided the field doesn't contain sep itself. Therefore quoting character values is only required if sep itself appears in the string value. Quoting may also be used to signify that numeric data should be read as text (or the column type can be overriden using colClasses). This is automatically detected and no arguments are needed to control it. If a character value starts " it should end with "; it may then include sep and '. If a character value starts ' it should end with '; it may then include sep and ". Just like the R parser. This is on a per field basis; i.e., an entire column need not follow consistent quoting. Just a few cells (perhaps just those that contain sep) may only be quoted.

There is no line length limit, not even a very large one. Since we are encouraging \code{list} columns (i.e. using \code{sep2}), this has the potential to encourage longer line lengths. So the approach of scanning each line into a buffer first and then rescanning that buffer is not used. A single pass approach is used.

The filename extension (such as .csv) is irrelevant for "auto" \code{sep} and \code{sep2}. Separator detection is entirely driven by the file contents. This can be useful when loading a large set of different files, which may not be named consistently, or may not have the extension .csv despite being csv. Some datasets have been collected over many years, one file per day for example. Sometimes the file name format has changed at some point in the past or even the format of the file itself. So the idea is that you can loop \code{read} through a set of files and as long as each file is regular and delimited, \code{read} can read them all.

All known line endings are detected automatically: \\n (*NIX including Mac), \\r\\n (Windows CRLF), \\r (old Mac) and \\n\\r (just in case). There is no need to convert input files first: \code{fread} running on any architecture will read a file from any architecture. Both \\r and \\n may be embedded in character strings (including column names) provided the field is quoted.

There is no need for a \code{skip} argument. Once the format and number of fields is detected from lines 30-39, the first row in the file containing a conformant number of fields is deemed to be the header row. Thus, human readable banner rows are automatically skipped. This feature can be useful if you are loading many files which may not all have consistently sized banners.

Finally, these few features are for fostering friendliness. Facilitated by a fair farthingsworth of (far from flaky, flawed
or fatuous) finagling. Furthermore, it's frustrating to forget but fear not, fortunately the (free) fread function's first facet is f; for fast, friendly, file or finagle.

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
\url{http://stackoverflow.com/questions/11782084/reading-in-large-text-files-in-r}\cr
\url{http://stackoverflow.com/questions/45972/mmap-vs-reading-blocks}\cr
\url{http://stackoverflow.com/questions/258091/when-should-i-use-mmap-for-file-access}\cr
\url{http://stackoverflow.com/a/9818473/403310}\cr
\url{http://stackoverflow.com/questions/9608950/reading-huge-files-using-memory-mapped-files}

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
system.time(DT <- fread("test.csv"))                                    #  3 sec (faster and friendlier)

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

system.time(DT <- fread("testbig.csv"))                                 # 30-40 sec
all(mapply(all.equal, DF, DT))                                          # TRUE


# Real data example (Airline data)
# http://stat-computing.org/dataexpo/2009/the-data.html

download.file("http://stat-computing.org/dataexpo/2009/2008.csv.bz2",
              destfile="2008.csv.bz2")                                  # 109MB (compressed)
system("bunzip2 2008.csv.bz2")                                          # 658MB (7,009,728 rows x 29 columns)
colClasses = sapply(read.csv("2008.csv",nrows=100),class)               # 4 character, 24 integer, 1 logical. Incorrect.
colClasses = sapply(read.csv("2008.csv",nrows=200),class)               # 5 character, 24 integer. Correct. Might have missed data
system.time(DF <- read.table("2008.csv", header=TRUE, sep=",",          
    quote="",stringsAsFactors=FALSE,comment.char="",nrows=7009730,      
    colClasses=colClasses)                                              # 360 secs
system.time(DT <- fread("2008.csv"))                                    #  40 secs
table(sapply(DT,class))                                                 # 5 character and 24 integer columns


# Reads URLs directly :
fread("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")

# Reads text input directly :
fread("A,B\n1,2\n3,4")

# including pasted input :
fread("A,B
1,2
3,4
")

}
}
\keyword{ data }

