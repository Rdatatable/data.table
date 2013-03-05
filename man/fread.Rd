\name{fread}
\alias{fread}
\title{ Fast and friendly file finagler }
\description{
   Similar to \code{read.table} but faster and more convenient. All controls such as \code{sep}, \code{colClasses} and \code{nrows} are automatically detected. \code{bit64::integer64} types are also detected and read directly without needing to read as character before converting.
   
   \bold{This function is still under development}. For example, dates are read as character (they can be converted afterwards using the excellent \code{fasttime} package or standard base functions) and embedded quotes (\code{"\""} and \code{""""}) have problems. There are other known issues that haven't been fixed and features not yet implemented. But, you may find it works in many cases. Please report problems to datatable-help or Stack Overflow's data.table tag.
   
   \bold{Not for production use yet}. Not because it's unstable in the sense that it crashes or is buggy (your testing will show whether it is stable in your cases or not) but because \code{fread}'s arguments and behaviour is likely to change in future; i.e., we expect to make (hopefully minor) non-backwards-compatible changes. Why has it been released to CRAN then? Because a maintenance release was asked for by CRAN maintainers to comply with new stricter tests in R-devel, and a few Bioconductor packages depend on data.table and Bioconductor requires packages to pass R-devel checks. It was quicker to leave \code{fread} in and write these paragraphs, than take \code{fread} out.
   
}
\usage{
fread(input, sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE, verbose=FALSE, autostart=30)
}
\arguments{
  \item{input}{ Either the file name to read (containing no \\n character) or the input itself as a string (containing at least one \\n), see examples. In both cases, a length 1 character string. A filename may be a URL starting http:// or file://. }
  \item{sep}{ The separator between columns. Defaults to the first character in the set [\code{,\\t |;:}] that exists on line \code{autostart} outside quoted (\code{""}) regions, and separates the rows above \code{autostart} into a consistent number of fields, too. }
  \item{sep2}{ The separator \emph{within} columns. A \code{list} column will be returned where each cell is a vector of values. This is much faster using less working memory than \code{strsplit} afterwards or similar techniques. For each column \code{sep2} can be different and is the first character in the same set above [\code{,\\t |;:}], other than \code{sep}, that exists inside each field outside quoted regions on line \code{autostart}. NB: \code{sep2} is not yet implemented. }
  \item{nrows}{ The number of rows to read, by default -1 means all. Unlike \code{read.table}, it doesn't help speed to set this to the number of rows in the file (or an estimate), since the number of rows is automatically determined and is already fast. Only set \code{nrows} if you require the first 10 rows, for example, or are reading the file in chunks. }
  \item{header}{ Does the first data line contain column names? Defaults according to whether every field on the first data line is type character. }
  \item{na.strings}{ A character vector of strings to convert to \code{NA_character_}. By default for columns read as type character \code{",,"} is read as a blank string (\code{""}) and \code{",NA,"} is read as \code{NA_character_}. Typical alternatives might be \code{na.strings=NULL} or perhaps \code{na.strings=c("NA","N/A","")}. }
  \item{stringsAsFactors}{ Convert all character columns to factors? }
  \item{verbose}{ Be chatty and report timings? }
  \item{autostart}{ Any line number within the region of machine readable delimited text, by default 30. If the file is shorter or this line is empty (e.g. short files with trailing blank lines) then the last non empty line is used. This line and the lines above it are used to auto detect \code{sep}, \code{sep2} and the number of fields. It's extremely unlikely that \code{autostart} should ever need to be changed, we hope. }
}
\details{

Once the separator is found on line \code{autostart}, the number of columns is determined. Then the file is searched backwards from \code{autostart} until a row is found that doesn't have that number of columns, or the start of file is reached. Thus, the first data row is found and any human readable banners are automatically skipped. This feature can be particularly useful for loading a set of files which may not all have consistently sized banners.

The first 5 rows, middle 5 rows and last 5 rows are then read to determine column types. The lowest type for each column is chosen from the ordered list \code{integer}, \code{integer64}, \code{double}, \code{character}. This enables \code{fread} to allocate exactly the right number of rows, with columns of the right type, up front once. The file may of course \emph{still} contain data of a different type in rows other than first, middle and last 5. In that case, the column types are bumped mid read and the data read on previous rows is coerced. Setting \code{verbose=TRUE} reports the line and field number of each mid read type bump, and how long this type bumping took (if any).

There is no line length limit, not even a very large one. Since we are encouraging \code{list} columns (i.e. \code{sep2}) this has the potential to encourage longer line lengths. So the approach of scanning each line into a buffer first and then rescanning that buffer is not used. There is no field width limit either, not even a very large one. There are no buffers used in \code{fread}'s C code at all. The only limits are those imposed by R itself: the maximum width of a character string perhaps, for example.

\code{character} columns can be quoted (\code{...,2,"Joe Bloggs",3.14,...}) or not quoted (\code{...,2,Joe Bloggs,3.14,...}). Spaces and other whitepace (other than \code{sep} and \code{\\n}) may appear in an unquoted character field, provided the field doesn't contain \code{sep} itself. Therefore quoting character values is only required if \code{sep} itself appears in the string value. Quoting may also be used to signify that numeric data should be read as text (or that can be achieved by specifying the column type via \code{colClasses}). Field quoting is automatically detected and no arguments are needed to control it.

The filename extension (such as .csv) is irrelevant for "auto" \code{sep} and \code{sep2}. Separator detection is entirely driven by the file contents. This can be useful when loading a set of different files which may not be named consistently, or may not have the extension .csv despite being csv. Some datasets have been collected over many years, one file per day for example. Sometimes the file name format has changed at some point in the past or even the format of the file itself. So the idea is that you can loop \code{fread} through a set of files and as long as each file is regular and delimited, \code{fread} can read them all. Whether they all stack is another matter but at least each one is read quickly without you needing to vary \code{colClasses} in \code{read.table} or \code{read.csv}.

All known line endings are detected automatically: \code{\\n} (*NIX including Mac), \code{\\r\\n} (Windows CRLF), \code{\\r} (old Mac) and \code{\\n\\r} (just in case). There is no need to convert input files first. \code{fread} running on any architecture will read a file from any architecture. Both \code{\\r} and \code{\\n} may be embedded in character strings (including column names) provided the field is quoted.

Furthermore, these few features are for fostering friendliness. Facilitated by a fair farthingsworth of (far from flaky, flawed
or fatuous) finagling. Finally, it's frustrating to forget but fear not fine friends, fortunately the (free) fread function's first facet is f; for fast, friendly, file or finagle.

}
\value{
    A \code{data.table}.
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

# Reads pasted input directly :
fread("A,B
1,2
3,4
")

# Finds the first data line automatically :
fread("
This is perhaps a banner line or two or ten.
A,B
1,2
3,4
")

# Detects whether column names are present automatically :
fread("
1,2
3,4
")

}
}
\keyword{ data }

