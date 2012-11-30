\name{read}
\alias{read}
\title{ Fast and friendly file finagler }
\description{
   Similar to \code{read.table} but much faster and more convenient. All controls such as delimiter, colClasses and nrows are automatically detected. Dual-delimited files such as BED are automatically detected: columns 11 and 12 are each read directly into list columns where each cell is itself an integer vector (no need for strsplit). bit64::integer64 types are also detected and read directly without needing to read as character then convert.
   
   This help file is in development and may document features that haven't been implemented yet.
}
\usage{
read(fnam, sep="auto", sep2="auto", header="auto", na.strings=NULL, stringsAsFactors=FALSE, verbose=FALSE)
}
\arguments{
  \item{fnam}{ The file name to read, as a length 1 character string. May be a URL starting http:// or file://, and on Windows where R has been started with --internet2, a URL starting https:// }
  \item{sep}{ The separator between columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n]. }
  \item{sep2}{ The separator \emph{within} columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n<sep>]. }
  \item{header}{ Does the first line contain column names? Defaults according to a comparison of field types in rows 1 vs 2.}
  \item{na.strings}{ A character vector of strings to convert to \code{NA_character_}. By default, ",," is read as a blank string \code{""} and ",NA," is read as \code{"NA"}. Typical values might be \code{na.strings="NA"}, or \code{na.strings=c("NA","N/A","")}. }
  \item{stringsAsFactors}{ Convert all character columns to factors? }
  \item{verbose}{ Be chatty? }
}
\details{

character columns can be single or double quoted, or not quoted at all. Spaces and other whitepace (in fact anything other than sep) may appear in unquoted character column, provided the field doesn't contain sep itself. Therefore quoting character values is only required if sep itself appears in the string value. This is automatically detected and no arguments are needed to control it. If a character value starts " it should end with "; it may then include sep and '. If a character value starts ' it should end with '; it may then include sep and ". Just like the R parser. This is on a per field basis; i.e., an entire column need not follow consistent quoting. Just a few cells (perhaps just those that contain sep) may only be quoted. The quoting character can be different for different fields.

There is no line length limit, not even a very large one. Since we are encouraging list columns using a secondary separator, where the data has that structure, this has the potential to encourage longer line lengths. So the approach of scanning each line into a buffer first and then rescanning that buffer is not used. We use a single pass approach.

The filename extension (such as .csv) is irrelevant for "auto" \code{sep} and \code{sep2}. Separator detection is entirely driven by the file contents. This can be useful when loading a large set of files which may not be named consistently.

There is no need for a \code{skip} argument. Once the format and number of fields is detected from lines 30-39, the first row in the file containing a conformant number of fields is deemed to be the header row. Thus, human readable banner rows are automatically skipped. This feature can be useful if you are loading many files which may not all have consistently sized banners.

Features like these are intended to foster friendliness. Achieved with a fair amount of fenagling.

}
\value{
    A data.table.
}
\references{
\url{http://cran.r-project.org/doc/manuals/R-data.html}
\url{http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r}
\url{www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html}
\url{https://stat.ethz.ch/pipermail/r-help/2007-August/138315.html}
\url{http://www.cerebralmastication.com/2009/11/loading-big-data-into-r/}
\url{http://stackoverflow.com/questions/9061736/faster-than-scan-with-rcpp}
\url{http://stackoverflow.com/questions/415515/how-can-i-read-and-manipulate-csv-file-data-in-c}
\url{http://stackoverflow.com/questions/9352887/strategies-for-reading-in-csv-files-in-pieces}
\url{http://stackoverflow.com/questions/11782084/reading-in-large-text-files-in-r}

finagler = "to get or achieve by guile or manipulation" \url{http://dictionary.reference.com/browse/finagler}
}
\seealso{ \code{\link[utils]{read.csv}}, \code{\link[base]{url}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
\dontrun{

# demo speedup
n=1e6
DT = data.table( a=sample(1:1000,n,replace=TRUE),
                 b=sample(1:1000,n,replace=TRUE),
                 c=rnorm(n),
                 d=sample(c("foo","bar","baz","qux","quux"),n,replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000,n,replace=TRUE) )
DT[2,b:=NA_integer_]
DT[4,c:=NA_real_]
# DT[3,d:=NA_character_]
DT[5,d:=""]
write.table(DT,"test.csv",sep=",",row.names=FALSE,quote=FALSE)

cat("File size (MB):",round(file.info("test.csv")$size/1024^2),"\n")  # 50 MB (1e6 rows x 6 columns)
system.time(x <- read.csv("test.csv",stringsAsFactors=FALSE))         # 60 sec
system.time(y <- read.table("test.csv",header=TRUE,sep=",",quote="",stringsAsFactors=FALSE,comment.char="",nrows=as.integer(n),colClasses=c("integer","integer","numeric","character","numeric","integer")))
                                                                      # 10 sec (all known advice with known nrows, see references)
identical(x,y)                                                        #   TRUE

system.time(z <- read("test.csv"))                                    #  5 sec (faster and friendlier)
all.equal(as.data.table(x), z)                                        #   TRUE

# read("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")          # Format is auto detected.

}
}
\keyword{ data }

