\name{read}
\alias{read}
\title{ Fast and friendly file finagler }
\description{
   Similar to read.table but much faster and more convenient. All controls such as delimiter, colClasses and nrows are automatically detected. Additionally, dual delimited files such as BED are automatically detected;  columns 11 and 12 are each read directly into list columns where each cell is itself an integer vector (no need for strsplit). bit64::integer64 types are also read directly.
   
   This help file may (currently, in development) document features that haven't been implemented yet.
}
\usage{
read(fnam, sep="auto", sep2="auto", header="auto", na.strings=NULL, stringsAsFactors=FALSE, verbose=TRUE)
}
\arguments{
  \item{fnam}{ The file name to read, as a length 1 character string. May be a URL starting http:// or file://, and on Windows where R has been started with --internet2, a URL starting https:// }
  \item{sep}{ Optional. The separator between columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n]. }
  \item{sep2}{ Optional. The separator \emph{within} columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n<sep>]. For regular .csv files where each column is an atomic vector, sep2 will default to ""; i.e., not relevant. }
  \item{header}{ Optional. Does the first line contain column names? Defaults according to a comparison of item types in rows 1 -vs- 2.}
  \item{na.strings}{ Optonal. By default (and always) ,, is treated as NA_character_. Additionally a vector of string values may be supplied to na.strings, all of which are converted to NA_character_ in all character columns; e.g., na.strings=c("N/A","NA"," "). Where " " represents all whitepace. }
  \item{stringsAsFactors}{ By default character columns are left as character. Convert to factor when TRUE. }
  \item{verbose}{ Optional. Become chatty, by default FALSE. Currently TRUE in development. }
}
\details{
Missing values are commonly represented in delimited files by, well, missing data; i.e., ",," (if the delimiter is comma). But in R "NA" has been written out by default to represent missingness. This file reader copes with both, but should be slightly faster with ",," vs ",NA,". This speed difference depends on how many NA there are and other factors. But even if there are a lot of NA, it should still be significantly faster than the current implementation of read.table. If you can remove NA in you files (using sed or a similar tool) then feel free to experiment for your cases. We suggest missing values are written as ",," going forward; e.g., set \code{na=""} in write.table and write.csv.

",NA," in character columns are read as the "NA" string literal by default, not NA_character_. na.strings can be used to change this, but isn't yet implemented.  (TO DO).

character columns can be single or double quoted, or not quoted at all. Spaces and other whitepace (in fact anything other than sep) may appear in unquoted character column, provided the field doesn't contain sep itself. Therefore quoting character values is only required if sep itself appears in the string value. This is all automatically detected and no arguments are needed to control it. If a character value starts " it should end with "; it may then include sep and '. If a character value starts ' it should end with '; it may then include sep and ". Just like the R parser. This is on a per field basis; i.e., an entire column need not follow consistent quoting. Just a few cells (perhaps just those that contain sep) may only be quoted. And the quoting character can vary within that column.  Features like this are intended to foster friendliness, achieved with a fair amount of fenangling.

There is no line length limit, not even a very large one. Since we are encouraging list columns using a secondary separator where the data has that structure, this has the potential to encourage longer line lengths. So the approach of scanning each line into a buffer first and then rescanning that buffer is not used. We use a single pass approach.

The filename extension (such as .csv) is irrelevant for "auto" sep and sep2. It's entirely driven by the file contents.

}
\value{
    A data.table.
}
\references{
\url{http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r}
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

cat("File size (MB):",round(file.info("test.csv")$size/1024^2),"\n")  # 50MB
system.time(x <- read.csv("test.csv",stringsAsFactors=FALSE))  # 63s
system.time(y <- read("test.csv"))                             #  5s
all(mapply(all.equal,x,y))                                     # TRUE

# read("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")          # Format is auto detected.

}
}
\keyword{ data }

