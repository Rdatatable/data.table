\name{read}
\alias{read}
\title{ Fast and friendly file reader }
\description{
   Similar to read.table but much faster and more convenient. All controls such as delimiter, colClasses and nrows are automatically detected. Additionally, dual delimited files such as BED are automatically detected and read (columns 11 and 12 are each read directly into list columns where each cell is itself an integer vector; no need for strsplit).
}
\usage{
read(fnam, sep, sep2, header, verbose=TRUE)
}
\arguments{
  \item{fnam}{ The file name to read, as a length 1 character string. May be a URL starting http:// or file://, and on Windows where R has been started with --internet2, a URL starting https:// }
  \item{sep}{ Optional. The separator between columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n]. }
  \item{sep2}{ Optional. The separator \emph{within} columns. Defaults to the first character not in the set [a-zA-Z0-9"'\\n<sep>]. For regular .csv files where each column is an atomic vector, sep2 will default to ""; i.e., not relevant. }
  \item{header}{ Optional. Does the first line contain column names? Defaults according to a comparison of item types in rows 1 -vs- 2.}
  \item{verbose}{ Optional. Become chatty, by default FALSE. Currently TRUE in development. }
}
\details{
The 
}
\value{
    A data.table.
}
\references{
\url{http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r}
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
DT[3,d:=NA_character_]
DT[5,d:=""]
write.table(DT,"test.csv",sep=",",row.names=FALSE,quote=FALSE)

cat("File size (MB):",round(file.info("test.csv")$size/1024^2),"\n")  # 50MB
system.time(x <- read.csv("test.csv",stringsAsFactors=FALSE))         # 63s
system.time(y <- read("test.csv"))                                    #  5s
all(mapply(all.equal,x,y))   # TRUE

read("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")          # Format is auto detected.

}
}
\keyword{ data }

