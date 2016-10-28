\name{fwrite}
\alias{fwrite}
\title{Fast CSV writer}
\description{
As \code{write.csv} and but \emph{much} faster (e.g. 1 minute versus 2 seconds) and just as flexible. Machines with 16GB of RAM or more always have more than one core so \code{fwrite} uses them; on all operating systems including Linux, Mac and Windows.
}
\usage{
fwrite(x, file = "", append = FALSE, quote = "auto", sep = ",",
       eol = if (.Platform$OS.type=="windows") "\r\n" else "\n",
       na = "", col.names = TRUE, qmethod = "double",
       verbose=FALSE, turbo=TRUE)
}
\arguments{
  \item{x}{A \code{data.table} or \code{data.frame} to write.}
  \item{file}{Output file name. \code{""} indicates output to the console. }
  \item{append}{If \code{TRUE}, the file is opened in append mode and column names (header row) are not written.}
  \item{quote}{When \code{"auto"}, character and factor columns as well as all column names will only be surrounded by double quotes if the field contains the separator, a line ending or the quote itself, otherwise the quote characters are omitted. If \code{FALSE} the fields are never wrapped with quotes even if this would break the CSV due to the contents of the field. If \code{TRUE} quotes are always included for character and factor columns.}
  \item{sep}{The separator between columns. Default is \code{","}.}
  \item{eol}{Line separator. Default is \code{"\r\n"} for windows and \code{"\n"} otherwise.}
  \item{na}{The string to use for missing values in the data. Default is a blank string, \code{""}.}
  \item{col.names}{A logical value indicating if the column names (header row) should be written to file. If missing and \code{append} is \code{TRUE}, this is forced to \code{FALSE} to prevent mid-file deployment of column names unless \code{file} already exists.}
\item{qmethod}{A character string specifying how to deal with embedded double quote characters when quoting strings.
      \itemize{
	\item{"escape" - the quote character (as well as the backslash character) is escaped in C style by a backslash, or}
	\item{"double" (default), in which case it is doubled.}
      }}
  \item{verbose}{Be chatty and report timings?}
  \item{turbo}{Use specialized custom C code to format numeric and integer columns. This reduces call overhead to the C library and avoids any use of memory buffers (copies) at all. Try with and without to see the difference it makes on your machine and please report any significant differences in output.}
}
\details{
\code{fwrite} began as a community contribution with a \href{https://github.com/Rdatatable/data.table/pull/1613}{Pull Request PR#1613} by Otto Seiskari. Following that, Matt worked on reducing time spent on I/O with buffered write. This resulted in further speed enhancements.

Since those improvements resulted in the time spent almost entirely on formatting, it meant that we could benefit a lot from parallelisation. This was also done, which improved performance even further (YMMV depending on the number of cores / threads per core in one's machine).

Finally, with \code{turbo = TRUE}, the time spent on formatting (through calls to C libraries) itself was reduced by implementing native C-code for writing \code{integer} and \code{numeric} types.

The logic for formatting \code{integer} columns is simpler (= faster) than \code{numeric} types. Therefore columns stored as \code{numeric} types, but in reality are \code{integers} are identified and formatted using integer logic for further improvement.

With this, writing a \code{data.table} of approximately 23 million rows and 19 columns (~2.85GB on disk) takes ~5.9s with \code{turbo = TRUE} and ~20s with \code{turbo = FALSE} on a 13' Macbook Pro with 512GB SSD and an i7 processor with 2 cores containing one thread per core (and two virtual threads via hyperthreading).

}
\seealso{
  \code{\link{fread}}, \code{\link[utils]{write.csv}}, \code{\link[utils]{write.table}}
}
\references{
  \url{http://blog.h2o.ai/2016/04/fast-csv-writing-for-r/}
}
\examples{
\dontrun{
fwrite(data.table(first=c(1,2), second=c(NA, 'foo"bar')), "table.csv")

# table.csv contains:

# "first","second"
# "1",""
# "2","foo""bar"

## speed benchmark:
# System spec: 13' macbook pro, 2 cores, 1 thread/core (+2 virtual threads),
# 256KB L2 cache per core, 4MB L3 cache, 512GB SSD

set.seed(45L)
dt = as.data.table(matrix(as.numeric(sample(5e6*10L)), ncol=10L)) # 381MB
system.time(fwrite(dt, "~/tmp.tsv", quote=FALSE, sep="\t")) # 0.795s, file size=439MB

# append to file
dt2 = as.data.table(matrix(runif(100), ncol=10L))
fwrite(dt2, "~/tmp.tsv", quote=FALSE, sep="\t", append=TRUE)
}
}
\keyword{ data }

