\name{fwrite}
\alias{fwrite}
\title{Fast CSV writer}
\description{
As \code{write.csv} and but \emph{much} faster (e.g. 1 minute versus 2 seconds) and just as flexible. Machines with 16GB of RAM or more always have more than one core so \code{fwrite} uses them; on all operating systems including Linux, Mac and Windows.
}
\usage{
fwrite(x, file = "", append = FALSE, quote = "auto", sep = ",", sep2 = c("",";",""),
       eol = if (.Platform$OS.type=="windows") "\r\n" else "\n",
       na = "", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("double","escape"),
       logicalAsInt = FALSE, buffMB = 8L, nThread = getDTthreads(),
       showProgress = getOption("datatable.showProgress"),
       verbose = getOption("datatable.verbose"),
       ..turbo=TRUE)
}
\arguments{
  \item{x}{A \code{data.table} or \code{data.frame} to write.}
  \item{file}{Output file name. \code{""} indicates output to the console. }
  \item{append}{If \code{TRUE}, the file is opened in append mode and column names (header row) are not written.}
  \item{quote}{When \code{"auto"}, character columns, factor columns and column names will only be surrounded by double quotes if the field does contain the separator, a line ending or the quote itself. If \code{FALSE} the fields are never wrapped with quotes even if this would break the CSV due to the contents of the field. If \code{TRUE} quotes are always included for character and factor columns.}
  \item{sep}{The separator between columns. Default is \code{","}.}
  \item{sep2}{For columns of type \code{list} where each item is an atomic vector, \code{sep2} controls how to separate items \emph{within} the column. The 1st item is placed at the start of the output field, the 2nd item is placed between each item and the 3rd at the end. The 1st and 3rd item may be any length character strings including \code{""} (default) for nothing. The 2nd item must be a single character and different from `sep` and `dec`.}  
  \item{eol}{Line separator. Default is \code{"\r\n"} for windows and \code{"\n"} otherwise.}
  \item{na}{The string to use for missing values in the data. Default is a blank string, \code{""}.}
  \item{dec}{The decimal separator, by default \code{"."}. See link in references. Cannot be the same as \code{sep}.}
  \item{row.names}{Should row names be written? For compatibility with \code{data.frame} and \code{write.csv} since \code{data.table} never has row names. Hence default \code{FALSE} unlike \code{write.csv}.} 
  \item{col.names}{Should the column names (header row) be written? If missing, \code{append=TRUE} and the file already exists, the default is set to \code{FALSE} for convenience to prevent column names appearing again mid file.}
  \item{qmethod}{A character string specifying how to deal with embedded double quote characters when quoting strings.
      \itemize{
	\item{"escape" - the quote character (as well as the backslash character) is escaped in C style by a backslash, or}
	\item{"double" (default, same as \code{write.csv}), in which case the double quote is doubled with another one.}
      }}
  \item{logicalAsInt}{Should \code{logical} values be written as \code{1} and \code{0} rather than \code{"TRUE"} and \code{"FALSE"}?}
  \item{buffMB}{The buffer size per thread in the range 1 to 1024. Experiment to see what works best for your data on your hardware.}
  \item{nThread}{The number of threads to use. For convenience of experimentation (perhaps in a loop) to see what works best for your data on your hardware.}
  \item{showProgress}{ Display a progress meter on the console when \code{file!=""}. }
  \item{verbose}{Be chatty and report timings?}
  \item{..turbo}{Use specialized custom C code to format numeric, integer and integer64 columns. This reduces call overhead to the C library and avoids any use of memory buffers (copies) at all. Try with and without to see the difference it makes on your machine and please report any significant differences in output. If you do find cases where \code{..turbo=FALSE} is needed, please report them as bugs, since this option WILL BE REMOVED in future. Hence why it has the \code{__} prefix.}
}
\details{
\code{fwrite} began as a community contribution with a \href{https://github.com/Rdatatable/data.table/pull/1613}{Pull Request PR#1613} by Otto Seiskari. Following that, Matt worked on reducing time spent on I/O with buffered write. This resulted in further speed enhancements.

Since those improvements resulted in the time spent almost entirely on formatting, it meant that we could benefit a lot from parallelisation. This was also done, which improved performance even further (YMMV depending on the number of cores / threads per core in one's machine).

Finally, with \code{..turbo = TRUE}, the time spent on formatting (through calls to C libraries) itself was reduced by implementing native C-code for writing \code{integer} and \code{numeric} types.

The logic for formatting \code{integer} columns is simpler (= faster) than \code{numeric} types. Therefore columns stored as \code{numeric} types, but in reality are \code{integers} are identified and formatted using integer logic for further improvement.

With this, writing a \code{data.table} of approximately 23 million rows and 19 columns (~2.85GB on disk) takes ~5.9s with \code{..turbo = TRUE} and ~20s with \code{..turbo = FALSE} on a 13' Macbook Pro with 512GB SSD and an i7 processor with 2 cores containing one thread per core (and two virtual threads via hyperthreading).

}
\seealso{
  \code{\link{setDTthreads}}, \code{\link{fread}}, \code{\link[utils]{write.csv}}, \code{\link[utils]{write.table}}
}
\references{
  \url{http://blog.h2o.ai/2016/04/fast-csv-writing-for-r/}
  \url{https://en.wikipedia.org/wiki/Decimal_mark}
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
system.time(fwrite(dt, "~/tmp.tsv", sep="\t")) # 0.795s, file size=439MB

# append to file
dt2 = as.data.table(matrix(runif(100), ncol=10L))
fwrite(dt2, "~/tmp.tsv", sep="\t", append=TRUE)
}
}
\keyword{ data }

