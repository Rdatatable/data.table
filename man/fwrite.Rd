\name{fwrite}
\alias{fwrite}
\title{Fast CSV writer}
\description{
As \code{write.csv} but much faster (e.g. 2 seconds versus 1 minute) and just as flexible. Modern machines almost surely have more than one CPU so \code{fwrite} uses them; on all operating systems including Linux, Mac and Windows.
}
\usage{
fwrite(x, file = "", append = FALSE, quote = "auto", sep = ",", sep2 = c("","|",""),
       eol = if (.Platform$OS.type=="windows") "\r\n" else "\n",
       na = "", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("double","escape"),
       logicalAsInt = FALSE, dateAs = c("yyyy-mm-dd", "yyyymmdd", "epoch"),
       buffMB = 8L, nThread = getDTthreads(),
       showProgress = getOption("datatable.showProgress"),
       verbose = getOption("datatable.verbose"),
       ..turbo=TRUE)
}
\arguments{
  \item{x}{Any \code{list} of same length vectors; e.g. \code{data.frame} and \code{data.table}.}
  \item{file}{Output file name. \code{""} indicates output to the console. }
  \item{append}{If \code{TRUE}, the file is opened in append mode and column names (header row) are not written.}
  \item{quote}{When \code{"auto"}, character fields, factor fields and column names will only be surrounded by double quotes when they need to be; i.e., when the field contains the separator \code{sep}, a line ending \code{\\n}, the double quote itself or (when \code{list} columns are present) \code{sep2[2]} (see \code{sep2} below). If \code{FALSE} the fields are not wrapped with quotes even if this would break the CSV due to the contents of the field. If \code{TRUE} double quotes are always included other than around numeric fields, as \code{write.csv}.}
  \item{sep}{The separator between columns. Default is \code{","}.}
  \item{sep2}{For columns of type \code{list} where each item is an atomic vector, \code{sep2} controls how to separate items \emph{within} the column. \code{sep2[1]} is written at the start of the output field, \code{sep2[2]} is placed between each item and \code{sep2[3]} is written at the end. \code{sep2[1]} and \code{sep2[3]} may be any length strings including empty \code{""} (default). \code{sep2[2]} must be a single character and (when \code{list} columns are present and therefore \code{sep2} is used) different from both \code{sep} and \code{dec}. The default (\code{|}) is chosen to visually distinguish from the default \code{sep}. In speaking, writing and in code comments we may refer to \code{sep2[2]} as simply "sep2".}  
  \item{eol}{Line separator. Default is \code{"\r\n"} for windows and \code{"\n"} otherwise.}
  \item{na}{The string to use for missing values in the data. Default is a blank string \code{""}.}
  \item{dec}{The decimal separator, by default \code{"."}. See link in references. Cannot be the same as \code{sep}.}
  \item{row.names}{Should row names be written? For compatibility with \code{data.frame} and \code{write.csv} since \code{data.table} never has row names. Hence default \code{FALSE} unlike \code{write.csv}.} 
  \item{col.names}{Should the column names (header row) be written? If missing, \code{append=TRUE} and the file already exists, the default is set to \code{FALSE} for convenience to prevent column names appearing again mid file.}
  \item{qmethod}{A character string specifying how to deal with embedded double quote characters when quoting strings.
      \itemize{
	\item{"escape" - the quote character (as well as the backslash character) is escaped in C style by a backslash, or}
	\item{"double" (default, same as \code{write.csv}), in which case the double quote is doubled with another one.}
      }}
  \item{logicalAsInt}{Should \code{logical} values be written as \code{1} and \code{0} rather than \code{"TRUE"} and \code{"FALSE"}?}
  \item{dateAs}{The default is \code{"yyyy-mm-dd"} to match \code{write.csv}. \code{"yyyymmdd"} drops the separator so that dates can be read as integers in human readable format (that choice allows fast extraction of \code{yyyy}, \code{mm} and \code{dd} parts using \code{\%/\%10000}, \code{\%/\%100 \%\%100} and \code{\%\%100} respectively. Finally, \code{"epoch"} writes the underlying integer as-is: the number of days since the 1970-01-01 epoch (negative before that; see \code{?Date}). All three options are very fast due to new specialized C code. You should not be able to notice any difference in write speed between these options. The date range supported and tested is [0000-03-01, 9999-12-31] (3,652,365 dates including 2,790 leap days).}
  \item{buffMB}{The buffer size (MB) per thread in the range 1 to 1024, default 8MB. Experiment to see what works best for your data on your hardware.}
  \item{nThread}{The number of threads to use. Experiment to see what works best for your data on your hardware.}
  \item{showProgress}{ Display a progress meter on the console? Ignored when \code{file==""}. }
  \item{verbose}{Be chatty and report timings?}
  \item{..turbo}{Use specialized custom C code to format numeric, integer and integer64 columns. This reduces call overhead to the C library and avoids copies. Try with and without to see the difference it makes on your machine and please report any differences in output. If you do find cases where \code{..turbo=FALSE} is needed, please report them as bugs, since this option WILL BE REMOVED in future. Hence why it has the \code{..} prefix.}
}
\details{
\code{fwrite} began as a community contribution with \href{https://github.com/Rdatatable/data.table/pull/1613}{pull request #1613} by Otto Seiskari. This gave Matt Dowle the impetus to specialize the numeric formatting and to parallelize: \url{http://blog.h2o.ai/2016/04/fast-csv-writing-for-r/}. Final items were tracked in \href{https://github.com/Rdatatable/data.table/issues/1664}{issue #1664} such as automatic quoting, \code{bit64::integer64} support, decimal/scientific formatting exactly matching \code{write.csv} between 2.225074e-308 and 1.797693e+308 to 15 significant figures, \code{row.names}, dates (between 0000-03-01 and 9999-12-31), times and \code{sep2} for \code{list} columns where each cell can itself be a vector.
}
\seealso{
  \code{\link{setDTthreads}}, \code{\link{fread}}, \code{\link[utils]{write.csv}}, \code{\link[utils]{write.table}}, \href{https://CRAN.R-project.org/package=bit64}{\code{bit64::integer64}}
}
\references{
  \url{http://howardhinnant.github.io/date_algorithms.html}
  \url{https://en.wikipedia.org/wiki/Decimal_mark}
}
\examples{

DF = data.frame(A=1:3, B=c("foo","A,Name","baz"))
fwrite(DF)
write.csv(DF, row.names=FALSE, quote=FALSE)  # same

fwrite(DF, row.names=TRUE, quote=TRUE)
write.csv(DF)                                # same

DF = data.frame(A=c(2.1,-1.234e-307,pi), B=c("foo","A,Name","bar"))
fwrite(DF, quote='auto')        # Just DF[2,2] is auto quoted 
write.csv(DF, row.names=FALSE)  # same numeric formatting

DT = data.table(A=c(2,5.6,-3),B=list(1:3,c("foo","A,Name","bar"),round(pi*1:3,2)))
fwrite(DT)
fwrite(DT, sep="|", sep2=c("{",",","}"))

\dontrun{

set.seed(1)
DT = as.data.table(lapply(1:10, sample, x=as.numeric(1:5e7), size=5e6))  #  382MB
system.time(fwrite(DT, "/dev/shm/tmp1.csv"))                             #  0.8s
system.time(write.csv(DT, "/dev/shm/tmp2.csv",                           # 60.6s
                      quote=FALSE, row.names=FALSE))         
system("diff /dev/shm/tmp1.csv /dev/shm/tmp2.csv")

set.seed(1)
N = 1e7
DT = data.table(
  str1=sample(sprintf("%010d",sample(N,1e5,replace=TRUE)), N, replace=TRUE),
  str2=sample(sprintf("%09d",sample(N,1e5,replace=TRUE)), N, replace=TRUE),
  str3=sample(sapply(sample(2:30, 100, TRUE), function(n) paste0(sample(LETTERS, n, TRUE), collapse="")), N, TRUE),
  str4=sprintf("%05d",sample(sample(1e5,50),N,TRUE)),
  num1=sample(round(rnorm(1e6,mean=6.5,sd=15),2), N, replace=TRUE),
  num2=sample(round(rnorm(1e6,mean=6.5,sd=15),10), N, replace=TRUE),
  str5=sample(c("Y","N"),N,TRUE),
  str6=sample(c("M","F"),N,TRUE),
  int1=sample(ceiling(rexp(1e6)), N, replace=TRUE),
  int2=sample(N,N,replace=TRUE)-N/2
)                                                                        #  774MB
system.time(fwrite(DT,"/dev/shm/tmp1.csv"))                              #  1.1s
system.time(write.csv(DT,"/dev/shm/tmp2.csv",row.names=F,quote=F))       # 63.2s
system("diff /dev/shm/tmp1.csv /dev/shm/tmp2.csv")

unlink("/dev/shm/tmp1.csv")
unlink("/dev/shm/tmp2.csv")
}

}
\keyword{ data }

