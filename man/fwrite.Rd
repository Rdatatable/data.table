\name{fwrite}
\alias{fwrite}
\title{Fast CSV writer}
\description{
As \code{write.csv} but much faster (e.g. 2 seconds versus 1 minute) and just as flexible. Modern machines almost surely have more than one CPU so \code{fwrite} uses them; on all operating systems including Linux, Mac and Windows.
}
\usage{
fwrite(x, file = "", append = FALSE, quote = "auto",
  sep=getOption("datatable.fwrite.sep", ","),
  sep2 = c("","|",""),
  eol = if (.Platform$OS.type=="windows") "\r\n" else "\n",
  na = "", dec = ".", row.names = FALSE, col.names = TRUE,
  qmethod = c("double","escape"),
  logical01 = getOption("datatable.logical01", FALSE),  # due to change to TRUE; see NEWS
  logicalAsInt = NULL,  # deprecated
  scipen = getOption('scipen', 0L),
  dateTimeAs = c("ISO","squash","epoch","write.csv"),
  buffMB = 8L, nThread = getDTthreads(verbose),
  showProgress = getOption("datatable.showProgress", interactive()),
  compress = c("auto", "none", "gzip"),
  compressLevel = 6L,
  yaml = FALSE,
  bom = FALSE,
  verbose = getOption("datatable.verbose", FALSE),
  encoding = "")
}
\arguments{
  \item{x}{Any \code{list} of same length vectors; e.g. \code{data.frame} and \code{data.table}. If \code{matrix}, it gets internally coerced to \code{data.table} preserving col names but not row names}
  \item{file}{Output file name. \code{""} indicates output to the console. }
  \item{append}{If \code{TRUE}, the file is opened in append mode and column names (header row) are not written.}
  \item{quote}{When \code{"auto"}, character fields, factor fields and column names will only be surrounded by double quotes when they need to be; i.e., when the field contains the separator \code{sep}, a line ending \code{\\n}, the double quote itself or (when \code{list} columns are present) \code{sep2[2]} (see \code{sep2} below). If \code{FALSE} the fields are not wrapped with quotes even if this would break the CSV due to the contents of the field. If \code{TRUE} double quotes are always included other than around numeric fields, as \code{write.csv}.}
  \item{sep}{The separator between columns. Default is \code{","}.}
  \item{sep2}{For columns of type \code{list} where each item is an atomic vector, \code{sep2} controls how to separate items \emph{within} the column. \code{sep2[1]} is written at the start of the output field, \code{sep2[2]} is placed between each item and \code{sep2[3]} is written at the end. \code{sep2[1]} and \code{sep2[3]} may be any length strings including empty \code{""} (default). \code{sep2[2]} must be a single character and (when \code{list} columns are present and therefore \code{sep2} is used) different from both \code{sep} and \code{dec}. The default (\code{|}) is chosen to visually distinguish from the default \code{sep}. In speaking, writing and in code comments we may refer to \code{sep2[2]} as simply "sep2".}
  \item{eol}{Line separator. Default is \code{"\r\n"} for Windows and \code{"\n"} otherwise.}
  \item{na}{The string to use for missing values in the data. Default is a blank string \code{""}.}
  \item{dec}{The decimal separator, by default \code{"."}. See link in references. Cannot be the same as \code{sep}.}
  \item{row.names}{Should row names be written? For compatibility with \code{data.frame} and \code{write.csv} since \code{data.table} never has row names. Hence default \code{FALSE} unlike \code{write.csv}.}
  \item{col.names}{Should the column names (header row) be written? The default is \code{TRUE} for new files and when overwriting existing files (\code{append=FALSE}). Otherwise, the default is \code{FALSE} to prevent column names appearing again mid-file when stacking a set of \code{data.table}s or appending rows to the end of a file.}
  \item{qmethod}{A character string specifying how to deal with embedded double quote characters when quoting strings.
      \itemize{
	\item "escape" - the quote character (as well as the backslash character) is escaped in C style by a backslash, or
	\item "double" (default, same as \code{write.csv}), in which case the double quote is doubled with another one.
      }}
  \item{logical01}{Should \code{logical} values be written as \code{1} and \code{0} rather than \code{"TRUE"} and \code{"FALSE"}?}
  \item{logicalAsInt}{Deprecated. Old name for `logical01`. Name change for consistency with `fread` for which `logicalAsInt` would not make sense.}
  \item{scipen}{ \code{integer} In terms of printing width, how much of a bias should there be towards printing whole numbers rather than scientific notation? See Details. }
  \item{dateTimeAs}{ How \code{Date}/\code{IDate}, \code{ITime} and \code{POSIXct} items are written.
      \itemize{
	\item "ISO" (default) - \code{2016-09-12}, \code{18:12:16} and \code{2016-09-12T18:12:16.999999Z}. 0, 3 or 6 digits of fractional seconds are printed if and when present for convenience, regardless of any R options such as \code{digits.secs}. The idea being that if milli and microseconds are present then you most likely want to retain them. R's internal UTC representation is written faithfully to encourage ISO standards, stymie timezone ambiguity and for speed. An option to consider is to start R in the UTC timezone simply with \code{"$ TZ='UTC' R"} at the shell (NB: it must be one or more spaces between \code{TZ='UTC'} and \code{R}, anything else will be silently ignored; this TZ setting applies just to that R process) or \code{Sys.setenv(TZ='UTC')} at the R prompt and then continue as if UTC were local time.
	\item "squash" - \code{20160912}, \code{181216} and \code{20160912181216999}. This option allows fast and simple extraction of \code{yyyy}, \code{mm}, \code{dd} and (most commonly to group by) \code{yyyymm} parts using integer div and mod operations. In R for example, one line helper functions could use \code{\%/\%10000}, \code{\%/\%100\%\%100}, \code{\%\%100} and \code{\%/\%100} respectively. POSIXct UTC is squashed to 17 digits (including 3 digits of milliseconds always, even if \code{000}) which may be read comfortably as \code{integer64} (automatically by \code{fread()}).
	\item "epoch" - \code{17056}, \code{65536} and \code{1473703936.999999}. The underlying number of days or seconds since the relevant epoch (1970-01-01, 00:00:00 and 1970-01-01T00:00:00Z respectively), negative before that (see \code{?Date}). 0, 3 or 6 digits of fractional seconds are printed if and when present.
	\item "write.csv" - this currently affects \code{POSIXct} only. It is written as \code{write.csv} does by using the \code{as.character} method which heeds \code{digits.secs} and converts from R's internal UTC representation back to local time (or the \code{"tzone"} attribute) as of that historical date. Accordingly this can be slow. All other column types (including \code{Date}, \code{IDate} and \code{ITime} which are independent of timezone) are written as the "ISO" option using fast C code which is already consistent with \code{write.csv}.
      }
  The first three options are fast due to new specialized C code. The epoch to date-part conversion uses a fast approach by Howard Hinnant (see references) using a day-of-year starting on 1 March. You should not be able to notice any difference in write speed between those three options. The date range supported for \code{Date} and \code{IDate} is [0000-03-01, 9999-12-31]. Every one of these 3,652,365 dates have been tested and compared to base R including all 2,790 leap days in this range. \cr \cr
  This option applies to vectors of date/time in list column cells, too. \cr \cr
  A fully flexible format string (such as \code{"\%m/\%d/\%Y"}) is not supported. This is to encourage use of ISO standards and because that flexibility is not known how to make fast at C level. We may be able to support one or two more specific options if required.
  }
  \item{buffMB}{The buffer size (MB) per thread in the range 1 to 1024, default 8MB. Experiment to see what works best for your data on your hardware.}
  \item{nThread}{The number of threads to use. Experiment to see what works best for your data on your hardware.}
  \item{showProgress}{ Display a progress meter on the console? Ignored when \code{file==""}. }
  \item{compress}{If \code{compress = "auto"} and if \code{file} ends in \code{.gz} then output format is gzipped csv else csv. If \code{compress = "none"}, output format is always csv. If \code{compress = "gzip"} then format is gzipped csv. Output to the console is never gzipped even if \code{compress = "gzip"}. By default, \code{compress = "auto"}.}
  \item{compressLevel}{Level of compression between 1 and 9, 6 by default. See \url{https://linux.die.net/man/1/gzip} for details.}
  \item{yaml}{If \code{TRUE}, \code{fwrite} will output a CSVY file, that is, a CSV file with metadata stored as a YAML header, using \code{\link[yaml]{as.yaml}}. See \code{Details}. }
  \item{bom}{If \code{TRUE} a BOM (Byte Order Mark) sequence (EF BB BF) is added at the beginning of the file; format 'UTF-8 with BOM'.}
  \item{verbose}{Be chatty and report timings?}
  \item{encoding}{ The encoding of the strings written to the CSV file. Default is \code{""}, which means writing raw bytes without considering the encoding. Other possible options are \code{"UTF-8"} and \code{"native"}. }
}
\details{
\code{fwrite} began as a community contribution with \href{https://github.com/Rdatatable/data.table/pull/1613}{pull request #1613} by Otto Seiskari. This gave Matt Dowle the impetus to specialize the numeric formatting and to parallelize: \url{https://h2o.ai/blog/2016/fast-csv-writing-for-r/}. Final items were tracked in \href{https://github.com/Rdatatable/data.table/issues/1664}{issue #1664} such as automatic quoting, \code{bit64::integer64} support, decimal/scientific formatting exactly matching \code{write.csv} between 2.225074e-308 and 1.797693e+308 to 15 significant figures, \code{row.names}, dates (between 0000-03-01 and 9999-12-31), times and \code{sep2} for \code{list} columns where each cell can itself be a vector.

To save space, \code{fwrite} prefers to write wide numeric values in scientific notation -- e.g. \code{10000000000} takes up much more space than \code{1e+10}. Most file readers (e.g. \code{\link{fread}}) understand scientific notation, so there's no fidelity loss. Like in base R, users can control this by specifying the \code{scipen} argument, which follows the same rules as \code{\link[base]{options}('scipen')}. \code{fwrite} will see how much space a value will take to write in scientific vs. decimal notation, and will only write in scientific notation if the latter is more than \code{scipen} characters wider. For \code{10000000000}, then, \code{1e+10} will be written whenever \code{scipen<6}.

\bold{CSVY Support:}

The following fields will be written to the header of the file and surrounded by \code{---} on top and bottom:

  \itemize{
    \item \code{source} - Contains the R version and \code{data.table} version used to write the file
    \item \code{creation_time_utc} - Current timestamp in UTC time just before the header is written
    \item \code{schema} with element \code{fields} giving \code{name}-\code{type} (\code{class}) pairs for the table; multi-class objects (e.g. \code{c('POSIXct', 'POSIXt')}) will have their first class written.
    \item \code{header} - same as \code{col.names} (which is \code{header} on input)
    \item \code{sep}
    \item \code{sep2}
    \item \code{eol}
    \item \code{na.strings} - same as \code{na}
    \item \code{dec}
    \item \code{qmethod}
    \item \code{logical01}
  }

}
\seealso{
  \code{\link{setDTthreads}}, \code{\link{fread}}, \code{\link[utils:write.table]{write.csv}}, \code{\link[utils:write.table]{write.table}}, \href{https://CRAN.R-project.org/package=bit64}{\code{bit64::integer64}}
}
\references{
  \url{https://howardhinnant.github.io/date_algorithms.html}\cr
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
DT = as.data.table( lapply(1:10, sample,
         x=as.numeric(1:5e7), size=5e6))                            #     382MB
system.time(fwrite(DT, "/dev/shm/tmp1.csv"))                        #      0.8s
system.time(write.csv(DT, "/dev/shm/tmp2.csv",                      #     60.6s
                      quote=FALSE, row.names=FALSE))
system("diff /dev/shm/tmp1.csv /dev/shm/tmp2.csv")                  # identical

set.seed(1)
N = 1e7
DT = data.table(
  str1=sample(sprintf("\%010d",sample(N,1e5,replace=TRUE)), N, replace=TRUE),
  str2=sample(sprintf("\%09d",sample(N,1e5,replace=TRUE)), N, replace=TRUE),
  str3=sample(sapply(sample(2:30, 100, TRUE), function(n)
     paste0(sample(LETTERS, n, TRUE), collapse="")), N, TRUE),
  str4=sprintf("\%05d",sample(sample(1e5,50),N,TRUE)),
  num1=sample(round(rnorm(1e6,mean=6.5,sd=15),2), N, replace=TRUE),
  num2=sample(round(rnorm(1e6,mean=6.5,sd=15),10), N, replace=TRUE),
  str5=sample(c("Y","N"),N,TRUE),
  str6=sample(c("M","F"),N,TRUE),
  int1=sample(ceiling(rexp(1e6)), N, replace=TRUE),
  int2=sample(N,N,replace=TRUE)-N/2
)                                                                   #     774MB
system.time(fwrite(DT,"/dev/shm/tmp1.csv"))                         #      1.1s
system.time(write.csv(DT,"/dev/shm/tmp2.csv",                       #     63.2s
                      row.names=FALSE, quote=FALSE))
system("diff /dev/shm/tmp1.csv /dev/shm/tmp2.csv")                  # identical

unlink("/dev/shm/tmp1.csv")
unlink("/dev/shm/tmp2.csv")
}

}
\keyword{ data }

