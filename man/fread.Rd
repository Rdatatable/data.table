\name{fread}
\alias{fread}
\title{ Fast and friendly file finagler }
\description{
   Similar to \code{read.table} but faster and more convenient. All controls such as \code{sep}, \code{colClasses} and \code{nrows} are automatically detected.

   \code{bit64::integer64}, \code{\link{IDate}}, and \code{\link{POSIXct}} types are also detected and read directly without needing to read as character before converting.

   \code{fread} is for \emph{regular} delimited files; i.e., where every row has the same number of columns. In future, secondary separator (\code{sep2}) may be specified \emph{within} each column. Such columns will be read as type \code{list} where each cell is itself a vector.
}
\usage{
fread(input, file, text, cmd, sep="auto", sep2="auto", dec=".", quote="\"",
nrows=Inf, header="auto",
na.strings=getOption("datatable.na.strings","NA"),  # due to change to ""; see NEWS
stringsAsFactors=FALSE, verbose=getOption("datatable.verbose", FALSE),
skip="__auto__", select=NULL, drop=NULL, colClasses=NULL,
integer64=getOption("datatable.integer64", "integer64"),
col.names,
check.names=FALSE, encoding="unknown",
strip.white=TRUE, fill=FALSE, blank.lines.skip=FALSE,
key=NULL, index=NULL,
showProgress=getOption("datatable.showProgress", interactive()),
data.table=getOption("datatable.fread.datatable", TRUE),
nThread=getDTthreads(verbose),
logical01=getOption("datatable.logical01", FALSE),  # due to change to TRUE; see NEWS
keepLeadingZeros = getOption("datatable.keepLeadingZeros", FALSE),
yaml=FALSE, autostart=NA, tmpdir=tempdir(), tz=""
)
}
\arguments{
  \item{input}{ A single character string. The value is inspected and deferred to either \code{file=} (if no \\n present), \code{text=} (if at least one \\n is present) or \code{cmd=} (if no \\n is present, at least one space is present, and it isn't a file name). Exactly one of \code{input=}, \code{file=}, \code{text=}, or \code{cmd=} should be used in the same call. }
  \item{file}{ File name in working directory, path to file (passed through \code{\link[base]{path.expand}} for convenience), or a URL starting http://, file://, etc. Compressed files with extension \file{.gz} and \file{.bz2} are supported if the \code{R.utils} package is installed. }
  \item{text}{ The input data itself as a character vector of one or more lines, for example as returned by \code{readLines()}. }
  \item{cmd}{ A shell command that pre-processes the file; e.g. \code{fread(cmd=paste("grep",word,"filename")}. See Details. }
  \item{sep}{ The separator between columns. Defaults to the character in the set \code{[,\\t |;:]} that separates the sample of rows into the most number of lines with the same number of fields. Use \code{NULL} or \code{""} to specify no separator; i.e. each line a single character column like \code{base::readLines} does.}
  \item{sep2}{ The separator \emph{within} columns. A \code{list} column will be returned where each cell is a vector of values. This is much faster using less working memory than \code{strsplit} afterwards or similar techniques. For each column \code{sep2} can be different and is the first character in the same set above [\code{,\\t |;}], other than \code{sep}, that exists inside each field outside quoted regions in the sample. NB: \code{sep2} is not yet implemented. }
  \item{nrows}{ The maximum number of rows to read. Unlike \code{read.table}, you do not need to set this to an estimate of the number of rows in the file for better speed because that is already automatically determined by \code{fread} almost instantly using the large sample of lines. \code{nrows=0} returns the column names and typed empty columns determined by the large sample; useful for a dry run of a large file or to quickly check format consistency of a set of files before starting to read any of them. }
  \item{header}{ Does the first data line contain column names? Defaults according to whether every non-empty field on the first data line is type character. If so, or TRUE is supplied, any empty column names are given a default name. }
  \item{na.strings}{ A character vector of strings which are to be interpreted as \code{NA} values. By default, \code{",,"} for columns of all types, including type \code{character} is read as \code{NA} for consistency. \code{,"",} is unambiguous and read as an empty string. To read \code{,NA,} as \code{NA}, set \code{na.strings="NA"}. To read \code{,,} as blank string \code{""}, set \code{na.strings=NULL}. When they occur in the file, the strings in \code{na.strings} should not appear quoted since that is how the string literal \code{,"NA",} is distinguished from \code{,NA,}, for example, when \code{na.strings="NA"}. }
  \item{stringsAsFactors}{ Convert all character columns to factors? }
  \item{verbose}{ Be chatty and report timings? }
  \item{skip}{ If 0 (default) start on the first line and from there finds the first row with a consistent number of columns. This automatically avoids irregular header information before the column names row. \code{skip>0} means ignore the first \code{skip} rows manually. \code{skip="string"} searches for \code{"string"} in the file (e.g. a substring of the column names row) and starts on that line (inspired by read.xls in package gdata). }
  \item{select}{ A vector of column names or numbers to keep, drop the rest. \code{select} may specify types too in the same way as \code{colClasses}; i.e., a vector of \code{colname=type} pairs, or a \code{list} of \code{type=col(s)} pairs. In all forms of \code{select}, the order that the columns are specified determines the order of the columns in the result. }
  \item{drop}{ Vector of column names or numbers to drop, keep the rest. }
  \item{colClasses}{ As in \code{\link[utils:read.table]{utils::read.csv}}; i.e., an unnamed vector of types corresponding to the columns in the file, or a named vector specifying types for a subset of the columns by name. The default, \code{NULL} means types are inferred from the data in the file. Further, \code{data.table} supports a named \code{list} of vectors of column names \emph{or numbers} where the \code{list} names are the class names; see examples. The \code{list} form makes it easier to set a batch of columns to be a particular class. When column numbers are used in the \code{list} form, they refer to the column number in the file not the column number after \code{select} or \code{drop} has been applied.
    If type coercion results in an error, introduces \code{NA}s, or would result in loss of accuracy, the coercion attempt is aborted for that column with warning and the column's type is left unchanged. If you really desire data loss (e.g. reading \code{3.14} as \code{integer}) you have to truncate such columns afterwards yourself explicitly so that this is clear to future readers of your code.
  }
  \item{integer64}{ "integer64" (default) reads columns detected as containing integers larger than 2^31 as type \code{bit64::integer64}. Alternatively, \code{"double"|"numeric"} reads as \code{utils::read.csv} does; i.e., possibly with loss of precision and if so silently. Or, "character". }
  \item{dec}{ The decimal separator as in \code{utils::read.csv}. If not "." (default) then usually ",". See details. }
  \item{col.names}{ A vector of optional names for the variables (columns). The default is to use the header column if present or detected, or if not "V" followed by the column number. This is applied after \code{check.names} and before \code{key} and \code{index}. }
  \item{check.names}{default is \code{FALSE}. If \code{TRUE} then the names of the variables in the \code{data.table} are checked to ensure that they are syntactically valid variable names. If necessary they are adjusted (by \code{\link{make.names}}) so that they are, and also to ensure that there are no duplicates.}
  \item{encoding}{ default is \code{"unknown"}. Other possible options are \code{"UTF-8"} and \code{"Latin-1"}.  Note: it is not used to re-encode the input, rather enables handling of encoded strings in their native encoding. }
  \item{quote}{ By default (\code{"\""}), if a field starts with a double quote, \code{fread} handles embedded quotes robustly as explained under \code{Details}. If it fails, then another attempt is made to read the field \emph{as is}, i.e., as if quotes are disabled. By setting \code{quote=""}, the field is always read as if quotes are disabled. It is not expected to ever need to pass anything other than \"\" to quote; i.e., to turn it off. }
  \item{strip.white}{ default is \code{TRUE}. Strips leading and trailing whitespaces of unquoted fields. If \code{FALSE}, only header trailing spaces are removed. }
  \item{fill}{logical (default is \code{FALSE}). If \code{TRUE} then in case the rows have unequal length, blank fields are implicitly filled.}
  \item{blank.lines.skip}{\code{logical}, default is \code{FALSE}. If \code{TRUE} blank lines in the input are ignored.}
  \item{key}{Character vector of one or more column names which is passed to \code{\link{setkey}}. It may be a single comma separated string such as \code{key="x,y,z"}, or a vector of names such as \code{key=c("x","y","z")}. Only valid when argument \code{data.table=TRUE}. Where applicable, this should refer to column names given in \code{col.names}. }
  \item{index}{ Character vector or list of character vectors of one or more column names which is passed to \code{\link{setindexv}}. As with \code{key}, comma-separated notation like \code{index="x,y,z"} is accepted for convenience. Only valid when argument \code{data.table=TRUE}. Where applicable, this should refer to column names given in \code{col.names}. }
  \item{showProgress}{ \code{TRUE} displays progress on the console if the ETA is greater than 3 seconds. It is produced in fread's C code where the very nice (but R level) txtProgressBar and tkProgressBar are not easily available. }
  \item{data.table}{ TRUE returns a \code{data.table}. FALSE returns a \code{data.frame}. The default for this argument can be changed with \code{options(datatable.fread.datatable=FALSE)}.}
  \item{nThread}{The number of threads to use. Experiment to see what works best for your data on your hardware.}
  \item{logical01}{If TRUE a column containing only 0s and 1s will be read as logical, otherwise as integer.}
  \item{keepLeadingZeros}{If TRUE a column containing numeric data with leading zeros will be read as character, otherwise leading zeros will be removed and converted to numeric.}
  \item{yaml}{ If \code{TRUE}, \code{fread} will attempt to parse (using \code{\link[yaml]{yaml.load}}) the top of the input as YAML, and further to glean parameters relevant to improving the performance of \code{fread} on the data itself. The entire YAML section is returned as parsed into a \code{list} in the \code{yaml_metadata} attribute. See \code{Details}. }
  \item{autostart}{ Deprecated and ignored with warning. Please use \code{skip} instead. }
  \item{tmpdir}{ Directory to use as the \code{tmpdir} argument for any \code{tempfile} calls, e.g. when the input is a URL or a shell command. The default is \code{tempdir()} which can be controlled by setting \code{TMPDIR} before starting the R session; see \code{\link[base:tempfile]{base::tempdir}}. }
  \item{tz}{ Relevant to datetime values which have no Z or UTC-offset at the end, i.e. \emph{unmarked} datetime, as written by \code{\link[utils]{write.csv}}. The default \code{tz=""} means interpet unmarked datetime in the timezone of the R session, for consistency with R's \code{as.POSIXct()} and backwards compatibility. Set \code{tz="UTC"} to read unmarked datetime in UTC. Note that \code{fwrite()} by default writes datetime in UTC including the final Z (i.e. UTC-marked datetime) and \code{fwrite}'s output will be read by \code{fread} consistently and quickly without needing to use \code{tz=} or \code{colClasses=}. If the TZ environment variable is set to \code{"UTC"} (or \code{""} on non-Windows where unset vs `""` is significant) then R's timezone is already UTC, the default \code{tz=""} means UTC, and unmarked datetime will be read as UTC. The TZ environment variable being unset, however, means local time, in both C and R, and is quite different from the TZ environment variable being set to \code{""} on non-Windows which means UTC not local. You can use \code{Sys.setenv(TZ="UTC")}, and \code{Sys.unsetenv("TZ")}, too, and \code{fread} will use the latest value. }
}
\details{

A sample of 10,000 rows is used for a very good estimate of column types. 100 contiguous rows are read from 100 equally spaced points throughout the file including the beginning, middle and the very end. This results in a better guess when a column changes type later in the file (e.g. blank at the beginning/only populated near the end, or 001 at the start but 0A0 later on). This very good type guess enables a single allocation of the correct type up front once for speed, memory efficiency and convenience of avoiding the need to set \code{colClasses} after an error. Even though the sample is large and jumping over the file, it is almost instant regardless of the size of the file because a lazy on-demand memory map is used. If a jump lands inside a quoted field containing newlines, each newline is tested until 5 lines are found following it with the expected number of fields. The lowest type for each column is chosen from the ordered list: \code{logical}, \code{integer}, \code{integer64}, \code{double}, \code{character}. Rarely, the file may contain data of a higher type in rows outside the sample (referred to as an out-of-sample type exception). In this event \code{fread} will \emph{automatically} reread just those columns from the beginning so that you don't have the inconvenience of having to set \code{colClasses} yourself; particularly helpful if you have a lot of columns. Such columns must be read from the beginning to correctly distinguish "00" from "000" when those have both been interpreted as integer 0 due to the sample but 00A occurs out of sample. Set \code{verbose=TRUE} to see a detailed report of the logic deployed to read your file.

There is no line length limit, not even a very large one. Since we are encouraging \code{list} columns (i.e. \code{sep2}) this has the potential to encourage longer line lengths. So the approach of scanning each line into a buffer first and then rescanning that buffer is not used. There are no buffers used in \code{fread}'s C code at all. The field width limit is limited by R itself: the maximum width of a character string (currently 2^31-1 bytes, 2GB).

The filename extension (such as .csv) is irrelevant for "auto" \code{sep} and \code{sep2}. Separator detection is entirely driven by the file contents. This can be useful when loading a set of different files which may not be named consistently, or may not have the extension .csv despite being csv. Some datasets have been collected over many years, one file per day for example. Sometimes the file name format has changed at some point in the past or even the format of the file itself. So the idea is that you can loop \code{fread} through a set of files and as long as each file is regular and delimited, \code{fread} can read them all. Whether they all stack is another matter but at least each one is read quickly without you needing to vary \code{colClasses} in \code{read.table} or \code{read.csv}.

If an empty line is encountered then reading stops there with warning if any text exists after the empty line such as a footer. The first line of any text discarded is included in the warning message. Unless, it is single-column input. In that case blank lines are significant (even at the very end) and represent NA in the single column. So that \code{fread(fwrite(DT))==DT}. This default behaviour can be controlled using \code{blank.lines.skip=TRUE|FALSE}.

\bold{Line endings:} All known line endings are detected automatically: \code{\\n} (*NIX including Mac), \code{\\r\\n} (Windows CRLF), \code{\\r} (old Mac) and \code{\\n\\r} (just in case). There is no need to convert input files first. \code{fread} running on any architecture will read a file from any architecture. Both \code{\\r} and \code{\\n} may be embedded in character strings (including column names) provided the field is quoted.

\bold{Decimal separator and locale:} \code{fread(\dots,dec=",")} should just work. \code{fread} uses C function \code{strtod} to read numeric data; e.g., \code{1.23} or \code{1,23}. \code{strtod} retrieves the decimal separator (\code{.} or \code{,} usually) from the locale of the R session rather than as an argument passed to the \code{strtod} function. So for \code{fread(\dots,dec=",")} to work, \code{fread} changes this (and only this) R session's locale temporarily to a locale which provides the desired decimal separator.

On Windows, "French_France.1252" is tried which should be available as standard (any locale with comma decimal separator would suffice) and on unix "fr_FR.utf8" (you may need to install this locale on unix). \code{fread()} is very careful to set the locale back again afterwards, even if the function fails with an error. The choice of locale is determined by \code{options()$datatable.fread.dec.locale}. This may be a \emph{vector} of locale names and if so they will be tried in turn until the desired \code{dec} is obtained; thus allowing more than two different decimal separators to be selected. This is a new feature in v1.9.6 and is experimental. In case of problems, turn it off with \code{options(datatable.fread.dec.experiment=FALSE)}.

\bold{Quotes:}

When \code{quote} is a single character,

  \itemize{
      \item{Spaces and other whitespace (other than \code{sep} and \code{\\n}) may appear in unquoted character fields, e.g., \code{\dots,2,Joe Bloggs,3.14,\dots}.}

      \item{When \code{character} columns are \emph{quoted}, they must start and end with that quoting character immediately followed by \code{sep} or \code{\\n}, e.g., \code{\dots,2,"Joe Bloggs",3.14,\dots}.

      In essence quoting character fields are \emph{required} only if \code{sep} or \code{\\n} appears in the string value. Quoting may be used to signify that numeric data should be read as text. Unescaped quotes may be present in a quoted field, e.g., \code{\dots,2,"Joe, "Bloggs"",3.14,\dots}, as well as escaped quotes, e.g., \code{\dots,2,"Joe \",Bloggs\"",3.14,\dots}.

      If an embedded quote is followed by the separator inside a quoted field, the embedded quotes up to that point in that field must be balanced; e.g. \code{\dots,2,"www.blah?x="one",y="two"",3.14,\dots}.

      On those fields that do not satisfy these conditions, e.g., fields with unbalanced quotes, \code{fread} re-attempts that field as if it isn't quoted. This is quite useful in reading files that contains fields with unbalanced quotes as well, automatically.}
  }

To read fields \emph{as is} instead, use \code{quote = ""}.

\bold{CSVY Support:}

Currently, the \code{yaml} setting is somewhat inflexible with respect to incorporating metadata to facilitate file reading. Information on column classes should be stored at the top level under the heading \code{schema} and subheading \code{fields}; those with both a \code{type} and a \code{name} sub-heading will be merged into \code{colClasses}. Other supported elements are as follows:

  \itemize{
    \item{ \code{sep} (or alias \code{delimiter}) }
    \item{ \code{header} }
    \item{ \code{quote} (or aliases \code{quoteChar}, \code{quote_char}) }
    \item{ \code{dec} (or alias \code{decimal}) }
    \item{ \code{na.strings} }
  }

\bold{File Download:}

When \code{input} begins with http://, https://, ftp://, ftps://, or file://, \code{fread} detects this and \emph{downloads} the target to a temporary file (at \code{tempfile()}) before proceeding to read the file as usual. Secure URLS (ftps:// and https://) are downloaded with \code{curl::curl_download}; ftp:// and http:// paths are downloaded with \code{download.file} and \code{method} set to \code{getOption("download.file.method")}, defaulting to \code{"auto"}; and file:// is downloaded with \code{download.file} with \code{method="internal"}. NB: this implies that for file://, even files found on the current machine will be "downloaded" (i.e., hard-copied) to a temporary file. See \code{\link{download.file}} for more details.

\bold{Shell commands:}

\code{fread} accepts shell commands for convenience. The input command is run and its output written to a file in \code{tmpdir} (\code{\link{tempdir}()} by default) to which \code{fread} is applied "as normal". The details are platform dependent -- \code{system} is used on UNIX environments, \code{shell} otherwise; see \code{\link[base]{system}}.

}
\value{
    A \code{data.table} by default, otherwise a \code{data.frame} when argument \code{data.table=FALSE}.
}
\references{
Background :\cr
\url{https://cran.r-project.org/doc/manuals/R-data.html}\cr
\url{http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r}\cr
\url{http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html}\cr
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

On YAML, see \url{http://yaml.org/}; on csvy, see \url{http://csvy.org/}.
}
\seealso{
  \code{\link[utils:read.table]{read.csv}}, \code{\link[base:connections]{url}}, \code{\link[base:locales]{Sys.setlocale}}, \code{\link{setDTthreads}}, \code{\link{fwrite}}, \href{https://CRAN.R-project.org/package=bit64}{\code{bit64::integer64}}
}
\examples{
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

# Numerical precision :

DT = fread("A\n1.010203040506070809010203040506\n")
# TODO: add numerals=c("allow.loss", "warn.loss", "no.loss") from base::read.table, +"use.Rmpfr"
typeof(DT$A)=="double"   # currently "allow.loss" with no option

DT = fread("A\n1.46761e-313\n")   # read as 'numeric'
DT[,sprintf("\%.15E",A)]   # beyond what double precision can store accurately to 15 digits
# For greater accuracy use colClasses to read as character, then package Rmpfr.

# colClasses
data = "A,B,C,D\n1,3,5,7\n2,4,6,8\n"
fread(data, colClasses=c(B="character",C="character",D="character"))  # as read.csv
fread(data, colClasses=list(character=c("B","C","D")))    # saves typing
fread(data, colClasses=list(character=2:4))     # same using column numbers

# drop
fread(data, colClasses=c("B"="NULL","C"="NULL"))   # as read.csv
fread(data, colClasses=list(NULL=c("B","C")))      #
fread(data, drop=c("B","C"))      # same but less typing, easier to read
fread(data, drop=2:3)             # same using column numbers

# select
# (in read.csv you need to work out which to drop)
fread(data, select=c("A","D"))    # less typing, easier to read
fread(data, select=c(1,4))        # same using column numbers

# select and types combined
fread(data, select=c(A="numeric", D="character"))
fread(data, select=list(numeric="A", character="D"))

# skip blank lines
fread("a,b\n1,a\n2,b\n\n\n3,c\n", blank.lines.skip=TRUE)
# fill
fread("a,b\n1,a\n2\n3,c\n", fill=TRUE)
fread("a,b\n\n1,a\n2\n\n3,c\n\n", fill=TRUE)

# fill with skip blank lines
fread("a,b\n\n1,a\n2\n\n3,c\n\n", fill=TRUE, blank.lines.skip=TRUE)

# check.names usage
fread("a b,a b\n1,2\n")
fread("a b,a b\n1,2\n", check.names=TRUE) # no duplicates + syntactically valid names

\dontrun{
# Demo speed-up
n = 1e6
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
cat("File size (MB):", round(file.info("test.csv")$size/1024^2),"\n")
# 50 MB (1e6 rows x 6 columns)

system.time(DF1 <-read.csv("test.csv",stringsAsFactors=FALSE))
# 5.4 sec (first time in fresh R session)

system.time(DF1 <- read.csv("test.csv",stringsAsFactors=FALSE))
# 3.9 sec (immediate repeat is faster, varies)

system.time(DF2 <- read.table("test.csv",header=TRUE,sep=",",quote="",
    stringsAsFactors=FALSE,comment.char="",nrows=n,
    colClasses=c("integer","integer","numeric",
                 "character","numeric","integer")))
# 1.2 sec (consistently). All known tricks and known nrows, see references.

system.time(DT <- fread("test.csv"))
# 0.1 sec (faster and friendlier)

identical(DF1, DF2)
all.equal(as.data.table(DF1), DT)

# Scaling up ...
l = vector("list",10)
for (i in 1:10) l[[i]] = DT
DTbig = rbindlist(l)
tables()
write.table(DTbig,"testbig.csv",sep=",",row.names=FALSE,quote=FALSE)
# 500MB csv (10 million rows x 6 columns)

system.time(DF <- read.table("testbig.csv",header=TRUE,sep=",",
    quote="",stringsAsFactors=FALSE,comment.char="",nrows=1e7,
    colClasses=c("integer","integer","numeric",
                 "character","numeric","integer")))
# 17.0 sec (varies)

system.time(DT <- fread("testbig.csv"))
#  0.8 sec

all(mapply(all.equal, DF, DT))


# Real data example (Airline data)
# http://stat-computing.org/dataexpo/2009/the-data.html

download.file("http://stat-computing.org/dataexpo/2009/2008.csv.bz2",
              destfile="2008.csv.bz2")
# 109MB (compressed)

system("bunzip2 2008.csv.bz2")
# 658MB (7,009,728 rows x 29 columns)

colClasses = sapply(read.csv("2008.csv",nrows=100,stringsAsFactors=FALSE),class)
# 4 character, 24 integer, 1 logical. Incorrect.

colClasses = sapply(read.csv("2008.csv",nrows=200,stringsAsFactors=FALSE),class)
# 5 character, 24 integer. Correct. Might have missed data only using 100 rows
# since read.table assumes colClasses is correct.

system.time(DF <- read.table("2008.csv", header=TRUE, sep=",",
    quote="",stringsAsFactors=FALSE,comment.char="",nrows=7009730,
    colClasses=colClasses))
# 24.4 secs

system.time(DT <- fread("2008.csv"))
#  1.9 secs

table(sapply(DT,class))
# 5 character and 24 integer columns. Correct without needing to worry about colClasses
# issue above.


# Reads URLs directly :
fread("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")

# Decompresses .gz and .bz2 automatically :
fread("http://stat-computing.org/dataexpo/2009/1987.csv.bz2")

}
}
\keyword{ data }
