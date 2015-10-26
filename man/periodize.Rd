\name{periodize}
\alias{periodize}
\alias{IPeriod}
\alias{as.IPeriod}
\alias{as.factor.IPeriod}
\alias{as.IPeriod.default}
\alias{as.IPeriod.POSIXct}
\alias{as.IPeriod.data.table}
\alias{as.IPeriod.IDate}
\title{Round datetime}
\description{Periodize datetime object by rounding \emph{ceiling} or \emph{floor} to neaerst period defined by \code{unit} and \code{amount}.}
\usage{
periodize(idate, itime = 0L, unit, amount = 1L, origin = "1970-01-01", type = "ceiling")

as.IPeriod(x, \dots)
\method{as.IPeriod}{default}(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", \dots)
\method{as.IPeriod}{POSIXct}(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", \dots)
\method{as.IPeriod}{data.table}(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", \dots)
\method{as.IPeriod}{IDate}(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", itime = 0L, \dots)

as.factor.IPeriod(x) # do not use without setting `origin`, read notes
}
\arguments{
  \item{idate}{\emph{POSIXct, \link{IDate}, \link{IDateTime}-like data.table}, if \emph{IDate} then also \code{itime} is expected to be provided.}
  \item{itime}{\link{ITime} class object. If \code{x} is not a \emph{IDate} then this argument can be removed, allowing simple \code{periodize(posixct, "hours")} calls.}
  \item{x}{object to be converted to \emph{IPeriod}, accepted are \emph{POSIXct, \link{IDate}, \link{IDateTime}-like data.table}.}
  \item{unit}{character scalar, one of \emph{secs, mins, hours, days}.}
  \item{amount}{integer amount of the unit that forms single period.}
  \item{origin}{character date format of origin to be used as internal start of integer values. Default \emph{1970-01-01}.}
  \item{type}{character scalar, one of \emph{ceiling, floor}, also \emph{up, down} supported. Default \emph{ceiling}.}
  \item{\dots}{argument to be matched in methods, e.g. \code{itime}.}
}
\details{
Function \code{periodize} is just a wrapper to \code{as.IPeriod} which has to make it handy to use in data.table \code{DT[, .(hourly_mean = mean(x)), .(iperiod = periodize(idate,itime,"hours")]}. It handle reordering unnamed input arguments when \code{x} is \emph{POSIXct, data.table} by removing \code{itime} from the expected argument sequence, allowing simple \code{periodize(posixct, "hours", 15L)} calls. It also has some defaults.
}
\value{Integer based \emph{IPeriod} class. Four attributes attached \emph{unit, amount, origin, type}.}
\author{Jan Gorecki}
\note{
Use tight \code{origin} argument for potential speed-up when < 100k values (see \link{IDateTime}), or in case of using \code{as.factor.IPeriod} because factor levels will start from \code{origin} value, see examples.
Timezone should be handled after extracting date from IPeriod, e.g. \code{as.POSIXct(iperiod, tz="")}. IPeriod data with different attributes should not be compared to each other. 
}
\seealso{\link{IDateTime}, \link{IDate}, \link{ITime}}
\examples{
# basic usage IPeriod
x = as.POSIXct("2015-10-13 09:48:15", tz="UTC")
print(x)
# rounding ceiling or floor to flexible time periods
as.IPeriod(x, "hours", 12)
as.IPeriod(x, "mins", 10)
as.IPeriod(x, "secs", 15)
as.IPeriod(x, "secs", 15, type = "floor")

# input args dynamic matching
idt = IDateTime(x)
# POSIXct
periodize(x, "hours", 2)
# IDateTime
periodize(idt, "hours", 2)
# IDate, ITime
periodize(idt$idate, idt$itime, "hours", 2)

# use to aggregate dataset
set.seed(1)
DT = data.table(ts = as.POSIXct(1444857558L+3600L*c(1:100)/10, tz="UTC", origin="1970-01-01"),
                x = rnorm(100))
# group by periodize of idate and itime fields
DT[, c("idate","itime") := IDateTime(ts)]
DT[, .(hourly_mean = mean(x),
       hourly_pos = sum(x > 0),
       hourly_neg = sum(x < 0),
       range = diff(range(x))),
   .(iperiod = periodize(idate, itime, "hours"))]
# group by periodize directly on POSIXct
DT[, .(hourly_mean = mean(x),
       hourly_pos = sum(x > 0),
       hourly_neg = sum(x < 0),
       range = diff(range(x))),
   .(iperiod = periodize(ts, "hours"))]

# extract from IPeriod
p = as.IPeriod(x, "hours", 12)
as.character(p)
as.POSIXct(p, tz="UTC")
as.IDate(p)
as.ITime(p)
IDateTime(p)
# don't try `as.factor` without setting `origin`!

# origin and as.factor
(p = periodize(x, "hours", 6, origin = "2015-10-13"))
as.factor.IPeriod(p)
(p = periodize(x, "hours", 6, origin = "2015-10-01"))
as.factor.IPeriod(p)
# input of: "hours", 6, origin = "1970-01-01" (default!)
# produces factor of 60k+ levels!

# input of: unit="secs", amount=1L
# produces 1s periods, which maps to POSIXct based numeric
now = Sys.time()
attr(now, "tzone") <- "UTC"
periodize(now, "secs", type = "floor") == as.integer(now)
periodize(now, "secs") == as.integer(now) + 1L
}
