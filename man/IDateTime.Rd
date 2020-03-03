\name{IDateTime}
\alias{IDate}
\alias{as.IDate}
\alias{ITime}
\alias{as.ITime}
\alias{IDateTime}
\alias{as.character.ITime}
\alias{as.Date.IDate}
\alias{as.IDate.Date}
\alias{as.IDate.default}
\alias{as.ITime.character}
\alias{as.ITime.default}
\alias{as.ITime.POSIXlt}
\alias{as.ITime.times}
\alias{as.list.IDate}
\alias{as.POSIXct.IDate}
\alias{as.POSIXct.ITime}
\alias{as.POSIXlt.ITime}
\alias{c.IDate}
\alias{c.ITime}
\alias{format.ITime}
\alias{IDateTime.default}
\alias{mean.IDate}
\alias{mean.ITime}
\alias{print.ITime}
\alias{rep.IDate}
\alias{rep.ITime}
\alias{round.IDate}
\alias{round.ITime}
\alias{trunc.ITime}
\alias{seq.IDate}
\alias{seq.ITime}
\alias{second}
\alias{minute}
\alias{hour}
\alias{yday}
\alias{wday}
\alias{mday}
\alias{week}
\alias{isoweek}
\alias{month}
\alias{quarter}
\alias{year}
\alias{IDate-class}
\alias{ITime-class}

\title{ Integer based date class }
\description{
   Date and time classes with integer storage for fast sorting and
   grouping. Still experimental!
}
\usage{
as.IDate(x, \dots)
\method{as.IDate}{default}(x, \dots, tz = attr(x, "tzone", exact=TRUE))
\method{as.IDate}{Date}(x, \dots)
\method{as.Date}{IDate}(x, \dots)
\method{as.POSIXct}{IDate}(x, tz = "UTC", time = 0, \dots)
\method{round}{IDate}(x, digits = c("weeks", "months", "quarters","years"), \ldots)

as.ITime(x, \dots)
\method{as.ITime}{default}(x, \dots)
\method{as.ITime}{POSIXlt}(x, ms = 'truncate', \dots)
\method{round}{ITime}(x, digits = c("hours", "minutes"), \ldots)
\method{trunc}{ITime}(x, units = c("hours", "minutes"), \ldots)

\method{as.POSIXct}{ITime}(x, tz = "UTC", date = Sys.Date(), \dots)
\method{as.character}{ITime}(x, \dots)
\method{format}{ITime}(x, \dots)

IDateTime(x, \dots)
\method{IDateTime}{default}(x, \dots)

second(x)
minute(x)
hour(x)
yday(x)
wday(x)
mday(x)
week(x)
isoweek(x)
month(x)
quarter(x)
year(x)

}

\arguments{
  \item{x}{an object}
  \item{\dots}{arguments to be passed to or from other methods. For
    \code{as.IDate.default}, arguments are passed to \code{as.Date}. For
    \code{as.ITime.default}, arguments are passed to \code{as.POSIXlt}.}
  \item{tz}{time zone (see \code{strptime}).}
  \item{date}{date object convertible with \code{as.IDate}.}
  \item{time}{time-of-day object convertible with \code{as.ITime}.}
  \item{digits}{really \code{units}; one of the units listed for rounding. May be abbreviated.}
  \item{units}{one of the units listed for truncating. May be abbreviated.}
  \item{ms}{ For \code{as.ITime} methods, what should be done with sub-second fractions of input? Valid values are \code{'truncate'} (floor), \code{'nearest'} (round), and \code{'ceil'} (ceiling). See Details. }
}
\details{
\code{IDate} is a date class derived from \code{Date}. It has the same
internal representation as the \code{Date} class, except the storage
mode is integer. \code{IDate} is a relatively simple wrapper, and it
should work in almost all situations as a replacement for \code{Date}.

Functions that use \code{Date} objects generally work for
\code{IDate} objects. This package provides specific methods for
\code{IDate} objects for \code{mean}, \code{cut}, \code{seq}, \code{c},
\code{rep}, and \code{split} to return an \code{IDate} object.

\code{ITime} is a time-of-day class stored as the integer number of
seconds in the day. \code{as.ITime} does not allow days longer than 24
hours. Because \code{ITime} is stored in seconds, you can add it to a
\code{POSIXct} object, but you should not add it to a \code{Date}
object.

Conversions to and from \code{Date} and \code{POSIXct} formats are provided.

\code{ITime} does not account for time zones. When converting
\code{ITime} and \code{IDate} to POSIXct with \code{as.POSIXct}, a time
zone may be specified.

Inputs like \code{'2018-05-15 12:34:56.789'} are ambiguous from the perspective of an \code{ITime} object -- the method of coercion of the 789 milliseconds is controlled by the \code{ms} argument to relevant methods. The default behavior (\code{ms = 'truncate'}) is to use \code{as.integer}, which has the effect of truncating anything after the decimal. Alternatives are to round to the nearest integer (\code{ms = 'nearest'}) or to round up (\code{ms = 'ceil'}).

In \code{as.POSIXct} methods for \code{ITime} and \code{IDate}, the
second argument is required to be \code{tz} based on the generic
template, but to make converting easier, the second argument is
interpreted as a date instead of a time zone if it is of type
\code{IDate} or \code{ITime}. Therefore, you can use either of the
following: \code{as.POSIXct(time, date)} or \code{as.POSIXct(date,
time)}.

\code{IDateTime} takes a date-time input and returns a data table with
columns \code{date} and \code{time}.

Using integer storage allows dates and/or times to be used as data table
keys. With positive integers with a range less than 100,000, grouping
and sorting is fast because radix sorting can be used (see
\code{sort.list}).

Several convenience functions like \code{hour} and \code{quarter} are
provided to group or extract by hour, month, and other date-time
intervals. \code{as.POSIXlt} is also useful. For example,
\code{as.POSIXlt(x)$mon} is the integer month. The R base convenience
functions \code{weekdays}, \code{months}, and \code{quarters} can also
be used, but these return character values, so they must be converted to
factors for use with data.table. \code{isoweek} is ISO 8601-consistent.

The \code{round} method for IDate's is useful for grouping and plotting. 
It can round to weeks, months, quarters, and years. Similarly, the \code{round}
and \code{trunc} methods for ITime's are useful for grouping and plotting.
They can round or truncate to hours and minutes. 
Note for ITime's with 30 seconds, rounding is inconsistent due to rounding off a 5. 
See 'Details' in \code{\link{round}} for more information.

}

\value{
   For \code{as.IDate}, a class of \code{IDate} and \code{Date} with the
   date stored as the number of days since some origin.

   For \code{as.ITime}, a class of \code{ITime}
   stored as the number of seconds in the day.

   For \code{IDateTime}, a data table with columns \code{idate} and
   \code{itime} in \code{IDate} and \code{ITime} format.

   \code{second}, \code{minute}, \code{hour}, \code{yday}, \code{wday},
   \code{mday}, \code{week}, \code{month}, \code{quarter},
   and \code{year} return integer values
   for second, minute, hour, day of year, day of week,
   day of month, week, month, quarter, and year, respectively.

   These values are all taken directly from the \code{POSIXlt} representation 
   of \code{x}, with the notable difference that while \code{yday}, \code{wday}, 
   and \code{mon} are all 0-based, here they are 1-based.

}
\references{

  G. Grothendieck and T. Petzoldt, ``Date and Time Classes in R,''
  R News, vol. 4, no. 1, June 2004.

  H. Wickham, http://gist.github.com/10238.

  ISO 8601, http://www.iso.org/iso/home/standards/iso8601.htm
}

\author{ Tom Short, t.short@ieee.org }

\seealso{ \code{\link{as.Date}}, \code{\link{as.POSIXct}},
  \code{\link{strptime}}, \code{\link{DateTimeClasses}}

}

\examples{

# create IDate:
(d <- as.IDate("2001-01-01"))

# S4 coercion also works
identical(as.IDate("2001-01-01"), as("2001-01-01", "IDate"))

# create ITime:
(t <- as.ITime("10:45"))

# S4 coercion also works
identical(as.ITime("10:45"), as("10:45", "ITime"))

(t <- as.ITime("10:45:04"))

(t <- as.ITime("10:45:04", format = "\%H:\%M:\%S"))

as.POSIXct("2001-01-01") + as.ITime("10:45")

datetime <- seq(as.POSIXct("2001-01-01"), as.POSIXct("2001-01-03"), by = "5 hour")
(af <- data.table(IDateTime(datetime), a = rep(1:2, 5), key = "a,idate,itime"))

af[, mean(a), by = "itime"]
af[, mean(a), by = list(hour = hour(itime))]
af[, mean(a), by = list(wday = factor(weekdays(idate)))]
af[, mean(a), by = list(wday = wday(idate))]

as.POSIXct(af$idate)
as.POSIXct(af$idate, time = af$itime)
as.POSIXct(af$idate, af$itime)
as.POSIXct(af$idate, time = af$itime, tz = "GMT")

as.POSIXct(af$itime, af$idate)
as.POSIXct(af$itime) # uses today's date

(seqdates <- seq(as.IDate("2001-01-01"), as.IDate("2001-08-03"), by = "3 weeks"))
round(seqdates, "months")

(seqtimes <- seq(as.ITime("07:00"), as.ITime("08:00"), by = 20))
round(seqtimes, "hours")
trunc(seqtimes, "hours")
   
}
\keyword{utilities}

