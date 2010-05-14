\name{IDateTime}
\alias{IDate}
\alias{as.IDate}
\alias{ITime}
\alias{as.ITime}
\alias{IDateTime}
\alias{as.character.ITime}
\alias{as.chron.IDate}
\alias{as.chron.ITime}
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
\alias{cut.IDate}
\alias{format.ITime}
\alias{IDateTime.default}
\alias{mean.IDate}
\alias{print.ITime}
\alias{rep.IDate}
\alias{rep.ITime}
\alias{round.IDate}
\alias{seq.IDate}
\alias{split.IDate}
\alias{hour}   
\alias{yday}   
\alias{wday}   
\alias{mday}   
\alias{week}   
\alias{month}  
\alias{quarter}
\alias{year}

\title{ Integer based date class }
\description{
   Date and time classes with integer storage for fast sorting and
   grouping. Still experimental!
}
\usage{
as.IDate(x, ...)
\method{as.IDate}{default}(x, \dots)
\method{as.IDate}{Date}(x, \dots)
\method{as.Date}{IDate}(x, \dots)
\method{as.POSIXct}{IDate}(x, time = 0, tz = "UTC", \dots)
\method{as.chron}{IDate}(x, time = NULL, \dots)
\method{round}{IDate}(x, units = c("weeks", "months", "quarters", "years"))

as.ITime(x, ...)
\method{as.ITime}{default}(x, \dots)
\method{as.POSIXct}{ITime}(x, date = as.Date(Sys.time()), tz = "UTC", \dots)
\method{as.chron}{ITime}(x, date = NULL, \dots)
\method{as.character}{ITime}(x, \dots)
\method{format}{ITime}(x, \dots)

IDateTime(x, ...)
\method{IDateTime}{default}(x, ...)

hour(x)   
yday(x)   
wday(x)   
mday(x)   
week(x)   
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
  \item{date}{date object convertable with \code{as.IDate}.}
  \item{time}{time-of-day object convertable with \code{as.ITime}.}
  \item{units}{one of the units listed for rounding. May be abbreviated.}
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

Conversions to and from \code{Date}, \code{POSIXct}, and \code{chron}
formats are provided.

\code{ITime} does not account for time zones. When converting
\code{ITime} and \code{IDate} to POSIXct with \code{as.POSIXct}, a time
zone may be specified.

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
factors for use with data.table.

The \code{round} method for IDate's is useful for grouping and plotting. It can
round to weeks, months, quarters, and years.

}

\value{
   For \code{as.IDate}, a class of \code{IDate} and \code{Date} with the
   date stored as the number of days since some origin.

   For \code{as.ITime}, a class of \code{ITime}
   stored as the number of seconds in the day.

   For \code{IDateTime}, a data table with columns \code{idate} and
   \code{itime} in \code{IDate} and \code{ITime} format.

   \code{hour}, code{yday}, \code{wday}, \code{mday}, \code{week},
   \code{month}, \code{quarter}, and \code{year} return integer values
   for hour, day of year, day of week, day of month, week, month,
   quarter, and year.
   
}
\references{

  G. Grothendieck and T. Petzoldt, ``Date and Time Classes in R,''
  R News, vol. 4, no. 1, June 2004.

  H. Wickham, http://gist.github.com/10238.
  
}

\author{ Tom Short, t.short@ieee.org }

\seealso{ \code{\link{as.Date}}, \code{\link{as.POSIXct}},
  \code{\link{strptime}}, \code{\link{DateTimeClasses}}

}

\examples{

    # create IDate:
    (d <- as.IDate("2001-01-01"))

    # create ITime:
    (t <- as.ITime("10:45"))

    (t <- as.ITime("10:45:04"))

    (t <- as.ITime("10:45:04", format = "\%H:\%M:\%S"))
    
    as.POSIXct("2001-01-01") + as.ITime("10:45")

    datetime <- seq(as.POSIXct("2001-01-01"), as.POSIXct("2001-01-03"), by = "5 hour")    
    (af <- data.table(IDateTime(datetime), a = rep(1:2, 5), key = "a,idate,itime"))

    af[, mean(a), by = "itime"]
    af[, mean(a), by = "hour = hour(itime)"]
    af[, mean(a), by = "wday = factor(weekdays(idate))"] 
    af[, mean(a), by = "wday = wday(idate)"] 

    as.POSIXct(af$idate) 
    as.POSIXct(af$idate, time = af$itime) 
    as.POSIXct(af$idate, af$itime) 
    as.POSIXct(af$idate, time = af$itime, tz = "GMT") 
    
    as.POSIXct(af$itime, af$idate)
    as.POSIXct(af$itime) # uses today's date

    (seqdates <- seq(as.IDate("2001-01-01"), as.IDate("2001-08-03"), by = "3 weeks"))
    round(seqdates, "months")
    
    if (require(chron)) {
        as.chron(as.IDate("2000-01-01"))
        as.chron(as.ITime("10:45"))
        as.chron(as.IDate("2000-01-01"), as.ITime("10:45"))
        as.chron(as.ITime("10:45"), as.IDate("2000-01-01"))
        as.ITime(chron(times = "11:01:01"))
        IDateTime(chron("12/31/98","10:45:00"))
    }  

}
\keyword{utilities}
\keyword{chron}

