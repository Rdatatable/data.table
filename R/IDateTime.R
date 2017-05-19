
###################################################################
# IDate -- a simple wrapper class around Date using integer storage
###################################################################

as.IDate <- function(x, ...) UseMethod("as.IDate")

as.IDate.default <-
    function(x, ...) as.IDate(as.Date(x, ...))

as.IDate.Date <- function(x, ...) {
    structure(as.integer(x), class=c("IDate","Date"))
}    

as.IDate.POSIXct <- function(x, ...) {
  tz = attr(x, "tzone")
  if (is.null(tz)) tz = "UTC"
  as.IDate(as.Date(x, tz = tz, ...))
}

as.IDate.IDate <- function(x, ...) x

as.Date.IDate <- function(x, ...) { 
    structure(as.numeric(x), class="Date")
}

mean.IDate <-
cut.IDate <-
seq.IDate <-
c.IDate <-
rep.IDate <-
split.IDate <-
unique.IDate <-
    function(x, ...) {
        as.IDate(NextMethod())
    }

# fix for #1315
as.list.IDate <- function(x, ...) NextMethod()

# rounding -- good for graphing / subsetting
## round.IDate <- function (x, digits, units=digits, ...) {
##     if (missing(digits)) digits <- units # workaround to provide a units argument to match the round generic and round.POSIXt
##     units <- match.arg(digits, c("weeks", "months", "quarters", "years"))
round.IDate <- function (x, digits=c("weeks", "months", "quarters", "years"), ...) {
    units <- match.arg(digits)
    as.IDate(switch(units,
                    weeks  = round(x, "year") + 7 * (yday(x) %/% 7),
                    months = ISOdate(year(x), month(x), 1),
                    quarters = ISOdate(year(x), 3 * (quarter(x)-1) + 1, 1),
                    years = ISOdate(year(x), 1, 1)))
}

#Adapted from `+.Date`
`+.IDate` <- function (e1, e2) {
    if (nargs() == 1L) 
        return(e1)
    if (inherits(e1, "difftime") || inherits(e2, "difftime"))
        stop("difftime objects may not be added to IDate. Use plain integer instead of difftime.")
    if ( (storage.mode(e1)=="double" && isReallyReal(e1)) ||
         (storage.mode(e2)=="double" && isReallyReal(e2)) ) {
        return(`+.Date`(e1,e2))
        # IDate doesn't support fractional days; revert to base Date
    }
    if (inherits(e1, "Date") && inherits(e2, "Date")) 
        stop("binary + is not defined for \"IDate\" objects")
    structure(as.integer(unclass(e1) + unclass(e2)), class = c("IDate", "Date"))
}

`-.IDate` <- function (e1, e2) {
    if (!inherits(e1, "IDate")) 
        stop("can only subtract from \"IDate\" objects")
    if (storage.mode(e1) != "integer")
        stop("Internal error: storage mode of IDate is somehow no longer integer")
    if (nargs() == 1) 
        stop("unary - is not defined for \"IDate\" objects")
    if (inherits(e2, "difftime"))
        stop("difftime objects may not be subtracted from IDate. Use plain integer instead of difftime.")
    if ( storage.mode(e2)=="double" && isReallyReal(e2) ) {
        return(`-.Date`(as.Date(e1),as.Date(e2)))
        # IDate doesn't support fractional days so revert to base Date
    }
    ans = as.integer(unclass(e1) - unclass(e2))
    if (!inherits(e2, "Date")) class(ans) = c("IDate","Date")
    return(ans)
}



###################################################################
# ITime -- Integer time-of-day class
#          Stored as seconds in the day
###################################################################

as.ITime <- function(x, ...) UseMethod("as.ITime")

as.ITime.default <- function(x, ...) {
    as.ITime(as.POSIXlt(x, ...))
}

as.ITime.character <- function(x, format, ...) {
    x <- unclass(x)
    if (!missing(format)) 
        return(as.ITime(strptime(x, format = format, ...)))
    y <- strptime(x, format = "%H:%M:%OS", ...)
    y.nas <- is.na(y)
    y[y.nas] <- strptime(x[y.nas], format = "%H:%M", ...)

    return(as.ITime(y))
}

as.ITime.POSIXlt <- function(x, ...) {
    structure(with(x, as.integer(sec) + min * 60L + hour * 3600L),
              class = "ITime")
}

as.character.ITime <- format.ITime <- function(x, ...) {
    # adapted from chron's format.times
    # Fix for #811. Thanks to @StefanFritsch for the code snippet
    neg <- x < 0L
    x  <- abs(unclass(x))
    hh <- x %/% 3600L
    mm <- (x - hh * 3600L) %/% 60L
    # #2171 -- trunc gives numeric but %02d requires integer;
    #   as.integer is also faster (but doesn't handle integer overflow)
    #   http://stackoverflow.com/questions/43894077
    ss <- as.integer(x - hh * 3600L - 60L * mm)
    res = sprintf('%02d:%02d:%02d', hh, mm, ss)
    # Fix for #1354, so that "NA" input is handled correctly.
    if (is.na(any(neg))) res[is.na(x)] = NA
    neg = which(neg)
    if (length(neg)) res[neg] = paste("-", res[neg], sep="")
    res
}

as.data.frame.ITime <- function(x, ...) {
    # This method is just for ggplot2, #1713
    # Avoids the error "cannot coerce class '"ITime"' into a data.frame", but for some reason
    # ggplot2 doesn't seem to call the print method to get axis labels, so still prints integers.
    # Tried converting to POSIXct but that gives the error below.
    # If user converts to POSIXct themselves, then it works for some reason.
    ans = list(x)
    # ans = list(as.POSIXct(x,tzone=""))  # ggplot2 gives "Error: Discrete value supplied to continuous scale"
    setattr(ans,"class","data.frame")
    setattr(ans,"row.names", .set_row_names(length(x)))
    setattr(ans,"names",NULL)
    ans
}

print.ITime <- function(x, ...) {
    print(format(x))
}

rep.ITime <- function (x, ...) 
{
    y <- rep(unclass(x), ...)
    structure(y, class = "ITime")
}

"[.ITime" <- function(x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

unique.ITime <- function(x, ...) {
    ans = NextMethod()
    setattr(ans,"class","ITime")
    ans
}

# create a data.table with IDate and ITime columns
#   should work for most date/time formats like chron or POSIXct

IDateTime <- function(x, ...) UseMethod("IDateTime")
IDateTime.default <- function(x, ...) {
    data.table(idate = as.IDate(x), itime = as.ITime(x))
}

# POSIXt support

as.POSIXct.IDate <- function(x, tz = "UTC", time = 0, ...) {
    if (missing(time) && inherits(tz, "ITime")) {
        time <- tz # allows you to use time as the 2nd argument
        tz <- "UTC"
    }
    if (tz == "") tz <- "UTC"
    as.POSIXct(as.POSIXlt(x, ...), tz, ...) + time
}

as.POSIXct.ITime <- function(x, tz = "UTC", date = as.Date(Sys.time()), ...) {
    if (missing(date) && any(class(tz) %in% c("Date", "IDate", "POSIXt", "dates"))) {
        date <- tz # allows you to use date as the 2nd argument
        tz <- "UTC"
    }
    as.POSIXct(as.POSIXlt(date), tz = tz) + x
}

as.POSIXlt.ITime <- function(x, ...) {
    as.POSIXlt(as.POSIXct(x, ...))
}

# chron support

as.chron.IDate <- function(x, time = NULL, ...) {
    if(!requireNamespace("chron", quietly = TRUE)) stop("Install suggested `chron` package to use `as.chron.IDate` function.") else {
        if (!is.null(time)) {
            chron::chron(dates. = chron::as.chron(as.Date(x)), times. = chron::as.chron(time))
        } else {
            chron::chron(dates. = chron::as.chron(as.Date(x)))
        }    
    }
}

as.chron.ITime <- function(x, date = NULL, ...) {
    if(!requireNamespace("chron", quietly = TRUE)) stop("Install suggested `chron` package to use `as.chron.ITime` function.") else {
        if (!is.null(date)) {
            chron::chron(dates. = chron::as.chron(as.Date(date)), times. = chron::as.chron(x))
        } else {
            chron::chron(times. = as.character(x))
        }  
    }
}

as.ITime.times <- function(x, ...) {
    x <- unclass(x)
    daypart <- x - floor(x)
    secs <- as.integer(round(daypart * 86400))
    structure(secs,
              class = "ITime")
}

###################################################################
# Date - time extraction functions
#   Adapted from Hadley Wickham's routines cited below to ensure
#   integer results.
#     http://gist.github.com/10238
#   See also Hadley's more advanced and complex lubridate package:
#     http://github.com/hadley/lubridate
#   lubridate routines do not return integer values.
###################################################################

second  <- function(x) as.integer(as.POSIXlt(x)$sec)
minute  <- function(x) as.POSIXlt(x)$min
hour    <- function(x) as.POSIXlt(x)$hour
yday    <- function(x) as.POSIXlt(x)$yday + 1L
wday    <- function(x) (unclass(as.IDate(x)) + 4L) %% 7L + 1L
mday    <- function(x) as.POSIXlt(x)$mday
week    <- function(x) yday(x) %/% 7L + 1L
isoweek <- function(x) {
  #ISO 8601-conformant week, as described at
  #  https://en.wikipedia.org/wiki/ISO_week_date
  
  #Approach:
  # * Find nearest Thursday to each element of x
  # * Find the number of weeks having passed between
  #   January 1st of the year of the nearest Thursdays and x
  
  xlt <- as.POSIXlt(x)
  
  #We want Thursday to be 3 (4 by default in POSIXlt), so
  #  subtract 1 and re-divide; also, POSIXlt increment by seconds
  nearest_thurs <- xlt + (3 - ((xlt$wday - 1) %% 7)) * 86400
  
  year_start <- as.POSIXct(paste0(as.POSIXlt(nearest_thurs)$year + 1900L, "-01-01"))
  
  as.integer(1 + unclass(difftime(nearest_thurs, year_start, units = "days")) %/% 7)
}
month   <- function(x) as.POSIXlt(x)$mon + 1L
quarter <- function(x) as.POSIXlt(x)$mon %/% 3L + 1L
year    <- function(x) as.POSIXlt(x)$year + 1900L


