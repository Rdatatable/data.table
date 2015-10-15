# IPeriod integer based class for time periods.
# methods provided for POSIXct, Date, IDate, ITime, IDateTime

# - [x] rounds faster and more flexible for IDateTime column pair, returns new integer type (similarly to `IDate` and `ITime`) which simply presents time groups as `as.numeric(POSIXct(.)) %/% seconds_in_time_period` + 1 by default to make *ceiling*.
# - [x] potentially more speedup when setting tight `origin` argument, due to integer values < 100k

# helpers ---------------------------------------------------------------

unit.secs <- function() c("secs" = 1L, "mins" = 60L, "hours" = 3600L, "days" = 86400L)
is.IDateTime <- function(x) is.data.table(x) && "idate" %in% names(x) && "itime" %in% names(x) && inherits(x[["idate"]], "IDate") && inherits(x[["itime"]], "ITime")

# as.IPeriod.Class ----------------------------------------------------------

IPeriod <- function(x, unit, amount, origin, type){
    if(type=="up") type="ceiling"
    if(type=="down") type="floor"
    offset = switch(type, "ceiling"=1L, "floor"=0L)
    structure(x + offset, unit = unit, amount = amount, origin = origin, type = type, class = "IPeriod")
}

as.IPeriod <- function(x, ...) UseMethod("as.IPeriod")

as.IPeriod.default <- function(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", ...){
    as.IPeriod(as.POSIXct(x), unit, amount, origin, type)
}

as.IPeriod.POSIXct <- function(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", ...){
    as.IPeriod(as.IDate(x), unit, amount, origin, type, itime = as.ITime(x))
}

as.IPeriod.numeric <- function(x, unit, amount, origin, type, ...){
    if(!is.integer(x)) x = as.integer(x)
    if(!is.integer(amount)) amount = as.integer(amount)
    IPeriod(x, unit, amount, origin, type)
}

as.IPeriod.data.table <- function(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", ...){
    if(!is.IDateTime(x)) stop("data.table method for as.IPeriod accept only IDateTime() compatible data.table. Fields named 'idate' and 'itime' of types IDate and ITime.")
    as.IPeriod(x$idate, unit, amount, origin, type, itime = x$itime)
}

as.IPeriod.IDate <- as.IPeriod.IDateTime <- function(x, unit, amount = 1L, origin = "1970-01-01", type = "ceiling", itime = 0L, ...){
    if(!is.integer(amount)) amount = as.integer(amount)
    stopifnot(is.integer(x), is.integer(itime), is.character(unit), is.integer(amount), is.character(origin), is.character(type))
    secs_in_iunit = amount * unit.secs()[[unit]]
    iunits_in_day = 86400L / secs_in_iunit
    if(iunits_in_day!=as.integer(iunits_in_day)) stop("Multiple time unit periods must fully fit into days cycles, e.g. for valid hours amount will be 1, 2, 3, 4, 6, 8, 12, 24+(?) hours, not 5 or 10 hours.")
    iunits_in_day = as.integer(iunits_in_day)
    idate_n = (unclass(x) - as.integer(as.Date(origin))) * iunits_in_day
    itime_n = unclass(itime) %/% secs_in_iunit
    as.IPeriod(idate_n + itime_n, unit, amount, origin, type)
}

periodize <- function(idate, itime = 0L, unit, amount = 1L, origin = "1970-01-01", type = "ceiling"){
    sc = sys.call()
    if(inherits(idate, "POSIXct")) idate = IDateTime(idate)
    # Argument offset when idate is IDateTime/POSIXct: unit=itime, amount=unit
    # Allows `periodize(IDateTime(x), "hours", 12L)` or `periodize(Sys.time(), "hours", 12L)`
    if(is.IDateTime(idate)){
        if(is.character(itime)){
            if(!missing(unit) && is.numeric(unit)){
                if(!"amount" %in% names(sc)) amount = unit
            }
            if(!"unit" %in% names(sc)) unit = itime
        }
        if("itime" %in% names(sc) || inherits(itime, "ITime")) warning("Argument 'itime' will be ignored. Periodize will use ITime column from IDateTime data.table provided to 'idate'.")
        itime = idate$itime
        idate = idate$idate
    }
    if(!is.integer(amount)) amount = as.integer(amount)
    stopifnot(inherits(idate, "IDate"), inherits(itime, "ITime"), is.character(unit), is.integer(amount), is.character(origin), is.character(type))
    as.IPeriod(x = idate, unit = unit, amount = amount, origin = origin, type = type, itime = itime)
}

# as.Class.IPeriod ----------------------------------------------------------

as.POSIXct.IPeriod <- function(x, tz = "", ...){
    attrs = attributes(x)
    secs_in_iunit = attrs$amount * unit.secs()[[attrs$unit]]
    as.POSIXct(c(unclass(x) * secs_in_iunit), tz = tz, origin = attrs$origin)
}

as.IDate.IPeriod <- function(x, ...){
    attrs = attributes(x)
    secs_in_iunit = attrs$amount * unit.secs()[[attrs$unit]]
    iunits_in_day = 86400L %/% secs_in_iunit
    as.IDate(unclass(x) %/% iunits_in_day, origin = attrs$origin)
}

as.ITime.IPeriod <- function(x, ...){
    attrs = attributes(x)
    secs_in_iunit = attrs$amount * unit.secs()[[attrs$unit]]
    iunits_in_day = 86400L / secs_in_iunit
    structure((unclass(x) %% iunits_in_day) * secs_in_iunit, class = "ITime")
    # to be simplified when as.ITime.numeric #1393 merged, now it would use POSIXlt
    #as.ITime((unclass(x) %% iunits_in_day) * secs_in_iunit)
}

as.character.IPeriod <- format.IPeriod <- function(x, ...){
    format(as.POSIXct(x), ...)
}

as.factor.IPeriod <- function(x){
    attrs = attributes(x)
    lastip = max(x, na.rm = TRUE)
    if(identical(attrs$origin,"1970-01-01")){
        if(lastip >= 1e5){
            warning(sprintf("You are going to create a factor of %s levels. You may want to change `periodize` `origin` argument. Read ?as.factor.IPeriod", lastip+1L), immediate. = TRUE)
        }
    }
    iip = seq(0L, lastip) # always return from origin including origin (so +1L length of levels), no matter if ceiling or floor
    attributes(iip) <- attrs
    f = as.integer(x) + 1L # add 1L due to added 0 address to the levels
    levels(f) <- format(iip)
    class(f) <- "factor"
    return(f)
}

IDateTime.IPeriod <- function(x, ...){
    data.table(idate = as.IDate(x), itime = as.ITime(x))
}

# methods.IPeriod ------------------------------------------------------------

print.IPeriod <- function(x, ...){
    print(format(x, ...))
}

rep.IPeriod <- function(x, ...){
    attrs <- attributes(x)
    y <- rep(unclass(x), ...)
    attributes(y) <- attrs
    structure(y, class = "IPeriod")
}

"[.IPeriod" <- function(x, ..., drop = TRUE){
    attrs <- attributes(x)
    class(x) <- NULL
    val <- NextMethod("[")
    attributes(val) <- attrs
    val
}

unique.IPeriod <- function(x, ...) {
    attrs <- attributes(x)
    ans <- NextMethod()
    attributes(ans) <- attrs
    ans
}
