time.taken <- function(started.at)
{
   if (!is.vector(started.at)) {
      # assume started.at=Sys.time(). Slower method due to POSIXt.
      secs <- as.double(difftime(Sys.time(), started.at, units="secs"))
   } else {
      # new faster method using started.at = proc.time()
      secs = proc.time()[3] - started.at[3]
   }
   mins <- secs %/% 60
   hrs <- mins %/% 60
   days <- hrs %/% 24
   mins = mins - hrs * 60
   hrs = hrs - 24*days
   if (secs >= 60) {
       if (days >= 1) res = sprintf("%d days ", as.integer(days)) else res=""
       paste(res,sprintf("%02.0f:%02.0f:%02.0f", hrs, mins, secs %% 60),sep="")
   } else {
       sprintf("%1.4fsec", secs)
   }
}
