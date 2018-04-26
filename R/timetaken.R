timetaken <- function(started.at)
{
   if (!inherits(started.at,"proc_time")) stop("Use started.at=proc.time() (faster) not Sys.time() (POSIXt and slow)")
   secs = proc.time()[3L] - started.at[3L]
   mins = as.integer(secs) %/% 60L
   hrs = mins %/% 60L
   days = hrs %/% 24L
   mins = mins - hrs * 60L
   hrs = hrs - days * 24L
   if (secs > 60.0) {
     res = if (days>=1L) paste0(days," day", if (days>1L) "s " else " ") else ""
     paste0(res,sprintf("%02d:%02d:%02d", hrs, mins, as.integer(secs) %% 60L))
   } else {
     sprintf(if (secs >= 10.0) "%.1fsec" else "%.3fsec", secs)
   }
}

