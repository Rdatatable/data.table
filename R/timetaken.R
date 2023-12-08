timetaken = function(started.at)
{
  if (!inherits(started.at,"proc_time")) stopf("Use started.at=proc.time() not Sys.time() (POSIXt and slow)")  # nocov
  format = function(secs) {
    if (secs > 60.0) {
      secs = as.integer(secs)
      sprintf("%02d:%02d:%02d", secs%/%3600L, (secs%/%60L)%%60L, secs%%60L)
    } else {
      sprintf(if (secs >= 10.0) "%.1fs" else "%.3fs", secs)
    }
  }
  tt = proc.time()-started.at  # diff all 3 times
  paste0(format(tt[3L])," elapsed (", format(tt[1L]), " cpu)")
}

