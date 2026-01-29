test(###
  #"fwrite: POSIXct should be written as ISO-8601, not numeric seconds",
  {
    oldtz = Sys.getenv("TZ", unset = NA)
    on.exit({
      if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)
    }, add = TRUE)
    
    Sys.setenv(TZ = "UTC")
    
    DT = data.table(
      x = seq(
        as.POSIXct("1970-01-01", tz = "UTC"),
        by = "1 sec",
        length.out = 3
      )
    )
    
    tmp = tempfile()
    fwrite(DT, tmp)
    
    out = readLines(tmp)
    
    stopifnot(
      out[1L] == "x",
      out[2L] == "1970-01-01T00:00:00Z",
      out[3L] == "1970-01-01T00:00:01Z",
      out[4L] == "1970-01-01T00:00:02Z"
    )
  })