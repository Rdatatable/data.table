# Fuzz harness slots for data.table::fwrite() and round-trips (src/fwrite.c, src/fwriteR.c).
#
# Input mode: "lines" (x is a character vector split on '\n'; .fuzz_file is a
# per-process scratch file path in R_GlobalEnv).

list(
  # 1: Mixed-type data.table with auto-quoting + fread round-trip
  function(x) {
    n <- as.numeric(x)
    i <- as.integer(n)
    l <- (i %% 2L == 0L)
    dt <- data.table(s = x, n = n, i = i, l = l)
    fwrite(dt, .fuzz_file)
    fread(file = .fuzz_file)
  },

  # 2: Quoting and escape qmethod variants
  function(x) {
    dt <- data.table(a = x, b = rev(x))
    fwrite(dt, .fuzz_file, quote = TRUE, qmethod = "escape")
    fwrite(dt, .fuzz_file, quote = TRUE, qmethod = "double")
    fwrite(dt, .fuzz_file, quote = FALSE)
  },

  # 3: European decimal comma, semicolon sep, custom na and eol
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(x = x, n = n)
    fwrite(dt, .fuzz_file, sep = ";", dec = ",", na = "NULL", eol = "\r\n")
    fread(file = .fuzz_file, sep = ";", dec = ",", na.strings = "NULL")
  },

  # 4: Numeric scipen formatting extremes
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(n = n, m = -n)
    fwrite(dt, .fuzz_file, scipen = 0L)
    fwrite(dt, .fuzz_file, scipen = 999L)
    fwrite(dt, .fuzz_file, scipen = -5L)
  },

  # 5: Factors, logical01, row.names, BOM
  function(x) {
    f <- factor(x)
    l <- !is.na(as.numeric(x))
    dt <- data.table(f = f, l = l)
    fwrite(dt, .fuzz_file, logical01 = TRUE, row.names = TRUE, bom = TRUE)
    fread(file = .fuzz_file)
  },

  # 6: Date and POSIXct serialization modes
  function(x) {
    n <- as.numeric(x)
    n[!is.finite(n)] <- 0
    d <- as.Date(n %% 50000, origin = "1970-01-01")
    p <- as.POSIXct(n %% 2e9, origin = "1970-01-01", tz = "UTC")
    dt <- data.table(d = d, p = p)
    fwrite(dt, .fuzz_file, dateTimeAs = "ISO")
    fwrite(dt, .fuzz_file, dateTimeAs = "squash")
    fwrite(dt, .fuzz_file, dateTimeAs = "epoch")
    fwrite(dt, .fuzz_file, dateTimeAs = "write.csv")
  },

  # 7: List columns with sep2
  function(x) {
    k <- seq_len(min(50L, length(x)))
    lcol <- lapply(k, function(idx) x[seq_len(min(3L, idx))])
    dt <- data.table(id = k, items = lcol)
    fwrite(dt, .fuzz_file, sep2 = c("", "|", ""))
  },

  # 8: Complex numbers and integer64-classed reals
  function(x) {
    n <- as.numeric(x)
    z <- complex(real = n, imaginary = rev(n))
    i64 <- n
    class(i64) <- "integer64"
    dt <- data.table(z = z, i64 = i64)
    fwrite(dt, .fuzz_file)
  },

  # 9: Streaming gzip compression in src/fwrite.c
  function(x) {
    dt <- data.table(a = x, b = as.numeric(x))
    fwrite(dt, .fuzz_file, compress = "gzip")
  },

  # 10: Append mode and col.names = FALSE
  function(x) {
    dt <- data.table(a = x)
    fwrite(dt, .fuzz_file, col.names = TRUE)
    fwrite(dt, .fuzz_file, append = TRUE, col.names = FALSE)
    fread(file = .fuzz_file)
  },

  # 11: UTF-8 and Latin-1 marked strings
  function(x) {
    u <- x
    Encoding(u) <- ifelse(validUTF8(u), "UTF-8", "unknown")
    l <- x
    Encoding(l) <- "latin1"
    dt <- data.table(u = u, l = l)
    fwrite(dt, .fuzz_file)
    fread(file = .fuzz_file, encoding = "UTF-8")
  },

  # 12: Multi-threaded fwrite
  function(x) {
    dt <- data.table(
      a = rep(x, length.out = 200L),
      b = as.numeric(rep(x, length.out = 200L))
    )
    fwrite(dt, .fuzz_file, nThread = 2L)
  }
)
