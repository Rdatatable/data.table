# Fuzz harness slots for rolling window statistics, NA filling, coalescing,
# conditional, range, shift, and integer date/time C primitives:
#   src/froll.c, src/frollR.c, src/frolladaptive.c, src/nafill.c,
#   src/coalesce.c, src/fifelse.c, src/between.c, src/shift.c, src/idatetime.c.

list(
  # 1: frollmean and frollsum (fast vs exact, na.rm, align)
  function(x) {
    n <- as.numeric(x)
    frollmean(n, c(1L, 3L, 5L), algo = "fast", na.rm = FALSE)
    frollmean(n, c(2L, 4L), algo = "exact", na.rm = TRUE, align = "center")
    frollsum(n, 3L, algo = "fast", align = "left", partial = TRUE)
    frollsum(n, 3L, algo = "exact", na.rm = TRUE)
  },

  # 2: frollmax, frollmin, frollprod
  function(x) {
    n <- as.numeric(x)
    frollmax(n, c(2L, 5L), algo = "fast", na.rm = TRUE)
    frollmax(n, 3L, algo = "exact", align = "left")
    frollmin(n, c(2L, 4L), algo = "fast", na.rm = FALSE)
    frollmin(n, 3L, algo = "exact", na.rm = TRUE)
    frollprod(n, 3L, algo = "fast", na.rm = TRUE)
    frollprod(n, 3L, algo = "exact", na.rm = FALSE)
  },

  # 3: frollmedian, frollvar, frollsd
  function(x) {
    n <- as.numeric(x)
    frollmedian(n, c(3L, 4L), algo = "fast", na.rm = TRUE)
    frollmedian(n, 3L, algo = "exact", na.rm = FALSE)
    frollvar(n, 3L, algo = "fast", na.rm = TRUE)
    frollvar(n, 3L, algo = "exact", na.rm = FALSE)
    frollsd(n, 3L, algo = "fast", na.rm = TRUE)
    frollsd(n, 3L, algo = "exact", na.rm = FALSE)
  },

  # 4: Adaptive rolling windows across all froll functions
  function(x) {
    n <- as.numeric(x)
    w <- pmax(1L, abs(as.integer(n)) %% 10L)
    w[is.na(w)] <- 2L
    frollmean(n, w, adaptive = TRUE, algo = "fast", na.rm = TRUE)
    frollmean(n, w, adaptive = TRUE, algo = "exact", align = "left")
    frollsum(n, w, adaptive = TRUE, partial = TRUE)
    frollmax(n, w, adaptive = TRUE, na.rm = TRUE)
    frollmin(n, w, adaptive = TRUE, na.rm = TRUE)
    frollmedian(n, w, adaptive = TRUE, na.rm = TRUE)
    frollvar(n, w, adaptive = TRUE, na.rm = TRUE)
    frollsd(n, w, adaptive = TRUE, na.rm = TRUE)
  },

  # 5: frolladapt helper on irregularly spaced integer index
  function(x) {
    i <- unique(sort(abs(as.integer(as.numeric(x))) %% 1000L))
    if (length(i) > 0L) {
      data.table:::frolladapt(i, 5L, partial = FALSE)
      data.table:::frolladapt(i, c(3L, 7L), partial = TRUE, give.names = TRUE)
    }
  },

  # 6: nafill and setnafill (const, locf, nocb) on numeric, integer, integer64
  function(x) {
    n <- as.numeric(x)
    i <- as.integer(n)
    i64 <- n
    class(i64) <- "integer64"
    nafill(n, type = "const", fill = 0)
    nafill(n, type = "locf")
    nafill(n, type = "nocb")
    nafill(i, type = "locf", nan = NA)
    dt <- data.table(n = n, i = i, i64 = i64)
    setnafill(dt, type = "locf")
    setnafill(dt, type = "nocb")
    setnafill(dt, type = "const", fill = 1L)
  },

  # 7: fcoalesce across character, numeric, integer
  function(x) {
    n <- as.numeric(x)
    i <- as.integer(n)
    x_na <- x
    x_na[!nzchar(x_na)] <- NA_character_
    fcoalesce(x_na, rev(x_na))
    fcoalesce(n, rev(n), 0)
    fcoalesce(i, rev(i), 0L)
  },

  # 8: fifelse and fcase across types
  function(x) {
    n <- as.numeric(x)
    cnd <- (n > 0)
    fifelse(cnd, x, rev(x), na = "<NA>")
    fifelse(cnd, n, -n, na = 0)
    fcase(n < 0, "neg", n == 0, "zero", n > 0, "pos", default = "na")
  },

  # 9: between and inrange
  function(x) {
    n <- as.numeric(x)
    lo <- pmin(n, rev(n))
    hi <- pmax(n, rev(n))
    between(n, lo, hi, incbounds = TRUE, NAbounds = TRUE)
    between(n, lo, hi, incbounds = FALSE, NAbounds = NA)
    s <- lo[!is.na(lo) & !is.na(hi)]
    e <- hi[!is.na(lo) & !is.na(hi)]
    if (length(s) > 0L) inrange(n, s, e, incbounds = TRUE)
  },

  # 10: shift (lag, lead, cyclic) across vector types
  function(x) {
    n <- as.numeric(x)
    shift(x, n = c(0L, 1L, -1L, 3L), fill = "", type = "lag")
    shift(n, n = c(1L, 2L), type = "lead")
    shift(x, n = c(1L, -2L), type = "cyclic")
  },

  # 11: froll on data.table columns with give.names = TRUE
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(a = n, b = rev(n))
    frollmean(dt, c(2L, 3L), give.names = TRUE, na.rm = TRUE)
    frollsum(dt, 2L, give.names = TRUE, partial = TRUE)
  },

  # 12: Multi-threaded froll
  function(x) {
    setDTthreads(2L)
    on.exit(setDTthreads(1L))
    n <- as.numeric(rep(x, length.out = 200L))
    frollmean(list(n, rev(n)), c(3L, 5L), algo = "exact", na.rm = TRUE)
  },

  # 13: IDate conversions, CconvertDate extractors, and round.IDate (src/idatetime.c)
  function(x) {
    i <- as.integer(as.numeric(x))
    # Clamp finite values to valid proleptic Gregorian day range while preserving NAs
    ok <- !is.na(i)
    i[ok] <- i[ok] %% 200000L
    d <- as.IDate(i, origin = "1970-01-01")
    year(d)
    month(d)
    mday(d)
    yday(d)
    wday(d)
    quarter(d)
    week(d)
    isoweek(d)
    yearmon(d)
    yearqtr(d)
    for (u in c("weeks", "months", "quarters", "years")) {
      round(d, u)
    }
    c(d, rev(d))
    unique(d)
  },

  # 14: ITime, IDateTime, and POSIXct round-trips (R/IDateTime.R)
  function(x) {
    n <- as.numeric(x)
    ok <- is.finite(n)
    n_sec <- rep(NA_real_, length(n))
    n_sec[ok] <- abs(n[ok]) %% 86400
    it <- as.ITime(n_sec)
    hour(it)
    minute(it)
    second(it)
    as.ITime(x)
    p <- as.POSIXct(ifelse(ok, n %% 1e9, NA_real_), origin = "1970-01-01", tz = "UTC")
    idt <- IDateTime(p)
    as.POSIXct(idt$idate, time = idt$itime, tz = "UTC")
  }
)

