# Fuzz harness slots for radix ordering, sorting, ranking, uniqueness,
# string hashing, and grouped aggregations:
#   src/forder.c, src/fsort.c, src/frank.c, src/uniqlist.c, src/chmatch.c,
#   src/dogroups.c, src/gsumm.c.

list(
  # 1: Character radix order (ascending, descending, na.last variants)
  function(x) {
    dt <- data.table(a = x)
    setorder(dt, a, na.last = TRUE)
    setorder(dt, -a, na.last = FALSE)
    dt[order(a, na.last = NA)]
  },

  # 2: Numeric radix order (including -0.0, NaN, Inf, -Inf, denormals)
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(n = n)
    setorder(dt, n, na.last = TRUE)
    setorder(dt, -n, na.last = FALSE)
    dt[order(n, na.last = NA)]
  },

  # 3: Integer and logical radix order (counting sort vs radix range branches)
  function(x) {
    i <- as.integer(as.numeric(x))
    l <- (i %% 3L == 0L)
    dt <- data.table(i = i, l = l)
    setorder(dt, i, -l, na.last = TRUE)
    setorder(dt, -i, l, na.last = FALSE)
  },

  # 4: 64-bit integer (bit64::integer64 representation on REALSXP) radix order
  function(x) {
    n <- as.numeric(x)
    class(n) <- "integer64"
    dt <- data.table(i64 = n)
    setorder(dt, i64, na.last = TRUE)
    setorder(dt, -i64, na.last = FALSE)
  },

  # 5: Multi-column mixed-type radix order + setkey / setindex
  function(x) {
    n <- as.numeric(x)
    i <- as.integer(n)
    dt <- data.table(s = x, n = n, i = rev(i))
    setkey(dt, s, n)
    setindex(dt, n, i)
    setorderv(dt, c("s", "n", "i"), order = c(1L, -1L, 1L))
  },

  # 6: fsort on numeric vectors (src/fsort.c)
  function(x) {
    n <- as.numeric(x)
    n_pos <- n[!is.na(n) & n >= 0]
    if (length(n_pos) > 0L) fsort(n_pos)
    fsort(n, decreasing = FALSE, na.last = TRUE, internal = FALSE)
  },

  # 7: frank / frankv across all ties.method modes
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(x = x, n = n)
    frank(dt, ties.method = "average", na.last = "keep")
    frank(dt, ties.method = "first", na.last = TRUE)
    frank(dt, ties.method = "last", na.last = FALSE)
    frank(dt, ties.method = "dense")
    frank(dt, ties.method = "min")
    frank(dt, ties.method = "max")
    frankv(n, ties.method = "random")
  },

  # 8: unique, duplicated, anyDuplicated, uniqueN on data.table
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(a = x, b = rev(n))
    unique(dt)
    unique(dt, fromLast = TRUE)
    unique(dt, by = "a")
    duplicated(dt)
    anyDuplicated(dt)
    uniqueN(dt)
    uniqueN(dt, by = "a")
  },

  # 9: rleid and rowid run-length / group-counter primitives
  function(x) {
    n <- as.numeric(x)
    rleid(x)
    rleid(x, n)
    rowid(x)
    rowid(x, n, prefix = "id")
  },

  # 10: chmatch and %chin% string hash table
  function(x) {
    chmatch(x, rev(x))
    chmatch(rev(x), x, nomatch = 0L)
    x %chin% rev(x)
    data.table:::chorder(x)
    data.table:::chgroup(x)
  },

  # 11: UTF-8 and Latin-1 mixed-encoding radix order and chmatch
  function(x) {
    u <- x
    Encoding(u) <- ifelse(validUTF8(u), "UTF-8", "unknown")
    l <- rev(x)
    Encoding(l) <- "latin1"
    m <- c(u, l)
    dt <- data.table(m = m)
    setorder(dt, m)
    unique(dt)
    chmatch(u, l)
    u %chin% l
  },

  # 12: Factor ordering, grouping, and uniqueness
  function(x) {
    f <- factor(x)
    dt <- data.table(f = f, v = seq_along(x))
    setorder(dt, f)
    unique(dt, by = "f")
    dt[, .(cnt = .N), by = f]
  },

  # 13: Grouped aggregations (forder retgrp = TRUE + dogroups / GForce)
  function(x) {
    n <- as.numeric(x)
    i <- as.integer(n)
    dt <- data.table(g = x, n = n, i = i)
    dt[, .(
      s = sum(n, na.rm = TRUE),
      m = mean(n, na.rm = TRUE),
      mn = min(i, na.rm = TRUE),
      mx = max(i, na.rm = TRUE),
      cnt = .N,
      grp = .GRP
    ), by = g]
  },

  # 14: keyby= and ad-hoc by= expressions
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(g = x, n = n)
    dt[, .(
      med = median(n, na.rm = TRUE),
      sd = sd(n, na.rm = TRUE),
      first = first(n),
      last = last(n)
    ), keyby = .(g, neg = n < 0)]
  },

  # 15: set() and := in-place column assignment + shallow/copy
  function(x) {
    dt <- data.table(a = x)
    dt[, b := as.numeric(a)]
    if (nrow(dt) > 0L) set(dt, i = 1L, j = "a", value = "z")
    dt[, b := NULL]
    copy(dt)
  },

  # 16: Multi-threaded radix sort
  function(x) {
    setDTthreads(2L)
    on.exit(setDTthreads(1L))
    dt <- data.table(
      a = rep(x, length.out = 250L),
      b = as.numeric(rep(x, length.out = 250L))
    )
    setorder(dt, a, -b)
  }
)
