# Fuzz harness slots for binary merge, rolling joins, non-equi joins,
# interval overlaps, and set operations (src/bmerge.c, src/ijoin.c, R/setops.R).

list(
  # 1: Character equi-joins with mult and nomatch variants
  function(x) {
    h <- ceiling(length(x) / 2)
    x1 <- head(x, h)
    x2 <- tail(x, -h)
    if (!length(x2)) x2 <- x1
    d1 <- data.table(k = x1, v = seq_along(x1))
    d2 <- data.table(k = x2, w = rev(seq_along(x2)))
    d1[d2, on = "k", nomatch = NA, allow.cartesian = TRUE]
    d1[d2, on = "k", nomatch = NULL, mult = "first"]
    d1[d2, on = "k", mult = "last"]
  },

  # 2: Rolling joins on numeric keys (roll = TRUE, -Inf, "nearest", numeric)
  function(x) {
    n <- as.numeric(x)
    h <- ceiling(length(n) / 2)
    n1 <- head(n, h)
    n2 <- tail(n, -h)
    if (!length(n2)) n2 <- n1
    d1 <- data.table(g = head(x, h), t = n1, v = seq_along(n1))
    d2 <- data.table(g = tail(x, length(n2)), t = n2)
    d1[d2, on = .(g, t), roll = TRUE]
    d1[d2, on = .(g, t), roll = -Inf]
    d1[d2, on = .(g, t), roll = "nearest"]
    d1[d2, on = .(g, t), roll = 2.5, rollends = c(TRUE, TRUE)]
    d1[d2, on = .(g, t), roll = -2.5, rollends = c(FALSE, FALSE)]
  },

  # 3: Rolling joins on integer keys
  function(x) {
    i <- as.integer(as.numeric(x))
    h <- ceiling(length(i) / 2)
    i1 <- head(i, h)
    i2 <- tail(i, -h)
    if (!length(i2)) i2 <- i1
    d1 <- data.table(t = i1, v = seq_along(i1))
    d2 <- data.table(t = i2)
    d1[d2, on = "t", roll = TRUE]
    d1[d2, on = "t", roll = "nearest"]
    d1[d2, on = "t", roll = 5L, rollends = c(TRUE, FALSE)]
  },

  # 4: Non-equi joins (src/bmerge.c non-equi binary search)
  function(x) {
    n <- as.numeric(x)
    n[is.na(n)] <- 0
    h <- ceiling(length(n) / 2)
    n1 <- head(n, h)
    n2 <- tail(n, -h)
    if (!length(n2)) n2 <- n1
    d1 <- data.table(g = head(x, h), a = n1, b = n1 + abs(rev(n1)))
    d2 <- data.table(g = tail(x, length(n2)), x = n2, y = n2 + 1)
    d1[d2, on = .(g, a <= x, b >= y), allow.cartesian = TRUE]
    d1[d2, on = .(a < x), mult = "first"]
    d1[d2, on = .(b >= x), mult = "last", nomatch = NULL]
  },

  # 5: Join with by = .EACHI aggregation
  function(x) {
    n <- as.numeric(x)
    h <- ceiling(length(x) / 2)
    x1 <- head(x, h)
    x2 <- tail(x, -h)
    if (!length(x2)) x2 <- x1
    d1 <- data.table(k = x1, v = head(n, h))
    d2 <- data.table(k = x2, w = seq_along(x2))
    d1[d2, .(cnt = .N, s = sum(v, na.rm = TRUE)), on = "k", by = .EACHI]
  },

  # 6: In-place update join (:= with on=)
  function(x) {
    h <- ceiling(length(x) / 2)
    x1 <- head(x, h)
    x2 <- tail(x, -h)
    if (!length(x2)) x2 <- x1
    d1 <- data.table(k = x1, v = seq_along(x1))
    d2 <- data.table(k = x2, w = rev(seq_along(x2)))
    d1[d2, v := i.w, on = "k"]
  },

  # 7: merge.data.table (inner, left, right, full outer)
  function(x) {
    h <- ceiling(length(x) / 2)
    x1 <- head(x, h)
    x2 <- tail(x, -h)
    if (!length(x2)) x2 <- x1
    d1 <- data.table(k = x1, v1 = seq_along(x1))
    d2 <- data.table(k = x2, v2 = seq_along(x2))
    merge(d1, d2, by = "k", all = TRUE, allow.cartesian = TRUE)
    merge(d1, d2, by = "k", all.x = TRUE, allow.cartesian = TRUE)
    merge(d1, d2, by = "k", all.y = TRUE, allow.cartesian = TRUE)
  },

  # 8: foverlaps interval joins (type = any, within, start, end, equal)
  function(x) {
    n <- as.numeric(x)
    n[!is.finite(n)] <- 0
    h <- ceiling(length(n) / 2)
    n1 <- head(n, h)
    n2 <- tail(n, -h)
    if (!length(n2)) n2 <- n1
    s1 <- pmin(n1, rev(n1))
    e1 <- pmax(n1, rev(n1))
    s2 <- pmin(n2, rev(n2))
    e2 <- pmax(n2, rev(n2))
    d1 <- data.table(start = s1, end = e1, val = seq_along(s1))
    d2 <- data.table(start = s2, end = e2)
    setkey(d2, start, end)
    foverlaps(d1, d2, type = "any", mult = "all")
    foverlaps(d1, d2, type = "within", mult = "first")
    foverlaps(d1, d2, type = "start", mult = "last", nomatch = NULL)
    foverlaps(d1, d2, type = "end", which = TRUE)
    foverlaps(d1, d2, type = "equal")
  },

  # 9: Integer64 joins
  function(x) {
    n <- as.numeric(x)
    class(n) <- "integer64"
    h <- ceiling(length(n) / 2)
    n1 <- head(n, h)
    n2 <- tail(n, -h)
    if (!length(n2)) n2 <- n1
    d1 <- data.table(k = n1, v = seq_along(n1))
    d2 <- data.table(k = n2)
    d1[d2, on = "k", mult = "first"]
    d1[d2, on = "k", roll = TRUE]
  },

  # 10: Multi-column composite key joins (character + factor + integer)
  function(x) {
    i <- as.integer(as.numeric(x))
    f <- factor(x)
    h <- ceiling(length(x) / 2)
    d1 <- data.table(a = head(x, h), b = head(f, h), c = head(i, h))
    d2 <- data.table(a = tail(x, h), b = tail(f, h), c = tail(i, h))
    setkey(d1, a, b, c)
    setkey(d2, a, b, c)
    d1[d2, allow.cartesian = TRUE]
  },

  # 11: Set operations: fintersect, funion, fsetdiff, fsetequal
  function(x) {
    n <- as.numeric(x)
    h <- ceiling(length(x) / 2)
    d1 <- data.table(a = head(x, h), b = head(n, h))
    d2 <- data.table(a = tail(x, h), b = tail(n, h))
    fintersect(d1, d2)
    fintersect(d1, d2, all = TRUE)
    funion(d1, d2)
    funion(d1, d2, all = TRUE)
    fsetdiff(d1, d2)
    fsetdiff(d1, d2, all = TRUE)
    fsetequal(d1, d2)
    fsetequal(d1, d2, all = FALSE)
  },

  # 12: Cross-encoding character joins (UTF-8 vs Latin-1)
  function(x) {
    u <- x
    Encoding(u) <- ifelse(validUTF8(u), "UTF-8", "unknown")
    l <- rev(x)
    Encoding(l) <- "latin1"
    d1 <- data.table(k = u, v = seq_along(u))
    d2 <- data.table(k = l)
    d1[d2, on = "k", allow.cartesian = TRUE]
  }
)
