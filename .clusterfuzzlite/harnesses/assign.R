# Fuzz harness slots for by-reference assignment, row deletion, and attribute
# mutation primitives (src/assign.c, src/deleterows.c).

list(
  # 1: Basic := add, update, and delete columns across integer/logical/negative i
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(a = x, b = n, c = seq_along(x))
    nr <- nrow(dt)
    if (nr > 0L) {
      i_raw <- as.integer(n)
      i_raw <- i_raw[!is.na(i_raw) & i_raw != -.Machine$integer.max - 1L]
      idx <- if (length(i_raw)) unique(abs(i_raw) %% nr + 1L) else 1L
      dt[idx, b := b + 1]
      dt[idx, d := paste0(a, "_new")]
      if (length(idx) > 0L && length(idx) < nr) {
        dt[-idx, c := 0L]
      }
      dt[!is.na(b) & b > 0, flag := TRUE]
      dt[, c := NULL]
    }
  },

  # 2: Sub-assignment with type coercion / promotion in Cassign (memrecycle)
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(
      lgl = as.logical(as.integer(n) %% 2L),
      int = as.integer(n),
      dbl = n,
      chr = x
    )
    nr <- nrow(dt)
    if (nr >= 2L) {
      i1 <- 1L
      i2 <- nr
      suppressWarnings({
        dt[i1, lgl := 42L]
        dt[i2, lgl := 3.14]
        dt[i1, int := 2.718]
        dt[i2, int := "coerced"]
        dt[i1, dbl := 1+2i]
      })
    }
  },

  # 3: Factor and ordered factor assignment & Csetlevels
  function(x) {
    f <- factor(x)
    o <- factor(x, ordered = TRUE)
    dt <- data.table(f = f, o = o, v = seq_along(x))
    nr <- nrow(dt)
    if (nr > 0L) {
      dt[1L, f := "new_level_xyz"]
      dt[nr, f := NA_character_]
      dt[1L, o := factor("ord_level_abc")]
      lv <- levels(dt$f)
      if (length(lv) > 0L) {
        setattr(dt$f, "levels", paste0(lv, "_s"))
      }
    }
  },

  # 4: Grouped :=, multi-column :=, and let()
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(g = x, v = n, idx = seq_along(x))
    dt[, c("m", "cnt") := .(mean(v, na.rm = TRUE), .N), by = g]
    dt[idx %% 2L == 1L, grp_rank := seq_len(.N), by = g]
    dt[, let(v2 = v * 2, m = NULL)]
  },

  # 5: Low-overhead set() across rows and columns (add, update, delete)
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(a = x, b = n)
    nr <- nrow(dt)
    if (nr > 0L) {
      rows <- seq_len(min(nr, 5L))
      for (r in rows) {
        set(dt, i = r, j = 1L, value = paste0(x[r], "!"))
        set(dt, i = r, j = "b", value = n[r] + 10)
      }
    }
    set(dt, i = NULL, j = "new_col", value = rev(x))
    set(dt, i = NULL, j = "a", value = NULL)
  },

  # 6: Row deletion by reference (.ROW := NULL / CdeleteRows) across column types
  function(x) {
    n <- as.numeric(x)
    i <- as.integer(n)
    dt <- data.table(
      chr = x,
      int = i,
      dbl = n,
      lgl = (i %% 2L == 0L),
      cplx = complex(real = ifelse(is.na(n), 0, n), imaginary = 1),
      raw = as.raw(abs(ifelse(is.na(i), 0L, i)) %% 256L),
      lst = as.list(x),
      alt = seq_along(x)
    )
    setkey(dt, chr)
    setindex(dt, int)
    nr <- nrow(dt)
    if (nr > 0L) {
      del <- which(seq_len(nr) %% 2L == 0L)
      if (length(del) > 0L) {
        dt[del, .ROW := NULL]
      }
      setallocrow(dt)
    }
    # Also exercise multi-threaded prefix sum in deleteRows
    setDTthreads(2L)
    on.exit(setDTthreads(1L))
    dt2 <- data.table(a = rep(x, length.out = 128L), b = seq_len(128L))
    dt2[b %% 3L == 0L, .ROW := NULL]
  },

  # 7: setnames() and setcolorder() with keys and secondary indices
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(a = x, b = n, c = rev(x), d = seq_along(x))
    setkey(dt, a, b)
    setindex(dt, c, d)
    setnames(dt, c("a", "c"), c("k1", " idx1 "), skip_absent = TRUE)
    setnames(dt, toupper)
    setcolorder(dt, c("D", "K1"), before = 1L)
    setcolorder(dt, "B", after = ncol(dt))
  },

  # 8: setDT(), setDF(), setalloccol(), truelength(), setattr()
  function(x) {
    n <- as.numeric(x)
    df <- data.frame(a = x, b = n, stringsAsFactors = FALSE)
    if (nrow(df) > 0L) {
      rownames(df) <- make.unique(paste0("r_", x))
    }
    setDT(df, keep.rownames = "rn", key = "a")
    truelength(df)
    setalloccol(df, 32L)
    setattr(df, "custom_attr", x)
    setDF(df)
  },

  # 9: List-column (VECSXP) assignment and recycling in Cassign
  function(x) {
    dt <- data.table(id = seq_along(x), a = x)
    nr <- nrow(dt)
    if (nr > 0L) {
      dt[, lst := as.list(a)]
      dt[1L, lst := list(list(x))]
      if (nr >= 2L) {
        dt[c(1L, nr), lst := list(1:3, letters[1:2])]
      }
    }
  },

  # 10: Sub-assignment via [<-, $<-, and [[<- methods
  function(x) {
    n <- as.numeric(x)
    dt <- data.table(a = x, b = n)
    if (nrow(dt) > 0L) {
      dt[1L, "a"] <- "replaced"
      dt[1L, 2L] <- -999
    }
    dt$c <- rev(x)
    dt[["d"]] <- seq_along(x)
    dt[["b"]] <- NULL
  },

  # 11: integer64 and Date / POSIXct coercion in Cassign memrecycle
  function(x) {
    n <- as.numeric(x)
    i64 <- n
    class(i64) <- "integer64"
    d <- as.Date(abs(as.integer(n)) %% 20000L, origin = "1970-01-01")
    dt <- data.table(i64 = i64, d = d, v = as.integer(n))
    if (nrow(dt) > 0L) {
      dt[1L, d := as.Date("2020-02-29")]
      dt[1L, v := NA_integer_]
    }
  },

  # 12: Update join := with multiple columns and nomatch rows
  function(x) {
    n <- as.numeric(x)
    h <- ceiling(length(x) / 2)
    x1 <- head(x, h)
    x2 <- tail(x, -h)
    if (!length(x2)) x2 <- x1
    d1 <- data.table(k = x1, v1 = head(n, h), v2 = seq_along(x1))
    d2 <- data.table(k = x2, w1 = tail(n, length(x2)), w2 = rev(seq_along(x2)))
    d1[d2, c("v1", "v2", "new_col") := .(i.w1, i.w2, i.w2 * 2L), on = "k"]
  }
)
