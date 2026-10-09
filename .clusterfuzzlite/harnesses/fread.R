# Fuzz harness slots for data.table::fread(text = x, ...) (src/fread.c, src/freadR.c).
#
# Input mode: "text" (x is a length-1 character vector containing the payload).

structure(
  mode = "text",
  list(
    # 1: Full auto-detection (sep, quote, header, skip, column types)
    function(x) fread(text = x),

    # 2: Explicit comma separator and header
    function(x) fread(text = x, sep = ",", header = TRUE),

    # 3: Tab-separated, unquoted
    function(x) fread(text = x, sep = "\t", quote = ""),

    # 4: Whitespace-separated with strip.white
    function(x) fread(text = x, sep = " ", strip.white = TRUE),

    # 5: Single-column line mode (sep = NULL)
    function(x) fread(text = x, sep = NULL, header = FALSE),

    # 6: Single-quote delimiter, preserve whitespace
    function(x) fread(text = x, quote = "'", strip.white = FALSE),

    # 7: Ragged rows with fill = TRUE and blank.lines.skip = TRUE
    function(x) fread(text = x, fill = TRUE, blank.lines.skip = TRUE),

    # 8: Unbounded ragged column growth (fill = Inf)
    function(x) fread(text = x, fill = Inf, header = FALSE),

    # 9: European decimal comma and semicolon separator
    function(x) fread(text = x, dec = ",", sep = ";"),

    # 10: Multiple NA string tokens
    function(x) fread(text = x, na.strings = c("NA", "", "null", "NULL", "-", "N/A")),

    # 11: Explicit line skip and row cap
    function(x) fread(text = x, skip = 1L, nrows = 5L),

    # 12: Substring search skip
    function(x) fread(text = x, skip = "a"),

    # 13: Header-only sample (nrows = 0L)
    function(x) fread(text = x, nrows = 0L),

    # 14: Logical 0/1, Y/N, and leading-zero preservation
    function(x) fread(text = x, logical01 = TRUE, logicalYN = TRUE, keepLeadingZeros = TRUE),

    # 15: Force all columns to character
    function(x) fread(text = x, colClasses = "character"),

    # 16: Explicit column class mapping + integer64 modes
    function(x) {
      fread(text = x, colClasses = list(integer = 1L), fill = TRUE)
      fread(text = x, integer64 = "character")
      fread(text = x, integer64 = "double")
    },

    # 17: Column selection by position and name
    function(x) {
      fread(text = x, select = c(1L, 2L), fill = TRUE)
      fread(text = x, header = FALSE, select = "V1")
    },

    # 18: Column drop + check.names + key/index
    function(x) fread(text = x, header = FALSE, drop = 2L, check.names = TRUE, key = "V1", index = "V1"),

    # 19: Explicit UTF-8 and Latin-1 markings + comment.char
    function(x) {
      fread(text = x, encoding = "UTF-8", comment.char = "#")
      fread(text = x, encoding = "Latin-1", stringsAsFactors = TRUE)
    },

    # 20: Multi-threaded chunked parsing across jump points
    function(x) fread(text = x, nThread = 2L, fill = TRUE)
  )
)
