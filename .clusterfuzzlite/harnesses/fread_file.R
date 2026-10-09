# Fuzz harness slots for data.table::fread(file = f, ...) mmap path (src/fread.c).
#
# Input mode: "file" (f is a path to a scratch file containing the raw bytes,
# including any embedded NULs or BOMs).

structure(
  mode = "file",
  list(
    # 1: Default mmap auto-detection
    function(f) fread(file = f),

    # 2: Comma-separated with fill = TRUE
    function(f) fread(file = f, sep = ",", fill = TRUE),

    # 3: Unquoted tab-separated
    function(f) fread(file = f, sep = "\t", quote = ""),

    # 4: Single-column line mode
    function(f) fread(file = f, sep = NULL, header = FALSE),

    # 5: Single-quote delimiter, preserve whitespace
    function(f) fread(file = f, quote = "'", strip.white = FALSE, fill = TRUE),

    # 6: Header-only and skip modes
    function(f) {
      fread(file = f, nrows = 0L)
      fread(file = f, skip = 1L, fill = TRUE)
    },

    # 7: Comment char and blank.lines.skip
    function(f) fread(file = f, comment.char = "#", blank.lines.skip = TRUE),

    # 8: Multi-threaded mmap chunking
    function(f) fread(file = f, nThread = 2L, fill = TRUE)
  )
)
