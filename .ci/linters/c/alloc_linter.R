# Ensure that we check the result of malloc()/calloc() for success
# More specifically, given that this is an AST-ignorant checker,
#   1. Find groups of malloc()/calloc() calls
#   2. Check the next line for a check like 'if (!x || !y)'
alloc_linter = function(c_obj) {
  lines = c_obj$lines
  # Be a bit more precise to avoid mentions in comments
  alloc_lines = grep(R"{=\s*([(]\w+\s*[*][)])?[mc]alloc[(]}", lines)
  if (!length(alloc_lines)) return()
  # int *tmp=(int*)malloc(...); or just int tmp=malloc(...);
  alloc_keys = lines[alloc_lines] |>
    strsplit(R"(\s*=\s*)") |>
    vapply(head, 1L, FUN.VALUE="") |>
    trimws() |>
    # just normalize the more exotic assignments, namely 'type *restrict key = ...'
    gsub(pattern = "[*]\\s*(restrict\\s*)?", replacement = "*") |>
    strsplit("*", fixed=TRUE) |>
    vapply(tail, 1L, FUN.VALUE="")
  alloc_grp_id = cumsum(c(TRUE, diff(alloc_lines) != 1L))

  # execute by group
  tapply(seq_along(alloc_lines), alloc_grp_id, function(grp_idx) {
    keys_regex = paste0("\\s*!\\s*", alloc_keys[grp_idx], "\\s*", collapse = "[|][|]")
    check_regex = paste0("if\\s*\\(", keys_regex)
    check_line = lines[alloc_lines[tail(grp_idx, 1L)] + 1L]
    # Rarely (once in fread.c as of initialization), error checking is handled
    #   but not immediately, so use 'NOCHECK' to escape.
    if (!grepl(check_regex, check_line) && !grepl("NOCHECK", check_line, fixed=TRUE)) {
      bad_lines_idx = seq(alloc_lines[grp_idx[1L]], length.out=length(grp_idx)+1L)
      cat("FILE: ", c_obj$path, "; LINES: ", head(bad_lines_idx, 1L), "-", tail(bad_lines_idx, 1L), "\n", sep="")
      writeLines(lines[bad_lines_idx])
      cat(strrep("-", max(nchar(lines[bad_lines_idx]))), "\n", sep="")
      stop("Expected the malloc()/calloc() usage above to be followed immediately by error checking.", call.=FALSE)
    }
  })
}
