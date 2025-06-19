# Run with: Rscript .ci/check-options.R
cat(">> Checking data.table options documentation consistency\n")

# Scan R source code for data.table options 
code_opts <- sort(unique(gsub('["\']', '',
  unlist(lapply( list.files("R", pattern = "\\.R$", full.names = TRUE),
    function(f) {
      lines <- suppressWarnings(readLines(f, warn = FALSE))
      regmatches(lines, gregexpr('["\'](datatable\\.[.A-Za-z0-9]+)["\']', lines))
    }
  ))
)))

# Scan the documentation file for data.table options
doc_file <- "man/data.table-options.Rd"
if (!file.exists(doc_file)) stop("CRITICAL: '", doc_file, "' not found.")
doc_opts <- sort(unique(unlist(
  regmatches(readLines(doc_file, warn = FALSE), gregexpr("(?<=\\\\code\\{)datatable\\.[^}]+", readLines(doc_file, warn = FALSE), perl = TRUE))
)))

# Compare the final lists and report status 
cat(sprintf("   Found %d options in code, %d in documentation.\n", length(code_opts), length(doc_opts)))

miss_in_doc  <- setdiff(code_opts, doc_opts)
miss_in_code <- setdiff(doc_opts, doc_opts)

if (length(miss_in_doc) || length(miss_in_code)) {
  message("  Mismatch in data.table options documentation:")
  if (length(miss_in_doc)) {
    message("    In code but MISSING from docs:\n    - ", paste(miss_in_doc, collapse="\n    - "))
  }
  if (length(miss_in_code)) {
    message("\n    In docs but NOT in code (check for typos/deprecation):\n    - ", paste(miss_in_code, collapse="\n    - "))
  }
  quit(status = 1)
}

message("  Options documentation is perfectly in sync.")
