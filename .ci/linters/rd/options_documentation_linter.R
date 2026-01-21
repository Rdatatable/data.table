# Ensure that data.table options in code match documentation
options_documentation_linter = function(rd_file) {
  if (!grepl("\\name{data.table-options}", readChar(rd_file, 100L), fixed = TRUE)) return(invisible())

  # Find options in R code
  walk_r_ast_for_options = function(expr) {
    if (is.call(expr) && length(expr) >= 2L && identical(expr[[1L]], quote(getOption)) && is.character(e2 <- expr[[2L]]) && startsWith(e2, "datatable.")) {
      e2
    } else if (is.recursive(expr)) {
      unlist(lapply(expr, walk_r_ast_for_options))
    }
  }

  # Find options in documentation
  walk_rd_ast_for_options = function(rd_element) {
    if (!is.list(rd_element)) return(character())

    result = character()
    if (isTRUE(attr(rd_element, "Rd_tag") == "\\code") && length(rd_element) >= 1L) {
      content = rd_element[[1L]]
      if (is.character(content) && startsWith(content, "datatable.")) {
        result = content
      }
    }
    c(result, unlist(lapply(rd_element, walk_rd_ast_for_options)))
  }

  code_opts = list.files("R", pattern = "\\.R$", full.names = TRUE) |>
    lapply(\(f) lapply(parse(f), walk_r_ast_for_options)) |>
    unlist() |>
    unique() |>
    setdiff("datatable.nomatch") # ignore deprecated option(s)

  doc_opts = rd_file |>
    tools::parse_Rd() |>
    walk_rd_ast_for_options() |>
    unique()

  miss_in_doc = setdiff(code_opts, doc_opts)
  miss_in_code = setdiff(doc_opts, code_opts)

  if (length(miss_in_doc) > 0L || length(miss_in_code) > 0L) {
    if (length(miss_in_doc) > 0L) {
      cat(sprintf("Options in code but missing from docs: %s\n", toString(miss_in_doc)))
    }
    if (length(miss_in_code) > 0L) {
      cat(sprintf("Options in docs but not in code: %s\n", toString(miss_in_code)))
    }
    stop("Please sync man/data.table-options.Rd with code options")
  }
}
