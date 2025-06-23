# Ensure that data.table options in code match documentation
check_options_documentation = function(rd_file) {
  if (!grepl("\\name{data.table-options}", rd_file, fixed = TRUE)) return(invisible())
  
  # Find options in R code
  walk_for_dt_options = function(expr) {
    result = character()
    if (is.call(expr) && length(expr) >= 2L && identical(expr[[1L]], quote(getOption)) && is.character(e2 <- expr[[2L]]) && startsWith(e2, "datatable.")) {
      result = e2
    } else if (is.recursive(expr)) {
      result = c(result, unlist(lapply(expr, walk_for_dt_options)))
    }
    result
  }
  get_options_from_code = function() {
    list.files("R", pattern = "\\.R$", full.names = TRUE) |>
      lapply(\(f) lapply(parse(f), walk_for_dt_options)) |>
      unlist() |>
      unique() |>
      sort()
  }
  
  # Find options in documentation
  walk_rd = function(rd_element) {
    result = character()
    if (!is.list(rd_element)) return(character())
    if (isTRUE(attr(rd_element, "Rd_tag") == "\\code") && length(rd_element) >= 1L) {
      content = rd_element[[1L]]
      if (is.character(content) && startsWith(content, "datatable.")) {
        result = content
      }
    }
    c(result, unlist(lapply(rd_element, walk_rd)))
  }

  code_opts = get_options_from_code()
  doc_opts = rd_file |>
    tools::parse_Rd() |>
    walk_rd() |>
    unique() |>
    sort()
  code_opts = setdiff(code_opts, "datatable.alloc")

  miss_in_doc = setdiff(code_opts, doc_opts)
  miss_in_code = setdiff(doc_opts, code_opts)

  if (length(miss_in_doc) > 0 || length(miss_in_code) > 0) {
    if (length(miss_in_doc) > 0) {
      cat("Options in code but missing from docs:", paste(miss_in_doc, collapse = ", "), "\n")
    }
    if (length(miss_in_code) > 0) {
      cat("Options in docs but not in code:", paste(miss_in_code, collapse = ", "), "\n")
    }
    stop("Please sync man/data.table-options.Rd with code options")
  }
}
