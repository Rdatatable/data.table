# Ensure that data.table options in code match documentation
check_options_documentation = function(rd_file) {
  if (!grepl("data.table-options", rd_file)) return(invisible())
  
  # Find options in R code
  get_options_from_code = function() {
    found = character(0)
    
    walk_ast = function(expr) {
      result = character(0)
      if (is.call(expr) && length(expr) >= 2 && identical(expr[[1]], quote(getOption)) && is.character(expr[[2]]) && startsWith(expr[[2]], "datatable.")) {
        result = expr[[2]]
      }
      if (is.recursive(expr)) {
        result = c(result, unlist(lapply(expr, walk_ast)))
      }
      result
    }
    r_files = list.files("R", pattern = "\\.R$", full.names = TRUE)
    for (file in r_files) {
      tryCatch({
        found = c(found, unlist(lapply(parse(file = file), walk_ast)))
      }, error = function(e) {})
    }
    sort(unique(found))
  }
  
  # Find options in documentation
  get_options_from_doc = function(rd_file) {
    if (!file.exists(rd_file)) return(character(0))
    
    tryCatch({
      found = character(0)
      walk_rd = function(rd_element) {
        result = character(0)
        if (is.list(rd_element)) {
          if (!is.null(attr(rd_element, "Rd_tag")) && attr(rd_element, "Rd_tag") == "\\code" && length(rd_element) >= 1) {
            content = rd_element[[1]]
            if (is.character(content) && startsWith(content, "datatable.")) {
              result = content
            }
          }
          result = c(result, unlist(lapply(rd_element, walk_rd)))
        }
        result
      }
      found = walk_rd(tools::parse_Rd(rd_file))
      sort(unique(found))
    }, error = function(e) character(0))
  }

  code_opts = get_options_from_code()
  doc_opts = get_options_from_doc(rd_file)
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
