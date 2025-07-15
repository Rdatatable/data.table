# Ensure that data.table condition classes in code match documentation
condition_classes_documentation_linter = function(rd_file) {
  if (!grepl("\\name{data.table-condition-classes}", readChar(rd_file, 100L), fixed = TRUE)) return(invisible())

  # Find condition classes in R code 
  walk_r_ast_for_classes = function(expr) {
    if (is.call(expr) && is.name(e <- expr[[1L]]) && as.character(e) %in% c("stopf", "warningf", "messagef", "packageStartupMessagef") && is.character(class_arg <- expr[["class"]]) && startsWith(class_arg, "dt_")) {
      class_arg
    } else if (is.recursive(expr)) {
      unlist(lapply(expr, walk_r_ast_for_classes))
    }
  }

  # Find condition classes in documentation  
  walk_rd_ast_for_classes = function(rd_element) {
    if (!is.list(rd_element)) return(character())

    result = character()
    if (isTRUE(attr(rd_element, "Rd_tag") == "\\code") && length(rd_element) >= 1L) {
      content = rd_element[[1L]]
      if (is.character(content) && startsWith(content, "dt_")) {
        result = content
      }
    }
    c(result, unlist(lapply(rd_element, walk_rd_ast_for_classes)))
  }

  code_classes = list.files("R", pattern = "\\.R$", full.names = TRUE) |>
    lapply(\(f) lapply(parse(f), walk_r_ast_for_classes)) |>
    unlist() |>
    unique()

  doc_classes = rd_file |>
    tools::parse_Rd() |>
    walk_rd_ast_for_classes() |>
    unique()

  miss_in_doc = setdiff(code_classes, doc_classes)
  miss_in_code = setdiff(doc_classes, code_classes)

  if (length(miss_in_doc) > 0L || length(miss_in_code) > 0L) {
    if (length(miss_in_doc) > 0L) {
      cat(sprintf("Condition classes in code but missing from docs: %s\n", toString(miss_in_doc)))
    }
    if (length(miss_in_code) > 0L) {
      cat(sprintf("Condition classes in docs but not in code: %s\n", toString(miss_in_code)))
    }
    stop("Please sync man/datatable-condition-classes.Rd with code condition classes")
  }
}

