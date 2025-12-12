# ensure no markdown-style backticks wind up in Rd where \code is intended
options_documentation_linter = function(rd_file) {
  rd = tools::parse_Rd(rd_file)

  error_if_backtick = function(rd_obj) {
    if (!is.recursive(rd_obj)) {
      if (any(grepl("`", rd_obj, fixed=TRUE))) {
        stop(sprintf(
          "Rd is not markdown -- backticks (`) don't render as code! Use \\code{...}.\nObserved in string '%s' in file %s",
          trimws(rd_obj), rd_file
        ))
      }
      return(invisible())
    }
    tags = vapply(rd_obj, \(x) attr(x, "Rd_tag") %||% "", FUN.VALUE="")
     # backtick is valid inside R code (e.g. \examples, \code, \preformatted)
    rd_obj = rd_obj[!tags %in% c("RCODE", "VERB")]
    lapply(rd_obj, error_if_backtick)
  }

  invisible(error_if_backtick(rd))
}
