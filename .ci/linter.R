library(data.table)
library(xml2)
library(xmlparsedata)

writeLines("
if (T) 1+1 else 2+2
", tmp <- tempfile())
lines = readLines(tmp)
xml = read_xml(xml_parse_data(parse(tmp)))

bad_lines_from_xpath = function(xml, xpath) {
  bad_expr = xml_find_all(xml, xpath)
  if (!length(bad_expr)) return(NULL)
  return(list(
    line1 = as.integer(xml_attr(bad_expr, 'line1')),
    line2 = as.integer(xml_attr(bad_expr, 'line2'))
  ))
}

# works to detect these numerics
has_int_as_numeric = function(exprlist) {
  # an exception for 1:3 etc, R returns integer regardless (not at parser level)
  seq_cond = "not(preceding-sibling::OP-COLON or following-sibling::OP-COLON)"
  # number() casts to numeric, which fails for TRUE/FALSE/L-suffixed integers.
  #   also allow explicit/intentional doubles to be written with a decimal
  number_cond = "number(text()) = text() and not(contains(text(), '.') or contains(text(), 'e'))"
  xpath = sprintf("//expr[%s and NUM_CONST[%s]]", seq_cond, number_cond)
  
  bad_lines_from_xpath(exprlist, xpath)
}

has_quoted_Call = function(exprlist) {
  xpath = "//expr[expr[SYMBOL_FUNCTION_CALL[text() = '.Call']] and expr[2][STR_CONST]]"
  
  bad_lines_from_xpath(exprlist, xpath)
}

has_plain_T_F = function(exprlist) {
  xpath = "//expr[SYMBOL[text() = 'T' or text() = 'F']]"
  
  bad_lines_from_xpath(exprlist, xpath)
}

has_ifelse = function(exprlist) {
  xpath = "//expr[SYMBOL_FUNCTION_CALL[text() = 'ifelse']]"
  
  bad_lines_from_xpath(exprlist, xpath)
}

has_system.time = function(exprlist) {
  xpath = "//expr[SYMBOL_FUNCTION_CALL[text() = 'system.time']]"
  
  bad_lines_from_xpath(exprlist, xpath)
}

get_src_tag_fmt = function(lines) {
  sprintf("%%s:%%%dd:%%s", ceiling(log10(length(lines))))
}

linters = list(
  has_int_as_numeric = has_int_as_numeric,
  has_quoted_Call = has_quoted_Call,
  has_plain_T_F = has_plain_T_F,
  has_ifelse = has_ifelse
)

for (f in list.files('R', full.names = TRUE)) {
  lines = readLines(f)
  xml = read_xml(xml_parse_data(parse(f, keep.source = TRUE)))
  
  for (ii in seq_along(linters)) {
    bad_lines = linters[[ii]](xml)
    if (length(bad_lines)) {
      cat("\n-----------------\n", names(linters)[ii], " found some issues\n", sep = "")
      for (linei in seq_len(length(bad_lines$line1))) {
        idx = seq(bad_lines$line1[linei], bad_lines$line2[linei])
        writeLines(sprintf(get_src_tag_fmt(lines), f, idx, lines[idx]))
      }
    }
  }
}

linters = c(
  linters[setdiff(names(linters), 'has_int_as_numeric')],
  list(has_system.time = has_system.time)
)

for (f in list.files('inst/tests', full.names = TRUE, pattern = 'Rraw')) {
  lines = readLines(f)
  xml = read_xml(xml_parse_data(parse(f, keep.source = TRUE)))
  
  for (ii in seq_along(linters)) {
    if (f == 'inst/tests/benchmark.Rraw' && names(linters)[ii] == 'has_system.time') next
    bad_lines = linters[[ii]](xml)
    if (length(bad_lines)) {
      cat("\n-----------------\n", names(linters)[ii], " found some issues\n", sep = "")
      for (linei in seq_len(length(bad_lines$line1))) {
        idx = seq(bad_lines$line1[linei], bad_lines$line2[linei])
        writeLines(sprintf(get_src_tag_fmt(lines), f, idx, lines[idx]))
      }
    }
  }
}
