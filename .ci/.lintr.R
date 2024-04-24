for (f in list.files('ci/linters', full.names=TRUE)) source(f)
rm(f)

linters = all_linters(
  packages = "lintr", # TODO(lintr->3.2.0): Remove this.
  # eq_assignment_linter(),
  brace_linter(allow_single_line = TRUE),
  # TODO(michaelchirico): Activate these incrementally. These are the
  #   parameterizations that match our style guide.
  # implicit_assignment_linter(allow_lazy = TRUE, allow_scoped = TRUE),
  # implicit_integer_linter(allow_colon = TRUE),
  # system_time_linter = undesirable_function_linter(c(
  #   system.time = "Only run timings in benchmark.Rraw"
  # )),
  # undesirable_function_linter(modify_defaults(
  #   default_undesirable_functions,
  #   ifelse = "Use fifelse instead.",
  #   Sys.setenv = NULL,
  #   library = NULL,
  #   options = NULL,
  #   par = NULL,
  #   setwd = NULL
  # )),
  undesirable_operator_linter(modify_defaults(
    default_undesirable_operators,
    `<<-` = NULL
  )),
  # TODO(lintr#2441): Use upstream implementation.
  assignment_linter = NULL,
  # TODO(lintr#2442): Use this once x[ , j, by] is supported.
  commas_linter = NULL,
  commented_code_linter = NULL,
  # TODO(linter->3.2.0): Activate this.
  consecutive_assertion_linter = NULL,
  cyclocomp_linter = NULL,
  function_argument_linter = NULL,
  indentation_linter = NULL,
  infix_spaces_linter = NULL,
  # TODO(R>3.2.0): Activate this, extending to recognize vapply_1i(x, length).
  lengths_linter = NULL,
  line_length_linter = NULL,
  missing_package_linter = NULL,
  namespace_linter = NULL,
  nonportable_path_linter = NULL,
  object_name_linter = NULL,
  object_usage_linter = NULL,
  quotes_linter = NULL,
  semicolon_linter = NULL,
  spaces_inside_linter = NULL,
  spaces_left_parentheses_linter = NULL,
  # TODO(michaelchirico): Only exclude from vignettes, not sure what's wrong.
  strings_as_factors_linter = NULL,
  # TODO(lintr->3.2.0): Fix on a valid TODO style, enforce it, and re-activate.
  todo_comment_linter = NULL,
  # TODO(michaelchirico): Enforce these and re-activate them one-by-one. Also stop using '<<-'.
  brace_linter = NULL,
  condition_call_linter = NULL,
  conjunct_test_linter = NULL,
  fixed_regex_linter = NULL,
  function_left_parentheses_linter = NULL,
  if_not_else_linter = NULL,
  implicit_assignment_linter = NULL,
  implicit_integer_linter = NULL,
  keyword_quote_linter = NULL,
  length_levels_linter = NULL,
  matrix_apply_linter = NULL,
  missing_argument_linter = NULL,
  nzchar_linter = NULL,
  object_overwrite_linter = NULL,
  paren_body_linter = NULL,
  redundant_equals_linter = NULL,
  rep_len_linter = NULL,
  repeat_linter = NULL,
  return_linter = NULL,
  sample_int_linter = NULL,
  scalar_in_linter = NULL,
  seq_linter = NULL,
  undesirable_function_linter = NULL,
  unnecessary_concatenation_linter = NULL,
  unnecessary_lambda_linter = NULL,
  unnecessary_nesting_linter = NULL,
  unreachable_code_linter = NULL,
  unused_import_linter = NULL
)
# TODO(lintr#2172): Glob with lintr itself.
exclusions = local({
  exclusion_for_dir <- function(dir, exclusions) {
    files = list.files(dir, pattern = "\\.(R|Rmd)$")
    stats::setNames(rep(list(exclusions), length(files)), files)
  }
  c(
    exclusion_for_dir("tests", list(
      quotes_linter = Inf,
      # TODO(michaelchirico): Enforce these and re-activate them one-by-one.
      implicit_integer_linter = Inf,
      infix_spaces_linter = Inf,
      undesirable_function_linter = Inf
    )),
    exclusion_for_dir("vignettes", list(
      quotes_linter = Inf
      # strings_as_factors_linter = Inf
      # system_time_linter = Inf
    ))
  )
})
