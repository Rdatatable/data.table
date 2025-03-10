dt_linters = new.env()
for (f in list.files('.ci/linters/r', full.names=TRUE)) sys.source(f, dt_linters)
rm(f)

# NB: Could do this inside the linter definition, this separation makes those files more standardized
dt_linters <- eapply(dt_linters, function(linter_factory) linter_factory())

linters = c(dt_linters, all_linters(
  packages = "lintr", # TODO(lintr->3.2.0): Remove this.
  # eq_assignment_linter(),
  brace_linter(allow_single_line = TRUE),
  implicit_integer_linter(allow_colon = TRUE),
  # TODO(michaelchirico): Activate these incrementally. These are the
  #   parameterizations that match our style guide.
  # implicit_assignment_linter(allow_lazy = TRUE, allow_scoped = TRUE),
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
  undesirable_operator_linter(),
  # TODO(lintr#2765): Use upstream implementation.
  # assignment_linter(operator = "="),
  assignment_linter = NULL,
  absolute_path_linter = NULL, # too many false positives
  # TODO(lintr#2442): Use this once x[ , j, by] is supported.
  commas_linter = NULL,
  commented_code_linter = NULL,
  # TODO(linter->3.2.0): Activate this.
  consecutive_assertion_linter = NULL,
  cyclocomp_linter = NULL,
  function_argument_linter = NULL,
  indentation_linter = NULL,
  infix_spaces_linter = NULL,
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
  # TODO(michaelchirico): Enforce these and re-activate them one-by-one.
  brace_linter = NULL,
  fixed_regex_linter = NULL,
  if_not_else_linter = NULL,
  implicit_assignment_linter = NULL,
  implicit_integer_linter = NULL,
  keyword_quote_linter = NULL,
  object_overwrite_linter = NULL,
  paren_body_linter = NULL,
  redundant_equals_linter = NULL,
  undesirable_function_linter = NULL,
  unnecessary_concatenation_linter = NULL,
  unnecessary_nesting_linter = NULL,
  unreachable_code_linter = NULL,
  unused_import_linter = NULL
))
rm(dt_linters)

exclusions = list(
  `../tests` = list(
    quotes_linter = Inf,
    # TODO(michaelchirico): Enforce these and re-activate them one-by-one.
    implicit_integer_linter = Inf,
    infix_spaces_linter = Inf,
    undesirable_function_linter = Inf
  ),
  `../vignettes*` = list(
    # assignment_linter = Inf,
    implicit_integer_linter = Inf,
    quotes_linter = Inf,
    sample_int_linter = Inf
    # strings_as_factors_linter = Inf
    # system_time_linter = Inf
  ),
  `../inst/tests` = list(
    library_call_linter = Inf,
    numeric_leading_zero_linter = Inf,
    undesirable_operator_linter = Inf, # For ':::', possibly we could be more careful to only exclude ':::'.
    # TODO(michaelchirico): Enforce these and re-activate them one-by-one.
    comparison_negation_linter = Inf,
    condition_call_linter = Inf,
    duplicate_argument_linter = Inf,
    equals_na_linter = Inf,
    missing_argument_linter = Inf,
    paste_linter = Inf,
    rep_len_linter = Inf,
    sample_int_linter = Inf,
    seq_linter = Inf,
    unnecessary_lambda_linter = Inf
  ),
  `../inst/tests/froll.Rraw` = list(
    dt_test_literal_linter = Inf # TODO(michaelchirico): Fix these once #5898, #5692, #5682, #5576, #5575, #5441 are merged.
  )
)
