dt_lengths_linter <- lintr::make_linter_from_function_xpath(
  function_names = c("sapply", "vapply", "vapply_1i", "map_int", "map_dbl"),
  xpath = "parent::expr/parent::expr[expr/SYMBOL[text() = 'length']]",
  lint_message = "Use lengths() to find the length of each element in a list."
)
