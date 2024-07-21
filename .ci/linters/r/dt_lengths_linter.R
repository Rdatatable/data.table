# This is lintr::lengths_linter(), but including 'vapply_1i()' which is more common internally in data.table
# alternatively, this could be defined with something like
#   dt_lengths_linter <- lintr::lengths_linter()
#   local({ # to remove 'e'
#     e <- environment(dt_lengths_linter)
#     e$function_names <- c(evalq(function_names, e), "vapply_1i")
#   })
#   but that feels more fragile than this.
dt_lengths_linter <- lintr::make_linter_from_function_xpath(
  function_names = c("sapply", "vapply", "vapply_1i", "map_int", "map_dbl"),
  xpath = "parent::expr/parent::expr[expr/SYMBOL[text() = 'length']]",
  lint_message = "Use lengths() to find the length of each element in a list."
)
