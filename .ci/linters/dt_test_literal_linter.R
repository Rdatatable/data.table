# Avoid test(SYMBOL) and test(SYMBOL+...) except in rare cases.
#   in the latter case, prefer test(NUMBER+expr).
dt_test_literal_linter <- lintr::make_linter_from_xpath(
  "
    //SYMBOL_FUNCTION_CALL[text() = 'test']
      /parent::expr
      /following-sibling::expr[1][SYMBOL or expr[1]/SYMBOL]
  ",
  "Prefer test's 'num' argument to start with a numeric literal except in rare cases, e.g. test(123, ...) or test(123+x/6, ...)"
)
