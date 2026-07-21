eval_parse_linter = make_linter_from_xpath(
  "//SYMBOL_FUNCTION_CALL[text() = 'parse']
     /ancestor::expr
     /preceding-sibling::expr[SYMBOL_FUNCTION_CALL[text() = 'eval']]
     /parent::expr
  ",
  "Avoid eval(parse()); build the language directly, possibly using substitute2()."
)
