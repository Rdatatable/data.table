class1_linter = lintr::make_linter_from_xpath(
  "
    //OP-LEFT-BRACKET[
      preceding-sibling::expr/expr/SYMBOL_FUNCTION_CALL[text() = 'class']
      and following-sibling::expr/NUM_CONST[text() = '1' or text() = '1L']
    ]
      /parent::expr
  ",
  "Use class1(x) to get class(x)[1L], or classes1(x) to do so for a full list/data.table"
)
