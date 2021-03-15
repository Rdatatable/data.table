"%notin%" = function(example, elements) {
  if (is.character(example)) {
    return(.Call(Cchin, example, elements, TRUE))
  } else {
    return(!match(example, elements, nomatch = 0))
  }
}
