"%notin%" = function(x, table) {
  if (is.character(example)) {
    return(.Call(Cchin, x, table, TRUE))
  } else {
    return(!match(x, table, nomatch = 0))
  }
}
