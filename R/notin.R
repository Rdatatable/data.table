"%notin%" = function(x, table) {
  if (is.character(x) && is.character((table))) {
    return(.Call(Cchin, x, table, TRUE))
  } else {
    return(!match(x, table, nomatch = 0))
  }
}
