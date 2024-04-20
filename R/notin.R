"%notin%" = function(x, table) {
  if (is.character(x) && is.character(table)) {
    .Call(Cnotchin, x, table)
  } else {
    match(x, table, nomatch = 0L) == 0L
  }
}
