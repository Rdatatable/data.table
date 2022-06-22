"%notin%" = function(x, table) {
  if (is.character(x) && is.character(table)) {
    result = .Call(Cnotchin, x, table)
  } else {
    result = match(x, table, nomatch = 0) == 0
  }
  return(result)
  
}
