# Intended to be used to create %notin% operator
notin = function(example, elements) {
  if (is.character(example)) {
    return(!chmatch(example, elements, nomatch = 0))
  } else {
    return(!match(example, elements, nomatch = 0))
  }
}

"%notin%" = function(example, elements) notin(example, elements)
