# Intended to be used to create %notin% operator
notin = function(example, elements) {
  return(!match(example, elements, nomatch = 0))
}

"%notin%" = function(example, elements) notin(example, elements)
