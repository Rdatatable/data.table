cbindlist = function(l, copy=TRUE) {
  ans = .Call(Ccbindlist, l, copy)
  if (anyDuplicated(names(ans))) { ## invalidate key and index
    setattr(ans, "sorted", NULL)
    setattr(ans, "index", integer())
  }
  setDT(ans)
  ans
}
