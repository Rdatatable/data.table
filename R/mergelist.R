cbindlist_impl_ = function(l, copy) {
  ans = .Call(Ccbindlist, l, copy)
  if (anyDuplicated(names(ans))) { ## invalidate key and index
    setattr(ans, "sorted", NULL)
    setattr(ans, "index", NULL)
  }
  setDT(ans)
  ans
}

cbindlist = function(l) cbindlist_impl_(l, copy=TRUE)
setcbindlist = function(l) cbindlist_impl_(l, copy=FALSE)
