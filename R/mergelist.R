cbindlist = function(x, copy=TRUE) {
  ans = .Call(Ccbindlist, x, copy)
  setDT(ans)
  ans
}
