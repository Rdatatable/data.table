cc(F)
d = as.data.table(list(as.double(1:10), as.double(2:11)))
d
ans = rollmean(d, 4)
dd = as.data.table(ans)
dd
str(dd)
