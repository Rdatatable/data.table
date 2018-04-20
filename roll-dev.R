cc(F)
d = as.data.table(list(1:10/2, 1:10/0.64))
replicate(5, cat("====================================================================\n")) -> devnul

ans = rollmean(d, c(3, 4))
as.data.table(ans)

d[, paste0("R",1:4) := rollmean(.SD, c(3, 4)), .SDcols=c("V1","V2")][]
