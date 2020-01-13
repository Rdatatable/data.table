cc(F)

# towards more modular [.data.table #852 and step by step push-down it to C
# also closes #4115 (same #2178)
# #4004
# #2069
# #2059
# remove branch in data.table.R#L909

f = function(x) all(x %between% c(2L,4L))
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(f), environment()) ## symbol -> eval -> fun
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(function(x) all(x %between% c(2L,4L))), environment()) ## lang -> eval -> fun

.Call(CexprCols, as.data.table(lapply(1:5, c)), c(4,2), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), c(4L,2L), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), c("V4","V2"), environment())

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c(2,4)), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c(4,2)), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c("V4","V2")), environment())

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(V2:V4), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(V4:V2), environment())

.Call(CexprCols, as.data.table(lapply(1:5, c)), (function()c(4L,2L))(), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), paste0("V",c(4L,2L)), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote((function()c(4L,2L))()), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(paste0("V",c(4L,2L))), environment())

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((c("V4","V2")))), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((4:2))), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((V4:V2))), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((V4))), environment())

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(is.numeric), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!is.numeric), environment())

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(2:4), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(4:2), environment())
as.data.table(lapply(1:5, c))[, 3:2]
r1 = 3L
r2 = 2L
as.data.table(lapply(1:5, c))[, r1:r2]
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(r1:r2), environment()) ## BC
V1 = 3L
V2 = 2L
as.data.table(lapply(1:5, c))[, V1:V2]
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(V1:V2), environment())

######as.data.table(lapply(1:5, c))[, "V3":"V2"]

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!"V3"), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(-"V3"), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!c("V2","V4")), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(-c("V2","V4")), environment())

as.data.table(lapply(1:5, c))[, ((3:2))]

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4")), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!patterns("V2|V4")), environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4", "V2|V5")), environment())

as.data.table(lapply(1:5, c))[, patterns("V")]

# 4004
iris <- as.data.table(iris)
iris[, c('Species', paste0(c('Sepal.', 'Petal.'), 'Length'))] ## OK
iris[, c('Species', grep('Length', names(iris), value = TRUE))] ## error

# #2059
data.table(a = 1, b = 2, c = 3)
sel_col = c('a', 'b')
dt[, !sel_col]
dt[, !c('a', 'b')]
dt[, !sel_col, with = FALSE]

#DT[, min(var):max(var)] #2069
DT = data.table(
  id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L),
  t = c(4L, 9L, 14L, 1L, 4L, 9L, 5L, 14L, 18L, 1L, 3L, 12L, 3L, 5L, 7L, 4L, 10L, 13L)
)
DT[, min(t):max(t)]
with(DT, min(t):max(t))
DT[, seq(min(id), max(id))]
DT[, min(t):max(t)]

#2178
DF = data.frame(a = 1, b = 2, c = 3, d = 4, e = 5)
DF[ , c(TRUE, FALSE)]
#   a c e
# 1 1 3 5
setDT(DF)
DF[ , c(TRUE, FALSE)]
#    a
# 1: 1

#4115
