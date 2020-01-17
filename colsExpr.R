cc(F)

# towards more modular [.data.table #852 and step by step push-down it to C
# also closes #4115 (same #2178)
# #4004
# #2069
# #2059
# remove branch in data.table.R#L909

f = function(x) all(x %between% c(2L,4L))
test(1.11, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(f), "j", environment()), error=".SDcols")
test(1.12, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(function(x) all(x %between% c(2L,4L))), "j", environment()), error=".SDcols")
test(1.13, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(f), ".SDcols", environment()), c(2L,3L,4L)) ## symbol -> eval -> fun
test(1.13, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(function(x) all(x %between% c(2L,4L))), ".SDcols", environment()), c(2L,3L,4L)) ## lang -> eval -> fun

test(1.21, .Call(CexprCols, as.data.table(lapply(1:5, c)), c(4,2), "j", environment()), c(4L,2L))
test(1.22, .Call(CexprCols, as.data.table(lapply(1:5, c)), c(4L,2L), "j", environment()), c(4L,2L))
test(1.23, .Call(CexprCols, as.data.table(lapply(1:5, c)), c("V4","V2"), "j", environment()), c(4L,2L))
test(1.24, .Call(CexprCols, as.data.table(lapply(1:5, c)), c(4,2), ".SDcols", environment()), c(4L,2L))
test(1.25, .Call(CexprCols, as.data.table(lapply(1:5, c)), c(4L,2L), ".SDcols", environment()), c(4L,2L))
test(1.26, .Call(CexprCols, as.data.table(lapply(1:5, c)), c("V4","V2"), ".SDcols", environment()), c(4L,2L))

test(1.31, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c(2,4)), "j", environment()), c(2L,4L))
test(1.32, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c(4,2)), "j", environment()), c(4L,2L))
test(1.33, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c("V4","V2")), "j", environment()), c(4L,2L))
test(1.34, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c(2,4)), ".SDcols", environment()), c(2L,4L))
test(1.35, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c(4,2)), ".SDcols", environment()), c(4L,2L))
test(1.36, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(c("V4","V2")), ".SDcols", environment()), c(4L,2L))

test(1.41, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(V2:V4), "j", environment()), c(2L,3L,4L))
test(1.42, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(V4:V2), "j", environment()), c(4L,3L,2L))
test(1.43, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(V2:V4), ".SDcols", environment()), c(2L,3L,4L))
test(1.44, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(V4:V2), ".SDcols", environment()), c(4L,3L,2L))

test(1.51, .Call(CexprCols, as.data.table(lapply(1:5, c)), (function()c(4L,2L))(), "j", environment()), c(4L,2L)) # note that this is call, not a function
test(1.52, .Call(CexprCols, as.data.table(lapply(1:5, c)), paste0("V",c(4L,2L)), "j", environment()), c(4L,2L))
test(1.53, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote((function()c(4L,2L))()), "j", environment()), c(4L,2L))
test(1.54, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(paste0("V",c(4L,2L))), "j", environment()), c(4L,2L))
test(1.55, .Call(CexprCols, as.data.table(lapply(1:5, c)), (function()c(4L,2L))(), ".SDcols", environment()), c(4L,2L))
test(1.56, .Call(CexprCols, as.data.table(lapply(1:5, c)), paste0("V",c(4L,2L)), ".SDcols", environment()), c(4L,2L))
test(1.57, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote((function()c(4L,2L))()), ".SDcols", environment()), c(4L,2L))
test(1.58, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(paste0("V",c(4L,2L))), ".SDcols", environment()), c(4L,2L))

test(1.61, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((c("V4","V2")))), "j", environment()), c(4L,2L))
test(1.62, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((4:2))), "j", environment()), c(4L,3L,2L))
test(1.63, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((V4:V2))), "j", environment()), c(4L,3L,2L))
test(1.64, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((V4))), "j", environment()), 4L)
test(1.65, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((c("V4","V2")))), ".SDcols", environment()), c(4L,2L))
test(1.66, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((4:2))), ".SDcols", environment()), c(4L,3L,2L))
test(1.67, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((V4:V2))), ".SDcols", environment()), c(4L,3L,2L))
test(1.68, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(((V4))), ".SDcols", environment()), error="'V4' not found")

test(1.71, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(is.numeric), "j", environment()), error="non existing column")
#test(1.72, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!is.numeric), "j", environment()), error="non existing column")
test(1.73, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(is.numeric), ".SDcols", environment()), c(1L,2L,3L,4L,5L))
#test(1.74, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!is.numeric), ".SDcols", environment()), integer())

test(1.81, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(2:4), "j", environment()), c(2L,3L,4L))
test(1.82, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(4:2), "j", environment()), c(4L,3L,2L))
test(1.83, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(2:4), ".SDcols", environment()), c(2L,3L,4L))
test(1.84, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(4:2), ".SDcols", environment()), c(4L,3L,2L))

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4")), "j", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!patterns("V2|V4")), "j", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4", "V2|V5")), "j", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4")), ".SDcols", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!patterns("V2|V4")), ".SDcols", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4", "V2|V5")), ".SDcols", environment())

patterns = c("V1","V2")
test(1.91, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns), "j", environment()), c(1L,2L))
test(1.92, .Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns), ".SDcols", environment()), c(1L,2L))
rm(patterns)

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

.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4")), "j", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!patterns("V2|V4")), "j", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4", "V2|V5")), "j", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4")), ".SDcols", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(!patterns("V2|V4")), ".SDcols", environment())
.Call(CexprCols, as.data.table(lapply(1:5, c)), quote(patterns("V2|V4", "V2|V5")), ".SDcols", environment())

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
