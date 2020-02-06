cc(F)

# towards more modular [.data.table #852 and step by step push-down it to C
# also closes #4115 (same #2178)
# #4004
# #2069
# #2059
# remove branch in data.table.R#L909

x = as.data.table(lapply(1:5, c))
with = copy(NA) # copy for dev till we not duplicate yet, to ensure not to override NA value

#DEV
# test after inverse peeling for !("V5")
test(9999.9921, exprCols(x, !("V5"), "j", with, environment()), 2:4)
test(9999.9922, exprCols(x, !("V5"), ".SDcols", with, environment()), 2:4)
#test(9999.9923, exprCols(x, !("V5"), "by", with, environment()), 2:4)
cols = c("V1","V5")
test(9999.9924, exprCols(x, !(..cols), "j", with, environment()), 2:4)
test(9999.9925, exprCols(x, !(..cols), ".SDcols", with, environment()), error="object '..cols' not found")
#test(9999.9926, exprCols(x, !(..cols), "by", with, environment()), 2:4)
rm(cols)
# function
f = function(x) all(x %between% c(2L,4L))
test(9999.9931, exprCols(x, f, "j", with, environment()), error="non existing column")
test(9999.9932, exprCols(x, f, ".SDcols", with, environment()), 2:4)
test(9999.9933, exprCols(x, !f, "j", with, environment()), error="invalid argument type")
test(9999.9934, exprCols(x, !f, ".SDcols", with, environment()), c(1L,5L))
rm(f)
test(9999.9935, exprCols(x, function(x) all(x %between% c(2L,4L)), "j", with, environment()), NULL)
test(9999.9936, exprCols(x, function(x) all(x %between% c(2L,4L)), ".SDcols", with, environment()), 2:4)
test(9999.9937, exprCols(x, !function(x) all(x %between% c(2L,4L)), "j", with, environment()), error="invalid argument type")
test(9999.9938, exprCols(x, !function(x) all(x %between% c(2L,4L)), ".SDcols", with, environment()), c(1L,5L))
# patterns
test(1.181, exprCols(x, patterns("V2|V4"), "j", with, environment()), error="")
test(1.182, exprCols(x, !patterns("V2|V4"), "j", with, environment()), error="")
test(1.183, exprCols(x, patterns("V2|V4", "V2|V5"), "j", with, environment()), error="")
test(1.184, exprCols(x, !patterns("V2|V4", "V2|V5"), "j", with, environment()), error="")
test(1.185, exprCols(x, patterns("V2|V4"), ".SDcols", with, environment()), c(2L,4L))
test(1.186, exprCols(x, !patterns("V2|V4"), ".SDcols", with, environment()), c(1L,3L,5L))
test(1.187, exprCols(x, patterns("V2|V4", "V2|V5"), ".SDcols", with, environment()), 2L)
test(1.188, exprCols(x, !patterns("V2|V4", "V2|V5"), ".SDcols", with, environment()), 2L)
patterns = c("V1","V2")
test(1.193, exprCols(x, patterns, "j", with, environment()), c(1L,2L))
test(1.194, exprCols(x, patterns, ".SDcols", with, environment()), c(1L,2L))
rm(patterns)

# calls
test(1.131, exprCols(x, (function()c(4L,2L))(), "j", with, environment()), c(4L,2L)) # note that this is call, not a function
test(1.132, exprCols(x, paste0("V",c(4L,2L)), "j", with, environment()), c(4L,2L))
test(1.133, exprCols(x, (function()c(4L,2L))(), ".SDcols", with, environment()), c(4L,2L))
test(1.134, exprCols(x, paste0("V",c(4L,2L)), ".SDcols", with, environment()), c(4L,2L))

# colon sequence from vars
as.data.table(lapply(1:5, c))[, 3:2]
r1 = 3L
r2 = 2L
as.data.table(lapply(1:5, c))[, r1:r2]
exprCols(x, r1:r2, "j", with, environment())
V1 = 3L
V2 = 2L
as.data.table(lapply(1:5, c))[, V1:V2]
exprCols(x, V1:V2, "j", with, environment())
# incorrect usage of col1:col2 selection #4235, warnings will be still there till we disabled old interface and switch fully to colselect
test(1.111, exprCols(x, "V3":"V2", "j", with, environment()), NULL)
test(1.111, exprCols(x, "V3":"V2", ".SDcols", with, environment()), NULL) # should raise own error!

# seqence from columns #2069
DT[, min(var):max(var)]

# non-unary minus, length 3 call
exprCols(x, V2-V3, "j", with, environment())

# subset DT[, c("x","x")] on DT having single x column and having duplicated x column name
x = data.table(V1=1L, V2=2L)
exprCols(x, c("V1","V1"), "j", with, environment())
x = data.table(V1=1L, V2=2L, V1=3L)
exprCols(x, c("V1","V1"), "j", with, environment())

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
