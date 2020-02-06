cc(F)

# towards more modular [.data.table #852 and step by step push-down it to C
# also closes #4115 (same #2178)
# #4004
# #2069
# #2059
# remove branch in data.table.R#L909

x = as.data.table(lapply(1:5, c))
with = copy(NA) # copy for dev till we not duplicate yet, to ensure not to override NA value

#z1 = "V1"
#z4 = "V4"
#x[, c(..z1,..z4)]
#x[, c(..z1,"V4")]

# incorrect length logical #4115
# incorrect length logical as variable in parent scope

# test for !!V3 --V3
# test after inverse peeling for -("V3")

# subset DT[, c("x","x")] on DT having single x column and having duplicated x column name

# function
## !function

# patterns
## !patterns

## old tests

f = function(x) all(x %between% c(2L,4L))
test(1.101, exprCols(x, f, "j", with, environment()), error="non existing column.*f")
test(1.102, exprCols(x, function(x) all(x %between% c(2L,4L)), "j", with, environment()), error="is not supported, use '.SDcols' instead")
test(1.103, exprCols(x, f, ".SDcols", with, environment()), c(2L,3L,4L))
test(1.103, exprCols(x, function(x) all(x %between% c(2L,4L)), ".SDcols", with, environment()), c(2L,3L,4L))
rm(f)

test(1.111, exprCols(x, c(4,2), "j", with, environment()), c(4L,2L))
test(1.112, exprCols(x, c(4L,2L), "j", with, environment()), c(4L,2L))
test(1.113, exprCols(x, c("V4","V2"), "j", with, environment()), c(4L,2L))
test(1.114, exprCols(x, c(4,2), ".SDcols", with, environment()), c(4L,2L))
test(1.115, exprCols(x, c(4L,2L), ".SDcols", with, environment()), c(4L,2L))
test(1.116, exprCols(x, c("V4","V2"), ".SDcols", with, environment()), c(4L,2L))

test(1.121, exprCols(x, V2:V4, "j", with, environment()), c(2L,3L,4L))
test(1.122, exprCols(x, V4:V2, "j", with, environment()), c(4L,3L,2L))
test(1.123, exprCols(x, V2:V4, ".SDcols", with, environment()), c(2L,3L,4L))
test(1.124, exprCols(x, V4:V2, ".SDcols", with, environment()), c(4L,3L,2L))

test(1.131, exprCols(x, (function()c(4L,2L))(), "j", with, environment()), c(4L,2L)) # note that this is call, not a function
test(1.132, exprCols(x, paste0("V",c(4L,2L)), "j", with, environment()), c(4L,2L))
test(1.133, exprCols(x, (function()c(4L,2L))(), ".SDcols", with, environment()), c(4L,2L))
test(1.134, exprCols(x, paste0("V",c(4L,2L)), ".SDcols", with, environment()), c(4L,2L))

test(1.141, exprCols(x, ((c("V4","V2"))), "j", with, environment()), c(4L,2L))
test(1.142, exprCols(x, ((4:2)), "j", with, environment()), c(4L,3L,2L))
test(1.143, exprCols(x, ((V4:V2)), "j", with, environment()), c(4L,3L,2L))
test(1.144, exprCols(x, ((V4)), "j", with, environment()), 4L)
test(1.145, exprCols(x, (("V4":"V2")), "j", with, environment()), error="NA/NaN argument", warning=c("NAs introduced by coercion","NAs introduced by coercion"))
test(1.146, exprCols(x, ((c("V4","V2"))), ".SDcols", with, environment()), c(4L,2L))
test(1.147, exprCols(x, ((4:2)), ".SDcols", with, environment()), c(4L,3L,2L))
test(1.148, exprCols(x, ((V4:V2)), ".SDcols", with, environment()), c(4L,3L,2L))
test(1.149, exprCols(x, ((V4)), ".SDcols", with, environment()), error="'V4' not found")
test(1.150, exprCols(x, (("V4":"V2")), ".SDcols", with, environment()), error="NA/NaN argument", warning=c("NAs introduced by coercion","NAs introduced by coercion"))
test(1.161, exprCols(x, 2:4, "j", with, environment()), c(2L,3L,4L))
test(1.162, exprCols(x, 4:2, "j", with, environment()), c(4L,3L,2L))
test(1.163, exprCols(x, 2:4, ".SDcols", with, environment()), c(2L,3L,4L))
test(1.164, exprCols(x, 4:2, ".SDcols", with, environment()), c(4L,3L,2L))

test(1.171, exprCols(x, is.numeric, "j", with, environment()), error="non existing column.*is.numeric")
#test(1.172, exprCols(x, !is.numeric, "j", with, environment()), error="non existing column.*is.numeric")
test(1.173, exprCols(x, is.numeric, ".SDcols", with, environment()), c(1L,2L,3L,4L,5L))
#test(1.174, exprCols(x, !is.numeric, ".SDcols", with, environment()), integer())

#test(1.181, exprCols(x, patterns("V2|V4"), "j", with, environment()), error="") # TODO
#test(1.182, exprCols(x, !patterns("V2|V4"), "j", with, environment()), error="")
#test(1.183, exprCols(x, patterns("V2|V4", "V2|V5"), "j", with, environment()), error="")
#test(1.184, exprCols(x, !patterns("V2|V4", "V2|V5"), "j", with, environment()), error="")
test(1.185, exprCols(x, patterns("V2|V4"), ".SDcols", with, environment()), c(2L,4L))
#test(1.186, exprCols(x, !patterns("V2|V4"), ".SDcols", with, environment()), c(1L,3L,5L)) # TODO
test(1.187, exprCols(x, patterns("V2|V4", "V2|V5"), ".SDcols", with, environment()), 2L)
#test(1.188, exprCols(x, !patterns("V2|V4", "V2|V5"), ".SDcols", with, environment()), 2L)

cols = c("V1","V2")
test(1.191, exprCols(x, cols, "j", with, environment()), c(1L,2L))
test(1.192, exprCols(x, cols, ".SDcols", with, environment()), c(1L,2L))
rm(cols)
patterns = c("V1","V2")
test(1.193, exprCols(x, patterns, "j", with, environment()), c(1L,2L))
test(1.194, exprCols(x, patterns, ".SDcols", with, environment()), c(1L,2L))
rm(patterns)

as.data.table(lapply(1:5, c))[, 3:2]
r1 = 3L
r2 = 2L
as.data.table(lapply(1:5, c))[, r1:r2]
exprCols(x, r1:r2, with, environment()) ## BC
V1 = 3L
V2 = 2L
as.data.table(lapply(1:5, c))[, V1:V2]
exprCols(x, V1:V2, with, environment())

as.data.table(lapply(1:5, c))[, "V3":"V2"]

exprCols(x, !"V3", with, environment())
exprCols(x, -"V3", with, environment())
exprCols(x, V2-V3, with, environment()) # length 3 call, non-unary minus
exprCols(x, !c("V2","V4"), with, environment())
exprCols(x, -c("V2","V4"), with, environment())

as.data.table(lapply(1:5, c))[, ((3:2))]

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
