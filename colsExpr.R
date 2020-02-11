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
