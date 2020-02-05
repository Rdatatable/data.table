cc(F)

# towards more modular [.data.table #852 and step by step push-down it to C
# also closes #4115 (same #2178)
# #4004
# #2069
# #2059
# remove branch in data.table.R#L909

x = as.data.table(lapply(1:5, c))
with = copy(NA) # copy for dev till we not duplicate yet, to ensure not to override NA value

# character as variable in parent scope
cols = c("V1","V3")
test(9999.601, exprCols(x, cols, "j", with, environment()), 1L)
test(9999.602, exprCols(x, cols, ".SDcols", with, environment()), 1L)
#test(9999.603, exprCols(x, cols, "by", with, environment()), 1L)
test(9999.604, exprCols(x, ((cols)), "j", with, environment()), 1L)
test(9999.605, exprCols(x, ((cols)), ".SDcols", with, environment()), 1L)
#test(9999.606, exprCols(x, ((cols)), "by", with, environment()), 1L)
test(9999.607, exprCols(x, {{cols}}, "j", with, environment()), 1L)
test(9999.608, exprCols(x, {{cols}}, ".SDcols", with, environment()), 1L)
#test(9999.609, exprCols(x, {{cols}}, "by", with, environment()), 1L)
test(9999.611, exprCols(x, -cols, "j", with, environment()), 1L)
test(9999.612, exprCols(x, -cols, ".SDcols", with, environment()), 1L)
#test(9999.613, exprCols(x, -cols, "by", with, environment()), 1L)
test(9999.614, exprCols(x, ((-cols)), "j", with, environment()), 1L)
test(9999.615, exprCols(x, ((-cols)), ".SDcols", with, environment()), 1L)
#test(9999.616, exprCols(x, ((-cols)), "by", with, environment()), 1L)
test(9999.617, exprCols(x, {{-cols}}, "j", with, environment()), 1L)
test(9999.618, exprCols(x, {{-cols}}, ".SDcols", with, environment()), 1L)
#test(9999.619, exprCols(x, {{-cols}}, "by", with, environment()), 1L)
test(9999.621, exprCols(x, !cols, "j", with, environment()), 1L)
test(9999.622, exprCols(x, !cols, ".SDcols", with, environment()), 1L)
#test(9999.623, exprCols(x, !cols, "by", with, environment()), 1L)
test(9999.624, exprCols(x, ((!cols)), "j", with, environment()), 1L)
test(9999.625, exprCols(x, ((!cols)), ".SDcols", with, environment()), 1L)
#test(9999.626, exprCols(x, ((!cols)), "by", with, environment()), 1L)
test(9999.627, exprCols(x, {{!cols}}, "j", with, environment()), 1L)
test(9999.628, exprCols(x, {{!cols}}, ".SDcols", with, environment()), 1L)
#test(9999.629, exprCols(x, {{!cols}}, "by", with, environment()), 1L)
test(9999.631, exprCols(x, ..cols, "j", with, environment()), 1L)
test(9999.632, exprCols(x, ..cols, ".SDcols", with, environment()), 1L)
#test(9999.633, exprCols(x, ..cols, "by", with, environment()), 1L)
test(9999.634, exprCols(x, ((..cols)), "j", with, environment()), 1L)
test(9999.635, exprCols(x, ((..cols)), ".SDcols", with, environment()), 1L)
#test(9999.636, exprCols(x, ((..cols)), "by", with, environment()), 1L)
test(9999.637, exprCols(x, {{..cols}}, "j", with, environment()), 1L)
test(9999.638, exprCols(x, {{..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.639, exprCols(x, {{..cols}}, "by", with, environment()), 1L)
test(9999.641, exprCols(x, -..cols, "j", with, environment()), 1L)
test(9999.642, exprCols(x, -..cols, ".SDcols", with, environment()), 1L)
#test(9999.643, exprCols(x, -..cols, "by", with, environment()), 1L)
test(9999.644, exprCols(x, ((-..cols)), "j", with, environment()), 1L)
test(9999.645, exprCols(x, ((-..cols)), ".SDcols", with, environment()), 1L)
#test(9999.646, exprCols(x, ((-..cols)), "by", with, environment()), 1L)
test(9999.647, exprCols(x, {{-..cols}}, "j", with, environment()), 1L)
test(9999.648, exprCols(x, {{-..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.649, exprCols(x, {{-..cols}}, "by", with, environment()), 1L)
test(9999.651, exprCols(x, !..cols, "j", with, environment()), 1L)
test(9999.652, exprCols(x, !..cols, ".SDcols", with, environment()), 1L)
#test(9999.653, exprCols(x, !..cols, "by", with, environment()), 1L)
test(9999.654, exprCols(x, ((!..cols)), "j", with, environment()), 1L)
test(9999.655, exprCols(x, ((!..cols)), ".SDcols", with, environment()), 1L)
#test(9999.656, exprCols(x, ((!..cols)), "by", with, environment()), 1L)
test(9999.657, exprCols(x, {{!..cols}}, "j", with, environment()), 1L)
test(9999.658, exprCols(x, {{!..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.659, exprCols(x, {{!..cols}}, "by", with, environment()), 1L)
rm(cols)
# character as variable in parent scope, variable name overlapping to column name
V1 = c("V1", "V3")
test(9999.701, exprCols(x, V1, "j", with, environment()), 1L)
test(9999.702, exprCols(x, V1, ".SDcols", with, environment()), 1L)
#test(9999.703, exprCols(x, V1, "by", with, environment()), 1L)
test(9999.704, exprCols(x, ((V1)), "j", with, environment()), 1L)
test(9999.705, exprCols(x, ((V1)), ".SDcols", with, environment()), 1L)
#test(9999.706, exprCols(x, ((V1)), "by", with, environment()), 1L)
test(9999.707, exprCols(x, {{V1}}, "j", with, environment()), 1L)
test(9999.708, exprCols(x, {{V1}}, ".SDcols", with, environment()), 1L)
#test(9999.709, exprCols(x, {{V1}}, "by", with, environment()), 1L)
test(9999.711, exprCols(x, -V1, "j", with, environment()), 1L)
test(9999.712, exprCols(x, -V1, ".SDcols", with, environment()), 1L)
#test(9999.713, exprCols(x, -V1, "by", with, environment()), 1L)
test(9999.714, exprCols(x, ((-V1)), "j", with, environment()), 1L)
test(9999.715, exprCols(x, ((-V1)), ".SDcols", with, environment()), 1L)
#test(9999.716, exprCols(x, ((-V1)), "by", with, environment()), 1L)
test(9999.717, exprCols(x, {{-V1}}, "j", with, environment()), 1L)
test(9999.718, exprCols(x, {{-V1}}, ".SDcols", with, environment()), 1L)
#test(9999.719, exprCols(x, {{-V1}}, "by", with, environment()), 1L)
test(9999.721, exprCols(x, !V1, "j", with, environment()), 1L)
test(9999.722, exprCols(x, !V1, ".SDcols", with, environment()), 1L)
#test(9999.723, exprCols(x, !V1, "by", with, environment()), 1L)
test(9999.724, exprCols(x, ((!V1)), "j", with, environment()), 1L)
test(9999.725, exprCols(x, ((!V1)), ".SDcols", with, environment()), 1L)
#test(9999.726, exprCols(x, ((!V1)), "by", with, environment()), 1L)
test(9999.727, exprCols(x, {{!V1}}, "j", with, environment()), 1L)
test(9999.728, exprCols(x, {{!V1}}, ".SDcols", with, environment()), 1L)
#test(9999.729, exprCols(x, {{!V1}}, "by", with, environment()), 1L)
test(9999.731, exprCols(x, ..V1, "j", with, environment()), 1L)
test(9999.732, exprCols(x, ..V1, ".SDcols", with, environment()), 1L)
#test(9999.733, exprCols(x, ..V1, "by", with, environment()), 1L)
test(9999.734, exprCols(x, ((..V1)), "j", with, environment()), 1L)
test(9999.735, exprCols(x, ((..V1)), ".SDcols", with, environment()), 1L)
#test(9999.736, exprCols(x, ((..V1)), "by", with, environment()), 1L)
test(9999.737, exprCols(x, {{..V1}}, "j", with, environment()), 1L)
test(9999.738, exprCols(x, {{..V1}}, ".SDcols", with, environment()), 1L)
#test(9999.739, exprCols(x, {{..V1}}, "by", with, environment()), 1L)
test(9999.741, exprCols(x, -..V1, "j", with, environment()), 1L)
test(9999.742, exprCols(x, -..V1, ".SDcols", with, environment()), 1L)
#test(9999.743, exprCols(x, -..V1, "by", with, environment()), 1L)
test(9999.744, exprCols(x, ((-..V1)), "j", with, environment()), 1L)
test(9999.745, exprCols(x, ((-..V1)), ".SDcols", with, environment()), 1L)
#test(9999.746, exprCols(x, ((-..V1)), "by", with, environment()), 1L)
test(9999.747, exprCols(x, {{-..V1}}, "j", with, environment()), 1L)
test(9999.748, exprCols(x, {{-..V1}}, ".SDcols", with, environment()), 1L)
#test(9999.749, exprCols(x, {{-..V1}}, "by", with, environment()), 1L)
test(9999.751, exprCols(x, !..V1, "j", with, environment()), 1L)
test(9999.752, exprCols(x, !..V1, ".SDcols", with, environment()), 1L)
#test(9999.753, exprCols(x, !..V1, "by", with, environment()), 1L)
test(9999.754, exprCols(x, ((!..V1)), "j", with, environment()), 1L)
test(9999.755, exprCols(x, ((!..V1)), ".SDcols", with, environment()), 1L)
#test(9999.756, exprCols(x, ((!..V1)), "by", with, environment()), 1L)
test(9999.757, exprCols(x, {{!..V1}}, "j", with, environment()), 1L)
test(9999.758, exprCols(x, {{!..V1}}, ".SDcols", with, environment()), 1L)
#test(9999.759, exprCols(x, {{!..V1}}, "by", with, environment()), 1L)
rm(V1)
# numeric as variable in parent scope
cols = c(1,3)
test(9999.801, exprCols(x, cols, "j", with, environment()), 1L)
test(9999.802, exprCols(x, cols, ".SDcols", with, environment()), 1L)
#test(9999.803, exprCols(x, cols, "by", with, environment()), 1L)
test(9999.804, exprCols(x, ((cols)), "j", with, environment()), 1L)
test(9999.805, exprCols(x, ((cols)), ".SDcols", with, environment()), 1L)
#test(9999.806, exprCols(x, ((cols)), "by", with, environment()), 1L)
test(9999.807, exprCols(x, {{cols}}, "j", with, environment()), 1L)
test(9999.808, exprCols(x, {{cols}}, ".SDcols", with, environment()), 1L)
#test(9999.809, exprCols(x, {{cols}}, "by", with, environment()), 1L)
test(9999.811, exprCols(x, -cols, "j", with, environment()), 1L)
test(9999.812, exprCols(x, -cols, ".SDcols", with, environment()), 1L)
#test(9999.813, exprCols(x, -cols, "by", with, environment()), 1L)
test(9999.814, exprCols(x, ((-cols)), "j", with, environment()), 1L)
test(9999.815, exprCols(x, ((-cols)), ".SDcols", with, environment()), 1L)
#test(9999.816, exprCols(x, ((-cols)), "by", with, environment()), 1L)
test(9999.817, exprCols(x, {{-cols}}, "j", with, environment()), 1L)
test(9999.818, exprCols(x, {{-cols}}, ".SDcols", with, environment()), 1L)
#test(9999.819, exprCols(x, {{-cols}}, "by", with, environment()), 1L)
test(9999.821, exprCols(x, !cols, "j", with, environment()), 1L)
test(9999.822, exprCols(x, !cols, ".SDcols", with, environment()), 1L)
#test(9999.823, exprCols(x, !cols, "by", with, environment()), 1L)
test(9999.824, exprCols(x, ((!cols)), "j", with, environment()), 1L)
test(9999.825, exprCols(x, ((!cols)), ".SDcols", with, environment()), 1L)
#test(9999.826, exprCols(x, ((!cols)), "by", with, environment()), 1L)
test(9999.827, exprCols(x, {{!cols}}, "j", with, environment()), 1L)
test(9999.828, exprCols(x, {{!cols}}, ".SDcols", with, environment()), 1L)
#test(9999.829, exprCols(x, {{!cols}}, "by", with, environment()), 1L)
test(9999.831, exprCols(x, ..cols, "j", with, environment()), 1L)
test(9999.832, exprCols(x, ..cols, ".SDcols", with, environment()), 1L)
#test(9999.833, exprCols(x, ..cols, "by", with, environment()), 1L)
test(9999.834, exprCols(x, ((..cols)), "j", with, environment()), 1L)
test(9999.835, exprCols(x, ((..cols)), ".SDcols", with, environment()), 1L)
#test(9999.836, exprCols(x, ((..cols)), "by", with, environment()), 1L)
test(9999.837, exprCols(x, {{..cols}}, "j", with, environment()), 1L)
test(9999.838, exprCols(x, {{..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.839, exprCols(x, {{..cols}}, "by", with, environment()), 1L)
test(9999.841, exprCols(x, -..cols, "j", with, environment()), 1L)
test(9999.842, exprCols(x, -..cols, ".SDcols", with, environment()), 1L)
#test(9999.843, exprCols(x, -..cols, "by", with, environment()), 1L)
test(9999.844, exprCols(x, ((-..cols)), "j", with, environment()), 1L)
test(9999.845, exprCols(x, ((-..cols)), ".SDcols", with, environment()), 1L)
#test(9999.846, exprCols(x, ((-..cols)), "by", with, environment()), 1L)
test(9999.847, exprCols(x, {{-..cols}}, "j", with, environment()), 1L)
test(9999.848, exprCols(x, {{-..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.849, exprCols(x, {{-..cols}}, "by", with, environment()), 1L)
test(9999.851, exprCols(x, !..cols, "j", with, environment()), 1L)
test(9999.852, exprCols(x, !..cols, ".SDcols", with, environment()), 1L)
#test(9999.853, exprCols(x, !..cols, "by", with, environment()), 1L)
test(9999.854, exprCols(x, ((!..cols)), "j", with, environment()), 1L)
test(9999.855, exprCols(x, ((!..cols)), ".SDcols", with, environment()), 1L)
#test(9999.856, exprCols(x, ((!..cols)), "by", with, environment()), 1L)
test(9999.857, exprCols(x, {{!..cols}}, "j", with, environment()), 1L)
test(9999.858, exprCols(x, {{!..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.859, exprCols(x, {{!..cols}}, "by", with, environment()), 1L)
rm(cols)
# logical as variable in parent scope
cols = c(TRUE,FALSE,TRUE,FALSE,FALSE)
test(9999.901, exprCols(x, cols, "j", with, environment()), 1L)
test(9999.902, exprCols(x, cols, ".SDcols", with, environment()), 1L)
#test(9999.903, exprCols(x, cols, "by", with, environment()), 1L)
test(9999.904, exprCols(x, ((cols)), "j", with, environment()), 1L)
test(9999.905, exprCols(x, ((cols)), ".SDcols", with, environment()), 1L)
#test(9999.906, exprCols(x, ((cols)), "by", with, environment()), 1L)
test(9999.907, exprCols(x, {{cols}}, "j", with, environment()), 1L)
test(9999.908, exprCols(x, {{cols}}, ".SDcols", with, environment()), 1L)
#test(9999.909, exprCols(x, {{cols}}, "by", with, environment()), 1L)
test(9999.911, exprCols(x, -cols, "j", with, environment()), 1L)
test(9999.912, exprCols(x, -cols, ".SDcols", with, environment()), 1L)
#test(9999.913, exprCols(x, -cols, "by", with, environment()), 1L)
test(9999.914, exprCols(x, ((-cols)), "j", with, environment()), 1L)
test(9999.915, exprCols(x, ((-cols)), ".SDcols", with, environment()), 1L)
#test(9999.916, exprCols(x, ((-cols)), "by", with, environment()), 1L)
test(9999.917, exprCols(x, {{-cols}}, "j", with, environment()), 1L)
test(9999.918, exprCols(x, {{-cols}}, ".SDcols", with, environment()), 1L)
#test(9999.919, exprCols(x, {{-cols}}, "by", with, environment()), 1L)
test(9999.921, exprCols(x, !cols, "j", with, environment()), 1L)
test(9999.922, exprCols(x, !cols, ".SDcols", with, environment()), 1L)
#test(9999.923, exprCols(x, !cols, "by", with, environment()), 1L)
test(9999.924, exprCols(x, ((!cols)), "j", with, environment()), 1L)
test(9999.925, exprCols(x, ((!cols)), ".SDcols", with, environment()), 1L)
#test(9999.926, exprCols(x, ((!cols)), "by", with, environment()), 1L)
test(9999.927, exprCols(x, {{!cols}}, "j", with, environment()), 1L)
test(9999.928, exprCols(x, {{!cols}}, ".SDcols", with, environment()), 1L)
#test(9999.929, exprCols(x, {{!cols}}, "by", with, environment()), 1L)
test(9999.931, exprCols(x, ..cols, "j", with, environment()), 1L)
test(9999.932, exprCols(x, ..cols, ".SDcols", with, environment()), 1L)
#test(9999.933, exprCols(x, ..cols, "by", with, environment()), 1L)
test(9999.934, exprCols(x, ((..cols)), "j", with, environment()), 1L)
test(9999.935, exprCols(x, ((..cols)), ".SDcols", with, environment()), 1L)
#test(9999.936, exprCols(x, ((..cols)), "by", with, environment()), 1L)
test(9999.937, exprCols(x, {{..cols}}, "j", with, environment()), 1L)
test(9999.938, exprCols(x, {{..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.939, exprCols(x, {{..cols}}, "by", with, environment()), 1L)
test(9999.941, exprCols(x, -..cols, "j", with, environment()), 1L)
test(9999.942, exprCols(x, -..cols, ".SDcols", with, environment()), 1L)
#test(9999.943, exprCols(x, -..cols, "by", with, environment()), 1L)
test(9999.944, exprCols(x, ((-..cols)), "j", with, environment()), 1L)
test(9999.945, exprCols(x, ((-..cols)), ".SDcols", with, environment()), 1L)
#test(9999.946, exprCols(x, ((-..cols)), "by", with, environment()), 1L)
test(9999.947, exprCols(x, {{-..cols}}, "j", with, environment()), 1L)
test(9999.948, exprCols(x, {{-..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.949, exprCols(x, {{-..cols}}, "by", with, environment()), 1L)
test(9999.951, exprCols(x, !..cols, "j", with, environment()), 1L)
test(9999.952, exprCols(x, !..cols, ".SDcols", with, environment()), 1L)
#test(9999.953, exprCols(x, !..cols, "by", with, environment()), 1L)
test(9999.954, exprCols(x, ((!..cols)), "j", with, environment()), 1L)
test(9999.955, exprCols(x, ((!..cols)), ".SDcols", with, environment()), 1L)
#test(9999.956, exprCols(x, ((!..cols)), "by", with, environment()), 1L)
test(9999.957, exprCols(x, {{!..cols}}, "j", with, environment()), 1L)
test(9999.958, exprCols(x, {{!..cols}}, ".SDcols", with, environment()), 1L)
#test(9999.959, exprCols(x, {{!..cols}}, "by", with, environment()), 1L)
rm(cols)

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
