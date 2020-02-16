cc(F)

# towards more modular [.data.table #852 and step by step push-down it to C
# also closes #4115 (same #2178)
# #4004
# #2069
# #2059
# remove branch in data.table.R#L909

x = as.data.table(lapply(1:5, c))
with = copy(NA) # copy for dev till we not duplicate yet, to ensure not to override NA value
