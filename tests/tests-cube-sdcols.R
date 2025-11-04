# tests/tests-cube-sdcols.R
# Test: cube() support for patterns with .SDcols
# Related issue: https://github.com/Rdatatable/data.table/issues/7354

library(data.table)

cat("Running test for cube() with .SDcols pattern...\n")

DT <- data.table(a1 = 1:2, a2 = 3:4, b = 5:6)

result <- tryCatch({
  cube(DT, .SDcols = patterns("^a"))
  "No error"
}, error = function(e) {
  paste("Error:", e$message)
})

cat("Result:", result, "\n")

# Expected: Should ideally handle .SDcols with patterns, but currently errors.

