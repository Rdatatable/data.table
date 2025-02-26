options(width = 200)
# Load compiled shared object
dyn.load("/home/yadav/dataTable/data.table/src/fmelt.so")

# Ensure the function is loaded
stopifnot(is.loaded("uniq_diff"))

# Test cases
test_uniq_diff <- function() {
  cat("Running tests for uniq_diff...\n")
  
  # Load required R functions
  library(data.table)
  
  # Test 1: Valid integer vector input
  input_1 <- as.integer(c(1, 2, 3, 4, 5))
  result_1 <- .Call("uniq_diff", input_1, as.integer(5), FALSE)
  expected_1 <- input_1  # Should return unique values
  stopifnot(identical(result_1, expected_1))
  cat("Test 1 passed!\n")
  
  # Test 2: Input with duplicates
  input_2 <- as.integer(c(1, 2, 2, 3, 4, 4, 5))
  result_2 <- .Call("uniq_diff", input_2, as.integer(5), FALSE)
  expected_2 <- as.integer(c(1, 2, 3, 4, 5))  # Should remove duplicates
  stopifnot(identical(result_2, expected_2))
  cat("Test 2 passed!\n")
  
  # Test 3: Invalid column numbers (out of range)
  input_3 <- as.integer(c(-1, 0, 1, 6, 2, 3))
  tryCatch({
    result_3 <- .Call("uniq_diff", input_3, as.integer(5), FALSE)
    cat("Test 3 failed: Expected an error but none occurred.\n")
  }, error = function(e) {
    cat("Test 3 passed! Caught expected error: ", e$message, "\n")
  })
  
  # Test 4: NA values in the input
  input_4 <- as.integer(c(1, 2, NA, 3, 4))
  result_4 <- .Call("uniq_diff", input_4, as.integer(5), TRUE)
  expected_4 <- as.integer(c(1, 2, NA, 3, 4))  # Should allow NA if is_measure is TRUE
  stopifnot(identical(result_4, expected_4))
  cat("Test 4 passed!\n")
  
  # Test 5: Empty input
  input_5 <- as.integer(integer(0))
  result_5 <- .Call("uniq_diff", input_5, as.integer(5), FALSE)
  stopifnot(length(result_5) == 0)  # Should return empty
  cat("Test 5 passed!\n")
  
  cat("All tests completed successfully!\n")
}

# Run the tests
test_uniq_diff()
