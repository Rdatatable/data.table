context("as.data.table and as.data.frame should yield comparable results")

as.dt_as.df_are.same <- function(x, quietly=FALSE, check.names=FALSE, showWarnings=TRUE) {
## compares as.data.table(x) to as.data.frame(x) 
##  by wrapping the former in as.data.frame and testing with identical
## returns TRUE if all values are equal, FALSE otherswise


  x.dt <- try(as.data.table(x), silent=TRUE)
  x.df <- try(as.data.frame(x, stringsAsFactors=FALSE), silent=TRUE)

  ## Check for errors
  iserr.dt <- inherits(x.dt, "try-error")
  iserr.df <- inherits(x.df, "try-error")
  if (iserr.dt && iserr.df) {
    message("both of as.data.frame(x) and as.data.table(x) threw an error")
    return(invisible(TRUE))
  }
  if (iserr.dt && !iserr.df) {
    warning("DT failed but DF did not")
    return(invisible(FALSE))
  }
  if (!iserr.dt && iserr.df) {
    warning("DF failed but DT did not")
    return(invisible(FALSE))
  }

  ## will compare x.dt_as.df to x.df
  x.dt_as.df <- as.data.frame(x.dt)

  if (!check.names) {
    setattr(x.dt, "names", rep(NA_character_, ncol(x.dt)))
    setattr(x.df, "names", rep(NA_character_, ncol(x.df)))
    rownames(x.df) <- NULL
    setattr(x.dt_as.df, "names", rep(NA_character_, ncol(x.df)))
    rownames(x.dt_as.df) <- NULL
  }

  ret <- identical(x.dt_as.df, x.df)

  ## if indetical fails, check the acutal values. 
  ## Perhaps it is just an attribtue that is different
  if (!ret && identical(dim(x.dt_as.df), dim(x.df))) {
    if (all(x.dt_as.df == x.df)) {
      ## try clearing attributes
      setattr(x.df[[1]], "dim", NULL)
      ## if still not idetnical, throw warning, but return TRUE
      if (showWarnings && !identical(x.df, x.dt_as.df))
        warning("All values and dimensions are the same between DF and DT.\nHowever, some differences remain, perhaps in attirbute or attirbute of a column")
      return(invisible(TRUE))
    }
  }

  ## If FALSE, use expect_equal for the detailed output
  if (!ret)
    return(testthat::expect_equal(x.dt_as.df, x.df, info="comparing as.data.frame(x) to as.data.frame(as.data.table(x))"))

  return(invisible(ret))
}


###############################################################################
## as.* conversions
test_that("convert matrices to data.table/data.frame", {
  mat_num  <- matrix(1:12, ncol=3)
  mat_char <- matrix(LETTERS[1:12], ncol=3)

  A <- array(1:30, dim=c(3, 2, 5))
  B <- array(-(1:600), dim=c(3, 2, 5, 2))
  A_onedim <- array(1:3, dim=c(3))
  A_twodim <- array(1:6, dim=c(3, 2))
  A_twodim_1x6 <- array(1:6, dim=c(1, 6))
  A.char <- copy(A)
  A.char[] <- c(LETTERS, letters)[A]
  B.char <- copy(B)
  B.char[] <- sapply(seq(B), function(x) paste(sample(LETTERS, 3), collapse=""))
  list_of_arrays <- list(A, B, 101:103)
  list_of_arrays_mats <- list(A, B, t(mat_char), 101:103)
  list_of_arrays_mats2 <- list(B.char, A, B, t(mat_char), 101:103)
  list_of_one_dim_arrays <- list(A_onedim, A_onedim)
  list_of_arrays_mats_fail <- list(A, B, mat_char, 101:103)  # failure expected for this one

  expect_error(as.data.table(list_of_arrays_mats_fail), regex="imply differing number", info="list of matrix and vectors. as.data.table should fail")

  expect_true(  as.dt_as.df_are.same(mat_char) )
  expect_true(  as.dt_as.df_are.same(mat_num) )
  expect_true(  as.dt_as.df_are.same(A) )
  expect_true(  as.dt_as.df_are.same(B) )
  expect_true(  as.dt_as.df_are.same( mat_char ) )
  expect_true(  as.dt_as.df_are.same( A_onedim ) )
  expect_true(  as.dt_as.df_are.same( A_twodim ) )
  expect_true(  as.dt_as.df_are.same( A_twodim_1x6 ) )
  expect_true(  as.dt_as.df_are.same( A.char ) )
  expect_true(  as.dt_as.df_are.same( B.char ) )
  expect_true(  as.dt_as.df_are.same( list_of_arrays ) )
  expect_true(  as.dt_as.df_are.same( list_of_arrays_mats ) )
  expect_true(  as.dt_as.df_are.same( list_of_arrays_mats2 ) )
  expect_true(  as.dt_as.df_are.same(list_of_one_dim_arrays, showWarnings=FALSE) )

})

