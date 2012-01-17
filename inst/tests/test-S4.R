context("S4 Compatability")

## S4 class definitions to test
setClass("Data.Table", contains="data.table")
setClass("S4Composition", representation(data="data.table"))

test_that("data.table can be a parent class", {
  ids <- sample(letters[1:3], 10, replace=TRUE)
  scores<- rnorm(10)
  dt <- data.table(id=ids, score=scores)
  dt.s4 <- new("Data.Table", data.table(id=ids, score=scores))

  expect_true(isS4(dt.s4))
  expect_true(inherits(dt.s4, 'data.table'))

  ## pull out data from S4 as.list, and compare to list from dt
  dt.s4.list <- dt.s4@.Data
  names(dt.s4.list) <- names(dt.s4)
  expect_equal(dt.s4.list, as.list(dt), check.attributes=FALSE,
               info="Underlying data not identical")
})

test_that("simple S4 conversion-isms work", {
  df = data.frame(a=sample(letters, 10), b=1:10)
  dt = as.data.table(df)
  expect_equal(as(df, 'data.table'), dt, check.attributes=FALSE)
  expect_equal(as(dt, 'data.frame'), df, check.attributes=FALSE)
})

test_that("data.table can be used in an S4 slot", {
  ## A class with a data.table slot
  dt <- data.table(a=sample(letters[1:3], 10, replace=TRUE), score=rnorm(10))
  dt.comp <- new("S4Composition", data=dt)
  expect_equal(dt.comp@data, dt)
})

test_that("S4 methods dispatch properly on data.table slots", {
  ## Make toy accessor functions and compare results against normal data.table
  ## access
  dt <- data.table(a=sample(letters[1:3], 10, replace=TRUE), score=rnorm(10))
  dt.comp <- new("S4Composition", data=dt)
  setGeneric("dtGet", function(x, what) standardGeneric("dtGet"))
  setMethod("dtGet", c(x="S4Composition", what="missing"),
            function(x, what) {
              x@data
            })
  setMethod("dtGet", c(x="S4Composition", what="ANY"),
            function(x, what) {
              x@data[[what]]
            })

  expect_equal(dtGet(dt.comp), dt, label='actually')
  expect_identical(dtGet(dt.comp, 1), dt[[1]])
  expect_identical(dtGet(dt.comp, 'b'), dt$b)
})


