context("Checking cycleRdata methods")

test_that("'is' function", {
  expect_true(is.cycleRdata(intervaldata))
  dat <- data.frame(x = rnorm(20), y = rnorm(20, 100))
  expect_false(is.cycleRdata(dat))
})

test_that("name protection", {
  expect_true(is.cycleRdata(ridedata))
  names(ridedata) <- names(ridedata)         # Good names.
  expect_true(is.cycleRdata(ridedata))
  expect_warning(names(ridedata) <- "test")  # Bad names.
  expect_false(is.cycleRdata(ridedata))
})

test_that("colname protection", {
  expect_true(is.cycleRdata(ridedata))
  colnames(ridedata) <- colnames(ridedata)      # Good names.
  expect_true(is.cycleRdata(ridedata))
  expect_warning(colnames(ridedata) <- "test")  # Bad names.
  expect_false(is.cycleRdata(ridedata))
})
