context("Checking summary metrics")

even <- function(x) x[seq_along(x) %% 2 == 0]
odd  <- function(x) x[seq_along(x) %% 2 == 1]

test_that("ride_time", {
  i  <- sort(sample(1:500, 100))
  x  <- unlist(mapply(":", odd(i), even(i)))
  expect_less_than(ride_time(x), max(x))
  expect_equal(ride_time(x), length(x))
})

test_that("NP, TSS etc.", {
  expect_that(length(NP(ridedata)), equals(1))
  expect_that(length(xPower(ridedata)), equals(1))
  expect_that(length(TSS(ridedata)), equals(1))
  expect_that(length(pwr_TRIMP(ridedata)), equals(1))
})
