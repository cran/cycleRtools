context("Checking NA splitting")

test_that("simple example", {
  x   <- floor(rnorm(100, 50, 20))
  nNa <- abs(floor(rnorm(1, 30, 10)))
  x[sample(seq_along(x), nNa)] <- NA
  sp <- split(x, cycleRtools:::na_split(x))
  expect_equal(length(sp[[1]]), nNa)
})

