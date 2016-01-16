context("Testing stop expansion")

"%notbtwn%" <- function(x, rng) x < floor(rng[[1]]) | x > floor(rng[[2]])
len <- sample(50:200, 1)

test_that("simple usage", {
  len <- length(t <- seq(from = 1, to = len, by = sample(c(0.5, 1), 1)))
  # Remove middle of sequence.
  t   <- t[i <- seq_along(t) %notbtwn% (len * c(0.25, 0.75))]
  dat <- data.frame(t, x = rnorm(length(t)))
  dat <- cycleRtools:::expand_stops(dat, tcol = "t")
  expect_equal(dat[!i, "x"], rep_len(NA_real_, length(dat[!i, "x"])))
})

test_that("non-uniform sampling", {
  len <- length(t <- seq(from = 1, to = len, by = sample(c(0.5, 1), 1)))
  # Add noise to the sampling column.
  t <- rnorm(len, 5, 2)
  # Remove middle of sequence.
  t   <- t[i <- seq_along(t) %notbtwn% (len * c(0.25, 0.75))]
  dat <- data.frame(t, x = rnorm(length(t)))
  expect_error(cycleRtools:::expand_stops(dat, tcol = "t"))
})
