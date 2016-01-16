context("Testing diff functions")

even <- function(x) x[seq_along(x) %% 2 == 0]
odd  <- function(x) x[seq_along(x) %% 2 == 1]

test_that("checking diff_section", {
  i    <- sort(sample(1:500, 100))
  ts.  <- unlist(mapply(":", odd(i), even(i)))
  br   <- abs(floor(rnorm(1, 10, 3)))
  goal <- rep_len(0, length(ts.))
  i    <- c(0, diff(ts.)) > br
  goal[c(1, which(i))] <- 1; goal <- cumsum(goal)
  test <- diff_section(ts., br)
  expect_equal(goal, test)
})

test_that("checking Diff", {
  expect_true(  # Without NAs.
    all(replicate(100, {
      x <- rnorm(1000, sample(200:400, 1), sample(100:300, 1))
      all.equal(diff(x), Diff(x))
    })))
  expect_true( # With NAs.
    all(replicate(100, {
      x <- rnorm(1000, sample(200:400, 1), sample(100:300, 1))
      x[sample(seq_along(x), 300)] <- NA
      all.equal(diff(x), Diff(x))
    })))
})
