context("Checking maximal mean value functions")

test_that("mmv2 with artifical data", {
  ## Rolling window size.
  win  <- abs(floor(rnorm(1, 100, 50)))

  ## Create a best window...
  best   <- rnorm(1, 400, 100)
  tofind <- rep_len(best, win)
  ## ... and hide it in some data.
  pad <- rnorm(sample(500:1500, 1), 100, 50)
  pad[pad > best] <- 0  # To be sure.

  expect_equal(best, mmv2(c(pad, tofind, pad), win))
})

test_that("mmv with example data", {
  win  <- abs(floor(rnorm(1, 100, 50)))
  dat  <- head(ridedata, win * 4)

  ## mmv will expand stops, which isn't appropriate for this test.
  if (any(dat$delta.t > 10)) stop("Data shouldn't be expanded!")

  test <- numeric(nrow(dat))
  for (i in win:nrow(dat))
    test[i] <- mean(dat$power.W[(i-(win-1)):i], na.rm = TRUE)
  out  <- unname(mmv(dat, "power.W", win)[1, ])

  expect_equal(max(test), out)
})
