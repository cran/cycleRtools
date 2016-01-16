context("Testing rolling operations")

test_that("weights generated correctly", {
  ema_weights_R <- function(x) {
    alpha <- 2 / (x + 1)
    i <- 1:x
    sm <- sum((alpha * (1 - alpha) ^ (1 - i)))
    (alpha * (1 - alpha) ^ (1 - i)) / sm
  }
  expect_equal(ema_weights(10), ema_weights_R(10))
  expect_equal(mean_weights(13), rep_len(1 / 13, 13))
})

win <- 30; x <- rnorm(100, 200, 50)

test_that("rolling mean", {
  rm  <- rollmean_(x, window = win, ema = FALSE, narm = FALSE)
  expect_equal(mean(x[1:win]), rm[win])

  i  <- sample(seq_along(x)[-(1:win)], 10)
  t1 <- vapply(i, function(m) mean(x[((m + 1) - win):m]), numeric(1))
  t2 <- rm[i]
  expect_equal(t1, t2)
})

test_that("exponentially-weighted rolling mean", {
  rm  <- rollmean_(x, window = win, ema = TRUE, narm = FALSE)
  wts <- ema_weights(win)
  expect_equal(sum(x[1:win] * wts), rm[win])

  i   <- sample(seq_along(x)[-(1:win)], 10)
  t1  <- vapply(i, function(m) sum(x[((m + 1) - win):m] * wts), numeric(1))
  t2  <- rm[i]
  expect_equal(t1, t2)
})

x[sample(seq_along(x)[-(1:win)], 10)] <- NA  # Insert NAs.

test_that("check na.rm with rolling mean", {
  rm_wna   <- rollmean_(x, window = win, ema = FALSE, narm = FALSE)
  rm_wona  <- rollmean_(x, window = win, ema = FALSE, narm = TRUE)

  i <- sample(seq_along(x)[-(1:win)], 10)
  t1 <- vapply(i, function(m) mean(x[((m + 1) - win):m], na.rm = FALSE), numeric(1))
  t2 <- rm_wna[i]
  expect_equal(t1, t2)

  t1 <- vapply(i, function(m) mean(x[((m + 1) - win):m], na.rm = TRUE), numeric(1))
  t2 <- rm_wona[i]
  expect_equal(t1, t2)
})

test_that("check na.rm with ema", {
  rm_wna   <- rollmean_(x, window = win, ema = TRUE, narm = FALSE)
  rm_wona  <- rollmean_(x, window = win, ema = TRUE, narm = TRUE)
  wts      <- ema_weights(win)

  i   <- sample(seq_along(x)[-(1:win)], 10)
  t1  <- vapply(i, function(m) sum(x[((m + 1) - win):m] * wts), numeric(1))
  t2  <- rm_wna[i]
  expect_equal(t1, t2)

  t1  <- vapply(i, function(m) {
    x <- na.omit(x[((m + 1) - win):m])
    sum(x * ema_weights(length(x)))
  }, numeric(1))
  t2  <- rm_wona[i]
  expect_equal(t1, t2)
})

test_that("check non-uniform rolling mean", {
  ## Create awkwardly sampled data.
  time <- cumsum(round(abs(rnorm(1000, 4, 2)), 2))
  x    <- runif(1000, 300, 400)
  df   <- data.frame(t = time, x)
  win  <- sample(40:60, 1)

  df$roll <- rollmean_nunif(df$x, df$t, window = win)

  r <- sample(seq_along(df$t), 1)       # Pick a random timestamp.
  tmp <- df[1:r, ]                      # Chop off the top.
  tmp <- tmp[tmp$t > (df$t[r] - win), ] # Chop off the bottom.
  expect_equal(mean(tmp$x), tmp$roll[nrow(tmp)])
})
