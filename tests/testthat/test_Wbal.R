context("Testing W' balance algorithm")

# ---Create some data --- #
## Some random inputs.
CP      <- sample(200:500, 1)
len     <- sort(sample(100:200, 3)) # Random segment lengths
pwr     <- c(sample(100:CP, 1), sample(CP:600, 1), sample(100:CP, 1))

## Create segments, manually generating W' balance.
stable  <- data.frame(time = rep_len(1, len[1]), power = pwr[1], Wbal = 0)

deplete <- data.frame(time = rep_len(1, len[2]), power = pwr[2],
                      Wbal = cumsum(rep_len(pwr[2], len[2]) - CP))

recover <- data.frame(time = rep_len(1, len[3]), power = pwr[3],
                      rec = exp(1)^(-seq_len(len[3])/(546*exp(1)^(-0.01*(CP-pwr[3]))+316)))

recover$Wbal <- max(deplete$Wbal) * recover$rec

## Bind segments together.
data      <- do.call("rbind", list(stable, deplete, recover[, -3]))
data$time <- cumsum(data$time)
# ----------------------- #

test_that("functions without NAs", {
  expect_equal(data$Wbal, Wbal_(data$time, data$power, CP))
  expect_equal(data$Wbal / 1000, Wbal(data, time, power, CP, noisy = TRUE)) # kJ.
})

test_that("with NAs", {
  ## NAs are allowed in power...
  data[sample(seq_len(nrow(data)), 1), "power"] <- NA
  expect_that(Wbal_(data$time, data$power, CP), not(throws_error()))
  ## ...but not time.
  data[sample(seq_len(nrow(data)), 1), "time"] <- NA
  expect_that(Wbal_(data$time, data$power, CP), throws_error())
})
