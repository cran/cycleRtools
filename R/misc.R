# These functions aren't exported, but are otherwise useful.
#  ------------------------------------------------------------------------
# Estimate sampling frequency for a dataset.
delta_estimate <- function(x) {
  delta <- min(unique(diff(x)), na.rm = TRUE)
  if (delta < 1 & delta != 0.5) {
    delta <- match(delta, round(delta, digits = 1:20))
    delta <- 1 / (10 ^ delta)
  }
  return(delta)
}

# Useful binary operator for subsetting.
"%btwn%" <- function(x, rng) x >= rng[[1]] & x <= rng[[2]]

# Generate colour palette without dependencies.
base_pal <- function(x, s) {
  # s = unique values;
  # x = subsequently matched against s to give hex colour codes.
  if (length(s) == 1)
    return("#000000")
  else
    rainbow(n = length(s))[match(x, s)]
}

# Exponentially-weighted moving average weights.
ema_weights <- function(x) {
  alpha <- 2 / {x + 1}
  i <- 1:x
  sm <- sum({alpha * {1 - alpha} ^ {1 - i}})
  return({{alpha * {1 - alpha} ^ {1 - i}}} / sm)
}

# Round only numeric columns in a data.frame.
roundf <- function(x, digits = 2) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <- round(x[numeric_columns], digits)
  return(x)
}

# Error message for functions with specific cycleRdata methods.
format_error <- function()
  message(paste(
    "Data is not properly formatted.",
    "Import the data via a read* function with the 'format' argument as TRUE.",
    "See ?read.", sep = "\n"))

# Unload DynLib.
.onUnload <- function (libpath) {
  library.dynam.unload("cycleRtools", libpath)
}

# Skeleton for format_* functions.
ride_data_skeleton <- function(len) {
  vals <- rep(NA, times = len)
  x <- data.frame(
    timer.s        = vals,
    timer.min      = vals,
    timestamp      = vals,
    delta.t        = vals,
    lat            = vals,
    lng            = vals,
    distance.km    = vals,
    speed.kmh      = vals,
    elevation.m    = vals,
    delta.elev     = vals,
    VAM            = vals,
    power.W        = vals,
    power.smooth.W = vals,
    work.J         = vals,
    Wexp.kJ        = vals,
    cadence.rpm    = vals,
    lap            = rep(1, times = len)
  )
  return(x)
}
