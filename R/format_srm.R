format_srm <- function(data) {
  message("Formatting data...")
  data_formatted <- ride_data_skeleton(dim(data)[[1]])
  #-----------------------------------------------------------------------------
  data_formatted$timer.s <-
    data$time - data$time[[1]]

  data_formatted$timer.min <-
    data_formatted$timer.s / 60

  data_formatted$delta.t <-
    c(0, diff(data_formatted$timer.s))

  data_formatted$speed.kmh <-
    data$speed

  data_formatted$distance.km <-
    cumsum(data_formatted$speed.kmh * (data_formatted$delta.t / (60 ^ 2)))

  data_formatted$elevation.m <-
    data$ele

  data_formatted$delta.elev <-
    c(0, diff(data_formatted$elevation.m))

  data_formatted$VAM <-
    data_formatted$delta.elev / data_formatted$timer.s
  data_formatted$VAM[[1]] <- 0
  data_formatted$VAM[data_formatted$VAM < 0] <- 0

  data_formatted$power.W <-
    data$pwr

  data_formatted$power.smooth.W <-
    rollmean_smth(data_formatted, "power.W", 25, ema = TRUE)

  data_formatted$work.J <-
    cumsum(data_formatted$power.W * data_formatted$delta.t)

  return(data_formatted)
}
