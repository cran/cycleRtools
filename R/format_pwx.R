format_pwx <- function(data) {
  message("Formatting data...")
  data_formatted <- ride_data_skeleton(dim(data)[[1]])
  #-----------------------------------------------------------------------------
  data_formatted$timer.s <-
    data$timeoffset - data$timeoffset[[1]]

  data_formatted$timer.min <-
    data_formatted$timer.s / 60

  data_formatted$timestamp <- data$timestamp

  data_formatted$delta.t <-
    c(0, diff(data_formatted$timer.s))

  data_formatted$distance.km <-
    data$dist / 1000

  data_formatted$speed.kmh <-
    (data$spd * 60^2) / 1000

  data_formatted$elevation.m <-
    data$alt

  data_formatted$elevation.m <-
    predict(smooth.spline(data_formatted$elevation.m))$y

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

  data_formatted$cadence.rpm <-
    data$cad

  return(data_formatted)
}
