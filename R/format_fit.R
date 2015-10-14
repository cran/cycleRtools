format_fit <- function(data) {
  message("Formatting data...")
  data_formatted <- ride_data_skeleton(dim(data)[[1]])
  #-----------------------------------------------------------------------------
  data_formatted$timer.s <-
    data$record.timestamp.s. - data$record.timestamp.s.[[1]]

  data_formatted$timer.min <-
    data_formatted$timer.s / 60

  data_formatted$delta.t <-
    c(0, diff(data_formatted$timer.s))

  data_formatted$timestamp <-
    as.POSIXct(data$record.timestamp.s., tz = "UTC", origin = "1989-12-31")

  data_formatted$lap <-
    data$lap
  # Columns conditional on positional data--------------------------------------
  if (!is.null(data$record.position_lat.semicircles.)) {
    data_formatted$lat <-
      data$record.position_lat.semicircles. * (180 / (2 ^ 31))

    data_formatted$lng <-
      data$record.position_long.semicircles. * (180 / (2 ^ 31))

    data_formatted$distance.km <-
      data$record.distance.m. / 1000

    data_formatted$speed.kmh <-
      (data$record.speed.m.s. * 60^2) / 1000
  }
  # Columns conditional on elevation data and/or positional data----------------
  if (!is.null(data$record.altitude.m.)) {
    data_formatted$elevation.m <-
      data$record.altitude.m.

    data_formatted$delta.elev <-
      c(0, diff(data$record.altitude.m.))

    data_formatted$VAM <-
      data_formatted$delta.elev / data_formatted$timer.s
    data_formatted$VAM[[1]] <- 0
    data_formatted$VAM[data_formatted$VAM < 0] <- 0
  }
  # Columns conditional on power data-------------------------------------------
  if (!is.null(data$record.power.watts.)) {
    data_formatted$power.W <-
      data$record.power.watts.

    data_formatted$power.smooth.W <-
      rollmean_smth(data_formatted, "power.W", 25, ema = TRUE)

    data_formatted$work.J <-
      cumsum(data_formatted$power.W * data_formatted$delta.t)
  }
  #-----------------------------------------------------------------------------
  return(data_formatted)
}
