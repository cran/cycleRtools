format_tcx <- function(data) {
  message("Formatting data...")
  data_formatted <- ride_data_skeleton(dim(data)[[1]])
  #-----------------------------------------------------------------------------
  data_formatted$timer.s <-
    as.numeric(data$Time) - as.numeric(data$Time[[1]])

  data_formatted$timer.min <-
    data_formatted$timer.s / 60

  data_formatted$timestamp <-
    as.POSIXct(data$Time, origin = Sys.time() - as.numeric(Sys.time()))

  data_formatted$delta.t <-
    c(0, diff(data_formatted$timer.s))

  data_formatted$distance.km <-
    data$DistanceMeters / 1000

  if (!is.null(data$PositionLatitudeDegrees))
    data_formatted$lat <- data$PositionLatitudeDegrees

  if (!is.null(data$PositionLongitudeDegrees))
    data_formatted$lng <- data$PositionLongitudeDegrees

  if (!is.null(data$ExtensionsTPXSpeed))
    data_formatted$speed.kmh <- (data$ExtensionsTPXSpeed * 60 ^ 2) / 1000

  data_formatted$elevation.m <-
    data$AltitudeMeters

  data_formatted$delta.elev <-
    c(0, diff(data_formatted$elevation.m))

  data_formatted$VAM <-
    data_formatted$delta.elev / data_formatted$timer.s
  data_formatted$VAM[[1]] <- 0
  data_formatted$VAM[data_formatted$VAM < 0] <- 0

  if (!is.null(data$ExtensionsTPXWatts)) {
    data_formatted$power.W <-
      data$ExtensionsTPXWatts

    data_formatted$power.smooth.W <-
      rollmean_smth(data_formatted, "power.W", 25, ema = TRUE)

    data_formatted$work.J <-
      cumsum(data_formatted$power.W * data_formatted$delta.t)
  }

  return(data_formatted)
}
