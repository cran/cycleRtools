format_GC <- function(data, filename) {
  start_time <- as.POSIXct(tools::file_path_sans_ext(basename(filename)),
                           format = "%Y_%m_%d_%H_%M_%S", tz = "")
  message("Formatting data...")
  data_formatted <- ride_data_skeleton(dim(data)[[1]])
  #-----------------------------------------------------------------------------
  data_formatted$timer.s <-
    data$secs

  data_formatted$timer.min <-
    data_formatted$timer.s / 60

  data_formatted$delta.t <-
    c(0, diff(data_formatted$timer.s))

  data_formatted$timestamp <-
    start_time + data_formatted$timer.s

  # Columns conditional on positional data--------------------------------------
  if (!is.null(data$lat)) {
    data_formatted$lat <-
      data$lat

    data_formatted$lng <-
      data$lon

    data_formatted$distance.km <-
      data$km

    data_formatted$speed.kmh <-
      data$kph
  }
  # Columns conditional on elevation data and/or positional data----------------
  if (!is.null(data$alt)) {
    data_formatted$elevation.m <-
      data$alt

    data_formatted$delta.elev <-
      c(0, diff(data$alt))

    data_formatted$VAM <-
      data_formatted$delta.elev / data_formatted$timer.s

    data_formatted$VAM[[1]] <- 0
    data_formatted$VAM[data_formatted$VAM < 0] <- 0
  }
  # Columns conditional on power data-------------------------------------------
  if (!is.null(data$watts)) {
    data_formatted$power.W <-
      data$watts

    data_formatted$power.smooth.W <-
      rollmean_smth(data_formatted, "power.W", 25, ema = TRUE)

    data_formatted$work.J <-
      cumsum(data_formatted$power.W * data_formatted$delta.t)
  }
  #-----------------------------------------------------------------------------
  return(data_formatted)
}
