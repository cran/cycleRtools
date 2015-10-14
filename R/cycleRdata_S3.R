#' Summary method for cycleRdata class.
#'
#' Relevant summary metrics for cycling data (class `cycleRdata`).
#'
#' @param object object for which a summary is desired.
#' @param sRPE optional; session Rating of Percieved Exertion (value between 1 and 10).
#' @param CP optional; Critical Power value (Watts).
#' @param round_digits optional; number of digits to round all metrics to.
#' @param prettynames self explanatory.
#' @param .smoothpwr character string; column name of smoothed power values. For internal use.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list object.
#'
#' @examples
#' data(cycling_data)
#' summary(cycling_data)
#'
#' @export
summary.cycleRdata <- function(object, sRPE = NULL, CP = NULL, round_digits = NULL,
                               prettynames = FALSE,  .smoothpwr = "power.smooth.W", ...) {

  data_exists <- function(column, d = object) {
    out <- all(!is.na(d[[column]]))
    return(out)
  }

  xPower <- function(x) {
    mean(x ^ 4, na.rm = TRUE) ^ (1 / 4)
  }
  # ----------------------------------------------------------------------------
  ride_tmin <- ride_time(object$timer.s) / 60

  if (!is.null(sRPE))
    if (sRPE > 10 | sRPE < 0)
      sRPE <- NULL
  # ----------------------------------------------------------------------------
  metrics <- list(
    date_time = c(
      ride.date        = format(object$timestamp[[1]], format = "%d %b %Y"),
      ride.start.time  = format(object$timestamp[[1]], format = "%H:%M:%S")
    ),

    overall = c(
      ride.time.min    = ride_tmin,

      elapsed.time.min = max(object$timer.s, na.rm = TRUE) / 60,

      distance.km      = ifelse(
        data_exists("distance.km"),
        max(object$distance.km, na.rm = TRUE), NA),

      work.kJ          = ifelse(
        data_exists("work.J"),
        max(object$work.J, na.rm = TRUE) / 1000, NA),

      norm.work.kJ     = ifelse(
        data_exists("power.W"),
        (ride_time(object$timer.s) * xPower(object[[.smoothpwr]])) / 1000, NA),

      climbing.m       = ifelse(
        data_exists("elevation.m"),
        sum(object[object$delta.elev > 0, "delta.elev"], na.rm = TRUE), NA),

      sRPE.score       = ifelse(
        !is.null(sRPE),
        sRPE * ride_tmin, NA),

      TRIMP.score      = ifelse(
        !is.null(CP) & data_exists("power.W"),
        pwr_TRIMP(object, CP), NA),

      speed.mean.kmh   = ifelse(
        data_exists("speed.kmh"),
        mean(object$speed.kmh, na.rm = TRUE), NA),

      speed.sd.kmh     = ifelse(
        data_exists("speed.kmh"),
        sd(object$speed.kmh, na.rm = TRUE), NA),

      power.mean.W     = ifelse(
        data_exists("power.W"),
        mean(object$power.W, na.rm = TRUE), NA),

      power.sd.W       = ifelse(
        data_exists("power.W"),
        sd(object$power.W, na.rm = TRUE), NA),

      xPower.W         = ifelse(
        data_exists(.smoothpwr),
        xPower(object[[.smoothpwr]]), NA),

      power.max.W       = ifelse(
        data_exists("power.W"),
        max(object$power.W, na.rm = TRUE), NA)
    )
  )
  if (prettynames) {
    names(metrics$date_time) <- c("Start date", "Start time")
    names(metrics$overall)   <- c(
      "Ride time (min)",
      "Elapsed time (min)",
      "Distance (km)",
      "Work (kJ)",
      "Normalised work (kJ)",
      "Climbing (m)",
      "sRPE score (au)",
      "TRIMP score (au)",
      "Mean speed (km.h)",
      "Speed SD (km.h)",
      "Mean power (W)",
      "Power SD (W)",
      "xPower (W)",
      "Max power (W)"
    )
  }
  # ----------------------------------------------------------------------------
  if (length(unique(object$lap)) > 1) {
    metrics$laps <- data.frame(
      lap.no          = as.numeric(unique(object$lap)),

      lap.time.min    = tapply(object$timer.s, object$lap, ride_time) / 60,

      lap.distance.km = if (data_exists("distance.km"))
        tapply(object$distance.km, object$lap, max, na.rm = TRUE)
      else NA,

      lap.work.kJ     = if (data_exists("work.J"))
        tapply(object$work.J, object$lap, max, na.rm = TRUE) / 1000
      else NA,

      lap.climbing.m  = if (data_exists("elevation.m"))
        tapply(object[object$delta.elev > 0, "delta.elev"],
               object[object$delta.elev > 0, "lap"], sum, na.rm = TRUE)
      else NA,

      lap.speed.mean.kmh   = if (data_exists("speed.kmh"))
        tapply(object$speed.kmh, object$lap, mean, na.rm = TRUE)
      else NA,

      lap.speed.sd.kmh   = if (data_exists("speed.kmh"))
        tapply(object$speed.kmh, object$lap, sd, na.rm = TRUE)
      else NA,

      lap.power.mean.W     = if (data_exists("power.W"))
        tapply(object$power.W, object$lap, mean, na.rm = TRUE)
      else NA,

      lap.power.sd.W     = if (data_exists("power.W"))
        tapply(object$power.W, object$lap, sd, na.rm = TRUE)
      else NA,

      lap.xPower.W    = if (data_exists("power.W"))
        tapply(object[[.smoothpwr]], object$lap, xPower)
      else NA,

      lap.power.max.W    = if (data_exists("power.W"))
        tapply(object$power.W, object$lap, max, na.rm = TRUE)
      else NA
    )

    if (data_exists("distance.km"))
      metrics$laps$lap.distance.km <- c(
        metrics$laps$lap.distance.km[[1]], diff(metrics$laps$lap.distance.km))
    if (data_exists("work.J"))
      metrics$laps$lap.work.kJ <- c(
        metrics$laps$lap.work.kJ[[1]], diff(metrics$laps$lap.work.kJ))

    if (prettynames)
      names(metrics$laps)   <- c(
        "Lap number",
        "Lap ride time (min)",
        "Lap distance (km)",
        "Lap work (kJ)",
        "Lap climbing (m)",
        "Lap mean speed (km.h)",
        "Lap speed SD (km.h)",
        "Lap mean power (W)",
        "Lap power SD (W)",
        "Lap xPower (W)",
        "Lap max power (W)"
      )
  }
  # ----------------------------------------------------------------------------
  if (!is.null(object$interval)) {
    metrics$intervals <- data.frame(
      interval.no          = as.numeric(unique(object$interval)),

      interval.time.min    = tapply(object$timer.s, object$interval, ride_time) / 60,

      interval.distance.km = if (data_exists("distance.km"))
        tapply(object$distance.km, object$interval, max, na.rm = TRUE)
      else NA,

      interval.work.kJ     = if (data_exists("work.J"))
        tapply(object$work.J, object$interval, max, na.rm = TRUE) / 1000
      else NA,

      interval.climbing.m  = if (data_exists("elevation.m"))
        tapply(object[object$delta.elev > 0, "delta.elev"],
               object[object$delta.elev > 0, "interval"], sum, na.rm = TRUE)
      else NA,

      interval.speed.mean.kmh   = if (data_exists("speed.kmh"))
        tapply(object$speed.kmh, object$interval, mean, na.rm = TRUE)
      else NA,

      interval.speed.sd.kmh   = if (data_exists("speed.kmh"))
        tapply(object$speed.kmh, object$interval, sd, na.rm = TRUE)
      else NA,

      interval.power.mean.W     = if (data_exists("power.W"))
        tapply(object$power.W, object$interval, mean, na.rm = TRUE)
      else NA,

      interval.power.sd.W     = if (data_exists("power.W"))
        tapply(object$power.W, object$interval, sd, na.rm = TRUE)
      else NA,

      interval.xPower.W    = if (data_exists("power.W"))
        tapply(object[[.smoothpwr]], object$interval, xPower)
      else NA,

      interval.power.max.W    = if (data_exists("power.W"))
        tapply(object$power.W, object$interval, max, na.rm = TRUE)
      else NA
    )

    if (data_exists("distance.km"))
      metrics$intervals$interval.distance.km <- c(
        metrics$intervals$interval.distance.km[[1]], diff(metrics$intervals$interval.distance.km))
    if (data_exists("work.J"))
      metrics$intervals$interval.work.kJ <- c(
        metrics$intervals$interval.work.kJ[[1]], diff(metrics$intervals$interval.work.kJ))

    if (prettynames)
      names(metrics$intervals)   <- c(
        "Interval number",
        "Interval ride time (min)",
        "Interval distance (km)",
        "Interval work (kJ)",
        "Interval climbing (m)",
        "Interval mean speed (km.h)",
        "Interval speed SD (km.h)",
        "Interval mean power (W)",
        "Interval power SD (W)",
        "Interval xPower (W)",
        "Interval max power (W)"
      )
  }
  # ----------------------------------------------------------------------------
  if (!is.null(round_digits)) {
    metrics[-1] <- lapply(metrics[-1], round, digits = round_digits)
  }
  return(metrics)
}
