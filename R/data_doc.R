#' Example cycling data.
#'
#' Formatted cycling data from a Garmin head-unit. imported via
#' \code{cycleRtools::read_fit("file_path", format = TRUE)}.
#'
#' @format An object of class c("cycleRdata", "data.frame").
#' \describe{
#' \item{timer.s}{an ongoing timer (seconds). Stoppages are not recorded per se,
#' but rather represented as breaks in the continuity of the timer.}
#' \item{timer.min}{as above, but in units of minutes.}
#' \item{timestamp}{"POSIXct" values, describing the actual time of day.}
#' \item{delta.t}{delta time values (seconds). Obtained via base::diff(timer.s).}
#' \item{lat}{latitude values (units: degrees).}
#' \item{lng}{longitude values (units: degrees).}
#' \item{distance.km}{cumulative distance (kilometres).}
#' \item{speed.kmh}{speed in kilometres per hour.}
#' \item{elevation.m}{altitude in metres.}
#' \item{delta.elev}{delta elevation (metres). Obtained via base::diff(elevation.m).}
#' \item{VAM}{"vertical ascent metres per second".}
#' \item{power.W}{power readings (watts).}
#' \item{power.smooth.W}{an exponentially-weighted 25-second moving average of power.W values.}
#' \item{work.J}{cumulative work (joules).}
#' \item{Wexp.kJ}{W' expended in units of kilojoules. See ?Wbal and references therein.}
#' \item{cadence.rpm}{pedalling cadence in units of revolutions per minute (rpm).}
#' \item{lap}{a numeric vector of lap "levels". Will only have values > 1 if lap data is available.}
#' \item{.elevation.corrected.m}{added for the sake of example; see package vignette.}
#' }
"cycling_data"

#' Example cycling interval data.
#'
#' Formatted cycling data from a Garmin head-unit. imported via
#' \code{cycleRtools::read_fit("file_path", format = TRUE)}. Included to demonstrate
#' the use of \code{interval_detect()}.
#'
#' @format An object of class c("cycleRdata", "data.frame").
#' \describe{
#' \item{timer.s}{an ongoing timer (seconds). Stoppages are not recorded per se,
#' but rather represented as breaks in the continuity of the timer.}
#' \item{timer.min}{as above, but in units of minutes.}
#' \item{timestamp}{"POSIXct" values, describing the actual time of day.}
#' \item{delta.t}{delta time values (seconds). Obtained via base::diff(timer.s).}
#' \item{lat}{latitude values (units: degrees).}
#' \item{lng}{longitude values (units: degrees).}
#' \item{distance.km}{cumulative distance (kilometres).}
#' \item{speed.kmh}{speed in kilometres per hour.}
#' \item{elevation.m}{altitude in metres.}
#' \item{delta.elev}{delta elevation (metres). Obtained via base::diff(elevation.m).}
#' \item{VAM}{"vertical ascent metres per second".}
#' \item{power.W}{power readings (watts).}
#' \item{power.smooth.W}{an exponentially-weighted 25-second moving average of power.W values.}
#' \item{work.J}{cumulative work (joules).}
#' \item{Wexp.kJ}{W' expended in units of kilojoules. See ?Wbal and references therein.}
#' \item{lap}{a numeric vector of lap "levels". Will only have values > 1 if lap data is available.}
#' }
"interval_data"

#' An example Power-time profile
#'
#' A named numeric vector of best power values, where names are the
#' corresponding time periods (seconds).
"Pt_prof"
