#' Detect Intervals in a Ride.
#'
#' Often a ride will contain intervals/efforts that are not in any way marked in
#' the device data (e.g. as "laps"). Using changepoint analysis, it is possible
#' to retrospectively identify these efforts. This is contingent on supplying
#' the number of changepoints to the underlying algorithm, simplified here as a
#' \code{sections} argument. For example, if there are two efforts amidst a
#' ride, this means we are looking to identify 5 \emph{sections} (i.e.
#' neutral-effort-neutral-effort-neutral). See \strong{Examples}. Requires
#' package \code{"changepoint"}.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param sections how many sections should be identified? see
#'   \strong{Description}.
#' @param plot logical; if TRUE, graphically displays the resultant sections.
#' @param ... graphical parameters to be passed to \code{par()}. Not used if
#'   \code{plot = FALSE}.
#'
#' @return if \code{plot = TRUE} nothing is returned. If \code{plot = FALSE}
#'   (default) a vector of section "levels" is returned.
#'
#' @examples
#' # "interval_data" is a ride data set, which involved two 10 minute efforts
#' # during an otherwise steady ride.
#' data(interval_data)
#' interval_data$interval <- interval_detect(interval_data, 5, FALSE)
#' # Two efforts = 5 sections.
#' # Were the two 10 minute efforts properly identified?
#' tapply(interval_data$delta.t, interval_data$interval, sum) / 60
#' # Plot
#' interval_detect(interval_data, 5, TRUE)
#'
#' @export
interval_detect <- function(data, sections, plot = FALSE, ...)
  UseMethod("interval_detect", data)
#' @export
interval_detect.default <- function(data, sections, plot = FALSE, ...)
  format_error()
#' @export
interval_detect.cycleRdata <- function(data, sections, plot = FALSE, ...) {
  data  <- data[, c("timer.s", "power.W")]  # Isolate relevant columns.
  data  <- uniform(data, verbose = FALSE)

  na.rm <- which(is.na(data[, 2]))
  data[na.rm, 2] <- 0

  minseglen <- 10 / delta_estimate(data[, 1])  # 10 seconds.

  cpts <- suppressWarnings(changepoint::cpt.mean(
    data[, 2], method = "BinSeg", minseglen = minseglen,
    Q = (sections - 1),  # Number of changepoints.
    class = FALSE        # Dont return "cpt" object.
  ))
  # Assemble "section" values.
  data$section <- 0
  data$section[c(1, cpts[-length(cpts)])] <- 1
  data$section <- cumsum(data$section)

  if (plot == TRUE) {  # Doesn't return anything.
    opar <- par(no.readonly = TRUE)
    par(...)
    # Underlying power data.
    with(data, plot(
      x = timer.s, y = power.W, type = "l",
      col = "#333333", lwd = 0.5,
      xlab = "Time (sec)", ylab = "Power (watts)"
    ))
    # Overlay section lines (mean powers).
    data$avepower <- ave(data$power.W, data$section)
    s <- unique(data$section)
    for (i in s)
      with(data[data$section == i, ],
           lines(x = timer.s, y = avepower, lwd = 3, col = base_pal(i, s)))

    par(opar)  # Reset graphics device.
    return(invisible())
  }
  data <- data[-na.rm, ]
  return(data$section)   # If plot argument is FALSE.
}
