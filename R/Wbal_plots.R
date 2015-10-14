#' W' balance plots.
#'
#' Generate three plots that effectively summarise a whole cycling dataset.
#'
#' The \code{n} argument describes plot options such that: \enumerate{\item
#' plots W' balance (kJ). \item plots power data (W). \item plots an elevation
#' profile (m).} These options can be concatenated to produce a stack of plots
#' as desired.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param x numeric; \code{1} will plot against time (sec); \code{2} will plot
#'   against time (minutes); and \code{3} will plot against distance.
#' @param n plots to be created (see details).
#' @param xlab character; x axis label for bottom plot.
#' @param xlim given in terms of \code{x}.
#' @param CP a value for critical power annotation.
#' @param laps logical; should laps be seperately coloured?
#' @param ... graphical parameters to be passed to \code{par()}.
#'
#' @return a variable number of plots.
#'
#' @seealso \code{\link{Wbal}}.
#'
#' @examples
#' data(cycling_data)
#' Wbal_plots(cycling_data, x = 2, n = c(1, 2, 3), CP = 300)
#' # Show just W' balance.
#' Wbal_plots(cycling_data, x = 2, n = 1)
#' # Elevation profile on top
#' Wbal_plots(cycling_data, x = 2, n = c(3, 1))
#' # Zoom all plots to 20-40 km.
#' Wbal_plots(cycling_data, x = 3, CP = 300, xlim = c(20, 40), xaxs = "i")
#'
#' @export
Wbal_plots            <- function(data, x = 1, n = c(1, 2, 3), xlab = NULL, xlim = NULL,
                                  CP = NULL, laps = FALSE, ...)
  UseMethod("Wbal_plots", data)
#' @export
Wbal_plots.default    <- function(data, x = 1, n = c(1, 2, 3), xlab = NULL, xlim = NULL,
                                  CP = NULL, laps = FALSE, ...)
  format_error()
#' @export
Wbal_plots.cycleRdata <- function(data, x = 1, n = c(1, 2, 3), xlab = NULL, xlim = NULL,
                                  CP = NULL, laps = FALSE, ...) {
  if (all(is.na(data$Wexp.kJ))) {
    stop("No W' balance data to plot", call. = FALSE)
  }
  # Plotting function definitions ----------------------------------------------
  Wbal_plot <- function(x) {
    plot(
      x = data[, x], y = data$Wexp.kJ,
      type = "l", col = "red", lwd = 1.7,
      ylab = "W' expended (kJ)", xlab = "",
      col.lab = "red", col.main = "red",
      main = paste(
        "Max W' Expended (kJ):",
        round(max(data$Wexp.kJ, na.rm = TRUE), digits = 2)
      ),
      ylim = rev(extendrange(data$Wexp.kJ))  # Invert axis.
    )
  }

  pwr_plot <- function(x, laps) {
    col <- ifelse(laps, "lap", "blue")
    smth_plot(data, x, "power.W", "power.smooth.W", colour = col,
              ylab = "Power (W)", xlab = "",
              col.lab = "blue", col.main = "blue",
              main = paste(
                "xPower:",
                round(mean(data$power.smooth.W ^ 4, na.rm = TRUE) ^ (1 / 4), 2)
              ),
              .string = TRUE)

    if (!is.null(CP))
      abline(h = CP, lty = 2, col = "red", lwd = 1.5)
  }

  elev_plot <- function(x) {
    climbing <- sum(data[data$delta.elev > 0, "delta.elev"], na.rm = TRUE)

    x    <- data[, x]
    y    <- data[, "elevation.m"]
    ylim <- extendrange(y)

    plot(x, y, type = "l", lwd = 3,
         ylab = "Elevation (m)", xlab = "",
         ylim = ylim,
         col.lab = "green", col.main = "green",
         main = paste("Climbing:", round(climbing, 2), "m"))

    y[is.na(y)] <- ylim[1]  # Creates breaks in polygon.
    area <- list(
      x = c(x[[1]], x, x[[length(x)]]),
      y = c(ylim[1], y, ylim[1])
    )
    polygon(area$x, area$y, col = "green", border = NA)
  }
  # ----------------------------------------------------------------------------
  opar <- par(no.readonly = TRUE)
  x    <- switch(x,
                 "timer.s",
                 "timer.min",
                 "distance.km"
  )
  if (!is.null(xlim))  # Subset data according to xlim arg (for titles).
    data <- data[data[, x] %btwn% xlim, ]

  par(mfrow = c(length(n), 1), ...)
  for (i in n)
    switch(i,
           Wbal_plot(x),
           pwr_plot(x, laps),
           elev_plot(x)
    )
  # Generate bottom most xlab.
  if (!is.null(xlab))
    mtext(xlab, side = 1, line = 2.5)
  else
    switch(x,
           "timer.s"     = mtext("Time (sec)", side = 1, line = 2.5),
           "timer.min"   = mtext("Time (min)", side = 1, line = 2.5),
           "distance.km" = mtext("Distance (km)", side = 1, line = 2.5)
    )
  par(opar)  # Reset graphics device.
}
