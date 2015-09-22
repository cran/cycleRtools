#' W' balance plots.
#'
#' Generate three plots that effectively summarise a whole cycling dataset.
#'
#' @details When passed a \strong{formatted} dataset, creates a column of three
#'   plots: \enumerate{\item W' balance. \item Power (raw with 25 sec moving
#'   averaged overlaid). \item Elevation profile} How many of these plots are
#'   shown is controlled by the \code{n} argument.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param x numeric; \code{1} will plot against time and \code{2} will plot
#'   against distance.
#' @param CP a value for critical power annotation.
#' @param n how many plots should be created (see details).
#' @param xlim passed to \code{\link[graphics]{plot}}.
#'
#' @return a variable number of plots.
#'
#' @seealso \code{\link{Wbal}}.
#'
#' @examples
#' data(cycling_data)
#' Wbal_plots(cycling_data, x = 2, CP = 300)
#' # Show just W' balance.
#' Wbal_plots(cycling_data, x = 2, n = 1)
#' # Zoom all plots to 20-40 km.
#' Wbal_plots(cycling_data, x = 2, CP = 300, xlim = c(20, 40))
#'
#' @export
Wbal_plots <- function(data, x = 1, CP = NULL, n = 3, xlim = NULL)
  UseMethod("Wbal_plots", data)
#' @export
Wbal_plots.default <- function(data, x = 1, CP = NULL, n = 3, xlim = NULL)
  format_error()
#' @export
Wbal_plots.cycleRdata <- function(data, x = 1, CP = NULL, n = 3, xlim = NULL) {
  if (all(is.na(data$Wexp.kJ))) {
    stop("No W' balance data to plot", call. = FALSE)
  }
  opar <- par(no.readonly = TRUE)  # Save par.
  x <- ifelse(x == 1, "timer.s", "distance.km")
  if (is.null(xlim)) xlim <- c(0, max(data[, x]))
  par(mfrow = c(n, 1))
  xs <- pretty(data[data[, x] %btwn% xlim, ][, x])
  cex <- ifelse(n == 1, 1, ifelse(n == 2, 1.25, 1.5))
  #-----------------------------------------------------------------------------
  if (n == 1) par(mar = c(4, 5.5, 2, 2)) else par(mar = c(0, 5.5, 3, 2))
  plot(
    x = data[, x], y = data$Wexp.kJ, type = "l", bty = "n", xaxs = "i",
    col = "red", lwd = 1.7, las = 2, ylim = rev(extendrange(data$Wexp.kJ)),
    xlim = xlim, ann = FALSE, axes = FALSE
  )
  axis(side = 1, labels = FALSE, at = xs)
  axis(side = 2, las = 2, cex.axis = cex)
  mtext("W\' expended (kJ)", side = 2, line = 4, col = "red")
  title(main = paste(
    "Max W' Expended (kJ):",
    round(max(data$Wexp.kJ, na.rm = TRUE), digits = 2)
  ), cex.main = {1.3 * cex})
  # Power data------------------------------------------------------------------
  if (n > 1) {
    if (n == 2) par(mar = c(4, 5.5, 2, 2)) else par(mar = c(0, 5.5, 2, 2))
    smth_plot(data, x, "power.W", "power.smooth.W",
              xlim = xlim, ann = FALSE, axes = FALSE)
    if (!is.null(CP)) abline(h = CP, lty = 2, col = "red", lwd = 1.5)
    axis(side = 1, labels = FALSE, at = xs)
    axis(side = 2, las = 2, cex.axis = cex)
    mtext("Power (W)", side = 2, line = 4)
  }
  # Elevation profile-----------------------------------------------------------
  if (n > 2) {
    par(mar = c(4, 5.5, 2, 2))
    plot(
      x = data[, x], y = data$elevation.m, type = "l", bty = "n",
      col = "green", xaxs = "i", ylim = extendrange(data$elevation.m),
      xlim = xlim, ann = FALSE, axes = FALSE
    )
    axis(side = 2, las = 2, cex.axis = cex)
    mtext("Elevation (m)", side = 2, line = 4, col = "green")
  }
  # xlab------------------------------------------------------------------------
  if (x == "timer.s") {
    axis(side = 1, labels = round(xs / 60), at = xs, cex.axis = cex)
    mtext("Time (min)", side = 1, line = 2.5)
  } else {
    axis(side = 1, labels = round(xs), at = xs, cex.axis = cex)
    mtext("Distance (km)", side = 1, line = 2.5)
  }
  #-----------------------------------------------------------------------------
  par(opar)  # Reset graphics device.
}
