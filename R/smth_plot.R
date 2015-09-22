#' Smoothed data plot.
#'
#' Create a plot with both raw and smoothed data lines. Convenience wrapper for
#' \link[graphics]{plot} whereby two plots are overaid with \code{par(new =
#' TRUE)}.
#'
#' @param data the dataset to be used.
#' @param x column identifier for the x axis data.
#' @param yraw column identifier for the (underlying) raw data.
#' @param ysmth column identifier for the smoothed data.
#' @param xlim optional xlim values to be passed to \code{plot()}.
#' @param ... further arguments to be passed to \code{plot()}.
#'
#' @examples
#' data(cycling_data)
#' smth_plot(cycling_data, timer.s, power.W, power.smooth.W)  # Default arguments.
#'
#' @export
smth_plot <- function(data, x = "timer.s", yraw = "power.W", ysmth = "power.smooth.W",
                      xlim = NULL, ...) {
  caller <- deparse(sys.call(-1))
  # If the caller isn't Wbal_plots...
  if (!any(grepl("Wbal_plots", caller))) {
    x     <- as.character(substitute(x))
    yraw  <- as.character(substitute(yraw))
    ysmth <- as.character(substitute(ysmth))
  }
  #-----------------------------------------------------------------------------
  ylim <- extendrange(data[, yraw])
  # Specified xlim is necessary for overlaying different lap lines.
  if (is.null(xlim)) xlim = c(0, max(data[, x]))
  #-----------------------------------------------------------------------------
  plot(
    x = data[, x], y = data[, yraw], type = "l", bty = "l", las = 1,
    xaxs = "i", ylim = ylim, xlim = xlim, col = "gray", ...
  )
  par(new = TRUE)
  if (!is.null(data$lap)) {
    s <- as.numeric(unique(data$lap))
    s <- s[!is.na(s)]
    invisible(lapply(s, FUN = function(i) { # Invisible prevents returning NULL.
      par(new = TRUE)
      plot(
        x = data[data$lap == i, x], y = data[data$lap == i, ysmth],
        type = "l", lwd = 1.7, col = base_pal(i, s), xaxs = "i",
        ylim = ylim, xlim = xlim, axes = FALSE, ann = FALSE
      )
    }))
  } else
    plot(
      x = data[, x], y = data[, ysmth], type = "l",
      bty = "l", las = 1, xaxs = "i", lwd = 1.7, ylim = ylim,
      axes = FALSE, ann = FALSE
    )
}
