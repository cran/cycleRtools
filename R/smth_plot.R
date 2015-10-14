#' Smoothed data plot.
#'
#' Create a plot with both raw and smoothed data lines.
#'
#' @param data the dataset to be used.
#' @param x column identifier for the x axis data.
#' @param yraw column identifier for the (underlying) raw data.
#' @param ysmth column identifier for the smoothed data.
#' @param colour level identifier in \code{data} by which to colour lines. Or a
#'   colour name. Or simply a nonexistent column name (equivalent to "black").
#' @param ... further arguments to be passed to \code{plot()}.
#' @param .string are column name arguments given as character strings? A
#'   backdoor around non-standard evaluation. Mainly for internal use.
#'
#' @examples
#' data(cycling_data)
#' # Create some artificial laps.
#' cycling_data$lap <- floor(seq(from = 1, to = 5, along.with = cycling_data[, 1]))
#' # Plot:
#' smth_plot(cycling_data, timer.s, power.W, power.smooth.W, colour = "lap")
#' # Plot with a single blue line:
#' smth_plot(cycling_data, colour = "blue")  # Default arguments.
#'
#' @export
smth_plot <- function(data, x = "timer.s", yraw = "power.W", ysmth = "power.smooth.W",
                      colour = "lap", ..., .string = FALSE) {
  if (!.string) {
    x      <- as.character(substitute(x))
    yraw   <- as.character(substitute(yraw))
    ysmth  <- as.character(substitute(ysmth))
    colour <- as.character(substitute(colour))
  }
  # ----------------------------------------------------------------------------
  # Fundamental plot.
  plot(x = data[, x], y = data[, yraw], type = "l", col = "gray", ...)
  # ----------------------------------------------------------------------------
  # Overlaid smooth line; handling colour argument.
  lwd <- 2
  if (!is.null(colour))
  {

    # Argument is a colour name.
    if (!inherits(try(col2rgb(colour), silent = TRUE), "try-error"))
    {
      lines(x = data[, x], y = data[, ysmth], col = colour, lwd = lwd)
    }
    # Argument is presumably a column name; does it exist?
    else if (!is.null(data[[colour]]))
    {
      s <- unique(data[[colour]])
      for (i in s)
        lines(x = data[data[, colour] == i, x],
              y = data[data[, colour] == i, ysmth],
              col = base_pal(i, s),
              lwd = lwd)
    }
    # Column doesn't exist.
    else
    {
      lines(x = data[, x], y = data[, ysmth], col = colour, lwd = lwd)
    }
  }
  # NULL argument.
  else
  {
    lines(x = data[, x], y = data[, ysmth], col = "black", lwd = lwd)
  }
}
