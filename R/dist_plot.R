#' Time distribution plot.
#'
#' Display the time distribution of values within a dataset. The distribution
#' can also be partitioned into zones if the \code{zbounds} argument is not
#' \code{NULL}.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param column column in \code{data} giving the values of interest. Needn't be
#'   quoted.
#' @param binwidth how should values in \code{column} be binned? E.g.
#'   \code{bindiwdth = 10} will create 10 watt bins if \code{column} is power
#'   data.
#' @param zbounds optional; a numeric vector of zone boundaries.
#' @param xlab x axis label.
#' @param xlim numeric vector of length 2; x axis limits.
#' @param ylim numeric vector of length 2; y axis limits.
#'
#' @examples
#' data(cycling_data)
#' dist_plot(
#'  data = cycling_data,
#'  column = power.W,
#'  binwidth = 10,
#'  zbounds = c(100, 200, 300),
#'  xlim = c(110, 500)
#' )
#'
#' @return a ggplot object.
#'
#' @export
dist_plot <- function(data, column = "power.W", binwidth = 10, zbounds = NULL,
                      xlab = NULL, xlim = NULL, ylim = NULL)
  UseMethod("dist_plot", data)
#' @export
dist_plot.default <- function(data, column = "power.W", binwidth = 10, zbounds = NULL,
                              xlab = NULL, xlim = NULL, ylim = NULL)
  format_error()
#' @export
dist_plot.cycleRdata <- function(data, column = "power.W", binwidth = 10, zbounds = NULL,
                                 xlab = NULL, xlim = NULL, ylim = NULL) {
  column    <- as.character(substitute(column))
  data_orig <- data                        # Needed later on.
  data      <- data[data$delta.t <= 20, ]  # Remove sig. stops.
  # Convert column of interest to bins.
  data[, column] <- floor(data[, column] / binwidth) * binwidth
  # Calculate percentage time in zbounds.
  t_total <- sum(data$delta.t, na.rm = TRUE)
  vals    <- as.data.frame(tapply(
    data$delta.t, data[, column], FUN = function(x)
    {sum(x, na.rm = TRUE) / t_total} * 100
  ))
  vals[, 2] <- as.numeric(rownames(vals))
  colnames(vals) <- c("t_pct", column)
  # If a zone argument is given...
  if (!is.null(zbounds)) {
    # y position for labels
    if (is.null(ylim))
      lab_ypos <- max(vals$t_pct, na.rm = TRUE) * 1.1
    else
      lab_ypos <- ylim[[2]] * 0.9  # Top of the plot.

    vals$Zone <- factor(zone_index(vals[, column], zbounds))
    # Assemble data frame for plot labels.
    z_min     <- tapply(vals$power.W, vals$Zone,
                        FUN = function(x) min(x, na.rm = TRUE))
    z_pct     <- zone_time(data_orig, column, zbounds, TRUE)
    z_labs    <- data.frame(z_min, z_pct = paste0(round(z_pct), "%"),
                            Zone = unique(vals$Zone), ypos = lab_ypos)
    # Plot
    p <-
      ggplot2::ggplot(vals, ggplot2::aes_string(x = column, y = "t_pct"),
                      environment = environment()) +
      ggplot2::geom_bar(stat = "identity", ggplot2::aes_string(fill = "Zone")) +
      ggplot2::geom_text(data = z_labs, ggplot2::aes_string(
        label = "z_pct", x = "z_min", y = "lab_ypos", colour = "Zone"),
        hjust = -0.5, vjust = 1)
  } else {  # If no zone argument is given...
    p <- ggplot2::ggplot(vals, ggplot2::aes_string(x = column, y = "t_pct")) +
      ggplot2::geom_bar(stat = "identity")
  }
  # Format etc.
  p <- p + ggplot2::labs(x = xlab, y = "Time (%)") +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  return(p)
}
