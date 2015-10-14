#' Zone-time distribution plot.
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
#' @param .string are column name arguments given as character strings? A
#'   backdoor around non-standard evaluation. Mainly for internal use.
#' @param ... arguments to be passed to \code{barplot()} and/or graphical
#'   parameters.
#'
#' @examples
#' data(cycling_data)
#' zdist_plot(
#'  data = cycling_data,
#'  column = power.W,
#'  binwidth = 10,
#'  zbounds = c(100, 200, 300),
#'  xlim = c(110, 500)
#' )
#'
#' @return nothing; a plot is sent to the current graphics device.
#'
#' @export
zdist_plot            <- function(data, column = "power.W", binwidth = 10, zbounds = NULL,
                                  .string = FALSE, ...)
  UseMethod("zdist_plot", data)
#' @export
zdist_plot.default    <- function(data, column = "power.W", binwidth = 10, zbounds = NULL,
                                  .string = FALSE, ...)
  format_error()
#' @export
zdist_plot.cycleRdata <- function(data, column = "power.W", binwidth = 10, zbounds = NULL,
                                  .string = FALSE, ...) {
  if (!.string)
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
  vals[, 2]      <- as.numeric(rownames(vals))
  colnames(vals) <- c("t_pct", column)
  # Plot -----------------------------------------------------------------------
  opar          <- par(no.readonly = TRUE)
  args          <- list(...)
  args_par      <- args[which(names(args) %in% names(par()))]
  par(args_par)
  # If a zone argument is given.
  if (!is.null(zbounds)) {
    vals$zone    <- factor(zone_index(vals[, column], zbounds))
    # Barplot arguments.
    args_default <- list(height    = vals$t_pct,
                         width     = binwidth,
                         names.arg = names(vals$t_pct),
                         xpd       = FALSE,
                         col       = base_pal(vals$zone, unique(vals$zone)),
                         ylab      = "Time (%)")
    args_usr      <- args[which(names(args) %in% names(formals(barplot.default)))]
    args_barplot  <- c(args_usr,
                       args_default[!names(args_default) %in% names(args_usr)])
    # Plot.
    do.call(barplot, args_barplot)
    # Annotate.
    z_pct     <- zone_time(data_orig, column, zbounds, TRUE, .string = TRUE)
    z_labs    <- paste0(round(z_pct), "%")
    legend("topright",
           legend    = paste0("Z", unique(vals$zone), ": ", z_labs),
           bty       = "n",
           fill      = base_pal(z_labs, z_labs),
           y.intersp = 1.2)
  } else {  # If no zone argument is given.
    warning("No zone bounds given.", call. = FALSE)
    # Barplot arguments.
    args_default <- list(height    = vals$t_pct,
                         width     = binwidth,
                         names.arg = names(vals$t_pct),
                         xpd       = FALSE,
                         ylab      = "Time (%)")
    args_usr      <- args[which(names(args) %in% names(formals(barplot.default)))]
    args_barplot  <- c(args_usr,
                       args_default[!names(args_default) %in% names(args_usr)])
    # Plot.
    do.call(barplot, args_barplot)
  }
  par(opar)            # Reset graphics device.
  return(invisible())  # Return nothing.
}
