#' Maximal mean values.
#'
#' Calculate maximal mean values for specified time periods.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param column column in \code{data} giving the values of interest. Needn't be
#'   quoted.
#' @param windows window size(s) for which to generate best averages, given in
#'   seconds.
#' @param delta the incremement with which to initially make the data uniform.
#'   If NULL (default), the most appropriate value is estimated.
#' @param verbose should messages be printed to the console?
#' @param .uniform are the rows of the data already uniform?
#' @param .string are column name arguments given as character strings? A
#'   backdoor around non-standard evaluation. Mainly for internal use.
#'
#' @return a matrix object with two rows: 1) best mean value(s) and 2) the time
#'   at which that value was recorded
#'
#' @examples
#' data(cycling_data)
#' # Generate best powers for 5, 10 and 20 minutes.
#' t_sec <- c(5, 10, 20) * 60
#' x <- mmv(cycling_data, power.W, t_sec)
#' # Show when those values were recorded in minutes
#' x[2, ] / 60
#'
#' @seealso For a more generic and efficient version of this function, see
#'   \code{\link{mmv2}}
#'
#' @export
mmv            <- function(data, column, windows, delta = NULL, verbose = TRUE,
                           .uniform = FALSE, .string = FALSE)
  UseMethod("mmv", data)
#' @export
mmv.default    <- function(data, column, windows, delta = NULL, verbose = TRUE,
                           .uniform = FALSE, .string = FALSE)
  format_error()
#'@export
mmv.cycleRdata <- function(data, column, windows, delta = NULL, verbose = TRUE,
                           .uniform = FALSE, .string = FALSE) {
  if (!.string)
    column <- as.character(substitute(column))

  data   <- data[c("timer.s", column)]
  if (!.uniform)
    data <- uniform(data, delta, verbose, .return_delta = TRUE)
  # Replace NAs with 0s.
  na.rm <- which(is.na(data[, column]))
  data[na.rm, column] <- 0

  mm_fn <- function(x) {
    mvavg <- rollmean_(data[, column], (x / delta))
    best  <- max(mvavg)
    t     <- data[, 1][match(best, mvavg)]
    t     <- t - x
    return(c(best, t))
  }

  max_vals <- sapply(windows, FUN = mm_fn)
  max_vals <- round(max_vals, digits = 2)
  rownames(max_vals) = c("Best mean value", "Recorded @")
  colnames(max_vals) <- paste(windows)
  return(max_vals)
}
