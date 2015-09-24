#' Smooth a column of a dataset.
#'
#' Smooth data with a right-aligned (zero-padded) rolling average. This is
#' intended for time-based smoothing - e.g. a 30-second moving average. Hence,
#' as rolling operations are performed row-wise in in the interest of
#' efficiency, the data must first be made time-uniform by row. Hence, this is
#' done via \code{\link{uniform}} by the first column of the data; thus allowing
#' the \code{column} to be smoothed.
#'
#' @param data a data.frame or matrix object to use for smoothing. The data is
#'   made \code{\link{uniform}} on the basis of the first column.
#' @param column the column name of the data to be smoothed, needn't be quoted.
#' @param smth.pd numeric; the time period over which to smooth (seconds).
#' @param ema should the moving average be exponentially weighted?
#' @param delta the incremement with which to initially make the data uniform.
#'   If NULL (default) a best estimate is used.
#' @param verbose should messages be printed to the console?
#' @param .uniform are the rows of the data already uniform?
#'
#' @return a vector of the same length as the \code{column}.
#'
#' @examples
#' \dontrun{
#' data(cycling_data)
#' # Smooth power data with a 30 second moving average.
#' rollmean_smth(cycling_data, power.W, 30)
#' # Or alternatively, use an exponentially weighted moving average.
#' rollmean_smth(cycling_data, power.W, 30, ema = TRUE)
#' }
#' @export
rollmean_smth <- function(data, column, smth.pd, ema = FALSE, delta = NULL,
                          verbose = TRUE, .uniform = FALSE) {
  column <- as.character(substitute(column))
  # Isolate relevant data.
  col1 <- names(data)[[1]]
  data <- data[c(col1, column)]
  # Make data uniform if required.
  if (!.uniform)
    data <- uniform(data, delta, verbose, .return_delta = TRUE)
  if (is.null(delta))
    delta <- delta_estimate(data[[col1]])
  if (verbose)
    message(paste0("Smoothing ", column, " using ", col1, " (", smth.pd,
                   ifelse(ema, "; ema = TRUE)...", ")...")))
  # Replace NAs with 0s.
  na.rm <- which(is.na(data[, column]))
  data[na.rm, column] <- 0

  if (ema)
    smth.data <- rollmean_ema_(data[, column], {smth.pd / delta},
                              ema_weights({smth.pd / delta}))
  else
    smth.data <- rollmean_(data[, column], {smth.pd / delta})
  # Strip NAs added by uniform().
  if (!.uniform)
    if (any(is.na(unique(data$index))))
      smth.data <- smth.data[-c(which(is.na(data$index)))]
  return(smth.data)
}
