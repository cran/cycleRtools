#' Create a uniform data.frame.
#'
#' Returns a data frame with rows that are uniformly incremented along the first
#' column. Useful for rolling operations, whereby a rolling time window is of
#' interest.
#'
#' @param data a data.frame or matrix object to make uniform. This is done on
#'   the basis of the first column.
#' @param delta the incremement by which to make the data uniform. If
#'   \code{NULL} (default) the most appropriate value is estimated.
#' @param verbose should messages be printed to the console?
#' @param .return_delta if TRUE the delta value is assigned to the calling
#'   environment. Intended for internal use.
#'
#' @return A uniform data frame. An index column is appended so that original
#'   data values can be extracted - e.g. after a rolling operation.
#'
#' @seealso source code for \code{\link{rollmean_smth}}
#'
#' @examples
#' t_sec <- c(1:10, 20:40, 50)            # Discontinuous timer values.
#' pwr   <- runif(length(t_sec), 0, 400)  # Some power values.
#' x     <- data.frame(t_sec, pwr)
#' uniform(x)
#'
#' @export
uniform <- function(data, delta = NULL, verbose = TRUE, .return_delta = FALSE) {
  # Get delta value-------------------------------------------------------------
  # Validate input.
  if (!is.null(delta))
    if (is.na(as.numeric(delta)))
      delta <- NULL
    if (is.null(delta))
      delta <- delta_estimate(data[, 1])
    if (verbose)
      message(paste("Creating uniform data using", names(data)[1],
                    ", delta value =", delta))
    # Index column to later recall original data--------------------------------
    data$index <- 1
    # Assemble full data with NAs to merge--------------------------------------
    col1.full <- seq(
      from = min(data[, 1], na.rm = TRUE),
      to = max(data[, 1], na.rm = TRUE),
      by = delta
    )
    data.empty <- as.data.frame(
      sapply(
        seq(from = 1, to = ncol(data)),
        FUN = function(x) rep(NA, times = length(col1.full))
      )
    )
    colnames(data.empty) <- colnames(data)
    data.empty[, 1] <- col1.full
    # Merge dataframes----------------------------------------------------------
    data.full <- merge(
      data,
      data.empty,
      by = colnames(data)[1],
      all = TRUE
    )
    # Remove "y" columns (i.e. those from the empty dataframe)------------------
    data.full <-
      data.full[, c(1, grep("\\.x$", colnames(data.full)))]
    colnames(data.full) <- colnames(data)
    if (.return_delta) assign("delta", delta, pos = parent.frame())
    return(data.full)
}
