#' Reset a dataset or vector.
#'
#' Subtracts the first element from all other elements.
#'
#' @details if x is a formatted dataset from a \code{read*} function, all the
#'   columns are reset as appropriate. This can be useful after subsetting a
#'   ride dataset, for example. Otherwise, this is a simple shortcut for \code{x
#'   - x[[1]]}.
#'
#' @param x a numeric vector or formatted cycling dataset (i.e. class \code{"cycleRdata"}).
#'
#' @return either a data frame or vector, depending on the class of \code{x}.
#'
#' @examples
#' data(cycling_data)
#' # Remove first minute of data and reset.
#' data_raw <- cycling_data[cycling_data$timer.s > 60, ]
#' data_reset <- reset(data_raw)
#' # Compare...
#' data_raw$distance.km[[1]]
#' data_reset$distance.km[[1]]
#'
#' @export
reset <- function(x) UseMethod("reset", x)
#' @export
reset.default <- function(x) {
  x <- x - x[[1]]
  return(x)
}
#' @export
reset.cycleRdata <- function(x) {
  x$timer.s <- x$timer.s - x$timer.s[[1]]
  x$timer.min <- x$timer.min - x$timer.min[[1]]
  x$distance.km <- x$distance.km - x$distance.km[[1]]
  x$work.J <- x$work.J - x$work.J[[1]]
  x$delta.t <- c(0, diff(x$timer.s))
  return(x)
}
