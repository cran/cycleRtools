#' Normalised power.
#'
#' Calculate a Normalised Power value. Normalised Power is a registered
#' trademark of Peaksware Inc.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param delta optional; the sampling frequency of data (in seconds per
#'   sample).
#' @param verbose should messages be printed to the console?
#'
#' @return a Normalised Power value.
#'
#' @examples
#' data(cycling_data)
#' NP(cycling_data)
#'
#' @export
NP <- function(data, delta = NULL, verbose = TRUE)
  UseMethod("NP", data)
#' @export
NP.default <- function(data, delta = NULL, verbose = TRUE)
  format_error()
#' @export
NP.cycleRdata <- function(data, delta = NULL, verbose = TRUE) {
  mean(rollmean_smth(data, "power.W", 30, ema = FALSE, delta, verbose) ^ 4) ^ (1 / 4)
}
