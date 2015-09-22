#' Training stress score.
#'
#' Calculate a Training Stress Score (TSS). TSS is a registered trademark of
#' Peaksware Inc.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param threshold a threshold value - e.g. FTP.
#' @param verbose should messages be printed to the console?
#'
#' @return a TSS score.
#'
#' @export
TSS <- function(data, threshold, verbose = TRUE)
  UseMethod("TSS", data)
#' @export
TSS.default <- function(data, threshold, verbose = TRUE)
  format_error()
#' @export
TSS.cycleRdata <- function(data, threshold, verbose = TRUE) {
  timeTotal <- ride_time(data$timer.s)
  NormPwr <- NP(data, verbose = verbose)
  message(paste("Normalised power =", round(NormPwr, digits = 2)))
  ((timeTotal * NormPwr * (NormPwr / threshold)) /
    (threshold * (60 ^ 2))) * 100
}
