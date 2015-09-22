#' Power-Based TRaining IMPulse.
#'
#' Calculate a \emph{normalised} TRIMP value using power data. This is a
#' power-based adaptation of Bannister's TRIMP, whereby critical power (CP) is
#' assumed to represent 90% of heart rate ratio (HRR). The final TRIMP score is
#' normalised to the score associated with one-hour's riding at CP, to aid
#' interpretation.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param CP a critical power value (watts).
#'
#' @return a normalised TRIMP score.
#'
#' @references Morton, R.H., Fitz-Clarke, J.R., Banister, E.W., 1990. Modeling
#'   human performance in running. Journal of Applied Physiology 69, 1171-1177.
#'
#' @export
pwr_TRIMP <- function(data, CP) UseMethod("pwr_TRIMP", data)
#' @export
pwr_TRIMP.default <- function(data, CP)
  format_error()
#' @export
pwr_TRIMP.cycleRdata <- function(data, CP) {
  pwr <- mean(data$power.smooth.W ^ 4, na.rm = TRUE) ^ (1 / 4)
  tmin <- ride_time(data$timer.s) / 60
  int <- {pwr / CP}
  ((tmin * 0.9 * int * exp(1.92 * 0.9 * int)) /
    (60 * 0.9 * exp(1.92 * 0.9))) * 100
}
