#' Calculate ride time.
#'
#' A simple function for calculating ride time as opposed to elapsed time.
#'
#' @param x a vector of time values.
#' @param delta numeric; the typical interval between time values, if \code{NULL} a
#'   best estimate is used.
#'
#' @return ride time in the same units as x.
#'
#' @examples
#' t_sec <- c(1:20, 50:70)
#' # elapsed time
#' max(t_sec)
#' # ride time.
#' ride_time(t_sec)
#'
#' @export
ride_time <- function(x, delta = NULL) {
  if (!is.null(delta))
    if (is.na(as.numeric(delta)))
      delta <- NULL

    if (is.null(delta))
      delta <- delta_estimate(x)
    #---------------------------------------------------------------------------
    full <- seq(from = 0, to = max(x, na.rm = TRUE), by = delta)
    dt <- c(0, diff(full))
    rt <- sum(dt[full %in% x])
    return(rt)
}
