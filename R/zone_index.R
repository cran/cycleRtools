#' Index zones.
#'
#' Generate a vector of zone "levels" from an input vector and defined
#' boundaries.
#'
#' @param x numeric; values to be "zoned".
#' @param zbounds numeric; values for zone boundaries.
#'
#' @examples
#' data(cycling_data)
#' cycling_data$zone <- zone_index(cycling_data$power.W, c(100, 200, 300))
#'
#' @return a numeric vector of zone values of the same length as \code{x}. The
#'   number of zone levels will be \code{length(zbounds) + 1}.
#'
#' @export
zone_index <- function(x, zbounds) {
  zbounds <- sort(unique(zbounds))    # Important!
  out     <- zone_index_(x, zbounds)  # Rcpp.
  return(out)
}
