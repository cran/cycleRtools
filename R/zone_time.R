#' Calculate time in zones.
#'
#' Given a vector of zone boundaries, sums the time spent in each zone.
#'
#' @param data a \strong{formatted} dataset produced by \code{read*()}.
#' @param column the column name of the data to which the zone boundaries
#'   relate.
#' @param zbounds numeric; zone boundaries.
#' @param pct should percentage values be returned?
#' @param .string are column name arguments given as character strings? A
#'   backdoor around non-standard evaluation. Mainly for internal use.
#'
#' @return a data frame of zone times.
#'
#' @export
zone_time <- function(data, column = "power.W", zbounds, pct = FALSE, .string = FALSE)
  UseMethod("zone_time", data)
#' @export
zone_time.default <- function(data, column = "power.W", zbounds, pct = FALSE, .string = FALSE)
  format_error()
#' @export
zone_time.cycleRdata <- function(data, column = "power.W", zbounds, pct = FALSE,
                                 .string = FALSE) {
  if (!.string)
    column <- as.character(substitute(column))
  data   <- data[data$delta.t <= 20, ]           # Remove sig. stops.
  data$z <- zone_index(data[, column], zbounds)  # Rcpp.
  out    <- tapply(
    data$delta.t, data$z, FUN = function(x) sum(x, na.rm = TRUE)
  )
  # Convert to percentage?
  if (pct) {
    out <- {out / sum(out, na.rm = TRUE)} * 100
    out <- round(out)
  }
  if (length(out) == 1)
    names(out) <- "Zone 1"
  else
    names(out) <- paste("Zone", 1:(length(zbounds) + 1))
  return(out)
}
