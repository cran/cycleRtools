#' W' balance.
#'
#' Generate a vector of W' balance values from time and power data. The
#' underlying algorithm is published in Skiba \emph{et al.} (2012), and can also
#' be examined in the function source code.
#'
#' @param data a data.frame/matrix object with time and power columns.
#' @param time character; name of the time (seconds) column in \code{data}.
#' @param pwr character; name of the power (watts) column in \code{data}.
#' @param CP a critical power value for use in the calculation.
#' @param .string are column name arguments given as character strings? A
#'   backdoor around non-standard evaluation. Mainly for internal use.
#'
#' @return A numeric vector of W' balance values, in \strong{kilojoules}.
#'
#' @examples
#' data(cycling_data)
#' cycling_data$Wexp.kJ <-
#'   Wbal(cycling_data, timer.s, power.smooth.W, CP = 300)
#'
#' @seealso \code{\link{Wbal_plots}}.
#'
#' @references Skiba, P. F., W. Chidnok, A. Vanhatalo, and A. M. Jones. Modeling
#'   the Expenditure and Reconstitution of Work Capacity above Critical Power.
#'   Med. Sci. Sports Exerc., Vol. 44, No. 8, pp. 1526-1532, 2012.
#'   \href{http://www.ncbi.nlm.nih.gov/pubmed/22382171}{PubMed link}.
#'
#' @export
Wbal <- function(data, time = "timer.s", pwr = "power.W", CP, .string = FALSE) {
  if (!.string) {
    time  <- data[, as.character(substitute(time))]
    pwr   <- data[, as.character(substitute(pwr))]
  }

  out <- Wbal_(time, pwr, CP)  # Rcpp.
  out <- out / 1000            # J to kJ.
  return(out)
}
