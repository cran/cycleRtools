#' W' balance.
#'
#' Generate a vector of W' balance values from time and power data. The
#' underlying algorithm is published in Skiba \emph{et al.} (2012).
#'
#' The algorithm used here, while based on Dr Phil Skiba's model, differs in
#' that values are positive as opposed to negative. The original published model
#' expressed W' balance as W' minus W' expended, the latter recovering with an
#' exponential time course when P < CP. An issue with this approach is that an
#' athlete might be seen to go into negative W' balance. Hence, to avoid
#' assumptions regarding available W', this algorithm returns W' expended (and
#' its recovery) as positive values; i.e. a ride is begun at 0 W' expended, and
#' it will \emph{increase} in response to supra-CP efforts.
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
