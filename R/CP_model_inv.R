#' Inverse critical power modelling.
#'
#' Generate critical power parameters via a linearised inverse time model.
#' Mainly useful when the length of inputs is == 2, making
#' \code{\link[stats]{nls}} inappropriate.
#'
#' @param P numeric; maximal mean power values for time periods given in the
#'   tsec argument.
#' @param tsec numeric; time values (seconds) that correspond to elements in P.
#'
#' @return a named vector of parameters (CP and W').
#'
#' @export
CP_model_inv <- function(P, tsec) {
  m <- lm(P ~ {1 / tsec})
  out <- m$coefficients
  names(out) <- c("CP", "W")
  return(out)
}
