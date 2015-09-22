#' Power-time modelling.
#'
#' Model the Power-time (Pt) relationship for a set of data. This is done via
#' nonlinear least squares regression of four models: an inverse model; an
#' exponential model; a bivariate power function model; and a three parameter
#' inverse model. An S3 object of class "Ptmodels" is returned, which currently
#' has methods for \link[base]{print}, \link[stats]{coef}, \link[base]{summary},
#' and \link[stats]{predict}. If inputs do not conform well to the models, a
#' warning message is generated. This function can make use of
#' \code{minpack.lm::nlsLM}.
#'
#' @param P a numeric vector of maximal mean power values for time periods given
#'   in the \code{tsec} argument.
#' @param tsec a numeric vector of time values that (positionally) correspond to
#'   elements in \code{P}.
#'
#' @return returns an S3 object of class "Ptmodels".
#'
#' @references R. Hugh Morton (1996) A 3-parameter critical power model,
#'   Ergonomics, 39:4, 611-619,
#'   \href{http://dx.doi.org/10.1080/00140139608964484}{DOI}.
#'
#' @examples
#' data(Pt_prof)
#' P    <- unname(Pt_prof)
#' tsec <- as.numeric(names(Pt_prof))
#' # Generate model object
#' m <- Pt_model(P, tsec)
#' # View
#' print(m)
#' # Plot
#' plot(P ~ tsec, cex = 0.2)
#' with(m$Pfn, curve(inv(x), add = TRUE, col = "red"))
#' with(m$Pfn, curve(exp(x), add = TRUE, col = "blue"))
#' with(m$Pfn, curve(pwr(x), add = TRUE, col = "purple"))
#' with(m$Pfn, curve(thrp(x), add = TRUE, col = "green"))
#' legend("topright", legend = round(m$table$RSE, 2),
#'        text.col = c("red", "blue", "purple", "green"),
#'        title = "RSE",title.col = "black",
#'        bty = "n")
#' @export
Pt_model <- function(P, tsec) {
  if (length(P) != length(tsec))
    stop("Mismatching values.", call. = FALSE)
  #  ---------------------------------------------------------------------------
  if (length(P) == 2) {
    m <- CP_model_inv(P, tsec)
    params <- data.frame(CP = m["CP"], "W'" = m["W"], row.names = "Inverse model")
    return(params)
  }
  #  ---------------------------------------------------------------------------
  models <- Pt_nls(requireNamespace("minpack.lm", quietly = TRUE), P, tsec)
  # Assemble S3 object ---------------------------------------------------------
  out <- list(
    models = models,
    table  = Pt_table(models),
    Pfn    = Pt_fn(models, "P"),
    tfn    = Pt_fn(models, "tsec")
  )
  class(out) <- "Ptmodels"
  #-----------------------------------------------------------------------------
  return(out)
}
