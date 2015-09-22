# Functions for creating S3 object "Ptmodels".
# ------------------------------------------------------------------------------
# Generate nls model objects.
Pt_nls <- function(LM = FALSE, P, tsec) {
  if (LM)  # Is minpack.lm installed?
    list(
      inv = minpack.lm::nlsLM(
        P ~ {(W / tsec) + CP},
        start = list(W  = 20000, CP = 300),
        upper =    c(W  = 60000, CP = 600),
        lower =    c(W  = 0,     CP = 0)
      ),
      exp = minpack.lm::nlsLM(
        P ~ {a * exp(1) ^ (k * tsec) + CP},
        start = list(a = diff(range(P)), k = -0.005, CP = min(P)),
        upper = c(a = 1000, k =  0, CP = 1000),
        lower = c(a = 0,    k = -1, CP = 0)
      ),
      pwr = minpack.lm::nlsLM(
        P ~ {k * (tsec ^ n) + A},
        start = list(k = 1500, n = -0.5, A = 0)
      ),
      thrp = minpack.lm::nlsLM(
        P ~ {(W / (tsec - k)) + CP},
        start = list(W  = 20000, CP = 300, k = -20),
        upper =    c(W  = Inf,   CP = 600, k = 0),
        lower =    c(W  = 0,     CP = 0,   k = -1000)
      )
    )
  else
    list(
      inv = nls(
        P ~ {(W / tsec) + CP},
        start = list(W  = 20000, CP = 300),
        control = list(maxiter = 1000)
      ),
      exp = nls(
        P ~ {a * exp(1) ^ (k * tsec) + CP},
        start = list(a = diff(range(P)), k = -0.005, CP = min(P)),
        control = list(maxiter = 1000)
      ),
      pwr = minpack.lm::nlsLM(
        P ~ {k * (tsec ^ n) + A},
        start = list(k = 1500, n = -0.5, A = 0),
        control = list(maxiter = 1000)
      ),
      thrp = nls(
        P ~ {(W / (tsec - k)) + CP},
        start = list(W  = 20000, CP = 300, k = -20),
        control = list(maxiter = 1000)
      )
    )
}

# Generate table from model objects.
Pt_table <- function(m) {
  coeff <- function(i) unname(round(coef(m[[i]]), 3))

  f   <- c(
    paste(coeff("inv"), collapse = " / x + "),
    paste0(coeff("exp")[1], " * e^ ", coeff("exp")[2], "x", " + ", coeff("exp")[3]),
    paste0(coeff("pwr")[[1]], " * x^", coeff("pwr")[[2]],
           ifelse(coeff("pwr")[[3]] < 0, " - ", " + "),
           abs(coeff("pwr")[[3]])),
    paste0(coeff("thrp")[1], " / (x",
           ifelse(coeff("thrp")[3] < 0, " + ", " - " ),
           abs(coeff("thrp")[3]), ") + ", coeff("thrp")[2])
  )

  RSE <- sapply(m, function(x) summary(x)$sigma)
  tab <- data.frame(formula = f, RSE  = RSE, fit  = NA)
  # Annotate data frame.
  bestfitrow <- which.min(tab$RSE)  # Residual Standard Error.
  tab$fit[bestfitrow] <- "**"
  tab$fit[is.na(tab$fit)] <- " "
  rownames(tab) <- c("Inverse", "Exponential", "Power", "Three-param")
  # Warning message if models go haywire.
  CP  <- c(coef(m$inv)[[2]], coef(m$exp)[[3]], coef(m$thrp)[[2]])
  if (any(CP < 0))
    warning("Inappropriate data for these models, revise inputs.", call. = FALSE)
  return(tab)
}

# Prediction functions
Pt_fn <- function(m, y = "P") {
  if (y == "P")
    out <- list(
      inv = function(x)
        unname({coef(m$inv)["W"] / x + coef(m$inv)["CP"]}),

      exp = function(x)
        unname({coef(m$exp)["a"] * exp(1) ^ (coef(m$exp)["k"] * x) + coef(m$exp)["CP"]}),

      pwr = function(x)
        unname({coef(m$pwr)["k"] * (x ^ coef(m$pwr)["n"]) + coef(m$pwr)["A"]}),

      thrp = function(x)
        unname({(coef(m$thrp)["W"] / (x - coef(m$thrp)["k"])) + coef(m$thrp)["CP"]})
    )
  else if (y == "tsec")
    out <- list(
      inv = function(x)
        unname({coef(m$inv)["W"] / (x - coef(m$inv)["CP"])}),

      exp = function(x)
        unname({(1 / coef(m$exp)["k"]) * log((x - coef(m$exp)["CP"]) / coef(m$exp)["a"])}),

      pwr = function(x)
        unname({((x - coef(m$pwr)["A"]) / coef(m$pwr)["k"]) ^ (1 / coef(m$pwr)["n"])}),

      thrp = function(x)
        unname({coef(m$thrp)["W"] / (x - coef(m$thrp)["CP"]) + coef(m$thrp)["k"]})
    )
  return(out)
}

# S3 Methods
# ------------------------------------------------------------------------------
#' @export
print.Ptmodels <- function(x, ...) {
  print(roundf(x$table), digits = 2)
}
#' @export
coef.Ptmodels <- function(object, ...) {
  lapply(object$models, coef)
}
#' @export
summary.Ptmodels <- function(object, ...) {
  lapply(object$models, summary)
}
#' Predict Power or Time
#'
#' Given a Ptmodels \code{object}, the predict.Ptmodels will produce a named
#' numeric vector of either time (seconds) or power (watts) values according to
#' the \code{x} and \code{y} arguments
#'
#' @param object an object of class "Ptmodels".
#' @param x the value for which to make a prediction, see below.
#' @param y the type of variable to predict: "P" will produce a power
#'   prediction, and hence assume \code{x} is a time value; and "tsec" vice
#'   versa.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a named numeric vector of predicted values. Names correspond to their
#'   respective models.
#'
#' @export
predict.Ptmodels <- function(object, x, y = "P", ...) {
  y <- switch(y,
              "P" = "P", "power" = "P", "pwr" = "P",
              "tsec" = "tsec",
              "P"  # Default.
  )
  if (y == "P")
    out <- suppressWarnings(sapply(object$Pfn, function(fn) fn(x)))
  else
  {
    out <- suppressWarnings(sapply(object$tfn, function(fn) fn(x)))
    out[out < 0 | is.nan(out)] <- Inf
  }
  return(out)
}
