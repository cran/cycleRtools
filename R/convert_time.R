#' Reformat time.
#'
#' Functions perform interconversion between "HH:MM:SS" format and seconds.
#'
#' @param x either a character string of the form "HH:MM:SS" ("HH" is optional)
#'   or a seconds value; can accept vectors.
#'
#' @return seconds value(s) for \emph{from}, and "HH:MM:SS" character string(s)
#'   for \emph{to}.
#'
#' @name convert_time
NULL
# ------------------------------------------------------------------------------
#' @rdname convert_time
#' @export
convert_from_time <- function(x) {
  ox  <- x             # Save original x input.
  x   <- x[!is.na(x)]  # Remove NAs.

  x   <- suppressWarnings(strsplit(x, ":"))
  x   <- lapply(x, rev)
  x   <- lapply(x, as.numeric)
  x   <- lapply(x, function(i) i * c(1, 60, 60 ^ 2)[1:length(i)])
  out <- unlist(lapply(x, sum))

  ox[!is.na(ox)] <- out
  return(as.numeric(ox))
}
#' @rdname convert_time
#' @export
convert_to_time <- function(x) {
  ox <- x             # Save original x input.
  x  <- x[!is.na(x)]  # Remove NAs.

  h <- rep(0, length.out = length(x))
  m <- floor(x / 60)
  s <- round(x %% 60)

  h[m > 60] <- floor(m[m > 60] / 60)
  m[m > 60] <- m[m > 60] - (60 * h[m > 60])

  out <- sprintf("%02d:%02d:%02d", h, m, s)
  out <- gsub("^00:", "", out)
  out <- gsub("^0", "", out)

  ox[!is.na(ox)] <- out
  return(ox)
}
