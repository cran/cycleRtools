#' Reformat time.
#'
#' Convert time from "HH:MM:SS" format to seconds.
#'
#' @param x character string; a time in (HH:)MM:SS format. HH is optional.
#'
#' @return a numeric time value in seconds.
#'
#' @examples
#' convert_time("3:21:05")       # Simple example.
#' convert_time("03:30:10.5")    # With decimal seconds.
#' convert_time("19:30.1")       # Without hours.
#' convert_time("19:30.1") / 60  # In minutes.
#'
#' @export
convert_time <- function(x) {
  err <- "Argument should be a character string of the format 'HH:MM:SS'."
  if (!is.character(x)) stop(err, call. = FALSE)
  x <- suppressWarnings(as.numeric(unlist(strsplit(x, ":"))))
  if (any(is.na(x))) stop(err, call. = FALSE)
  x <- rev(x)
  x[-1] <- x[-1] * 60
  if (length(x) == 3) x[3] <- x[3] * 60
  out <- sum(x)
  return(out)
}
