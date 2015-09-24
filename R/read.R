#' Read cycling device data.
#'
#' Read data from a cycling head unit into the R environment; optionally
#' formatting it for use with other functions in this package.
#'
#' Returns a data frame with all data parsed from the file; with an additional
#' lap (factor) column appended in the case of \code{read_fit()}.
#'
#' Note that most functions within this package depend on imported data being
#' formatted; i.e. \code{read*("file_path", format = TRUE)}. Hence, unless the
#' raw data is of particular interest and/or the user wants to process it
#' manually, the format argument should be TRUE (default). When working with a
#' formatted dataset, do not change existing column names. The formatted data
#' structure is described in detail in \link{cycling_data}.
#'
#' Garmin .fit file data is parsed with the java command line tool provided in
#' the \href{http://www.thisisant.com/resources/fit}{FIT SDK}. The latest source
#' code and licensing information can be found at the previous link.
#'
#' SRM device files (.srm) are also parsed at the command line, provided
#' \href{http://www.zuto.de/project/srmio/}{Rainer Clasen's srmio library} is
#' installed and available. The associated GitHub repo' can be found
#' \href{https://github.com/rclasen/srmio}{here}.
#'
#' @param file character; path to the file.
#' @param format logical; should data be formatted?
#' @param .list should data be returned as a list? Useful when data columns are
#'   of unequal length, as can sometimes be the case with \code{read_pwx} and
#'   \code{read_tcx}.
#'
#' @return A data frame or list, according to the \code{.list} argument.
#'
#' @describeIn read A wrapper for read_* functions that chooses the appropriate
#'   function based on file extension.
#'
#' @export
#'
#' @useDynLib cycleRtools
#' @importFrom Rcpp sourceCpp
#' @import stats
#' @import graphics
#' @import grDevices
#' @import utils
read <- function(file, format = TRUE, .list = FALSE) {
  if (grepl("\\.fit$", file))
    data <- read_fit(file, format)
  else if (grepl("\\.srm$", file))
    data <- read_srm(file, format)
  else if (grepl("\\.pwx$", file))
  {
    if (system2("xml2 test", stdout = FALSE, stderr = FALSE) != 127)
      data <- read_pwx2(file, format, .list)
    else
      data <- read_pwx(file, format, .list)
  }
  else if (grepl("\\.tcx$", file))
    data <- read_tcx2(file, format, .list)
  else
  {
    message("Unrecognised file extension, returning NULL.")
    data <- NULL
  }
  return(data)
}
