#' Download geographical elevation data.
#'
#' Downloads elevation data files to the working directory for use with
#' \code{\link{elevation_correct}}. Requires package \code{raster} to be
#' installed.
#'
#' @param country character string; the ISO3 country code (see
#'   \code{raster::getData("ISO3")}) for which to download the data. If "all",
#'   then all available data is downloaded - this may take some time.
#'
#' @return nothing.
#'
#' @examples
#' # Download elevation data for the UK into the home directory.
#' \dontrun{
#' setwd("~")
#' download_elev_data("GBR")
#' }
#'
#' @seealso \code{\link{elevation_correct}}.
#'
#' @export
download_elev_data <- function(country = "all") {
  if (country == "all") {
    response <-
      readline(paste("This will download ALL available elevation data, and will take some time.",
                     "Continue [Y/N]? "))
    if (!grepl(pattern = "y", response, ignore.case = TRUE))
      stop("Download aborted.", call. = FALSE)

    mx <- raster::getData("ISO3")
    for (i in mx[, 1]) {
      message(rep("-", times = 50))
      message(paste(mx[match(i, mx), 2]))
      message(rep("=", times = 50))
      tryCatch(
        raster::getData(
          "alt", country = i, download = TRUE, mask = TRUE
        ),
        error = function(e)
          message(paste(i, "not available")),
        finally = message("done")
      )
    }
  } else {
    if (country %in% raster::getData("ISO3")[, 1]) {
      message(paste("Downloading elevation data for", country, "to", getwd()))
      test <-
        try(suppressWarnings(raster::getData(
          "alt", country = country, download = TRUE, mask = TRUE
        )))
    } else {
      message("Invalid country argument or no data available.")
      return(1)
    }
    if (inherits(test, "try-error")) {
      message("No internet connection.")
      return(1)
    } else
      return(0)
  }
}
