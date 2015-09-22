#' Generate reliable elevation data.
#'
#' Using the latitude and longitude columns of the supplied \emph{formatted}
#' data, a vector of elevation values is returned of the same length. If no
#' elevation data files exist within the specified directory, files are first
#' downloaded. Note that NAs in the data will return corresponding NAs in the
#' corrected elevation.
#'
#' @param data a dataset with longitude ("lng") and lattitude ("lat") columns.
#' @param country character string; the country to which the data pertain, given
#'   as an ISO3 code (see \code{raster::getData("ISO3")})
#' @param dir the directory in which the necessary elevation data is contained
#'   or should be downloaded. "default" creates a hidden directory
#'   (.elevation_data) in the home directory.
#'
#' @return a vector of elevation values. If there is an error at any stage, a
#'   vector of NAs is returned.
#'
#' @examples
#' data(cycling_data)
#' cycling_data$elevation.m <- elevation_correct(cycling_data, "GBR")
#'
#' @seealso \code{\link{download_elev_data}}.
#'
#' @export
elevation_correct <- function(data, country, dir = "default") {
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Raster package not installed.", call. = FALSE)
  position_data <-  # Throw a format error early.
    data.frame(lng = data$lng, lat = data$lat)
  initial_dir <- getwd()
  if (dir == "default") {
    if (!dir.exists("~/.elevation_data")) dir.create("~/.elevation_data")
    setwd("~/.elevation_data")
  } else {
    if (!dir.exists(dir)) dir.create(dir)
    setwd(dir)
  }
  exit <- 0
  if (length(list.files(pattern = country)) == 0) {
    if (file.exists("elevation_data.zip")) {
      if(any(grepl(
        paste0(country, "*"), unzip("elevation_data.zip", list = TRUE)[, 1]
      ))) {
        message("Unzipping saved elevation data...")
        unzip(
          zipfile = "elevation_data.zip",
          files = grep(paste0(country, "*"),
                       unzip("elevation_data.zip", list = TRUE)[, 1],
                       value = TRUE)
        )
      } else {
        message("Invalid country argument or no data available.")
        exit <- 1
      }
    } else download_elev_data(country) -> exit
  }
  if (as.logical(exit)) {
    message("Error.")
    setwd(initial_dir)
    return(rep(NA, times = dim(data)[[1]]))
  }
  e <- raster::getData("alt", country = country, download = FALSE)
  # Extract interpolated elevation data from raster object.
  e <- raster::extract(e, position_data, method = "bilinear")
  setwd(initial_dir)
  return(e)
}
