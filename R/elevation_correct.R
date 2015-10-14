#' Generate reliable elevation data.
#'
#' Using the latitude and longitude columns of the supplied \emph{formatted}
#' data, a vector of elevation values is returned of the same length. If no
#' elevation data files exist within the working directory, files are first
#' downloaded. Note that NAs in the data will return corresponding NAs in the
#' corrected elevation.
#'
#' @param data a dataset with longitude ("lng") and lattitude ("lat") columns.
#' @param country character string; the country to which the data pertain, given
#'   as an ISO3 code (see \code{raster::getData("ISO3")})
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
elevation_correct <- function(data, country) {
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Raster package not installed.", call. = FALSE)
  position_data <-  # Throw a format error early.
    data.frame(lng = data$lng, lat = data$lat)

  exit <- 0
  if (length(list.files(pattern = country)) == 0)
  {
    download_elev_data(country) -> exit
  }
  if (as.logical(exit))
  {
    message("Error.")
    return(rep(NA, times = dim(data)[[1]]))
  }
  e <- raster::getData("alt", country = country, download = FALSE)
  # Extract interpolated elevation data from raster object.
  e <- raster::extract(e, position_data, method = "bilinear")
  return(e)
}
