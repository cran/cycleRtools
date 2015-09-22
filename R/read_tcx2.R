#' @describeIn read Read a Garmin .tcx file. Fast and dirty command line
#'   parsing. Requires the command line tool "xml2" to be installed.
#' @export
read_tcx2 <- function(file, format = TRUE, .list = FALSE) {
  if (system2("xml2", stdout = FALSE, stderr = FALSE) == 127)
    stop("xml2 not installed/on system path.", call. = FALSE)
  message("Reading .tcx file...")
  tmpf <- tempfile(fileext = ".txt")
  system2("xml2", args = c(
    "<", file, "|",                                # Convert to flat format.
    "grep", "Trackpoint", "|",                     # Remove irrelevant
    "awk", "-F'Trackpoint'", "'{print $2}'", "|",  # Remove anything < Trackpoint.
    "sed", "'s/\\///g'", "|",                      # Remove slashes.
    "grep", "=",                                   # Final check for relevance.
    ">", tmpf                                      # Redirect output to file.
  ))
  # Get column names.
  columns <- system2("cat", args = c(
    tmpf, "|",
    "cut", "-d=", "-f1", "|",
    "sort", "-u"
  ), stdout = TRUE)
  # Remove errant column name.
  columns <- grep("@", columns, value = TRUE, invert = TRUE)
  # Read data.
  data <- lapply(columns, function(x) {
    system2("cat", args = c(
      tmpf, "|",
      "grep", x, "|",
      "cut", "-d=", "-f2"
    ), stdout = TRUE) -> out
    if (all(!is.na(suppressWarnings(as.numeric(out)))))
      out <- as.numeric(out)  # Don't cast character columns.
    return(out)
  })
  names(data) <- columns
  # Reformat Time column from character.
  data$Time   <- as.POSIXct(
    gsub("[[:upper:]]", " ", data$Time),
    origin = Sys.time() - as.numeric(Sys.time()),
    format = "%Y-%m-%d %H:%M:%S ")
  if (.list)
    return(data)
  # Check all list elements are the same length; if not, trim all columns
  # down to the lowest value.
  len <- sapply(data, length)
  if (length(unique(len)) > 1) {
    warning(paste0(
      "Columns are not of equal length, trimming to the smallest column.\n",
      "use .list = TRUE to return a list object with lengths preserved."
    ), call. = FALSE)
    data <- sapply(data, function(x) x[1:min(len)])
  }
  data <- as.data.frame(data)
  if (format) {
    data <- format_tcx(data)
    class(data) <- c("cycleRdata", "data.frame")
  }
  return(data)
}
