#' @describeIn read Read a Garmin (Ltd) device .fit file. This invokes
#'   \code{\link[base]{system2}} to execute the FitCSVTool.jar command line tool
#'   (see \href{http://www.thisisant.com/resources/fit}{FIT SDK}). Hence, this
#'   function requires that Java (JRE/JDK) binaries be on the system path.
#'
#' @export
read_fit <- function(file, format = TRUE) {
  if (system2("java", stdout = FALSE, stderr = FALSE) == 127)
    stop("java binaries not installed/on system path", call. = FALSE)
  FitCSVTool <- system.file("java/FitCSVTool.jar", package = "cycleRtools")
  if (Sys.info()["sysname"] == "Windows")
    FitCSVTool <- Sys.which(FitCSVTool)
  tmpf       <- tempfile()
  #-----------------------------------------------------------------------------
  message("Getting record data...")
  system2("java", args = c(
    "-jar", FitCSVTool, "-b", file, tmpf,
    "--data", "record", "--defn", "none"
  ), stdout = FALSE)
  record_data <- read.csv(paste0(tmpf, "_data.csv"))
  #-----------------------------------------------------------------------------
  message("Getting lap data...")
  system2("java", args = c(
    "-jar", FitCSVTool, "-b", file, tmpf,
    "--data", "lap", "--defn", "none"
  ), stdout = FALSE)
  lap_data <- read.csv(paste0(tmpf, "_data.csv"))
  #-----------------------------------------------------------------------------
  message("Creating lap column...")
  record_data$lap <- 0
  record_data$lap[match(
    lap_data$lap.start_time, record_data$record.timestamp.s.
  )] <- 1
  record_data$lap[1] <- 1
  record_data$lap <- cumsum(record_data$lap)
  record_data$lap <- factor(record_data$lap, ordered = TRUE)
  #-----------------------------------------------------------------------------
  record_data$record.timestamp.s. <- as.POSIXct(
    record_data$record.timestamp.s., tz = "UTC", origin = "1989-12-31"
  )
  message("Done.")
  # Format----------------------------------------------------------------------
  if (format) {
    record_data <- format_fit(record_data)
    class(record_data) <- c("cycleRdata", "data.frame")
  }
  return(record_data)
}
