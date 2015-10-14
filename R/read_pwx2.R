#' @describeIn read Read a Training Peaks .pwx file. Requires the command
#'   line tool "xml2" to be installed.
#' @export
read_pwx2 <- function(file = file.choose(), format = TRUE, .list = FALSE) {
  if (system2("xml2", stdout = FALSE, stderr = FALSE) == 127)
    stop("xml2 not installed/on system path.", call. = FALSE)
  message("Reading .pwx file...")
  tmpf <- tempfile()
  system2("xml2", args = c("<", file, ">", tmpf))
  columns <- c("timeoffset", "hr", "spd", "pwr", "cad", "dist", "alt", "temp")
  data <- lapply(columns, FUN = function(x) {
    system2("cat", args = c(
      tmpf, "| grep",
      paste0("'^/pwx/workout/sample/", x, "='"),
      "| cut -d= -f2"
    ), stdout = TRUE) -> out
    return(as.numeric(out))
  })
  names(data) <- columns
  # Timestamp values------------------------------------------------------------
  timestamp <-
    system2("cat", args = c(
      tmpf, "| grep",
      paste0("'^/pwx/workout/time='"),
      "| cut -d= -f2"
    ), stdout = TRUE)
  timestamp <- sub("T", " ", timestamp)
  timestamp <- strptime(timestamp, "%Y-%m-%d %H:%M:%S")
  data$timestamp <- timestamp + data$timeoffset
  #-----------------------------------------------------------------------------
  if (.list)
    return(data)
  # Are all objects the same length?
  len <- sapply(data, length)
  if (length(unique(len)) > 1) {
    warning(paste0(
      "Columns are not of equal length, trimming to the smallest column.\n",
      "use .list = TRUE to return a list object with lengths preserved."
    ))
    data <-sapply(data, function(x) x[1:min(len)])
  }
  data <- as.data.frame(data)
  if (format) {
    data <- format_pwx(data)
    class(data) <- c("cycleRdata", "data.frame")
  }
  return(data)
}
