#' @describeIn read Read an SRM (.srm) file. This requires
#'   \href{http://www.zuto.de/project/srmio/}{Rainer Clasen's srmio library} to
#'   be installed and on the system path.
#' @export
read_srm <- function(file, format = TRUE) {
  if (system2("srmcmd", stdout = FALSE, stderr = FALSE) == 127)
    stop(paste(
      "srmio library not installed/on the system path.",
      "visit http://www.zuto.de/project/srmio/", sep = "\n"),
      call. = FALSE)
  message("Reading .srm file...")
  tmpf     <- tempfile()
  system2("srmcmd", args = c("--read", file), stdout = tmpf)
  data <- read.table(tmpf, header = TRUE, sep = "\t")
  if (format) {
    data <- format_srm(data)
    class(data) <- c("cycleRdata", "data.frame")
  }
  return(data)
}
