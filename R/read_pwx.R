#' @describeIn read Read a Training Peaks .pwx file. Requires the "XML"
#'   package to be installed. \code{\link{read_pwx2}} is a faster alternative
#'   that uses system commands.
#' @export
read_pwx <- function(file = file.choose(), format = TRUE, .list = FALSE) {
  message("Reading .pwx file...")
  file_xml  <- XML::xmlParse(file)
  namespaces <- XML::xmlNamespaceDefinitions(file_xml, simplify = TRUE)
  columns <- names(XML::xmlChildren(
    XML::xmlRoot(file_xml)[["workout"]][["sample"]]
  ))
  pblen <- 50 / length(columns)  # Progress bar length.
  data <- lapply(columns, FUN = function(x) {
    message(rep("=", times = pblen), appendLF = FALSE)
    XML::xpathSApply(
      file_xml, path = paste0("//ns:", x),
      namespaces = c(ns = namespaces[[1]]), simplify = TRUE,
      fun = XML::xmlValue) -> out
    return(as.numeric(out))
  })
  message("\nDone.")
  names(data) <- columns
  # Timestamp values------------------------------------------------------------
  timestamp <- XML::xmlValue(XML::xmlRoot(file_xml)[["workout"]][["time"]])
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
    data <- sapply(data, function(x) x[1:min(len)])
  }
  data <- as.data.frame(data)
  if (format) {
    data <- format_pwx(data)
    class(data) <- c("cycleRdata", "data.frame")
  }
  return(data)
}
