#' @describeIn read Read a Garmin .tcx file. Requires the "XML"
#'   package to be installed. \code{\link{read_tcx2}} is a faster alternative
#'   that uses system commands.
#' @export
read_tcx <- function(file = file.choose(), format = TRUE, .list = FALSE) {
  file_xml   <- XML::xmlParse(file)
  xmlrt      <- XML::xmlRoot(file_xml)
  namespaces <- XML::xmlNamespaceDefinitions(xmlrt, simplify = TRUE)
  # Get data field names, and prefix with namespace.
  columns <- names(XML::xmlChildren(
    XML::xmlRoot(file_xml)[["Activities"]][["Activity"]][["Lap"]][["Track"]][["Trackpoint"]]
  ))
  columns <- paste0("/ns:", columns)
  # Additional child fields.
  if ("/ns:Position" %in% columns)
    columns <- c(columns[-match("/ns:Position", columns)],
                 "/ns:Position/ns:LatitudeDegrees",
                 "/ns:Position/ns:LongitudeDegrees")

  len <- length(columns)
  if ("/ns:Extensions" %in% columns) {
    columns <- columns[-match("/ns:Extensions", columns)]
    columns_ext <- names(XML::xmlChildren(
      XML::xmlRoot(file_xml)[["Activities"]][["Activity"]][["Lap"]]
      [["Track"]][["Trackpoint"]][["Extensions"]][["TPX"]]
    ))
    len <- len + length(columns_ext)
  }
  message("Reading .tcx data...") # --------------------------------------------
  pblen <- 50 / len  # Progress bar length.
  data <- lapply(columns, FUN = function(x) {
    message(rep("=", times = pblen), appendLF = FALSE)
    XML::xpathSApply(file_xml, path = paste0("//ns:Trackpoint", x),
                     namespaces = c(ns = namespaces[[4]]),
                     fun = XML::xmlValue) -> out
    if (all(!is.na(suppressWarnings(as.numeric(out)))))
      out <- as.numeric(out)  # Don't cast character columns.
    return(out)
  })
  names(data) <-
    sapply(strsplit(columns, ":"), function(x) return(x[length(x)]))
  # Extensions - note the different namespace.
  if (exists("columns_ext")) {
    data_ext <- lapply(columns_ext, FUN = function(x) {
      message(rep("=", times = pblen), appendLF = FALSE)
      XML::xpathSApply(file_xml, path = paste0("//ns:", x),
                       namespaces = c(ns = namespaces[[2]]),
                       fun = XML::xmlValue) -> out
      if (all(!is.na(suppressWarnings(as.numeric(out)))))
        out <- as.numeric(out)  # Don't cast character columns.
      return(out)
    })
    names(data_ext) <- columns_ext
    data <- append(data, data_ext)
  }
  message("\nDone.")
  # Reformat Time column from character.
  data$Time   <- as.POSIXct(
    gsub("[[:upper:]]", " ", data$Time),
    origin = Sys.time() - as.numeric(Sys.time()),
    format = "%Y-%m-%d %H:%M:%S ")
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
    data <- format_tcx(data)
    class(data) <- c("cycleRdata", "data.frame")
  }
  return(data)
}
