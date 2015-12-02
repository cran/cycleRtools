#' @describeIn read Read a Training Peaks .pwx file. Requires the "xml2" package
#'   to be installed. Will make use of the "parallel" package if available.
#' @export
read_pwx <- function(file = file.choose(), format = TRUE) {
  x    <- xml2::read_xml(file)
  ns   <- xml2::xml_ns(x)
  cols <- suppressWarnings(
    xml2::xml_name(xml2::xml_children(xml2::xml_find_one(x, "//d1:sample", ns = ns)))
  )

  message("Reading .pwx file...") # --------------------------------------------
  if (requireNamespace("parallel", quietly = TRUE)) {
    data <- parallel::mclapply(cols, function(c) {
      as.numeric(xml2::xml_text(xml2::xml_find_all(x, paste0("//d1:", c), ns)))
    })
  } else {
    data <- lapply(cols, function(c) {
      as.numeric(xml2::xml_text(xml2::xml_find_all(x, paste0("//d1:", c), ns)))
    })
  }
  names(data) <- cols

  # Timestamp ------------------------------------------------------------------
  ts <- xml2::xml_text(xml2::xml_find_one(x, "//d1:time", ns = ns))
  ts <- sub("T", " ", ts)
  ts <- strptime(ts, "%Y-%m-%d %H:%M:%S")
  data$timestamp <- ts + data$timeoffset

  data <- as.data.frame(data)
  if (format) {
    data        <- format_pwx(data)
    class(data) <- c("cycleRdata", "data.frame")
  }
  return(data)
}
