#' @describeIn read Read a Garmin .tcx file. Requires the "xml2" package to be
#'   installed. Will make use of the "parallel" package if available.
#' @export
read_tcx <- function(file = file.choose(), format = TRUE) {
  x    <- xml2::read_xml(file)
  ns   <- xml2::xml_ns(x)
  cols <- suppressWarnings(
    xml2::xml_name(xml2::xml_children(xml2::xml_find_one(x, "//d1:Trackpoint", ns)))
  )
  cols <- paste0("//d1:", cols)

  # Get extra data columns as appropriate --------------------------------------
  if (any(grepl("Position", cols))) {
    cols <- cols[!grepl("Position", cols)]
    tmp  <- suppressWarnings(
      xml2::xml_name(xml2::xml_children(xml2::xml_find_one(x, "//d1:Position", ns)))
    )
    cols <- c(cols, paste0("//d1:", tmp))
  }
  if (any(grepl("Extensions", cols))) {
    cols <- cols[!grepl("Extensions", cols)]
    tmp  <- suppressWarnings(
      xml2::xml_name(xml2::xml_children(xml2::xml_find_one(x, "//ns3:TPX", ns)))
    )
    cols <- c(cols, paste0("//ns3:", tmp))
  }

  # Only look in trackpoints.
  trcols <- paste0("//d1:Trackpoint", cols)

  message("Reading .tcx file...") # --------------------------------------------
  if (requireNamespace("parallel", quietly = TRUE)) {
    data <- parallel::mclapply(trcols, function(c) {
      out <- xml2::xml_text(xml2::xml_find_all(x, c, ns))
      if (all(!is.na(suppressWarnings(as.numeric(out)))))
        out <- as.numeric(out)  # Don't cast character columns.
      return(out)
    })
  } else {
    data <- lapply(trcols, function(c) {
      out <- xml2::xml_text(xml2::xml_find_all(x, c, ns))
      if (all(!is.na(suppressWarnings(as.numeric(out)))))
        out <- as.numeric(out)  # Don't cast character columns.
      return(out)
    })
  }

  names(data) <- sapply(strsplit(cols, ":"), function(x) return(x[length(x)]))
  # Deal with missing fields ---------------------------------------------------
  len <- sapply(data, length)
  if (length(unique(len)) > 1) {
    message("Resolving missing data points...")
    issues  <- names(len[len < max(len, na.rm = TRUE)])
    .issues <- cols[sapply(issues, function(x) which(grepl(x, cols)))]
    nds     <- xml2::as_list(xml2::xml_find_all(x, "//d1:Trackpoint", ns))
    logi    <- sapply(.issues, function(i) {
      tr    <- lapply(nds, function(n)
        try(xml2::xml_find_one(n, paste0(".", i), ns), silent = TRUE))
      out   <- sapply(tr, function(t) !inherits(t, "try-error"))
      return(out)
    })
    colnames(logi)   <- issues
    to_correct       <- match(issues, names(data))
    data[to_correct] <- lapply(to_correct, function(i)
      ifelse(logi[, names(data[i])], data[[i]], NA))
  }

  # Reformat timestamps --------------------------------------------------------
  data$Time   <- as.POSIXct(
    gsub("[[:upper:]]", " ", data$Time),
    origin = Sys.time() - as.numeric(Sys.time()),
    format = "%Y-%m-%d %H:%M:%S ")

  data <- as.data.frame(data)
  if (format) {
    data        <- format_tcx(data)
    class(data) <- c("cycleRdata", "data.frame")
  }
  return(data)
}
