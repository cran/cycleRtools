# IN PROGRESS ------------------------------------------------------------------
# summary.cycleRdata <- function(data) {
#   "Ride time (min)"    <- ride_time(data$timer.s) / 60
#   "Elapsed time (min)" <- max(data$timer.s, na.rm = TRUE) / 60
#   "No. of laps"        <- as.numeric(max(data$lap))
#
#   "Speed stats" <- data.frame(
#     Mean   = mean(data$speed.kmh, na.rm = TRUE),
#     Median = median(data$speed.kmh, na.rm = TRUE),
#     Max    = max(data$speed.kmh, na.rm = TRUE),
#     SD     = sd(data$speed.kmh, na.rm = TRUE),
#     IQR    = IQR(data$speed.kmh, na.rm = TRUE)
#   )
#   "Power stats" <- data.frame(
#     Mean   = mean(data$power.W, na.rm = TRUE),
#     Median = median(data$power.W, na.rm = TRUE),
#     Max    = max(data$power.W, na.rm = TRUE),
#     SD     = sd(data$power.W, na.rm = TRUE),
#     IQR    = IQR(data$power.W, na.rm = TRUE)
#   )
#   #-----------------------------------------------------------------------------
#   ob  <- grep("data", invert = TRUE,    # Remove data from the returned list.
#               rev(ls(sorted = FALSE)),  # Use the above order.
#               value = TRUE)
#   out <- mget(ob)
#   out <- lapply(out, round, digits = 2)
#   return(out)
# }
