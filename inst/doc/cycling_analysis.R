## ------------------------------------------------------------------------
library(cycleRtools)
data("cycling_data")

## ------------------------------------------------------------------------
colnames(cycling_data)

## ------------------------------------------------------------------------
cycling_data$Wexp.kJ <- Wbal(data = cycling_data, 
                             time = timer.s,  # Name of time column.
                             pwr  = power.W,  # Name of power column.
                             CP   = 330)      # Critical power value.
summary(cycling_data$Wexp.kJ)

## ---- fig.height=5, fig.width=7------------------------------------------
Wbal_plots(data = cycling_data,
           x    = 2,     # Plot against distance.
           CP   = 330)   # Not necessary; annotates power plot.

## ---- fig.width=7--------------------------------------------------------
par(mfrow = c(1, 2), mar = c(4.1, 4.1, 1.1, 1.1))
with(cycling_data, plot(x = distance.km, 
                        y = elevation.m, 
                        type = "l", 
                        col = "green"))
with(cycling_data, plot(x = distance.km, 
                        y = .elevation.corrected.m, 
                        type = "l", 
                        col = "red"))

## ---- fig.width=7--------------------------------------------------------
par(mar = c(4.1, 4.1, 1.1, 1.1))
with(cycling_data, plot(x = distance.km, 
                        y = (elevation.m - .elevation.corrected.m), 
                        cex = 0.2))

## ------------------------------------------------------------------------
tsec <- c(1, 5, 20) * 60  # Time windows must be in units of seconds.
mmv(data = cycling_data, 
    pds  = tsec, 
    column = power.W, 
    verbose = FALSE)


## ------------------------------------------------------------------------
mmv2(x   = uniform(cycling_data, verbose = FALSE)$power.W,  # Messy!
     pds = tsec)

## ---- fig.width=7--------------------------------------------------------
zone_time(data    = cycling_data, 
          column  = power.W,       # Because we're interested in power.
          zbounds = c(300, 400),   # Zone boundaries.
          pct     = TRUE)          # Return zone times as percentages.

# And plot the above:
zdist_plot(data    = cycling_data, 
          column   = power.W, 
          zbounds  = c(300, 400))

## ------------------------------------------------------------------------
# A hypothetical, and tragically short, 1 minute ride.
timer.s <- 1:60
diff_section(timer.s)
# A similiarly tragic ride, but now with a 1 minute cafe stop.
timer.s <- c(1:60, 120:180)
diff_section(timer.s)
# Applied to an *actual* ride:
cycling_data$stop_sections <- diff_section(cycling_data$timer.s)
# How many breaks were there?
unique(cycling_data$stop_sections)

## ------------------------------------------------------------------------
# Average powers for the 5 sections.
with(cycling_data, tapply(power.W, stop_sections, mean))

## ------------------------------------------------------------------------
max(cycling_data$timer.s) / 60  # In minutes.

## ------------------------------------------------------------------------
ride_time(cycling_data$timer.s) / 60  # In minutes.

