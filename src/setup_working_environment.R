#
# directories
#

setwd("/home/rstudio/germanRadarAnalysis_precipitationScaling/")

dir_plots <- "plots/"
dir_data <- "data/"

#
# constants
#
dewpoint_timebeforeevent <- 3 #hours
dewpoint_stationradius <- 25 * 1000 #km
maxduration <- 24 # time steps (in 5 minute units)
precipitationTypes <- c("mixed", "convective", "stratiform")

# months, years and seasons
years  <- as.character(seq(2001, 2020))
months <- sprintf(seq(3, 11), fmt = "%02g")
months.names <-
  c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
seasons <- c("spring", "summer", "autumn")

#
# define some graphical parameters
#
pl.linewidth <- .5
pl.basesize <- 8

#
# functions
#

# bin data with either constant or varying bin size
binequal <- function(x, n = 5, fixed.width = NULL, members = FALSE) {
  # x: data to be binned
  # get range
  rng <- range(x, na.rm = T)
  # calculate lower and upper limits depending on range and width
  if (members) {
    nobs <- length(x)
    nbin <- ceiling(nobs / n)
  } else{
    nbin <- n + 1
  }
  # calc percentage ranges
  prange <- seq(0, 1, length = nbin)
  # calculate quantiles
  qu <- quantile(x,
                 probs = prange,
                 na.rm = T,
                 type = 5)
  # calc bin bounds
  lowers <- qu[1:(length(prange) - 1)]
  uppers <- qu[2:length(prange)]
  bins <- cbind(lowers, uppers)
  rownames(bins) <- paste("BIN", seq(1, dim(bins)[1]))
  # now mark rows with bin numbers
  classes <- rep(NA, length(x))
  for (i in 1:dim(bins)[1]) {
    if (i == 1) {
      classes[x >= bins[i, 1] & x <= bins[i, 2]] <- i
    } else{
      classes[x > bins[i, 1] & x <= bins[i, 2]] <- i
    }
  }
  classes <-
    list(
      classes = classes,
      bins = bins,
      bmean = aggregate(x ~ classes, FUN = mean, na.rm = T)
    )
  return(classes)
  #
}

# generate scaling lines for plots
genScalingLines <- function(r = 0.07, x.from = -1, x.to = 25, y.from = -4.6, y.to = 5, x.step = 0.1, y.step = 0.4){
  x <- seq(x.from, x.to, x.step)
  y <- exp(r * x - y.to)
  scal.lines <- cbind(lin = rep(-1*y.to, length(x)),
                      x = x,
                      y = y)
  for (i in seq(y.from, y.to, y.step)) {
    y <- exp(r * x + i)
    scal.lines <-
      rbind(scal.lines, cbind(
        lin = rep(i, length(x)),
        x = x,
        y = y
      ))
  }
  scal.lines <- as.data.frame(scal.lines)
  return(scal.lines)
}
