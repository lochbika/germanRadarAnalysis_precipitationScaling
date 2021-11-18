#
# directories
#

setwd("/home/kai/Documents/germanRadarAnalysis_precipitationScaling/")

dir_plots <- "plots/"
dir_data <- "data/"

#
# constants
#

track_mindur <- 3  # 3 timesteps = 15 minutes
track_mindist <- 5 # km
dewpoint_timebeforeevent <- 3 #hours
dewpoint_stationradius <- 40 #km
maxduration <- 180
Td.min <- 5 #degC
Td.max <- 50
Td.range <- c(Td.min, Td.max)
area.min <- 0 # km^2
area.max <- 100 ** 2

# for which years and months?
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
binequal <- function(x, n = 5, members = FALSE) {
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
