library(ggplot2)
library(reshape2)
library(RColorBrewer)

# load saved tracks file
load(file = paste(dir_data, "tracks.database.RData", sep = ""))

# colors for scaling diagram
colors <- brewer.pal(5, "Set1")

# do the analysis for specific months
months.selected <- matrix(months, ncol = 3, byrow = TRUE)

# label the seasons
tracks.database$season <- ""
for(season in 1:3){
  tracks.database$season[format(tracks.database$datetime, "%m") %in% months.selected[season,]] <- seasons[season]
}

#
# plot descriptive stats about dew point temperature
#

pl <- ggplot(tracks.database, aes(x=Td_distance, group=season, color=season)) + geom_freqpoly(bins=100)
pl
pl <- ggplot(tracks.database, aes(x=Td_value, group=season, color=season)) + geom_freqpoly(binwidth=0.5)
pl

cumdist.distance <- quantile(tracks.database$Td_distance/1000, probs=seq(0,1,0.001),type = 5, na.rm = T)
cumdist.distance <- melt(cumdist.distance)
cumdist.distance$x <- seq(0,1,0.001)*100
cumdist.Td <- quantile(tracks.database$Td_value, probs=seq(0,1,0.001),type = 5, na.rm = T)
cumdist.Td <- melt(cumdist.Td)
cumdist.Td$x <- seq(0,1,0.001)*100

pl <- ggplot(cumdist.distance, aes(x=value, y=x)) + geom_line()
pl

pl <- ggplot(cumdist.Td, aes(x=value, y=x)) + geom_line()
pl
