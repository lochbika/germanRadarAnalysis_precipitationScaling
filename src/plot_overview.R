library(ggplot2)
library(reshape2)
library(RColorBrewer)

# load saved tracks file
load(file = paste(dir_data, "tracks.database.RData", sep = ""))

# convert distance to km
tracks.database$Td_distance <- tracks.database$Td_distance / 1000

# colors for scaling diagram
colors <- brewer.pal(5, "Set1")

# do the analysis for specific months
months.selected <- matrix(months, ncol = 3, byrow = TRUE)

# label the seasons
tracks.database$season <- ""
for (season in 1:3) {
  tracks.database$season[format(tracks.database$datetime, "%m") %in% months.selected[season, ]] <-
    seasons[season]
}

#
# plot descriptive stats about dew point temperature
#
pl <-
  ggplot(tracks.database,
         aes(x = Td_distance, group = season, color = season)) +
  geom_freqpoly(bins = 100) +
  xlab("distance [km]") +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_text(size = pl.basesize)
  )
#pl
ggsave(
  paste(dir_plots, "Td_distance_dist_by_season.png", sep = ""),
  plot = pl,
  width = 6,
  height = 8,
  units = "cm"
)

pl <-
  ggplot(tracks.database, aes(x = Td_value, group = season, color = season)) +
  geom_freqpoly(binwidth = 0.3) +
  xlab(expression(paste(T[d], " [", degree, "C]"))) + xlim(-10, 25) +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = "None",
    legend.justification = c("right", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_text(size = pl.basesize)
  )
ggsave(
  paste(dir_plots, "Td_value_dist_by_season.png", sep = ""),
  plot = pl,
  width = 6,
  height = 8,
  units = "cm"
)

# cumulative distributions by season
prct <- seq(0, 1, 0.0001)

cumdist.distance <-
  aggregate(
    tracks.database$Td_distance,
    by = list(tracks.database$season),
    FUN = quantile,
    probs = prct,
    type = 5,
    na.rm = TRUE
  )
cumdist.distance <- melt(cumdist.distance$x)
colnames(cumdist.distance) <- c("season","percentile","distance")
cumdist.distance$percentile <- rep(prct,each=3)*100
cumdist.distance$season <- seasons[cumdist.distance$season]

# plot
pl <- ggplot(cumdist.distance, aes(x = distance, y = percentile, group=season,color=season)) +
  geom_path() +
  xlab("distance [km]") +
  ylab("percentile [%]") +
  xlim(0, 100) +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = "None",
    legend.justification = c("right", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_text(size = pl.basesize)
  )
ggsave(
  paste(dir_plots, "Td_distance_cumdist_by_season.png", sep = ""),
  plot = pl,
  width = 6,
  height = 8,
  units = "cm"
)


cumdist.Td <-
  aggregate(
    tracks.database$Td_value,
    by = list(tracks.database$season),
    FUN = quantile,
    probs = prct,
    type = 5,
    na.rm = TRUE
  )
cumdist.Td <- melt(cumdist.Td$x)
colnames(cumdist.Td) <- c("season","percentile","Td")
cumdist.Td$percentile <- rep(prct,each=3)*100
cumdist.Td$season <- seasons[cumdist.Td$season]


pl <- ggplot(cumdist.Td, aes(x = Td, y = percentile, color=season, group=season)) +
  geom_line() +
  xlab(expression(paste(T[d], " [", degree, "C]"))) +
  ylab("percentile [%]") +
  xlim(-10, 25) +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = "None",
    legend.justification = c("right", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_text(size = pl.basesize)
  )
ggsave(
  paste(dir_plots, "Td_value_cumdist_by_season.png", sep = ""),
  plot = pl,
  width = 6,
  height = 8,
  units = "cm"
)

