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
for (season in 1:3) {
  tracks.database$season[format(tracks.database$datetime, "%m") %in% months.selected[season, ]] <-
    seasons[season]
}

# convert precipitation types to factors
tracks.database$CS_grid_valueQ1 <-
  factor(tracks.database$CS_grid_valueQ1,
         levels = c(0, 1, 2),
         labels = precipitationTypes)
tracks.database$CS_grid_valueQ2 <-
  factor(tracks.database$CS_grid_valueQ2,
         levels = c(0, 1, 2),
         labels = precipitationTypes)

# convert season to factor
tracks.database$season <-
  factor(tracks.database$season, levels = c("spring", "summer", "autumn"))

# remove missing entries (Td and precipitation type)
missingEntries <-
  is.na(tracks.database$Td_value) |
  is.na(tracks.database$CS_grid_valueQ1) |
  is.na(tracks.database$CS_grid_valueQ1)
print(paste(
  "Number of missing values:",
  sum(missingEntries),
  sum(missingEntries) / dim(tracks.database)[1] * 100,
  "%"
))
tracks.database <- tracks.database[!(missingEntries),]

# use only tracks that have a Td_distance < 25km
tracks.database <-
  tracks.database[tracks.database$Td_distance <= dewpoint_stationradius,]
# use only tracks that are up to 2 hours long
tracks.database <- tracks.database[tracks.database$duration <= maxduration,]

# restrict analysis to 2001 to 2015
tracks.database <- tracks.database[format(tracks.database$datetime, "%Y") %in% as.character(seq(2001,2015)), ]

# Q1
# plot descriptive stats about dew point temperature 
# taking precipitation type into account
#

# pl <-
#   ggplot(tracks.database,
#          aes(x = Td_distance, group = season, color = season)) +
#   geom_freqpoly(bins = 100) +
#   geom_freqpoly(data=tracks.database, bins = 100, aes(x=Td_distance), inherit.aes = FALSE) +
#   xlab("distance [km]") + 
#   facet_grid(rows = vars(CS_grid_valueQ1)) +
#   theme_bw(base_size = pl.basesize) +
#   theme(
#     panel.spacing = unit(0, "line"),
#     strip.background = element_blank(),
#     strip.placement = 'outside',
#     strip.text = element_text(size = pl.basesize),
#     legend.position = c(1, 1),
#     legend.justification = c("right", "top"),
#     legend.background = element_blank(),
#     plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
#     axis.text = element_text(size = pl.basesize),
#     legend.title = element_blank(),
#     legend.text = element_text(size = pl.basesize),
#     legend.key.height = unit(.5, "line"),
#     axis.title.y = element_text(size = pl.basesize)
#   )
# 
# ggsave(
#   paste(dir_plots, "Td_distance_dist_by_precipTypeQ1_season.png", sep = ""),
#   plot = pl,
#   width = 6,
#   height = 8,
#   units = "cm"
# )

pl.TdQ1 <-
  ggplot(tracks.database, aes(x = Td_value, group = season, color = season)) +
  geom_freqpoly(binwidth = 0.3) +
  geom_freqpoly(data=tracks.database, binwidth = 0.3, aes(x=Td_value, color = "all"), inherit.aes = FALSE) +
  xlab(expression(paste(T[d], " [", degree, "C]"))) + xlim(-10, 25) +
  facet_grid(rows = vars(CS_grid_valueQ1)) +
  ylim(0,15000) +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = c(0, 0.36),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_text(size = pl.basesize)
  )
ggsave(
  paste(dir_plots, "Td_value_dist_by_precipTypeQ1_season.png", sep = ""),
  plot = pl.TdQ1,
  width = 6,
  height = 8,
  units = "cm"
)

# Q2
# plot descriptive stats about dew point temperature 
# taking precipitation type into account
#

# pl <-
#   ggplot(tracks.database,
#          aes(x = Td_distance, group = season, color = season)) +
#   geom_freqpoly(bins = 100) +
#   xlab("distance [km]") + 
#   facet_grid(rows = vars(CS_grid_valueQ2)) +
#   theme_bw(base_size = pl.basesize) +
#   theme(
#     panel.spacing = unit(0, "line"),
#     strip.background = element_blank(),
#     strip.placement = 'outside',
#     strip.text = element_text(size = pl.basesize),
#     legend.position = c(1, 1),
#     legend.justification = c("right", "top"),
#     legend.background = element_blank(),
#     plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
#     axis.text = element_text(size = pl.basesize),
#     legend.title = element_blank(),
#     legend.text = element_text(size = pl.basesize),
#     legend.key.height = unit(.5, "line"),
#     axis.title.y = element_text(size = pl.basesize)
#   )
# 
# ggsave(
#   paste(dir_plots, "Td_distance_dist_by_precipTypeQ2_season.png", sep = ""),
#   plot = pl,
#   width = 6,
#   height = 8,
#   units = "cm"
# )

pl.TdQ2 <-
  ggplot(tracks.database, aes(x = Td_value, group = season, color = season)) +
  geom_freqpoly(binwidth = 0.3) +
  geom_freqpoly(data=tracks.database, binwidth = 0.3, aes(x=Td_value, color = "all"), inherit.aes = FALSE) +
  xlab(expression(paste(T[d], " [", degree, "C]"))) + xlim(-10, 25) +
  facet_grid(rows = vars(CS_grid_valueQ2)) +
  ylim(0,15000) +
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
  paste(dir_plots, "Td_value_dist_by_precipTypeQ2_season.png", sep = ""),
  plot = pl.TdQ2,
  width = 6,
  height = 8,
  units = "cm"
)

# Q2 - Q1
# plot descriptive stats about dew point temperature 
# taking precipitation type into account
#

bin.breaks <- seq(min(tracks.database$Td_value), max(tracks.database$Td_value), 0.5)
bin.center <- bin.breaks[2:length(bin.breaks)] - 0.25

Td.bins <- cut(tracks.database$Td_value, breaks = bin.breaks, labels = F, include.lowest = T)
Td.bins <- bin.center[Td.bins]

tracks.database$Td.bins <- Td.bins
rm(Td.bins)

Td.freqQ1 <- table(tracks.database$Td.bins, tracks.database$season, tracks.database$CS_grid_valueQ1)
Td.freqQ2 <- table(tracks.database$Td.bins, tracks.database$season, tracks.database$CS_grid_valueQ2)

Td.freq <- melt(Td.freqQ1)
Td.freq$value <- melt(Td.freqQ2)$value - Td.freq$value

colnames(Td.freq) <- c("Td_value", "season", "precType", "diff")

# add data for all seasons
Td.freq.all <- apply(Td.freqQ2,MARGIN=c(1,3),FUN=sum) - apply(Td.freqQ1,MARGIN=c(1,3),FUN=sum)
Td.freq.all <- melt(Td.freq.all)
colnames(Td.freq.all) <- c("Td_value", "precType", "diff")
Td.freq.all$season <- "all"

pl <-
  ggplot(Td.freq, aes(x = Td_value, y=diff, group = season, color = season)) +
  geom_path() +
  geom_path(data=Td.freq.all, aes(x=Td_value, y=diff, color = "all"), inherit.aes = FALSE) +
  xlab(expression(paste(T[d], " [", degree, "C]"))) + xlim(-10, 25) +
  facet_grid(rows = vars(precType)) +
  ylim(-4500, 4500) +
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
  paste(dir_plots, "Td_value_dist_by_precipType_season_difference.png", sep = ""),
  plot = pl,
  width = 6,
  height = 8,
  units = "cm"
)
