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
  tracks.database$season[format(tracks.database$datetime, "%m") %in% months.selected[season,]] <-
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
tracks.database <- tracks.database[!(missingEntries), ]

# use only tracks that have a Td_distance < 25km
tracks.database <-
  tracks.database[tracks.database$Td_distance <= dewpoint_stationradius, ]
# use only tracks that are up to 2 hours long
tracks.database <-
  tracks.database[tracks.database$duration <= maxduration, ]

# restrict analysis to 2001 to 2015
tracks.database <-
  tracks.database[format(tracks.database$datetime, "%Y") %in% as.character(seq(2001, 2015)),]

# Td must be at least 0 degC
tracks.database <- tracks.database[tracks.database$Td_value >= 0, ]

#
# first all in one; do not distinguish between precipitation types
#

# bin Td data
Td.bins <-
  binequal(tracks.database$Td_value, n = 10000, members = TRUE)
rownames(Td.bins$bmean) <- Td.bins$bmean[, 1]

# calculate percentiles for each Td bin
prct <- c(.95, .975, .99, .995)
scaling <-
  aggregate(
    tracks.database$peakvalue,
    FUN = quantile,
    type = 5,
    probs = prct,
    by = list(Td.bins$classes)
  )

scaling$Group.1 <- Td.bins$bmean[as.character(scaling$Group.1), 2]

# calculate scaling rate based on linear fit of log-transformed data
for(i in 1:length(prct)) {
  fit <- lm(log(scaling$x[, i]) ~ scaling$Group.1)
  print(paste("Scaling rate,", prct[i]*100,"%", round(fit$coefficients[2] * 100, digits =
                                        2)))
}


scaling.df <- data.frame(value = as.vector(scaling$x))
scaling.df$Td <- rep(scaling$Group.1, length(prct))
#scaling.df$precipType <- rep(scaling$Group.2, length(prct))
scaling.df$percentile <-
  rep(paste("p", prct * 100, sep = ""), each = dim(Td.bins$bmean)[1])

## create a data frame with values for the scaling lines
scal.lines <- genScalingLines(r = 0.14)

pl.scal.cell <-
  ggplot(
    scaling.df,
    aes(
      x = Td,
      y = value,
      group = percentile,
      shape = percentile,
      color = percentile
    )
  ) + geom_point(size = 1) +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = c(0, 1),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(
    trans = "log",
    limits = c(0.5, 25),
    breaks = seq(.5, 25, 5),
    labels = seq(.5, 25, 5)
  ) + geom_smooth(
    method = "lm",
    se = T,
    colour = "black",
    size = .5
  ) +
  xlab(expression(T[d] * " [" * degree * "C]")) +
  geom_path(
    data = genScalingLines(),
    aes(x = x, y = y, group = lin),
    inherit.aes = F,
    col = "grey",
    linetype = 2,
    size = 0.35
  ) +
  geom_path(
    data = genScalingLines(r = 0.14),
    aes(x = x, y = y, group = lin),
    inherit.aes = F,
    col = "orange",
    linetype = 2,
    size = 0.35
  ) +
  xlim(0, 20)

#pl.scal.cell

ggsave(
  plot = pl.scal.cell,
  filename = paste(dir_plots,
                   "scaling_all_seasons.png",
                   sep = "") ,
  width = 6.5,
  height = 8,
  units = "cm"
)

#
# first all in one; with respect to precipitation type
#

# bin for each precipitation type
for (pType in precipitationTypes) {
  # select tracks with precipitation type
  tracks.selected <-
    tracks.database[tracks.database$CS_grid_valueQ2 == pType, ]
  
  # bin Td data
  Td.bins <-
    binequal(tracks.selected$Td_value, n = 10000, members = TRUE)
  rownames(Td.bins$bmean) <- Td.bins$bmean[, 1]
  
  # calculate percentiles for each Td bin
  prct <- c(.95, .975, .99, .995)
  scaling <-
    aggregate(
      tracks.selected$peakvalue,
      FUN = quantile,
      type = 5,
      probs = prct,
      by = list(Td.bins$classes)
    )
  
  scaling$Group.1 <- Td.bins$bmean[as.character(scaling$Group.1), 2]
  
  # calculate scaling rate based on linear fit of log-transformed data
  for(i in 1:length(prct)) {
    fit <- lm(log(scaling$x[, i]) ~ scaling$Group.1)
    print(paste("Scaling rate,",pType, prct[i]*100,"%", round(fit$coefficients[2] * 100, digits =
                                                          2)))
  }
  
  scaling.df <- data.frame(value = as.vector(scaling$x))
  scaling.df$Td <- rep(scaling$Group.1, length(prct))
  scaling.df$precipType <- rep(pType, length(prct))
  scaling.df$percentile <-
    rep(paste("p", prct * 100, sep = ""), each = dim(Td.bins$bmean)[1])
  if (exists("scaling.final")) {
    scaling.final <- rbind(scaling.final, scaling.df)
  } else{
    scaling.final <- scaling.df
  }
}
scaling.df <- scaling.final
rm(scaling.final)

## create a data frame with values for the scaling lines
scal.lines <- genScalingLines()

pl.scal.cell <-
  ggplot(
    scaling.df,
    aes(
      x = Td,
      y = value,
      group = percentile,
      shape = percentile,
      color = percentile
    )
  ) + geom_point(size = 1) +
  facet_grid(cols = vars(precipType)) +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = c(0, 1),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(
    trans = "log",
    limits = c(0.5, 25),
    breaks = seq(.5, 25, 5),
    labels = seq(.5, 25, 5)
  ) + geom_smooth(
    method = "lm",
    se = T,
    colour = "black",
    size = .5
  ) +
  xlab(expression(T[d] * " [" * degree * "C]")) +
  geom_path(
    data = genScalingLines(),
    aes(x = x, y = y, group = lin),
    inherit.aes = F,
    col = "grey",
    linetype = 2,
    size = 0.35
  ) +
  geom_path(
    data = genScalingLines(r = 0.14),
    aes(x = x, y = y, group = lin),
    inherit.aes = F,
    col = "orange",
    linetype = 2,
    size = 0.35
  ) +
  xlim(0, 20)

#pl.scal.cell

ggsave(
  plot = pl.scal.cell,
  filename = paste(dir_plots,
                   "scaling_all_seasons_precipType.png",
                   sep = "") ,
  width = 12,
  height = 8,
  units = "cm"
)

# now by season
for (season in 1:dim(months.selected)[1]) {
  # select tracks by function
  tracks.selected <-
    tracks.database[tracks.database$season == seasons[season] &
                      format(tracks.database$datetime, "%Y") %in% c("2007", "2008"),]
  
  # bin Td data
  Td.bins <-
    binequal(tracks.selected$Td_value, n = 2500, members = TRUE)
  rownames(Td.bins$bmean) <- Td.bins$bmean[, 1]
  
  # calculate percentiles for each Td bin
  prct <- c(.95, .975, .99, .995)
  scaling <-
    aggregate(
      tracks.selected$peakvalue,
      FUN = quantile,
      type = 5,
      probs = prct,
      by = list(Td.bins$classes, tracks.selected$CS_grid_valueQ2)
    )
  
  scaling$Group.1 <- Td.bins$bmean[as.character(scaling$Group.1), 2]
  scaling.df <- data.frame(value = as.vector(scaling$x))
  scaling.df$Td <- rep(scaling$Group.1, length(prct))
  scaling.df$precipType <- rep(scaling$Group.2, length(prct))
  scaling.df$percentile <-
    rep(paste("p", prct * 100, sep = ""), each = dim(Td.bins$bmean)[1] *
          3)
  
  #############
  ######
  ## plot peakvalue
  ######
  #############
  
  ## create a data frame with values for the scaling lines
  scal.lines <- genScalingLines()
  
  pl.scal.cell <-
    ggplot(
      scaling.df,
      aes(
        x = Td,
        y = value,
        group = percentile,
        shape = percentile,
        color = percentile
      )
    ) + geom_point(size = 1) +
    facet_grid(rows = vars(precipType)) +
    theme_bw(base_size = pl.basesize) +
    theme(
      panel.spacing = unit(0, "line"),
      strip.background = element_blank(),
      strip.placement = 'outside',
      strip.text = element_text(size = pl.basesize),
      legend.position = c(0, 1),
      legend.justification = c("left", "top"),
      legend.background = element_blank(),
      plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
      axis.text = element_text(size = pl.basesize),
      legend.title = element_blank(),
      legend.text = element_text(size = pl.basesize),
      legend.key.height = unit(.5, "line"),
      axis.title.y = element_blank()
    ) +
    scale_y_continuous(
      trans = "log",
      limits = c(0.5, 25),
      breaks = seq(.5, 25, 5),
      labels = seq(.5, 25, 5)
    ) + geom_smooth(
      method = "lm",
      se = T,
      colour = "black",
      size = .5
    ) +
    xlab(expression(T[d] * " [" * degree * "C]")) +
    geom_path(
      data = genScalingLines(),
      aes(x = x, y = y, group = lin),
      inherit.aes = F,
      col = "grey",
      linetype = 2,
      size = 0.35
    ) +
    geom_path(
      data = genScalingLines(r = 0.14),
      aes(x = x, y = y, group = lin),
      inherit.aes = F,
      col = "orange",
      linetype = 2,
      size = 0.35
    ) +
    xlim(5, 20)
  
  #pl.scal.cell
  
  ggsave(
    plot = pl.scal.cell,
    filename = paste(dir_plots,
                     "scaling_",
                     seasons[season],
                     ".png",
                     sep = "") ,
    width = 6.5,
    height = 12,
    units = "cm"
  )
}
