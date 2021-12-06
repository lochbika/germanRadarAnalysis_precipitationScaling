library(ggplot2)
library(reshape2)
library(RColorBrewer)

# load saved tracks file
load(file = paste(dir_data, "tracks.database.RData", sep = ""))

# colors for scaling diagram
colors <- brewer.pal(5, "Set1")

# do the analysis for specific months
#months.selected <- months
months.selected <- matrix(months, ncol = 3, byrow = TRUE)

for (season in 1:dim(months.selected)[1]) {
  # select tracks by function
  tracks.selected <-
    selectTracks(
      tracks.global,
      months.selected[season,],
      Td.range,
      dewpoint_stationradius,
      maxduration,
      track_mindist
    )
  
  tracks.selected <- tracks.selected[tracks.selected$CS_grid_valueQ2 == 1,]
  
  # bin Td data
  Td.bins <-
    binequal(tracks.selected$Td_value, n = 10000, members = TRUE)
  
  # calculate percentiles for each Td bin
  prct <- c(.9, .95, .975, .99)
  scaling <-
    aggregate(
      tracks.selected$peakvalue,
      FUN = quantile,
      type = 5,
      probs = prct,
      by = list(Td.bins$classes)
    )
  
  scaling$Group.1 <- Td.bins$bmean[, 2]
  
  scaling.df <- data.frame(value = as.vector(scaling$x))
  scaling.df$Td <- rep(scaling$Group.1, length(prct))
  scaling.df$percentile <-
    rep(paste("p", prct * 100, sep = ""), each = dim(Td.bins$bmean)[1])
  
  #############
  ######
  ## plot peakvalue
  ######
  #############
  
  ## create a data frame with values for the scaling lines
  x <- seq(min(scaling.df$Td) - 2, max(scaling.df$Td) + 2, .1)
  y <- exp(0.07 * x - 5)
  scal.lines <- cbind(lin = rep(-5, length(x)),
                      x = x,
                      y = y)
  for (i in seq(-4.6, 5, .4)) {
    y <- exp(0.07 * x + i)
    scal.lines <-
      rbind(scal.lines, cbind(
        lin = rep(i, length(x)),
        x = x,
        y = y
      ))
  }
  scal.lines <- as.data.frame(scal.lines)
  
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
      limits = c(0.5, 15),
      breaks = seq(.5, 15, 2),
      labels = seq(.5, 15, 2)
    ) + geom_smooth(
      method = "lm",
      se = T,
      colour = "black",
      size = .5
    ) +
    xlab(expression(T[d] * " [" * degree * "C]")) +
    geom_path(
      data = scal.lines,
      aes(x = x, y = y, group = lin),
      inherit.aes = F,
      col = "grey",
      linetype = 2
    ) +
    xlim(5, 20)
  
  pl.scal.cell
  
  ggsave(
    plot = pl.scal.cell,
    filename = paste(
      dir_plots,
      "scaling_",
      paste(months.names[months %in% months.selected[season, ]], collapse = "-"),
      ".png",
      sep = ""
    ) ,
    width = 6.5,
    height = 8,
    units = "cm"
  )
}
