library(geosphere)
library(doFuture)
library(tictoc)
library(progressr)

# load saved tracks file
load(file = paste(dir_data, "tracks.database.RData", sep = ""))
load(file = paste(dir_data, "stationdata_TD_DWD.RData", sep = ""))
load(file = paste(dir_data, "station_metaIndex_dew_point.RData", sep = ""))

# calculate the distance matrix
trackstationdistance <- distm(as.matrix(cbind(
  tracks.database$coorLon, tracks.database$coorLat
)),
as.matrix(cbind(
  metaIndex$geoLaenge, metaIndex$geoBreite
)))

# now create a matrix with the station IDs for each track
trackstationids <-
  matrix(
    rep(metaIndex$Stations_id, each = nrow(trackstationdistance)),
    ncol = dim(trackstationdistance)[2],
    nrow = dim(trackstationdistance)[1]
  )

# sort each row of the two matrices by the distance
{
  trackstationids.ordered <- trackstationids
  
  for (i in seq_len(nrow(trackstationids)))
    trackstationids.ordered[i, ] <-
    trackstationids[i, order(trackstationdistance[i,])]
  }
{
  trackstationdistance.ordered <- trackstationdistance
  
  for (i in seq_len(nrow(trackstationdistance)))
    trackstationdistance.ordered[i, ] <-
    trackstationdistance[i, order(trackstationdistance[i,])]
}
rm(trackstationdistance, trackstationids)

# put it together in a list and save it to disk; only keep the closest 10 stations
tracksandstations <-
  list(stationIDs = trackstationids.ordered[, 1:10], distances = trackstationdistance.ordered[, 1:10])
save(
  tracksandstations,
  file = paste(dir_data, "tracksandstations_IDs_distance.RData", sep = "")
)
rm(trackstationdistance.ordered, trackstationids.ordered)

# --------------------------------------------
# start here if distance matrix already exists
load(paste(dir_data, "tracksandstations_IDs_distance.RData", sep = ""))

# generate date for n hours ahead
ahead_time <-
  as.numeric(tracks.database$datetime) - dewpoint_timebeforeevent * 3600
# round it down to the full hour
ahead_time <- ahead_time - (ahead_time %% 3600)
# convert back to time object
ahead_time <-
  as.POSIXct(ahead_time,
             tz = "UTC",
             origin = "1970-01-01 00:00")

tracks.database$Td_value <- rep(NA, dim(tracks.database)[1])
tracks.database$TT_value <- rep(NA, dim(tracks.database)[1])
tracks.database$Td_distance <- rep(NA, dim(tracks.database)[1])
for (track in 1:dim(tracks.database)[1]) {
  if (track %% 1000 == 0)
    print(track)
  # temporary store station data in new object
  trystation <- 1
  temp <-
    stationdataTD[[as.character(tracksandstations[["stationIDs"]][track, trystation])]]
  match.date <- temp$datetime == ahead_time[track]
  selectedTd <- temp$TD[match.date]
  selectedTT <- temp$TT[match.date]
  selectedDist <-
    tracksandstations[["distances"]][track, trystation]
  while ((length(selectedTd) == 0 |
          isTRUE(is.na(selectedTd))) & trystation < 10) {
    trystation <- trystation + 1
    temp <-
      stationdataTD[[as.character(tracksandstations[["stationIDs"]][track, trystation])]]
    match.date <- temp$datetime == ahead_time[track]
    selectedTd <- temp$TD[match.date]
    selectedTT <- temp$TT[match.date]
    selectedDist <-
      tracksandstations[["distances"]][track, trystation]
  }
  if (length(selectedTd) == 0 |
      isTRUE(is.na(selectedTd))) {
    tracks.database$Td_value[track] <- NA
    tracks.database$TT_value[track] <- NA
    tracks.database$Td_distance[track] <- NA
  } else{
    tracks.database$Td_value[track] <- selectedTd
    tracks.database$TT_value[track] <- selectedTT
    tracks.database$Td_distance[track] <- selectedDist
  }
}


# update tracks file
save(tracks.database, file = paste(dir_data, "tracks.database.RData", sep =
                                   ""))
