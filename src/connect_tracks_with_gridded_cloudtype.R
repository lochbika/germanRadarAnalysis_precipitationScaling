library(geosphere)

# load saved tracks file
load(file = paste(dir_data, "tracks.global.RData", sep = ""))
grid.definition <-
  read.csv(file = paste(dir_data, "grid_CS_type_aggregatoion.csv", sep = ""))

# calculate the distance matrix
trackgriddistance <- distm(as.matrix(cbind(
  tracks.global$coorLon,
  tracks.global$coorLat
)),
as.matrix(cbind(grid.definition$X,
                grid.definition$Y)))

# now create a matrix with the station IDs for each track
gridstationids <-
  matrix(
    rep(seq(1, 6), each = nrow(trackgriddistance)),
    ncol = dim(trackgriddistance)[2],
    nrow = dim(trackgriddistance)[1]
  )

# sort each row of the two matrices by the distance
{
  gridstationids.ordered <- gridstationids
  
  for (i in seq_len(nrow(gridstationids)))
    gridstationids.ordered[i,] <-
    gridstationids[i, order(trackgriddistance[i, ])]
  }
{
  trackgriddistance.ordered <- trackgriddistance
  
  for (i in seq_len(nrow(trackgriddistance)))
    trackgriddistance.ordered[i,] <-
    trackgriddistance[i, order(trackgriddistance[i, ])]
}
rm(trackgriddistance, gridstationids)

# put it together in a list and save it to disk; only keep the closest station
tracksandgrid <-
  list(gridIDs = gridstationids.ordered[,1], distances = trackgriddistance.ordered[,1])
save(
  tracksandgrid,
  file = paste(
    dir_data,
    "tracksglobalandgrid_IDs_distance_cloud_type_Q1_Q2_gridded.RData",
    sep = ""
  )
)
rm(gridstationids.ordered, trackgriddistance.ordered)

# --------------------------------------------
# start here if distance matrix already exists
load(
  paste(
    dir_data,
    "tracksglobalandgrid_IDs_distance_cloud_type_Q1_Q2_gridded.RData",
    sep = ""
  )
)
load(file = paste(dir_data, "parallel_hourly_gridded_cloudtypes_Q1_Q2_timeseries.RData", sep = ""))

# create temporary vectors for cloud types
CS_grid_valueQ1 <-
  rep(-1, dim(tracks.global)[1])
CS_grid_valueQ2 <-
  rep(-1, dim(tracks.global)[1])
CS_grid_distance <-
  rep(-1, dim(tracks.global)[1])
# generate dates for gridded data from dimnames
grid.cloudtypes.date <-
  as.POSIXct(as.numeric(dimnames(grid.cloudtypes.timeseries)$date), tz = "UTC",
             origin = "1970-01-01 00:00")

# loop tracks
for (track in 1:dim(tracks.global)[1]) {
  if (track %% 1000 == 0)
    print(track)
  # closest grid point
  closestGridoint <- tracksandgrid$gridIDs[track]
  # closest date for track in gridded data; get only index
  closestDate <-
    which.min(abs(
      difftime(grid.cloudtypes.date, tracks.global$datetime[track])
    ))
  
  # select the record
  CS_grid_distance[track] <-
    tracksandgrid$distances[track]
  CS_grid_valueQ1[track] <-
    grid.cloudtypes.timeseries[closestGridoint, 1, closestDate]
  CS_grid_valueQ2[track] <-
    grid.cloudtypes.timeseries[closestGridoint, 2, closestDate]
}

# add the temporary fields to tracks.global
tracks.global$CS_grid_distance <- CS_grid_distance
tracks.global$CS_grid_valueQ1 <- CS_grid_valueQ1
tracks.global$CS_grid_valueQ2 <- CS_grid_valueQ2
rm(CS_grid_distance, CS_grid_valueQ1, CS_grid_valueQ2)

# update tracks file
save(tracks.global,
     file = paste(dir_data, "tracks.global.RData", sep =
                    ""))
