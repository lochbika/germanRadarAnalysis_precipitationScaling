# germanRadarAnalysis_precipitationScaling

This repository is part of the germanRADARanalysis project. Here, we first connect the events to co-located dew point temperature records from automatic weather stations in Germany. Then we analyse the scaling of event peak intensity with dew point temperature. Lastly, we investigate how the scaling depends on the different precipitation type. More details about the project can be found on [my personal website](https://lochbihler.nl/?page_id=302).

## Required data
To successfully run the analysis you will need two data sets.
1. [the event catalog](https://drive.google.com/file/d/1KhyuW35YjlhtV5UKppolyEU9SkonG0ib/view?usp=sharing)
2. [the gridded precipitation type data set](https://drive.google.com/file/d/1LzOh5TYaBKpGl0D7n-ggL2C0gECN5Xgk/view?usp=sharing)

Extract both data sets into a subfolder named data.

## How to run the R scripts
This repository includes a RStudio project file. Of course, you can still use other IDEs to run the code.
Scripts are in src/, data in data/ and plots will be saved in the plots/ directory.
Make sure to adjust your working directory in the src/setup_working_environment.R script.

Always run the src/setup_working_environment.R script at least once in each new R session before executing any other code.

# Connect events to dew point and precipitation type data

Open the RStudio project file in RStudio or use your favorite way of running R scripts.

1. run or source the src/setup_working_environment.R script
2. run the src/prepare_station_data_dew_point.R script to download the DWD data and create a tailored station data set
3. run the src/connect_tracks_with_dewpoint.R script to connect each track with a co-located dew point record (3 hours prior to the event)
4. finally, run the src/connect_tracks_with_gridded_precipitationtype.R. This will add the precipitation type criteria to the tracks database.

# The final data set
The final data set can be downloaded from [here.](https://drive.google.com/file/d/1it0pPeHJFlsJMPNB52cfC6r1x-AkZ0TU/view?usp=sharing)
