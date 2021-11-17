# germanRADARanalysis_precipitationScaling

This repository is part of the germanRADARanalysis project. Here, we first connect the events to co-located dew point temperature records from automatic weather stations in Germany. Then we analyse the scaling of event peak intensity with dew point temperature. Lastly, we investigate how the scaling depends on the different precipitation type. More details about the project can be found on [my personal website](https://lochbihler.nl/?page_id=302).

# Required data
To successfully run the analysis you will need two data sets.
1. [the event catalog](https://drive.google.com/file/d/1KhyuW35YjlhtV5UKppolyEU9SkonG0ib/view?usp=sharing)
2. [the gridded precipitation type data set](https://drive.google.com/file/d/1LzOh5TYaBKpGl0D7n-ggL2C0gECN5Xgk/view?usp=sharing)

## Steps to run

Open the RStudio project file in RStudio or use your favorite way of running R scripts.

1. run or source the src/setup_working_environment.R script
2. run the src/prepare_station_data_cloud_type.R script to download the DWD data and create a tailored station data set
3. finally run the src/create_gridded_CS_type_dataset.R script to produce the gridded precipitation type data set
