
## summarise the line types 

library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)
library(readr)


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")


# read in moveclass data 
pathraw <- st_read(file.path(raw_dat, "locations_raw_2025", "paths_2020_2025_movetype_20260128.gpkg"))


head(pathraw)
path <- pathraw |> 
  select(tag.id, from_id,  to_id, from_date_time, to_date_time,from_movement_final, to_movement_final)

st_write(path, file.path(raw_dat, "locations_raw_2025", "paths_2020_2025_movetype.gpkg"))
