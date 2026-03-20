

## Compare teh geolocator data to GPS data 


####################################################################################
library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
#library(gganimate)
#library(ggspatial)

#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final/draft_outputs_2026")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures_2026")


geo <- read_csv("C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\complete\\2021_NWRC\\04_deliverables\\data\\rekn_rufa_location_estimates_final.csv")
geo <- geo |> 
  mutate(X = as.numeric(as.character("location lat"))) |> 
  filter(!is.na("location lat")) |> 
  filter(!is.na("location long")) 

geosf <- st_as_sf(geo,  coords = c("location lat", "location long"), crs = 4326)
