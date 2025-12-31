
# Read in R packages #
library(lubridate)
library(sf)
#library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


# gens directory: 
data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")

# Set Input and Output folder paths #
#data_folder <- file.path("./02_data/REKN_gps/data")
#output_folder <- file.path("./02_data/REKN_gps/output_temp")


#raw_dat <- file.path(data_folder, "other_dataset") # changed this to point to the same raw data location as others
raw_dat <- file.path(data_folder, "movebank_locations_20251229")

# Set keyword to use to pull desired datasets
key = "Florida" # added keyword variable similar to sth carolina script

# Pull location and reference data file names based on keyword
filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# Read in location data
ndatr <- read.csv(file.path(raw_dat, filesoi)) |> 
  mutate(tag.id = tag.local.identifier)  
  

# Read in reference data
nref <- read.csv(file.path(raw_dat, filesoi_ref)) |> 
  mutate(animal.taxon = "Calidris canutus") |> 
 #select(-animal.taxon.detail) |> 
  mutate(animal.id = deployment.id) |> 
  dplyr::mutate(deploy_date_time = ymd_hms(deploy.on.date))|> 
  dplyr::mutate(deploy_date = as_date(deploy_date_time)) 


all_dat <- left_join(ndatr, nref) |> 
  filter(!is.na(location.long))


# write out 
#clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(output_folder, "pt_secondyr_test.gpkg"), append = F)

# #save out file
clean_save = all_dat  |>  mutate(proj = "second_yr")
saveRDS(clean_save, file = file.path(output_folder, "rekn_secondyr_20251230.rds"))



