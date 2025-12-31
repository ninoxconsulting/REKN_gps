
#########################################################################
### 3)  (Newstead) or "Gulf to Arctic" ###

# Read in R packages #
library(lubridate)
library(sf)
library(adehabitatHR)
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


# note created a new subfolder and moved a copy of the final outputs here so we can use it in the code
previous_ref_dat <- file.path(data_folder, "compiled_REKN_data2023")
# read in the previous reference dataset
old_ref <- read.csv(file.path(previous_ref_dat, "reference_data_edited.csv")) |> 
  filter(proj == "Newstead")

old_ref <- old_ref |> 
  select(-animal.taxon, -animal.id , -tag.serial.no, -tag.manufacturer.name, -tag.model, -animal.ring.id, - deploy.on.date, -X) |> 
  select(where(~!all(is.na(.))))





# Set keyword to use to pull desired datasets
key = "Gulf to Arctic" # added keyword variable similar to sth carolina script

# Pull location and reference data file names based on keyword
filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# Read in location data
ndatr <- read.csv(file.path(raw_dat, filesoi)) |> 
  mutate(tag.id = tag.local.identifier)



# Read in reference data
nref <- read.csv(file.path(raw_dat, filesoi_ref)) |> 
  mutate(animal.ring.id = ifelse(!animal.marker.id == "", animal.marker.id, animal.ring.id)) |> 
  select(-animal.marker.id)


rall <- left_join(nref, old_ref, by = c("tag.id")) |> 
  mutate(animal.sex = ifelse(animal.sex.x  == "" & animal.sex.y  == "U", "u", animal.sex.x)) |> 
  select(-animal.sex.x, -animal.sex.y) |> 
  mutate(proj ="Newstead") |> 
  filter(animal.id != "")

 
# 
# # check which cols are appropraite to add and adjust this line as needed
#   dplyr::select(any_of(c("study.site", "animal.id", "deploy.on.date", "deploy.on.measurements",  "deploy.on.latitude",      
#                 "deploy.on.longitude" ,"animal.sex" , "animal.ring.id",  "animal.life.stage" ))
# ndat <- ndatr %>%
#   #filter(`lotek.crc.status.text` != "OK(corrected)")  %>%
#   rename("animal.id" = individual.local.identifier) |> 
#   mutate(proj ="Newstead",
#          import.marked.outlier = as.character(import.marked.outlier),
#          visible = as.character(visible)) %>%
#   mutate(date_time = timestamp, 
#          tag.id = tag.local.identifier) 
#   #dplyr::select( -event.id, -study.name, -individual.taxon.canonical.name) 



## Join the Location data to the Reference data ##
ndatr <- ndatr |> 
  filter(tag.local.identifier %in% rall$tag.id)

nout <- left_join(ndatr, rall)

nout <- nout |> 
  dplyr::mutate(deploy_date_time = ymd_hms(deploy.on.date))|> 
  dplyr::mutate(deploy_date = as_date(deploy_date_time)) 

#tt <- nout |> 
#  select(tag.id,deploy.on.date, deploy_date_time,deploy_date)|> 
#  unique()


## Perform final data cleaning for output ##

all_dat <- nout %>%
  filter(!is.na(location.long)) #|> 
  #mutate(id = seq(1, length(nout$visible), 1)) %>%
  #dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date))
# 
# # write out 
# clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path(output_folder, "pt_news_test.gpkg"), append = F)
# 
# 
# summ <- all_dat  %>% 
#   group_by(tag.id) |> 
#   count()



# #save out file
saveRDS(all_dat, file = file.path(output_folder, "rekn_newstead_20251230.rds"))


# write out 
#clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(output_folder, "pt_news_test.gpkg"), append = F)


# 