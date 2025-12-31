
### South Carolina or "Southeast USA to Arctic" ###

# Read in R packages #
library(lubridate)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)

# to do - could add some more reference data - emailed Felicia to check if available. 


# gens directory: 
data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")


# Set Input and Output folder paths #
#data_folder <- file.path("./02_data/REKN_gps/data")
#output_folder <- file.path("./02_data/REKN_gps/output_temp")

raw_dat <- file.path(data_folder, "movebank_locations_20251229")

# Set keyword to use to pull desired datasets
key = "Southeast USA" # changed from South Carolina based on the name of the downloaded files for this area

# Pull location and reference data file names based on keyword
filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]


# Read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep |> 
  mutate("animal.ring.id" = animal.id) |> 
  mutate(animal.ring.id = ifelse(str_detect(animal.ring.id, "-"),animal.ring.id, NA)) |> 
  mutate(tag.local.identifier = tag.id) |> 
  select(-animal.nickname)|> 
  dplyr::mutate(deploy_date_time = ymd_hms(deploy.on.date))|> 
  dplyr::mutate(deploy_date = as_date(deploy_date_time)) 





# Read in location data
bout <- read.csv(file.path(raw_dat, filesoi))

# calculate time differences
bout <- bout  %>% mutate(date_time = ymd_hms(timestamp)) |> 
  select(-manually.marked.outlier)

# merge these together and output 

all_dat <- left_join(bout, brep) 

#TODO: might still need to review this, jsut commentign out for testing
# %>%
#   dplyr::mutate(argos.lc = as.character(argos.lc)) %>%
#   dplyr::select(-individual.local.identifier, -tag.local.identifier)%>%
#   mutate(tag.id = as.character(tag.id)) %>%
#   filter(location.long >= -120,
#          location.long <= -62) %>% 
#   mutate(tag.model = "Sunbird Solar Argos") %>%
#   filter(!is.na(location.long), 
#          !is.na(location.lat)) %>%
#   mutate(deploy.on.latitude = 32.53945, 
#          deploy.on.longitude = -80.17069)%>%
#   dplyr::filter(argos.lc != "Z")%>%
#   dplyr::filter(argos.lc != "") %>%
#   dplyr::select(-visible) |> 
#   dplyr::mutate(animal.id = str_c("KIAWAH_", tag.id, "_2023"))


all_dat <- all_dat %>%
  filter(!is.na(location.long)) 

# #save out file
clean_save = all_dat  %>% mutate(proj = "sthcarolina_arctic")
saveRDS(clean_save, file = file.path(output_folder, "rekn_sthcarolina_20251230.rds"))


# write out 
#clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(output_folder, "pt_sth_20251230.gpkg"), append = F)

