
### South Carolina or "Southeast USA to Arctic" ###

# Read in R packages #
library(lubridate)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)

# Set Input and Output folder paths #
data_folder <- file.path("./02_data/REKN_gps/data")
output_folder <- file.path("./02_data/REKN_gps/output_temp")

raw_dat <- file.path(data_folder, "movebank_locations_20251210")

# Set keyword to use to pull desired datasets
key = "Southeast USA" # changed from South Carolina based on the name of the downloaded files for this area

# Pull location and reference data file names based on keyword
filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# Read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  dplyr::select(c(tag.id , animal.id, deploy.on.date, animal.life.stage, tag.model, animal.sex,
                  #deployment.comments # this column does not exist in the downloaded data
                  ,tag.manufacturer.name )) %>% 
  rename("animal.ring.id" = animal.id,
         #"animal.marker.id" = deployment.comments # this column does not exist in the downloaded data
         ) %>%
  mutate(study.site = "KIAWAH")

brep <- brep %>%
  mutate(animal.marker.id = '') # adding a blank column for animal.marker.id in order to be compatible in later joins


brep  <- brep [complete.cases(brep ), ]

brep <- brep %>%
  mutate(tag.local.identifier = tag.id)

# Read in location data
btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                #gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, argos.altitude, #algorithm.marked.outlier,
                tag.local.identifier)

# calculate time differences
bout <- bout  %>% mutate(date_time = ymd_hms(timestamp)) 

# merge these together and output 

all_dat <- left_join(bout, brep) %>%
  dplyr::mutate(argos.lc = as.character(argos.lc)) %>%
  dplyr::select(-individual.local.identifier, -tag.local.identifier)%>%
  mutate(tag.id = as.character(tag.id)) %>%
  filter(location.long >= -120,
         location.long <= -62) %>% 
  mutate(tag.model = "Sunbird Solar Argos") %>%
  filter(!is.na(location.long), 
         !is.na(location.lat)) %>%
  mutate(deploy.on.latitude = 32.53945, 
         deploy.on.longitude = -80.17069)%>%
  dplyr::filter(argos.lc != "Z")%>%
  dplyr::filter(argos.lc != "") %>%
  dplyr::select(-visible) |> 
  dplyr::mutate(animal.id = str_c("KIAWAH_", tag.id, "_2023"))


all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$tag.id), 1))%>%
  dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date))


# #save out file
clean_save = all_dat  %>% mutate(proj = "sthcarolina_arctic")
saveRDS(clean_save, file = file.path(output_folder, "rekn_sthcarolina_20251211.rds"))


# write out 
#clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path("output", "pt_sth_20240123.gpkg"), append = F)

