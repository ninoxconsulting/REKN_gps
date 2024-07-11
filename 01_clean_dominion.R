#clean_dominion


## Summary of  Dominion data set 
library(lubridate)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")

raw_dat <- file.path(data_folder, "movebank_locations_20231219")

# data_set3 : spring migration 
key = "Red Knot - CVOW"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))

brep <- brep %>%
  mutate(tag.local.identifier = tag.id)

btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                argos.altitude,  
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier) %>%
  mutate(date_time = ymd_hms(timestamp))

# merge these together and output 
all_dat <- left_join(bout, brep )

all_dat <- all_dat %>%
  filter(!is.na(location.long)) %>%
  filter(!is.na(location.lat))%>%
  mutate(animal.marker.id = animal.id)%>%
  dplyr::select(-deployment.id)%>%
  dplyr::filter(argos.lc != "Z")


all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, -animal.taxon.detail,
                -deploy.off.date) %>% 
  mutate(animal.id = str_c("dom_",  tag.id , "_", year(deploy.on.date)))%>%
  mutate(study.site = case_when(
    deploy.on.person == "Loring" ~ "MONOM", 
    deploy.on.person == "Feigin" ~ "NBRIG", 
    .default = as.character(NA)))%>%
  mutate(tag.id = as.character(tag.id)) 


all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))


# #save out file
clean_save = all_dat %>% mutate(proj = "dominion")
saveRDS(clean_save, file = file.path(output_folder, "rekn_dominion_20240708.rds"))

#clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path("output", "pt_dom_20240129.gpkg"), append = F)


