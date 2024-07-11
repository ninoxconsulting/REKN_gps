## Summary of Ocean Winds

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")


raw_dat <- file.path(data_folder, "movebank_locations_20231219")

filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "Red Knot Fall Migration"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]


# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  mutate(tag.local.identifier = tag.id)

#length(brep$tag.id) # 61 tags 

# read in track data 

btemp <- read.csv(file.path(raw_dat, filesoi))

bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier,
                height.above.ellipsoid)%>%
  mutate(date_time = ymd_hms(timestamp))


all_dat <- left_join(bout, brep )

all_dat <- all_dat |> 
  mutate(tag.id = as.character(tag.id)) %>%
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75", 
         tag.manufacturer.name="Lotek") %>%
  filter(!is.na(location.long), 
         !is.na(location.lat)) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z")


all_dat <- all_dat %>% 
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site == "TWOMILE" ~ 39.0277,
    study.site == "STOHARB" ~ 39.0277,
    study.site == "AVALON" ~ 39.0979,
    study.site == "NBRIG" ~ 39.436)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site == "TWOMILE" ~ -74.7808,
    study.site == "STOHARB" ~ -74.7808,
    study.site == "AVALON" ~ -74.7138,
    study.site == "NBRIG" ~ -74.346))


head(all_dat)

all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, 
                -deploy.off.date)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))%>%
 dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date))

#save out file
clean_save = all_dat %>% mutate(proj = "OceanWinds")
saveRDS(clean_save, file = file.path(output_folder, "rekn_ocean_20240708.rds"))

#clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(manual_edits, "rekn_oceanwinds_20240123.gpkg"), append = F)




