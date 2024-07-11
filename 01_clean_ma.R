
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
key = "Red Knot Fall MA Migration Study"

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
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier, gps.fix.type.raw, 
                import.marked.outlier,
                height.above.ellipsoid) %>%
  mutate(date_time = ymd_hms(timestamp))

all_dat <- left_join(bout, brep )%>%
  mutate(argos.lc = as.character(argos.lc)) %>%
  rename("animal.marker.id" = animal.id)%>%
  dplyr::select(-attachment.type, -animal.taxon.detail)%>%
  mutate(tag.id = as.character(tag.id))%>%
  mutate(animal.id = deployment.id)

#head(all_dat)

all_dat <- all_dat %>%
  mutate(tag.id = as.character(tag.id)) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z")%>%
  filter(!is.na(location.long), 
         !is.na(location.lat)) %>%
  mutate(deploy.on.latitude = 39.20075, 
         deploy.on.longitude = -75.0255,
         study.site = "THOMP")

all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, 
                -deploy.off.date, - deployment.id, -animal.comments, -visible)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$tag.id), 1))%>%
  dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date))


# #save out file
clean_save = all_dat %>% mutate(proj = "ma_migration")
saveRDS(clean_save, file = file.path(output_folder, "rekn_ma_mig_20240708.rds"))


#clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path("output_folder", "pt_MAmig_20240129.gpkg"), append = F)


