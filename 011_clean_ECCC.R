## ECCC tags

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(readr)
library(dplyr)

# gens directory: 

data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")

# TODO: 
# appears a number fo tags are missing reference data. Did not go through entire list but these are ones previously identified as
# missing deployment information (check main tracking sheet) and also some new ones that have tracks but not info. 
# reached out to theo (dec 29th so see if there is missing info available.)


# cams directory
#data_folder <- file.path("./02_data/REKN_gps/data") # "./" refers to the current working directory, with the subsequent folders below
#output_folder <- file.path("./02_data/REKN_gps/output_temp")


raw_dat <- file.path(data_folder, "movebank_locations_20251229")


# read in tracking sheet to fix categories of deployed and not deployed 

keyyy <- read_csv(file.path(data_folder, "movebank_reference", "movebank_ref_all_deployments.csv")) %>% # removed "ECCC_" prefix from ref_all csv based on file name
  dplyr:: filter(Project == "ECCC") %>%
  dplyr::select(track_data, ArgosID, Banding_or_recapture_Date) %>%
  rename("tag.id" = ArgosID) %>%
  dplyr::mutate(deploy.on.date = dmy(Banding_or_recapture_Date)) |> 
  dplyr::mutate(deploy.on.date.final = make_datetime(
         year = year(deploy.on.date), month = month(deploy.on.date), 
         day = day(deploy.on.date), hour = 09, min = 00, sec = 00 )) 

keyyy <- dplyr::select(keyyy, c(track_data, tag.id, deploy.on.date.final))|> 
    mutate(tag.id = as.numeric(tag.id))

## generate a list from previous ref data where there was no banding data. 
#keyyy_id_with_ref_2023 <- read_csv(file.path(data_folder, "movebank_reference", "movebank_ref_all_deployments.csv")) %>% # removed "ECCC_" prefix from ref_all csv based on file name
#  dplyr:: filter(Project == "ECCC") %>%
#  dplyr::select(track_data, ArgosID, Banding_or_recapture_Date) %>%
#  rename("tag.id" = ArgosID) |> 
#  dplyr::filter(is.na(Banding_or_recapture_Date)) |> 
#  mutate(tag.id = as.numeric(tag.id))



filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "ECCC"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]


# read in reference data # note this is incomplete

brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  mutate(tag.local.identifier = tag.id)%>% 
  filter(!is.na(tag.id))


# read in old reference and join this data 
previous_ref_dat <- file.path(data_folder, "compiled_REKN_data2023")
# read in the previous reference dataset
old_ref <- read.csv(file.path(previous_ref_dat, "reference_data_edited.csv")) |> 
  filter(proj == "ECCC")

# join old ref and new ref data together 
brep <- left_join(brep, old_ref) |> 
 select(where(~!all(is.na(.))))



# check how many tags in reference data 
#uref <- unique(brep$tag.id)
# 203 tags on reference dataset



# read in the location data 
btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp |> 
  mutate(date_time = ymd_hms(timestamp))

# check how many unique tags in location data
#uids <- sort(unique(bout$tag.local.identifier))
#setdiff (uids, uref)

# review the tags that have no reference data associated with them
all_dat <- left_join(bout, brep, by = 'tag.local.identifier' )%>%
  filter(!is.na(location.long), 
         !is.na(location.lat))
# 
# sum_all_dat <- all_dat |> 
#   group_by(tag.id) |> 
#   filter(deploy.on.date == "") |> 
#   summarise(count = n())
# 
# sum_all_dat <- left_join(keyyy_id_with_ref_2023, sum_all_dat)
# 
# clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path(output_folder, "rekn_eccc_test.gpkg"), append = F)
# 


#head(all_dat)

# all_dat <- all_dat 
#   mutate(tag.id = as.character(tag.id)) %>%
#   filter() %>% 
#   dplyr::mutate(lotek.crc.status = case_when(
#     is.na(lotek.crc.status) ~ "",
#     TRUE ~ as.character(lotek.crc.status)))%>%
#   dplyr::filter(lotek.crc.status != "E")%>%
#   dplyr::mutate(argos.lc = case_when(
#     is.na(argos.lc) ~ "",
#     TRUE ~ as.character(argos.lc)))%>%
#   dplyr::filter(argos.lc != "Z")%>%
#   filter(!is.na(location.long), 
#          !is.na(location.lat))%>%
#   mutate(tag.model = case_when(
#     tag.model == "gps-pinpoint" ~ "Lotek PinPoint GPS-Argos 75", 
#     tag.model == "PinPoint 75" ~ "Lotek PinPoint GPS-Argos 75", 
#     tag.model ==  "Sunbird" ~ "Sunbird Solar Argos",
#     tag.model ==  "sunbird" ~ "Sunbird Solar Argos",
#     tag.model == "microwave telemetry"~ "Solar 2-g PTT",
#     .default = as.character(tag.model))) %>%
#   mutate(tag.manufacturer.name = case_when(
#     tag.model == "Solar 2-g PTT" ~ "Microwave Telemetry", 
#     TRUE ~ as.character(tag.manufacturer.name)))


#unique(all_dat$animal.id)

#"FORTESCU" "KIMBLESO" "MOORES"   "EASTPIT"  "PEIXE"    "NBRIG"   
# 
# all_dat <- all_dat %>% 
#   dplyr::mutate(deploy.on.latitude = case_when(
#     study.site == "FORTESCU" ~ 39.0322,
#     study.site == "KIMBLESO" ~ 39.20075,
#     study.site == "MOORES" ~ 39.20075,
#     study.site == "EASTPIT" ~ 39.1958,
#     study.site == "PEIXE" ~ -31.401,
#     study.site == "NBRIG" ~ 39.436)) %>%
#   dplyr::mutate(deploy.on.longitude = case_when(
#     study.site == "FORTESCU" ~ -74.7948,
#     study.site == "KIMBLESO" ~ -75.0255,
#     study.site == "MOORES" ~ -75.0255,
#     study.site == "EASTPIT" ~ -75.02589,
#     study.site == "PEIXE" ~  -51.066,
#     study.site == "NBRIG" ~ -74.346))

summ <- all_dat %>% 
  group_by(tag.id) |> 
  count()

#unique(all_dat$tag.id)


# filter by the key of track type. 
track <- keyyy %>% filter(track_data == "track") %>% pull(tag.id)

all_dat <- all_dat  |> 
  mutate(include = ifelse(tag.id %in% track, "y", "n"))

all_dat <- left_join(all_dat, keyyy)%>% 
  dplyr::select(-deploy.on.date)%>% 
  rename("deploy.on.date1" = deploy.on.date.final)%>%
  dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date1)) 

# 
# checks <- all_dat %>% dplyr::filter(is.na(deploy.on.date)) |> 
#   pull(tag.id)
# 
#   unique(checks)
# #

#length(unique(out$tag.id))
#checks <- all_dat %>% dplyr::filter(animal.id == "") |> 
#  pull(tag.id)
#
#unique(checks)

# 11 tags had no refernce information 

# #save out file
clean_save = all_dat %>% mutate(proj = "ECCC")
saveRDS(clean_save, file = file.path(output_folder, "rekn_eccc_20251230.rds"))

clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(output_folder, "rekn_eccc_test.gpkg"), append = F)
# 