#02_clean_quebec file


# import and clean raw data

# Compile all the other pieces of data - not currently on movebank format 

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)
library(geosphere)


# set up raw data inputs 

data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")


raw_dat <- file.path(data_folder, "other_dataset")

filesoi <- list.files(raw_dat)


#####################################################
# 1) Yves_Aubres 

# download raw location data # note many have no lat longs
qbirds <- read.csv(file.path(raw_dat, "aurey_yves", "redknotcan_6687_QuebecRawLotek.csv"))

# read in reference data 
qref <- read_xlsx(file.path(raw_dat, "aurey_yves", "ReferenceQuebec_2020_2023_ArgosDeployment26Jan2024.xlsx"), 
                   .name_repair = "universal") %>%
  mutate(tag.id = as.numeric(Tag.ID)) %>%
  filter(!is.na(tag.id)) %>%
  dplyr::select(-animal.id, -...23, -deploy.on.timestamp) 


# select important cols
qb <- qbirds |> 
  dplyr::select(Tag_ID, UTC_Date,  UTC_Time,  Latitude,  Longitude,  Location.Quality)


# calculate time differences
qb <- qb %>%
  mutate(date_time = ymd(UTC_Date),
         timestamp = str_c(UTC_Date, " ", UTC_Time))%>%
  mutate(date_time = ymd_hms(timestamp )) %>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         time = hms(UTC_Time),
         hour = hour(time),
         minute = minute(time)) %>% 
  dplyr::select(-time) %>%
  mutate(Tag_ID = gsub("6687:", "", Tag_ID)) %>%
  rename("tag.id" = Tag_ID,
         "location.lat" = Latitude,
         "location.long" = Longitude ,
         "argos.lc" = Location.Quality) %>%
  mutate(animal.id = str_c("MING_", tag.id ),
         tag.id = as.numeric(tag.id)) %>%
  dplyr::select(- UTC_Date, -UTC_Time, -day, -hour, -minute)


qout <- left_join(qb, qref, by = "tag.id")


# filter out Quebec ids that need to be updated 
qb <- qout |> 
  mutate(toremove = case_when(
    tag.id == 232347 ~ 1, # this is a upland sandpiper
    tag.id == 213948 ~ 1, 
    tag.id == 224455 ~ 1,
    tag.id == 232341 ~ 1, 
    tag.id == 232342 ~ 1, 
    tag.id == 232341 ~ 1, 
    tag.id == 232342 ~ 1, 
    tag.id == 232344 ~ 1, 
    tag.id == 239414 ~ 1,
    tag.id == 242699 ~ 1,
    tag.id == 239423 & month == 7 ~ 1,
    tag.id == 239420 & month == 7 ~ 1,
    tag.id == 232346  ~1, # this is a upland sandpiper
    tag.id == 239425 & month == 8 ~1,
    tag.id == 229370 & year == 2022 & month ==6 ~ 1,
    tag.id == 239414 & year == 2023 & month ==2 ~ 1,
    tag.id == 239413 & year == 2023 & month ==2 ~ 1,
    tag.id == 239409 & year == 2023 & month ==2 ~ 1,
    tag.id == 239408 & year == 2023 & month ==2 ~ 1,
    tag.id == 239412 & year == 2023 & month ==2 ~ 1,
    tag.id == 239411 & year == 2023 & month ==7 ~ 1,
    tag.id == 232345 & year == 2022 & month ==5~ 1,
    tag.id == 232351 & year == 2022 & month ==5~ 1,
    tag.id == 232352 & year == 2022 & month ==5~ 1,
    tag.id == 232353 & year == 2022 & month ==5~ 1,
    tag.id == 232350 & year == 2022 & month ==5~ 1,
    tag.id == 232348 & year == 2022 & month ==5~ 1,
    tag.id == 239408 & year == 2023 & month ==7~ 1,
    tag.id == 239409 & year == 2023 & month ==7~ 1,
    tag.id == 239410 & year == 2023 & month ==7~ 1,
    tag.id == 239421 & year == 2023 & month ==7~ 1,
    tag.id == 239412 & year == 2023 & month ==7~ 1,
    tag.id == 239413 & year == 2023 & month ==7~ 1,
    tag.id == 239415 & year == 2023 & month ==7~ 1,
    tag.id == 239416 & year == 2023 & month ==7~ 1,
    tag.id == 239417 & year == 2023 & month ==7~ 1,
    tag.id == 239418 & year == 2023 & month ==7~ 1,
    tag.id == 239422 & year == 2023 & month ==7~ 1,
    tag.id == 239419 & year == 2023 & month ==7~ 1,
    tag.id == 239424 & year == 2023 & month ==7~ 1,
    tag.id == 239424 & year == 2023 & month ==8~ 1,
    tag.id == 239425 & year == 2023 & month ==7~ 1,
    tag.id == 229364 & year == 2022 & month ==6~ 1,
    tag.id == 229363 & year == 2022 & month ==6~ 1,
    tag.id == 229366 & year == 2022 & month ==6~ 1,
    tag.id == 229368 & year == 2022 & month ==6~ 1,
    tag.id == 229369 & year == 2022 & month ==6~ 1,
    year == 2022 & month ==6~ 1,
    location.long > 160 ~ 1, 
    TRUE ~ NA))


#head(qb)

qb <- qb %>% 
  filter(is.na(toremove)) %>% 
  dplyr::select(-toremove) 

# remove the ids with no lat/long or invalid locations 
qb <- qb %>% 
  filter(!is.na(location.long), 
         !is.na(location.lat))%>%
  mutate(tag.id = as.character(tag.id)) %>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z") 


all_dat <- qb  %>%
  mutate(id = seq(1, length(qb$tag.id), 1)) %>%
  dplyr::select(-year, -month) %>%
  filter(!is.na(location.lat))%>%
  dplyr::mutate(tag.mass = as.numeric(tag.mass))%>% 
  mutate(tag.model = case_when(
    tag.model == "lotek PinPoint 75" ~ "Lotek PinPoint GPS-Argos 75",
    tag.model == "Sunbird Lotek" ~ "Sunbird Solar Argos",
    #tag.model == "PTT solarLotek" ~ "Solar 2-g PTT",
    TRUE ~ as.character(tag.model))) %>%
  dplyr::select(-old.tag.id, -Tag.ID)%>%
  filter(animal.taxon == "Calidris canutus") |> 
  rename("animal.comments" = comment.for.not.working)


# #save out file
clean_save = all_dat %>% mutate(proj = "Mingnan")
saveRDS(clean_save, file = file.path(output_folder, "rekn_mignon_raw_20240708.rds"))

