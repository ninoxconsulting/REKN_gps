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


# read in reference data 
qref <- read_xlsx(file.path(raw_dat, "aurey_yves", "ReferenceQuebec_2020_2023_ArgosDeployment26Jan2024.xlsx"), 
                  .name_repair = "universal") %>%
  dplyr::mutate(tag.id = as.character(Tag.ID)) %>%
  dplyr::filter(!is.na(tag.id)) %>%
  dplyr::select(-animal.id, -...25, -deploy.on.timestamp) 



# read in reference data edddited to check the tracks
qref2 <-  read.csv(file.path(raw_dat, "aurey_yves", "movebank_ref_all_deployments.csv")) %>%
  dplyr:: filter(Project == "Red Knot rufa Migration Quebec") %>%
  dplyr::select(notes, track_data, Location, `deploy.on.timestamp`, ArgosID)%>%
  rename("tag.id" = ArgosID)%>% 
  filter(notes != "no_location_data")%>%
  filter(track_data  !=  "not_rekn" )%>%
  filter(track_data !=  "not_deployed")
  #dplyr::mutate(correct_date = as.Date(`Banding_or_recapture_Date`, origin = "1900-01-01") )
  
length(qref2$notes)
  
qref <- left_join(qref2, qref)


# download raw location data # note many have no lat longs
qbirds <- read.csv(file.path(raw_dat, "aurey_yves", "redknotcan_6687_QuebecRawLotek.csv"))

# since the initial dat compilation there has been more data added to movebank. we will check this also up to December 2023

 qmovebank <- read.csv(file.path(data_folder,"movebank_locations_20240706", "Red Knot rufa Migration Quebec (1).csv"))%>%
   dplyr::mutate(argos.lc = as.character(argos.lc))%>%
   dplyr::mutate(date_time = ymd_hms(timestamp))  %>%
   dplyr::mutate(year = year(date_time )) %>% 
   dplyr::mutate(tag.id = as.character(tag.local.identifier))


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
  mutate(#animal.id = str_c("MING_", tag.id, "_", year),
         tag.id = as.numeric(tag.id)) %>%
  dplyr::select(- UTC_Date, -UTC_Time, -day, -hour, -minute) %>% 
  filter(!is.na(location.long), 
         !is.na(location.lat))%>%
  mutate(tag.id = as.character(tag.id)) %>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z") 

allq <- bind_rows(qmovebank, qb)%>%
  dplyr::select(-visible, -event.id,-algorithm.marked.outlier)%>% 
  dplyr::select(-month)


locnos <- unique(allq$tag.id)
qrefnos <- unique(qref$tag.id)



missing_refs <- setdiff(locnos,qrefnos)
missing_locals <- setdiff(qrefnos,locnos)

# join the reference to location data
qout <- left_join(allq, qref, by = "tag.id")


qout <- qout  %>% 
  dplyr::mutate(pt_time = ymd_hms(timestamp)) |> 
  dplyr::mutate( deploy_date_time = ymd_hms(deploy.on.timestamp)) |> 
  mutate(pre_dep = ifelse(pt_time >= deploy_date_time, 1, 0 )) %>% 
  mutate(deploy.on.date = deploy_date_time)%>%
  filter(pre_dep == 1) |> 
  dplyr::select(-Tag.ID, -deploy.on.timestamp, -Location, -Argos_3, 
                -Argos_1, -old.tag.id, -  pt_time , - deploy_date_time,
                -pre_dep) |> 
dplyr::mutate(tag.mass = as.numeric(tag.mass))%>%
  dplyr::filter(track_data == "track")%>%
  dplyr::select(-track_data, -notes)

all_dat <- qout %>%
  mutate(id = seq(1, length(qout$tag.id), 1)) %>%
  dplyr::mutate(tag.mass = as.numeric(tag.mass))%>% 
  mutate(tag.model = case_when(
    tag.model == "Sunbird Lotek" ~ "Sunbird Solar Argos",
    tag.model == "lotek PinPoint 76" ~ "Lotek PinPoint GPS-Argos 75",
    #tag.model == "Loteck 5/PPT-10"  ~ "Lotek PinPoint GPS-Argos 75",
    TRUE ~ as.character(tag.model))) %>%
  rename("animal.comments" = comment.for.not.working) |> 
  mutate(attachment.type = deployment.comment) |> 
  dplyr::select(-lotek.crc.status.text, -mortality.status, -tag.voltage,
                -tag.local.identifier, -individual.taxon.canonical.name,
                -individual.local.identifier, -external.temperature, -capture.timestamp,
                -deployment.comment)%>%
  mutate(animal.id = str_c("MING_", tag.id, "_", year)) %>% 
  dplyr::select(-year, -study.name)
                

  # create and animal.id
#locations_idno_year_banded


# #save out file
clean_save = all_dat %>% mutate(proj = "Mingnan")
saveRDS(clean_save, file = file.path(output_folder, "rekn_mignon_raw_20240708.rds"))

