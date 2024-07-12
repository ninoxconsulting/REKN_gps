
#####################################################
# 2)  Johnson 2017 and 2018 

library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)



# set up raw data inputs 

data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")


raw_dat <- file.path(data_folder, "other_dataset")

filesoi <- list.files(raw_dat)


j17 <- read.csv(file.path(raw_dat, "johnson", "REKN_GPSprocessed_2017.csv")) %>% 
  dplyr::mutate(Lat = as.numeric(Lat), 
                Long = as.numeric(as.character(Long)),
                Long_new = as.numeric(Long_new), 
  )

#23
#43

j18 <- read.csv(file.path(raw_dat, "johnson", "REKN_GPSprocessed_2018.csv"))


jd <- bind_rows(j17, j18)%>%
  #   filter(CRC %in% c(NA , "OK")) %>%
  #   filter(LocType_new != "Bad") %>%
  rename("animal.id" = FlagID,
         "location.lat" = Lat,
         "location.long" = Long) %>%
  mutate(proj ="Johnson_GPS",
         data_type = LocType)

# Basic summary of individuals

jgps <- jd %>%
  mutate(arrive = mdy(Date)) %>%
  mutate(year = year(arrive), 
         month = month(arrive), 
         day = day(arrive)) |> 
  rename("animal.ring.id" = Band,
         "animal.mass" = Mass,
         "lotek.crc.status" = CRC, 
         "sensor.type"  = data_type,
         "gps.fix.type.raw" = Accuracy,
         "tag.id" = TagID,
         "animal.sex" = Sex,
         "animal.comments" = Note) %>%
  dplyr::select(-Cohort, -Sequence, -Site, -Period)%>%
  mutate(Time = case_when(
    is.na(Time) ~ "11:00:00",
    Time == "na" ~"11:00:00",
    TRUE ~ as.character(Time))) %>%
  mutate(Time2 = hms(Time),
         animal.ring.id = as.character(animal.ring.id)) %>%
  mutate(hour = hour(Time2), 
         minute= minute(Time2))%>%
  mutate(timestamp = str_c(as.character(arrive), " ", as.character(Time)))%>%
  mutate(date_time = ymd_hms(timestamp)) 


jgps <- jgps %>%
  mutate(sensor.type = case_when(
    sensor.type %in% c("Doppler", "Doppler ") ~ "argos-doppler-shift", 
    sensor.type == "GPS" ~ "gps", 
    sensor.type == "deploy" ~ "deploy", 
    sensor.type == "3D" ~ NA, 
    sensor.type == "A3" ~ NA, 
    sensor.type == "AB" ~ NA,
    sensor.type == "Resight"  ~ "resight", 
    .default = NA
  ))



jgps  <- jgps  |> 
  mutate(toremove = case_when(
    tag.id == 175067 & year == 2023 & month ==5 ~ 1,
    tag.id == 175071 & year == 2022 & month ==4 ~ 1,
    tag.id == 175077 & year == 2019 & month ==1 ~ 1,
    tag.id == 175068 & year == 2018 & month ==12 ~ 1,
    TRUE ~ NA))


head(jgps)

jgps <- jgps %>% 
  filter(is.na(toremove)) %>% 
  dplyr::select(-toremove) |> 
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75", 
         tag.manufacturer.name = "Lotek" )%>%
  mutate(tag.id = as.character(tag.id))%>%
  mutate(sensor.type = tolower(sensor.type)) %>% 
  mutate(deploy.on.latitude = 46.95371, 
         deploy.on.longitude = -124.043211, 
         study.site = "SandIs")


# needs to be fixed here 
deploy_dates <- jgps %>%
  dplyr::select(tag.id, date_time, LocType) %>%
  filter(LocType == "Deploy") %>% 
  mutate(deploy.on.date = date_time) %>% 
  dplyr::select(-LocType,-date_time)


jgps <- left_join(jgps , deploy_dates)%>%
  mutate(tag.id = as.character(tag.id))%>%
  mutate(sensor.type = tolower(sensor.type), 
         deploy.on.date = as.character(deploy.on.date))


#str(jgps)
jgps <- jgps |> 
  dplyr::select(-TagID2, -LocType_new, -Corr, -Time2, -Time, 
                -Long_new, -LocType_new, -LocType, - Date, -arrive, - month, -day, -hour, -minute, -year)%>% 
  filter(!is.na(location.long), 
         !is.na(location.lat))


all_dat <- jgps |> 
  mutate(date_time = ymd_hms(timestamp))|> 
  filter(location.lat < 90 ) |> 
  filter(location.lat> -90) 

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$tag.id), 1))



## Re edit the deploy date
deploy_dates <- all_dat %>%
  dplyr::select(tag.id, date_time) %>% 
  arrange(date_time,  by = tag.id) %>% 
  slice_min(date_time, by = tag.id) %>% 
  rename("deploy.on.date" = date_time) %>% 
  distinct()%>%
  mutate(deploy.on.date = as.character(deploy.on.date))


all_dat <- left_join(all_dat, deploy_dates)



summ <- all_dat %>% 
  group_by(tag.id) |> 
  count()

single <- summ %>% filter(n==1)%>% 
  pull(tag.id)

working_tags <- summ %>% filter(n>1)%>% 
  pull(tag.id)

## 8 tags have only 1 record and flaged as 

all_dat <- all_dat |> 
  filter(tag.id %in% working_tags )  %>%
  dplyr::mutate(animal.marker.id = animal.id) |> 
  dplyr::mutate(animal.id = str_c("SANDIS_", tag.id, "_", year(deploy.on.date)))%>%
  dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date)) 




#year(all_dat$deploy.on.date)

# #save out file
saveRDS(all_dat, file = file.path(output_folder, "rekn_john_20240708.rds"))


