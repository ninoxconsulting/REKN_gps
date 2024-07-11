
#########################################################################
# 3)  (Newstead)

library(lubridate)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")


raw_dat <- file.path(data_folder, "other_dataset")

filesoi <- list.files(raw_dat)



ndatr <- read_xlsx(file.path(raw_dat, "Newstead","CBBEP_Newstead_Red Knot Gulf to Arctic.xlsx"), 
                   .name_repair = "universal")

nref<- read_xlsx(file.path(raw_dat, "Newstead","CBBEP_Newstead_Red Knot Gulf to Arctic.xlsx"), 
                 sheet = 'Capture Data', .name_repair = "universal") %>% 
  mutate(animal.id = individual.local.identifier) %>%
  rename("animal.sex" = Sex, 
         "animal.life.stage" = Age, 
         "animal.ring.id" = Band.number) 

nref <- nref %>%
  mutate(deploy.on.date = Capture.date,
         deploy.on.measurements = str_c("{culmenInMillimeters:",Culmen, ",theadInMilimeters:", TotalHead,",wingInMillimeters:",Wing,"}"),
         animal.mass = Weight,
         study.site = case_when(
           Site == "Elmers Island" ~ "elmer",
           Site == "Grand Isle" ~ "grandis",
           Site == "Padre Island National Seashore - Southbeach" ~ "padre",
           Site == "Fourchon Beach/Caminada" ~ "fourc",
         ))%>% 
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site == "elmer" ~ 29.1774,
    study.site == "grandis" ~ 29.2451,
    study.site == "padre" ~ 27.063,
    study.site == "fourc" ~ 29.1)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site == "elmer" ~ -90.0724,
    study.site == "grandis" ~ -89.9767,
    study.site == "padre" ~ -97.415,
    study.site == "fourc" ~ -90.226))%>%
  dplyr::select("study.site", "animal.id", "deploy.on.date", "deploy.on.measurements",  "deploy.on.latitude",      
                "deploy.on.longitude" ,"animal.sex" , "animal.ring.id",  "animal.life.stage"      )


ndat <- ndatr %>%
  #filter(`lotek.crc.status.text` != "OK(corrected)")  %>%
  rename("animal.id" = individual.local.identifier,
         "location.lat" = location.lat,
         "location.long" = location.long)%>%
  mutate(proj ="Newstead",
         data_type = "GPS", 
         import.marked.outlier = as.character(import.marked.outlier),
         visible = as.character(visible)) %>%
  mutate(arrive = ymd_hms(Date)) %>%
  mutate(year = year(arrive)) %>%
  mutate(month = month(arrive)) %>%
  mutate(day = day(arrive)) %>%
  mutate(hour = hour(arrive),
         minute = minute(arrive)) %>% 
  dplyr::select(-Date) %>%
  mutate(timestamp = as.character(timestamp)) %>% 
  mutate(date_time = arrive, 
         tag.id = tag.local.identifier)%>% 
  dplyr::select( -event.id, -arrive, -study.name, -individual.taxon.canonical.name,-lotek.crc.status.text) %>%
  mutate(tag.id = as.character(tag.id))

ndat <- left_join(ndat, nref)


ndat3v <- read.csv(file.path(raw_dat, "Newstead","3 V.csv"))

ndat3v  <- ndat3v   %>%
  mutate(date_time = ymd_hms(timestamp))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time))%>% 
  dplyr::select(-utm.northing, -utm.easting ,  -study.timezone,  -mortality.status, tag.voltage,
                -individual.taxon.canonical.name,   -event.id,   -study.local.timestamp,
                -external.temperature, -study.name,  -tag.voltage, -utm.zone, -lotek.crc.status.text ) %>%
  mutate(proj = "Newstead") %>%
  rename("animal.id" = individual.local.identifier)%>%
  mutate(tag.id = tag.local.identifier) %>%
  mutate(tag.id = as.character(tag.id))%>%
  #mutate("date_time" = timestamp)%>%  
  mutate(timestamp = as.character(timestamp))%>%
  mutate(deploy.on.latitude = 27.063, 
         deploy.on.longitude = -97.415 , 
         study.site = "Padre") 

aa <- ndat3v %>%
  dplyr::select(date_time, "animal.id" )%>%
  slice_min(date_time) %>% 
  distinct(date_time)%>%
  pull()

ndat3v  <- ndat3v   %>%
  mutate(deploy.on.date = aa)


ndat_out <- bind_rows(ndat, ndat3v) %>%
  dplyr::select(-height.above.ellipsoid, -data_type, -tag.local.identifier, -import.marked.outlier, 
                -year, -month, -day, -hour, -minute)%>%
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75", 
         tag.manufacturer.name = "Lotek", 
         deploy.on.date = as.character(deploy.on.date),
         animal.ring.id = as.character(animal.ring.id))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time))%>%
  filter(!is.na(location.long)) %>%
  filter(!is.na(location.lat))%>%
  filter(year < 2025)


all_dat <- ndat_out %>%
  mutate(id = seq(1, length(ndat_out$visible), 1))

all_dat <- all_dat %>%
  dplyr::select(-year, -month, -day, - minute, -hour)

summ <- all_dat  %>% 
  group_by(tag.id) |> 
  count()



# #save out file
saveRDS(all_dat, file = file.path(output_folder, "rekn_newstead_20240708.rds"))


# write out 
#clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(output_folder, "pt_news_test.gpkg"), append = F)


# 