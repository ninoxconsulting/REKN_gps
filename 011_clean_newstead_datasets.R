
#########################################################################
### 3)  (Newstead) or "Gulf to Arctic" ###

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


#raw_dat <- file.path(data_folder, "other_dataset") # changed this to point to the same raw data location as others
raw_dat <- file.path(data_folder, "movebank_locations_20251210")

#filesoi <- list.files(raw_dat) # no longer reading all files in the folder, see addition of keyword below

# Set keyword to use to pull desired datasets
key = "Gulf to Arctic" # added keyword variable similar to sth carolina script

# Pull location and reference data file names based on keyword
filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# Read in location data
ndatr <- read.csv(file.path(raw_dat, filesoi)) # replaced read in of location data for CSV, downloaded data no longer xlsx format
  #read_xlsx(file.path(raw_dat, "Newstead","Red Knot Gulf to Arctic.xlsx"), # removed "CBBEP_Newstead_" file prefix for reading file
            #.name_repair = "universal")


# Read in reference data
nref<- read.csv(file.path(raw_dat, filesoi_ref)) 

  #read_xlsx(file.path(raw_dat, "Newstead","CBBEP_Newstead_Red Knot Gulf to Arctic.xlsx"), 
            #sheet = 'Capture Data', .name_repair = "universal") %>% 
  #mutate(animal.id = individual.local.identifier) %>% # this is no longer needed as 'animal.id' column name exists in raw data
  #rename(#"animal.sex" = Sex, # no longer needed as 'animal.sex' column name exists in raw data
         #"animal.life.stage" = Age, # do not see either column name present in data, may need to add as blank column
         #"animal.ring.id" = Band.number) # no longer needed as 'animal.ring.id' column name exists in raw data

# **had to make several changes here due to differing column names / absence of previously existing columns in the new download. commented out most of the pre-existing stuff for reference and added new lines as needed for ref data
nref <- nref %>%
  mutate(#deploy.on.date = Capture.date, # not needed, deploy.on.date already exists with proper name
         #deploy.on.measurements = str_c("{culmenInMillimeters:",Culmen, ",theadInMilimeters:", TotalHead,",wingInMillimeters:",Wing,"}"),
         deploy.on.measurements = NA, # assign this column to be null since there is no existing data for this in the new download.
         #animal.mass = Weight,
         animal.mass = NA, # assign this column to be null since there is no existing data for this in the new download.
         animal.life.stage = NA, # set this to null as it doesn't appear to exist in this data cut
         study.site = case_when( # replacing "Site ==" with CONTAINS for deployment.id column as it appears to contain the study site for this download. However most rows for this are blank (will set to NA)
           str_detect(deployment.id, "Elmers Island") ~ "elmer",
           str_detect(deployment.id, "Grand Isle") ~ "grandis",
           str_detect(deployment.id, "Padre Island National Seashore") ~ "padre",
           str_detect(deployment.id, "Fourchon Beach/Caminada") ~ "fourc",
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

# ***need to modify this section next!***
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
  mutate(id = seq(1, length(ndat_out$visible), 1))%>%
  dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date))

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