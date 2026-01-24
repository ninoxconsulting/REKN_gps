# create stopover/ migration classifications 

library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")



# read in the ref data
ref <- read_csv(file.path(final_dat, "reference_data_raw_2020_2025_edited.csv"))

# note this is only 2023-2025 data 
loc_all <- read_csv(file.path(final_dat, "location_data_raw_2025.csv"))

length(loc_all$proj)

summ_tag <- loc_all |> 
  group_by(proj, tag.id)%>%
  count()

summ_proj <- loc_all |> 
  group_by(proj)%>%
  count()

# split out the basic information 

loc <- loc_all |> 
  dplyr::select(  -"...1",-"argos.iq" ,  ,-"argos.lat1" ,  -"argos.gdop"  ,-"argos.best.level",                 
                  -"argos.lat2" , -"argos.lon1"  ,   -"argos.lon2",  -"argos.calcul.freq" , -"argos.error.radius" ,                    
                 -"argos.nb.mes", -"argos.nb.mes.120", - "argos.nopc", -"import.marked.outlier" ,                    
                 -"argos.orientation", -"argos.pass.duration",  -"argos.semi.major" ,             
                 -  "argos.semi.minor", -"argos.sensor.1" ,  -"argos.sensor.2" ,-"argos.sat.id" ,  -"external.temperature",
                 -"mortality.status",-"argos.altitude",- "sensor.type",-"argos.transmission.timestamp",
                  -"argos.sensor.3", - "argos.sensor.4"  , -"argos.valid.location.algorithm", -"height.above.ellipsoid") 
    


# keep this to add back to the full dataset later... 




# general criteria 

## 1)  Stopover
# A location where a bird stops for ≥1days and successive location estimates are within 25km. 

# determine the order for each location per tag.id, 
# add the deployment (first location)'
# add stopover

loc <- loc %>% 
  group_by(tag.id) |> 
  arrange(date_time) |> 
  dplyr::mutate(tag.id.order = row_number())%>% 
  ungroup() |> 
  dplyr::mutate(stopover = ifelse(is.na(location.lat_prior), "deployment", NA)) |> 
  dplyr::mutate(stopover = case_when(
     gcd_m >= 25000 & diff >24 ~ "stopover"
  )) 
         
#diff = difference in hours
 

## 2) breeding areas 

#  - above 65 degrees lat 
#  - between months of XXX 
#  - (most northerly or longest )

loc <- loc %>% 
  mutate(movement_temp = case_when(
    month %in% c(6,7) & location.lat > 61  & stopover == "stopover" ~ "breeding"
  ))

  

## wintering grounds 

# Wintering locations were defined as any location used during December-January,
# or locations occurring further south than those recorded in december to January 
# and any locations within a certain distance of the December-January locations 
# (500 km for birds wintering in North or Central America, 750 km for birds wintering 
# in South America). (willow Pers comm.)) to be consistent with other shorebird analysis. 

# - Dec and January 

loc <- loc %>% 
  dplyr::mutate(movement_temp = case_when(
    month %in% c(12,1,2) ~ "wintering",
    month %in% c(10,9,8) ~ "south_migration",
    month %in% c(4,5) ~ "north_migration",
    .default = movement_temp
    ))



#unique(loc$proj)
#"ECCC" ,"second_yr" , "sthcarolina_arctic", "Newstead"    

   
loc1 <- loc |> 
  dplyr::mutate(movement_temp = ifelse("tag.id.order" == 1, "deployment", movement_temp)) |> 
  dplyr::mutate(stopover = case_when(
    gcd_m <= 25000 & diff >24 ~ "stopover",
    .default = stopover)) 

loc1 <- loc1 |> 
  dplyr::mutate(movement_temp = case_when(
    month %in% c(6,7) & location.lat > 61  & stopover == "stopover" ~ "breeding",
  .default = movement_temp))
  

loc2 <- loc1 %>% 
  dplyr::mutate(movement_temp = case_when(
    month %in% c(12,1,2) ~ "wintering",
    month %in% c(10,9,8) ~ "south_migration",
    month %in% c(4,5) ~ "north_migration",
    .default = movement_temp
  ))


clean_sf <- st_as_sf(loc2, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(raw_dat, "locations_raw_2025", "loc_20251230_raw.gpkg"), append = F)
#st_write(clean_sf, file.path(raw_dat, "locations_final", "loc_20240712_ocean_final.gpkg"), append = F)



############################################################################



# manually review the reference dataset (reference_data_raw_2020_2025_edited.xlsx)
# keep as xlsx due to date issues. 


# manually check the spatial dataset (loc_20260116_raw_edited.gpkg) 



# manually create a check list with n/s/b/w catergories (2017 - 2025)
#final_tags_list_edited_20260119.csv




#201139 - deleted one values in new dataset as error (newstead)



####################################################################################
# join together the 2023 - 2025 along with final dataset from 2017-2023. 

# 2023-2025
d2 <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_20260116_raw_edited.gpkg"))

d2 <- d2 |> 
  mutate(gps.fix.type.raw = as.character(gps.fix.type.raw)) |> 
  mutate(movebank_event = Event) |> 
  select(c(-Event, -outlier.comments)) |> 
  rename("movement_final" = movement_temp)

#2017-2023
d1 <-  st_read( file.path("../..","05_deliverables", "01_data", "rekn_gps_final_20231219.gpkg"))

d1 <- d1 |> 
  mutate(timestamp = ymd_hms(timestamp)) |> 
  select(-argos.altitude, -movement_temp)
    
## Run checks#   

# names(d1)
# names(d2)
# 
# unique(d2$outlier.comments)
unique(d1$lotek.crc.status)
unique(d2$lotek.crc.status)
# unique(d1$gps.fix.type.raw)
# unique(d2$gps.fix.type.raw)
# unique(d1$argos.lc)
# unique(d2$argos.lc)
# unique(d1$Event)
# unique(d2$Event)

all <- bind_rows(d1, d2)
unique(all$lotek.crc.status)
#head(all)

# drop the error tags argos.lc == Z
unique(all$argos.lc)

# 335486
all <- all |> 
  filter(argos.lc %in% c(NA, "A", "2", "B", "1", "0", "3"))
# 335264

# drop the lotec status = E
unique(all$lotek.crc.status)

all <- all |> 
  filter(lotek.crc.status %in% c(NA, "G", "F", "C" ,"OK","OK(corrected)"))
# 335257

todrop <- c(238593,238597, 238599, 238600, 238601, 238602, 
            238606, 238607, 238608, 238609, 280827, 285989, 285990,
            285992, 285993, 285994, 285997)

all <- all |> 
  mutate(tag.id.drop = ifelse(tag.id %in% todrop, 1, 0)) |> 
  filter(tag.id.drop == 0)# |> 
 # select(-tag.id.drop)

#334945



st_write(all, file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_raw.gpkg"), append = F)



##################################################################################
####################################################################################
library(dplyr)

# review all the tag by project 

alldf <- all |> st_drop_geometry()

tp <- alldf |> 
  group_by(proj) |> 
  count(tag.id)


tp |> 
  ungroup() |> 
  select(-n) |> 
  group_by(proj) |> 
  summarise(n())

###################################################################################
## reviewed the data and updatesd table. 


# manually check each tag and assign the rufa/rosellarri type. 


# for Rufa subsp manually check each tag and assign subsp options. 


#####################################################################################
# read back in and update the 


al <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_raw.gpkg"))


# update the stopovers for 2024 and 2025 
aa <- al |> filter(year %in% c(2024,2025))
ab <- al |> filter(year %in% c(2017:2023))



# check the combinations
aa |> st_drop_geometry() |> 
  select(stopover, movement_final,movement_temp) |> unique()
  #select(stopover, movement_final) |> unique()

aa <- aa |> 
  mutate(movement_temp = case_when(
    stopover == "stopover" & movement_final == "wintering" ~ "wintering",
    is.na(stopover) & movement_final == "wintering" ~ "wintering",
    is.na(stopover) & movement_final == "outlier" ~ "uncertain_location",
    is.na(stopover) & movement_final == "north_migration" ~ "north_migration",
    is.na(stopover) & movement_final == "south_migration" ~ "south_migration",
    is.na(stopover) & movement_final == "breeding" ~ "breeding",
    stopover == "deployment" ~ "deployment",
    stopover == "deplyment" ~ "deployment",
    stopover == "Deployment" ~ "deployment",
    movement_final == "deployment" ~ "deployment",
    is.na(stopover) & movement_final == "deployment" ~ "deployment",
    stopover == "" & movement_final == "north_migration"~ "north_migration",
    stopover == "stopover" & movement_final == "north_migration" ~ "north_stopover",
    stopover == "stopover" & movement_final == "south_migration" ~ "south_stopover",
    stopover == "stopover'" & movement_final == "south_migration" ~ "south_stopover",
    stopover == "stopover" & movement_final == "breeding" ~ "breeding",
    stopover == "stopover" & movement_final == "outlier" ~ "uncertain_location",
    stopover == "stopover" & movement_final == "dropped_tag" ~ "uncertain_location"
  ))


aa<- aa |> 
  rename("movement_final_old" =  movement_final) 
  
aa<- aa |> 
  rename("movement_final" =  movement_temp) 

all_out <- bind_rows(ab, aa)


st_write(all_out, file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_edited.gpkg"), append = F)



##################################################################################

# join the raw location data (all the argos values) and check the values match 

raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")

# read spatial data 
df_all <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_edited.gpkg"))
events_to_keep <- unique(df_all$movebank_event)

# read in the old data raw (2017 - 2023)
loc1 <- read_csv(file.path(final_dat, "location_data_raw.csv")) 

loc11 <- loc1|> 
  mutate(date_time = as.POSIXlt(date_time, format = "%Y-%m-%d %H:%M"),
         timestamp= as.POSIXlt(timestamp, format = "%Y-%m-%d %H:%M"))


#head(loc1)

# read in the new datasets (2024-2025)
loc2 <- read_csv(file.path(final_dat, "location_data_raw_2025.csv"))
#> length(loc2$proj)
#[1] 256778
loc22 <- loc2 |> 
  filter(Event %in% events_to_keep)|> 
  mutate(gps.fix.type.raw = as.character(gps.fix.type.raw))|> 
  mutate(movebank_event = Event) |> 
  select(c(-Event))


loc_all <- bind_rows(loc11, loc22)
write.csv(loc_all, file.path(final_dat, "location_data_2017_2025.csv"))

#head(loc1)
#head(loc22)

