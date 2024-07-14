# create stopover/ migration classifications 

library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")


# write out the entire dataset 

# read in the ref data
ref <- read_csv(file.path(final_dat, "reference_data_edited.csv"))

loc_all <- read_csv(file.path(final_dat, "location_data_raw.csv"))


# split out the basic information 

loc <- loc_all |> 
  dplyr::select(  -"...1",-"argos.iq" ,  ,-"argos.lat1" ,  -"argos.gdop"  ,-"argos.best.level",                 
                  -"argos.lat2" , -"argos.lon1"  ,   -"argos.lon2",  -"argos.calcul.freq" , -"argos.error.radius" ,                    
                 -"argos.nb.mes", -"argos.nb.mes.120", - "argos.nopc", -"import.marked.outlier" ,                    
                 -"argos.orientation", -"argos.pass.duration",  -"argos.semi.major" ,             
                 -  "argos.semi.minor", -"argos.sensor.1" ,  -"argos.sensor.2" ,               
                  -"argos.sensor.3", - "argos.sensor.4"  , -"argos.valid.location.algorithm", -"height.above.ellipsoid") 
    

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


# update the values which were flagged in roselarri dataset 
loc <- loc |> 
  mutate(movement_temp = case_when(
    Event %in% c("breed", "Breed") ~ "breeding",
    Event %in% c( "Spring stop", "Spring stopover") ~ "north_stopover",
    Event == "Spring migrate"~ "north_migration",
    Event ==  "Autumn" ~ "southward" ,
    .default = movement_temp
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

# check Johnson_dataset

loc <- loc |> 
  dplyr::mutate(movement_temp = case_when(
    proj == "Johnson_GPS" & location.lat < 20 ~ "location_error",
    proj == "Johnson_GPS" & location.lat > 74 ~ "location_error",
    proj == "Johnson_GPS" & location.long < 140 & location.long >90 ~ "location_error",
   # month %in% c(6,7) & location.lat > 64  & stopover == "stopover" ~ "breeding",
    .default = movement_temp
  ))


loc_johnson <- loc %>% filter(proj =="Johnson_GPS") 

# # write out the entire dataset 
clean_sf <- st_as_sf(loc_johnson, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(raw_dat, "locations_raw", "loc_20240712_johnson_raw.gpkg"), append = F)
st_write(clean_sf, file.path(raw_dat, "locations_final", "loc_20240712_johnson_final.gpkg"), append = F)

#head(loc)
#unique(loc$proj)
   
loc1 <- loc |> 
  dplyr::mutate(movement_temp = ifelse("tag.id.order" == 1, "deployment", movement_temp)) |> 
  dplyr::mutate(stopover = case_when(
    gcd_m <= 25000 & diff >24 ~ "stopover",
    .default = stopover)) 

loc1 <- loc1 |> 
  dplyr::mutate(movement_temp = case_when(
    month %in% c(6,7) & location.lat > 61  & stopover == "stopover" ~ "breeding",
  .default = movement_temp))
  

loc_mispillion <- loc1 %>% dplyr::filter(proj =="Mispillion") 
# # write out the entire dataset 
clean_sf <- st_as_sf(loc_mispillion, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(raw_dat, "locations_raw", "loc_20240712_mispillion_raw.gpkg"), append = F)
st_write(clean_sf, file.path(raw_dat, "locations_final", "loc_20240712_mispillion_final.gpkg"), append = F)



## ECCC


loc2 <- loc1 %>% 
  dplyr::mutate(movement_temp = case_when(
    month %in% c(12,1,2) ~ "wintering",
    month %in% c(10,9,8) ~ "south_migration",
    month %in% c(4,5) ~ "north_migration",
    .default = movement_temp
  ))




loc_eccc <- loc2 %>% dplyr::filter(proj =="ECCC") 
clean_sf <- st_as_sf(loc_eccc, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(raw_dat, "locations_raw", "loc_20240712_eccc_raw.gpkg"), append = F)
st_write(clean_sf, file.path(raw_dat, "locations_final", "loc_20240712_eccc_final.gpkg"), append = F)


## MA migration 

loc_ma <- loc2 %>% dplyr::filter(proj =="ma_migration") 

clean_sf <- st_as_sf(loc_ma, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(raw_dat, "locations_raw", "loc_20240712_ma_raw.gpkg"), append = F)
st_write(clean_sf, file.path(raw_dat, "locations_final", "loc_20240712_ma_final.gpkg"), append = F)






##






loc_sthcarolina <- loc1 %>% dplyr::filter(proj =="sthcarolina_arctic") 

# # write out the entire dataset 
clean_sf <- st_as_sf(loc_sthcarolina, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(raw_dat, "locations_raw", "loc_20240712_sthcarolina_raw.gpkg"), append = F)
st_write(clean_sf, file.path(raw_dat, "locations_final", "loc_20240712_sthcarolina_final.gpkg"), append = F)




