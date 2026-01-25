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
d2 <- st_read( file.path(raw_dat, "locations_raw_2025", "loc_20260116_raw_edited.gpkg"))

#2017-2023
d1 <-  st_read( file.path("../..","05_deliverables", "01_data", "rekn_gps_final_20231219.gpkg"))
d1 <- d1 |> 
  


all <- bind_rows(d1, d2)




##################################################################################
####################################################################################
# 
# library(lubridate)
# library(sf)
# library(stringr)
# library(readr)
# library(dplyr)
# 
# 
# #data_folder <- file.path("../../02_data/REKN_gps/data")
# raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
# final_dat <- file.path("../../02_data/REKN_gps/output_final")
# 
# 
# # read in the ref data
# ref <- read_csv(file.path(final_dat, "final_tags_list_edited.csv"))
# 
# 
# ## After manual edits and verification, combine all files back together: 
# 
#   
# final_input <- file.path(raw_dat, "locations_final") 
# final_dat <- file.path("../../02_data/REKN_gps/output_final")
# 
# # join the manual edits together and merge to the main dataset 
# list.files(final_input, pattern = "*.gpkg$" )
# 
# 
# man1 <- st_read(file.path(final_input, "loc_20240712_atlantic_final.gpkg"))
# man2 <- st_read(file.path(final_input, "loc_20240712_dom_final.gpkg" ))
# man3 <- st_read(file.path(final_input, "loc_20240712_eccc_final.gpkg" ))
# man4 <- st_read(file.path(final_input,  "loc_20240712_johnson_final.gpkg"))
# man5 <- st_read(file.path(final_input,  "loc_20240712_ma_final.gpkg"  ))
# man6 <- st_read(file.path(final_input, "loc_20240712_ming_final.gpkg"  ))
# man7 <- st_read(file.path(final_input,  "loc_20240712_mispillion_final.gpkg"))
# man8 <- st_read(file.path(final_input,  "loc_20240712_new_final.gpkg"  ))
# man9 <- st_read(file.path(final_input, "loc_20240712_ocean_final.gpkg"   ))
# man10 <- st_read(file.path(final_input,  "loc_20240712_spring_final.gpkg" ))
# man11 <- st_read(file.path(final_input,  "loc_20240712_sthcarolina_final.gpkg" ))
# 
# 
# man_out <- bind_rows(man1, man2, man3, man4, man5, man6, man7, man8, man9, man10, man11) 
# length(man_out$proj)
# 
# unique(man_out$movement_final)
# 
# 
# #summ_proj_sf <- man_out |> 
# #  group_by(proj)%>%
# #  count()
# 
# st_write(man_out, file.path(final_dat, "rekn_moveclass_20240716.gpkg"), append = F)
# 
# 
# 
