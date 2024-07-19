
#library(leaflet)
#library(RColorBrewer)

# combine all th individual edited dataset to produce a single dataset (not this requires a phase 2 for the migration portion)
library(geosphere)
library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)

#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")

filesoi <- list.files(raw_dat)


spring <- readRDS(file.path(raw_dat, "rekn_spring_usfws_20240708.rds")) 
dom <- readRDS(file.path(raw_dat, "rekn_dominion_20240708.rds"))    
at <- readRDS(file.path(raw_dat, "rekn_atlantic_20240708.rds"))
john <- readRDS(file.path(raw_dat, "rekn_john_20240708.rds"))   
ma <- readRDS(file.path(raw_dat, "rekn_ma_mig_20240708.rds" ))        
oc <- readRDS(file.path(raw_dat, "rekn_ocean_20240708.rds"))  
mils <- readRDS(file.path(raw_dat, "rekn_mispillion_20240708.rds"))   
sth <- readRDS(file.path(raw_dat, "rekn_sthcarolina_20240708.rds"))   
new <- readRDS(file.path(raw_dat, "rekn_newstead_20240708.rds"))      
eccc <- readRDS(file.path(raw_dat, "rekn_eccc_20240708.rds")) 
qu <- readRDS(file.path(raw_dat, "rekn_mignon_raw_20240708.rds"))   

all <- bind_rows(eccc, spring, dom, at, john, ma, oc, mils, sth, qu, new) %>%
  dplyr::select(-visible) %>%
  dplyr::mutate(animal.taxon = "Calidris canutus")%>% 
  dplyr::mutate(date_time = ymd_hms(timestamp)) %>%
  dplyr::mutate(deploy_date_time = ymd_hms(deploy.on.date)) 


#94081

#write.csv(all, file.path(output_folder, "compiled_202407114.csv"))
# 
# # check the values 
summ <- all %>%
  group_by(tag.id, proj) |>
  count()

unique_id <- all%>% dplyr::select(tag.id, proj) %>% unique()

# check the values
summ <- unique_id %>%
  group_by(proj) |>
  count()
# 
# unique(all$proj)
# unique(all$algorithm.marked.outlier)
# unique(all$import.marked.outlier)
# unique(all$study.site)
# 


#######################################
# filter records than occur before deplopyment 

out <- all %>%
    mutate(pre_dep = ifelse(date_time >= deploy_date_time, 1, 0 )) |> 
    mutate(pre_dep = ifelse(is.na(pre_dep), 1, pre_dep)) |> 
    filter(pre_dep == 1) |> 
    dplyr::select(-pre_dep)

#length(out$tag.id)  #94247  
#length(unique(out$tag.id)) #356

#write.csv(out, file.path(output_folder, "compiled_202407114.csv"))

# filter dates and generate the duration 
out <- out %>%
  mutate(date_time = ymd_hms(timestamp))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time)) %>%
  filter(year<=2023) %>%
  #filter(year>=2019) %>%
  arrange(date_time, by_group = tag.id)

#length(out$tag.id)  # 88787
#length(unique(out$tag.id)) #356

out <- out %>%
  group_by(tag.id) |> 
  mutate(diff = difftime(date_time, lag(date_time),  units = c("hours")), 
         diff = as.numeric(diff)) 


############################################################################
## Calculate distance between points and bearing

bdd_det <- out  |> 
  #filter(tag.id == 230318) |> 
  group_by(tag.id) |> 
  mutate(location.long_prior = lag(location.long, 1L),
         location.lat_prior = lag(location.lat, 1L))

bdd_det <- bdd_det |> 
  rowwise() %>%
  dplyr::mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
                bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
                speed_mhr = round((gcd_m/diff)/1000,1))%>% 
  ungroup()


#length(unique(bdd_det$tag.id))


## Add id values 

bdd_det <- bdd_det %>%
  dplyr::mutate(id = seq(1, length(bdd_det$tag.id), 1))


# write out the entire dataset 
#write.csv(bdd_det, file.path(output_folder, "compiled_20240711.csv"))
#clean_sf <- st_as_sf(bdd_det, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(output_folder, "rekns_20240711.gpkg"), append = F)

# 
# ## get list of unique id in which to only need to do this once. 
# id_summ <- bdd_det |> 
#   group_by(proj, tag.id, animal.id) |> 
#   count()%>% 
#   mutate(subspecies = ifelse(proj == "Johnson_GPS", "roselarri", "rufa"))
# 
# # write out the csv to be used to summarise tag movements. 
# write.csv(id_summ, file.path(raw_dat, "final_tags_list_blank.csv"))
# 


## Split data into the refernce and location datasets and output only most important columns
#bdd_det <- read.csv(file.path(output_folder, "compiled_20240711.csv")) |> dplyr::select(-X)

# extract reference information 
ref <- bdd_det %>% 
  dplyr::select( animal.id, animal.life.stage, animal.marker.id, 
                animal.mass,animal.ring.id, animal.sex, animal.taxon, "attachment.type", 
                "capture.timestamp",  "deploy.on.date" ,"deploy.on.latitude", "deploy.on.longitude" , 
                "deploy.on.measurements", "deploy.on.person" , "deployment.comments", 
                "duty.cycle", "tag.beacon.frequency"   ,   "tag.manufacturer.name",
                "tag.mass", "tag.id", "tag.serial.no" ,  "tag.model", "study.site", proj,
                deploy.off.date,"deploy_date_time", "duty.cycle" ,"tag.comments", "track_data") %>%
  distinct()

#length(unique(ref$tag.id))

 
loc <- bdd_det %>% dplyr::select("proj", id, "tag.id", tag.model, "location.lat" ,"location.long" ,  
                             "date_time", "timestamp", "year",   "month",    "day" ,   "hour" ,    "minute" , 
                             "argos.lc" , "lotek.crc.status", "gps.fix.type.raw" , 
                             "bearing" , "diff", "gcd_m",   "speed_mhr",  #"breeding" ,"direction" ,
                             "algorithm.marked.outlier" , "argos.altitude",  "argos.best.level" , "argos.calcul.freq" ,            
                             "argos.error.radius", "argos.gdop" , "argos.iq", "import.marked.outlier" ,                     
                              "argos.lat1" , "argos.lat2" ,                   
                              "argos.lon1" , "argos.lon2" ,"argos.nb.mes" ,                 
                              "argos.nb.mes.120" ,"argos.nopc" ,"argos.orientation",             
                             "argos.pass.duration" , "argos.semi.major" , "argos.semi.minor"  ,            
                              "argos.sensor.1" , "argos.sensor.2" , "argos.sensor.3" ,               
                              "argos.sensor.4" , "argos.valid.location.algorithm", 
                             "location.lat_prior" , "location.long_prior","Event" ,                     
                             "height.above.ellipsoid"  , "argos.altitude") %>%
  unique()


# clean up the spatial resolution # can remove these cols. 
unique(loc$argos.lc)
unique(loc$lotek.crc.status)
unique(loc$gps.fix.type.raw)

## How many tags and types of tage 
unique(ref$tag.model)


a <- ref %>% group_by(tag.id)%>%count()



# write out the csv to be used to summarise tag movements. 
write.csv(ref, file.path(final_dat, "reference_data_raw.csv"))

# note reviewed the refernce data and removed or deleted duplicate references

write.csv(loc, file.path(final_dat, "location_data_raw.csv"))

