
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



# gens directory: 
data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")


# note created a new subfolder and moved a copy of the final outputs here so we can use it in the code
previous_ref_dat <- file.path(data_folder, "compiled_REKN_data2023")

# read in the previous reference dataset
old_ref <- read.csv(file.path(previous_ref_dat, "reference_data_edited.csv"))
# read in the previous location data 
old_loc <- read.csv(file.path(previous_ref_dat, "location_data_raw.csv"))
next_id <- max(old_loc$id)+1 # get last id value #88765
old_compiled <- read.csv(file.path(raw_dat, "compiled_20240711.csv"))

oldtags <- unique(old_compiled$tag.id)
oldref <- unique(old_ref$tag.id)
#filesoi <- list.files(raw_dat)





#list the data files to compile

secyr <- readRDS(file.path(raw_dat, "rekn_secondyr_20251230.rds")) 
sth <- readRDS(file.path(raw_dat, "rekn_sthcarolina_20251230.rds"))   
new <- readRDS(file.path(raw_dat, "rekn_newstead_20251230.rds"))
eccc <- readRDS(file.path(raw_dat, "rekn_eccc_20251230.rds")) 


all <- bind_rows(secyr, sth, new, eccc) |> 
  dplyr::mutate(individual.taxon.canonical.name = "Calidris canutus") |> 
  dplyr::mutate(deploy_date_time = ymd_hms(deploy.on.date)) |> 
  dplyr::mutate(date_time = ymd_hms(timestamp)) |> 
  dplyr::mutate(year = year(date_time)) |> 
  filter(year <=2025) |> 
  mutate(tag.model = case_when(
#    tag.model == "gps-pinpoint" ~ "Lotek PinPoint GPS-Argos 75", 
    tag.model == "PinPoint 75" ~ "Lotek PinPoint GPS-Argos 75", 
    tag.model ==  "Sunbird" ~ "Sunbird Solar Argos",
    tag.model ==  "sunbird" ~ "Sunbird Solar Argos",
    #tag.model == "microwave telemetry"~ "Solar 2-g PTT",
    .default = as.character(tag.model))) %>%
  mutate(tag.manufacturer.name = case_when(
    tag.model == "Lotek Wireless" ~ "Lotek", 
    TRUE ~ as.character(tag.manufacturer.name)))





#length(all$event.id) #343112   drop future dates #336823

## remove all dates that are future 
#sort(unique(all$year))

# filter the post 2023 data 
post_2023 <- all |> filter(year>2023)

# # check which tags are in the old and new daat
# newtags <- unique(post_2023$tag.id)
# #oldtags[oldtags %in% newtags]
# oldref[oldref %in% newtags]
# 
# newtags[newtags %in% oldtags]
# newtags[newtags %in% oldref]
# oldref[oldref %in% newtags]



# write out everything. 
write.csv(all, file.path(raw_dat, "compiled_20251230.csv"))

# write out the post 2023 data 
write.csv(post_2023, file.path(raw_dat, "compiled_post202320251230.csv"))

#clean_sf <- st_as_sf(all, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(output_folder, "rekns_20251230.gpkg"), append = F)



# dont filter by visible as this seems to drop many points, and not all clearly outliers

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
 unique(all$proj)
 unique(all$algorithm.marked.outlier)
 unique(all$import.marked.outlier)
 unique(all$study.site)
 unique(all$animal.id)
 unique(all$deployment.id)

 # 


#######################################
# filter records than occur before deplopyment 

out <- post_2023 %>%
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
         minute = minute(date_time)) |>
  mutate(Event = event.id ) |> 
  # filter the dodgy location values
  filter(location.lat <90 & location.lat >= -90) |> 
  filter(location.long <180 & location.long >= -180) |> 
  arrange(date_time, by_group = tag.id)

#length(out$tag.id)  #  256886
#length(unique(out$tag.id)) #177

out <- out |> 
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
totlength = length(bdd_det$tag.id) + next_id-1

bdd_det <- bdd_det %>%
  dplyr::mutate(id = seq(next_id, totlength, 1))


# write out the entire dataset 
write.csv(bdd_det, file.path(output_folder, "compiled_post2023_20251230.csv"))
clean_sf <- st_as_sf(bdd_det, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(output_folder, "rekns_post2023_20251230.gpkg"), append = F)


# # ## get list of unique id in which to only need to do this once. 
# id_summ <- bdd_det |>
#   group_by(proj, tag.id, animal.id) |>
#   count()#%>%
#   #mutate(subspecies = ifelse(proj == "Johnson_GPS", "roselarri", "rufa"))
# # 
# # # write out the csv to be used to summarise tag movements. 
#  write.csv(id_summ, file.path(raw_dat, "final_tags_list_blank_2025.csv"))
# # 


## Split data into the refernce and location datasets and output only most important columns
#bdd_det <- read.csv(file.path(output_folder, "compiled_20240711.csv")) |> dplyr::select(-X)

# extract reference information 
ref <- bdd_det %>% 
  dplyr::select( animal.id, animal.life.stage, animal.marker.id, "animal.comments",
                animal.mass, animal.ring.id, animal.sex, animal.taxon, "animal.taxon.detail", 
                "attachment.type", individual.taxon.canonical.name,individual.local.identifier,
                "capture.timestamp", "capture.method" , tag.local.identifier,
                "deploy.on.date" ,"deploy.on.latitude", "deploy.on.longitude" ,deployment.end.type, 
                "deploy.on.measurements", "deploy.on.person" , "deployment.comments", "deployment.id",
                #"duty.cycle", 
                "tag.beacon.frequency"   ,   "tag.manufacturer.name",
                "tag.mass", "tag.id", "tag.serial.no" ,  "tag.model", "study.site", proj, "study.name", 
                deploy.off.date,"deploy_date","deploy_date_time" ,"tag.comments", "track_data") %>%
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
                             "height.above.ellipsoid" , "argos.sat.id", "argos.transmission.timestamp",
                             "external.temperature","outlier.comments" ,"mortality.status",sensor.type,
                             visible
                             ) |> 
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

