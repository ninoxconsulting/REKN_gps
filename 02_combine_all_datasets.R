
#library(leaflet)
#library(RColorBrewer)

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")

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


#write.csv(all, file.path(output_folder, "compiled_202407114.csv"))

# check the values 
summ <- all %>%
  group_by(tag.id, proj) |>
  count()

unique_id <- all%>% dplyr::select(tag.id, proj) %>% unique()

# check the values 
summ <- unique_id %>%
  group_by(proj) |>
  count()

unique(all$proj)
unique(all$algorithm.marked.outlier)
unique(all$import.marked.outlier)
unique(all$study.site)



#######################################
# filter records than occur before deplopyment 
# length 94262

out <- all %>%
    mutate(pre_dep = ifelse(date_time >= deploy_date_time, 1, 0 )) |> 
    mutate(pre_dep = ifelse(is.na(pre_dep), 1, pre_dep)) |> 
    filter(pre_dep == 1) |> 
    dplyr::select(-pre_dep)
  
#write.csv(out, file.path(output_folder, "compiled_202407114.csv"))

summ <- out |> 
  group_by(tag.id, pre_dep) %>% 
  count()

# filter dates and generate the durtation 

out <- all %>%
  mutate(date_time = ymd_hms(timestamp))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time)) %>%
  filter(year<=2023) %>%
  arrange(date_time, by_group = tag.id)

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
                speed_mhr = round((gcd_m/diff)/1000,1))

# 
# # we can see a big range in the time intervals for the fixes
# range(bdd_det$diff, na.rm = TRUE)
# 
# ggplot(bdd_det, aes(tag.id, diff)) +
#   geom_col(aes(fill = tag.model))#, position = position_stack(reverse = TRUE))


# write out: 
#write.csv(bdd_det, file.path("output", "xall_rekn_20240207.csv"), row.names = F)
clean_sf <- st_as_sf(all, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("output", "xall_rekn_20240207.gpkg"), append = F)



