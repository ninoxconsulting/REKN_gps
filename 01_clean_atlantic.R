
## Atlantic dataset and spring dataset - there are duplicated tags in these sets so combining into single script to clean

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)

data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")

raw_dat <- file.path(data_folder, "movebank_locations_20231219")

### Spring ##########################################

key = "Spring Migration"

filesoi <- list.files(raw_dat, pattern = key)

filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]


# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  mutate(tag.local.identifier = tag.id)


sort(unique(brep$tag.id)) # 16 tags 
         
# number of unique tags = no of ids 
btemp <- read.csv(file.path(raw_dat, filesoi))

bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status, 
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier, height.above.ellipsoid)%>%
  mutate(date_time = ymd_hms(timestamp))

sort(unique(bout$tag.local.identifier))
# only 15 tags in the data points (missing 213837 - which appears in the atlantic shores dataset)


setdiff(unique(brep$tag.id),unique(bout$tag.local.identifier) )


all_dat <- left_join(bout, brep ) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z") %>%
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site == "THOMPS" ~ 39.20075,
    study.site == "NORBURYC" ~ 39.20075)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site == "THOMPS" ~ -75.0255,
    study.site == "NORBURYC" ~ -75.0255)) %>%
  dplyr::select(-tag.local.identifier, -individual.local.identifier, -height.above.ellipsoid) %>%
  dplyr::mutate(argos.lc = as.character(argos.lc))%>%
  mutate(tag.id = as.character(tag.id)) %>%
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75",
         tag.manufacturer.name = "Lotek")|> 
  filter(location.lat < 90 ) |> 
  filter(location.lat> -90)


head(all_dat)

spring_tags <- unique(all_dat$tag.id)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))


spring_all_dat <- all_dat


# missing the data of 213837 need to add from Atlantic shores dataset

#############################################################################

## review the tags for Atlantic program
### Need to upload the fixed altantic shores document to movebank, but using this as tempory fix 


key = "Atlantic"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  mutate(tag.local.identifier = tag.id) %>% 
  filter(animal.id != "not_deployed", 
         animal.id != "fail")
  

# 107 tags original - not_deployed or fail. 
#length(unique(brep$tag.id)) # 95 

#sort(unique(brep$tag.id)) 

# read in track data

btemp <- read.csv(file.path(raw_dat, filesoi))

bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                algorithm.marked.outlier, argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, 
                tag.local.identifier)%>%
  mutate(date_time = ymd_hms(timestamp))

#length(unique(bout$tag.local.identifier))

# 87 tags in the dataset 

#sort(setdiff(unique(brep$tag.id), unique(bout$tag.local.identifier)))
 #[1] 204353 204354 204355 204356 204358 204360 204363 204365 204366 204367 204368 204372
#[13] 204373 204374 204376 204377 204378 204379 224074 224090

all_dat <- left_join(bout, brep )

#unique(bout$tag.local.identifier)
#unique(brep$tag.local.identifier)

# checked against failed and not deployed list and can remove any with a NA for animal.id 
all_length = length(all_dat$visible)

all_dat <- all_dat %>% 
    filter(!is.na(animal.id))

edit_length = length(all_dat$visible)
all_length - edit_length # should = 64

#head(all_dat)#2089

all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, 
                -deploy.off.date, -algorithm.marked.outlier, -import.marked.outlier) %>%
  mutate(tag.id = as.character(tag.id)) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z")

unique(all_dat$animal.id)


all_dat <- all_dat %>% 
  mutate(study.site1 = substr( animal.id, start = 1, stop = 4)) %>%
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site1 == "NBRI" ~ 39.436,
    study.site1 == "Thom" ~ 39.20075,
    study.site1 == "Norb" ~ 39.20075)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site1 == "NBRI" ~ -74.346,
    study.site1 == "Thom" ~ -75.0255,
    study.site1 == "Norb" ~ -75.0255))%>%
  mutate(study.site = study.site1) %>%
  dplyr::select(-study.site1)


# select out the spring tags 

spring_tag_data <- all_dat %>% 
  filter(tag.id %in% spring_tags)

extra_spring <- all_dat %>% 
  filter(tag.id == 213837)

spring_tag_data <- bind_rows(spring_tag_data, extra_spring  )


# select out the remaining atlantic tags 

at_tag <- all_dat %>%
  filter(!tag.id %in% spring_tags)%>%
  filter(!tag.id == 213837)


# # Nbrig latitutde/ longitide 
# Noth Brigatine 
# -74.346 39.436
# 
# 
# # Thom = thompson beach 
# -75.0255 , 39.20075
# 
# # norb 
#    -75.0255 , 39.20075

summ <- at_tag %>% 
  group_by(tag.id) |> 
  count()

# drop the failed tags  
#failed_tags <- at_tag %>% 
#  filter(tag.id == 229329)

at_tag <- at_tag %>% 
  dplyr::filter(tag.id != "229329")%>%
  dplyr::select(-visible, -data.sort.temp, -edited.manually) |> 
  dplyr::filter(!is.na(location.long), 
                !is.na(location.lat)) 


at_tag <- at_tag %>%
  dplyr::mutate(deploy.on.date = ymd_hm(deploy.on.date))


# #save out file
clean_save = at_tag %>% mutate(proj = "atlantic")
saveRDS(clean_save, file = file.path(output_folder, "rekn_atlantic_20240708.rds"))



###########################################################################

# merge the spring_tag from atlantic and stpring tag data together 

# spring data downloaded direct from movebank
# spring data downloaded from atlantic dataset 

#head(spring_tag_data) # from At
#head(spring_all_dat)  # from spring tags dataset

ospring <- bind_rows(spring_all_dat, spring_tag_data)
ospring <- ospring 

ospring <- ospring |> 
  group_by(tag.id) |> 
  arrange(date_time) |> 
  ungroup() |> 
  dplyr::mutate(animal.taxon = "Calidris canutus") |> 
  mutate( animal.id = toupper(animal.id))


# update study site 

 ospring <- ospring |> 
  mutate(study.site = case_when(
    study.site == "THOMPS"~ "THOM",
    study.site == "NORBURYC"~ "NORB",
    .default = study.site 
  ))

# update the date for  
library(tidyr)
 
ospring <- ospring %>%
  dplyr::mutate(capture.timestamp = case_when(
    tag.id == 213837 ~ "2021-05-19 19:32:00.000",
    .default = capture.timestamp
    )) |> 
  group_by(tag.id) |> 
  fill(capture.timestamp , .direction = "downup") |> 
  mutate(deploy.on.latitude = 39.20075 ) |> 
  mutate(deploy.on.longitude = -75.0255 ) |> 
    dplyr::ungroup() %>%
  mutate(deployment.comments = NA)%>%
  group_by(tag.id) |> 
  arrange(date_time) |> 
  mutate(deploy.on.date = as.POSIXct(deploy.on.date, format = "%Y-%m-%d %H:%M")) |> 
  mutate (tag.comments = case_when(
    tag.comments == "" ~ NA,
    .default = tag.comments
  )) |> 
  mutate (import.marked.outlier = case_when(
    import.marked.outlier == "" ~ NA,
    .default = import.marked.outlier)) |> 
dplyr::select(-id)

# length(ospring$visible)

ospring <- ospring |> 
  dplyr::distinct()%>%
  ungroup()

# length(ospring$visible)

ospring <- ospring %>%
  dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date)) |> 
  dplyr::mutate(id = seq(1, length(ospring$visible), 1))%>%
  dplyr::select(-visible, -data.sort.temp, -edited.manually)



# #save out file
clean_save = ospring %>% mutate(proj = "spring_USFWS")

saveRDS(clean_save, file = file.path(output_folder, "rekn_spring_usfws_20240708.rds"))



