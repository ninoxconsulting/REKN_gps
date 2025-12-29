
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


#TODO - join the previous compiled reference data with this dataset (as we had more information in the earier dataset). 
# FULL DATA Set "reference_data_edited.csv" on google drive. 
# https://docs.google.com/spreadsheets/d/1WQD-Bo7uZ4xla9O6j8gJMZD6CCKW6UbtKUlZ4hL7IH0/edit?usp=drive_link

# TODO: missing some reference information on tags : 
#212359
#212360
#212361
#212362
#212363
# emailed Newstead to check if more data available. 


# gens directory: 
data_folder <- file.path("../../02_data/REKN_gps/data")
output_folder <- file.path("../../02_data/REKN_gps/output_temp")

# Set Input and Output folder paths #
data_folder <- file.path("./02_data/REKN_gps/data")
output_folder <- file.path("./02_data/REKN_gps/output_temp")


#raw_dat <- file.path(data_folder, "other_dataset") # changed this to point to the same raw data location as others
raw_dat <- file.path(data_folder, "movebank_locations_20251210")


# note created a new subfolder and moved a copy of the final outputs here so we can use it in the code
previous_ref_dat <- file.path(data_folder, "compiled_REKN_data2023")
# read in the previous reference dataset
old_ref <- read.csv(file.path(previous_ref_dat, "reference_data_edited.csv")) |> 
  filter(proj == "Newstead")





# Set keyword to use to pull desired datasets
key = "Gulf to Arctic" # added keyword variable similar to sth carolina script

# Pull location and reference data file names based on keyword
filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# Read in location data
ndatr <- read.csv(file.path(raw_dat, filesoi)) 

# Read in reference data
nref <- read.csv(file.path(raw_dat, filesoi_ref)) 


# TODO; merge the old ref data with teh new one where there is missing information. 

 
## Process Reference Data ##
nref <- nref %>%
  mutate(deploy.on.measurements = NA, #
         animal.mass = NA, 
         animal.life.stage = NA) |> 
# check which cols are appropraite to add and adjust this line as needed
  dplyr::select(any_of(c("study.site", "animal.id", "deploy.on.date", "deploy.on.measurements",  "deploy.on.latitude",      
                "deploy.on.longitude" ,"animal.sex" , "animal.ring.id",  "animal.life.stage" )))


## Process Location Data ##

ndat <- ndatr %>%
  #filter(`lotek.crc.status.text` != "OK(corrected)")  %>%
  rename("animal.id" = individual.local.identifier) |> 
  mutate(proj ="Newstead",
         import.marked.outlier = as.character(import.marked.outlier),
         visible = as.character(visible)) %>%
  mutate(date_time = timestamp, 
         tag.id = tag.local.identifier) 
  #dplyr::select( -event.id, -study.name, -individual.taxon.canonical.name) 

## Join the Location data to the Reference data ##
nout <- left_join(ndat, nref)

## Perform final data cleaning for output ##

all_dat <- nout %>%
  filter(!is.na(location.long)) #|> 
  #mutate(id = seq(1, length(nout$visible), 1)) %>%
  #dplyr::mutate(deploy.on.date = ymd_hms(deploy.on.date))

# write out 
clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(output_folder, "pt_news_test.gpkg"), append = F)



#all_dat <- all_dat %>%
#  dplyr::select(-year, -month, -day, - minute, -hour) # comment this out since we no longer have year/month fields to worry about

summ <- all_dat  %>% 
  group_by(tag.id) |> 
  count()



# #save out file
saveRDS(all_dat, file = file.path(output_folder, "rekn_newstead_20251217.rds"))


# write out 
#clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(output_folder, "pt_news_test.gpkg"), append = F)


# 