

##################################################################################
####################################################################################

library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)


#data_folder <- file.path("../../02_data/REKN_gps/data")
#raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")


# read in the ref data
ref <- read_csv(file.path(final_dat, "final_tags_list_edited.csv"))
ref_id <- ref %>% select("tag.id" , "proj", "subspecies", "subpop", 
                         "north", "breeding" , "south","wintering" ,  
                         "type", "usable"  )

#tag.ids_y <- ref_id |> filter(usable == 'y')


## red in compiled data with movements 


df_all <- st_read(file.path(final_dat,"rekn_moveclass_20240716.gpkg" ))

unique(df_all$movement_final)

df_usable <- df_all %>% 
  filter(movement_final != "uncertain_location")


## 









man_out <- man_out %>%
  #man_out <- man11 %>%
  cbind(st_coordinates(.))%>%
  rename(location.lat = Y, 
         location.long = X) %>%
  st_drop_geometry()











## checks 
#length(sub_dir$tag.id)
#length(man_out$tag.id)
#length(st$tag.id)

# stsf <- st_as_sf(man_out, coords = c("location.long", "location.lat"), crs = 4326)
# 
# write_sf(stsf, file.path(raw_dat, "test_edited_compiled2.gpkg"))
# 
# unique(stsf$proj)
# 
# 
# 

#### 
## Estimate the stopover locations per state and month 

install.packages("USA.state.boundaries")
library(USA.state.boundaries)
library(ggplot2)

# load tggplot2# load the map

data(state_boundaries_wgs84)

us <- state_boundaries_wgs84 %>% 
  select(NAME,  STATE_ABBR, TYPE)


# might need to read in the "key" with the assigned subspecies types......


# plotting with ggplot2
ggplot(state_boundaries_wgs84) + geom_sf()


basic_stopovers <- man1 %>% 
  select(movement_final)





