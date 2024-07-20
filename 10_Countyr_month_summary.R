
############################################################################

# Rufa subpopulations 

####################################################################################
library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)


final_dat <- file.path("../../02_data/REKN_gps/output_final")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures")

# read in ref data 
ref <- read_csv(file.path(final_dat, "reference_data_edited.csv"))
ref_due <- ref %>% select(proj, tag.id, tag.model, study.site) 

#dur <- read_csv( file.path(out.plots, "duration_per_tag_353.csv"))

# read in the sub_population list 

pop <- read_csv(file.path(final_dat, "final_tags_list_edited.csv"))
pop_id <- pop %>% 
  select("tag.id" , "proj", "subspecies", "subpop", 
         "north", "breeding" , "south","wintering" ,  
         "type", "usable"  ) |> 
  filter(usable == 'y') %>% 
  filter(subspecies == "rufa") %>%
  left_join(ref_due)

rufa_ids <- pop_id$tag.id

## read in compiled data with movements and limit to rufa 

df_all <- st_read(file.path(final_dat,"rekn_moveclass_20240716.gpkg" )) %>% 
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")



df_all <- df_all %>% select(proj, tag.id, date_time, year, month, day, movement_final) %>% 
  left_join(pop_id)
  
  
###############################################################
# 

# check the USA 

#### 
## Estimate the stopover locations per state and month 

install.packages("USA.state.boundaries")
library(USA.state.boundaries)
library(ggplot2)

# load tggplot2# load the map

data(state_boundaries_wgs84)

us <- state_boundaries_wgs84 %>% select(NAME,  STATE_ABBR, TYPE)
st_write(us, file.path(out.plots, "us_states.gpkg"))

# might need to read in the "key" with the assigned subspecies types......

# intersect with country 

world <- ne_countries(scale = "medium", returnclass = "sf") %>% select (admin)
st_write(world, file.path(out.plots, "world.gpkg"))



#################################################

### read in intersection ....



rk <- st_intersection(df_all, world)

# intersect with US states 
rk <- st_intersection(df_all, us)


# plotting with ggplot2
ggplot(state_boundaries_wgs84) + geom_sf()


basic_stopovers <- man1 %>% 
  select(movement_final)

