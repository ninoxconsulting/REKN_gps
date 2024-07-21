
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
#ref <- read_csv(file.path(final_dat, "reference_data_edited.csv"))
#ref_due <- ref %>% select(proj, tag.id, tag.model, study.site) 

# read in the sub_population list 

pop <- read_csv(file.path(final_dat, "final_tags_list_edited.csv"))
pop_id <- pop %>% 
  select("tag.id" , "proj", "subspecies", "subpop", 
         "north", "breeding" , "south","wintering" ,  
         "type", "usable"  ) |> 
  filter(usable == 'y') %>% 
  filter(subspecies == "rufa") #%>%
  #left_join(ref_due)

rufa_ids <- pop_id$tag.id

## read in compiled data with movements and limit to rufa 

  
###############################################################

# load tggplot2# load the map
# 
# data(state_boundaries_wgs84)
# 
# us <- state_boundaries_wgs84 %>% select(NAME,  STATE_ABBR, TYPE)
# st_write(us, file.path(out.plots, "us_states.gpkg"))

# might need to read in the "key" with the assigned subspecies types......

# intersect with country 

#world <- ne_countries(scale = "medium", returnclass = "sf") %>% select (admin)
#st_write(world, file.path(out.plots, "world.gpkg"))



int <- st_read(file.path(final_dat, "rekn_moveclass_20240716_usable_world_ca_us_int.gpkg"))
 
## add the subpop to the table 
  
intt <- int |> 
  select(-"admin", - "NAME", -"STATE_ABBR", -"PRNAME", - "PREABBR" ) |> 
  left_join(pop_id)%>% 
  select(-tag.model, -usable, -north, -breeding, -south, -wintering, -tag.id.order, -proj) |> 
  mutate(weekno = week(date_time)) 
  
# 
# ## Summary by Week of year 
# cs <- intt |> 
#   st_drop_geometry() |> 
#   select(-date_time, -year, -month, -subspecies, -study.site, -type, -movement_final) |> 
#   filter(!is.na(country)) |> 
#   distinct()
# 
# cs_summ <- cs|> 
#   group_by(country, state, subpop, weekno) |>
#   count()
#   

## Summary by Month 

ms <- intt |> 
  st_drop_geometry() |> 
  select(-date_time, -year, -weekno, -subspecies, -type, -movement_final) |> 
  filter(!is.na(country)) |> 
  distinct()

ms_summ <- ms|> 
  group_by(country, state, subpop, month) |>
  count()









co_sum <- int |> 
  st_drop_geometry()
  group_by(year, month, country, state, tag.id) |> 
  count()




%>% 
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")

# plotting with ggplot2
ggplot(state_boundaries_wgs84) + geom_sf()


basic_stopovers <- man1 %>% 
  select(movement_final)

