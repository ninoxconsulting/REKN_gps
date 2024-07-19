


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

