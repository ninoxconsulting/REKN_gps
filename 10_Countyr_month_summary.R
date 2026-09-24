
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

#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final/draft_outputs_2026")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures_2026")

# read in the ref data
ref <- read_csv(file.path(final_dat, "reference_data_2020_2025_20260124.csv"))
ref_due <- ref %>% 
  select(proj, tag.id, tag.model, study.site)

# read in the key 
pop <- read_csv(file.path(final_dat, "final_tags_list_edited_20260126.csv"))
pop_id <- pop %>% 
  select("tag.id" , "proj", "subspecies", "subpop", 
         "north", "breeding" , "south","wintering" ,  
         "type", "usable"  ) |> 
  filter(usable == 'y') %>% 
  filter(subspecies == "rufa") %>%
  left_join(ref_due)

rufa_ids <- pop_id$tag.id

## read in compiled data with movements and limit to rufa 
df_all <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_movetype_20260125.gpkg")) |> 
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")

# ## generate a paired down version of the stopover locations for mapping only not for analysis 
#df_stopover_subset <- st_read(file.path(out.plots , "rufa_stopovers.gpkg"))
##############################################################################

library(dplyr); library(sf); library(lubridate); library(geosphere)

site_threshold_m <- 25   # fixes >30 km apart start a new site
min_days_after   <- 2       # onward fix must be at least this far past departure

bouts <- df_all |>                                  # or df_stopover_subset
  filter(movement_final %in% c("north_stopover", "south_stopover")) |>
  mutate(lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) |>
  st_drop_geometry() |>
  distinct(tag.id, date_time, .keep_all = TRUE) |>
  arrange(tag.id, date_time, tag.id.order) |>
  group_by(tag.id) |>
  mutate(
    dist_next_m = distHaversine(cbind(lon, lat),
                                cbind(lead(lon), lead(lat))),
    moved       = coalesce(dist_next_m > site_threshold_m, FALSE),
    site_id     = cumsum(lag(moved, default = FALSE)) + 1
  ) |>
  ungroup()


dep <- bouts |>
  group_by(tag.id, site_id) |>
  summarise(
    arrive   = min(date_time),
    depart   = max(date_time),          # last fix at the site
    n_fix    = n(),
    lon      = mean(lon),
    lat      = mean(lat),
    movement = first(movement_final),
    .groups  = "drop"
  ) |>
  group_by(tag.id) |>
  arrange(tag.id, site_id) |>
  mutate(
    next_fix  = lead(arrive),                                   # first fix elsewhere
    gap_days  = as.numeric(difftime(next_fix, depart, units = "days")),
    is_last   = site_id == max(site_id)                         # censored: tag may have failed
  ) |>
  ungroup() |>
  filter(!is_last, gap_days >= min_days_after) |>
  mutate(
    depart_month = month(depart, label = TRUE, abbr = FALSE),
   # depart_week  = week(depart),
    depart_doy   = yday(depart)
  )


# left join with tag_ids> 

dep <- dep |>
  inner_join(
    pop_id |> select(tag.id, proj, subspecies, subpop, type, study.site),
    by = "tag.id"
  ) 


americas_admin <- ne_countries(returnclass = "sf") |>
  st_drop_geometry() |>
  filter(region_un == "Americas") |>
  pull(admin)

states <- ne_states(returnclass = "sf") |>
  filter(admin %in% americas_admin) |>
  select(state = name, country = admin, geometry)
states <- st_make_valid(states)
dep_sf <- dep |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

dep_sf <- st_join(dep_sf, states, join = st_intersects)


miss <- is.na(dep_sf$state)
sum(miss)   # check how many before deciding

if (any(miss)) {
  nearest <- st_nearest_feature(dep_sf[miss, ], states)
  gap_km  <- as.numeric(st_distance(dep_sf[miss, ], states[nearest, ],
                                    by_element = TRUE)) / 1000
  
  dep_sf$state[miss]      <- states$state[nearest]
  dep_sf$country[miss]    <- states$country[nearest]
  dep_sf$snap_km          <- 0
  dep_sf$snap_km[miss]    <- gap_km
}

summary(dep_sf$snap_km[dep_sf$snap_km > 0])


# select 

dep_tab <- dep_sf |>
  st_drop_geometry() |>
  filter(!is.na(state)) |>
  group_by(subpop, country, state) |>
  summarise(n_tags    = n_distinct(tag.id),
            n_bouts   = n(),
            months    = paste(sort(unique(as.character(depart_month))),
                              collapse = ", "),
            .groups = "drop") |>
  arrange(subpop, desc(n_tags))

dep_tab

west <- dep_tab |> 
  filter(subpop == "West")

south <- dep_tab |> 
  filter(subpop == "South")
se <- dep_tab |> 
  filter(subpop == "SE")
nsa <- dep_tab |> 
  filter(subpop == "NSA")





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
  filter(subspecies == "rufa") %>%
  select(-usable, -north, -breeding, -south, -wintering)
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

### read in intersected table 

int <- st_read(file.path(final_dat, "rekn_moveclass_20240716_usable_world_ca_us_int.gpkg"))

## add the subpop to the table 
  
inn <- int |> 
  select(tag.id, date_time, year, month, movement_final, admin, NAME, STATE_ABBR, PRNAME, PREABBR)%>% 
  st_drop_geometry() |> 
  dplyr::mutate(country = case_when(
    is.na(admin) & !is.na(NAME) ~ "United States of America",
    is.na(admin) & !is.na(PRNAME) ~ "Canada",
    .default = admin
  ))|> 
  dplyr::mutate(state = case_when(
     !is.na(NAME) ~ NAME,
     !is.na(PRNAME) ~ PRNAME,
    .default = NA
  ))%>% 
  filter(movement_final %in% c("deployment", "south_stopover", "north_stopover" , "breeding", "wintering" ))



intt <- inn |> 
  select(-"admin",  -"NAME", -"STATE_ABBR", -"PRNAME", -"PREABBR" ) |> 
  left_join(pop_id)

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
  select(-date_time, -year,  -subspecies, -type, -movement_final) |> 
  filter(!is.na(country)) |> 
  distinct()

ms_summ <- ms|> 
  group_by(country, state, subpop, month) |>
  count()

write.csv(ms_summ , file.path(final_dat, "figures", "country_by_month_occupancy.csv"))





