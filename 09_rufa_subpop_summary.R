
##################################################################################

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

ref_due <- ref %>% 
  select(proj, tag.id, tag.model, study.site) 

# read in duration tags
dur <- read_csv( file.path(out.plots, "duration_per_tag_353.csv"))

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

### SUMMARY OF SUB populations 

## 
# count number of 
pop_sum <- pop_id |> 
  group_by(subpop)%>% 
  count()

## count per subpop per type of data available

pop_sum <- pop_id |> 
  group_by(subpop, type)%>% 
  count()


### Figure 6: Movement types 

df_limit <- df_all %>% 
  filter(movement_final %in% c("breeding", "north_stopover", "south_stopover", "wintering"))

# create a plot by movement type 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = df_limit , size = 1.5, aes(fill = movement_final, colour = movement_final), alpha = 0.2)+#colour = "dark blue") +
  scale_color_viridis_d()+
  facet_wrap(~movement_final)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -30), ylim = c(-60, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        legend.position="none")

global

#ggsave(file.path(out.plots,"figure6_rufa_movmentclass.jpg"), width = 15, height = 30,units = "cm", dpi = 600)



############################################################################
### SUB POPULATION REVIEW 

#unique(pop_id$subpop)
#[1] "NSA"          "NSA_South"    "SE"           "SE_NSA"       "SE_NSA_South" "South"       
#[7] "west"   

## generate a review table 

# get first date of departure 
dur_type <- df_all %>% 
  group_by(tag.id, movement_final) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts, movement_final)%>% 
  st_drop_geometry() |> 
  distinct() %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(max_ts)) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |> 
  mutate(dur_min = round(as.numeric(duration)/60,1))%>%  
  mutate(dur_hrs = round(as.numeric(dur_min)/60,1))%>%  
  mutate(dur_days = round( dur_hrs/24,1))%>%  
  mutate(year = year(min)) %>% 
  select(tag.id, min, max, duration, dur_days, year, movement_final)%>%
  ungroup()


# join the reference data 

dur_type_move <- left_join(dur_type , pop_id)


write.csv(dur_type_move , file.path(out.plots, "rufa_duration_movement_type_rufa.csv"))


## generate a paired down version of the stopover locations for mapping only not for analysis 

library(geosphere)

df_stopover_subset <- df_all |> 
  group_by(tag.id) |> 
  arrange(timestamp) |>
  cbind(st_coordinates(df_all)) |> 
  mutate(movement_final_next = lead(movement_final, 1L)) |> 
  rowwise() |> 
  mutate(toremove = ifelse(movement_final_next == movement_final, 0,1)) |> # if the next movement type is identical
  ungroup() |> 
  filter(movement_final != "north_migration") |> 
  filter(movement_final != "south_migration") |> 
  mutate(location.long_prior = lead(X, 1L),
         location.lat_prior = lead(Y, 1L))%>%
  rowwise() %>%
  dplyr::mutate(gcd_m_2 = distHaversine(c(location.long_prior,location.lat_prior), c(X, Y)))%>% 
  mutate(toremove2 = ifelse(gcd_m_2 > 100000, 1, 0)) |> # if distance is greater than 500km  
  ungroup() %>%
  select(tag.id, date_time, tag.id.order, movement_final,movement_final_next, gcd_m_2 , toremove, toremove2) |> 
  rowwise() |> 
  mutate(keep = toremove + toremove2)



st_write(df_stopover_subset , file.path(out.plots , "rufa_stopovers.gpkg"), append = FALSE)





