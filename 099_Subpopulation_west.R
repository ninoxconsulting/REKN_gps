

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
library(viridisLite) 


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final/draft_outputs_2026")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures_2026")


# 1. Generate the 4 specific hex codes from the viridis palette (e.g., option "D")
viridis_colors <- viridis(4,option = "D")

# 2. Name the colors by your specific factor levels to lock them in
# Replace 'Cat1', 'Cat2', etc., with your actual factor level names
color_mapping <- c(
  "north_stopover" = viridis_colors[1],
  "breeding" = viridis_colors[2],
  "south_stopover" = viridis_colors[3],
  "wintering" = viridis_colors[4]
)

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


# read in duration (raw and compiled)

dur <- read_csv (fs::path(final_dat, "duration_tags_2025_outliers_removed.csv"))
dur_type_move <- read_csv(file.path(out.plots, "rufa_duration_movement_type_rufa.csv"))
 
# ## generate a paired down version of the stopover locations for mapping only not for analysis 
 df_stopover_subset <- st_read(file.path(out.plots , "rufa_stopovers.gpkg"))


#############################################################################

# Western

##############################################################################

wgwp_id <- pop_id %>% filter(subpop == "West" ) |> arrange(type)

# all locations 
wgwp <- df_all %>% 
  filter(tag.id %in% wgwp_id$tag.id) %>%
  mutate(movement_final = case_when(
  movement_final == "deployment" ~ "north_migration",
  .default = movement_final
)) %>% 
  filter(movement_final != "north_migration") |> 
  filter(movement_final != "south_migration") 
  
wgwp$movement_final <- factor(wgwp$movement_final, levels=c("north_stopover", "breeding","south_stopover", "wintering"))

df_all_seg <- df_all %>%
  filter(tag.id %in% wgwp_id$tag.id) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  arrange(tag.id, date_time, tag.id.order) %>%
  group_by(tag.id) %>%
  mutate(lon_end = lead(lon),
         lat_end = lead(lat)) %>%
  ungroup() %>%
  filter(!is.na(lon_end))

## western duration for tags 
durw <- dur |> 
  filter(tag.id %in% wgwp_id$tag.id) |> 
  arrange(desc(duration))


# locations of deployment 
wgwp_id |> 
  group_by(study.site) |> 
  count()


# stopoverlocations
wgwp_stopover <- df_stopover_subset |> 
  filter(tag.id %in% wgwp_id$tag.id) %>%
 # mutate(movement_final = case_when(
#    movement_final == "deployment" ~ "north_stopover",
#    .default = movement_final
#  )) %>% 
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)


#wgwp_stopover_test <- wgwp_stopover |> 
#  filter(tag.id == 228177)

wgwp_dur <- dur_type_move %>% 
  filter(tag.id %in% wgwp_id$tag.id)

# add lines 
#tracks <- wgwp
wgwp_seg <- wgwp %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  arrange(tag.id, date_time, tag.id.order) %>%
  group_by(tag.id) %>%
  mutate(lon_end = lead(lon),
         lat_end = lead(lat)) %>%
  ungroup() %>%
  filter(!is.na(lon_end))



# Geographic distributon of tags ## figure 6 

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
#  geom_segment(data = wgwp_seg,
#               aes(x = lon, y = lat, xend = lon_end, yend = lat_end,
##                   colour = movement_final),
#               alpha = 0.3, linewidth = 0.4,
#               inherit.aes = FALSE, show.legend = FALSE) +
  geom_segment(data = df_all_seg,
               aes(x = lon, y = lat, xend = lon_end, yend = lat_end,
                   colour = movement_final),
               alpha = 0.3, linewidth = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  geom_sf(data = wgwp, size = 2.5,  aes(colour = movement_final)) +#colour = "dark blue") +
  #scale_color_viridis_d(name = "Movement Type") +
  scale_color_manual(values = color_mapping,name = "Movement Type")+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())


global

ggsave(file.path(out.plots,"fig11_west_stopovers_combined.jpg"), width = 20, height = 20,units = "cm", dpi = 600)



### Figure 7 

# Geographic distributon of all tag (all stopover data) tags 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = wgwp, size = 2.5, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_manual(values = color_mapping,name = "Movement Type")+
  geom_segment(data = df_all_seg,
               aes(x = lon, y = lat, xend = lon_end, yend = lat_end,
                   colour = movement_final),
               alpha = 0.3, linewidth = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig12_west_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


### Figure 8 

#filtered breedign locations 
wgwp_breed <- wgwp |> 
  group_by(tag.id) |>
  filter(movement_final == "breeding") |> 
  slice_head(, n = 1)

wgwp_other <- wgwp_stopover |> 
  filter(movement_final != "breeding") |> 
  filter(movement_final != "deployment")    

wgwp_breed <- bind_rows(wgwp_breed, wgwp_other)


## Breeding locations 

wgwp_breed_ids <- unique(wgwp_breed$tag.id)
df_all_seg_breed <- df_all_seg |> 
  filter(tag.id %in% wgwp_breed_ids)

library(rnaturalearth)

islands <- tibble::tribble(
  ~name,                     ~lon,    ~lat,
  "Banks I.",              -121.5,   73.0,
  "Victoria I.",           -110.0,   70.5,
  "Melville I.",           -111.5,   75.3,
  #"Prince Patrick I.",     -119.5,   76.8,
  "Bathurst I.",            -99.5,   75.8,
  "Prince of Wales I.",     -99.0,   72.8,
  "Somerset I.",            -93.3,   73.2,
  "Devon I.",               -88.0,   75.3,
  "King William I.",        -97.5,   69.0,
  "Southampton I.",         -84.5,   64.5,
  "Coats I.",               -82.5,   62.5,
  "Prince Charles I.",      -76.2,   67.8,
  "Baffin I.",              -70.0,   68.5,
  "Bylot I.",               -78.6,   73.2
)
# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = wgwp_breed, size = 2.5, aes(colour= movement_final)) +#colour = "dark blue") +
  #scale_color_viridis_d(name = "Movement Type") + 
  geom_segment(data = df_all_seg_breed,
               aes(x = lon, y = lat, xend = lon_end, yend = lat_end,
                   colour = movement_final),
               alpha = 0.3, linewidth = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  scale_color_manual(values = color_mapping,name = "Movement Type")+
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-125, -60), ylim = c(55, 79), expand = FALSE)+
  theme_bw()+
  geom_text(data = islands,
            aes(x = lon, y = lat, label = name),
            inherit.aes = FALSE,
            colour = "grey25", size = 3, fontface = "italic")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #legend.title = "", 
    #legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global

ggsave(file.path(out.plots,"fig11_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)




########################################################################
# Map by month 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = wgwp, size = 2.5, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~month)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

#ggsave(file.path(out.plots,"fig12_west_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)
















############## Migration map ###################

wgwp_stopover <- cbind(wgwp_stopover, st_coordinates(wgwp_stopover))

library(leaflet)

pal <- colorFactor(
   palette = "viridis",
   domain = unique(wgwp_stopover$tag.id))

birdmapall <- leaflet(wgwp_stopover) %>%
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = wgwp_stopover$X, lat = wgwp_stopover$Y, 
                   weight = 4, color = ~pal(wgwp_stopover$tag.id), 
                   fill = TRUE,
                   label = ~tag.id,
                   radius = ~2 ,
                   popup = ~ tag.id) %>%
addPolylines(data = wgwp_stopover, lng = wgwp_stopover$X, lat = wgwp_stopover$Y,
             color = "grey",   opacity = 0.1, stroke = TRUE)# %>%

birdmapall




# # date depart for spring migration 
# Texax
# 2021 May 18 - 201135 - via Prairies + hudson Bay 
# 2020 May 15 - 201139 - via Prairies returned 2020 July 19 
# 2023 May 24 - 228166 - via Prairies - returned august 9th
# 
# New Orleans
# 2021 May 18 - 201151 - via prairies
# 2021 May 19 - 201140 - via prairies
# 2021 May 21 - 201137 - short
# 
# 2021 May 20 - 201146 - via Hudson Bay - return 08 28th
# 2021 May 26 - 201150 - via Hudson Bay 
# 2021 May 28 - 201160 - via Hudson Bay 
# 2021 May 30 - 201159 - via Hudson Bay 
# 2021 May 30 - 201163 - via Hudson Bay 
# 2021 May 31 - 201143 - via Hudson Bay 
# 2021 May 30 - 201145 - via Hudson Bay 
# 2021 June 05 - 201165 - via Hudson Bay 


## date for arriving at breeding areas 
wgwp_breed_arrive <- wgwp |> 
  group_by(tag.id) |>
  filter(movement_final == "breeding") |> 
  slice_head(, n = 1) |> 
  select(tag.id, date_time,  year,month, day)

# victoria is : June 8th - 24th arrival 
# east arctic (228177) : June 18th  
# king william : grouped with Victoria is 



wgwp_breed_depart <- wgwp |> 
  group_by(tag.id) |>
  filter(movement_final == "breeding") |> 
  slice_tail(, n = 1) |> 
  select(tag.id, date_time,  year,month, day)

# birds whco departed sth  n = 6

#201139 via centre (no breeding) # discussed 
#201146 via Hudson Bay  - 58 days breeding depart Aug 11th 
#228166 via Hudson Bay - 40 days breeding depart Jul 18th
#228170 via hudson Bay  - 44 days June 14th - depart July 17th 
#228184 via hudson Bay - 37 days June 18th - depart July 6th
#232599 via hudson bay - 46 days June 24th - depart July 9th 




