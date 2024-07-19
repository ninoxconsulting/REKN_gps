

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


# read in duration (raw and compiled)

dur <- read_csv( file.path(out.plots, "duration_per_tag_353.csv"))
dur_type_move <- read_csv(file.path(out.plots, "rufa_duration_movement_type_rufa.csv"))


## generate a paired down version of the stopover locations for mapping only not for analysis 
df_stopover_subset <- st_read(file.path(out.plots , "rufa_stopovers.gpkg"))



#############################################################################

# Western

##############################################################################

wgwp_id <- pop_id %>% filter(subpop == "west" ) |> arrange(type)

# all locations 
wgwp <- df_all %>% 
  filter(tag.id %in% wgwp_id$tag.id) %>%
  mutate(movement_final = case_when(
  movement_final == "deployment" ~ "north_migration",
  .default = movement_final
)) %>% 
  filter(movement_final != "north_migration") |> 
  filter(movement_final != "south_migration") 
  


# stopoverlocations
wgwp_stopover <-df_stopover_subset |> 
  filter(tag.id %in% wgwp_id$tag.id) %>%
  mutate(movement_final = case_when(
    movement_final == "deployment" ~ "north_stopover",
    .default = movement_final
  )) %>% 
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)


wgwp_dur <- dur_type_move %>% 
  filter(tag.id %in% wgwp_id$tag.id)

# Geographic distributon of tags ## figure 6 

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = wgwp_stopover, size = 2.5,  aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig6_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)



### Figure 7 

# Geographic distributon of all tag (all stopover data) tags 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = wgwp, size = 2.5, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig7_west_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


### Figure 8 

## Breeding locations 

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = wgwp_stopover, size = 3, aes(colour= movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-125, -60), ylim = c(55, 79), expand = FALSE)+
  theme_bw()+
  #labs(colour = "Type") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #legend.title = "", 
    #legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global

ggsave(file.path(out.plots,"fig9_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


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





