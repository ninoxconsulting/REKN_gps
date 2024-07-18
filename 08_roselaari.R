
library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)


final_dat <- file.path("../../02_data/REKN_gps/output_final")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures")


# read in the ref data
ref <- read_csv(file.path(final_dat, "reference_data_edited.csv"))
loc <- read_csv(file.path(final_dat, "location_data_raw.csv"))
df_subset <- st_read(file.path(final_dat,"rekn_moveclass_20240716_usable.gpkg" ))


# read in the stopover data
ref_key <- read_csv(file.path(final_dat, "final_tags_list_edited.csv"))
ref_id <- ref_key  %>% select("tag.id" , "proj", "subspecies", "subpop", 
                              "north", "breeding" , "south","wintering" ,  
                              "type", "usable"  )



allsf <- left_join(df_subset, ref_id)%>% 
  filter(movement_final !="uncertain_location")



###############################################################
# create a basic plot 

world <- ne_countries(scale = "medium", returnclass = "sf")
#Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America 
global <- ggplot(data = world) +
  geom_sf(color = "grey") +
  geom_sf(data = allsf, size = 1, colour = "darkblue" , alpha = 0.3) + #aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_color_viridis_d(option = "magma",begin = 0.1)+
  facet_wrap(~subspecies)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-180, -20), ylim = c(-60, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

## output rose figure 5. 

ggsave(file.path(out.plots,"figure5_rose_rufa.jpg"), width = 30, height = 25,units = "cm", dpi = 600)


