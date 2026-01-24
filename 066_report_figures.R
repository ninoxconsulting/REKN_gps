# generate figures for report 

library("rnaturalearth")
library("rnaturalearthdata")
library (ggplot2)
library(readr)
library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures_2026")


# read in the ref data
ref <- read_xlsx(file.path(final_dat, "reference_data_raw_2020_2025_edited.xlsx"))

# read in combined location data 
loc <- read_csv(file.path(final_dat, "location_data_2017_2025.csv"))

# read in moveclass data 
df_all <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_edited.gpkg"))


# filter the uncertain locations 

unique(df_all$movement_final)

rf_sf <- df_all %>% 
  filter(movement_final != "uncertain_location")



######################################################

# figure 1 over view of all points (filtering the )

# Geographic distributon of tags
world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")%>% 
  select(admin)

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 1.2, alpha = 0.2, colour = "dark blue") +
  #facet_wrap(~movement_dir)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-185, -20), ylim = c(-60, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global
ggsave(file.path(out.plots,"figure1_all_reknnoalpha0.2.jpg"), width = 30, height = 30,units = "cm", dpi = 600)

