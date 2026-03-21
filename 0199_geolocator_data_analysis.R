

## Compare teh geolocator data to GPS data 


####################################################################################
library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
#library(gganimate)
#library(ggspatial)

#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final/draft_outputs_2026")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures_2026")

library(readxl)
ref_geo <- read_csv(fs::path("C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\complete\\2021_NWRC\\04_deliverables\\data\\ReferenceData_copy.csv"))
ref_geo <-  ref_geo |> 
  select("Subpop", `animal-id`)


geo <- read_csv("C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\complete\\2021_NWRC\\04_deliverables\\data\\rekn_rufa_location_estimates_final.csv")

#unique(geo$`location lat`)
geo <- geo |>
  mutate(X = as.numeric(as.character("location lat"))) 

geo <- geo |> 
  filter(!is.na("location lat")) |> 
  filter(!is.na("location long")) 

geo <- geo %>% tidyr::drop_na("location lat") 

geo <- geo|> 
  left_join(ref_geo)

geosf <- st_as_sf(geo,  coords = c("location long", "location lat"), crs = 4326)
st_write(geosf,fs::path(final_dat, "geolocator_rufa.gpkg"), append = F)
      
geosf <- geosf |> 
  select(`animal-id`, `location description`, `arrive date` ,`depart date`,'year_arr', 'year_dep', south , north,Subpop ) |> 
  mutate(month = month(`arrive date`)) |> 
  filter(!is.na(month))
  
## PLOTS FOR ALL VALUES (includ uncertaon_)
#################################################################################

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
# Map by month 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = geosf, size = 1, alpha=0.8, aes(colour = month)) +#colour = "dark blue") +
  #scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~month)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-170, -25), ylim = c(-58, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global


################################################################################

## read in teh rufa gps data 

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

df_all <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_movetype_20260125.gpkg")) %>% 
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("West", "SE", "NSA", "South")) %>% 
  select(proj, tag.id, date_time, year, month, movement_final, subspecies, subpop, type)

df_all_points <- df_all


# read in duration (raw and compiled)
#dur <- read_csv (fs::path(final_dat, "duration_tags_2025_outliers_removed.csv"))

############################################################################
### SUB POPULATION REVIEW 
# read in duration 

dur_type_move <- read_csv(file.path(out.plots, "rufa_duration_movement_type_rufa.csv"))

## generate a paired down version of the stopover locations for mapping only not for analysis 

df_stopover_subset <- st_read(file.path(out.plots , "rufa_stopovers.gpkg"))%>%
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)%>% 
  left_join(pop_id) %>% 
  filter(subpop %in% c("West", "SE", "NSA", "South"))%>% 
  select(proj, tag.id, date_time, movement_final, subspecies, subpop, type) |> 
  mutate(month = month(date_time )) |> 
  filter(movement_final !="breeding") |> 
  filter(movement_final !="deployment") 

# plot 1 by movemnt type 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = df_stopover_subset, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

# plot 2 by month

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = df_stopover_subset, size = 1, alpha=0.7,colour = "dark blue") +
  #scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~month)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global


###########################################################

# merge the two data sets together 
df_stopover_subset <- df_stopover_subset |> 
  mutate(tag_type = "gps/satellite") 

st_geometry(df_stopover_subset)= "geometry"

geosf <- geosf |> 
  mutate(tag_type = "geolocator")

all <- bind_rows(df_stopover_subset,geosf)
all_subb <- all |> filter(month %in% c(4,5,6,7,8,9,10,11))


summm <- all |> group_by(tag_type,tag.id,`animal-id`) |> count() |> st_drop_geometry() |> 
  group_by(tag_type) |>  count()

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = all_subb, size = 1, alpha=0.5,aes(colour = tag_type)) +
  scale_color_viridis_d(name = "Tag Type", begin = 0.2, end = 0.7) + 
  facet_wrap(~month)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig40_geo_gps_stopovers_month.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


###################################


## all locations 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = all, size = 1, alpha=0.5,colour = "dark blue") +
  scale_color_viridis_d(name = "Tag Type", begin = 0.4, end = 0.9) + 
  facet_wrap(~tag_type)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global



ggsave(file.path(out.plots,"fig40_geo_gps_stopovers_all.jpg"), width = 30, height = 20,units = "cm", dpi = 600)

#"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"





###############################################################################


#### compare the subpopulations 


head(all)

unique(all$Subpop)
unique(all$subpop)

subpop <- all |> 
  mutate(subpop_all = case_when(
    Subpop =="TDF"  ~ 'South',
    Subpop =="NorthCoast SAM"  ~ 'NSA',
    Subpop =="West Gulf"  ~ 'West',
    Subpop =="South East Mainland Nth AM"  ~ 'SE',
    Subpop =="SE Carribean North America"  ~ 'SE',   
    subpop =='South'  ~ 'South',
    subpop =='NSA' ~ 'NSA',
    subpop =='West' ~ 'West',
    subpop =='SE'  ~ 'SE'
  ))

subpop |> group_by(subpop_all, tag_type) |> count()


## compare locations per subpopulation 


#South 
south <- subpop |> filter(subpop_all == "South")
summm <- south |> group_by(tag_type,tag.id,`animal-id`) |> count() |> st_drop_geometry() |> 
  group_by(tag_type) |>  count()

## all locations 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south, size = 1, alpha=0.5,colour = "dark blue") +
  scale_color_viridis_d(name = "Tag Type", begin = 0.4, end = 0.9) + 
  facet_wrap(~tag_type)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig40_geo_gps_south_all.jpg"), width = 20, height = 20,units = "cm", dpi = 600)

#"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"


##########################################################
#West
south <- subpop |> filter(subpop_all == "West") 

summm <- south |> group_by(tag_type,tag.id,`animal-id`) |> count() |> st_drop_geometry() |> 
  group_by(tag_type) |>  count()

## all locations 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south, size = 1, alpha=0.5,colour = "dark blue") +
  scale_color_viridis_d(name = "Tag Type", begin = 0.4, end = 0.9) + 
  facet_wrap(~tag_type)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig40_geo_gps_west_all.jpg"), width = 20, height = 20,units = "cm", dpi = 600)

#"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"




##################################################################


##########################################################
#NSA
south <- subpop |> filter(subpop_all == "NSA")

summm <- south |> group_by(tag_type,tag.id,`animal-id`) |> count() |> st_drop_geometry() |> 
  group_by(tag_type) |>  count()

## all locations 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south, size = 1, alpha=0.5,colour = "dark blue") +
  scale_color_viridis_d(name = "Tag Type", begin = 0.4, end = 0.9) + 
  facet_wrap(~tag_type)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig40_geo_gps_nsa_all.jpg"), width = 20, height = 20,units = "cm", dpi = 600)

#"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"


##################################################################
#SE
south <- subpop |> filter(subpop_all == "SE")
summm <- south |> group_by(tag_type,tag.id,`animal-id`) |> count() |> st_drop_geometry() |> 
  group_by(tag_type) |>  count()
## all locations 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south, size = 1, alpha=0.5,colour = "dark blue") +
  scale_color_viridis_d(name = "Tag Type", begin = 0.4, end = 0.9) + 
  facet_wrap(~tag_type)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig40_geo_gps_se_all.jpg"), width = 20, height = 20,units = "cm", dpi = 600)

#"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"

   
        


################################################################################
## long lived individuals 


# db_ivl 

ll <- all |> 
  filter(`animal-id` == "db_ivl") |> 
  select(`animal-id`, `arrive date`, `depart date`) |> 
  mutate(yearmo = paste0(year(`arrive date`),0, month(`arrive date`))) |> 
  mutate(year = year(`arrive date`))




## all locations 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = ll, size = 3, alpha=.8, aes(colour = yearmo)) + #colour = "dark blue") +
  scale_color_viridis_d(name = "Date") + 
  facet_wrap(~year)+
  xlab("Longitude") + ylab("Latitude") +
  #geom_sf_text(data = ll, aes(label = `arrive date`))+
  coord_sf(xlim = c(-120, -20), ylim = c(-30, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global


ggsave(file.path(out.plots,"fig50_geo_dv_ivl.jpg"), width = 20, height = 20,units = "cm", dpi = 600)













#####rekn_roselaari_daily_positions_johnson.csv
geo <- read_csv("C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\complete\\2021_NWRC\\04_deliverables\\data\\rekn_roselaari_daily_positions_johnson.csv")
geosf <- st_as_sf(geo,  coords = c("Median.long", "Median.lat"), crs = 4326)

geosf <- geosf |> 
  mutate(geo.id = Birdid) |> 
  select(Date, geo.id)
#st_write(geosf,fs::path(final_dat, "geolocator_rose.gpkg"), append = F)





