
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


# read in duration (raw and compiled)

dur <- read_csv (fs::path(final_dat, "duration_tags_2025_outliers_removed.csv"))

dur_type_move <- read_csv(file.path(out.plots, "rufa_duration_movement_type_rufa.csv"))


# ## generate a paired down version of the stopover locations for mapping only not for analysis 
df_stopover_subset <- st_read(file.path(out.plots , "rufa_stopovers.gpkg"))


#############################################################################

# south  east

##############################################################################

se_id <- pop_id %>% filter(subpop == "SE" ) |> arrange(type)

# type of tags 
se_id |> group_by(tag.model) |> count()

# all locations 
se <- df_all %>% 
  filter(tag.id %in% se_id$tag.id) %>%
  #mutate(movement_final = case_when(
  #  movement_final == "deployment" ~ "north_migration",
  #  .default = movement_final
  #)) %>% 
  filter(movement_final != "north_migration") |> 
  filter(movement_final != "south_migration") 


## southern duration for tags 
durs <- dur |> 
  filter(tag.id %in% se_id$tag.id) |> 
  arrange(desc(duration))

# locations of deployment 
se_id |> 
  group_by(study.site) |> 
  count()

se_dur <- dur_type_move %>% 
  filter(tag.id %in% se_id$tag.id)

# stopoverlocations
se_stopover <-df_stopover_subset |> 
  filter(tag.id %in% se_id$tag.id) %>%
# mutate(movement_final = case_when(
#    movement_final == "deployment" ~ "north_stopover",
#    .default = movement_final
#  )) %>% 
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)



########################################################
# Geographic distributon of tags ## figure 11 = COmBINED

# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# Americas <- world %>% dplyr::filter(region_un == "Americas")
# #Americas <- world %>% dplyr::filter(continent == "North America")
# 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = south, size = 1, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
#   scale_color_viridis_d(name = "Movement Type") + 
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# global
# 
# ggsave(file.path(out.plots,"fig11_south_stopovers__fall_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)



########################################################
# Geographic distributon of tags ## figure 16- spring

# figure 12  ######### north stopovers 

se_stopover_spring <- se |> 
  filter(movement_final != "south_stopover")|> 
  filter(movement_final != "deployment")

# pair down the breeding  and select single locaion select only one breeding location for clarity
se_breed <- se_stopover_spring %>% filter(movement_final == "breeding")|>
  group_by(tag.id) |>
  filter(movement_final == "breeding") |>
  slice_head(, n = 1)

# join back 
se_stopover_spring <- se_stopover_spring |> 
  filter(movement_final != "breeding")

se_stopover_spring <-  bind_rows(se_stopover_spring, se_breed )

# pair down the winterign  and select single locaion select only one breeding location for clarity
se_winter <- se_stopover_spring %>% filter(movement_final == "wintering")|>
  group_by(tag.id) |>
  filter(movement_final == "wintering") |>
  slice_head(, n = 1)

# join back 
se_stopover_spring <- se_stopover_spring |> 
  filter(movement_final != "wintering")

se_stopover_spring <-  bind_rows(se_stopover_spring, se_winter )

########################################################

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se_stopover_spring, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -40), ylim = c(10, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig21_se_stopovers__fall_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)





###########################################################
# Geographic distributon of tags ## figure 16 - fall

se_stopover_fall <- se |> 
  filter(movement_final != "north_stopover") |> 
  filter(movement_final != "deployment")

# pair down the breeding  and select single locaion select only one breeding location for clarity
se_breed <- se_stopover_fall %>% filter(movement_final == "breeding")|>
  group_by(tag.id) |>
  filter(movement_final == "breeding") |>
  slice_head(, n = 1)

# join back 
se_stopover_fall <- se_stopover_fall |> 
  filter(movement_final != "breeding")

se_stopover_fall <-  bind_rows(se_stopover_fall, se_breed)

# pair down the winterign  and select single locaion select only one breeding location for clarity
se_winter <- se_stopover_fall %>% filter(movement_final == "wintering")|>
  group_by(tag.id) |>
  filter(movement_final == "wintering") |>
  slice_head(, n = 1)

# join back 
se_stopover_fall <- se_stopover_fall |> 
  filter(movement_final != "wintering")

se_stopover_fall <-  bind_rows(se_stopover_fall, se_winter )


##################################

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se_stopover_fall, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -40), ylim = c(5, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig21_south_stopovers_spring_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)




###############################################################################

# # 
# # ### Figure 17
# # 
# # # Geographic distributon of all tag (all stopover data) tags 
# # 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = se, size = 2, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
#   scale_color_viridis_d(name = "Movement Type") +
#   facet_wrap(~tag.id)+
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-130, -40), ylim = c(5, 80), expand = FALSE)+
#  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# global
# # 
# # #ggsave(file.path(out.plots,"fig12_south_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)
# # 

###############################################################################
# 
# ### Figure 8 
# 
# ## Breeding locations 
# 
# se_breed <- se_stopover %>% filter(movement_final == "breeding")
# 
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = se_breed, size = 2.1, aes(colour= tag.id)) +#colour = "dark blue") +
#   scale_color_viridis(name = "tag ID") + 
#   #facet_wrap(~movement_final)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   # xlab("Longitude") + ylab("Latitude") +
#   #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
#   theme_bw()+
#   #labs(colour = "Type") + 
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_blank()
#     #legend.title = "", 
#     #legend.position = "bottom",
#     #legend.key.width = unit(3, "lines")
#   )
# 
# global
# 
# ## alteernate breding plot 
# 
# se_breed <- se_stopover %>% filter(movement_final == "breeding")
# 
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = se_breed, size = 3, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
#   #scale_color_viridis_d(name = "Movement Type") + 
#   #geom_sf(data = south_breed, size = 1.5, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
#   scale_color_viridis_d(name = "Tag ID") + 
#   #scale_color_brewer(palette = "Spectral", name = 'Tag ID')+
#   #facet_wrap(~movement_final)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   # xlab("Longitude") + ylab("Latitude") +
#   #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   coord_sf(xlim = c(-120, -70), ylim = c(59, 78), expand = FALSE)+
#  # coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
#   theme_bw()+
#   #labs(colour = "Type") + 
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_blank()
#     #legend.title = "", 
#     #legend.position = "bottom",
#     #legend.key.width = unit(3, "lines")
#   )
# 
# global
# 
# #ggsave(file.path(out.plots,"fig9_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)
# 



## Breeding locations - alternate 
# pair down the breeding  and select single locaion select only one breeding location for clarity
se_breed <- se %>% filter(movement_final == "breeding")|>
  group_by(tag.id) |>
  filter(movement_final == "breeding") |>
  slice_head(, n = 1)


# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se_breed, size = 2, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Tag ID") + 
  coord_sf(xlim = c(-120, -70), ylim = c(59, 78), expand = FALSE)+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

global

ggsave(file.path(out.plots,"fig22_se_breeding.jpg"), width = 25, height = 25,units = "cm", dpi = 600)


# 
# 
# ## WINTERING 
# 
# se_winter <- se %>% filter(movement_final == "wintering")
# 
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = se_winter, size = 1.5, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
#   scale_color_viridis_d(name = "Tag ID") + 
#   #facet_wrap(~movement_final)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   # xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   #coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
#   theme_bw()+
#   #labs(colour = "Type") + 
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_blank()
#     #legend.title = "", 
#     #legend.position = "bottom",
#     #legend.key.width = unit(3, "lines")
#   )
# 
# global

#ggsave(file.path(out.plots,"fig9_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)





############## Migration map ###################

south_stopover <- cbind(se_stopover, st_coordinates(se_stopover))

library(leaflet)

pal <- colorFactor(
  palette = "viridis",
  domain = unique(south_stopover$tag.id))

birdmapall <- leaflet(south_stopover) %>%
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = south_stopover$X, lat = south_stopover$Y, 
                   weight = 4, color = ~pal(south_stopover$tag.id), 
                   fill = TRUE,
                   label = ~tag.id,
                   radius = ~2 ,
                   popup = ~ tag.id) %>%
  addPolylines(data = south_stopover, lng = south_stopover$X, lat = south_stopover$Y,
               color = "grey",   opacity = 0.1, stroke = TRUE)# %>%

birdmapall



####################################################

## SE migration spring north 

# n = 21
sth.tg <- c(221858, 224080,224082,224088,233931,234376,236444,221844,221845,221847,221850,221856,
            221860,221863,221866,234370,240171,260808,260809,260810,240175)
  
# n = 27
nth.tgs <- c(213829, 213833, 230306,281664,282295,282306,213830,213831,260817,282286,
             282309, 260692, 260812, 282291,213834,242656,242657,242658,260688,260689,281662,
             281663, 282283,282289, 282294,282297, 282310)


##############################################################################
## north bound tags 

### split into two groups 

se_fig <- se |> 
  mutate(movement_final = case_when(
    movement_final == "deployment" ~ "north_stopover",
    .default = movement_final
  )) 
  

### split into two groups 

st.nth <- se_fig |> 
  filter(tag.id %in% nth.tgs)

# locations of deployment 
se_id |> 
  filter(tag.id %in% nth.tgs) |> 
  group_by(study.site) |> 
  count()


# Geographic distributon of nth tags all stopover data) tags (n = 25)

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = st.nth, size = 2, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
   coord_sf(xlim = c(-130, -50), ylim = c(10, 80), expand = FALSE)+
   theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig22_se_nthmigration_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


# Repeat for southwards birds 

## still to do ...

se_fig <- se |> 
  mutate(movement_final = case_when(
    movement_final == "deployment" ~ "south_stopover",
    .default = movement_final
  )) 


### split into two groups 

st.nth <- se_fig |> 
  filter(tag.id %in% sth.tg)

# locations of deployment 
se_id |> 
  filter(tag.id %in% sth.tg) |> 
  group_by(study.site) |> 
  count()


# Geographic distributon of nth tags all stopover data) tags (n = 25)

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = st.nth, size = 2, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type", begin = 0.5) + 
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -50), ylim = c(10, 60), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig23_se_sthmigration_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)



###########################################################

# Map by month 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se, size = 2.5, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~month)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -50), ylim = c(10, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global




## detailed descriptions 


##### Birds ######
## PARTIAL FALL (n = 21)
# 221858 - delaware bay (deploy) Nov 12-13) - sth Florida (Daytona Beach)(nov 15 - 26) # tag dies 
# 224080- atlantic city (del Bay) Deployed (August 21 - 22) - sth north Charleston, Sth Carolina (Aug 25 - Sept 10 ) - Bahamas (Sep 14 - 20) - tags dies 
# 224082 -atlantic shores - Deployed (August 21 - 28) - sth Sapelo Island WMA, Georgia (August 30 - sept 12) -  Cuba   (Sept 12 - sept 24 )
# 224088 -atlantic shores - Deployed (August 27 - 29) - sth to Cuba  (Sep 1- 26) - tags dies 
# 233931 -Atlantic Coast - deployed (Sept 30 - Oct 11) - Long Island New York (Oct 11 - 26 )
# 234376 - Atlantic Coast deployed (August 15- Oct 19) - two distinct stopovers Stone harbour and Brigantine
# 236444 - deploy Monomoy NJ, August 27 - 30 - Barbuda is Sept 3 - 25- depart directly south (unusual for SE) more similar to NSA migration pattern?
# 221844 - atlantic shores - Deployed (nov 12 - 19) - Southport, NOrth Carolina (nov   Dec 6) # tag dies 
# 221845- atlantic shores - Deployed (nov 12 - 18) - Pamlico sound, NOrth Carolina (nov 19  Dec 5) # tag dies 
# 221847 - atlantic shores - Deployed (nov 12 - 22) - Pamlico sound, NOrth Carolina (nov 11  - 25 ) # tag dies 
# 221850- atlantic shores - Deployed (nov 12 - 13) - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies
# 221856- atlantic shores - Deployed (nov 12 - 13) - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies 
# 221860- atlantic shores - Deployed (nov 12 - 13) - Exmore, Virginia (nov 13- 14 ) # tag dies 
# 221863- atlantic shores - Deployed (nov 12 - 22) - cAPE cARNAVERAL, Florida  (nov 24 - 26 ) # tag dies 
# 221866 - atlantic shores - Deployed (nov 12 - 13) - north Charlston, Sth Carolina (nov 16 - Dec 10 )
# 234370 - Atlantic Coast deployed (August 26 -Dec 4) - Bahamas (Dec 6 - 14) - tags dies 
# 240171 - Del bay (Oct 2 - 15) - sth to Virginia (Oct 16 - 30) - north carolina (nov 9 - Feb 16 (next yr)- tag ends ? dropped?
# 260808 - Del Bay (Oct 2 - 25) - sth to Nth carolina (Oct 25 - Nov 20) - tag dies 
# 260809 - Del Bay (Oct 2 - Nov 9) - to sth carolina (Nov 9 - Nov 26) - tag dies 
# 260810 - Del Bay (Oct 2 - Oct 24) - to Nthcarolina (Oct 24 - Nov 14) - tag dies 
# 240175 - Del bay (Oct 2 - 27) - multiple stops short georgia - florida (Oct 29 - June 3 - tag dropped?

## SPRING (n = )
# 213829 - Del Bay deploy (May 22 - 28) - head north (toronto - tag died)
# 213833 - Del Bay depart (May 28th) -  East James Bay (may 29/30 - dropped tag here) 
# 230306 - Del Bay depart (June 1 ) - go west - sth carolina (June 4 - july 10) tag drops here
# 281664 - Sth carolina (march 31 - may 5) - del bay (May 5-28)- james bay (may 29 - sep 11 - dropped tag?)
# 282295 - Sth carolina (march 31 - may 20) - james bay (may 22 - june 30 - dropped tag?)
# 282306 - Sth carolina (march 31 - may 25) - del bay (may 26) - james bay (may 29 - June 8 - dropped tag?)
# 213830 - Del Bay depart (May 24th)- JAMES BAY (May 29 - June 6 ) ~7 days - multiple stops on Nelson River, Arviat - (June 7) - stopover King william (June 12) - Vic Isalnd (breeding ground - arr June 20 - Sept 8th - dropped tag here)
# 213831 - Del Bay depart (May 28th)- James Bay (may 30 - June 10) - 10 days - Coats Isalnd (June 11 - June 16 ) - last transmission (possible breeding?)
# 260817 - sth Carolina (march 31 - may 20)- james bay (May 22-June 7) - prince of wales (june 9 - ongoing (tag dropped))
# 282286 - sth Carolina (march 31 - may 21)- james bay (May 22-June 6) - Victoria Is (june 8 - ongoing (tag dropped))
# 282309 - sth Carolina (march 31 - may 21)- james bay (May 22-June 6) - Hudson bay (June 6 - june 14) - King william (june 16 - ongoing (tag dropped)
# 260692 - sth Carolina (May 16 - may 18)- james bay (May 20-June 3) - Baffin is(June 6 - june 16) - Prince charles Is (east arctic) (june 17 - July 24) - sth to st laurence Qc (July 26 - Aug 4) - del bay (Aug 7-10) - sth carolina (same location (aug 8 -sep 9) - down and back up north to Cuba (Sep 11- Oct 21 -tag ends)
# 260812 - sth Carolina (march 31 - may 25)- james bay (May 27-June 3) - Hudson Bay (JUne 3 - 13) - Matty Is(june 17 - july 18)- hudson bay (july 18-28) -sth carolina (same location (aug 5 - oct 19) - tag dies
# 282291 - sth Carolina (may 18 - 20)- james bay (May 24-June 6) - prince wales (june 9 - july 12)- james Bay (july 15-aug 3) - turks caycos (aug 8 - Dec 4) - tag dies? potential other types
# 213834 - Delaware bay (may 28 -june 1)- Hudson Bay (june 8-10), Vic Is (june 15 - jul 14) - Hudson Bay (july 18-20) - dies here
# 242656- sth carolina Kiawah Beach (May 23) - EAST JAMES BAY (May 24 - June 3 ) _ 9 days - HB Nelson River (June 4-7) - multiple short stops Queen Maud Gulf Bird Sanctuary - Vic Island - breeding ground (June 10 - August 3) - hudson Bay(aug 8-16) - sth Carolina (Aug 20 -continued - tag dies Dec 16)
# 242657- sth carolina Kiawah Beach (May 22) - Del Bay (May 25 - May 28) - E. James Bay - (june 1 -6) -5 days - Hiurarryuaa / King William Isalnad (june 7 - 9) - Breeding : Prince of Wales june 9 - August 4)- brd of Nunavut and manitoba (August 6 -7 )-WEST JAMES BAY (August 8 - 19) - Sth Caroline August 20 - Oct 8- Georgia - (OCt 8 - 31)
# 242658 -sth carolina Kiawah Beach (May 13) - Monom (May 13 - 29) - E. James Bay - (May 31 - june 6) 7 days - Baffin Is (June 7 -15) - Breeding : prince Charles Is (June 16 - August 10) - Westn James Bay (Akimiski island bird Sanctuary - (August 12 -24) - Flew direct to Cuba (August 28 - Oct 7)
# 260688 -sth carolina Kiawah Beach (May 16-21) - E. James Bay - (May 22 - june 5) - Nun(june 5-10) #### (June 7 -15) - Vic Is (june 12 - july 11)  - James Bay (july17 -Aug 3) - Georgia (Aug 8-11) tag ends
# 260689-sth carolina Kiawah Beach (May 16-21) - E. James Bay - (May 23 - june 5) - Vic Is (june 5 - july 18)  - HUdson bay  (july23-aug 1) - Georgia (Aug 8-22) - Florida (aug 22 - sep 9)
# 281662- sth Carolina (march 31 - may 23)- james bay (May 28-June 6) - sth Hudson Bay (JUne 3 - 13) west Hidson Bay (june 7-15) - Vic is (june 26- july 9)- king will (July 12-22) - west hudson bay (july 23-26) -sth hudson bay (july 28- NOv - tag dropped here
# 281663 - sth Carolina (multi stops) (march 31 - may 20)- james bay (May 22-June 6) - sthampton is (june 7- july 9) -sth james bay (july 15-20) - sth carolina (july 22- aug 19)  - tag ended here
# 282283-  sth Carolina (May 18-20)- james bay (May 22-June 7)  - king william (june 9- july 3) - James Bay ( july 5- 29) - Cuba (Aug 1- Oct 12 (tag dropped?)) 
# 282296 - POTENTIAL NSA? - sth Carolina (may 18 - 20)- james bay (May 22-June 5) - Vic SI (june 7 - july 15)-- hudson bay (July 21 - 28) -  james Bay (july 29-aug 3) - st carolina (aug 5 - 19) - venzuaela (aug 21 - nov 11) - tag dies? potential other types
# 282288 - POTENTIAL NSA? - sth Carolina (may 18 - 20)- del bay (may 20 -22) -  james bay (May 24-June 7) - Vic SI (june 8 - july 17)-- hudson bay (July 21 - aug 10) - open ocean like NSA - venzuaela (aug 15 - oct 9) - tag dies? potential other types
# 282289 - sth Carolina (may 18 - 20)- del bay (may 20 -22) -  james bay (may 22- june 7) - prince of wales(june 10-july 13)- james bay (july 14-30) - sth carolina (aug 1 - 12) - dom repulblic(Aug 14-sept 21)- tag dies? 
# 282294 - sth Carolina (march 31 - may22)- del bay (may 23 -26) - james bay (May 28-June 6) - king william is (june 8- july 13) -sth hudson bay (july 17- aug13)- mulitple short stops sth carolina, floridoa, - cuba (aug 18 - sep 13)- tag ends     tag dropped here
# 282297 - sth Carolina (march 31 - may24)- monomoy (may 25 -june 1) - james bay (June 3-7) - -sth hudson bay (july 7- 12) - king william is (june 18- july 13) -sth hudson bay (july 14-21) stops here
# 282310-  sth Carolina (march 31 - may20)- james bay (may 22- sept 9) tag dropped. stops here

















##northward - SPRING (n = 27)
# 213829 - Del Bay deploy (May 22-28) - head north (toronto - tag died)
# 213833 - Del Bay depart (May  28)   - James Bay (may 29/30 - dropped tag here) 
# 213830 - Del Bay depart (May 24)    - James Bay (May 29 - June 6 ) ~7 days - multiple stops on Nelson River, Arviat - (June 7) - stopover King william (June 12) - Vic Isalnd (breeding ground - arr June 20 - Sept 8th - dropped tag here)
# 213831 - Del Bay depart (May 28)    - James Bay (may 30 - June 10) - 10 days - Coats Isalnd (June 11 - June 16 ) - last transmission (possible breeding?)
# 213834 - Del bay    ( May 28 -june 1)- Hudson Bay (june 8-10), Vic Is (june 15 - jul 14) - Hudson Bay (july 18-20) - dies here
# 230306 - Del Bay depart (June 1)    - go west - sth carolina (June 4 - july 10) tag drops here

# 281664 - Sth carolina (march 31 - may 5)  - del bay (May 5-28)     - james bay (may 29 - sep 11 - dropped tag?)
# 282306 - Sth carolina (march 31 - may 25) - del bay (may 26)       - james bay (may 29 - June 8 - dropped tag?)
# 242657- sth carolina  (May 22) -          - Del Bay (May 25-28)    - E. James Bay - (june 1 -6) -5 days - Hiurarryuaa / King William Isalnad (june 7 - 9) - Breeding : Prince of Wales june 9 - August 4)- brd of Nunavut and manitoba (August 6 -7 )-WEST JAMES BAY (August 8 - 19) - Sth Caroline August 20 - Oct 8- Georgia - (OCt 8 - 31)
# 282289 - sth Carolina (may 18 - 20)       - del bay (may 20 -22)   -  james bay (may 22- june 7) 15 days- prince of wales(june 10-july 13)- james bay (july 14-30) - sth carolina (aug 1 - 12) - dom repulblic(Aug 14-sept 21)- tag dies? 
# 242658 -sth carolina  (May 13) - Monom (May 13 - 29)               - E. James Bay - (May 31 - june 6) 7 days - Baffin Is (June 7 -15) - Breeding : prince Charles Is (June 16 - August 10) - Westn James Bay (Akimiski island bird Sanctuary - (August 12 -24) - Flew direct to Cuba (August 28 - Oct 7)
# 282294 - sth Carolina (march 31 - may22)- del bay (may 23 -26)     - james bay (May 28-June 6) - 9 days king william is (june 8- july 13) -sth hudson bay (july 17- aug13)- mulitple short stops sth carolina, floridoa, - cuba (aug 18 - sep 13)- tag ends     tag dropped here
# 282297 - sth Carolina (march 31 - may24)- monomoy (may 25 -june 1) - james bay (June 3-7) - 4 days -sth hudson bay (july 7- 12) - king william is (june 18- july 13) -sth hudson bay (july 14-21) stops here

# 282295 - Sth carolina (march 31 - may 20)                          - james bay (may 22 - june 30 - dropped tag?)
# 260817 - sth Carolina (march 31 - may 20)                          - james bay (May 22-June 7) ~ 16days- prince of wales (june 9 - ongoing (tag dropped))
# 282286 - sth Carolina (march 31 - may 21)                          - james bay (May 22-June 6) ~15days- Victoria Is (june 8 - ongoing (tag dropped))
# 282309 - sth Carolina (march 31 - may 21)                          - james bay (May 22-June 6) ~15 days - Hudson bay (June 6 - june 14) - King william (june 16 - ongoing (tag dropped)
# 260692 - sth Carolina (May 16 - may 18)                            - james bay (May 20-June 3) ~14 days- Baffin is(June 6 - june 16) - Prince charles Is (east arctic) (june 17 - July 24) - sth to st laurence Qc (July 26 - Aug 4) - del bay (Aug 7-10) - sth carolina (same location (aug 8 -sep 9) - down and back up north to Cuba (Sep 11- Oct 21 -tag ends)
# 260812 - sth Carolina (march 31 - may 25)                          - james bay (May 27-June 3) ~ 7days- Hudson Bay (JUne 3 - 13) - Matty Is(june 17 - july 18)- hudson bay (july 18-28) -sth carolina (same location (aug 5 - oct 19) - tag dies
# 282291 - sth Carolina (may 18 - 20)                                - james bay (May 24-June 6) - 13days prince wales (june 9 - july 12)- james Bay (july 15-aug 3) - turks caycos (aug 8 - Dec 4) - tag dies? potential other types
# 242656- sth carolina  (May 23)                                     -JAMES BAY (May 24 - June 3 ) _ 9 days - HB Nelson River (June 4-7) - multiple short stops Queen Maud Gulf Bird Sanctuary - Vic Island - breeding ground (June 10 - August 3) - hudson Bay(aug 8-16) - sth Carolina (Aug 20 -continued - tag dies Dec 16)
# 260688 -sth carolina  (May 16-21)                                  - James Bay - (May 22 - june 5) 14days- Nun(june 5-10) #### (June 7 -15) - Vic Is (june 12 - july 11)  - James Bay (july17 -Aug 3) - Georgia (Aug 8-11) tag ends
# 260689-sth carolina   (May 16-21)                                  - James Bay - (May 23 - june 5) 13days - Vic Is (june 5 - july 18)  - HUdson bay  (july23-aug 1) - Georgia (Aug 8-22) - Florida (aug 22 - sep 9)
# 281662- sth Carolina  (march 31 - may 23)                          - james bay (May 28-June 6) - 9 days-sth Hudson Bay (JUne 3 - 13) west Hidson Bay (june 7-15) - Vic is (june 26- july 9)- king will (July 12-22) - west hudson bay (july 23-26) -sth hudson bay (july 28- NOv - tag dropped here
# 281663 - sth Carolina (multi stops) (march 31 - may 20)            - james bay (May 22-June 6) - 15 days sthampton is (june 7- july 9) -sth james bay (july 15-20) - sth carolina (july 22- aug 19)  - tag ended here
# 282283-  sth Carolina (May 18-20)                                  -james bay (May 22-June 7)  - 16days king william (june 9- july 3) - James Bay ( july 5- 29) - Cuba (Aug 1- Oct 12 (tag dropped?)) 
# 282310-  sth Carolina (march 31 - may20)                           - james bay (may 22- sept 9) tag dropped. stops here

# 282296 - POTENTIAL NSA? - sth Carolina (may 18 - 20)- james bay (May 22-June 5) - Vic SI (june 7 - july 15)-- hudson bay (July 21 - 28) -  james Bay (july 29-aug 3) - st carolina (aug 5 - 19) - venzuaela (aug 21 - nov 11) - tag dies? potential other types
# 282288 - POTENTIAL NSA? - sth Carolina (may 18 - 20)- del bay (may 20 -22) -  james bay (May 24-June 7) - Vic SI (june 8 - july 17)-- hudson bay (July 21 - aug 10) - open ocean like NSA - venzuaela (aug 15 - oct 9) - tag dies? potential other types


# breeding - arrival 

## western arctic ####### ### arrive June 8 - 18 depart July 3-13) ##
# 282309 - sth Carolina - james bay (May 22-June 6)               - Hudson bay (June 6 - june 14)         - King william (june 16 - ongoing (tag dropped)
# 282283-  sth Carolina (May 18-20)- james bay (May 22-June 7)                                            - king william (june 9- july 3)       - James Bay ( july 5- 29) - Cuba (Aug 1- Oct 12 (tag dropped?)) 
# 282297 - sth Carolina - monomoy - james bay (June 3-7) -        -sth hudson bay (july 7- 12)            - king william (june 18- july 13)     -sth hudson bay (july 14-21) stops here
# 282294 - sth Carolina - del bay - james bay (May 28-June 6)                                             - king william  (june 8- july 13)     -sth hudson bay (july 17- aug13)- mulitple short stops sth carolina, floridoa, - cuba (aug 18 - sep 13)- tag ends     tag dropped here

### arrive June 5 - 26* depart July 11 - august 3) ##
# 213830 - Del Bay- James Bay   - multiple stops on Nelson River, Arviat -(June 7) -King william (June 12)    - Vic I( June 20 - Sept 8th - dropped tag here)
# 260689  -sth carolina - E. James Bay - (May 23 - june 5)                                                    - Vic Is (june 5 - july 18)       - HUdson bay  (july23-aug 1) - Georgia (Aug 8-22) - Florida (aug 22 - sep 9)
# 213834 - Delaware bay  -                                      - Hudson Bay (june 8-10),                     - Vic Is (june 15 - jul 14)       - Hudson Bay (july 18-20) - dies here
# 282286 - sth Carolina - james bay (May 22-June 6)                                                           - Vic Is (june 8 - ongoing (tag dropped))
# 242656- sth carolina- JAMES BAY - HB Nelson River (June 4-7) - multiple stops Queen Maud Gulf Bird Sanctuary- Vic Is (June 10 - August 3) - hudson Bay(aug 8-16) - sth Carolina (Aug 20 -continued - tag dies Dec 16)
# 260688 -sth carolina  - E. James Bay - (May 22 - june 5)          - Nun(june 5-10)                          - Vic Is (june 12 - july 11)  - James Bay (july17 -Aug 3) - Georgia (Aug 8-11) tag ends
# 281662- sth Carolina  - james bay (May 28-June 6) - sth Hudson Bay (JUne 3 - 13) west Hidson Bay (june 7-15)  - Vic is (june 26- july 9)- king will (July 12-22) - west hudson bay (july 23-26) -sth hudson bay (july 28- NOv - tag dropped here

### arrive June 9 - 17  depart July 12 - august 4) ##
# 260817 - sth Carolina - james bay (May 22-June 7)                                   - prince of wales (june 9 - ongoing (tag dropped))
# 282291 - sth Carolina - james bay (May 24-June 6)                                   - prince of wales (june 9 - july 12)         - james Bay (july 15-aug 3) - turks caycos (aug 8 - Dec 4) - tag dies? potential other types
# 282289 - sth Carolina - del bay  -  james bay (may 22- june 7)                      - prince of wales (june 10-july 13)          - james bay (july 14-30) - sth carolina (aug 1 - 12) - dom repulblic(Aug 14-sept 21)- tag dies? 
# 260812 - sth Carolina - james bay (May 27-June 3) - Hudson Bay (JUne 3 - 13)        - Matty Is        (june 17 - july 18)- hudson bay (july 18-28) -sth carolina (same location (aug 5 - oct 19) - tag dies
# 242657- sth carolina  Del Bay - James Bay (june 1 -6) King William (june 7 - 9)     - Prince of Wales (june 9 - August 4)- brd of Nunavut and manitoba (August 6 -7 )-WEST JAMES BAY (August 8 - 19) - Sth Caroline August 20 - Oct 8- Georgia - (OCt 8 - 31)


## eastern arctic ### June 7 -16  depart July 9 - august 10 )
# 213831 - Del Bay- James Bay (may 30 - June 10) - 10 days                                  - Coats Is          (June 11 - June 16 ) - last transmission (possible breeding?)
# 281663 - sth Carolina (multi stops) (march 31 - may 20)- james bay (May 22-June 6)        - sthampton is      (june 7- july 9) -sth james bay (july 15-20) - sth carolina (july 22- aug 19)  - tag ended here
# 260692 - sth Carolina - james bay (May 20-June 3)       - Baffin is (June 6 - 16)         - Prince charles Is (june 17 - July 24) - sth to st laurence Qc (July 26 - Aug 4) - del bay (Aug 7-10) - sth carolina (same location (aug 8 -sep 9) - down and back up north to Cuba (Sep 11- Oct 21 -tag ends)
# 242658 -sth carolina - Monom -James Bay  - Baffin Is (June 7 -15)                         -prince Charles Is (June 16 - August 10) - Westn James Bay (Akimiski island bird Sanctuary - (August 12 -24) - Flew direct to Cuba (August 28 - Oct 7)


# stopped at james bay 




## departure from breeding ##

# 282297 - sth Carolina - monomoy - james bay -sth hudson bay - king william (june 18- july 13) - hudson bay (july 14-21) stops here
# 282294 - sth Carolina - del bay - james bay - king william (june 8- july 13)                  - hudson bay (july 17- aug13)- mulitple stops sth carolina, floridoa, - cuba (aug 18 - sep 13)- tag ends     tag dropped here
# 260689  -sth carolina - E. James Bay - Vic Is (june 5 - july 18)                              - hudson bay (july23-aug 1)           - Georgia (Aug 8-22) - Florida (aug 22 - sep 9)
# 213834 - Delaware bay - Hudson Bay- Vic Is (june 15 - jul 14)                                 - Hudson Bay (july 18-20) - dies here
# 242656- sth carolina- JAMES BAY - HB Nelson River - Vic Is (June 10 - August 3)               - hudson Bay (aug 8-16)               - sth Carolina (Aug 20 -continued - tag dies Dec 16)
# 281662- sth Carolina  - james bay- sth H Bay- west Hidson Bay- Vic is- king will (July 12-22) - west hudson bay (july 23-26) -sth hudson bay (july 28- NOv - tag dropped here
# 260812 - sth Carolina - james bay (May 27-June 3) - Hudson Bay - Matty Is (june 17 - july 18) - hudson bay (july 18-28)             -sth carolina (same location (aug 5 - oct 19) - tag dies

# 242657- sth carolina  Del Bay - James Bay- King William  - Prince of Wales (june 9 - August 4)- james bay (August 8 - 19)        - Sth Caroline August 20 - Oct 8- Georgia - (OCt 8 - 31)
# 281663 - sth Carolina (multi stops)- james bay - sthampton is (june 7- july 9)                - james bay (july 15-20)          - sth carolina (july 22- aug 19)  - tag ended here
# 242658 -sth carolina - Monom -James Bay - Baffin Is-prince Charles Is (June 16 - August 10)   - james bay (August 12 -24)             - Flew direct to Cuba (August 28 - Oct 7)
# 282289 - sth Carolina - del bay  -  james bay  prince of wales (june 10-july 13)              - james bay (july 14-30)          - sth carolina (aug 1 - 12) - dom repulblic(Aug 14-sept 21)- tag dies? 
# 282291 - sth Carolina - james bay - prince of wales (june 9 - july 12)                        - james Bay (july 15-aug 3)             - turks caycos (aug 8 - Dec 4) - tag dies? potential other types
# 260688 -sth carolina  - E. James Bay - Nun - Vic Is (june 12 - july 11)                       - James Bay (july17 -Aug 3)       - Georgia (Aug 8-11) tag ends
# 282283-  sth Carolina - james bay- king william (june 9- july 3)                              - James Bay (july 5- 29)                - Cuba (Aug 1- Oct 12 (tag dropped?)) 

# 260692 - sth Carolina - james bay - Baffin is- Prince charles Is (june 17 - July 24)          - sth to st laurence Qc (July 26 - Aug 4) - del bay (Aug 7-10) - sth carolina (same location (aug 8 -sep 9) - down and back up north to Cuba (Sep 11- Oct 21 -tag ends)





# fall - southward 

# 224080 - atlantic city (del Bay) (August 21 - 22)         - sth north Charleston, Sth Carolina (Aug 25 - Sept 10 ) - Bahamas (Sep 14 - 20) - tags dies 
# 224082 - atlantic shores - Deployed (August 21 - 28)      - sth Sapelo Island WMA, Georgia (August 30 - sept 12) -  Cuba   (Sept 12 - sept 24 )
# 224088 - atlantic shores - Deployed (August 27 - 29)      - sth to Cuba  (Sep 1- 26) - tags dies 
# 233931 - Atlantic Coast - deployed (Sept 30 - Oct 11)     - Long Island New York (Oct 11 - 26 )
# 234376 - Atlantic Coast deployed (August 15- Oct 19)      - two distinct stopovers Stone harbour and Brigantine
# 236444 - deploy Monomoy NJ, August 27 - 30 -              - Barbuda is Sept 3 - 25- depart directly south (unusual for SE) more similar to NSA migration pattern?
# 234370 - Atlantic Coast deployed (August 26 -Dec 4)       - Bahamas (Dec 6 - 14) - tags dies 

# 221844 - atlantic shores - Deployed (nov 12 - 19)         - Southport,     NOrth Carolina (nov   Dec 6) # tag dies 
# 221845 - atlantic shores - Deployed (nov 12 - 18)         - Pamlico sound, NOrth Carolina (nov 19  Dec 5) # tag dies 
# 221847 - atlantic shores - Deployed (nov 12 - 22)         - Pamlico sound, NOrth Carolina (nov 11  - 25 ) # tag dies 
# 221850 - atlantic shores - Deployed (nov 12 - 13)         - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies
# 221856 - atlantic shores - Deployed (nov 12 - 13)         - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies 
# 260810 - Del Bay (Oct 2 - Oct 24)                         - Nthcarolina (Oct 24 - Nov 14) - tag dies 
# 260808 - Del Bay (Oct 2 - 25) -                           - Nth carolina (Oct 25 - Nov 20) - tag dies 

# 221866 - atlantic shores - Deployed (nov 12 - 13)         - Sth Carolina (nov 16 - Dec 10 )
# 260809 - Del Bay (Oct 2 - Nov 9)                          - to sth carolina (Nov 9 - Nov 26) - tag dies 

# 221860 - atlantic shores - Deployed (nov 12 - 13)         - Virginia (nov 13- 14 ) # tag dies 
# 240171 - Del bay (Oct 2 - 15)                             - Virginia (Oct 16 - 30) - north carolina (nov 9 - Feb 16 (next yr)- tag ends ? dropped?

# 221863 - atlantic shores - Deployed (nov 12 - 22)         - cAPE cARNAVERAL, Florida  (nov 24 - 26 ) # tag dies 
# 240175 - Del bay (Oct 2 - 27)                             - multiple stops short georgia - florida (Oct 29 - June 3 - tag dropped?
# 221858 - delaware bay (deploy) Nov 12-13)                 - sth Florida (Daytona Beach)(nov 15 - 26) # tag dies 


# 260692 - sth Carolina (May 16 - may 18)- james bay (May 20-June 3) - Baffin is(June 6 - june 16) - Prince charles Is (east arctic) (june 17 - July 24) - sth to st laurence Qc (July 26 - Aug 4) - del bay (Aug 7-10) - sth carolina (same location (aug 8 -sep 9) - down and back up north to Cuba (Sep 11- Oct 21 -tag ends)
# 260812 - sth Carolina (march 31 - may 25)- james bay (May 27-June 3) - Hudson Bay (JUne 3 - 13) - Matty Is(june 17 - july 18)- hudson bay (july 18-28) -sth carolina (same location (aug 5 - oct 19) - tag dies
# 282291 - sth Carolina (may 18 - 20)- james bay (May 24-June 6) - prince wales (june 9 - july 12)- james Bay (july 15-aug 3) - turks caycos (aug 8 - Dec 4) - tag dies? potential other types
# 213834 - Delaware bay (may 28 -june 1)- Hudson Bay (june 8-10), Vic Is (june 15 - jul 14) - Hudson Bay (july 18-20) - dies here
# 242656- sth carolina Kiawah Beach (May 23) - EAST JAMES BAY (May 24 - June 3 ) _ 9 days - HB Nelson River (June 4-7) - multiple short stops Queen Maud Gulf Bird Sanctuary - Vic Island - breeding ground (June 10 - August 3) - hudson Bay(aug 8-16) - sth Carolina (Aug 20 -continued - tag dies Dec 16)
# 242657- sth carolina Kiawah Beach (May 22) - Del Bay (May 25 - May 28) - E. James Bay - (june 1 -6) -5 days - Hiurarryuaa / King William Isalnad (june 7 - 9) - Breeding : Prince of Wales june 9 - August 4)- brd of Nunavut and manitoba (August 6 -7 )-WEST JAMES BAY (August 8 - 19) - Sth Caroline August 20 - Oct 8- Georgia - (OCt 8 - 31)
# 242658 -sth carolina Kiawah Beach (May 13) - Monom (May 13 - 29) - E. James Bay - (May 31 - june 6) 7 days - Baffin Is (June 7 -15) - Breeding : prince Charles Is (June 16 - August 10) - Westn James Bay (Akimiski island bird Sanctuary - (August 12 -24) - Flew direct to Cuba (August 28 - Oct 7)
# 260688 -sth carolina Kiawah Beach (May 16-21) - E. James Bay - (May 22 - june 5) - Nun(june 5-10) #### (June 7 -15) - Vic Is (june 12 - july 11)  - James Bay (july17 -Aug 3) - Georgia (Aug 8-11) tag ends
# 260689- sth carolina Kiawah Beach (May 16-21) - E. James Bay - (May 23 - june 5) - Vic Is (june 5 - july 18)  - HUdson bay  (july23-aug 1) - Georgia (Aug 8-22) - Florida (aug 22 - sep 9)
# 281662- sth Carolina (march 31 - may 23)- james bay (May 28-June 6) - sth Hudson Bay (JUne 3 - 13) west Hidson Bay (june 7-15) - Vic is (june 26- july 9)- king will (July 12-22) - west hudson bay (july 23-26) -sth hudson bay (july 28- NOv - tag dropped here
# 281663 - sth Carolina (multi stops) (march 31 - may 20)- james bay (May 22-June 6) - sthampton is (june 7- july 9) -sth james bay (july 15-20) - sth carolina (july 22- aug 19)  - tag ended here
# 282283-  sth Carolina (May 18-20)- james bay (May 22-June 7)  - king william (june 9- july 3) - James Bay ( july 5- 29) - Cuba (Aug 1- Oct 12 (tag dropped?)) 
# 282289 - sth Carolina (may 18 - 20)- del bay (may 20 -22) -  james bay (may 22- june 7) - prince of wales(june 10-july 13)- james bay (july 14-30) - sth carolina (aug 1 - 12) - dom repulblic(Aug 14-sept 21)- tag dies? 
# 282294 - sth Carolina (march 31 - may22)- del bay (may 23 -26) - james bay (May 28-June 6) - king william is (june 8- july 13) -sth hudson bay (july 17- aug13)- mulitple short stops sth carolina, floridoa, - cuba (aug 18 - sep 13)- tag ends     tag dropped here
# 282297 - sth Carolina (march 31 - may24)- monomoy (may 25 -june 1) - james bay (June 3-7) - -sth hudson bay (july 7- 12) - king william is (june 18- july 13) -sth hudson bay (july 14-21) stops here
# 282310-  sth Carolina (march 31 - may20)- james bay (may 22- sept 9) tag dropped. stops here






### wintering - only includes tags that had some Dec records 

# 221844 - atlantic shores - Deployed (nov 12 - 19) - Southport, NOrth Carolina (nov   Dec 6) # tag dies 
# 221845- atlantic shores - Deployed (nov 12 - 18) - Pamlico sound,         NOrth Carolina (nov 19  Dec 5) # tag dies 
# 221850- atlantic shores - Deployed (nov 12 - 13) - Pamlico sound,         NOrth Carolina (nov 14  -  Dec 5 ) # tag dies
# 221856- atlantic shores - Deployed (nov 12 - 13) - Pamlico sound,         NOrth Carolina (nov 14  -  Dec 5 ) # tag dies
# 260808 - Del Bay (Oct 2 - 25) -                                Nth carolina (Oct 25 - Nov 20) - tag dies 
# 260810 - Del Bay (Oct 2 - Oct 24) - to                                    Nthcarolina (Oct 24 - Nov 14) - tag dies 
# 240171 - Del bay (Oct 2 - 15) - sth to Virginia (Oct 16 - 30)           - north carolina (nov 9 - Feb 16 (next yr)- tag ends ? dropped?

# 221866 - atlantic shores - Deployed (nov 12 - 13) - north Charlston,         - Sth Carolina (nov 16 - Dec 10 )
# 260809 - Del Bay (Oct 2 - Nov 9) - to                                        - sth carolina (Nov 9 - Nov 26) - tag dies 
# 242656- sth carolina Kiawah Beach (May 23) - EAST JAMES BAY (May 24 - June 3 ) _ 9 days - HB Nelson River (June 4-7) - multiple short stops Queen Maud Gulf Bird Sanctuary - Vic Island - breeding ground (June 10 - August 3) - hudson Bay(aug 8-16) - sth Carolina (Aug 20 -continued - tag dies Dec 16)
# 260812 - sth Carolina (march 31 - may 25)- james bay (May 27-June 3) - Hudson Bay (JUne 3 - 13) - Matty Is(june 17 - july 18)- hudson bay (july 18-28) -sth carolina (same location (aug 5 - oct 19) - tag dies

# 221860- atlantic shores - Deployed (nov 12 - 13) - Exmore,                - Virginia (nov 13-dec 5 ) # tag dies 

# 221863- atlantic shores - Deployed (nov 12 - 22) - cAPE cARNAVERAL,       - Florida  (nov 24 - 26 ) # tag dies 
# 240175 - Del bay (Oct 2 - 27) - multiple stops short georgia              - florida (Oct 29 - June 3 - tag dropped?

# 234370 - Atlantic Coast deployed (August 26 -Dec 4)                     - Bahamas (Dec 6 - 14) - tags dies 
# 282291 - sth Carolina (may 18 - 20)- james bay (May 24-June 6) - prince wales (june 9 - july 12)- james Bay (july 15-aug 3) - turks caycos (aug 8 - Dec 4) - tag dies? potential other types


# 260692 - sth Carolina (May 16 - may 18)- james bay (May 20-June 3) - Baffin is(June 6 - june 16) - Prince charles Is (east arctic) (june 17 - July 24) - sth to st laurence Qc (July 26 - Aug 4) - del bay (Aug 7-10) - sth carolina (same location (aug 8 -sep 9) - down and back up north to Cuba (Sep 11- Oct 21 -tag ends)



















####### old version ###########################

## Nth Bound 
###  Depart US 

# 213829  - Del Bay depart may 28th - head north (toronto - tag died)

# 213833  - Del Bay depart (May 28th) 
#          -  East James Bay (may 29/30 - dropped tag here) 


## EASTERN ARCTI

# 242658  _ Kiawah Beach (stayed in area - depart - May 13)
#         - Monom (May 13 - 29)
#         - Eastern James Bay - (May 31 - june 6)       7 days 
#         - Baffin Is (June 7 -15) 
#         - Breeding : prince Charles Is (June 16 - August 10) 
#         ....more

# 213831   -  Del Bay depart (May 28th)  
#          - East James Bay (may 30 - Juen 10th ) - 10 days 
#          - Coats Isalnd (June 11 - June 16 ) - last transmission (possible breeding?)


## WEST

# 230306 - - Del Bay depart (June 1 ) 
#           - Nelson River, (June 4 - 10)
#           - multiple short stops Queen Maud Gulf Bird Sanctuary ()
#           - Vic Island - breeding ground arrive (June 20 - july 12)


# 213830  - Del Bay depart (May 24th)  - 
#            - EAST JAMES BAY (May 29 th to June 6th ) _ 7 days 
#            - multiple stops on Nelson River, Arviat - (June 7th)
#            - stopover King william (June 12th)  
#            - Vic Isalnd (breeding ground - arr June 20 - Sept 8th - dropped tag here)

# 242656  _ Kiawah Beach (stayed in area - depart - May 23)
#         - EAST JAMES BAY (May 24 - June 3 ) _ 9 days 
#          - HB Nelson River (June 4 - June 7 )
#          - multiple short stops Queen Maud Gulf Bird Sanctuary ?Victoria Is ()
#          - Vic Island - breeding ground arrive (June 10 - August 3)
##          ....more


# 242657  - _ Kiawah Beach (stayed in area - depart - May 22)
#          - Delaware Bay (May 25 - May 28)
#          - Eastern James Bay - (june 1 -6)  -  5 days 
#          - Hiurarryuaa / King William Isalnad (june 7 - 9)
#          - Breeding : Prince of Wales june 9 - August 4)
#           ....more
          



##########################################################
#South from breeding grounds 

# west Arctic

#'
# 242656  _ Kiawah Beach (stayed in area - depart - May 23)
#         - EAST JAMES BAY (May 24 - June 3 ) _ 9 days 
#          - HB Nelson River (June 4 - June 7 )
#          - multiple short stops Queen Maud Gulf Bird Sanctuary ?Victoria Is ()
#          - Vic Island - breeding ground arrive (June 10 - August 3)
##          ....more
#          - brd of Nunavut and manitoba (August 8 - 16)
#          - North Carolina (August 18 - 20 ) 
#          - Sth carolina (same as deploymet local)August 20 - Dec 12 - possible dropped tag? 
          

# 242657  - _ Kiawah Beach (stayed in area - depart - May 22)
#          - Delaware Bay (May 25 - May 28)
#          - Eastern James Bay - (june 1 -6)  -  5 days 
#          - Hiurarryuaa / King William Isalnad (june 7 - 9)
#          - Breeding : Prince of Wales june 9 - August 4)
#           ....more
#           - brd of Nunavut and manitoba (August 6 -7 )
#           - WEST JAMES BAY (August 8 - 19) 
#           - Sth Caroline August 20 - Oct 8
#           - Georgia - (OCt 8 - 31)


#EAST Arctic 

# 242658  _ Kiawah Beach (stayed in area - depart - May 13)
#         - Monom (May 13 - 29)
#         - Eastern James Bay - (May 31 - june 6)       7 days 
#         - Baffin Is (June 7 -15) 
#         - Breeding : prince Charles Is (June 16 - August 10) 
#         ....more
#         - Westn James Bay (Akimiski island bird Sanctuary - (August 12 -24) 
#         - Flew direct to Cuba (August 28 - Oct 7)


#


## Tags deployed ## 

# north Carolina (5) 

# 221844 - atlantic shores - Deployed (nov 12 - 19) \
#          - Southport, NOrth Carolina (nov   Dec 6) # tag dies 

# 221845- atlantic shores - Deployed (nov 12 - 18) 
#          - Pamlico sound, NOrth Carolina (nov 19  Dec 5) # tag dies 

# 221847 - atlantic shores - Deployed (nov 12 - 22) 
#          - Pamlico sound, NOrth Carolina (nov 11  - 25 ) # tag dies 

# 221850- atlantic shores - Deployed (nov 12 - 13) 
#          - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies

# 221856- atlantic shores - Deployed (nov 12 - 13) 
#          - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies 



## florida (2)

# 221858- atlantic shores - Deployed (nov 12 - 13) 
#         - Daytona Beach , Florida  (nov 15 - 26 ) # tag dies 

# 221863 -  atlantic shores - Deployed (nov 12 - 22) 
#         - cAPE cARNAVERAL, Florida  (nov 24 - 26 ) # tag dies 



#VIrginia(1)

# 221860- atlantic shores - Deployed (nov 12 - 13) 
#          - Exmore, Virginia (nov 13- 14 ) # tag dies 
#          - atlantic shores, Virginia (nov 15 -  noV 26 ) # tag dies 
#         - Exmore, Virginia (nov 27 -  Dec 5 ) # tag dies 



## Sth Carolina(1)

# 221866 - atlantic shores - Deployed (nov 12 - 13) 
#         - north Charlston, Sth Carolina (nov 16 - Dec 10 )







## NORTH - nEW YORK (1)
# 233931 - Atlantic Coast deployed (Sept 30 - Oct 11)
#           - Long Island New York (Oct 11 - 26 )

# NEW JERSEY (1)
#234376 - Atlantic Coast deployed (August 15- Oct 19) - two distinct stopovers Stone harbour and Brigantine


## CARIBEAN (5)
#224080- atlantic shores - Deployed (August 21 - 22) 
#        - north Charlston, Sth Carolina (Aug 25 - Sept 10 )
#        -  Bahamas (Sep 14 - 20) - tags dies 

#234370 - Atlantic Coast deployed (August 26 -Dec 4)
#        -  Bahamas (Dec 6 - 14) - tags dies 

#224082 -  atlantic shores - Deployed (August 21 - 28)
#       - Sapelo Island WMA, Georgia  - Deployed (August 30 - sept 12)
#       - Cuba   (Sept 12 - sept 24 )

# 224088 - atlantic shores - Deployed (August 27 - 29) 
#        -Cuba  (Sep 1- 26) - tags dies 



#236444 - deploy Monomoy NJ, August 27 - 30 
# - Barbuda is Sept 3 - 25- depart directly souuth (unusual for SE) more similar to NSA migration pattern?





## wintering 

# 221844 - atlantic shores - Deployed (nov 12 - 19) \
#          - Southport, NOrth Carolina (nov   Dec 6) # tag dies 

# 221845- atlantic shores - Deployed (nov 12 - 18) 
#          - Pamlico sound, NOrth Carolina (nov 19  Dec 5) # tag dies 

# 221847 - atlantic shores - Deployed (nov 12 - 22) 
#          - Pamlico sound, NOrth Carolina (nov 11  - 25 ) # tag dies 

# 221850- atlantic shores - Deployed (nov 12 - 13) 
#          - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies

# 221856- atlantic shores - Deployed (nov 12 - 13) 
#          - Pamlico sound, NOrth Carolina (nov 14  -  Dec 5 ) # tag dies 

# 221860- atlantic shores - Deployed (nov 12 - 13) 
#          - Exmore, Virginia (nov 13- 14 ) # tag dies 
#          - atlantic shores, Virginia (nov 15 -  noV 26 ) # tag dies 
#         - Exmore, Virginia (nov 27 -  Dec 5 ) # tag dies 

# 221863 -  atlantic shores - Deployed (nov 12 - 22) 
#         - cAPE cARNAVERAL, Florida  (nov 24 - 26 ) # tag dies 

# 221866 - atlantic shores - Deployed (nov 12 - 13) 
#         - north Charlston, Sth Carolina (nov 16 - Dec 10 )
 
# 234370 - Atlantic Coast deployed (August 26 -Dec 4)
#        -  Bahamas (Dec 6 - 14) - tags dies 

# 221858- atlantic shores - Deployed (nov 12 - 13) 
#         - Daytona Beach , Florida  (nov 15 - 26 ) # tag dies 





