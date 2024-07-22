
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

#df_limit <- df_all %>% 
# filter(movement_final %in% c("breeding", "north_stopover", "south_stopover", "wintering"))


############################################################################
### SUB POPULATION REVIEW 
# read in duration 

dur_type_move <- read_csv(file.path(out.plots, "rufa_duration_movement_type_rufa.csv"))


## generate a paired down version of the stopover locations for mapping only not for analysis 

df_stopover_subset <- st_read(file.path(out.plots , "rufa_stopovers.gpkg"))



#############################################################################

# south  east

##############################################################################

se_id <- pop_id %>% filter(subpop == "SE" ) |> arrange(type)

# all locations 
se<- df_all %>% 
  filter(tag.id %in% se_id $tag.id) %>%
  mutate(movement_final = case_when(
    movement_final == "deployment" ~ "north_migration",
    .default = movement_final
  )) %>% 
  filter(movement_final != "north_migration") |> 
  filter(movement_final != "south_migration") 


# stopoverlocations
se_stopover <-df_stopover_subset |> 
  filter(tag.id %in% se_id $tag.id) %>%
# mutate(movement_final = case_when(
#    movement_final == "deployment" ~ "north_stopover",
#    .default = movement_final
#  )) %>% 
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)


se_dur <- dur_type_move %>% 
  filter(tag.id %in% se_id$tag.id)



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

se_stopover_spring <- se |> 
  filter(movement_final != "south_stopover")

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se_stopover_spring, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -40), ylim = c(5, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

#ggsave(file.path(out.plots,"fig16_se_stopovers__fall_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)

###########################################################
# Geographic distributon of tags ## figure 16 - fall

se_stopover_fall <- se |> 
  filter(movement_final != "north_stopover")


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

#ggsave(file.path(out.plots,"fig16_south_stopovers_spring_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)





###############################################################################

# 
# ### Figure 17
# 
# # Geographic distributon of all tag (all stopover data) tags 
# 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se, size = 2, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") +
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -40), ylim = c(5, 80), expand = FALSE)+
 #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global
# 
# #ggsave(file.path(out.plots,"fig12_south_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)
# 

###############################################################################

### Figure 8 

## Breeding locations 

se_breed <- se_stopover %>% filter(movement_final == "breeding")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se_breed, size = 2.1, aes(colour= movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  #facet_wrap(~movement_final)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
  theme_bw()+
  #labs(colour = "Type") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    #legend.title = "", 
    #legend.position = "bottom",
    #legend.key.width = unit(3, "lines")
  )

global

## alteernate breding plot 

se_breed <- se_stopover %>% filter(movement_final == "breeding")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se_breed, size = 3, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
  #scale_color_viridis_d(name = "Movement Type") + 
  #geom_sf(data = south_breed, size = 1.5, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
  # scale_color_viridis_d(name = "Tag ID") + 
  scale_color_brewer(palette = "Spectral", name = 'Tag ID')+
  #facet_wrap(~movement_final)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-120, -70), ylim = c(59, 78), expand = FALSE)+
 # coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
  theme_bw()+
  #labs(colour = "Type") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    #legend.title = "", 
    #legend.position = "bottom",
    #legend.key.width = unit(3, "lines")
  )

global

#ggsave(file.path(out.plots,"fig9_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)




## Breeding locations - alternate 

#south_breed <- south %>% filter(movement_final == "breeding")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = se_breed, size = 1.5, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Tag ID") + 
  #facet_wrap(~movement_final)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
  theme_bw()+
  #labs(colour = "Type") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    #legend.title = "", 
    #legend.position = "bottom",
    #legend.key.width = unit(3, "lines")
  )

global

#ggsave(file.path(out.plots,"fig9_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


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





