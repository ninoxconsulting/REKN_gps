
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

# south

##############################################################################

south_id <- pop_id %>% filter(subpop == "South" ) |> arrange(type)

# all locations 
south <- df_all %>% 
  filter(tag.id %in% south_id $tag.id) %>%
  mutate(movement_final = case_when(
    movement_final == "deployment" ~ "north_migration",
    .default = movement_final
  )) %>% 
  filter(movement_final != "north_migration") |> 
  filter(movement_final != "south_migration") 


# stopoverlocations
south_stopover <-df_stopover_subset |> 
  filter(tag.id %in% south_id $tag.id) %>%
  mutate(movement_final = case_when(
    movement_final == "deployment" ~ "north_stopover",
    .default = movement_final
  )) %>% 
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)


south_dur <- dur_type_move %>% 
  filter(tag.id %in% south_id$tag.id)



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
# Geographic distributon of tags ## figure 11 - spring

south_stopover_spring <- south |> 
  filter(movement_final != "south_stopover")

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south_stopover_spring, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig11_south_stopovers__fall_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)



###########################################################
# Geographic distributon of tags ## figure 11 - fall

south_stopover_fall <- south |> 
  filter(movement_final != "north_stopover")


world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south_stopover_fall, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig11_south_stopovers_spring_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)





###############################################################################


### Figure 12

# Geographic distributon of all tag (all stopover data) tags 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south, size = 2, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-60, 80), expand = FALSE)+
 #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig12_south_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


###############################################################################

### Figure 8 

## Breeding locations 

south_breed <- south_stopover %>% filter(movement_final == "breeding")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south_breed, size = 2.1, aes(colour= movement_final)) +#colour = "dark blue") +
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

ggsave(file.path(out.plots,"fig9_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)




## Breeding locations - alternate 

south_breed <- south %>% filter(movement_final == "breeding")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south_breed, size = 1.5, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
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

ggsave(file.path(out.plots,"fig9_west_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)






















############## Migration map ###################

south_stopover <- cbind(south_stopover, st_coordinates(south_stopover))

library(leaflet)

pal <- colorFactor(
  palette = "viridis",
  domain = unique(south_stopover$tag.id))

birdmapall <- leaflet(wgwp_stopover) %>%
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


#deployemtn summary 

#"234375" - southward NJ
# "240158" "240159" 240164", 240165",  "240167",  "241166",  "241167""229312", 229314,-Piexe 
# "232982", 240168 , 213835, 213841  - NJ nth 



# # fall migration - depart date 

# 240158 - depart Lago de Piexe/ sth sth Am - may 15th
# 240159 - depart Lago de Piexe/ sth sth Am - May 24 h 2023
# 240164 - depart Lago de Piexe - April 11 th and depart sth sth Am may 27th 2023
# 240165 - depart Lago de Piexe - April 26th depart sth sth depart may 27th 
# "241166 -  depart Lago de Piexe -   April 13th  depart sth sth depart may 27th 
# 240167 - depart Lago de Piexe - April 10 th and depart sth sth Am may 29th 2023
# 241167 - depart Lago de Piexe - April 26 th and depart sth sth Am may 25th 2023

############################## 240166 - depart Lago de Piexe/ sth sth Am - May 8th 2023
############################### 241166 - depart Lago de Piexe/ sth sth Am - May 1st 2023


# #Depart Nth Brazil 
# 240158 - depart Marahao - may 15th 
# 240159 - depart Marahao - may 24th
# 241166 - depart Marahao - may 27th
# 240164 - depart Marahao = may 27th 
# 241167 - depart New Amsterdam - May 25th
# 240165 - depart New Amsterdam = may 27th 
# 241167 - depart Amapa - May 30th 
# 
# #Depart US mainland 
# 213830 - depart del bay May 24th (2021)
# 213835 - Depart del Bay - june 1 (2021) - tagged in delbay (unknown arrival)
# 213841 - Depart del Bay - june 1 (2021) - tagged in delbay (unknown arrival)
# 232982 - Depart del Bay - May 30th (2023) - tagged in delbay (unknown arrival)
# 240156  - depart del Bay May 29 2023 - tagged in delbay (unknown arrival)
# 240159  - depart del Bay May 31 2023) (arrive 29th May) 
# 240158  - depart sthport (nth carolina)  june 7th 2023 (arrive 20th May) 
# 240161  - depart del bay - May 29 2023 - tagged in delbay (unknown arrival)
# 240164  - depart del bay - June 20th 2023 - multiple stops on mainland(arrive 2nd june)
# 240165  - depart del bay - June 15th 2023 - multiple stops on mainland (arrive 6th june)
# 240167  - depart del bay - June 11th 2023 - arrive june 4th
# 240166  - depart del bay - June 11th 2023 - arrive june 1st
# 240167  - depart del bay - June 8th 2023 - arrive May 27th 
# 
# 
# #Arrive/depart Hudson bay - noth bound 
# 229312 arrive sth carolina may 18th / virginia beach / 25th - 28th May skip DEL BAY 
# 240164 arrive Nth Caroline june 2 to june 17 / del bay (june 18 - 21)    *multiple stops) - 2023 - 4 days 
# 210165 arrive Nth Crolin june 3rd to june 8th / del bay june 11 - 16th  
# 240158 arrive nth Carolina may 20 - may 30 th / skip DEL BAY 
# 240167 arrive Virginia june 3rd to june 11  
# 241166 arrive Virginia - june 1 to 11th h 
# 241167 arrive virginia/maryland may 27th to June 8th  
# 240159 arrive del bay june 2 and depart June 8 (multiple stops)- 2023 - 5 days
# 
# those bankded at delaware bay 

#232982, - deploy Del bay depart May 31
#213835, - deploy Del Bay - depart june 1
#213841 - deploy Del Bay - depart june 1
# 240168 arrive del bay may 16th = 30th





# # arrive/depart breeding grounds 
 240168 arrive prinec of wales isladn Juen 6 and tag dies Sept 8th 
 232982 arrive Victoria is June 9th and July 16 th depart is  - DEPARTURE 
 240158 arrive Victoria is June 12th - til ? tag died 
 213841 arrive Victoria is june 14th - Sept 19 (tag died ?())
 240159 arrive Victoria is June 17th and july 11 (last transmit)
# 213830 arrive Victoria is June 20th to Sept 8th (tag died ?())
 240167 arrive Victoria is june 22 and depart July 26th (full record)- DEPARTURE 
 240166 arrive Victoria is june 22 and depart July 4th (tag died)
# 
  arrive Coasts Is june 14th - july 2nd (tag died?) 
 240167 -june 21 and depart Jule 16th (full record)
 240164 arrive Prince of wales island june 29th - depart july 21 (full record)- DEPARTURE 
# 
# 
# # arrival stopover HB
# 232982 - arrive HBay (July 21st - August 10 th) - direct to Sth Am 
# 240161 - from Prince charles arrive (july 26th - August 15th)- via sth hamp, hb (multiple stops) - different stops on retunr than towards                                   
# 240164 -  from Vic Is = (August 18 - Sept 23)  -multple stops in HB - ends here 
# 240167 - from Vic is = single stop in hudson By (July 27th - August 8th)
# 
# # stopover delaware bay                   
# 234375 -  depart delaware bay August 31 - banded here - direct to sth am ()                     
# 240167 - from hudson bay (August 12 - 31th) the depart to Guyana 
# 
# 
# 
# 232982 - VicIs - Hudson Bay -                  - Marahoa - sth Brazil (multiple stops)
# 234375 -                      DEL BAY (multiple) - GUYANA - sth Brazil
# 240167 - VicIS - hudson Bay - DELBAY (multiple) -  GUYANA(multiple stops) - sth Brazil(multiple stops)
# 241167 - VicIS -hudson Bay (multiple) -       - GUYANA
# 
# 
# # arrive nth sth americs                      
# 232982 - arrive  stheast maranhao (August 17th - Sept 28th) - diredct from HB         
# 240167 - arrive October 8th - stayed to at least Dec 19th (tag ran out) - Reserva natural bahia San Blas (
#   234375 - arrive Guyana Sept 4th - Sept 26th direct from nth america                   
#   
#   
#   # arrive wintering grouds 
#   232982 - arrive  15th Oct (mutliple stop sth brazil/ argentina) - tag died
#   232982  - arrive Mar del plata October 1th - from Nth Sth Am.    
#   
#   
#   
#   # 
#   # hist(bddvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 2)")
#   # summary(bddvisit$revisits)
#   # 
#   # head(bddvisit$revisitStats)
#   # 
#   # 
#   # par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
#   # plot(martinvisit, martin, legendPos = c(13, -10))
#   # 
#   # 
#   # 
#   # 
#   # 
#   # 
#   # 
#   # 





