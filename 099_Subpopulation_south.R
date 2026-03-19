
##################################################################################

# Rufa subpopulations - south 

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

# south

##############################################################################

south_id <- pop_id %>% filter(subpop == "South" ) |> arrange(type)
#45 south and usable 

# type of tags 
south_id |> group_by(tag.model) |> count()
  
# all locations 
south <- df_all %>% 
  filter(tag.id %in% south_id $tag.id) %>%
#  mutate(movement_final = case_when(
#    movement_final == "deployment" ~ "north_migration",
#    .default = movement_final
#  )) %>% 
  filter(movement_final != "north_migration") |> 
  filter(movement_final != "south_migration") 


## southern duration for tags 
durs <- dur |> 
  filter(tag.id %in% south_id$tag.id) |> 
  arrange(desc(duration))

# locations of deployment 
south_id |> 
  group_by(study.site) |> 
  count()

# stopoverlocations
south_stopover <- df_stopover_subset |> 
  filter(tag.id %in% south_id$tag.id) %>%
#  mutate(movement_final = case_when(
#    movement_final == "deployment" ~ "north_stopover",
#    .default = movement_final
#  )) %>% 
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)


south_dur <- dur_type_move %>% 
  filter(tag.id %in% south_id$tag.id)



########################################################
# # Geographic distributon of tags ## figure 11 = COmBINED
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
#   coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
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

# northward migration # depart dates 

nth_mi <- south_dur |>
  select(tag.id, min, max, dur_days, year, proj, breeding, south, north, wintering, study.site) 

br <- nth_mi |> 
  filter(study.site == "PEIXE" ) |> 
  filter(north == 'y') |> 
  filter(dur_days >0)

unique(br$tag.id)
unique(nth_mi$study.site)


# figure 12  ######### north stopovers 
 
south_stopover_spring <- south |> 
  filter(movement_final != "south_stopover")|> 
  filter(movement_final != "deployment")

# pair down the breeding  and select single locaion select only one breeding location for clarity
south_breed <- south_stopover_spring %>% filter(movement_final == "breeding")|>
  group_by(tag.id) |>
  filter(movement_final == "breeding") |>
  slice_head(, n = 1)

# join back 
south_stopover_spring <- south_stopover_spring |> 
  filter(movement_final != "breeding")
  
south_stopover_spring <-  bind_rows(south_stopover_spring, south_breed)

# pair down the winterign  and select single locaion select only one breeding location for clarity
south_winter <- south_stopover_spring %>% filter(movement_final == "wintering")|>
  group_by(tag.id) |>
  filter(movement_final == "wintering") |>
  slice_head(, n = 1)

# join back 
south_stopover_spring <- south_stopover_spring |> 
  filter(movement_final != "wintering")

south_stopover_spring <-  bind_rows(south_stopover_spring, south_winter )



# plot the 


world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south_stopover_spring, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig12_south_stopovers__fall_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)








###########################################################
# Geographic distributon of tags ## figure 11 - fall

south_stopover_fall <- south |> 
  filter(movement_final != "north_stopover") |> 
  filter(movement_final != "deployment")

# pair down the breeding  and select single locaion select only one breeding location for clarity
south_breed <- south_stopover_fall %>% filter(movement_final == "breeding")|>
  group_by(tag.id) |>
  filter(movement_final == "breeding") |>
  slice_head(, n = 1)

# join back 
south_stopover_fall <- south_stopover_fall |> 
  filter(movement_final != "breeding")

south_stopover_fall <-  bind_rows(south_stopover_fall, south_breed)

# pair down the winterign  and select single locaion select only one breeding location for clarity
south_winter <- south_stopover_fall %>% filter(movement_final == "wintering")|>
  group_by(tag.id) |>
  filter(movement_final == "wintering") |>
  slice_head(, n = 1)

# join back 
south_stopover_fall <- south_stopover_fall |> 
  filter(movement_final != "wintering")

south_stopover_fall <-  bind_rows(south_stopover_fall, south_winter )




world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south_stopover_fall, size = 2, alpha = 0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig15_south_stopovers_spring_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)





###############################################################################

### Figure 12
#Northward migrats: #(n = 25)
nrth.tag <- c(229312, 229314, 240165, 242579, 261440, 262941,262944,262948,
            213835,213841, 240158,240159, 241166,255007, 261441,262940, 262945,
            232982,261435,240167,240164,240168,241167, 282311, 262946)

#South ward migrants: #(n = 20)
sth.tg <-c(234375, 261434, 261436,261437,261438,261443,261450,261452,261453,
            280804,280805,280806,280807,280808,280809,280811,280812,280813,285995,285996)

south_fig <- south |> 
  filter(movement_final != "deployment")


### split into two groups 

st.nth <- south_fig |> 
  filter(tag.id %in% nrth.tag)


# Geographic distributon of nth tags all stopover data) tags (n = 25)

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = st.nth, size = 2, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-60, 80), expand = FALSE)+
 #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig12_south_north_migration_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)


#############################
#South ward migrants: #(n = 20)

st.nth <- south_fig |> 
  filter(tag.id %in% sth.tg) |> 
 
  # Geographic distributon of nth tags all stopover data) tags 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = st.nth, size = 2, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  #scale_fill_manual(values = cyl_colors)+
  scale_color_viridis_d(name = "Movement Type", begin = 0.5) + 
  facet_wrap(~tag.id)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-60, 70), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

ggsave(file.path(out.plots,"fig12_south_sth_migration_stopovers_pertag.jpg"), width = 30, height = 30,units = "cm", dpi = 600)

#"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"




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

#filtered breedign locations
south_breed <- south_breed |>
  group_by(tag.id) |>
  filter(movement_final == "breeding") |>
  slice_head(, n = 1)

# wgwp_other <- wgwp_stopover |>
#   filter(movement_final != "breeding")
# 
# wgwp_breed <- bind_rows(wgwp_breed, wgwp_other)



# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south_breed, size = 1.5, aes(colour= as.character(tag.id))) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Tag ID") + 
  #scale_color_brewer(palette = "Spectral", name = 'Tag ID')+
  #facet_wrap(~movement_final)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-120, -70), ylim = c(59, 78), expand = FALSE)+
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

ggsave(file.path(out.plots,"fig14_south_stopovers_combined.jpg"), width = 30, height = 30,units = "cm", dpi = 600)





### 

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

# 240158 - depart Lago de Piexe/ sth sth Am - April 13
# 240159 - depart Lago de Piexe/ sth sth Am - April 13th h 2023
# 240164 - depart Lago de Piexe             - April 11 th and depart sth sth Am may 27th 2023
# 240165 - depart Lago de Piexe -           - April 26th depart sth sth depart may 27th 
# 241166 -  depart Lago de Piexe -          - April 13th  depart sth sth depart may 27th 
# 240167 - depart Lago de Piexe -           - April 10 th and depart sth sth Am may 29th 2023
# 241167 - depart Lago de Piexe -           - April 26 th and depart sth sth Am may 25th 2023
# 229312 - depart Lago de Piexe             - April 28 to nrth Brazil, then surinane 
# 229314 - depart Lago de Piexe             - April 29 to  surinane (May 6th) - tag died 
# 242579 - depart Lago de Piexe             - April 28 direct towards surinane - tag dies on route
# 261440 - depart Lago de Piexe             - April 24 to hops along east Brazil,
# 262941- depart                            - April 26 to hops along east Brazil,
# 262944 - depart                           - may 2nd , stopped in carribean (Nasseau) May 8th - 17th -
# 262948 - depart                           - april 28th, stopped in island nth venezuala May 3rd to 9th - 
# 255007 - depart Lago de Piexe             - April 28 to surinane - depart sth am May 23rd
# 261441 - depart Lago de Piexe             - April 28 to surinane - depart sth am May 18thrd
# 262940 - depart Lago de Piexe             - April 28 to surinane - depart sth am May 18thrd
# 262945 - depart Lago de Piexe             - April 29 direct to carribeam (near Nasseau)
# 262946- depart Lago de Piexe              - May 9th  to nth brazil and then retruned sth August 16th 


############################## 240166 - depart Lago de Piexe/ sth sth Am - May 8th 2023
############################### 241166 - depart Lago de Piexe/ sth sth Am - May 1st 2023


# #Depart Nth Brazil 
# 240158 - depart Marahao, brazil  - may 15th 
# 240159 - depart Marahao, brazil - may 24th
# 241166 - depart Marahao, brazil - may 27th
# 240164 - depart Marahao, brazil = may 27th 
# 240165 - depart surinane - depart May 27th  to nrth carolina (June 3rd) then delaware bay (june 12th arrival) - tag dies here
# 261440 - depart Marahao, brazil - april 28th to nrth carolina (may 21st)   tag dies here

# 241167 - depart New Amsterdam - May 25th
# 240165 - depart New Amsterdam = may 27th 
# 241167 - depart Amapa         - May 30th 
# 229312 - depart surinane      - May 15th  to sth carolina (May 18th) then multiple stops james bay, hudson bay - didnt reach breeding 

# 262944 - depart may 2nd , stopped in carribean (Nasseau) May 8th - 17th - then to nth carolina (May 20th) then multiple stops  SE coast (delaware bay), heading north  june 2nd - james - hudson bay - arctic sth of Vic island (june 6th) - potential drop tag before breeding?(marked as not breeding as unsure?) 
# 262948 - depart april 28th, stopped in island nth venezuala May 3rd to 9th - then to nth carolina (May 13th) - tag dies here
# 262945 - depart Lago de Piexe April 29 direct to carribeam (near Nasseau) May 8th - 19th -then sth carolina / delaware bay  to james and hudson bay then potenitla breeding? Arrive June 9th - tag dies here so unclear if is breeding?


## Arrive in the US mainland 

# 229312 - arrive sth Carolina (may 15th) Virginia, Del bay (depart may 31) for James Bay ~ 15 days 
# 240165  - arrive nth Carolina (june 6th) , del bay arrive june 11 - tag dies            ~ unknow days 
# 261440 - arrived nth Carolina (may 24th) - tag dies here                                ~ unknow days 
# 262944 - arrived nth Carolina (May 20th ) , del bay (arrive May 26th - June 2nd)        ~ 13 days
# 262948 - arrived nth carolina (may 14th ) - tag died here                               ~ unknow days 
# 240158  - arrive nth Carolina May 20th)   depart May 31st - direct to hudson Bay        ~ 11 days
# 240159 - arrive vriginia (May 30) , Delaware Bay (june2 2nd 8th) then depaart nth       ~ 9 days
# 241166 - arrive virginia (May 31st), depart june 9th to hudson bay                      ~ 10 days 
# 255007 - arrive virginia/Delaware bay (May 26th), delaware bay (may 27th - june 2nd)    ~ 7 days 
# 261441 - arrive georgia (May 21 - 24th), nth carolina, virginia, dle bay (may 27th - 31st) then north ~11 days
# 262940 - arrive virginia (may 26th - depart june 3rd ) direct nth                       - 8 days
# 262945 - arrive sth carolina (May 20 - 24th), del bay (may 24 - june 1st)               ~ 12 days 
# 240167 - arrive virginia (june 3 - 12)then direct nth                                   ~ 9 days
# 240164 - arrive nth carolina ( june 3rd - 18th), del bay (june 18 - 21)                 ~ 18 days
# 241167 - arrive virginia (may 28th - june 8th ) then direct                             ~ 11 days 

## totals : 
# sth carolina (n = 2) all onwards to Del Bay 
# nth Carolina (n = 6) most onwards to Del Bay , 1 direct to arctic
# virginia (n = 6), about 1/2 Del bay or 
# georgia (n - 1), then hopped onto SE coast 

# multiple stops - 
# direct single stop ( 5), multiple stop inclu del bay (8)


############################################################
# #Depart US mainland (add the birds tagged in USA) 

# 282311 - tagged in sth carolina           - may 20) - James Bay (may 26 - june 6) - Victoria Is (June 7 - July 24) - James Bay (July 26 - August 11) - Surine (Aug 16 - sep 10)- Baihia San Blass (Sep 16 -20) tag dies
# 213830 - depart del bay                   - May 24th (2021)
# 240156  - depart del Bay                  - May 29 2023 - tagged in DB - james bay (june 1 -3) - arrive beverly is (nth of kind William) (june 6th - 16 th) - dropped tag - unsure breeding? 
# 261435 - tagged in DB                     - May 30) - James Bay (June 1 - 8), King william (June 9 - july 7) - sth - james Bay (July 8 - July 29) - direct to Brazil Maraha(August 16 - Sept1) - sth Bahia san blas (Sept 8-Oct 8) - sth to Tierra del Fuaego Nth (Oct 9th - tag drop)
# 240168 - tagged in DB May (16th =         - may 30th) arrive prinec of wales isladn (June 6 -august 5th) - Hudson Bay (Aug 11 - 16) -direct to Guyana (aug 31 - Sep 9) - surinane (sept 12-15 tag dies )
# 240161  - depart del bay                  - May 29 2023 - tagged in DB - James Bay (May 31 - June 6) _ prince charles Is (east) (june 16 - July 21) - sth hudson Bay /james bay (july 26 - August 12 - dropped tag)
# 232982 - Depart del Bay -                 - May 30th (2023) - tagged in DB, james bay June (1 - 3), Hudson Bay stopover - breeding in Vic island (june 12th - July 13) headed south - james bay (july 20- August 10)- direct Sth Am  - Belem (August 16 - Sept 28)- sth to Bahai San Blas (October -14) tag dropped after this time
# 240158  - arrive nth Carolina May 20th)   - May 31st - direct to james Bay (Jun 1-7), Victoria is (June 12th - dropped tag )  
# 261441 - arrive georgia (May 21 - 24th),   - may 31st) - james bay (june 2-7) arrived King william Is(june 16 - Sept - dropped tag?)
# 229312 -Del bay depart                    - may 31) for James Bay , hudson bay (june 8 - 9) tag dies 
# 240158  - arrive nth Carolina May 20th)   - May 31st - direct to james Bay (Jun 1-7), Victoria is (June 12th - dropped tag )  
# 213835 - Depart del Bay -                 - june 1 (2021)- tagged in DB, - james bay June (4 - 8th), east side of Hudson Bay - breeding potential coasts island(>) (arrive june 14th tp July 2nd - tag dies) tagged in delbay (unknown arrival)
# 213841 - Depart del Bay -                 - june 1 (2021) - tagged in DB, james bay June (6 - 9), Hudson Bay stopover - breeding in Vic island (june 12th - September 17 - dropped tag>) 
# 262945 - arrive sth carolina delBay        -june 1st)- hudson bay (june 4 -8) - nunavut mainland (june 9th - july 29 )- tag dropped - potnteil not breeding? 
# 262944 - arrived nth Carolina, del bay    - June 2nd) - hudson bay (june 4 - 8), northern nunavut mainland june 6 -9) - not reached breeding (?)
# 255007 - virginia (may 29) Del bay may 27 - june 2nd) - james Bay (june 2-5), west hudson bay (june 6-12), nth mainland nunavut (june 14-18), Victoria Is (June 21 - August - tag dropped?)
# 262940 - arrive virginia (may 26th -       - june 3rd ) several short stopover james and husdon bay - Sthampton is (June 15 - onwards - dropped tag?)
# 241166 - virginia (May 31st),             - june 9th to hudson bay (june 14-19) - victoria is (june 22 - july 4 - tag dies)
# 241167  arrive virginia (may 28th -       - june 8th ) - James Bay (june 9-14) - Vic Is (Nun) June 21 - July 16 - sth james Bay july 23 - Aug 12) - French Guiana (Aug 15 - tag dies )
# 240159  - depart del Bay                  - June 8th 2023) - james Bay (June 10 - 12) - Victoria Is (June 16th - dropped tag here)
# 240167  - arrive virginia (june 3 - 12)   - June 12 - then direct nth Hudson bay (june 15-20) - Vic island (June 23 - july 22) - sth to Hudson bay (july27 - august 10th) - sth carolina (August 15 - 29) - Guyana (Sept 4 - Oct 1) - San Blas (OCt 8th - tag dropped)
# 240164  - depart del bay                  - June 21th 2023 - multiple stops on mainland(arrive 2nd june) - james bay (June 21- 25) - Prince Of wales is (june 29 - July 21) - sth to hudscon bay (August 18 - tag dropped)



#########################################
## Arrive/depart Hudson bay - north bound 

# 213830 - depart del bay May 24th (2021) -                                       - James Bay (May 25 - June 5) ~11days - multiple hudson bay stops - arrive breeding area June june 20 - tag dies 
# 282311 - tagged in sth carolina (may 18 -20)                                    - James Bay (may 26 - june 6) ~ 11 days- Victoria Is (June 7 - July 24) - James Bay (July 26 - August 11) - Surine (Aug 16 - sep 10)- Baihia San Blass (Sep 16 -20) tag dies
# 240168 - tagged in DB May (16th = 30th)                                         - jAMES BAY (May 31 - June 2) - 3 days-  arrive prinec of wales isladn (June 6 -august 5th) - Hudson Bay (Aug 11 - 16) -direct to Guyana (aug 31 - Sep 9) - surinane (sept 12-15 tag dies )
# 240161  - depart del bay - May 29 2023 - tagged in DB -                         - James Bay (May 31 - June 6) - 7 days - prince charles Is (east) (june 16 - July 21) - sth hudson Bay /james bay (july 26 - August 12 - dropped tag)
# 213835 - Depart del Bay - june 1 (2021)- tagged in DB,                          - james bay June (4 - 8),     - 4 days - east side of Hudson Bay - breeding potential coasts island(>) (arrive june 14th tp July 2nd - tag dies) tagged in delbay (unknown arrival)
# 213841 - Depart del Bay - june 1 (2021) - tagged in DB,                         - james bay June (6 - 9),     - 3 days - Hudson Bay stopover - breeding in Vic island (june 20th) tag died on breefing area  
# 232982 - Depart del Bay - May 30th (2023) - tagged in DB,                       - james bay June (1 - 3),     - 3 days - Hudson Bay stopover - breeding in Vic island (june 12th - July 13) headed south - james bay (july 20- August 10)- direct Sth Am  - Belem (August 16 - Sept 28)- sth to Bahai San Blas (October -14) tag dropped after this time
# 261435 - tagged in DB (depart May 30)                                           - James Bay (June 1 - 8),     - 7 days - King william (June 9 - july 7) - sth - james Bay (July 8 - July 29) - direct to Brazil Maraha(August 16 - Sept1) - sth Bahia san blas (Sept 8-Oct 8) - sth to Tierra del Fuaego Nth (Oct 9th - tag drop)
# 241167  arrive virginia (may 28th - june 8th )                                  - James Bay (june 9-14)       - 5 days - Vic Is (Nun) June 21 - July 16 - sth james Bay july 23 - Aug 12) - French Guiana (Aug 15 - tag dies )
# 229312 - arrive sth Carolina (may 15th) Virginia, Del bay (depart may 31) for   - James Bay (JUNE 4),         - 4 days - hudson bay (june 8 - 9) tag dies 
# 255007 - arrive virginia/Delaware bay (May 26th), delbay (may 27 - juNE 2)      - james Bay (june 2-5),       - 9 days - west hudson bay (june 6-12), nth mainland nunavut (june 14-18), Victoria Is (June 21 - August - tag dropped?)
# 240158  - arrive nth Carolina May 20th)   - May 31st                            - james Bay (Jun 1-7),        - 7 days - Victoria is (June 12th - dropped tag )  
# 240159  - depart del Bay                  - June 8th 2023)                      - james Bay (June 10 - 12)    - 2 days - Victoria Is (June 16th - dropped tag here)
# 261441 - arrive georgia (May 21 - 24th), nth caro, virg, dle bay (may 27-31)   - james bay (june 2-7)         - 5 days -  arrived King william Is(june 16 - Sept - dropped tag?)
# 240164  - depart del bay - June 20th 2023 - multiple stops (arrive 2nd june)    - james bay (June 21- 25)     - 4 days - Prince Of wales is (june 29 - July 21) - sth to hudscon bay (August 18 - tag dropped)

# 262945 - arrive sth carolina (May 20 - 24th), del bay (may 24 - june 1st)       - hudson bay (june 4 -8) -    - 4 days - nunavut mainland (june 9th - july 29 )- tag dropped - potnteil not breeding? 
# 262944 - arrived nth Carolina (May 20th ) , del bay ( May 26th - June 2nd)      - hudson bay (june 4 - 8),    - 4 days - northern nunavut mainland june 6 -9) - not reached breeding (?)
# 241166 - arrive virginia (May 31st), depart june 9th                            - hudson bay (june 14-19) -   - 5 days - victoria is (june 22 - july 4 - tag dies)
# 240167  - arrive virginia (june 3 - 12) then direct                             - nth Hudson bay (june 15-20) - 5 days  - Vic island (June 23 - july 22) - sth to Hudson bay (july27 - august 10th) - sth carolina (August 15 - 29) - Guyana (Sept 4 - Oct 1) - San Blas (OCt 8th - tag dropped)



###########################################################
# # arrive/depart breeding grounds 

# 16 arrive in breeding ground and 7 southwards
## Central Eastern arctic

# Coasts is - 213835 - breeding potential coasts island(>) (  - june 14 - July 2nd - tag dies) tagged in delbay 
# Southhamp is - 262940 - arrive virginia (may 26th -         - june 3rd ) several short stopover james and husdon bay - Sthampton is (June 15 - onwards - dropped tag?)
# Nun. mainland 262945 - arrive sth carolina delBay           - june 1st)- hudson bay (june 4 -8) - nunavut mainland (june 9th - july 29 )- tag dropped - potnteil not breeding? 

# Western Arctic
### kind william 
# 261435 - tagged in DB (depart May 30) - James Bay (June 1 - 8),     King william (June 9 - july 7) ~ 29 days       - sth - james Bay (July 8 - July 29) - direct to Brazil Maraha(August 16 - Sept1) - sth Bahia san blas (Sept 8-Oct 8) - sth to Tierra del Fuaego Nth (Oct 9th - tag drop)
# 261441 - georgia/ dle bay - james bay (june 2-7) arrived            King william (june 16 - Sept - dropped tag?

# vic is 
# 213841 - Hudson Bay stopover - breeding in Vic island  ( june 20th - tag dies ) 
# 232982 - Hudson Bay stopover - breeding in Vic island  ( june 12th - July 13)      ~ 30 days  - headed south - james bay (july 20- August 10)- direct Sth Am  - Belem (August 16 - Sept 28)- sth to Bahai San Blas (October -14) tag dropped after this time
# 240158 - arrive Victoria is                              June 12th - til ? tag died 
# 240159 - arrive Victoria is                              June 17th and july 11 (last transmit)
# 240167 -nth Hudson bay (june 15-20)   -       Vic island (June 23 - july 22)        ~ 30 days     sth to Hudson bay (july27 - august 10th) - sth carolina (August 15 - 29) - Guyana (Sept 4 - Oct 1) - San Blas (OCt 8th - tag dropped)
# 241166 -  hudson bay (june 14-19) -          victoria is (june 22 - july 4 - tag dies)
# 241167  arrive virginia - James Bay (june 9-14) - Vic Is (Nun) June 21 - July 16    ~ 26 days   - sth james Bay july 23 - Aug 12) - French Guiana (Aug 15 - tag dies )
# 282311 - tagged in sth carolina  James Bay - Victoria Is (June 7 - July 24)         ~ 47 days       - James Bay (July 26 - August 11) - Surine (Aug 16 - sep 10)- Baihia San Blass (Sep 16 -20) tag dies
# 255007 - arrive virginia/Delaware   nth mainland nunavut (june 14-18), Victoria Is (June 21 - August - tag dropped?)

#prince of wales 
# 240164 - james bay (June 21- 25) -      Prince Of wales is (june 29 - July 21)  ~ 22 days - sth to hudscon bay (August 18 - tag dropped)
# 240168 - tagged in DB May (16th = 30th) arrive prinec of wales isladn (June 6 -august 5th) ~ 59 days  - Hudson Bay (Aug 11 - 16) -direct to Guyana (aug 31 - Sep 9) - surinane (sept 12-15 tag dies )

#Ave no 
#59 + 22+ 47 + 26+30+30+29

#To add
# 213830  SE SUBSP - depart del bay May 24th (2021) - arrive James Bay (May 25 - June 5th) - multiple hudson bay stops - arrive breeding area Vic island June june 20 - tag dies 
# 240161  SE_NSA - depart del bay - May 29 2023 - tagged in DB - James Bay (May 31 - June 6) _ prince charles Is (east) (june 16 - July 21) - sth hudson Bay /james bay (july 26 - August 12 - dropped tag)




# south bound migration 

# # arrival stopover HB - (n = 7)  
# 232982 -  breeding in Vic island (june 12th - July 13)    - james bay (july 20- August 10)         ~ 20 days                - direct Sth Am  - Belem (August 16 - Sept 28)- sth to Bahai San Blas (October -14) tag dropped after this time
# 240164  - breeding Prince Of wales (june 29 - July 21)    - Hudson bay (August 18 - tag dropped)   
# 240167  - breeding Vic island (June 23 - july 22)         - Hudson bay (july 27 - august 10th)     ~14 days                 - sth carolina (August 15 - 29) - Guyana (Sept 4 - Oct 1) - San Blas (OCt 8th - tag dropped)
# 240168- from Prince of wales -                            - Hudson bay (August 6 - 17th) then james bay (August 18 - 24)    - direct sth America                           
# 241167 - from Vic is = ultiple short stop,                - Hudson bay (July 27th - August 10th)    ~14 days                - direct Sth AM 
# 261435 - King william (June 9 - july 7) - sth             - james Bay (July 8 - July 29)            ~ 20 dyas               - direct to Brazil Maraha(August 16 - Sept1) - sth Bahia san blas (Sept 8-Oct 8) - sth to Tierra del Fuaego Nth (Oct 9th - tag drop)
# 282311 - Victoria Is (June 7 - July 24)                   - James Bay (July 26 - August 11)        ~17 days                 - Surine (Aug 16 - sep 10)- Baihia San Blass (Sep 16 -20) tag dies


# 234375 - taggge in US



# # stopover delaware bay  ########################                 

# # arrive wintering grounds - 
# 232982 - US banded - arrive  15th Oct (mutliple stop sth brazil/ argentina) - tag died arrive Mar del plata October 1th - from Nth Sth Am. 
# 240167 - FULL distribution - arrive virginia (june 3 - 12) then direct nth Hudson bay (june 15-20) - Vic island (June 23 - july 22) - sth to Hudson bay (july27 - august 10th) - sth carolina (August 15 - 29) - Guyana (Sept 4 - Oct 1) - San Blas (OCt 8th - tag dropped)
# 261435 - US banded - tagged in DB (depart May 30) - James Bay (June 1 - 8), King william (June 9 - july 7) - sth - james Bay (July 8 - July 29) - direct to Brazil Maraha(August 16 - Sept1) - sth Bahia san blas (Sept 8-Oct 8) - sth to Tierra del Fuaego Nth (Oct 9th - tag drop)
# 234375 - US banded -  depart delaware bay August 31 - banded here - direct to sth am ()                     
# 262946 - Stayed on sth am continent 

# 19 banded in Piexe (oct - Dec 2025)




########################################################################
# Map by month 

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = south, size = 2.5, alpha=0.8, aes(colour = movement_final)) +#colour = "dark blue") +
  scale_color_viridis_d(name = "Movement Type") + 
  facet_wrap(~month)+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-58, 80), expand = FALSE)+
  
   theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global





# exta info 

# #list of tags departing from Brazil heading north
# # 229312 - depart April 28 to nrth Brazil, then surinane - depart (May 15th ) to sth carolina (May 18th) then multiple stops james bay, hudson bay - didnt reach breeding 
# # 229314 - depart April 29 to  surinane (May 6th) - tag died 
# # 240165 - depart April 28 to hops along east Brazil, then surinane - depart (May 27th ) to nrth carolina (June 3rd) then delaware bay (june 12th arrival) - tag dies here
# # 242579 - depart April 28 direct towards surinane - tag dies on route
# # 261440 - depart April 24 to hops along east Brazil, then nrth brazil- depart (april 28th ) to nrth carolina (may 21st)   tag dies here
# # 262941- depart April 26 to hops along east Brazil, then nrth brazil (may 3rd)  tag dies here
# # 262944 - depart may 2nd , stopped in carribean (Nasseau) May 8th - 17th - then to nth carolina (May 20th) then multiple stops  SE coast (delaware bay), heading north  june 2nd - james - hudson bay - arctic sth of Vic island (june 6th) - potential drop tag before breeding?(marked as not breeding as unsure?) 
# # 262948 - depart april 28th, stopped in island nth venezuala May 3rd to 9th - then to nth carolina (May 13th) - tag dies here
# # 213835 - USA deployed
# # 213841- USA deployed
# # 240158- depart April 13 to hops along east Brazil,then nrth Brazil depart (May 16th ) to sth carolina (May 20 -22nd) then james bay on to Vic island (arrrive June12th) 
# 
# # 262945 - depart Lago de Piexe April 29 direct to carribeam (near Nasseau) May 8th - 19th -then sth carolina / delaware bay  to james and hudson bay then potenitla breeding? Arrive June 9th - tag dies here so unclear if is breeding?
# # 232982 - US tagged 
# # 261435 - US tagged

# 213835 - Depart del Bay - june 1 (2021)- tagged in DB, - james bay June (4 - 8th), east side of Hudson Bay - breeding potential coasts island(>) (arrive june 14th tp July 2nd - tag dies) tagged in delbay (unknown arrival)
# 213841 - Depart del Bay - june 1 (2021) - tagged in DB, james bay June (6 - 9), Hudson Bay stopover - breeding in Vic island (june 20th) tag died on breefing area  
# 232982 - Depart del Bay - May 30th (2023) - tagged in DB, james bay June (1 - 3), Hudson Bay stopover - breeding in Vic island (june 12th - July 13) headed south - james bay (july 20- August 10)- direct Sth Am  - Belem (August 16 - Sept 28)- sth to Bahai San Blas (October -14) tag dropped after this time
# 240158 - arrive Victoria is June 12th - til ? tag died 
# 240159 - arrive Victoria is June 17th and july 11 (last transmit)
# 240164 - depart del bay - June 20th 2023 - multiple stops on mainland(arrive 2nd june) - james bay (June 21- 25) - Prince Of wales is (june 29 - July 21) - sth to hudscon bay (August 18 - tag dropped)
# 240167 - arrive virginia (june 3 - 12) then direct nth Hudson bay (june 15-20) - Vic island (June 23 - july 22) - sth to Hudson bay (july27 - august 10th) - sth carolina (August 15 - 29) - Guyana (Sept 4 - Oct 1) - San Blas (OCt 8th - tag dropped)
# 240168 - tagged in DB May (16th = 30th) arrive prinec of wales isladn (June 6 -august 5th) - Hudson Bay (Aug 11 - 16) -direct to Guyana (aug 31 - Sep 9) - surinane (sept 12-15 tag dies )
# 241166 - arrive virginia (May 31st), depart june 9th to hudson bay (june 14-19) - victoria is (june 22 - july 4 - tag dies)
# 241167  arrive virginia (may 28th - june 8th ) - James Bay (june 9-14) - Vic Is (Nun) June 21 - July 16 - sth james Bay july 23 - Aug 12) - French Guiana (Aug 15 - tag dies )
# 255007 - arrive virginia/Delaware bay (May 26th), delaware bay (may 27th - june 2nd) - james Bay (june 2-5), west hudson bay (june 6-12), nth mainland nunavut (june 14-18), Victoria Is (June 21 - August - tag dropped?)
# 261435 - tagged in DB (depart May 30) - James Bay (June 1 - 8), King william (June 9 - july 7) - sth - james Bay (July 8 - July 29) - direct to Brazil Maraha(August 16 - Sept1) - sth Bahia san blas (Sept 8-Oct 8) - sth to Tierra del Fuaego Nth (Oct 9th - tag drop)
# 261441 - arrive georgia (May 21 - 24th), nth carolina, virginia, dle bay (may 27th - 31st) - james bay (june 2-7) arrived King william Is(june 16 - Sept - dropped tag?
# 262940 - arrive virginia (may 26th -       - june 3rd ) several short stopover james and husdon bay - Sthampton is (June 15 - onwards - dropped tag?)
# 262945 - arrive sth carolina delBay        -june 1st)- hudson bay (june 4 -8) - nunavut mainland (june 9th - july 29 )- tag dropped - potnteil not breeding? 
# 282311 - tagged in sth carolina (may 18 -20) - James Bay (may 26 - june 6) - Victoria Is (June 7 - July 24) - James Bay (July 26 - August 11) - Surine (Aug 16 - sep 10)- Baihia San Blass (Sep 16 -20) tag dies



