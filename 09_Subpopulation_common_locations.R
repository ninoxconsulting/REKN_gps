
##################################################################################

# Rufa subpopulations - COMMEON AREAS 

####################################################################################
library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(gganimate)
library(ggspatial)

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

df_all <- st_read(file.path(final_dat,"rekn_moveclass_20240716_usable.gpkg" )) %>% 
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(proj, tag.id, date_time, year, month, movement_final, subspecies, subpop, type)

df_all_points <- df_all
############################################################################
### SUB POPULATION REVIEW 
# read in duration 

#dur_type_move <- read_csv(file.path(out.plots, "rufa_duration_movement_type_rufa.csv"))

## generate a paired down version of the stopover locations for mapping only not for analysis 

df_stopover_subset <- st_read(file.path(out.plots , "rufa_stopovers.gpkg"))%>%
  filter(keep >1) %>% 
  dplyr::select(-movement_final_next, -toremove, -toremove2, -keep)%>% 
  left_join(pop_id) %>% 
  filter(subpop %in% c("west", "SE", "NSA", "South"))%>% 
  select(proj, tag.id, date_time, movement_final, subspecies, subpop, type)



############################################################################

### breeding grounds 


br_stoppovers <- df_stopover_subset |>  filter(movement_final == "breeding")
br_stoppovers <- br_stoppovers |> 
  group_by(tag.id) |> 
  slice_sample(n = 1) |> 
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  ))

#br_all <- df_all |>  filter(movement_final == "breeding")

br_summ <- br_stoppovers |> group_by(subpop) |> 
  st_drop_geometry() |> 
  count()



# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = br_stoppovers, size = 2.5, aes(colour= subpop_val)) +#colour = "dark blue") +
  scale_color_brewer(palette = "Set1", name = "Subpopulation") + 
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


## breeding overlap

br_dur <- df_all |>  filter(movement_final == "breeding") %>% 
  group_by(tag.id)%>% 
  mutate(start_date = week(min(date_time)),
         end_date = week(max(date_time)))%>%
  filter(type != "partial spring/breeding")  %>% 
  st_drop_geometry()%>% 
  select(-proj, -date_time, -type, -year, -month, -movement_final, -subspecies)%>% 
  distinct() %>% 
  mutate(region = case_when(
    tag.id %in% c( 228166, 201146 , 232982, 242583,  241167,  240167, 213834,242656) ~ "Victoria Island",
    tag.id %in% c( 242657, 240168, 240164) ~ "Prince of Wales Is.",
    tag.id == 238544 ~ "Southampton Is.",
    tag.id == 242580 ~ "Matty Is.",
    tag.id == 242658 ~ "Prince Charles Is.",
    tag.id == 232981 ~ "Baffin Is."
    ))|> 
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  ))




## figure of duration 

ggplot(br_dur, aes(y=factor(region))) +
  geom_segment(aes(x=start_date, xend=end_date, y=factor(subpop), yend=factor(subpop),color=subpop_val), linewidth = 3)+
  scale_color_brewer(palette = "Set1", name = "Subpopulation")+
  xlim(10,40)+
  #(aes(x=start_date, y=factor(subpop), size = 2), colour = "blue") +
  #geom_point(aes(x=start_date, y=factor(subpop), size = 2), colour = "red") +
  facet_wrap(~region) +
  #ggtitle("Breeding Locations") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Week of Year") + ylab("breeeding regions") +
  theme_bw()
  #theme(legend.position = "none")




################################################################################

## Hudson Bay Sth stopovers 
#############################################################################

hb_all <- st_read(file.path(final_dat,"Hudson_Bay_usage.gpkg" )) %>% 
  mutate(st_type = "hudson_bay")
jb_all <- st_read(file.path(final_dat,"James_Bay_usage.gpkg" )) %>% 
  mutate(st_type = "james_bay")

all <- rbind(hb_all, jb_all) %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type, st_type, tag.id.order)%>% 
  filter(movement_final %in% c("north_stopover", "south_stopover")) %>%
  mutate(subpop_val = case_when(
  subpop == "NSA" ~ "NSA",
  subpop == "South" ~ "Southern",
  subpop == "west" ~ "Western",
  subpop == "SE" ~ "SE",
)) |> 
group_by(tag.id, st_type) |> 
  slice_min(n = 1,  order_by = tag.id.order) |> 
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  ))





########################################################
# Geographic distributon of tags ## figure 11 = COmBINED

df_all <- all %>% 
  filter(movement_final %in% c("north_stopover", "south_stopover")) 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = df_all, size = 2.75, alpha = 0.8, aes(colour = subpop_val, alpha = 0.8)) +#colour = "dark blue") +
  scale_color_brewer(palette = "Set1", name = "Subpopulation")+
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~movement_final) +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -20), ylim = c(-20, 80), expand = FALSE)+
  coord_sf(xlim = c(-100, -75), ylim = c(49, 65), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global



#####################################################

## arrival dates - James Bay  NORTH 

jb <- all |> filter(st_type == "james_bay") |> 
  st_drop_geometry()%>% 
  filter(movement_final == "north_stopover") |> 
  group_by(tag.id)%>%
  mutate(start_date = yday(min(date_time)),
         end_date = yday(max(date_time)))%>% 
  select( -subspecies)%>%
  distinct()%>% 
  arrange(start_date)
  
  
# arrival dates - Nelson River - NORTH 

hb <- all |> filter(st_type == "hudson_bay") |> 
  st_drop_geometry()%>% 
  filter(movement_final == "north_stopover") |> 
  group_by(tag.id)%>%
  mutate(start_date = yday(min(date_time)),
         end_date = yday(max(date_time)))%>% 
  select( -subspecies)%>%
  distinct()%>% 
  arrange(start_date)


## UP TO HERE - STILL NEED TO DO A SOUTH MIGRATION 
doubletop <- hb |> filter(tag.id %>% jb$tag.id)

aa <- hb$tag.id
bb <- jb$ tag.id

aa[aa %in% bb]



# depart dates - Nelson River - NORTH 

hb_dep <-  rbind(hb_all, jb_all) %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type, st_type, tag.id.order)%>% 
  filter(movement_final %in% c("north_stopover", "south_stopover")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  )) |> 
  group_by(tag.id, st_type) |> 
  slice_max(n = 1,  order_by = tag.id.order) |> 
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  ))|> 
  filter(st_type == "hudson_bay") |> 
  st_drop_geometry()%>% 
  filter(movement_final == "north_stopover") |> 
  group_by(tag.id)%>%
  mutate(start_date = yday(min(date_time)),
         end_date = yday(max(date_time)))%>% 
  select( -subspecies)%>%
  distinct()%>% 
  arrange(start_date)







## Overlap Dates 

all_dur <- rbind(hb_all, jb_all) %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type, st_type, tag.id.order)%>% 
  filter(movement_final %in% c("north_stopover", "south_stopover")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  )) 

br_dur <- all_dur |>  filter(movement_final %in% c("north_stopover", "south_stopover")) %>% 
  group_by(tag.id)%>% 
  mutate(start_date = week(min(date_time)),
         end_date = week(max(date_time)))%>%
  #filter(type != "partial spring/breeding")  %>% 
  st_drop_geometry()%>% 
  select( -date_time, -type, , -subspecies,-tag.id.order)%>% 
  distinct() %>% 
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  )) |> 
  distinct()


# 
# 
# ## figure of duration 
# 
# ggplot(br_dur, aes(y=factor(movement_final))) +
#   geom_segment(aes(x=start_date, xend=end_date, y=factor(subpop_val), yend=factor(subpop_val),color=subpop_val), linewidth = 3)+
#   scale_color_brewer(palette = "Set1", name = "Subpopulation")+
#   xlim(10,40)+
#   #(aes(x=start_date, y=factor(subpop), size = 2), colour = "blue") +
#   #geom_point(aes(x=start_date, y=factor(subpop), size = 2), colour = "red") +
#   facet_wrap(~movement_final) +
#   #ggtitle("Breeding Locations") +
#   #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
#   xlab("Week of Year") + ylab("breeeding regions") +
#   theme_bw()
# #theme(legend.position = "none")



###############################################################################
## Animated graphic? 
###############################################################################
# 
# 
# ani_dur <- df_all_points  |> 
#   dplyr::mutate(date_day = yday(date_time)) 
# 
# ani_dur_spring <- cbind (ani_dur, st_coordinates(df_all_points))   %>%
#   st_drop_geometry() |> 
#    filter(date_day <200) |> 
#    filter(date_day >140) 
# # #  dplyr::filter(date_day <230)
# 
# 
# 
# ## Generate the plots: 
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# Americas <- world %>% dplyr::filter(continent == "North America")
# 
# # entire north America 
# global_north <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_point(data = ani_dur_spring, aes(x = X, y = Y, colour = subpop), size = 3) +#colour = "dark blue") +
#   scale_color_brewer(palette = "Set1", name = "Subpopulation")+
#   #facet_wrap(~tag.id)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   # xlab("Longitude") + ylab("Latitude") +
#   #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   coord_sf(xlim = c(-110, -65), ylim = c(45, 70), expand = FALSE)+
#   theme_bw()+
#   labs(colour = "Subpopulation") + 
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_blank(),
#     legend.position = "bottom",
#     legend.key.width = unit(3, "lines")
#   )
# 
# global_north
# 
# p_animate_north <- global_north + 
#   transition_time(date_day) + 
#   labs(title = "Date: {round(frame_time)}") +
#   #enter_fade()+
#   #ease_aes('bounce-out')+
#   enter_grow()  + enter_drift(x_mod = -1) + 
#   shadow_wake(wake_length = 0.1, size = 1.5, colour = "grey", alpha = 0.5,falloff = "cubic-in")+
#   exit_shrink() + exit_drift(x_mod = 5)
#   
#   #enter_drift(x_mod = -1) +
#   #shadow_trail(max_frames = 5)+
#   #shadow_wake(wake_length = 0.1, size = 2, colour = "grey", alpha = 0.8)+
#   #exit_fly()
#   #exit_shrink()
# 
# animate(
#   p_animate_north, 
#   width = 10, 
#   height = 8, 
#   units = "in", 
#   res = 72, 
#   fps = 1, #10 default  
#   nframes = 50
# )






################################################################################

## Delaware Bay / Atlantic coast stopover 

#############################################################################

db <- st_read(file.path(final_dat,"Delaware_Bay_usage.gpkg" )) 

db <- db  %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type,  tag.id.order)%>% 
  filter(movement_final %in% c("north_stopover", "south_stopover")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  ))

all <- db |> 
  group_by(tag.id,movement_final) |> 
  slice_min(n = 1,  order_by = tag.id.order)
  #slice_max(n = 1,  order_by = tag.id.order))
  #slice_sample(n = 1)

   # slice_min(n = 1,  order_by = tag.id.order))


########################################################
# Geographic distributon of tags 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = all, size = 2.5, alpha = 0.8, aes(colour = subpop_val, alpha = 0.8)) +#colour = "dark blue") +
  scale_color_brewer(palette = "Set1", name = "Subpopulation")+
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~movement_final) +
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global



#####################################################

## arrival dates - Del Bay - NORTH 

jb_arrive <- all |>  # all has the earliest date 
  st_drop_geometry()%>% 
  filter(movement_final == "north_stopover") |> 
  group_by(tag.id)%>%
  mutate(start_date = yday(min(date_time)),
         end_date = yday(max(date_time)))%>% 
  select( -subspecies)%>%
  distinct()%>% 
  arrange(start_date)


# depart dates - Del Bay - NORTH 

db_dep <- db  %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type,  tag.id.order)%>% 
  filter(movement_final %in% c("north_stopover")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  )) |> 
  group_by(tag.id,movement_final) |> 
  slice_max(n = 1,  order_by = tag.id.order)%>% # get the latest date 
  distinct() |> 
  st_drop_geometry()


# depart dates - Del Bay - SOUTH 

db_dep <- db  %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type,  tag.id.order)%>% 
  filter(movement_final %in% c("south_stopover")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  )) 
  
db_dep_arrive <-  db_dep |> 
  group_by(tag.id,movement_final) |> 
  slice_min(n = 1,  order_by = tag.id.order)%>% # get the latest date 
  distinct() |> 
  st_drop_geometry()

db_dep_dep <-  db_dep |> 
  group_by(tag.id,movement_final) |> 
  slice_max(n = 1,  order_by = tag.id.order)%>% # get the latest date 
  distinct() |> 
  st_drop_geometry()







################################################################################

# Northern Sth America 

###############################################################################


db <- st_read(file.path(final_dat,"Northern_SthAm_usageV1.gpkg" )) 

db <- db  %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type,  tag.id.order)%>% 
  filter(movement_final %in% c("north_stopover", "south_stopover", "wintering")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  ))

db_min <- db |> 
  group_by(tag.id,movement_final) |> 
  slice_min(n = 1,  order_by = tag.id.order)

db_max <- db |> 
  group_by(tag.id,movement_final) |> 
  slice_max(n = 1,  order_by = tag.id.order)

db_min_sth <- db_min |> filter(movement_final == "north_stopover")
db_max_sth <- db_max |> filter(movement_final == "north_stopover")

db_min_sth <- db_min |> filter(movement_final == "south_stopover")
db_max_sth <- db_max |> filter(movement_final == "south_stopover")

########################################################
# Geographic distributon of tags 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = db_min, size = 2.5, alpha = 0.8, aes(colour = subpop_val, alpha = 0.8)) +#colour = "dark blue") +
  scale_color_brewer(palette = "Set1", name = "Subpopulation")+
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~movement_final, nrow = 2) +
  coord_sf(xlim = c(-80, -25), ylim = c(-15, 20), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global





#####################################################

## arrival dates - Del Bay - NORTH 

jb_arrive <- all |>  # all has the earliest date 
  st_drop_geometry()%>% 
  filter(movement_final == "north_stopover") |> 
  group_by(tag.id)%>%
  mutate(start_date = yday(min(date_time)),
         end_date = yday(max(date_time)))%>% 
  select( -subspecies)%>%
  distinct()%>% 
  arrange(start_date)


# depart dates - Del Bay - NORTH 

db_dep <- db  %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type,  tag.id.order)%>% 
  filter(movement_final %in% c("north_stopover")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  )) |> 
  group_by(tag.id,movement_final) |> 
  slice_max(n = 1,  order_by = tag.id.order)%>% # get the latest date 
  distinct() |> 
  st_drop_geometry()


# depart dates - Del Bay - SOUTH 

db_dep <- db  %>%
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")%>% 
  left_join(pop_id)%>% 
  filter(subpop %in% c("west", "SE", "NSA", "South")) %>% 
  select(tag.id, date_time, month, movement_final, subspecies, subpop, type,  tag.id.order)%>% 
  filter(movement_final %in% c("south_stopover")) %>%
  mutate(subpop_val = case_when(
    subpop == "NSA" ~ "NSA",
    subpop == "South" ~ "Southern",
    subpop == "west" ~ "Western",
    subpop == "SE" ~ "SE",
  )) 

db_dep_arrive <-  db_dep |> 
  group_by(tag.id,movement_final) |> 
  slice_min(n = 1,  order_by = tag.id.order)%>% # get the latest date 
  distinct() |> 
  st_drop_geometry()

db_dep_dep <-  db_dep |> 
  group_by(tag.id,movement_final) |> 
  slice_max(n = 1,  order_by = tag.id.order)%>% # get the latest date 
  distinct() |> 
  st_drop_geometry()



