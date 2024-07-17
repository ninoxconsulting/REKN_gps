
## summary of all tag types 

library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures")


# read in the ref data
ref <- read_csv(file.path(final_dat, "reference_data_edited.csv"))
df_all <- st_read(file.path(final_dat,"rekn_moveclass_20240716.gpkg" ))


# read in the key data
ref_key <- read_csv(file.path(final_dat, "final_tags_list_edited.csv"))
ref_id <- ref_key  %>% select("tag.id" , "proj", "subspecies", "subpop", 
                              "north", "breeding" , "south","wintering" ,  
                              "type", "usable"  )

tag.ids_y <- ref_id |> filter(usable == 'y')%>% select(tag.id)%>% pull()


# filter the refernce data 
ref_u <- ref |> filter(tag.id %in% tag.ids_y)



##############################################################################

## Age / sex of individuals 

## capture age and sex 
sex_sum <- ref_u |> 
  dplyr::select(tag.id, study.site, proj, animal.life.stage, animal.sex) |> 
  group_by(animal.sex, proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))


age_sum <- ref_u |> 
  dplyr::select(tag.id, study.site, proj, animal.life.stage) |> 
  group_by(animal.life.stage , study.site) |> 
  #group_by(animal.life.stage, proj ) |> 
  #group_by(animal.life.stage ) |> 
  summarise(no.of.tags = length(unique(tag.id)))


HY_movement <- ref_u %>% 
  dplyr::filter(animal.life.stage == "HY") |> 
  select(study.site, tag.id)

HY_tags <- HY_movement$tag.id 

# calcualte the duration of tags 
hy_sp <- df_all %>% 
  filter(tag.id %in% HY_tags) %>%
  group_by(tag.id) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts)%>% 
  st_drop_geometry() |> 
  distinct() %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(max_ts)) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |> 
  select(tag.id, min, duration)


HY_movement <- left_join(HY_movement, hy_sp)


# Atlantic shores
# "221847" - NOv banded - ocean winds - local movement USA
# "221860" - NOv banded - ocean winds - local movement USA
# "233930" -  Sept - ocean winds -- local movement USA
# "233927 -  Sept - ocean winds -- local movement USA
#
# MOnomoy 
# "234370" - August -Oceanwinds _ to bahamas - late departure
# 
# MIngagn 
# "194904" - 2020 banding Oct first shows up in Venezuela
# "229364" - 2022 - August band  - depart Nov - Nthstham 
# "229365"-  2022 - August band  - depart Nov - NthSth
# "229368" - 2022 - August band - depart Nov - NthSth
# "232345" - 2022 - August band - depart Nov - NthSth
# "232348" - 2022 - August band - depart Nov - NthSth
# "232349"- 2022 -August band - depart Nov - NthSth
# "232351" - 2022 - August band - depart Nov - NthSth
# "232353" - 2022 - August band - depart Nov - NthSth
# "239408" - August bank - depart Nov - NthSth
# "239409" - August bank - depart Nov - NthSth
# "239410" - August band - depart Nov - NthSth
# "239411" - August band - depart Nov - NthSth
# "239412" - August band - depart Nov - NthSth
# "239413" - August band - depart Nov - NthSth

# "239419" - August band - depart Nov - NthSth
# "239420" - August band - depart Nov - NthSth
# "239421" - August band - depart Nov - NthSth
# "239422" - August band - depart Nov - NthSth
# "239423" - August band - depart Nov - NthSth
# "239424"- August band - depart Nov - NthSth
# "239425"- August band - depart Nov - NthSth
# "242698" - August band - depart Nov - NthSth
# "242702" - August band - depart Nov - NthSth


# NOte of interest 
2022 cohort depart 
2023 got caught in storm 


# calculate the departure dates and success


# get first date of departure 
hy_st_depart <- df_all %>% 
  filter(tag.id %in% HY_tags) %>%
  group_by(tag.id, movement_final) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, movement_final)%>% 
  st_drop_geometry() |> 
  filter(movement_final == "south_migration") |> 
  distinct()


HY_movement <- left_join(HY_movement, hy_st_depart )%>% 
  filter(!is.na(movement_final))%>% 
  mutate(yr_dep = year(min_ts),
        mth_dep = month(min_ts),
        day_dep = day(min_ts)) %>% 
  filter(yr_dep == 2023)%>% 
  filter(mth_dep == 11)%>% 
  arrange(day_dep)


###################################

### if there is time explore this a little more. 


###############################################



# ## map out the HY 
# 
# rf_sf <- hy
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# Americas <- world %>% dplyr::filter(region_un == "Americas")
# #Americas <- world %>% dplyr::filter(continent == "North America")
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = rf_sf, size = 3,  aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
#   scale_fill_viridis_d(option = "magma",begin = 0.1)+
#   facet_wrap(~tag.id)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# global



######################

## Sex distribution 

######################


# review the data from refernce set

mf <- ref_u |> 
  filter(animal.sex %in% c("M", "F"))

mfids <- mf$tag.id


ref_u |> 
  group_by (animal.sex, proj, animal.life.stage) |> 
  count()

mfs <- mf |> 
  select(tag.id, animal.sex, animal.life.stage, proj) |> 
  st_drop_geometry() |> 
  distinct() 

# review the spatial data 


# calcualte the duration of tags 
mf_sp <- df_all %>% 
  filter(tag.id %in% mfids) %>%
  group_by(tag.id) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts)%>% 
  st_drop_geometry() |> 
  distinct() %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(max_ts)) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |> 
  select(tag.id, min, duration)

mf_movement <- left_join(mfs , mf_sp)

# get first date of departure 
mf_sp_sth <- df_all %>% 
  filter(tag.id %in% mfids) %>%
  group_by(tag.id, movement_final) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts, movement_final)%>% 
  st_drop_geometry() |> 
  filter(movement_final == "south_migration") |> 
  distinct()


mf_movement_all <- left_join(mf_movement , mf_sp_sth)



############# RUFA SUBSPECESI ###############################


mf_movement <- mf_movement_all |> 
  dplyr::filter(proj != "Johnson_GPS")


## Summary of tags in mingan 
# M 
# "194904" - 2020 - M - HY - recods in Venezuela
# "229363" - 2022 - M - AHY - depart 2022-09-01
# "232352" - 2022 - M - AHY -  depart 2022-09-018
# "232345" - 2022 - M - HY - 
# "229365" - 2022 - M - HY - 
# "229368" - 2022 - M - HY - 

# femal 
"229364" - HY (F) - 
"232348" - HY (F) - 
"232349" - HY (F) - 
"232351" - HY (F) - 
"232353" - HY (F) - 



# calcualte the duration of tags 
hy_sp <- df_all %>% 
  filter(tag.id %in% HY_tags) %>%
  group_by(tag.id) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts)%>% 
  st_drop_geometry() |> 
  distinct() %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(max_ts)) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |> 
  select(tag.id, min, duration)


HY_movement <- left_join(HY_movement, hy_sp)






allsf <- df_all %>%
  mutate(tag.id = as.character(tag.id),
         date_time = ymd_hms(date_time)) |> 
  filter(tag.id == 232352) %>%
  select(tag.id, date_time, day)

## map out odd individual 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = allsf, size = 2, colour = "dark blue") +
  #geom_sf_text(data = allsf, aes(label = day))+
  scale_fill_viridis_d(option = "magma",begin = 0.1)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-90, -40), ylim = c(0, 60), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global





############# ROSELARRI SUBSPECESI ###############################



# calcualte the duration of tags 
mf_sp <- df_all %>% 
  filter(tag.id %in% mfids) %>%
  group_by(tag.id) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts)%>% 
  st_drop_geometry() |> 
  distinct() %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(max_ts)) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |> 
  select(tag.id, min, duration)

mf_movement <- left_join(mfs , mf_sp)


mf_move_rose <- mf_movement |> 
  dplyr::filter(proj == "Johnson_GPS")

mf_rose_id <- mf_move_rose |> select(tag.id, animal.sex)

mf_move_yr <- mf_move_rose |> 
  mutate(yr = year(min)) |> 
  group_by(animal.sex, yr) |> 
  count()





## plot the differnce in male and female 

# get first date of departure 
mf_rose_sf <- df_all %>% 
  filter(tag.id %in% mfids)%>% 
  select(tag.id,date_time, movement_final)%>% 
  left_join(mf_rose_id) %>% 
  filter(movement_final != "uncertain_location")%>%
  filter(!is.na(animal.sex)) %>% 
  mutate(movement_final = case_when(
    movement_final == "deployment" ~ "north_migration",
    .default = movement_final
  )) |> 
  mutate(animal.sex = case_when(
    animal.sex == "M" ~ "Male",
    animal.sex == "F" ~ "Female",
    .default = animal.sex
  ))



###############################################################
# create a basic plot 

world <- ne_countries(scale = "medium", returnclass = "sf")
#Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America 
global <- ggplot(data = world) +
  geom_sf(color = "grey") +
  #geom_sf(data = mf_rose_sf, size = 1.2, alpha = 0.2,colour ="darkblue" ) + #aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  geom_sf(data = mf_rose_sf, size = 2.5,aes(fill = movement_final, colour = movement_final) ) + #aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_color_viridis_d(option = "viridis",begin = 0.1)+
  facet_wrap(~animal.sex)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-180, -20), ylim = c(-60, 80), expand = FALSE)+
  coord_sf(xlim = c(-185, -90), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global


global
ggsave(file.path(out.plots,"figure2_sex_rose.jpg"), width = 40, height = 20,units = "cm", dpi = 600)






########################################################

## arrivl at breeding grounds


# get first date of departure 
mf_sp_sth <- df_all %>% 
  filter(tag.id %in% mfids) %>%
  group_by(tag.id, movement_final) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts, movement_final)%>% 
  st_drop_geometry() |> 
  distinct()


mf_move_rose <- left_join(mf_move_rose , mf_sp_sth)


## arrival at breeding grounds 

breed_arrival <- mf_move_rose  |> 
  filter(movement_final == "breeding")%>% 
  select(tag.id, animal.sex, min_ts)%>% 
  arrange(animal.sex, min_ts)

sum_breed_arrive <- breed_arrival |> 
  group_by(animal.sex) |> 
  count()



## depart at breeding grounds 

breed_depart <- mf_move_rose  |> 
  filter(movement_final == "south_migration")%>% 
  select(tag.id, animal.sex, min_ts)%>% 
  arrange(animal.sex, min_ts)%>% 
  rename("breed_dep" = min_ts)

sum_breed_depart <- breed_depart  |> 
  group_by(animal.sex) |> 
  count()


# duration of breeding grounds


breed_dur = left_join(breed_depart, breed_arrival)%>% 
  filter(!is.na(min_ts)) %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(breed_dep )) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) %>% 
  select(animal.sex, duration)%>% 
  arrange(animal.sex, duration)


breed_dur_sum <- breed_dur %>%
  group_by(animal.sex) |> 
  mutate(average = mean(as.numeric(duration)))






## Summary of tags in mingan 
# M 
# "194904" - 2020 - M - HY - recods in Venezuela
# "229363" - 2022 - M - AHY - depart 2022-09-01
# "232352" - 2022 - M - AHY -  depart 2022-09-018
# "232345" - 2022 - M - HY - 
# "229365" - 2022 - M - HY - 
# "229368" - 2022 - M - HY - 

# femal 
"229364" - HY (F) - 
  "232348" - HY (F) - 
  "232349" - HY (F) - 
  "232351" - HY (F) - 
  "232353" - HY (F) - 
  
  
  
  # calcualte the duration of tags 
  hy_sp <- df_all %>% 
  filter(tag.id %in% HY_tags) %>%
  group_by(tag.id) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts)%>% 
  st_drop_geometry() |> 
  distinct() %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(max_ts)) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |> 
  select(tag.id, min, duration)


HY_movement <- left_join(HY_movement, hy_sp)











allsf <- df_all %>%
  mutate(tag.id = as.character(tag.id),
         date_time = ymd_hms(date_time)) |> 
  filter(tag.id == 232352) %>%
  select(tag.id, date_time, day)

## map out odd individual 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = allsf, size = 2, colour = "dark blue") +
  #geom_sf_text(data = allsf, aes(label = day))+
  scale_fill_viridis_d(option = "magma",begin = 0.1)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-90, -40), ylim = c(0, 60), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

