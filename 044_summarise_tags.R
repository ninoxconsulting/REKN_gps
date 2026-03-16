
## summary of all tag types 

library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
#install.packages("viridisLite")


#data_folder <- file.path("../../02_data/REKN_gps/data")
raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
final_dat <- file.path("../../02_data/REKN_gps/output_final/draft_outputs_2026")
out.plots <- file.path("../../02_data/REKN_gps/output_final/figures_2026")

# # read in the ref data
# ref <- read_csv(file.path(final_dat, "reference_data_raw_2020_2025_edited_20260124.csv"))
# 
# # read in combined location data 
# loc <- read_csv(file.path(final_dat, "location_data_2017_2025.csv"))
# 
# # read in moveclass data 
# df_all <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_edited.gpkg"))
# 
# 
# # drop the fields n
# df_all <- df_all |> 
#   select(-c(movement_final_old, bearing, diff, gcd_m, speed_mhr, algorithm.marked.outlier, location.lat_prior, location.long_prior, Event)) |> 
#   select(-timestamp)
# 
# st_write(df_all, (file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_movetype_20260125.gpkg")))
# 




## March 2026

# for the final analysis I am using the following files: 
# in the folder: 
# draft_outputs_2026

# 1) final_tags_list_edited_20260126.csv - key for the tags to match type and duration of tag location 
# 2) loc_2020_2025_movetype_20200125.gpkg = spatial datset with movement catergory 
# 3) paths_2020_2025_movtype_20260128.gpkg = line to match movetype file above

# movebank files 
# 4) loc_data_2017_2025.csv = full columns for movebank upload
# 5) referenc_data_2020_2025_20260124.csv = line to match movetype file above


ref <- read_csv(file.path(final_dat, "reference_data_2020_2025_20260124.csv"))
unique(ref$tag.comments)

ref <- ref |> 
  dplyr::filter(!tag.comments %in% c("tag failed"))


# read in the key 

tagls <- read_csv(file.path(final_dat, "final_tags_list_edited_20260126.csv"))
tagls <- tagls |> 
  select(tag.id, usable)

ref_y <- left_join(ref, tagls, by = "tag.id") |> 
  filter(usable =="y" )



# filtered location records to usable tags 
loc <- read_csv(file.path(final_dat, "location_data_2017_2025.csv")) |> 
  select(-c("...1", "...2")) |> 
  filter(tag.id %in% ref_y$tag.id)


# clean up the spatial resolution # can remove these cols. 
#unique(loc$argos.lc)
#unique(loc$lotek.crc.status)
#unique(loc$gps.fix.type.raw)

## How many tags and types of tage 
unique(ref$tag.model)

# quick fix for location data
#length(loc$proj)#
#
#loc <- loc |> 
#  dplyr::filter(year > 2000)
#length(loc$proj)

tag.proj
### SUMMARY OF ANIMALS ## NOTE THIS IS THE FULL DATA SET (n = 353)

# tags per project 
tag.proj <-  ref %>% 
  dplyr::select(tag.id, proj) %>%
  group_by( proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))


# tag number per tag type
tag.types <- ref %>% 
  dplyr::select(tag.id, tag.model ) %>%
  group_by(tag.model) |> 
  summarise(no.of.tags = length(unique(tag.id)))

tag.types <- ref %>% 
  dplyr::select(tag.id, tag.model, proj) %>%
  group_by(tag.model, proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))

# # type of fix per tag model (accuracy per tag type - only relevant for pinpoint)
# tag.types.model <- loc %>%
#  dplyr::select(tag.id, tag.model, proj, gps.fix.type.raw) %>%
#  group_by(gps.fix.type.raw, tag.model) |>
#  summarise(no.of.tags = length(unique(tag.id)))



### Capture location # usable tags

study.site <- ref_y |> 
  dplyr::select(tag.id, study.site, deploy.on.date, deploy.on.latitude,  deploy.on.longitude ) |> 
  group_by(study.site) |> 
  summarise(no.of.tags = length(unique(tag.id)))


study.site.proj <- ref |> 
  dplyr::select(tag.id, study.site, proj ) |> 
  group_by(study.site, proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))


# capture_loc <- ref |> 
#   dplyr::select(tag.id, study.site,  animal.life.stage, animal.sex, deploy.on.date, deploy.on.latitude,  deploy.on.longitude ) %>%
#   distinct() %>%
#   st_as_sf( coords = c("deploy.on.longitude", "deploy.on.latitude"), crs = 4326)
# 
# write out capture locations 
#write_sf(capture_loc, file.path(raw_dat , "maps", "capture_locations.gpkg"))




### Summary statistics

## Age / sex of individuals 

## capture age and sex 
sex_sum <- ref_y |> 
  dplyr::select(tag.id, study.site, proj, animal.life.stage, animal.sex) |> 
  group_by(animal.sex, proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))

age_sum <- ref_y |> 
  dplyr::select(tag.id, study.site, proj, animal.life.stage) |> 
  #group_by(animal.life.stage , study.site) |> 
  group_by(animal.life.stage, proj ) |> 
  #group_by(animal.life.stage ) |> 
  summarise(no.of.tags = length(unique(tag.id)))

age_sum <- ref_y |> 
  dplyr::select(tag.id, study.site, proj, animal.life.stage) |> 
  #group_by(animal.life.stage , study.site) |> 
  group_by(animal.life.stage,study.site ) |> 
  #group_by(animal.life.stage ) |> 
  summarise(no.of.tags = length(unique(tag.id)))




# ring-id for subpop information
rids <- ref %>%
  dplyr::select(animal.ring.id, tag.id, proj) %>% 
  group_by(animal.ring.id, tag.id ) %>%
  count()










### Duration statistics 


# the tags that need editing 
# 232985 - drop anything after 2022 as reords appearing in 2023 and 2025
loc_1 <- loc |> 
  filter(tag.id == 232985) |> 
  filter(year <= 2022)

loc <- loc |> 
  filter(tag.id != 232985)

loc <- bind_rows(loc, loc_1)


#232982
#potential dropped tag in southert wintering grounds 
#migration in 2023 tagged in delaware bay, breeding in arctic, return to TDF on 
#2023-10-14 - tag continued to transmitt from location for 2024 - june then again in 2025 April - Dec

loc_1 <- loc |> 
  filter(tag.id == 232982) |> 
  filter(year <=2023)

loc <- loc |> 
  filter(tag.id != 232982)

loc <- bind_rows(loc, loc_1)

#240164
#potential dropped tag along hudson Bay 
#migration in 2023  breeding in arctic, return sth dropped tag in HB
# last date in 2023? 
# recorded in 2024 an 2025 in same locations 

loc_1 <- loc |> 
  filter(tag.id == 240164) |> 
  filter(year <=2023)

loc <- loc |> 
  filter(tag.id != 240164)

loc <- bind_rows(loc, loc_1)

#240159
#potential dropped tag in breedign area
#migration in 2023  breeding in arctic from Brazil
# last date in 2023? 
# recorded in 2024 an 2025 in same locations 

loc_1 <- loc |> 
  filter(tag.id == 240159) |> 
  filter(year <=2023)

loc <- loc |> 
  filter(tag.id != 240159)

loc <- bind_rows(loc, loc_1)

# tag 242574
loc_1 <- loc |> 
  filter(tag.id == 242574) |> 
  filter(year <=2023)

loc <- loc |> 
  filter(tag.id != 242574)

loc <- bind_rows(loc, loc_1)


# tag 238555
loc_1 <- loc |> 
  filter(tag.id == 238555) |> 
  filter(year <=2023)

loc <- loc |> 
  filter(tag.id != 238555)

loc <- bind_rows(loc, loc_1)


# tag 238561
loc_1 <- loc |> 
  filter(tag.id == 238561) |> 
  filter(year <=2023)

loc <- loc |> 
  filter(tag.id != 238561)

loc <- bind_rows(loc, loc_1)

# tag 238576
loc_1 <- loc |> 
  filter(tag.id == 238576) |> 
  filter(year <=2023)

loc <- loc |> 
  filter(tag.id != 238576)

loc <- bind_rows(loc, loc_1)

# tag 213841
loc_1 <- loc |> 
  filter(tag.id == 213841) |> 
  filter(year <=2022)

loc <- loc |> 
  filter(tag.id != 213841)

loc <- bind_rows(loc, loc_1)



# tag 240167 - static after wintering 
loc_1 <- loc |> 
  filter(tag.id == 240167) |> 
  filter(year <= 2023) 

loc <- loc |> 
  filter(tag.id != 240167)

loc <- bind_rows(loc, loc_1)



# tag 242570 - static on departure
loc_1 <- loc |> 
  filter(tag.id == 242570) |> 
  filter(year <= 2023) 

loc <- loc |> 
  filter(tag.id != 242570)

loc <- bind_rows(loc, loc_1)


# tag 242580 - static 
loc_1 <- loc |> 
  filter(tag.id == 242580) |> 
  filter(year <= 2023) 

loc <- loc |> 
  filter(tag.id != 242580)

loc <- bind_rows(loc, loc_1)


# tag 240172 - static 
loc_1 <- loc |> 
  filter(tag.id == 240172) |> 
  filter(year <= 2024) 

loc <- loc |> 
  filter(tag.id != 240172)

loc <- bind_rows(loc, loc_1)


# tag 260698 - static 
loc_1 <- loc |> 
  filter(tag.id == 260698) |> 
  filter(year <= 2024) 

loc <- loc |> 
  filter(tag.id != 260698)

loc <- bind_rows(loc, loc_1)


# tag 232986 - static 
loc_1 <- loc |> 
  filter(tag.id == 232986) |> 
  filter(year <= 2022) 

loc <- loc |> 
  filter(tag.id != 232986)

loc <- bind_rows(loc, loc_1)

# tag 240171 - static 
loc_1 <- loc |> 
  filter(tag.id == 240171) |> 
  filter(year <= 2024) 

loc <- loc |> 
  filter(tag.id != 240171)

loc <- bind_rows(loc, loc_1)


# tag 240175 - static 
loc_1 <- loc |> 
  filter(tag.id == 240175) |> 
  filter(year <= 2024) 

loc <- loc |> 
  filter(tag.id != 240175)

loc <- bind_rows(loc, loc_1)


# # tag 240175 - static 
# loc_1 <- loc |> 
#   filter(tag.id == 240175) |> 
#   filter(year <= 2024) 
# 
# loc <- loc |> 
#   filter(tag.id != 240175)
# 
# loc <- bind_rows(loc, loc_1)
# 








# calculate the duration of the updated version of data 

range(loc$year)

table_max <- loc |> 
  dplyr::mutate(date_time1 = ymd_hm(date_time))|> 
  dplyr::select(tag.id, date_time1) |> 
  # group_by(tag.local.identifier)%>%  -->
  slice_max(date_time1, by = tag.id) |> 
  distinct()

colnames(table_max)<- c("tag.id","max") 

table_min <-  loc %>% 
  dplyr::mutate(date_time1 = ymd_hm(date_time)) |>   
  dplyr::select(tag.id, date_time1) %>%  
  # group_by(tag.local.identifier)%>%   
  slice_min(date_time1, by = tag.id)  |> 
  distinct()

colnames(table_min)<- c("tag.id", "min")  

dur <- left_join(table_max, table_min, by = join_by(tag.id)) |> 
  distinct() |>   
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |>  # days 
  mutate(dur_days = round( as.numeric(duration))) |> 
  mutate(year = year(min))  


dur_plot <- ggplot(dur, aes(y=factor(tag.id))) +  
  geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), size=1)+  
  xlab("Date") + ylab("Tag")   

dur_plot  

dur_hist <- ggplot(dur, aes(x= dur_days))+  
  geom_histogram() + #fill="white", position="dodge") +  
  scale_color_viridis_d()+
  #facet_wrap(~year)+  
  xlab("duration (days)")   

dur_hist  

# p_duration <- ggplot(dloc, aes(factor(month), fill = factor(year)))+  
#   geom_bar(position = "dodge") +
#   scale_color_viridis_d()+
#   #xlim(1,12)+  
#   #facet_wrap(~tag.local.identifier)+  
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
# 



# duration of tag per tag type 

dur_type <- dur %>% 
  left_join(ref) 

dur_hist <- ggplot(dur_type, aes(x= dur_days, fill = tag.model)) +  
  geom_histogram(position = "dodge") + #fill="white", position="dodge") +  
  scale_fill_viridis_d()+
  xlab("duration (days)")  

dur_hist


# generate the summary values for tabel in report

dur_type_sum <- dur_type |> select(tag.id , dur_days, tag.model)%>%
  distinct()%>% 
  filter(dur_days<340) |>  # note this drops the one pinpony 355 - so totals need to account for this. 
  group_by( tag.model) |> 
  summarise(count = n(),
            min = min(dur_days), 
            max = max(dur_days),
            mean_dur = mean(dur_days), 
            sd = sd(dur_days))


durdf <- dur_type |> 
  select(tag.id , dur_days, tag.model,attachment.type  ) |> 
  filter(tag.id !=194904)


ggplot( durdf, aes(y=dur_days , x=tag.model, fill = tag.model)) +
  geom_violin(width=1, alpha=0.2) +
  geom_boxplot(width=0.1, color="darkgrey", alpha=0.9) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ggtitle("A Violin wrapping a boxplot") +
  xlab("")+
  ylab("duration (days)")


ggsave(filename = fs::path(out.plots, "figure3_tag_duration.jpg"), 
       width = 13, height = 13, units = "cm")


## attachment type ################################

durdf

atttype <- durdf |> 
  group_by(attachment.type,tag.model) |> 
  summarise(count = n(),
            min = min(dur_days), 
            max = max(dur_days),
            mean_dur = mean(dur_days), 
            sd = sd(dur_days)) |> 
  filter(!is.na(attachment.type )) |> 
  filter(tag.model == "Sunbird Solar Argos") |> 
  select(-tag.model)







#####################################################################################

# seasonality by tag 

bs <- loc |> 
  dplyr::select(month, year, tag.id) %>% 
  distinct() %>%
  group_by(month, year) %>%
  count()
# mutate(sdate = ymd(ddate))

# no of tags per month (all years)

ggplot(bs, aes(x = as.factor(month), y = n)) +
  geom_col() 


# By year 
ggplot(bs, aes(x = as.factor(month), y = n, fill = as.factor(year))) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  scale_fill_viridis_d()+
  #geom_bar(stat = "identity", position = "dodge") #+
  facet_wrap(~year)+
  theme_bw()+
  xlab("month")+ ylab("count")+
  theme(legend.position = "none")



ggsave(filename = fs::path(out.plots, "figure6_tag_seasonality.jpg"), 
       width = 25, height = 20, units = "cm")



####################################################################################












#   
#   
# 
# ######################################################################
# ## Filter tags which cant be used 
# 
# bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv"))
# 
# edit_tags <- unique(bcat$tag.id)
# 
# 
# xx <- setdiff(edit_tags, dat_tags)
# # these tags already 
# #213948 229367 232341 232342 232344 239414 242699
# 
# 
# bcat <- left_join(bcat, model.id)
# 
# # filter the tags which cant be used 
# cant_use <- bcat |> 
#   filter(Catergory == "Not usable") %>%
#   dplyr::select(tag.id) %>%
#   pull()
# 
# 
# bd <- bdat %>% 
#   filter(! tag.id %in% cant_use)
# 
# # no of tags per project - note the duplicate atlantic and spring migration is still in this data set so 
# # need to manually remove the duplicates
# 
# no_ids_proj <- bd %>% 
#   group_by(proj)%>%
#   summarise(n_tags = length(unique(tag.id)))
# 
# 
# bdd <- bd |> 
#   dplyr::select(location.long, location.lat, gps.fix.type.raw, lotek.crc.status, sensor.type,
#                 argos.lc,  tag.model, date_time, year, month, day, hour, minute, tag.id)
# 


# Types of data : GPS vs argos

#The data split between gps and argos data records. Large proportion are argos doppler shift.   These represent two different methods of calculating the locations and all types can be used. 

tag_type <- bdd %>%
  dplyr::select(tag.id, sensor.type) %>%
  group_by(tag.id, sensor.type)%>%
  summarise(n = n())

#tag_type

tag_type_date <- bdd %>%
  dplyr::select(tag.id, sensor.type, year, month) %>%
  group_by(tag.id, sensor.type, year,month)%>%
  summarise(n = n())

#tag_type_date

p_alldat <- ggplot( bdd, aes(year, fill = sensor.type))+
  geom_bar(position = "dodge") 


p_alldat 


#####################################################################################

# Duration between pings/ 
bdd <- all |> 
  mutate(ddate = ymd_hms(date_time)) |> 
  arrange(tag.id, ddate,tag.model)

ggplot(bdd, aes(tag.id, diff)) +
  geom_col(aes(fill = tag.model))#, position = position_stack(reverse = TRUE))


# average and min max between gps 

bdd_dur_sum <- bdd_dur |> 
  group_by(tag.id) |> 
  filter(diff != 0) |> 
  summarise(mean = mean(diff, na.rm = T), 
            min = min(diff, na.rm = T), 
            max = max(diff, na.rm = T))

bdd_dur_sum <- left_join(bdd_dur_sum, bcat)%>%
  select(-X, -Subspecies, -Subpopulations, -Catergory, -To, -From, -Directon, -Duplicates, -animal.id)


# Checked these tags and correct 

# 232985 - sunbird 
# 232986 - sunbird 

# 229370 - sunbird? Mingan
# 240157 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 240155 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 240162 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 240160 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 242570 - sunbird , gap in timing but seems legit
# 242573 - sunbird , gap in timing but seems legit
# 240156 - sunbird - legit


# Number of ppoints and average frequency. 

ggplot(bdd_dur_sum, aes(mean, fill = tag.model)) +
  geom_bar(stat = "identity")#, position = position_stack(reverse = TRUE))





#  
# ###############################################
# # map data
# 
# ru <- ru[complete.cases(ru), ]
# rf_sf <- st_as_sf(ru, coords = c("lng","lat"), crs = 4326, agr = "constant")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# Americas <- world %>% 
#   dplyr::filter(region_un == "Americas")
# 
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = rf_sf, size = 1, colour = "dark blue") +
#   #facet_wrap(~common_name)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-170.15, -30), ylim = c(-60, 80), expand = FALSE)+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# global
# 
# #######################
# 
# rekn <- ru %>% 
#   filter(!is.na(lat)) %>%
#   filter(arr_month %in% c(4,5,6,7,8,9,10,11)) 
# 
# 
# rf_sf <- st_as_sf(rekn, coords = c("lng","lat"), crs = 4326, agr = "constant")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# Americas <- world %>% 
#   dplyr::filter(region_un == "Americas")
# 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = rf_sf , aes(color = arr_month), alpha = 0.5, size = 1) +
#   #facet_wrap(~arr_month, nrow = 3)+
#   scale_color_viridis_c() +
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-170.15, -30), ylim = c(-60, 80),, expand = FALSE)+
#   theme_minimal() +
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         legend.text=element_text(size=10))+
#   guides(color = guide_legend(override.aes = list(size = 3,alpha = 1)))
# 
# global 
# 
# 
# # map data
# 
# 
# bdat_sp <- ru 
# 
# 
# 
# #month_col = sort(unique(bdat_sp$arr_month))
# palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)
# 
# pal <- colorFactor(
#   palette = "viridis",
#   domain = unique(bdat_sp$tag.local.identifier))
# 
# tags <- unique(bdat_sp$tag.local.identifier)
# 
# 
# birdmapall <- leaflet(bdat_sp) %>%
#   # add a dark basemap
#   #addProviderTiles("CartoDB.DarkMatter") %>%
#   addProviderTiles("CartoDB") %>%
#   addCircleMarkers(lng = bdat_sp$lng, lat = bdat_sp$lat, 
#                    weight = 3, color = ~palette1(bdat_sp$arr_month), 
#                    fill = TRUE,
#                    label = ~arr_month,
#                    #radius = ~dur_days/10,
#                    fillColor = ~palette1(bdat_sp$arr_month)) %>%
#   #popup = ~animal.id) %>%
#   addPolylines(data = bdat_sp, lng = bdat_sp$lng, lat = bdat_sp$lat, 
#                color = "grey",  opacity = 0.1, stroke = TRUE) %>%
#   addLegend("bottomleft", pal = palette1, values = ~bdat_sp$arr_month,
#             title = "Arrival Month",
#             opacity = 1) 
# 
# birdmapall
# 
# 
