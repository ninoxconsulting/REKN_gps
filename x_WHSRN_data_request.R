## Data summary for the WHSRN identification of nesting locations 

# looking for the locations (+500m buffer) 
# - wintering 
# - southward stopover
# - northwarn stopover

library(dplyr)
library(sf)
library(readr)

final_dat <- file.path("../../02_data/REKN_gps/output_final")
out_dir <- file.path("../../02_data/WHSRN_data_request")

pop <- read_csv(file.path(final_dat, "final_tags_list_edited.csv"))
pop_id <- pop %>% 
  select("tag.id" , "proj", "subspecies", "subpop", 
         "north", "breeding" , "south","wintering" ,  
         "type", "usable"  ) |> 
  filter(usable == 'y') %>% 
  filter(subspecies == "rufa") #%>%
  #left_join(ref_due)

rufa_ids <- pop_id$tag.id

df_all <- st_read(file.path(final_dat,"rekn_moveclass_20240716.gpkg" )) %>% 
  filter(tag.id %in% rufa_ids ) %>% 
  filter(movement_final != "uncertain_location")

df_limit <- df_all %>% 
  filter(movement_final %in% c("north_stopover", "south_stopover", "wintering"))

st_write(df_limit, fs::path(out_dir, "WHSRN_redknot_generalised_gps_locals.gpkg"))
         
# buffer size 
# buffered in qgis using radius of 0.0005 degrees. 

# read back in and clean up the files



df_all <- st_read(file.path(out_dir,"WHSRN_redknot_buffered_all_cols1.gpkg")) 
df_all <- df_all |> 
  filter(!argos.lc %in% c("A", "0", "1", "B"))


df_sub <- df_all |> 
  select( tag.model, movement_final)

st_write(df_sub, fs::path(out_dir, "WHSRN_redknot_buffered_limited_cols.gpkg"), append = FALSE)

# 
# Type,	              Class,	   Estimated Error (meters),
# 
# GPS Positioning,	    G,	     <100 m
# Argos Doppler Shift,	3,	     < 250 m 
# Argos Doppler Shift,	2,	     > 250 m and < 500 m 
# Argos Doppler Shift,	1,	     > 500 m and < 1500 m
# Argos Doppler Shift,	0, 	     > 1500 m radius
# Argos Doppler Shift,	A,     	Unbounded accuracy estimation (3 messages)
# Argos Doppler Shift,	B, 	    Unbounded accuracy estimation (1 or 2 messages)
# Argos Doppler Shift,	Z,	    Invalid Location





  


