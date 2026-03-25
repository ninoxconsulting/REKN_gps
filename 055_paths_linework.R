## summarise the line types

library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)
library(readr)

#
# #data_folder <- file.path("../../02_data/REKN_gps/data")
# raw_dat <- file.path("../../02_data/REKN_gps/output_temp")
# final_dat <- file.path("../../02_data/REKN_gps/output_final")
#
#
# # read in moveclass data
# pathraw <- st_read(file.path(raw_dat, "locations_raw_2025", "paths_2020_2025_movetype_20260128.gpkg"))
#
#
# head(pathraw)
# path <- pathraw |>
#   select(tag.id, from_id,  to_id, from_date_time, to_date_time,from_movement_final, to_movement_final)
#
# st_write(path, file.path(raw_dat, "locations_raw_2025", "paths_2020_2025_movetype.gpkg"))
#
#
# # potential to rerun this based on the paireed down version.


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


# data_folder <- file.path("../../02_data/REKN_gps/data")
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
  select(
    "tag.id", "proj", "subspecies", "subpop",
    "north", "breeding", "south", "wintering",
    "type", "usable"
  ) |>
  filter(usable == "y") %>%
  # filter(subspecies == "rufa") %>%
  left_join(ref_due)

rekn_ids <- pop_id$tag.id

## read in compiled data with movements and limit to rufa

# df_all <- st_read(file.path(raw_dat, "locations_raw_2025", "loc_2020_2025_movetype_20260125.gpkg")) |>
#  #filter(tag.id %in% rekn_ids ) %>%
#  filter(movement_final != "uncertain_location")

df <- st_read(file.path(final_dat, "loc_2020_2025_movetype_20260125.gpkg")) |>
  filter(tag.id %in% rekn_ids) |>
  filter(movement_final != "uncertain_location") |>
  filter(movement_final != "outlier") |>
  filter(movement_final != "outliers")

# unique(df$tag.id)

points <- df
proj <- sf::st_crs(points)
points <- points |>
  group_by(tag.id) |>
  arrange(date_time) |>
  ungroup() |>
  mutate(id_order = seq(1, length(points$id), 1))

points <- points |> tibble::rowid_to_column("idr")

## convert GPSPoints to a table for manipulation
GPSPoints <- cbind(points, sf::st_coordinates(points))
GPSPoints <- GPSPoints |> sf::st_drop_geometry()

# iterate through transect id
tag_id <- unique(GPSPoints$tag.id)
#tag_id <- tag_id[1:200]

all_lines <- purrr::map(tag_id, function(x) {
  # x <- tag_id[1] # testing line
  #print(x)
  # x = 242570

  GPSPoints_transect <- GPSPoints |>
    dplyr::filter(tag.id == x)

  ## Define the Line Start and End Coordinates and Add XY coordinates as

  GPSPoints_transect |>
    dplyr::mutate(
      Xend = dplyr::lead(.data$X),
      Yend = dplyr::lead(.data$Y),
      DTend = dplyr::lead(.data$date_time)
    ) |>
    dplyr::filter(!is.na(.data$Yend)) |>
    dplyr::rowwise(.data$id_order) |>
    dplyr::mutate(geometry = sf::st_sfc(
      sf::st_linestring(
        x = matrix(c(.data$X, .data$Xend, .data$Y, .data$Yend), ncol = 2)
      )
    )) |>
    sf::st_sf(crs = proj)
}) |> dplyr::bind_rows()

# check 1:100
unique(all_lines$tag.id)

# all_lines <- sf::st_make_valid(all_lines)
al<- all_lines |>
  mutate(date_time_from = date_time) |>
  mutate(date_time_to = DTend) |>
  select(proj, tag.id, date_time_from, year, month, day, hour, date_time_to, stopover, movement_final)


sf::st_write(al, fs::path(final_dat, "paths_2017_2025_movetype_20260324.gpkg"), driver = "GPKG", append = FALSE)



#
#
# ## Need to remove excess lines -- currently there are lines that run between the plots
# within <- lengths(sf::st_within(all_lines, planT)) > 0
# all_lines <- all_lines[within, ]
#
# all_lines <- sf::st_make_valid(all_lines)
#
# # drop any points produced by line segment creation
# geom_type <- as.character(unique(sf::st_geometry_type(all_lines, by_geometry = TRUE)))
#
# if ("POINT" %in% geom_type) {
#   all_lines<- all_lines[grep("POINT", sf::st_geometry_type(all_lines, by_geometry = TRUE), invert = TRUE),]
#   all_lines <- sf::st_make_valid(all_lines)
# }
#
# all_lines <- all_lines |> dplyr::select(-c("X", "Y", "TID", "ID", "Xend", "Yend"))
#
#
# sf::st_write(all_lines, out_loc, driver = "GPKG", append = FALSE)
#
