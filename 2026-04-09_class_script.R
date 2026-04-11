# 2026-04-09 class script - intersections and distance calculations (5.3)

# .zst is a zipped .csv

library(tidyverse)
library(mapdata)
library(sf)
library(lubridate)

ais_day = read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais_day)
dim(ais_day)

# sog = speed over ground, cog = course over ground

lon_bounds = c(-82, -76)
lat_bounds = c(25, 34)

world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

# read in USA crit hab from previous class
USA_crit_hab = st_read(dsn = "data/North_Atlantic_Right_Whale_Critical_Habitat",
                      layer = "North_Atlantic_Right_Whale_Critical_Habitat")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgray") +
  geom_sf(data = USA_crit_hab, fill = "brown") +
  geom_point(data = ais_day, aes(x = longitude, y = latitude, color = call_sign)) +
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) +
  guides(color = "none") +
  theme_classic()

# what vessels are in the crit hab? which are breaking the law?
# grab ais points in the crit hab

Sys.time() # prints out time on your computer - you could use this to see how long processes are taking
ships_RW_intersect = ais_day %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>%
  st_intersection(USA_crit_hab) %>% 
  dplyr::select(geometry)

head(ships_RW_intersect)
dim(ships_RW_intersect)

# come back to - something is wrong
law_breakers = ships_RW_intersect %>%
  filter(length>20,
    sog>10)
head(law_breakers)
unique(law_breakers$vessel_name)
class(law_breakers$base_date_time)
#

illegal_paths = law_breakers %>%
  mutate(date_time = lubridate::ymd_hms(base_date_time)) %>%
  arrange(date_time) %>%
  group_by(call_sign) %>%
  summarize(do_union = F) %>%
  st_cast("LINESTRING") %>%
  st_make_valid() # are all of your lines valid lines? (are there at least 2 pts?) - this will dump nonvalid lines

head(illegal_paths)
class(illegal_paths$date_time)

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group=group, fill = "black")) +
  geom_sf(data = USA_crit_hab, fill = "yellow") +
  geom_sf(data = illegal_paths, aes(color = call_sign)) + 
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) +
  geom_point(aes(x = -81.09, y = 32.08), shape = 8, color = "hotpink") +
  theme_classic()

illegal_path_lengths = illegal_paths %>%
  mutate(track_length_m = st_length(geometry))

head(illegal_path_lengths)

sum(illegal_path_lengths$track_length_m)