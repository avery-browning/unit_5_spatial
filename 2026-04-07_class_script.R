# 2026-04-07 class script - 5.2 points, lines, and polygons - vector data
# but ALSO multipoint, linestring, multilinestring, multipolygon, geometrycollection

library(sf) # "simple features" - meant to make spatial info play nice
# fxns preceded by st_ for spatial type
library(tidyverse)
library(mapdata)

# goal is to create a plot that show the location of deceased right whales

carcass = read.csv("data/RW_carcasses_2017.csv")
head(carcass)
dim(carcass) # 17 rows for 17 dead whales
USA_crit_hab = st_read(dsn = "data/North_Atlantic_Right_Whale_Critical_Habitat",
                      layer = "North_Atlantic_Right_Whale_Critical_Habitat")
USA_crit_hab # notice the crs is NAD83
# reproject crit hab polys into WGS84
st_crs(USA_crit_hab)$epsg
wgs84_epsg = 4326
USA_crit_hab_sf = st_transform(USA_crit_hab, crs = wgs84_epsg)

# read in canadian data

CAN_crit_hab = read.csv("data/NARW_canadian_critical_habitat_2017.csv")
head(CAN_crit_hab)
table(CAN_crit_hab$habitat)

# draw a line bwt points to create a closed polygon, have to start and end with the same pt, pts have to go in order
# turn pts into polys

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs = wgs84_epsg) %>%
  group_by(habitat, country) %>%
  summarize(do_union = F) %>% # if you don't include this, pts will get reordered
  st_cast(to = "POLYGON")


CAN_crit_hab_sf
head(USA_crit_hab_sf)
USA_crit_hab_sf$country = "USA"
USA_crit_hab_sf$habitat = c("GOM", "SEUS")
USA_crit_hab_sf = USA_crit_hab_sf %>%
  dplyr::select(country, habitat)
head(USA_crit_hab_sf)

crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)
crit_hab

long_bounds = c(-72, -54)
lat_bounds = c(39, 53)

library(mapdata)

world_map = map_data("wordHires", ylim = lat_bounds, xlim = long_bounds)

ggplot() +
  geom_polygon(data = world_map, aes(x = lon, y = lat, group = group), fill = "black") +
  geom_sf(data = crit_hab, aes(fill = country)) +
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position))
