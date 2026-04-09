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

lon_bounds = c(-72, -54)
lat_bounds = c(39, 53)

library(mapdata)

world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)


# come back to - scale error

ggplot() +
  geom_raster(data = bath_m, aes(x = x, y = y, fill = depth_m)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") +
  geom_sf(data = crit_hab, aes(fill = "blue")) +
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position)) +
  scale_fill_gradientn(colors = c("black", "navy", "blue4", "lightblue"), values = scales::rescale(-5000, -3000, -300, 0),
name = "Depth (m)") +
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) +
  theme_classic()

# only 1 whale carcass was found inside the critical habitat, others were outside those boundaries
# mismatch on what we are protecting and where the whales actually are

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1] - 2,
                                  lon2 = lon_bounds[2] + 2,
                                  lat1 = lat_bounds[1] - 2,
                                  lat2 = lat_bounds[2] + 2
                                  )

class(bath_m_raw)
bathy_m_fortify = marmap::fortify.bathy(bath_m_raw)
head(bathy_m_fortify)
bath_m = bathy_m_fortify %>%
  mutate(depth_m = ifelse(z>20, NA, z))
tail(bath_m)

# gg new scale will let you use multiple scales
# gg plot layers geometry in the order that it was coded