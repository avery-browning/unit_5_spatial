# 2026-04-02 class script

library(tidyverse)
# install.packages("raster")
library(raster) # reads our net CDF files
# install.packages("mapdata")
library(mapdata)  # map_data worldHires coastline - holds the coastline data
# install.packages("marmap")
library(marmap)   # getNOAA.bathy()
# library(ncdf4)  # also good for reading netCDF files
library(dplyr)

# Rasters - working in L3 today
# Rasters need an extent (boundaries of your map), origin (point closest to 0,0), and 
# resolution (the size of a single grid cell)

# Winyah Bay - use L2 (final project)
chl_raster = raster("unit_5_spatial/data/AQUA_MODIS.20020701_20250731.L3m.MC.CHL.chlor_a.9km.nc")
class(chl_raster)
chl_raster

names(chl_raster) = "chl_a"

chl_pts = raster::rasterToPoints(chl_raster, spatial = T)
class(chl_pts)
chl_df = data.frame(chl_pts)
summary(chl_df)

hist(chl_df$chl_a)
hist(log10(chl_df$chl_a))

cols = rainbow(n = 7, rev = T)[-1]

global_chl_map = ggplot() +
  geom_raster(data = chl_df , aes(x = x, y = y, fill = log10(chl_a))) +
  scale_fill_gradientn(colors = cols, limits = c(-1.75, 0.75), 
                        oob = scales::squish, name = "log_10(chl_a)") +
  theme_classic()
# ggsave(global_chl_map, filename = "unit_5_spatial/figures/my_chl_map.png", height = 5, width = 9)

global_chl_map

# subset Gulf of Maine

lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

# crop the raster - remember, if you were cropping a dataframe you would use filter

chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))
chl_GOM_raster

chl_GOM_df = data.frame( rasterToPoints(chl_GOM_raster, spatial = T))
chl_GOM_df
head(chl_GOM_df)

world_map = map_data("worldHires") # Hi res - no it's no camelCase, but it's someone else's fxn
head(world_map) 

GOM_chl_map = ggplot() +
  geom_raster(data = chl_GOM_df, aes(x = x, y = y, fill = log10(chl_a))) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") +
  coord_fixed(xlim = lon_bounds, ylim = lat_bounds, expand = F) +
    scale_fill_gradientn(colors = cols, limits = c(-1, 1.75), 
                        oob = scales::squish, name = "log_10(chl_a)") +
  theme_bw()
GOM_chl_map
ggsave(GOM_chl_map, filename = "unit_5_spatial/figures/GOM_chl_map.png", height = 5, width = 9)

# bathymetry

lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], lon2 = lon_bounds[2],
                                    lat1 = lat_bounds[1], lat2 = lat_bounds[2]
)
class(bath_m_raw)

bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)
summary(bath_m_df)

bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>%
  dplyr::select(-z)

head(bath_m)

GOM_bath_map = ggplot() +
  geom_raster(data = bath_m, aes(x = x, y = y, fill = depth_m)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                                    fill = "gray", color = "black") +
  scale_fill_gradient(colors = c("black", "darkblue", "lightblue"), 
                      values = scales::rescale(c(-6000, -300, 0)), 
                      name = "Depth (m") +
  geom_contour(data = bath_m, aes(x = x, y = y, z = depth_m), breaks = c(-500), color = "red") +
  geom_contour(data = bath_m, aes(x = x, y = y, z = depth_m), breaks = c(-250), color = "green") +
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand = F) +
  theme_bw()

install.packages("sf")

# --------------class start 2026-04-07

ggsave(GOM_bath_map, filename = "figures/my_bathy_map.png")

# combine bathymetry and chl

bath_m_raster = marmap::as.raster(bath_m_raw)
bath_m_raster
chl_GOM_raster

