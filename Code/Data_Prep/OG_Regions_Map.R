library(terra)
library(sf)
library(tidyverse)

setwd("~/Old_Growth/ARC_Vis")

Montana <- st_read("County_Boundaries/County_Boundaries.shp")
Wyoming <- st_read("tl_2023_56_cousub/tl_2023_56_cousub.shp") 

NWFP <- vect("R6_Forest.shp")

FS_Regions <- st_read("S_USA.AdministrativeRegion/S_USA.AdministrativeRegion.shp") %>%
  filter(REGION < 10) %>%
  vect()

Region_6 <- st_read("S_USA.AdministrativeRegion/S_USA.AdministrativeRegion.shp") %>%
  filter(REGION == "06") %>%
  vect()

Region_1 <- st_read("S_USA.AdministrativeRegion/S_USA.AdministrativeRegion.shp") %>%
  filter(REGION == "01") %>%
  vect()

#=================================================================#
# Cropping data
#=================================================================#

Western_Montana <- Montana %>%
  filter(NAMELABEL %in% c("Lincoln", "Flathead", "Lake", "Sanders", "Mineral", "Missoula", "Ravalli", 
                        "Powell", "Lewis and Clark", "Granite", "Deer Lodge", "Jefferson", "Broadwater", 
                        "Silver Bow", "Beaverhead", "Madison", "Gallatin", "Park")) %>%
  dplyr::select(NAME, geometry) %>%
  #st_union() %>%
  #vect()
  
north_tip <- st_intersection(Northern_Wyoming, st_as_sf(Region_1))


Northern_Wyoming <- Wyoming %>%
  st_transform(crs(Montana)) %>%
  st_intersection(st_as_sf(Region_1)) %>%
  dplyr::select(NAME, geometry)

Western_Montana_R <- rbind(Western_Montana, Northern_Wyoming) %>%
  filter(NAME %in% c(Western_Montana$NAME, "Yellowstone National Park")) %>%
  st_union() %>%
  vect()

xmin <- -135 #10.6 #-135 # minimum x-coordinate -95.78502
xmax <- -55 #159 #-55  # maximum x-coordinate -95.25329
ymin <- 25 #50  # minimum y-coordinate 51.21902
ymax <- 50

# Bounding Box
bbox <- ext(xmin, xmax, ymin, ymax)

FS_Regions <- crop(FS_Regions, bbox)

FS_Regions <- project(FS_Regions, NWFP)
Region_1 <- project(Region_1, NWFP)
Region_6 <- project(Region_6, NWFP)

#=================================================================#
#
#=================================================================#

split <- rbind(Region_1, Western_Montana_R)

buffered_boundaries <- st_buffer(st_as_sf(Western_Montana_R), dist = 1)

result <- st_difference(st_as_sf(split), buffered_boundaries)

#=================================================================#
#
#=================================================================#

split <- rbind(Region_6, NWFP) 

PNW <- st_difference(st_as_sf(NWFP), st_as_sf(Region_6)) %>% vect()

NWFP_Region_6 <- st_intersection(st_as_sf(NWFP), st_as_sf(Region_6)) %>%
  mutate(REGION ="06.1") %>%
  select(REGION, geometry) %>%
  vect()

not_NWFP_Region_6 <- st_difference(st_as_sf(Region_6), st_as_sf(NWFP)) %>%
  mutate(REGION = "06.2") %>%
  select(REGION, geometry) %>%
  vect()

joined_Region_6 <- rbind(NWFP_Region_6, not_NWFP_Region_6) 

writeVector(joined_Region_6, "Region_6.shp")

#=================================================================#
#
#=================================================================#

Region_1 <- st_read("Region_1/Region_1.shp") %>%
  vect()

Regions <- st_read("S_USA.AdministrativeRegion/S_USA.AdministrativeRegion.shp") %>%
  filter(!REGION %in% c("06", "01", "10")) %>%
  vect()

Regions <- crop(Regions, bbox)

joined_Regions <- rbind(Regions, joined_Region_6, Region_1) %>%
  st_as_sf()

st_write(joined_Regions, "OG/FS_Regions_OG.shp")




