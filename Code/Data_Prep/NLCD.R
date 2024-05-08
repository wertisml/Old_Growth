library(arrow)
library(tidyverse)
library(terra)
library(future)
library(FedData)
library(tidyterra)

setwd("~/Old_Growth")

source("Code/Data_Prep/Species_Grouping.R")

plan(multisession, workers = 12)

#==============================================================================#
# NLCD
#==============================================================================#

EcoRegions <- vect("C:/Users/wertisml/Documents/Fire/Shapefiles/EcoRegions/na_cec_eco_l2/NA_CEC_Eco_Level2.shp") %>% 
  tidyterra::select(NA_L2CODE, NA_L2NAME) %>%
  tidyterra::filter(NA_L2NAME == "OZARK/OUACHITA-APPALACHIAN FORESTS") %>%
  project("epsg:6933")

NLCD <- get_nlcd(template = EcoRegions,
                 label = "Appalachians",
                 year = 2019)

# TCC <- get_nlcd(template = EcoRegions,
#                 label = "Appalachians",
#                 dataset = "canopy",
#                 year = 2019,
#                 extraction.dir = "Files/gpkg/",
#                 raster.options =c("COMPRESS=NONE", "TFW=YES", "datatype=INT1U"),
#                 force.redo = T)

NLCD <- NLCD %>%
  rast() %>% 
  project(crs(EcoRegions))

# This crops the raster to the extent of the shp and then masks out the raster
# not within the shapefile bounds
c

#writeRaster(NLCD, "Files/gpkg/Appalachian_NLCD.tif", overwrite=T)

Canopy <- TCC %>%
  project(crs(EcoRegions))

Canopy <- crop(Canopy, EcoRegions, mask=T)

#==============================================================================#
# Forest
#==============================================================================#

# Filter to only keep the forest values
NLCD <- rast("Files/gpkg/Appalachian_NLCD.tif") %in% c(41,42,43)

cond <- open_dataset('Files/PLot_and_Cond_Regions.parquet') %>% 
  select(STATEAB, puid, cuid, FORTYPCD, CONDPROP_UNADJ, LAT, LON, EMAP_HEX, REGION, MEASYEAR) %>%
  filter(REGION %in% c("08", "09")) %>%
  collect() %>%
  arrange(cuid, desc(MEASYEAR), sum(!is.na(.))) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup() %>% 
  left_join(fread('Files/CONUS_hex_epa2_lut.csv'), by='EMAP_HEX')

App <- cond %>%
  filter(NA_L2NAME == "OZARK/OUACHITA-APPALACHIAN FORESTS") %>%
  cbind(Forest_Type(., FORTYPCD)) %>% 
  group_by(puid) %>%
  slice(which.max(CONDPROP_UNADJ)) %>%
  ungroup()


