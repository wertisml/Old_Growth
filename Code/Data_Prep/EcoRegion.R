library(terra)
library(tidyverse)
library(future)

setwd("~/Old_Growth")

hex <- vect('Files/gpkg/conus_hex_epsg6933.gpkg') 
EcoRegions <- vect("C:/Users/wertisml/Documents/Fire/Shapefiles/EcoRegions/na_cec_eco_l2/NA_CEC_Eco_Level2.shp")

EcoRegions <- EcoRegions[,1:2]

# Change the projection
EcoRegions <- project(EcoRegions, hex)
# Crop to match the shape of hex
EcoRegions <- crop(EcoRegions, hex)

y <- rast(ext(EcoRegions), resolution = 500)

cropped_raster <- rasterize(EcoRegions, y, "NA_L2CODE")

plan(multisession, workers = 12)
Eco <- terra::extract(cropped_raster, hex, method = "linear", fun = max, na.rm = T)

Eco_Reg <- EcoRegions %>% 
    as.data.frame() %>% 
    distinct(.keep_all = TRUE) %>%
    mutate(ID = row_number() - 1)
  
result_df <- data.frame(EMAP_HEX = hex$EMAP_HEX,
                          Eco) %>%
  rename(Code = NA_L2CODE) %>%
  select(-ID) %>%
  left_join(Eco_Reg, by = c("Code" = "ID")) %>%
  rename(EPA2_code = NA_L2CODE) %>%
  select(-Code) %>%
  drop_na()

write_csv(result_df, "Files/CONUS_hex_epa2_lut.csv")
