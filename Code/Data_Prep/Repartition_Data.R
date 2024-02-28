library(terra)
library(tidyverse)
library(arrow)
library(future)
library(sf)

setwd("~/Old_Growth")

#==============================================================#
#
#==============================================================#

cond <- open_dataset('Files/cond_and_plot.parquet') %>% 
  select(-BAPA_LIVE, -BAPA_DEAD) %>%
  collect() %>%
  data.frame()

FS_OG_Regions <- vect("ARC_Vis/OG/FS_Regions_OG.shp")

#==============================================================#
# Add regions to cond
#==============================================================#

plan(multisession, workers = 12)

simple_cond <- cond %>% 
  select(cuid, LAT, LON) %>% 
  distinct(cuid, .keep_all = TRUE)

cond_sf <- vect(simple_cond, geom = c("LON", "LAT"), crs = crs(FS_OG_Regions))

FS_OG_Regions <- FS_OG_Regions[, c("REGION", "REGIONNAME")]

# Perform a spatial join
result <- st_join(st_as_sf(cond_sf), st_as_sf(FS_OG_Regions))

result_df <- result %>%
  as.data.frame() %>%
  select(-geometry)

Cond <- cond %>% 
  distinct(cuid, .keep_all = TRUE) %>%
  select(-REGION, -LAT, -LON) %>%
  left_join(result_df, by = "cuid") 

plan(sequential)

write_parquet(Cond, "PLot_and_Cond_Regions.parquet")

#==============================================================#
# Combine Tree and Cond
#==============================================================#

plan(multisession, workers = 12)

Trees <- list.files(path = "Files/rFIA/",
                    pattern = "_TREE.csv$",
                    recursive = T,
                    full.names = T)

combined_data <- lapply(Trees, function(file) {
  read_csv(file, col_select = c("PLT_CN", "STATECD", "UNITCD", "COUNTYCD",
                                "PLOT", "CONDID", "DIA", "TPA_UNADJ",
                                "CULL", "SPCD", "HT", "STANDING_DEAD_CD", 
                                "STATUSCD", "INVYR", "TREE"))}) %>%
  bind_rows() %>%
  mutate(cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_")) %>%
  select(-STATECD, -UNITCD, -COUNTYCD, -PLOT, -CONDID)

PC_Regions <- open_dataset("Files/PLot_and_Cond_Regions.parquet") %>%
  select(cuid, PLT_CN, REGION) %>%
  collect()

Tree_Regions <- combined_data %>% 
  left_join(PC_Regions, by = join_by(PLT_CN, cuid)) %>%
  distinct(.keep_all = TRUE)

plan(sequential)

write_parquet(Tree_Regions, "Files/Trees.parquet")






