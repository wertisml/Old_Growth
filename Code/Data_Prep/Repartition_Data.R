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

FS_OG_Regions <- vect("Files/gpkg/FS_Regions_OG.shp")

Fire_Shed <- st_read("Files/gpkg/usfs_firesheds_epsg4326.gpkg") %>%
  select(Fireshed_Code, geom) %>%
  vect()

EMAP <- st_read('Files/gpkg/conus_hex_epsg6933.gpkg') %>%
  select(EMAP_HEX, geom) %>%
  vect()

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
  select(-REGION) %>%
  left_join(result_df, by = "cuid") 

# Capture all the NA REGION values that occur due to poor lat/lon
NA_REGION_sf <- Cond %>%
  filter(is.na(REGION)) %>%
  select(cuid) %>%
  left_join(simple_cond, by = 'cuid') %>%
  vect(geom = c("LON", "LAT"), crs = crs(FS_OG_Regions))

nearest <- st_nearest_feature(st_as_sf(NA_REGION_sf), st_as_sf(FS_OG_Regions))

NA_REGION_sf$REGION <- FS_OG_Regions$REGION[nearest]

# Fix the missing EMAP locations similar to REGION

na_EMAP_HEX <- Cond %>% 
  filter(is.na(EMAP_HEX)) %>% 
  select(cuid, LAT, LON) %>%
  st_as_sf(., coords=c('LON','LAT'), remove=F, crs=st_crs(4326)) %>% 
  st_transform(6933) %>%
  vect()

nearest <- st_nearest_feature(st_as_sf(na_EMAP_HEX), st_as_sf(EMAP))

na_EMAP_HEX$EMAP_HEX <- EMAP$EMAP_HEX[nearest]

# Fix the missing Fire Shed locations similar to REGION
sf_use_s2(FALSE)

na_Fire_Shed <- Cond %>% 
  filter(is.na(Fireshed_Code)) %>% 
  select(cuid, LAT, LON) %>%
  vect(geom = c("LON", "LAT"), crs = crs(Fire_Shed))

nearest <- st_nearest_feature(st_as_sf(na_Fire_Shed), st_as_sf(Fire_Shed))

na_Fire_Shed$Fireshed_Code <- Fire_Shed$Fireshed_Code[nearest]

# Join everythign back together
Cond <- Cond %>%
  left_join(na_Fire_Shed %>%
              as.data.table(),
            by = "cuid", suffix = c("_df1", "_df2")) %>%
  mutate(Fireshed_Code = coalesce(Fireshed_Code_df2, Fireshed_Code_df1)) %>%
  select(-Fireshed_Code_df2, -Fireshed_Code_df1) %>%
  left_join(NA_REGION_sf %>%
              as.data.table(),
            by = "cuid", suffix = c("_df1", "_df2")) %>%
  mutate(REGION = coalesce(REGION_df2, REGION_df1)) %>%
  select(-REGION_df2, -REGION_df1) %>%
  left_join(na_EMAP_HEX %>%
              as.data.table() %>%
              select(-LAT, -LON),
            by = "cuid", suffix = c("_df1", "_df2")) %>%
  mutate(EMAP_HEX = coalesce(EMAP_HEX_df2, EMAP_HEX_df1)) %>%
  select(-EMAP_HEX_df2, -EMAP_HEX_df1)

plan(sequential)

write_parquet(Cond, "Files/PLot_and_Cond_Regions.parquet")

#==============================================================#
# Combine Tree and Cond
#==============================================================#

plan(multisession, workers = 12)

Trees <- list.files(path = "Files/rFIA/",
                    pattern = "_TREE.csv$",
                    recursive = T,
                    full.names = T)

combined_data <- lapply(Trees, function(file) {
  read_csv(file, col_select = c("CN" ,"PLT_CN", "STATECD", "UNITCD", "COUNTYCD",
                                "PLOT", "CONDID", "DIA", "TPA_UNADJ",
                                "CULL", "SPCD", "HT", "STANDING_DEAD_CD", 
                                "STATUSCD", "INVYR", "TREE", "TOTAGE", "CCLCD"))}) %>%
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






