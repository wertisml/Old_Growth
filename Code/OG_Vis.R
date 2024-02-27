library(arrow)
library(tidyverse)
library(terra)
library(sf)

setwd("~/MOG")

FS_OG_Regions <- vect("ARC_Vis/OG/FS_Regions_OG.shp")

Regions <- open_dataset("Files/OG_Regions/") %>% # This reads in the whole folder of parquet files
  # You can do /Region1.1.parquet if you wanted to look at an individual region
  select(cuid, Old_Growth, STATEAB, REGION, contains("class")) %>% # select the columns you want before reading into memory
  # You can add filter or some forms of mutate here before collecting.
  #filter(REGION == "06") %>%
  collect() # Reads into memory, prior to this you only see the metadata.

location <- open_dataset('Files/cond_and_plot.parquet') %>% 
  select(cuid, LAT, LON, STATEAB) %>% 
  filter(STATEAB %in% Regions$STATEAB) %>%
  collect() %>%
  distinct(cuid, .keep_all = TRUE) 

Regions_OG <- Regions %>%
  left_join(location)

cond_sf <- vect(Regions_OG, geom = c("LON", "LAT"), crs = crs(FS_OG_Regions))

OG_Map <- st_join(st_as_sf(cond_sf), st_as_sf(FS_OG_Regions))

plot(OG_Map[2])
st_write(OG_Map, "ARC_Vis/OG_Plots/Regions.shp")

#=================================================================#
# Viz points
#=================================================================#

FS_OG_Regions <- vect("ARC_Vis/OG/FS_Regions_OG.shp")

data <- open_dataset("Files/Step_2/All_Regions_EMAP_HEX.parquet") %>%
  select(REGION, Old_Growth, LON, LAT) %>%
  collect() %>%
  drop_na(Old_Growth)

data_sf <- vect(data, geom = c("LON", "LAT"), crs = crs(FS_OG_Regions))

OG_Spots <- st_join(st_as_sf(data_sf), st_as_sf(FS_OG_Regions))

OG_Spots <- OG_Spots %>%
  mutate(REGION = round(as.numeric(REGION.x)),
         Color = ifelse(REGION == 1, '#052114',
                         ifelse(REGION == 2, '#541700',
                                 ifelse(REGION == 3, '#003366',
                                         ifelse(REGION == 4, '#035630',
                                                 ifelse(REGION == 5, '#555599',
                                                         ifelse(REGION == 6, '#e97042',
                                                                 ifelse(REGION == 8, '#edd000', '#66cdaa'))))))))

plot(FS_OG_Regions, axes=F)
plot(OG_Spots[2], col = OG_Spots$Color, add=T, pch = 20)

st_write(OG_Spots, "Old_Growth")

data %>%
  mutate(REGION = round(as.numeric(REGION))) %>%
  group_by(REGION) %>%
  summarise(Old_Growth = n())

#=====================================================================#
# Old Growth Breakdown by Region
#=====================================================================#

open_dataset("Files/Step_2/All_Regions_EMAP_HEX.parquet") %>% 
  collect() %>%
  drop_na(REGION) %>%
  mutate(REGION = round(as.numeric(REGION)),
         Old_Growth = as.numeric(Old_Growth == 'Old Growth'),
         OG_prop = Old_Growth * CONDPROP_UNADJ,
         forested_prop = forested * CONDPROP_UNADJ, # Forested is just cond != 999
         forested_prop = replace(forested_prop, COND_STATUS_CD %in% c(3,4,5),NA)) %>% 
  group_by(REGION) %>%
  summarise(OG = sum(Old_Growth, na.rm=T),
            Forested_Plot = sum(forested, na.rm=T)) %>%
  mutate(Percent_OG = (OG/Forested_Plot)*100,
         CONUS = sum(OG)/sum(Forested_Plot)*100)

#=====================================================================#
#
#=====================================================================#

Regions <- open_dataset("Files/OG_Regions/Region6.2.parquet") %>% 
  select(Old_Growth, contains("class"), cuid, puid) %>% 
  collect()
table(Regions$Old_Growth)
summary(Regions)

Regions %>%
  select(contains("class"), Old_Growth) %>%
  mutate(Old_Growth = fifelse(Old_Growth == "Old Growth", TRUE, FALSE, na=FALSE)) %>%
  mutate_at(vars(contains("class"), Old_Growth), ~replace_na(as.numeric(.), 0)) %>%
  cor() %>% 
  round(2)
