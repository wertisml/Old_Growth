library(arrow)
library(tidyverse)
library(terra)
library(sf)

setwd("~/Old_Growth")

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
         CONUS = sum(OG)/sum(Forested_Plot)*100,
         Total = sum(OG))

#==============================================================================#
# Percent Old Growth by Forest Type Group
#==============================================================================#

source("Code/Data_Prep/Species_Grouping.R")

Forest_Group_Breakdown <- open_dataset("Files/Step_2/All_Regions_EMAP_HEX.parquet") %>% 
  select(FORTYPCD, Old_Growth, EMAP_HEX) %>%
  collect() %>%
  left_join(fread('Files/CONUS_hex_epa2_lut.csv'), by='EMAP_HEX') %>%
  cbind(Forest_Type(., FORTYPCD)) %>%
  group_by(Group) %>% #REGION # add in region to get the split by group and region
  mutate(ratio_old_growth = sum(Old_Growth == "Old Growth", na.rm=T) / n(),
         Count = n()) %>%
  summarise(Percent_Old_Growth = round(mean(ratio_old_growth, na.rm = TRUE)*100, 2),
            Count = mean(Count))

Old_Growth_FORTYPCD_Region <- open_dataset("Files/Step_2/All_Regions_EMAP_HEX.parquet") %>% 
  select(FORTYPCD, Old_Growth, REGION) %>%
  collect() %>%
  group_by(FORTYPCD, REGION) %>%
  mutate(ratio_old_growth = sum(Old_Growth == "Old Growth", na.rm=T) / n(),
         Count = n()) %>%
  summarise(Percent_Old_Growth = round(mean(ratio_old_growth, na.rm = TRUE)*100, 2),
            Count = mean(Count))


#=====================================================================#
# Correlation Matrix
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
