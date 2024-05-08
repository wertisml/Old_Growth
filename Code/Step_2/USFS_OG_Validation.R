library(arrow)
library(tidyverse)
library(future)
library(terra)
library(data.table)

setwd("~/Old_Growth")

plan(multisession, workers = 12)

FS_OG_Regions <- vect("ARC_Vis/OG/FS_Regions_OG.shp")

USFS_OG <- read_csv("Files/Step_2/MOG_Master_Plot_List_v9.csv") %>%
  mutate(MOG = fifelse(MOG == 1, 1, 0, na=0))

Region_Plot <- open_dataset("Files/OG_Regions/") %>% 
  dplyr::select(Old_Growth, cuid, PLT_CN, REGION, puid, EMAP_HEX) %>%
  collect() %>%
  as.data.table() %>%
  mutate(CONDID = as.numeric(str_extract(cuid, "(?<=_)[^_]+$")),
         Old_Growth = fifelse(Old_Growth == "Old Growth", 1, 0, na=0))

# Non of the missing cuid data occurs in any of the PLOT information 
National_Forest <- USFS_OG %>%
  left_join(Region_Plot, 
            by = c("PLT_CN", "CONDID")) %>%
  filter(!is.na(cuid))

#====================================================================#
# Filter data by region
#====================================================================#

#table(National_Forest$Old_Growth, National_Forest$MOG)

Filter <- National_Forest %>%
  filter(REGION == "09",
         Old_Growth != MOG) %>%
  select(cuid, puid, MOG, Old_Growth)

table(Filter$Old_Growth, Filter$MOG)

No_OG <- Filter %>%
  filter(MOG == 1)

No_MOG <- Filter %>%
  filter(Old_Growth == 1)





table <- National_Forest %>%
  select(MOG, Old_Growth, FORTYPCD, REGION) %>%
  group_by(FORTYPCD, REGION) %>%
  summarise(MOG = sum(MOG),
            Old_Growth = sum(Old_Growth)) %>%
  arrange(REGION)

write_csv(table, "Files/Step_2/OG_v_MOG.csv")

#====================================================================#
# Plotting the Old Growth data
#====================================================================#

data_sf <- vect(National_Forest, geom = c("LON", "LAT"), crs = crs(FS_OG_Regions))

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
plot(OG_Spots['MOG'], col = OG_Spots$Color, add=T, pch = 20)

#====================================================================#
#
#====================================================================#

Data <- National_Forest %>%
  mutate(Old_Growth = as.numeric(Old_Growth == 'Old Growth'),
         MOG_prop = MOG * CONDPROP_UNADJ,
         OG_prop = Old_Growth * CONDPROP_UNADJ,
         forested_prop = forested * CONDPROP_UNADJ, # Forested is just cond != 999
         forested_prop = replace(forested_prop, COND_STATUS_CD %in% c(3,4,5),NA)) %>% 
  group_by(puid) %>% 
  # calculate proportion of each OG flavor and forest on each plot
  summarise(Fireshed_Code = unique(Fireshed_Code),
            EMAP_HEX = unique(EMAP_HEX),
            MEASYEAR = unique(MEASYEAR),
            STATEAB = unique(STATEAB),
            plot_MOG_prop = sum(MOG_prop, na.rm=T),
            plot_OG_prop = sum(OG_prop, na.rm=T),
            plot_prop_forest = sum(forested_prop, na.rm=T),
            n_cond_og = sum(Old_Growth == 1, na.rm=T),
            n_cond_forested = n()) %>% 
  left_join(fread('Files/CONUS_hex_epa2_lut.csv'), by='EMAP_HEX')

hex <- st_read('Files/gpkg/conus_hex_epsg6933.gpkg') 

source("Code/Map_Color.R")

resHex <- function(data, grid_data, grid_name){
  
  est <- data %>% 
    group_by(EMAP_HEX) %>%
    summarise(n = sum(n_cond_forested > 0),
              region = unique({{grid_name}}),
              
              x_hat = mean(plot_prop_forest, na.rm=T),
              v_x_hat = var(plot_prop_forest) / n,
              
              y_hat_OG = mean(plot_OG_prop, na.rm=T),
              v_y_hat_OG = var(plot_OG_prop) / n,
              cov_xy_hat_OG = cov(plot_prop_forest,plot_OG_prop) / n,
              r_hat_OG = y_hat_OG / x_hat,
              v_r_hat_OG = (1/x_hat^2) * (v_y_hat_OG + (r_hat_OG^2)*v_x_hat - 2*r_hat_OG*cov_xy_hat_OG),
              
              y_hat_MOG = mean(plot_MOG_prop, na.rm=T),
              v_y_hat_MOG = var(plot_MOG_prop) / n,
              cov_xy_hat_MOG = cov(plot_prop_forest,plot_MOG_prop) / n,
              r_hat_MOG = y_hat_MOG / x_hat,
              v_r_hat_MOG = (1/x_hat^2) * (v_y_hat_MOG + (r_hat_MOG^2)*v_x_hat - 2*r_hat_MOG*cov_xy_hat_MOG)
              ) %>%
    select(EMAP_HEX,region,n,contains('r_'),x_hat, v_x_hat)
  
  out <- grid_data %>%
    left_join(est) %>% 
    rename(OG_RATIO = r_hat_OG,
           MOG_RATIO = r_hat_MOG,
           OG_RATIO_VAR = v_r_hat_OG,
           class_f_RATIO = x_hat,
           class_f_RATIO_VAR = v_x_hat) %>%
    st_transform(4326)
  
  return(out)
  
}

OG_Forest <- resHex(Data, hex, grid_name = EMAP_HEX)

var1 <- OG_Forest$OG_RATIO
var2 <- OG_Forest$MOG_RATIO

dif <- var1-var2

OG_Forest$dif <- dif

dcr <- div_color_ramp(OG_Forest$dif, include_all =F, Extreme = T, hot_col = 'mediumseagreen', cold_col = 'violet', middle_range = .99)
plot(OG_Forest['dif'],pal=dcr$cols, breaks=dcr$breaks, border = NA)

#====================================================================#
# Segment by shapefile
#====================================================================#

file <- list.files(path = "Files/gpkg/Neil/",
           pattern = ".gpkg")

USFS_OG <- read_csv("Files/Step_2/MOG_Master_Plot_List_v9.csv") %>%
  mutate(MOG = ifelse(MOG == 1, MOG, NA)) %>% 
  drop_na()

Plots <- open_dataset("Files/PLot_and_Cond_Regions.parquet") %>%
  dplyr::select(LON, LAT, cuid, PLT_CN, puid) %>%
  mutate(Lon = LON,
         Lat = LAT) %>%
  collect() %>%
  as.data.table() %>%
  mutate(CONDID = as.numeric(str_extract(cuid, "(?<=_)[^_]+$")))

# Non of the missing cuid data occurs in any of the PLOT information 
National_Forest <- USFS_OG %>%
  left_join(Plots, 
            by = c("PLT_CN", "CONDID")) %>%
  filter(!is.na(cuid))

for(i in 1:length(file)){
  
Boundary <- vect(paste0("Files/gpkg/Neil/", file[i]))

National_Forest_sf <- vect(National_Forest, geom = c("LON", "LAT"), crs = crs(Boundary))

sf_crop <- terra::crop(National_Forest_sf, Boundary)

writeVector(sf_crop, paste0("Files/gpkg/Neil/Output/",file[i]), overwrite = T)
}
