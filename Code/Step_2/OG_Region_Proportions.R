library(arrow)
library(tidyverse)
library(terra)
library(sf)
library(data.table)
library(future)

setwd("~/Old_Growth")

#================================================================#
# Bring in USFS old growth and my old growth
#================================================================#

USFS_OG <- read_csv("Files/Step_2/MOG_Master_Plot_List_v9.csv") %>%
  mutate(MOG = fifelse(MOG == 1, 1, 0, na=0))

# Non of the missing cuid data occurs in any of the PLOT information 
National_Forest <- USFS_OG %>%
  left_join(open_dataset("Files/Step_2/All_Regions_EMAP_HEX.parquet") %>% 
              collect() %>%
              as.data.table() %>%
              mutate(CONDID = as.numeric(str_extract(cuid, "(?<=_)[^_]+$")),
                     Old_Growth = fifelse(Old_Growth == "Old Growth", 1, 0, na=0)), 
            by = c("PLT_CN", "CONDID")) %>%
  filter(!is.na(cuid))

#================================================================#
# aggregate the plot data to get proportions
#================================================================#

Region_Plot <- National_Forest %>%
  mutate(MOG_prop = MOG * CONDPROP_UNADJ,
         OG_prop = Old_Growth * CONDPROP_UNADJ,
         forested_prop = forested * CONDPROP_UNADJ, # Forested is just cond != 999
         forested_prop = replace(forested_prop, COND_STATUS_CD %in% c(3,4,5),NA)) %>% 
  group_by(puid) %>% 
  summarise(REGION = unique(REGION),
            Fireshed_Code = unique(Fireshed_Code),
            EMAP_HEX = unique(EMAP_HEX),
            MEASYEAR = unique(MEASYEAR),
            STATEAB = unique(STATEAB),
            plot_MOG_prop = sum(MOG_prop, na.rm=T),
            plot_OG_prop = sum(OG_prop, na.rm=T),
            plot_prop_forest = sum(forested_prop, na.rm=T),
            n_cond_mog = sum(MOG == 1, na.rm=T),
            n_cond_og = sum(Old_Growth == 1, na.rm=T),
            n_cond_forested = n()) %>% 
  left_join(fread('Files/CONUS_hex_epa2_lut.csv'), by='EMAP_HEX')

#================================================================#
# Combine plots by hex to get ratios
#================================================================#

source("Code/Map_Color.R")
hex <- st_read('Files/gpkg/conus_hex_epsg6933.gpkg') 

resHex <- function(data, grid_data, grid_name){
  
  est <- data %>% 
    group_by({{grid_name}}) %>%
    summarise(REGION = unique(REGION),
              n = sum(n_cond_forested > 0),
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
              v_r_hat_MOG = (1/x_hat^2) * (v_y_hat_MOG + (r_hat_MOG^2)*v_x_hat - 2*r_hat_MOG*cov_xy_hat_MOG)) %>%
    select({{grid_name}},region,n,contains('r_'),x_hat, v_x_hat, REGION)
  
  out <- grid_data %>%
    left_join(est) %>% 
    rename(OG_RATIO = r_hat_OG,
           OG_RATIO_VAR = v_r_hat_OG,
           MOG_RATIO = r_hat_MOG,
           MOG_RATIO_VAR = v_r_hat_MOG,
           class_f_RATIO = x_hat,
           class_f_RATIO_VAR = v_x_hat) %>%
    mutate(dif = coalesce(OG_RATIO,0) - coalesce(MOG_RATIO,0)) %>%
    st_transform(4326)
  
  return(out)
  
}

OG_Forest <- resHex(Region_Plot, hex, grid_name = EMAP_HEX) %>% drop_na()

dcr <- div_color_ramp(OG_Forest$OG_RATIO, increments = T,include_all = F, Extreme = F, middle_range = .99)
plot(OG_Forest['class_f_RATIO'],pal=dcr$cols, breaks=dcr$breaks, border = NA)
plot(OG_Forest['OG_RATIO'],pal=dcr$cols, breaks=dcr$breaks, border = NA)
plot(OG_Forest['MOG_RATIO'],pal=dcr$cols, breaks=dcr$breaks, border = NA)

#================================================================#
# Old Growth by region
#================================================================#

OG_Forest %>%
  mutate(REGION = round(as.numeric(REGION))) %>% 
  group_by(REGION) %>%
  mutate(Forested_Area = area_ha * class_f_RATIO,
         MOG_Area = MOG_RATIO * area_ha,
         OG_Area = OG_RATIO * area_ha) %>%
  summarise(MOG_Area = sum(MOG_Area),
            OG_Area = sum(OG_Area),
            Forested_Area = sum(Forested_Area),
            MOG_Percent = round((MOG_Area / Forested_Area)*100,3),
            OG_Percent = round((OG_Area / Forested_Area)*100,3)) %>%
  as.data.frame() %>%
  select(-geom)



