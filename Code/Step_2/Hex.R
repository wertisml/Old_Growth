library(arrow)
library(tidyverse)
library(terra)
library(sf)
library(data.table)
library(future)

setwd("~/Old_Growth")

#hex <- st_read('Files/gpkg/conus_hex_epsg6933.gpkg') 
hex <- st_read('Files/gpkg/ef_hex.gpkg')

Regions <- open_dataset("Files/OG_Regions/") %>% 
  #filter(REGION %in% c("08", "09")) %>%
  mutate(puid = str_remove(cuid, "_\\d+$")) %>%
  select(cuid, puid, Old_Growth, STATEAB, REGION) %>% 
  collect()

location <- open_dataset('Files/Cond_and_Plot.parquet') %>% 
  select(cuid, LAT, LON, STATEAB, MEASYEAR, CONDPROP_UNADJ, COND_STATUS_CD,
         FORTYPCD, EMAP_HEX, Fireshed_Code) %>% 
  filter(STATEAB %in% Regions$STATEAB) %>%
  mutate(puid = str_remove(cuid, "_\\d+$")) %>%
  collect() %>%
  distinct(cuid, .keep_all = TRUE) %>%
  mutate(forested = fifelse(FORTYPCD == 999, FALSE, TRUE, na=FALSE))

Regions <- Regions %>%
  right_join(location, by = join_by(cuid, puid, STATEAB))

write_parquet(Regions, "Files/Step_2/All_Regions_EMAP_HEX.parquet")

#==============================================================================#
#
#==============================================================================#

plan(multisession, workers = 12)

Region_Plot <- open_dataset("Files/Step_2/All_Regions_EMAP_HEX.parquet") %>% 
  collect() %>%
  mutate(Old_Growth = as.numeric(Old_Growth == 'Old Growth'),
         OG_prop = Old_Growth * CONDPROP_UNADJ,
         forested_prop = forested * CONDPROP_UNADJ, # Forested is just cond != 999
         forested_prop = replace(forested_prop, COND_STATUS_CD %in% c(3,4,5),NA)) %>% 
  group_by(puid) %>% 
  # calculate proportion of each OG flavor and forest on each plot
  summarise(Fireshed_Code = unique(Fireshed_Code),
            EMAP_HEX = unique(EMAP_HEX),
            MEASYEAR = unique(MEASYEAR),
            STATEAB = unique(STATEAB),
            plot_OG_prop = sum(OG_prop, na.rm=T),
            plot_prop_forest = sum(forested_prop, na.rm=T),
            n_cond_og = sum(Old_Growth == 1, na.rm=T),
            n_cond_forested = n()) %>% 
  left_join(fread('Files/CONUS_hex_epa2_lut.csv'), by='EMAP_HEX')

#==============================================================================#
#
#==============================================================================#

source("Code/Map_Color.R")
hex <- st_read('Files/gpkg/conus_hex_epsg6933.gpkg') 

#Fire_Shed <- st_read("Files/gpkg/usfs_firesheds_epsg4326.gpkg") %>%
#  select(Fireshed_Code, OBJECTID)

resHex <- function(data, grid_data, grid_name){
  
  est <- data %>% 
    group_by({{grid_name}}) %>%
    summarise(n = sum(n_cond_forested > 0),
              region = unique({{grid_name}}),
              
              x_hat = mean(plot_prop_forest, na.rm=T),
              v_x_hat = var(plot_prop_forest) / n,
              
              y_hat_OG = mean(plot_OG_prop, na.rm=T),
              v_y_hat_OG = var(plot_OG_prop) / n,
              cov_xy_hat_OG = cov(plot_prop_forest,plot_OG_prop) / n,
              r_hat_OG = y_hat_OG / x_hat,
              v_r_hat_OG = (1/x_hat^2) * (v_y_hat_OG + (r_hat_OG^2)*v_x_hat - 2*r_hat_OG*cov_xy_hat_OG)) %>%
    select({{grid_name}},region,n,contains('r_'),x_hat, v_x_hat)

  out <- grid_data %>%
    left_join(est) %>% 
    rename(OG_RATIO = r_hat_OG,
           OG_RATIO_VAR = v_r_hat_OG,
           class_f_RATIO = x_hat,
           class_f_RATIO_VAR = v_x_hat) %>%
    st_transform(4326)
  
  return(out)

}

OG_Forest <- resHex(Region_Plot, hex, grid_name = EMAP_HEX)

dcr <- div_color_ramp(OG_Forest$OG_RATIO, increments = T,include_all = F, Extreme = F, middle_range = .99)
plot(OG_Forest['class_f_RATIO'],pal=dcr$cols, breaks=dcr$breaks, border = NA)
plot(OG_Forest['OG_RATIO'],pal=dcr$cols, breaks=dcr$breaks, border = NA)

plan(sequential)

st_write(OG_Forest, 'Files/gpkg/R8_R9_hex_Old_Growth.gpkg', append=F)

#==============================================================================#
# making mean and standard error estimates for different groupings (total EF and by ecoregion)
#==============================================================================#

est_og_flavors <- function(data, ...){
  
  est <- data %>% 
    group_by(!!! ensyms(...)) %>% 
    summarise(
      n = sum(n_cond_forested > 0),
      
      x_hat = mean(plot_prop_forest, na.rm=T),
      v_x_hat = var(plot_prop_forest) / n,
      
      y_hat_OG = mean(plot_OG_prop, na.rm=T),
      v_y_hat_OG = var(plot_OG_prop) / n,
      cov_xy_hat_OG = cov(plot_prop_forest,plot_OG_prop) / n,
      r_hat_OG = y_hat_OG / x_hat,
      v_r_hat_OG = (1/x_hat^2) * (v_y_hat_OG + (r_hat_OG^2)*v_x_hat - 2*r_hat_OG*cov_xy_hat_OG),
      
    ) %>% 
    select(n,contains('r_'),x_hat, v_x_hat,...) %>% 
    rename(
      OG_RATIO = r_hat_OG,
      OG_RATIO_VAR = v_r_hat_OG,
      
      class_f_RATIO = x_hat,
      class_f_RATIO_VAR = v_x_hat,
    ) %>% 
    mutate(
      OG_RCI = sqrt(OG_RATIO_VAR) / sqrt(n) * qnorm(0.975),
    ) %>% 
    select(!contains(c('VAR','_f_'))) %>% 
    relocate(...) %>%
    mutate(across(c(starts_with("class")), ~ .x * 100)) #%>% 
    #as.data.frame()

  return(est)
}

# All plots
all <- est_og_flavors(Region_Plot) %>% mutate(EPA2_code='all')

# By EPA region
by_region <- est_og_flavors(Region_Plot, 'EPA2_code') %>% mutate(EPA2_code = as.character(EPA2_code))

# Combine both groups
og_est <- bind_rows(all,by_region) %>%
  arrange(desc(OG_RATIO)) %>%
  relocate(EPA2_code) %>% 
  select(-n)

sg <- apply(og_est[,-1], 2, formatC, format='fg', digits=3)

out <- bind_cols(og_est[1],sg)

write.csv(out, 'Files/Step_2/ogp_estimation.csv', row.names = F, quote=F)

#==============================================================================#
# Percent Old Growth by Forest Type Group
#==============================================================================#
source("Code/Data_Prep/Species_Grouping.R")

Region_Plot <- open_dataset("Files/Step_2/All_Regions_EMAP_HEX.parquet") %>% 
  select(FORTYPCD, Old_Growth, EMAP_HEX) %>%
  collect() %>%
  left_join(fread('Files/CONUS_hex_epa2_lut.csv'), by='EMAP_HEX') %>%
  cbind(Forest_Type(., FORTYPCD)) %>%
  group_by(Group) %>%
  mutate(ratio_old_growth = sum(Old_Growth == "Old Growth", na.rm=T) / n(),
         Count = n()) %>%
  summarise(Percent_Old_Growth = round(mean(ratio_old_growth, na.rm = TRUE)*100, 2),
            Count = mean(Count))
  





est_og_flavors(Region_Plot %>% 
                 mutate(EPA2_NS = case_when(
                   EPA2_code %in% c(5.23,8.12,9.2) ~ 'N',
                   EPA2_code %in% c(8.3,8.4,8.5) ~ 'S',
                 )), 'EPA2_NS') %>% 
  select(-n)

# Not sure what exactly this is suppose to do

cor_flavors <- function(est,...){
  cc <- est %>% 
    # filter(
    #   # fia_pf > 0.25,
    #   !is.na(region)
    # ) %>% 
    #st_drop_geometry() %>% 
    group_by(!!! ensyms(...)) %>%
    summarise(
      cor_OG = cor(OG_RATIO, use='complete.obs')
    ) 
  return(cc)
}

by_region <- cor_flavors(out, 'EPA2_code') %>% mutate(EPA2_code=as.character(EPA2_code))
all <- cor_flavors(out) %>% mutate(region='all')









