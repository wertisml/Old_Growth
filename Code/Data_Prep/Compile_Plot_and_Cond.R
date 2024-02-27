library(future)
library(tidyverse)
library(arrow)
library(rFIA)
library(data.table)
library(sf)

setwd("~/MOG")

states_list <- read_csv('Files/state_table.csv')
states <- unique(states_list$STATEAB[states_list$STATEAB %in% states_list$STATEAB])

hex <- st_read('Files/gpkg/conus_hex_epsg6933.gpkg') %>%
   dplyr::select(-c(area_ha))

Fire_Shed <- vect("Files/gpkg/usfs_firesheds_epsg4326.gpkg")
Fire_Shed <- Fire_Shed[,c("Fireshed_Code")]
sf_use_s2(FALSE)

options(timeout=3600)

Plots <- c()

plan(multisession, workers = 12)

for(i in 1:length(states)){

  state <- states[i]

  FIA <- readFIA(paste0('Files/rFIA/',state), nCores=12, inMemory=T, states=state)
  fia <- clipFIA(FIA, mostRecent = T)
  
  tpa_live <- tpa(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), treeType = 'live', nCores=12) %>%
    mutate(TPA_LIVE = TPA,
           BAPA_LIVE = BAA,
           cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_"),
           puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_")) %>%
   select(TPA_LIVE, BAPA_LIVE, cuid, puid) %>%
    distinct(cuid, .keep_all = T) %>% 
    ungroup()

  tpa_dead <- tpa(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), treeType = 'dead', nCores=12) %>%
    mutate(TPA_DEAD = TPA,
           BAPA_DEAD = BAA,
           cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_"),
           puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_")) %>%
    select(TPA_DEAD, BAPA_DEAD, cuid, puid) %>%
    distinct(cuid, .keep_all = T) %>% 
    ungroup()

  TPA <- purrr::reduce(list(tpa_live, tpa_dead), left_join,by=c('puid','cuid')) 

  Plot <- read_csv(paste0('Files/rFIA/',state,"/", state, "_PLOT.csv"),
                  col_select = c("CN", "INVYR", "MEASYEAR", "STATECD", "UNITCD",
                                 "COUNTYCD", "PLOT", "ECOSUBCD", "LAT", "LON")) %>%
    mutate(puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"))%>% 
    rename(PLT_CN=CN) %>% 
    group_by(puid) %>% 
    filter(MEASYEAR == max(MEASYEAR)) %>% 
    ungroup() %>% 
    filter(MEASYEAR >= 2010)
  
  pp <- Plot %>% 
    filter(!is.na(LAT) & !is.na(LON)) %>% 
    st_as_sf(., coords=c('LON','LAT'), remove=F, crs=st_crs(4326)) %>% 
    st_transform(6933) %>% 
    st_intersection(x=., y=hex) %>% 
    st_drop_geometry() %>% 
    dplyr::select(puid,MEASYEAR,EMAP_HEX)
  
  FS <- Plot %>%
    filter(!is.na(LAT) & !is.na(LON)) %>% 
    vect(., geom=c('LON','LAT'), crs=crs(Fire_Shed)) %>%
    st_intersection(x=st_as_sf(.), y=st_as_sf(Fire_Shed)) %>% 
    st_drop_geometry() %>% 
    dplyr::select(puid,MEASYEAR,Fireshed_Code)

  Plot <- Plot %>% 
    left_join(y=pp, by=c('puid','MEASYEAR')) %>%
    left_join(y=FS, by=c('puid','MEASYEAR'))
  
  Cond <- read_csv(paste0('Files/rFIA/',state,"/", state, "_COND.csv"),
                   col_select = c("STATECD", "UNITCD", "COUNTYCD", "PLOT",
                                  "CONDID", "INVYR", "STDAGE", "FORTYPCD",
                                  "PLT_CN", "CN", "PHYSCLCD", "SITECLCD",
                                  "ADFORCD", "HABTYPCD1", "ADFORCD",
                                  "HABTYPCD1_PUB_CD", "CONDPROP_UNADJ",
                                  "COND_STATUS_CD", "BALIVE")) %>%
    mutate(cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_"),
          puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
          STATEAB = state) %>%
    rename(COND_CN=CN)%>% 
    filter(PLT_CN %in% Plot$PLT_CN) %>% 
    relocate(puid,cuid)

  out <- Cond %>% 
    left_join(Plot %>%
                select(puid, MEASYEAR, ECOSUBCD, LAT, LON, EMAP_HEX, Fireshed_Code), by='puid') %>%
    left_join(TPA %>%
                select(cuid, TPA_LIVE, BAPA_LIVE, TPA_DEAD, BAPA_DEAD), by = 'cuid') %>%
    select(-STATECD, -UNITCD, -COUNTYCD, -PLOT, -CONDID, -COND_CN) %>%
    group_by(puid) %>% 
    filter(MEASYEAR == max(MEASYEAR))

  Plots[[i]] <- out
  
  log <- paste0(Sys.time(),' ',state,' (',i,'/',length(states),') n=', nrow(out))
  print(log)
}

plan(sequential)

compiled_data <- rbindlist(Plots, use.names = TRUE, fill = TRUE)

compiled_data <- merge(compiled_data, states_list, by = "STATEAB", all.x = TRUE)

write_parquet(compiled_data, "Cond_and_Plot.parquet")
