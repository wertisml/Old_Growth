##################################################################################################
#
# JMB 
# November 2021
# Calculates FIA plot variables by plot within each state
# 
###################################################################################################

rm(list=ls())

setwd("~/MOG")

library(tidyverse)
library(sf)
library(rFIA)
library(data.table)
library(bit64)
library(rFIA)
library(tidyr)
library(nngeo)

options(digits=5)

source('Code/plotting_tools.R')

states <- state.abb[state.abb != c('HI', 'AK')]
state_table <- read.csv('Files/state_table.csv')

hex <- st_read('Files/gpkg/conus_hex_epsg6933.gpkg') %>% 
  dplyr::select(-c(area_ha))

###################################################################################################

# getFIA(states=states,dir='/Volumes/RESEARCH/rFIA/original_data_0207',load=F, nCores=8)

###################################################################################################

clean <- function(dat) {
  dat %>% 
    mutate(
      puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_")
    ) %>% 
    dplyr::select(-c(STATECD, UNITCD, COUNTYCD, PLOT, YEAR, CONDID,N,contains(c('TPA','BAA','SE','nPlots','ACRE')))) %>% 
    relocate(puid, cuid) %>% 
    # group_by(cuid, MEASYEAR) %>% 
    # remove duplicates, weird duplicates from rFIA::biomass()
    distinct(cuid, .keep_all = T) %>% 
    # arrange(cuid) %>% 
    # filter(row_number()==1) %>% 
    ungroup()
}

main_tables <- function(fia, nCores){
  
  bio_live <- biomass(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), bioMethod='JENKINS', nCores=nCores) %>%
    mutate(AGBD_LIVE = round(BIO_ACRE * 2.471 * 0.907,3)) %>%
    clean()
  
  bio_dead <- biomass(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), treeType = 'dead', bioMethod='JENKINS', nCores=nCores) %>%
    mutate(AGBD_DEAD = round(BIO_ACRE * 2.471 * 0.907,3)) %>%
    clean()
  
  tpa_live <- tpa(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), nCores=nCores) %>%
    mutate(TPH_LIVE = round(TPA * (2.471/10.76),3),
           BAPH_LIVE = round(BAA * (2.471/10.76),3)) %>%
    clean()
  
  tpa_dead <- tpa(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), treeType = 'dead', nCores=nCores) %>%
    mutate(TPH_DEAD = round(TPA * (2.471/10.76),3),
           BAPH_DEAD = round(BAA * (2.471/10.76),3)) %>%
    clean()
  
  sdl <- seedling(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), nCores=nCores) %>%
    mutate(SPH = TPA * 2.471) %>%
    clean()
  
  sst <- standStruct(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), nCores=nCores) %>%
    clean()
  
  down_woody_mat <- dwm(fia, grpBy=c(STATECD, UNITCD, COUNTYCD, PLOT, CONDID), nCores=nCores, byFuelType=F) %>%
    mutate(DWM = round(BIO_ACRE * 2.471 * 0.907,3),
           puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
           cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_")) %>%
    dplyr::select(-c(STATECD, UNITCD, COUNTYCD, PLOT, YEAR, CONDID,N,contains(c('TPA','BAA','SE','nPlots','ACRE')))) %>% 
    relocate(puid, cuid) 
  
  out <- purrr::reduce(list(bio_live,bio_dead,tpa_live,tpa_dead,sdl,sst,down_woody_mat), left_join,by=c('puid','cuid')) 
  
  return(out)
}

tree_geom <- function(fia, plot){
  # 4) calculate some final variables by hand from tree tables
  tree <- fia$TREE %>%
    mutate(
      puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_")
    ) %>% 
    filter(PLT_CN %in% plot$PLT_CN) %>%
    group_by(cuid) %>%
    summarise(puid = unique(puid),
              dbh_mean = mean(DIA, na.rm=T) * 0.0254,
              dbh_sd = sd(DIA, na.rm=T) * 0.0254,
              dbh_q50 = quantile(DIA, probs=0.5, na.rm=T) * 0.0254,
              # dbh_q75 = quantile(DIA, probs=0.75, na.rm=T) * 0.0254,
              # dbh_q90 = quantile(DIA, probs=0.9, na.rm=T) * 0.0254,
              # dbh_q95 = quantile(DIA, probs=0.95, na.rm=T) * 0.0254,
              # dbh_q99 = quantile(DIA, probs=0.99, na.rm=T) * 0.0254,
              dbh_max = max(DIA, na.rm=T) * 0.0254,
              # dbh_Ngt_050cm = sum(DIA * 0.0254 > .5, na.rm=T),
              # dbh_Ngt_075cm = sum(DIA * 0.0254 > .75, na.rm=T),
              # dbh_Ngt_100cm = sum(DIA * 0.0254 > 1, na.rm=T),
              # dbh_Ngt_125cm = sum(DIA * 0.0254 > 1.25, na.rm=T),
              # dbh_Ngt_150cm = sum(DIA * 0.0254 > 1.5, na.rm=T),
              ht_max = max(ACTUALHT, na.rm=T) * 0.3048,
              ht_mean = mean(ACTUALHT, na.rm=T) * 0.3048,
              ht_med = median(ACTUALHT, na.rm=T) * 0.3048,
              ht_sd = sd(ACTUALHT, na.rm=T) * 0.3048,
              n_trees = sum(STATUSCD == 1),
              # .groups='keep'
    ) %>%
    mutate(across(.cols=where(is.numeric),round,3))
  # ungroup() 
  return(tree)
}

crown_class <- function(fia, plot){
  cols <- c(perc_open=NA_real_,perc_dom=NA_real_,perc_codom=NA_real_,perc_inter=NA_real_,perc_over=NA_real_,perc_NA=NA_real_)
  crcl <- fia$TREE %>%
    mutate(
      puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_")
    ) %>% 
    filter(PLT_CN %in% plot$PLT_CN) %>% 
    group_by(cuid,INVYR,CCLCD) %>%
    summarise(n = n(), .groups='keep') %>%
    mutate(freq = n / sum(n) * 100,
           CCLCD = case_when(
             CCLCD == 1 ~ names(cols)[1],
             CCLCD == 2 ~ names(cols)[2],
             CCLCD == 3 ~ names(cols)[3],
             CCLCD == 4 ~ names(cols)[4],
             CCLCD == 5 ~ names(cols)[5],
             is.na(CCLCD) ~ names(cols)[6])) %>%
    pivot_wider(id_cols = cuid, names_from = CCLCD, values_from = freq) %>%
    add_column(!!!cols[setdiff(names(cols), names(.))]) %>%
    relocate(cuid,perc_dom,perc_codom,perc_inter,perc_over,perc_open,perc_NA) %>%
    ungroup() 
  return(crcl)
}


# i <- 'RI'
# i <- 'NY'


# states <- state_table$STATEAB[state_table$REGION %in% c(8,9)]
# states <- c("ND",'SD','NE','KS')

ef_states <- st_read('Files/gpkg/ef_states.gpkg') %>% pull(state_abbr)
states <- ef_states[!ef_states %in% 'DC']

#Mine
Region_8 <- state_table %>% filter(REGION == 8)
states <- states[states %in% Region_8$STATEAB]

for(i in states){
  
  # 1) get rFIA data
  state <- i
  FIA <- readFIA(paste0('Files/rFIA/Region_8/',state), nCores=12, inMemory=T, states=state)
  fia <- clipFIA(FIA, mostRecent = T)
  
  # make master list of plots, use PLT_CN to filter other tables
  plot <- fia$PLOT %>% 
    dplyr::select(CN, PREV_PLT_CN, INVYR, MEASYEAR, STATECD, UNITCD, COUNTYCD, PLOT, PLOT_STATUS_CD, LAT, LON, INTENSITY) %>% 
    mutate(
      puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_")
    ) %>% 
    rename(PLT_CN=CN) %>% 
    group_by(puid) %>% 
    filter(MEASYEAR == max(MEASYEAR)) %>% 
    ungroup() %>% 
    filter(MEASYEAR >= 2010)
  
  # intersect hex to get EMAP_HEX val
  pp <- plot %>% 
    filter(!is.na(LAT) & !is.na(LON)) %>% 
    st_as_sf(., coords=c('LON','LAT'), remove=F, crs=st_crs(4326)) %>% 
    st_transform(6933) %>% 
    st_intersection(x=., y=hex) %>% 
    st_drop_geometry() %>% 
    dplyr::select(puid,MEASYEAR,EMAP_HEX)
  
  plot <- plot %>% 
    left_join(y=pp, by=c('puid','MEASYEAR'))
  
  # get master list of conditions for those plots - filtered using PLT_CN to get correct puid/MEASYEAR combo
  cond <- fia$COND %>% 
    dplyr::select(CN, PLT_CN, STATECD, UNITCD, COUNTYCD, PLOT, INVYR, CONDID, COND_STATUS_CD, RESERVCD, OWNCD, FORTYPCD, STDSZCD, STDAGE, FLDSZCD, STDORGCD, MAPDEN, PHYSCLCD, SITECLCD, CONDPROP_UNADJ) %>% 
    mutate(
      puid = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_")
    ) %>% 
    rename(COND_CN=CN) %>% 
    # dplyr::select(-c(STATECD, MEASYEAR, UNITCD, COUNTYCD, PLOT)) %>% 
    filter(PLT_CN %in% plot$PLT_CN) %>% 
    relocate(puid,cuid)
  
  # get tree geom and crown class - both filtered using PLT_CN to get correct puid/MEASYEAR combo
  tg <- tree_geom(fia, plot=plot)
  cc <- crown_class(fia, plot=plot)
  
  # get main tables from rFIA functions
  dat <- main_tables(fia, nCores=12)
  
  
  out <- cond %>% 
    left_join(plot[,c('puid','MEASYEAR')], by='puid') %>% 
    left_join(tg, by=c('puid','cuid')) %>% 
    left_join(cc, by='cuid') %>% 
    left_join(dat, by=c('puid','cuid')) %>% 
    relocate(puid,cuid,PLT_CN,COND_CN,COND_STATUS_CD,CONDID,INVYR,MEASYEAR,STATECD,UNITCD,COUNTYCD,PLOT)
  
  # out %>% filter(is.na(AGBD_LIVE) & COND_STATUS_CD == 1)
  # 1_3_113_22_1
  
  
  
  plot_name <- paste0('./data/fia/fiadb/state_tables_0417/FIA_PLOT_',state,'_mostrecent.csv')
  fwrite(plot,plot_name)
  
  cond_name <- paste0('./data/fia/fiadb/state_tables_0417/FIA_COND_',state,'_mostrecent.csv')
  fwrite(out,cond_name)
  
  print(state)
  
}


##################

files <- list.files('./data/fia/fiadb/state_tables_0417', full.names = T)

pp <- rbindlist(lapply(files[grepl('PLOT',files,)], function(x){fread(x)})) %>% as.data.frame()
cc <- rbindlist(lapply(files[grepl('COND',files,)], function(x){fread(x)})) %>% as.data.frame()



missing <- pp %>% 
  filter(is.na(EMAP_HEX)) %>% 
  st_as_sf(coords=c('LON','LAT'), crs=st_crs(4326), remove=F)

hex4326 <- hex %>% st_transform(4326)

nn <- st_nn(x=missing, y=hex4326, parallel = 8)
missing_hexes <- hex4326$EMAP_HEX[unlist(nn)]
missing$EMAP_HEX <- missing_hexes

ppp <- pp %>% 
  filter(!is.na(EMAP_HEX)) %>% 
  bind_rows(missing) %>% 
  dplyr::select(-c(geometry)) %>% 
  left_join(state_table) %>% 
  relocate(STATEAB, REGION, .after=MEASYEAR) %>% 
  relocate(puid)
  


## clean up and organize plot and cond tables
ccc <- cc %>% 
  mutate(
    forested = case_when(
      COND_STATUS_CD == 1 ~ TRUE,
      COND_STATUS_CD != 1 ~ FALSE),
    n_cond = case_when(
      CONDPROP_UNADJ == 1 ~ 'single',
      CONDPROP_UNADJ < 1 ~ 'multiple'),
    agbd_na = case_when(
      is.na(AGBD_LIVE) ~ TRUE,
      !is.na(AGBD_LIVE) ~ FALSE),
    ## 03/08 - make intersection flag to use to filter for plots we can use for GEDI intersections for OMY/MOG modeling
    int_flag = case_when(
      forested & n_cond == 'single' & FORTYPCD != 999 & !agbd_na ~ TRUE,
      TRUE ~ FALSE)
    ) %>% 
  left_join(state_table) %>% 
  relocate(STATEAB, REGION, .after=MEASYEAR)


## change region of ND, SD, NE, and KS to reg 9
ccc$REGION[ccc$STATEAB %in% c('ND','SD','NE','KS')] <- 9

## clip ccc and ppp to ef extent
ef <- st_read('./data/ef/ef_extent.gpkg') 
eff <- st_simplify(ef, dTolerance = 1000)
hex_ef <- st_intersection(hex, eff)



p_ef <- ppp %>% filter(EMAP_HEX %in% hex_ef$EMAP_HEX)
c_ef <- ccc %>% filter(puid %in% p_ef$puid) 



  

## 3/8 Notes: as far as I can tell, conditions that are missing DBH-HEIGHT-BIOMASS variables
## are due to FIA errors (sampling, recording), a plot being non-stocked (fortypcd==999)
## or logging - if all the trees were cut down, etc.  These plots can't be used, and thus
# are excluded from the int_flag (using !agbd_na)

# troubleshooting
# ww <- which(ccc$agbd_na & !is.na(ccc$n_trees))
# ccc %>% slice(ww) %>% filter(STATEAB == 'CT')
# 9_1_5_611_1

## now write out final cleaned and organized tables
cond_name <- paste0('./data/fia/fiadb/FIA_COND_EF_all_mostrecent_0417.csv')
fwrite(c_ef,cond_name)

plot_name <- paste0('./data/fia/fiadb/FIA_PLOT_EF_all_mostrecent_0417.csv')
fwrite(p_ef,plot_name)

##################
##################

# troubleshooting results and comparisons to 0207 tables

foo <- cc %>% 
  select(-c(MEASYEAR,STATECD,COUNTYCD,UNITCD,PLOT)) %>% 
  left_join(out, by='PLT_CN') %>% 
  left_join(state_table) %>% 
  mutate(
    forested = case_when(
      COND_STATUS_CD == 1 ~ TRUE,
      COND_STATUS_CD != 1 ~ FALSE),
    n_cond = case_when(
      CONDPROP_UNADJ == 1 ~ 'single',
      CONDPROP_UNADJ < 1 ~ 'multiple'),
    agbd_na = case_when(
      is.na(AGBD_LIVE) ~ TRUE,
      !is.na(AGBD_LIVE) ~ FALSE
    )
  ) %>% 
  filter(forested==T)

foo %>% filter(STATEAB == 'DE')

t <- table(foo$STATEAB, foo$agbd_na)
t <- rbind(t, total = colSums(t))
t <- cbind(t, t[,2] / rowSums(t) * 100)


# ###################################################################################################
# ## SUBPLOT table
# 
# states <- state.abb[state.abb != c('HI', 'AK')]
# 
# b <- list()
# for(i in states){
#   
#   # 1) get rFIA data
#   # state <- states[i]
#   state <- i
#   fia <- readFIA('/Volumes/RESEARCH/rFIA/original_data', nCores=10, inMemory=T, states=state, tables='SUBPLOT')
#   # fia <- clipFIA(FIA, mostRecent = TRUE)
#   
#   subp <- fia$SUBPLOT %>% 
#     dplyr::select(CN, PLT_CN, INVYR, STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SUBP, SUBP_STATUS_CD, SUBPCOND, CONDLIST) %>% 
#     mutate(
#       spuid = paste(STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SUBP, sep = "_"),
#       puid = paste(STATECD, INVYR, UNITCD, COUNTYCD, PLOT, sep = "_")
#     ) %>% 
#     rename(SUBP_CN=CN)
#   
#   
#   b[[state]] <- subp
#   print(state)
#   
# }
# 
# res <- as.data.frame(rbindlist(b))
# fwrite(res,'./data/fia/fiadb/CONUS_FIA_SUBPLOT_all_plots_all_years.csv')
# 
# 
# ###################################################################################################
# ## BOUNDARY table
# 
# states <- state.abb[state.abb != c('HI', 'AK')]
# 
# b <- list()
# for(i in states){
#   
#   # 1) get rFIA data
#   # state <- states[i]
#   state <- i
#   bb <- fread(paste0('/Volumes/RESEARCH/rFIA/original_data/',state,'_BOUNDARY.CSV'), integer64 = 'integer64') %>% 
#     as.data.frame() %>% 
#     dplyr::select(-contains(c('CREATED','MODIFIED')))
#   
#   b[[state]] <- bb
#   print(state)
#   
# }
# 
# res <- as.data.frame(rbindlist(b))
# fwrite(res,'./data/fia/fiadb/CONUS_FIA_BOUNDARY_all_plots_all_years.csv')

