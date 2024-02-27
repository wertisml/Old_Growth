#################################################################################################
#
# Ch3 - function to classify FIA plot condition as OG or not
#
#################################################################################################

rm(list=ls())

setwd("~/MOG")

library(rFIA)
library(sf)
library(sfhelpers)
library(lwgeom)
library(data.table)
library(dplyr)
library(stringr)
library(terra)
library(parallel)
library(future)
library(openxlsx)


options(scipen=999)

# FIA state names

source('Code/ch3_tools.R')
source('Code/plotting_tools.R')

state_table <- read.csv('Files/state_table.csv')

epa_lut <- fread('Files/old/ef_hex_epa2_lut_0613.csv') %>% as.data.frame()

#################################################################################################
# cond and plot tables, other data

cond <- fread('Files/old/FIA_COND_EF_all_mostrecent_0417.csv') %>% as.data.frame()
plot <- fread('Files/old/FIA_PLOT_EF_all_mostrecent_0417.csv') %>% as.data.frame()

# out <- cond %>% select(puid:STATEAB)
# fwrite(out, './data/forTravis/FIA_COND_EF_all_mostrecent_0417_forKevin_20230421.csv')


# dd <- read.xlsx('./definitions/my_og_definitions.xlsx',sheet='harmonized_0531', colNames=T, rows=1:22)
dd <- read.xlsx('Files/old/my_og_definitions.xlsx',sheet='harmonized_0718', colNames=T, rows=1:22)

#################################################################################################

# check region 9 forest types and if missing in og definitions list

# ef_ff <- sort(unique(cond$FORTYPCD))
# dd_ff <- sort(as.numeric(unique(str_split(paste(unlist(dd$FORTYPCD),collapse=', '),pattern = ', ')[[1]])))
# 
# # missing 
# missing <- ef_ff[!ef_ff %in% dd_ff]
# 
# # proportion of missing forest types relative to all plots in region 9
# cond %>%
#   filter(REGION == 9,
#          COND_STATUS_CD == 1) %>%
#   group_by(FORTYPCD) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n) * 100) %>%
#   filter(FORTYPCD %in% missing) %>%
#   arrange(desc(freq))
# 
# 
# cond %>%
#   filter(REGION == 8,
#          COND_STATUS_CD == 1) %>%
#   group_by(FORTYPCD) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n) * 100) %>%
#   filter(FORTYPCD %in% missing) %>%
#   arrange(desc(freq))


#################################################################################################
#################################################################################################

Region_8 <- state_table %>% filter(REGION == 8)
states <- states[states %in% Region_8$STATEAB]

states <- sort(unique(cond$STATEAB))

state_table$REGION[state_table$STATEAB %in% c('KS','SD','ND','NE')] <- 9

## 0629 manual change - change MO and KS regions from N to S because the dry oak forests there are likly more similar to those in S region 
## than states like WV or PA
state_table$REGION[state_table$STATEAB %in% c('KS','MO')] <- 8


mog <- list()
for(i in 1:length(states)){
# for(i in c(3,4,29)){  
  
  # filter cond table for this state
  state <- states[i]
  # Get the plots with the "cond"ition(s) we are looking for
  cc <- cond %>%  # Leaves us with only "good" plots
    filter(STATEAB == state,
           forested == T, # Because we need the plots to be forested
           FORTYPCD != 999) # Removes plots that were logged or stocked
  
  # filter definitions for this state's region - get community types specific to this region AND the ones that are shared
  # Uses the state we provided to identify the region we are in, then extracts the rules for that region
  defs <- dd %>% 
    filter(grepl(state_table$REGION[state_table$STATEAB == state],region))
  
  # read in trees, filter for only those conditions in cc
  # Leaves in only the forested plots based on our earlier conditions
  # TREE.csv contains all the individual trees for the state/ ROI
  tt <- fread(paste0('Files/rFIA/', state,'/',state,'_TREE.CSV'), integer64 = 'integer64') %>% as.data.frame() %>% 
    filter(PLT_CN %in% cc$PLT_CN) 
  
  # function to classify cond based on X number of trees larger than Y inches and Z stand age
  classify_cond <- function(x,tree){
  # res <- list()
  # for(j in 1:length(cc$cuid)){
    # x=cc$cuid[j]
    # get this condition and its corresponding row in the definitions df
    ccc <- cc %>% filter(cuid == x)
    idx <- which(grepl(ccc$FORTYPCD, defs$FORTYPCD)) 
    if(length(idx)==0) idx <- which(defs$name == 'other')
    
    # get tree table just for this plot
    tree <- tree %>% 
      mutate(cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_")) %>% 
      filter(cuid == x) %>% 
      group_by(cuid)
    
    t <- lapply(X=idx, FUN=classify_mog, tree=tree, ccc=ccc) %>% rbindlist %>% as.data.frame() 
    types <- paste(sort(t$community_abb), collapse=' ')
    ## Option 1: keep classifications from all community groups this fortypcd is associated with
    # foo <- pivot_wider(t, id_cols=cuid, names_from=community_abb, values_from = contains('class'), names_sort = T) 
    ## Option 2: keep classifications for community group that have the most OG/mature occurances
    oo <- t %>%
      mutate(abbs = types) %>% 
      rowwise() %>%
      mutate(n_old = sum(c_across(contains('class')) == "old"),
             n_mature = sum(c_across(contains('class')) == "mature")) %>% 
      group_by(cuid) %>% 
      # multiplefiltering in this order takes the row with the most old, and if a tie, the row with the most mature,  If everything is tied, take first, as all possible community types are saved in a col
      filter(n_old == max(n_old)) %>% 
      filter(n_mature == max(n_mature)) %>% 
      filter(row_number()==1)
      
    return(oo)
    # res[[j]] <- out
    # print(j)
    
  }
  
  plan(multisession, workers = 12)
  #res <- mclapply(X=cc$cuid, FUN=classify_cond, tree=tt) %>% rbindlist() %>% as.data.frame()
  res <- lapply(X=cc$cuid, FUN=classify_cond, tree=tt) %>% rbindlist() %>% as.data.frame()
  
  plan(sequential)
  
  out <- cc %>% 
    left_join(res, by='cuid')
  
  
  mog[[state]] <- out
  
  out_name <- paste0('Files/rFIA/Region_8/FIA_COND_OMY_',state,'.csv')
  fwrite(out,out_name)
  
  log <- paste0(Sys.time(),' ',state,' (',i,'/',length(states),') n=', nrow(res))
  print(log)
  
}

# read in OMY classified data 
files <- list.files('Files/rFIA/Region_8/',
                    pattern = "FIA_COND_OMY_",
                    full.names = T)
dat <- rbindlist(lapply(files, function(x){fread(x)})) %>% as.data.frame()

# remove forested cond's from original cond file and bind rows with classified plots to make sure we have ALL (forested and non) CONDs in the OMY file
non_forested <- cond %>% filter(!cuid %in% dat$cuid)
res <- bind_rows(dat,non_forested) %>% arrange(STATEAB, puid, cuid) %>% 
  # now set SITECLCD to NULL because its only there because of the bind_rows with cond, need to remove and add back in to get for all rows
  mutate(SITECLCD = NULL) %>% 
  left_join(y=cond %>% dplyr::select(cuid,SITECLCD), by='cuid')
  


#################################################################################
#################################################################################
## 04/27 - fill in classifications for non-forested and non-stocked plots (NA)

mask <- res$COND_STATUS_CD != 1 | res$FORTYPCD == 999 | is.na(res$STDAGE)
res[mask,c('class_a', 'class_s1', 'class_s2', 'class_fia1', 'class_fia2')] <- NA_character_

foo <- res %>% 
  mutate(
    class_a = factor(class_a, levels = c("old", "mature", "young")),
    class_s1 = factor(class_s1, levels = c("old", "mature", "young")),
    class_s2 = factor(class_s2, levels = c("old", "mature", "young")),
    class_fia1 = factor(class_fia1, levels = c("old", "mature", "young")),
    class_fia2 = factor(class_fia2, levels = c("old", "mature", "young")),
    )

#################################################################################
#################################################################################

### TWS Biomass-accumulation based OMY classifications


#========================================================================================#
# Skip for now
#========================================================================================#

# filter for east, assign a FIA forest group to each plot to replicate TWS OG classifications
fg <- fread('./definitions/FIA_ForestGroups.csv') %>% as.data.frame()
bks <- c(fg %>% pull(code),998,1000)
labs <- c(gsub(' Group','',fg$name),'Non-stocked')



cc <- foo %>% 
  mutate(forest_group = cut(FORTYPCD, breaks=bks, labels=labs))


# tidy the tws data so that I can make the age-based classifications without needing to rerun the states loop
# can do these classifications here via dataframe methods


lengthen_tws <- function(tws){
  
  p <- tws$`Site productivity class code`
  pp <- str_split(p, ', ', simplify = F)
  
  res <- list()
  for(i in 1:length(pp)){
    
    row <- tws[i]
    n <- length(pp[[i]])
    ps <- as.numeric(pp[[i]])
    
    dat <- row[rep(1,n),]
    dat$`Site productivity class code` <- ps
    res[[i]] <- dat
    
  }
  
  out <- bind_rows(res) %>% as.data.frame() %>% arrange(code)
  out$forest_group <- gsub(' Group','',out$`Forest type-group`)
  out$SITECLCD <- out$`Site productivity class code`
  return(out)
  
}

tws <- lengthen_tws(fread('./definitions/tws_age_table.csv'))

omy <- cc %>% 
  left_join(tws, by=c('forest_group','SITECLCD')) %>% 
  # dplyr::select(code,forest_group, SITECLCD,`Forest type-group`, STDAGE, age_young, age_mature, age_old) %>% 
  mutate(class_b = case_when(
    FORTYPCD == 999 ~ NA_character_,
    STDAGE >= age_old ~ 'old',
    STDAGE >= age_mature ~ 'mature',
    !is.na(STDAGE) | STDAGE == 0 ~ 'young',
  ),
  class_b = factor(class_b, levels = c("old", "mature", "young")),
  ) %>% 
  dplyr::select(-c(`Forest type-group`,`Site productivity class code`,age_young ,age_mature, carbon_young, carbon_mature, carbon_old)) %>% 
  relocate(class_b, .after=class_a) %>% 
  relocate(SITECLCD, .after=PHYSCLCD) %>% 
  ## 05/31 - additions - make combination classifications
  mutate(
    class_ab = case_when(
      class_a == 'old' & class_b == 'old' ~ 'old',
      (class_a == 'old' & class_b == 'mature') | (class_a == 'mature' & class_b == 'old') ~ 'old mature',
      class_a == 'mature' & class_b == 'mature' ~ 'mature',
      (class_a == 'young' & class_b == 'mature') | (class_a == 'mature' & class_b == 'young') ~ 'mature young',
      class_a == 'young' & class_b == 'young' ~ 'young',
      TRUE ~ NA_character_
    ),
    class_as1 = case_when(
      class_a == 'old' & class_s1 == 'old' ~ 'old',
      (class_a == 'old' & class_s1 == 'mature') | (class_a == 'mature' & class_s1 == 'old') ~ 'old mature',
      class_a == 'mature' & class_s1 == 'mature' ~ 'mature',
      (class_a == 'young' & class_s1 == 'mature') | (class_a == 'mature' & class_s1 == 'young') ~ 'mature young',
      class_a == 'young' & class_s1 == 'young' ~ 'young',
      TRUE ~ NA_character_
    ),
    class_bs1 = case_when(
      class_b == 'old' & class_s1 == 'old' ~ 'old',
      (class_b == 'old' & class_s1 == 'mature') | (class_b == 'mature' & class_s1 == 'old') ~ 'old mature',
      class_b == 'mature' & class_s1 == 'mature' ~ 'mature',
      (class_b == 'young' & class_s1 == 'mature') | (class_b == 'mature' & class_s1 == 'young') ~ 'mature young',
      class_b == 'young' & class_s1 == 'young' ~ 'young',
      TRUE ~ NA_character_
    ),
    class_abs1 = case_when(
      class_a == 'old' & class_b == 'old' & class_s1 == 'old' ~ 'old',
      class_a %in% c('old','mature') & class_b %in% c('old','mature') & class_s1 %in% c('old','mature') ~ 'mature',
      TRUE ~ 'young'
    )
  ) %>% 
  relocate(class_ab,class_as1,class_bs1,class_abs1, .after=class_s1) %>% 
  left_join(x=., y=dd[,c('abb','og_age')], by=c('community_abb'='abb')) %>% 
  rename(og_age_tws = age_old,
         og_age_fia = og_age) %>% 
  mutate(og_age_dif = og_age_fia - og_age_tws) %>% 
  left_join(x=., y=plot[,c('puid','EMAP_HEX')], by='puid') %>% 
  left_join(y=epa_lut, by='EMAP_HEX')
  


# fwrite(omy,'./data/fia/fiadb/FIA_COND_EF_all_mostrecent_0718_OMY.csv')

# fixed the tropical hardwood FORTYPCD that didnt get joined to the TWS age table - these plots were all classified as functionally young when some should have been mature
# none were cassified as old and so no results later in paper would be changed
fwrite(omy,'./data/fia/fiadb/FIA_COND_EF_all_mostrecent_0807_OMY.csv')



omy %>% filter(forested) %>% 
  group_by(forest_group) %>% 
  summarise(
    na_og_age_tws = sum(is.na(og_age_tws)),
  )


omy %>% filter(forested, is.na(og_age_tws)) %>% pull(FORTYPCD) %>% unique() %>% sort()

omy %>% filter(forested) %>% 
  group_by(community_abb) %>% 
  summarise(
    na_og_age_tws = sum(is.na(og_age_tws)),
  ) %>% print(n=50)


omy %>% filter(forested,
               is.na(class_a),
               FORTYPCD != 999,
               !is.na(STDAGE))
  
