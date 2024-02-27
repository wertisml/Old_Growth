library(rFIA)
library(sf)
library(sfhelpers)
library(lwgeom)
library(tidyverse)
library(data.table)
library(terra)
library(parallel)
library(future)
library(openxlsx)
library(arrow)

options(scipen=999)

setwd("~/MOG")

#source('Code/plotting_tools.R')

state_table <- read_csv('Files/state_table.csv')

epa_lut <- read_csv('Files/ef_hex_epa2_lut_0613.csv') %>% as.data.frame()

cond <- open_dataset('Files/cond_and_plot.parquet') %>% 
  filter(REGION == 8) %>%
  select(-BAPA_LIVE, -BAPA_DEAD) %>%
  collect()

dd <- read_csv("Files/Region_8_NWFP.csv")

#==============================================================================#
#
#==============================================================================#

Region <- function(Region_Number){
Region <- state_table %>% filter(REGION == Region_Number)
states <- unique(cond$STATEAB[cond$STATEAB %in% Region$STATEAB])

source(paste0("Code/Region_",Region_Number,"_OG.R"))

return(states)
}

states <- Region(8)

start <- now()
plan(multisession, workers = 12)

mog <- list()
for(i in 1:length(states)){
  
  # filter cond table for this state
  state <- states[i]
  # Get the plots with the "cond"ition(s) we are looking for
  cc <- cond %>%  # Leaves us with only "good" plots
    filter(STATEAB == state,
           #forested == T, # Because we need the plots to be forested
           FORTYPCD != 999) # Removes plots that were logged or stocked
  
  # filter definitions for this state's region - get community types specific to this region AND the ones that are shared
  # Uses the state we provided to identify the region we are in, then extracts the rules for that region
  defs <- dd %>% 
    filter(grepl(state_table$REGION[state_table$STATEAB == state],region))
  
  # read in trees, filter for only those conditions in cc
  # Leaves in only the forested plots based on our earlier conditions
  # TREE.csv contains all the individual trees for the state/ ROI
  tt <- fread(paste0('Files/rFIA/', state,'/',state,'_TREE.CSV'), integer64 = 'integer64') %>% as.data.frame() %>% 
    filter(PLT_CN %in% cc$PLT_CN) %>%
    mutate(Status = ifelse(STATUSCD == 1, "Live", 
                           ifelse(STATUSCD == 2, "Dead", NA)),
           Downed_Dead = ifelse(STANDING_DEAD_CD == 0, "Downed", 
                                ifelse(STANDING_DEAD_CD == 1, "Standing", "Live"))) %>%
    dplyr::select(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, DIA, TPA_UNADJ,
                  CULL, Status, SPCD, HT, Downed_Dead)
  
  # function to classify cond based on X number of trees larger than Y inches and Z stand age
  classify_cond <- function(x,tree){
    
    # get this condition and its corresponding row in the definitions df
    ccc <- cc %>% filter(cuid == x)
    idx <- which(grepl(ccc$FORTYPCD, defs$FIA_Code)) 
    if(length(idx)==0) idx <- which(defs$OG_Type == 'other')
    
    # get tree table just for this plot
    tree <- tree %>% 
      mutate(cuid = paste(STATECD, UNITCD, COUNTYCD, PLOT, CONDID, sep = "_"),
             BA = (DIA * abs(DIA) * 0.005454)) %>% 
      filter(cuid == x) %>% 
      group_by(cuid) %>%
      dplyr::select(-UNITCD, -COUNTYCD, -PLOT, -CONDID)
    
    t <- lapply(X=idx, FUN=classify_mog, tree=tree, ccc=ccc) %>% rbindlist %>% data.frame() 
    #types <- paste(sort(t$community_abb), collapse=' ')
    ## Option 1: keep classifications from all community groups this fortypcd is associated with
    # foo <- pivot_wider(t, id_cols=cuid, names_from=community_abb, values_from = contains('class'), names_sort = T) 
    ## Option 2: keep classifications for community group that have the most OG/mature occurances
    oo <- t %>%
      #mutate(abbs = types) %>% # Abreviation of the tree type
      rowwise() %>%
      mutate(n_old = sum(c_across(contains('class')) == TRUE)) %>% 
      group_by(cuid) %>% # For some reason in my method of doing this cuid isn ot kept
      # When multiple FIA codes work, the oldest is selected
      filter(n_old == max(n_old)) %>% 
      filter(row_number()==1)
    
    return(oo)
    
  }
  
  res <- lapply(X=cc$cuid, FUN=classify_cond, tree=tt) %>% 
    rbindlist(fill = TRUE) %>% 
    data.frame()
  
  out <- cc %>% 
    left_join(res, by='cuid') %>%
    rowwise() %>%
    mutate(Old_Growth = if_else(all(c_across(contains('class'))), "Old Growth", NA))
  
  mog[[state]] <- out
  
  out_name <- paste0('Files/rFIA/FIA_COND_OMY_',state,'.parquet')
  write_parquet(out,out_name)
  
  log <- paste0(Sys.time(),' ',state,' (',i,'/',length(states),') n=', nrow(res))
  print(log)
  
}

plan(sequential)

end <- now()
end-start

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






