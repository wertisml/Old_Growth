library(rFIA)
library(sf)
library(sfhelpers)
library(lwgeom)
library(tidyverse)
library(data.table)
library(terra)
library(parallel)
library(future)
library(arrow)
library(progress)

setwd("~/MOG")

FS_Region <- 6
Sub_Region <- ".2"

cond <- open_dataset('Files/PLot_and_Cond_Regions.parquet') %>% 
  filter(REGION == paste0("0",FS_Region,Sub_Region),
         FORTYPCD != 999) %>%
  collect() %>%
  arrange(cuid, desc(MEASYEAR), sum(!is.na(.))) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup()

defs <- read_csv(paste0("Files/Region_",FS_Region,".csv")) %>%
  filter(Forest_Region == paste0(FS_Region,Sub_Region))

Region <- function(Region_Number){
  source(paste0("Code/Region_",Region_Number,"_OG.R"))
}

Region(FS_Region)

#==============================================================================#
#
#==============================================================================#

plan(multisession, workers = 12)

tt <- open_dataset("Files/Trees.parquet") %>%
  filter(PLT_CN %in% cond$PLT_CN) %>%
  collect() %>%
  mutate(Status = ifelse(STATUSCD == 1, "Live", 
                         ifelse(STATUSCD == 2, "Dead", NA)),
         Downed_Dead = ifelse(STANDING_DEAD_CD == 0, "Downed", 
                              ifelse(STANDING_DEAD_CD == 1, "Standing", "Live")),
         BA = (DIA * abs(DIA) * 0.005454)) %>% # is this calculated correctly?
  dplyr::select(-STATUSCD, -STANDING_DEAD_CD) 

  # function to classify cond based on X number of trees larger than Y inches and Z stand age
  classify_cond <- function(x,tree){
    pb$tick()
    #cat("Processing cuid:", x, "\n") # So you can see which cuid is getting used, helpful for issues
    # get this condition and its corresponding row in the definitions df
    ccc <- cond %>% filter(cuid == x)
    idx <- which(grepl(ccc$FORTYPCD, defs$FIA_Code)) 
    if(length(idx)==0){return(NULL)}
    
    # get tree table just for this plot
    tree <- tree %>% 
      filter(cuid == x) %>% 
      group_by(cuid)
    if(nrow(tree)==0){return(NULL)}
    
    t <- lapply(X=idx, FUN=classify_mog, tree=tree, ccc=ccc) %>% rbindlist %>% data.frame() 
    #types <- paste(sort(t$community_abb), collapse=' ')
    ## Option 1: keep classifications from all community groups this fortypcd is associated with
    # foo <- pivot_wider(t, id_cols=cuid, names_from=community_abb, values_from = contains('class'), names_sort = T) 
    ## Option 2: keep classifications for community group that have the most OG/mature occurances
    oo <- t %>%
      #mutate(abbs = types) %>% # Abreviation of the tree type
      rowwise() %>%
      mutate(n_old = sum(c_across(contains('class')) == TRUE, na.rm=T)) %>% 
      group_by(cuid) %>% # For some reason in my method of doing this cuid isn ot kept
      # When multiple FIA codes work, the oldest is selected
      filter(n_old == max(n_old)) %>% 
      filter(row_number()==1)
    
    return(oo)
    
}

pb <- progress_bar$new(
  format = "[:bar] :percent | :current/:total | Elapsed: :elapsed | ETA: :eta",
  total = length(unique(cond$cuid))
)

res <- lapply(X=cond$cuid, FUN=classify_cond, tree=tt) %>% 
  rbindlist(fill = TRUE) %>% 
  data.frame()

pb$terminate()

out <- cond %>% 
  left_join(res, by='cuid') %>%
  rowwise() %>%
  mutate(Old_Growth = if_else(all(c_across(contains('class'))), "Old Growth", NA))
  
out_name <- paste0('Files/Region',FS_Region,'.parquet')
write_parquet(out,out_name)
  
plan(sequential)






