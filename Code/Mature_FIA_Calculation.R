library(lwgeom)
library(tidyverse)
library(data.table)
library(parallel)
library(future)
library(arrow)
library(progress)

setwd("~/Old_Growth")

FS_Region <- 8
Sub_Region <- ""

cond <- open_dataset('Files/PLot_and_Cond_Regions.parquet') %>% 
  filter(REGION == paste0("0",FS_Region,Sub_Region),
         FORTYPCD != 999) %>%
  select(cuid, puid, INVYR, PLT_CN, STDAGE, FORTYPCD, MEASYEAR, REGION, CONDPROP_UNADJ) %>%
  collect() %>%
  arrange(cuid, desc(MEASYEAR), sum(!is.na(.))) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup()

age <- read_csv("Files/Mature_Age.csv")

Vegtype <- read_csv("Files/Vegtype.csv") %>%
  filter(grepl(paste0("^R",FS_Region), Vegtype))

age <- age %>%
  left_join(Vegtype, join_by("Vegetation Type" == "fgroupname"))

source("Code/Mature_Growth.R")

#==============================================================================#
#
#==============================================================================#

plan(multisession, workers = 12)

tt <- open_dataset("Files/Trees.parquet") %>%
  filter(cuid %in% cond$cuid) %>%
  collect() %>%
  mutate(Status = ifelse(STATUSCD == 1, "Live", 
                         ifelse(STATUSCD == 2, "Dead", NA)),
         Downed_Dead = ifelse(STANDING_DEAD_CD == 0, "Downed", 
                              ifelse(STANDING_DEAD_CD == 1, "Standing", "Live")),
         BA = (DIA * abs(DIA) * 0.005454)) %>% # is this calculated correctly?
  group_by(cuid) %>%
  filter(INVYR == max(INVYR)) %>%
  dplyr::select(-STATUSCD, -STANDING_DEAD_CD)

# function to classify cond based on X number of trees larger than Y inches and Z stand age
classify_cond <- function(x,tree){
  pb$tick()

  ccc <- cond %>% filter(cuid == x)
  #cuid <- ccc$cuid
  mature_age <- which(grepl(ccc$FORTYPCD, age$FIA_Code)) 
  if(length(mature_age)==0){return(NULL)}

  # get tree table just for this plot
  tree <- tree %>% 
    filter(cuid == x) %>% 
    group_by(cuid) %>%
    left_join(ccc %>%
                select(cuid, CONDPROP_UNADJ),
              join_by(cuid))
  
  if(nrow(tree)==0){return(NULL)}

  t <- lapply(X = mature_age, FUN=Classify_Mature, tree=tree, ccc=ccc) %>% rbindlist %>% data.frame() 
  
  return(t)
  
}

pb <- progress_bar$new(
  format = "[:bar] :percent | :current/:total | Elapsed: :elapsed | ETA: :eta",
  total = length(unique(cond$cuid))
)

res <- lapply(X=cond$cuid, FUN=classify_cond, tree=tt) %>% 
  rbindlist(fill = TRUE) %>% 
  data.frame()

plan(sequential)

out <- cond %>%
  select(REGION, cuid) %>%
  left_join(res, by='cuid') %>%
  rowwise()

write_parquet(out, "Mature_Metrics.parquet")








