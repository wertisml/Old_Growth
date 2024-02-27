#################################################################################################
#
# Ch3 tools
#
#################################################################################################
library(rFIA)
library(sf)
library(sfhelpers)
library(lwgeom)
library(data.table)
library(dplyr)
library(stringr)
library(terra)
library(parallel)

options(scipen=999)

# FIA state names


statesdf <- rFIA:::intData$stateNames %>% 
  filter(!STATEAB %in% c('HI', 'AK','AS','FM','GU','MP','PW','PR','VI')) %>% 
  mutate(REGION = c(8,3,8,5,2,9,9,8,8,4,9,9,9,2,8,8,9,9,9,9,9,8,9,1,2,4,9,9,3,9,8,1,9,8,6,9,9,8,2,8,8,4,9,8,6,9,9,2))

states <- statesdf$STATEAB
# write.csv(statesdf, '/Users/jamis/dropbox/myphd/ch3/data/fia/misc/state_table.csv', row.names = F, quote = F)





#################################################################




get_omy <- function(type=NULL){

# fread('./data/fia/fiadb/FIA_COND_EF_all_mostrecent_0417_OMY.csv', na.strings=c('')) %>% 
# fread('./data/fia/fiadb/FIA_COND_EF_all_mostrecent_0427_OMY.csv', na.strings=c('')) %>% 
# fread('./data/fia/fiadb/FIA_COND_EF_all_mostrecent_0531_OMY.csv', na.strings=c('')) %>% 
  # fread('./data/fia/fiadb/FIA_COND_EF_all_mostrecent_0629_OMY.csv', na.strings=c('')) %>% 
  
  # omy_name <- './data/fia/fiadb/FIA_COND_EF_all_mostrecent_0718_OMY.csv'
  omy_name <- 'Files/old/FIA_COND_EF_all_mostrecent_0807_OMY.csv'

  
  if(!is.null(type)){
    if(type == 'upper'){
      omy_name <- gsub('_OMY','_OMY_upper',omy_name)
    } else if (type == 'lower'){
      omy_name <- gsub('_OMY','_OMY_lower',omy_name)
    }
  }
  
  fread(omy_name, na.strings=c('')) %>% 
  #as.data.frame() %>% 
  mutate(
    class_a = factor(class_a, levels = c("old", "mature", "young")),
    class_b = factor(class_b, levels = c("old", "mature", "young")),
    class_s1 = factor(class_s1, levels = c("old", "mature", "young")),
    class_s2 = factor(class_s2, levels = c("old", "mature", "young")),
    class_fia1 = factor(class_fia1, levels = c("old", "mature", "young")),
    class_fia2 = factor(class_fia2, levels = c("old", "mature", "young")),
    class_valid = !is.na(class_a)
  )
}


# foo <- get_omy()


#################################################################

# rpart modeling file management

make_model_dir <- function(x){
  dir <- paste0('./figures/gedi_hex_forested/models/rpart/',mod_dir)
  if(!dir.exists(dir)) {
    dir.create(dir)
    dir.create(paste0(dir,'/maps'))
  }
}

get_model_dir <- function(x){
  paste0('./figures/gedi_hex_forested/models/rpart/',x)
}


################################################################
# functions to classify FIA plot conditions as young, mature, old

n_big_tpa <- function(tree, big_tree_size){ 
  sum(tree$TPA_UNADJ[tree$DIA > big_tree_size], na.rm=T) 
  }



# function to classify MOG for a single community type
classify_mog <- function(idx, tree, ccc){
tree %>% 
  summarise(
    nbtpa1_og = n_big_tpa(tree=., big_tree_size = defs$og_d1[idx]),
    nbtpa1_ma = n_big_tpa(tree=., big_tree_size = defs$ma_d1[idx]),
    nbtpa2_og = n_big_tpa(tree=., big_tree_size = defs$og_d2[idx]),
    nbtpa2_ma = n_big_tpa(tree=., big_tree_size = defs$ma_d2[idx]),
  ) %>% 
  left_join(x=ccc,y=., by='cuid') %>% 
  mutate(
    class_a = case_when(
      ( STDAGE + (2023-MEASYEAR) > defs$og_age[idx] ) ~ 'old',
      ( STDAGE + (2023-MEASYEAR) > defs$ma_age[idx] ) ~ 'mature',
      TRUE ~ 'young'
    ),
    class_s1 = case_when(
      ( nbtpa1_og > defs$tpa1[idx] ) ~ 'old',
      ( nbtpa1_ma > defs$tpa1[idx] ) ~ 'mature',
      TRUE ~ 'young'
    ),
    class_s2 = case_when(
      # need to apply conversions to basal area to convert from m2/ha to ft2/acre
      ( nbtpa2_og > defs$tpa2[idx] ) & ( BAPH_LIVE / (2.471/10.76) > defs$og_ba2[idx] ) ~ 'old',
      ( nbtpa2_ma > defs$tpa2[idx] ) & ( BAPH_LIVE / (2.471/10.76) > defs$ma_ba2[idx] ) ~ 'mature',
      TRUE ~ 'young'
    ),
    class_fia1 = case_when(
      ( nbtpa1_og > defs$tpa1[idx] ) & ( STDAGE + (2023-MEASYEAR) > defs$og_age[idx] ) ~ 'old',
      ( nbtpa1_ma > defs$tpa1[idx] ) & ( STDAGE + (2023-MEASYEAR) > defs$ma_age[idx] ) ~ 'mature',
      TRUE ~ 'young'
    ),
    class_fia2 = case_when(
      ( nbtpa2_og > defs$tpa2[idx] ) & ( BAPH_LIVE / (2.471/10.76) > defs$og_ba2[idx] ) & ( STDAGE + (2023-MEASYEAR) > defs$og_age[idx] ) ~ 'old',
      ( nbtpa2_ma > defs$tpa2[idx] ) & ( BAPH_LIVE / (2.471/10.76) > defs$ma_ba2[idx] ) & ( STDAGE + (2023-MEASYEAR) > defs$ma_age[idx] ) ~ 'mature',
      TRUE ~ 'young'
    ),
    community_abb = defs$abb[idx],
    # community_type = defs$type[idx],
    # community_name = defs$name[idx],
  ) %>% 
  dplyr::select(cuid, contains('class'), nbtpa1_og, nbtpa1_ma, nbtpa2_og, nbtpa2_ma, contains('community')) %>% ungroup()


}


## pipe-able function to make categorical variable into set of binary variables
make_binary <- function(.data, colname){
  o_name <- paste0(colname,'_o')
  m_name <- paste0(colname,'_m')
  y_name <- paste0(colname,'_y')
  df <- .data
  df[[o_name]] <- as.numeric(df[[colname]] == 'old')
  df[[m_name]] <- as.numeric(df[[colname]] == 'mature')
  df[[y_name]] <- as.numeric(df[[colname]] == 'young')
  df[[o_name]][is.na(df[[o_name]])] <- 0
  df[[m_name]][is.na(df[[m_name]])] <- 0
  df[[y_name]][is.na(df[[y_name]])] <- 0
  return(df)
}


make_binary_old <- function(.data, colname){
  o_name <- paste0(colname,'_o')
  df <- .data
  df[[o_name]] <- as.numeric(df[[colname]] == 'old')
  df[[o_name]][is.na(df[[o_name]])] <- 0
  return(df)
}

make_binary_mature <- function(.data, colname){
  name <- paste0(colname,'_m')
  df <- .data
  df[[name]] <- as.numeric(df[[colname]] == 'old mature' | df[[colname]] == 'mature')
  df[[name]][is.na(df[[name]])] <- 0
  return(df)
}




## rpart variable importance functions

## get variable importance for each OG flavor for all regions
get_var_imp <- function(x, var){
  vi <- round(x[[var]]$variable.importance,3)
  vi_scaled <- round(vi * (100/sum(vi)),3)
  dat <- data.frame(vi_raw = vi, vi_scaled=vi_scaled)
  dat$variable <- names(vi)
  row.names(dat) <- NULL
  dat %>% relocate(variable) %>% left_join(y=vardat, by='variable')
}

## now synthesize variable importance in each region for each OG flavor 
agg_top_vi_by_metric <- function(x, n){
  # out <- x[1:n,] %>%
  out <- x %>%
    # group_by(metric) %>% 
    group_by(type1) %>% 
    summarise(
      vi_raw = sum(vi_raw),
      vi_scaled = sum(vi_scaled),
      n = n()
    ) %>% 
    arrange(desc(vi_raw))
  
  if(any(!unique(vardat$type1) %in% out$type1)){
    
    missing <- data.frame(type1=unique(vardat$type1)[!unique(vardat$type1) %in% out$type1])
    out <- bind_rows(out,missing)
    out[is.na(out)] <- 0
  }
  return(out)
}

plot_vi <- function(x, names = T, ...){
  
  x <- x %>% arrange(type1)
  if(names){
    na <- rev(x$type1)
  } else{
    na <- rep('',nrow(x))
    # na <- rev(x$type)
  }
  
  
  barplot(rev(x$vi_scaled), horiz=T, names.arg=na, ylim=c(-.5,10.5), xaxt='n',...)
  # par(mar=mm)
  vi_sum <- paste0(sum(x$n, na.rm=T),' vars: ',round(sum(x$vi_scaled, na.rm=T)),'%')
  legend('bottomright',vi_sum,bty='n', col='white', text.font=1, cex=1)
  
}
plot_vi_by_flavor <- function(reg){
  
  # xlim = c(0,max(a_o_vi_agg[[reg]]$vi_scaled,b_o_vi_agg[[reg]]$vi_scaled,s_o_vi_agg[[reg]]$vi_scaled,STDAGE_vi_agg[[reg]]$vi_scaled, na.rm=T))
  xlim = c(0,max(a_o_vi_agg[[reg]]$vi_scaled,b_o_vi_agg[[reg]]$vi_scaled,s_o_vi_agg[[reg]]$vi_scaled, na.rm=T))
  
  par(mfrow=c(2,2))
  plot_vi(a_o_vi_agg[[reg]], main=paste0(reg,', temporal'), xlim=xlim)
  plot_vi(b_o_vi_agg[[reg]], main=paste0(reg,', functional'), xlim=xlim)
  plot_vi(s_o_vi_agg[[reg]], main=paste0(reg,', physical'), xlim=xlim)
  # plot_vi(STDAGE_vi_agg[[reg]], main=paste0(reg,', STDAGE'), xlim=xlim)
  
}
plot_vi_by_region <- function(vi, flavor){
  
  # xlim = c(
  #   0,
  #   bind_rows(vi) %>% pull(vi_scaled) %>% max(., na.rm=T)
  # )

  xlim=c(0,30)
  
  par(mfrow=c(1,6))
  
  for(i in 1:length(regions)){
    
    reg <- as.character(regions[i])
    k <- key %>% select(type1) %>% distinct()
    rvi <- vi[[reg]] 
    rrvi <- bind_rows(rvi, k %>% filter(!k$type1 %in% rvi$type1))
    
    
    mm <- par()$mar
    
    if(i==1){
      
      par(mar=c(1,2,1,1))
      plot_vi(rrvi, main=paste0(flavor,': ', reg), xlim=xlim, names=T)

    } else{
      par(mar=c(1,2,1,1))
      plot_vi(rrvi, main=paste0(flavor,': ', reg), xlim=xlim, names=F)
    }
    

    par(mar=mm)
    
    
    
  }
}



################################################################
## OLD - might use later if needed

## extends a line into the polygon
# line_stretchntrim <- function(line, polygon) {
#   if (st_crs(line) != st_crs(polygon))
#     return("CRS not matching")
#   bb <- st_bbox(polygon)
#   bbdiagLength <-
#     as.numeric(sqrt((bb$xmin - bb$xmax) ^ 2 + (bb$ymin - bb$ymax) ^ 2))
#   xy <- st_coordinates(line)[, 1:2]
#   npairs <- nrow(xy) / 2
#   etline <- NULL
#   for (i in 1:npairs) {
#     ii <- (i - 1) * 2 + 1
#     x <- as.numeric(xy[ii:(ii + 1), 1])
#     y <- as.numeric(xy[ii:(ii + 1), 2])
#     dxline <- diff(x)
#     dyline <- diff(y)
#     d <- sqrt(dxline ^ 2 + dyline ^ 2)
#     scale <- abs(as.numeric(bbdiagLength)) # * extra if need be
#     signx <- sign(dxline)
#     signy <- sign(dyline)
#     theta <- atan(dxline / dyline)
#     #  expand
#     if (signy == 1) {
#       dx1 <-  -sin(theta) * scale #* d
#       dy1 <-  -cos(theta) * scale #* d
#       dx2 <-    sin(theta) * scale #* d
#       dy2 <-    cos(theta) * scale #* d
#     }
#     if (signy == -1) {
#       dx1 <-    sin(theta) * scale# * d
#       dy1 <-    cos(theta) * scale# * d
#       dx2 <-  -sin(theta) * scale# * d
#       dy2 <-  -cos(theta) * scale# * d
#     }
#     if ((dxline == 0) * (signy == -1)) {
#       dx1 <-  0
#       dy1 <-  cos(theta) * scale# * d
#       dx2 <-  0
#       dy2 <-  -cos(theta) * scale# * d
#     }
#     if ((dxline == 0) * (signy ==  1)) {
#       dx1 <-  0
#       dy1 <-  -cos(theta) * scale# * d
#       dx2 <-  0
#       dy2 <-    cos(theta) * scale# * d
#     }
#     if ((signx == 1) * (dyline == 0)) {
#       dx1 <-  -sin(theta) * scale# * d
#       dy1 <-  0
#       dx2 <-    sin(theta) * scale# * d
#       dy2 <-  0
#     }
#     if ((signx == -1) * (dyline == 0)) {
#       dx1 <-    sin(theta) * scale# * d
#       dy1 <-  0
#       dx2 <-  -sin(theta) * scale# * d
#       dy2 <-  0
#     }
#     x1 <- x[1] + dx1
#     y1 <- y[1] + dy1
#     x2 <- x[2] + dx2
#     y2 <- y[2] + dy2
#     sline <- st_linestring(matrix(c(x1, y1, x2, y2),byrow = TRUE, ncol = 2))
#     slineSf <- st_sf(geom = st_sfc(sline), crs = st_crs(polygon))
#     stline <-  st_intersection(slineSf, polygon)
#     etline <- if (i == 1)
#       stline
#     else
#       rbind(etline, stline)
#   }
#   etline
# }

## gets a bbox from a polygon
# bbox_polygon <- function(x) {
#   bb <- sf::st_bbox(x)
#   
#   p <- matrix(
#     c(bb["xmin"], bb["ymin"], 
#       bb["xmin"], bb["ymax"],
#       bb["xmax"], bb["ymax"], 
#       bb["xmax"], bb["ymin"], 
#       bb["xmin"], bb["ymin"]),
#     ncol = 2, byrow = T
#   )
#   
#   sf::st_polygon(list(p))
# }
# 
