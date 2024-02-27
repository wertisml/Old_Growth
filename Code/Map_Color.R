library(sf)
library(remotes)
library(colortools)
library(tidyverse)

div_color_ramp <- function(difference_var, middle_range=.5, include_all=F, Extreme=F, increments = F, hot_col='orangered', cold_col='royalblue3') {
  
  if(middle_range==1){
    diff_range <- range(difference_var, na.rm=T)
    diff_range <- c(floor(diff_range[1]),ceiling(diff_range[2]))
  } else{
    tail <- (1-middle_range)/2
    quants <- quantile(difference_var,probs=c(tail,1-tail), na.rm=T)
    diff_range <- c(floor(quants[1]),ceiling(quants[2]))  
  }
  
  extreme_val <- max(abs(diff_range), na.rm=T)
  ncols <- extreme_val * 2 + 1
  if(ncols<21){ ncols <- 21}
  extreme_bks <- seq(-extreme_val,extreme_val,length.out=ncols)
  extreme_bks <- c(extreme_bks[extreme_bks < 0],0,extreme_bks[extreme_bks > 0])
  
  # add most positive or negative extreme into breaks
  if(min(difference_var, na.rm=T) < min(extreme_bks, na.rm=T)) {extreme_bks <- c(floor(min(difference_var, na.rm=T)),extreme_bks)}
  if(max(difference_var, na.rm=T) > max(extreme_bks, na.rm=T)) {extreme_bks <- c(extreme_bks,ceiling(max(difference_var, na.rm=T)))}
  
  hot_cols <- setColors(hot_col, 10)[c(2,1)]
  cold_cols <- setColors(cold_col, 10)[c(1,10)]
  
  # div_col_pal <- colorRampPalette(colors=c('#8470FF','#70C7FF','white','#EDBC64','#ED6473'))
  div_col_pal <- colorRampPalette(colors=c(cold_cols,'white',hot_cols))
  div_cols <- div_col_pal(length(extreme_bks))
  
  # find if max or min is less extreme
  less_extreme <- which.min(abs(diff_range))
  
  # if extremes are equally far from zero - return dcols and dbreaks
  if(abs(diff_range)[1] == abs(diff_range)[2]){
    dbks <- extreme_bks
    
    if(which.max(abs(range(difference_var, na.rm=T))) == 1){
      dcols <- div_cols[-length(div_cols)]
    } else{
      dcols <- div_cols[-1]
    }
  } else{
    # which which break point is closest to the max/min that is less extreme
    if(less_extreme == 2){
      stop <- which.min(abs(extreme_bks - diff_range[less_extreme]))
      start <- 1
      dcols <- div_cols[start:(stop-1)]
      dbks <- extreme_bks[start:(stop)]
    } else {
      stop <- ncols
      start <- which.min(abs(extreme_bks - diff_range[less_extreme]))
      dcols <- div_cols[(start+1):stop]
      dbks <- extreme_bks[(start):stop]
    }
  }
  
  if(include_all==T){
    # include the values at the extremes that are out of the breaks when inner 90% is used
    dbks <- c(floor(min(difference_var, na.rm=T)),dbks[2:(length(dbks)-1)],ceiling(max(difference_var, na.rm=T)))  
  }
  if(increments==T){
    # include the values at the extremes that are out of the breaks when inner 90% is used
    dbks <- seq(0,1,0.05)  
    div_col_pal <- colorRampPalette(colors=c('#e5f5f9','#109210', 'forestgreen', 'darkgreen','#005417'))
    div_cols <- div_col_pal(length(dbks))
    dcols <- div_cols[1:20]
  }
  if(Extreme == T){
    dbks <- sort(c(-1/1.8^(seq_len(10) - 1), 0, 1/1.8^(seq_len(10) - 1))) # This is specifically for the minimum difference in the R8/R9 data 
  }
  
  l <- list(cols = dcols,
            breaks = dbks)
  
  return(l)
  
}