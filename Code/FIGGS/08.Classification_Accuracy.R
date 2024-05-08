#------------ OLD GROWTH CLASSIFICATION ACCURACY ----------------------------#

# create new dataframe
fia.mf.class <- og.fia

# start doing classification
for (i in 1:nrow(fia.mf.class)) {
  
  # subset fia data 
  fia.sub <- og.fia[i,]
  
  # subset age cut data
  veg.age.sub <- subset(veg.age.cut, subset = GROUP == fia.sub$Vegtype)
  
  # check if there is data 
  if (nrow(na.omit(veg.age.sub)) > 0) {
    
    # calculate correlation weighting factor
    veg.age.sub$cor.abs <- abs(as.numeric(veg.age.sub$COR))
    cor.fact <- sum(veg.age.sub$cor.abs)
    veg.age.sub$cor.weight <- veg.age.sub$cor.abs / cor.fact
    
    # pull out variable names 
    veg.age.var <- veg.age.sub$VARIABLE
    
    # pull out only the metrics we need 
    veg.age.sub$var.value <- as.numeric(fia.sub[,veg.age.var])
    
    # split data into negative and positive correlations
    veg.pos.age.sub <- veg.age.sub[veg.age.sub$COR > 0,]
    veg.neg.age.sub <- veg.age.sub[veg.age.sub$COR < 0,]
    
    # do classification
    veg.pos.age.sub$mature.class <- ifelse(veg.pos.age.sub$var.value >= veg.pos.age.sub$MATURE.CUT, 1,0)
    veg.neg.age.sub$mature.class <- ifelse(veg.neg.age.sub$var.value <= veg.neg.age.sub$MATURE.CUT, 1,0)
    
    # combine data
    veg.age.sub <- rbind(veg.pos.age.sub, veg.neg.age.sub)
    
    # find weighted value
    fia.mf.class$FINAL.CLASS[i] <- round(sum(veg.age.sub$cor.weight * veg.age.sub$mature.class),2)
  }
  
  # if no data then do this
  if (nrow(na.omit(veg.age.sub)) == 0) {
    
    fia.mf.class$FINAL.CLASS[i] <- NA
    
  }
}

# pull out all data over 0.5 and all less than 0.5
og.mature <- fia.mf.class[fia.mf.class$FINAL.CLASS >= 0.5,]
not.og.mature <- fia.mf.class[fia.mf.class$FINAL.CLASS < 0.5,]

# check percent correct
og.class.acc <- round((nrow(og.mature)/nrow(fia.mf.class))*100, 2)

#  book keeping
print(paste0('OFE estimates ', round(sum(na.omit(as.numeric(og.fia$acres)))), ' acres of old growth forest on NFS and BLM lands.'))

#  book keeping
print(paste0('FIGSS estimates ', mature.est[81,3], ' acres of mature forest on NFS and BLM lands.'))

# book keeping
print(paste0('After applying the mature thresholds to old growth plots, FIGSS correctly classifies ', og.class.acc,
             '% of old growth plots as old growth. The total number of plots in this analysis is ', nrow(og.mature),'.'))
