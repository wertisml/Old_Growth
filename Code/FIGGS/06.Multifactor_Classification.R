#------------ MULTIFACTOR CLASSIFICATION ---------------------------------------------#

# book keeping
print('RUNNING MULTIFACTOR CLASSIFICATION.')

# create new dataframe
fia.mf.class <- not.og.fia 
fia.mf.weight <- NULL

# start doing classification
for (i in 1:nrow(fia.mf.class)) {
  
  # subset fia data 
  fia.sub <- not.og.fia[i,]
  
  # subset age cut data
  veg.age.sub <- subset(veg.age.cut, subset = GROUP == fia.sub$Vegtype)
  
  # check if there is data 
  if (nrow(na.omit(veg.age.sub)) > 0) {
    
    # calculate correlation weighting factor
    veg.age.sub$cor.abs <- abs(as.numeric(veg.age.sub$COR))
    cor.fact <- sum(veg.age.sub$cor.abs)
    veg.age.sub$cor.weight <- round(veg.age.sub$cor.abs / cor.fact, 2)
    
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
  #fia.mf.weight <- rbind(fia.mf.weight, veg.age.sub)
  
  # if no data then do this
  if (nrow(na.omit(veg.age.sub)) == 0) {
    
    fia.mf.class$FINAL.CLASS[i] <- NA
    
  }
}

# make sure plot_cn are numeric
head(fia.mf.class)
#fia.mf.class$PLT_CN <- as.numeric(fia.mf.class$PLT_CN)

# write to disc
write.csv(fia.mf.class, 
          paste0(out.directory, "FIGSS_CLASSIFICATION_",".csv"))

Weights <- fia.mf.weight %>% 
  select(GROUP, WALKDOWN, VARIABLE, COR, MATURE.CUT, cor.weight) %>%
  rename(`Vegetation Class` = GROUP,
         Walkdown = WALKDOWN,
         Indicators = VARIABLE,
         Correlation = COR,
         Threshold = MATURE.CUT,
         Weight = cor.weight) %>%
  distinct(.keep_all = T)

write_csv(Weights, "Weights_Matrix.csv")