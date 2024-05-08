#------------ GET MATURE THRESHOLDS FOR EACH VEG GROUP ------------------#

# book keeping
print('MATURE THRESHOLDS BY VEG GROUP.')

# set up dataframe to store everything in
veg.age.cut <- matrix(0, ncol=1,nrow=6000)
veg.age.cut <- as.data.frame(veg.age.cut)
veg.age.cut$GROUP <- "NA"
veg.age.cut$VARIABLE <- "NA"
veg.age.cut$COR <- 0
veg.age.cut$OG.PERC <- 0
veg.age.cut$PERC.100 <- 0
veg.age.cut$PLOTS <- 0

# set counter
q <- 1

# loop through each forest type group and do some stuff 
for (z in 1:length(unique(og.fia$Vegtype))) {
  
  # save just the species name
  sp.name <- unique(og.fia$Vegtype)[z]
  
  # check to see if things match what we need
  if (sp.name == 'R1 Hardwoods') { 
    
    # pull out forest type group
    fia.sp <- og.fia[og.fia$fgroupname == 'Aspen / birch group',]
    
    # find the metrics for that forest type group
    ftg.att <- ftg.var.final[ftg.var.final$GROUP == 'Aspen / birch group',][,2]
    
    # clean up og data to only include the above metrics
    fia.sp1 <- fia.sp[,c(1:12)]
    fia.sp2 <- fia.sp[,ftg.att]
    tree.data <- cbind(fia.sp1, fia.sp2)
    names(tree.data)[13:ncol(tree.data)] <- ftg.att
    
    # loop through each variable and do stuff 
    for (y in 13:ncol(tree.data)) {
      
      # pull out just metric and then filter for NA values 
      tree.age <- na.omit(tree.data[,y])
      
      # add some info the dataframe
      veg.age.cut$GROUP[q] <- sp.name
      veg.age.cut$VARIABLE[q] <- names(tree.data)[y]
      
      # find correlation and add it to matrix
      fia.ftg <- ftg.var.final[ftg.var.final$GROUP == 'Aspen / birch group',]
      
      if (names(tree.data)[y] == "STDAGE") {
        veg.age.cut$COR[q] <- 1
      }
      
      if (names(tree.data)[y] != "STDAGE") {
        
        fia.var <- fia.ftg[fia.ftg$VARIABLE == names(tree.data)[y],]
        veg.age.cut$COR[q] <- fia.var$COR
      }
      
      # find cutoffs and add to dataframe
      veg.age.cut$OG.PERC[q] <- quantile(as.numeric(tree.age), 0.25)
      veg.age.cut$PERC.100[q] <- quantile(as.numeric(tree.age), 1)
      
      # find number of plots and add that too data 
      veg.age.cut$PLOTS[q] <- length(tree.age)
      
      # fix counter
      q <- q+1
    }
  }
  else if (sp.name == 'R2 Ponderosa Pine') {
    
    # pull out forest type group
    fia.sp <- og.fia[og.fia$fgroupname == 'Ponderosa pine group',]
    
    # find the metrics for that forest type group
    ftg.att <- ftg.var.final[ftg.var.final$GROUP == 'Ponderosa pine group',][,2]
    
    # clean up og data to only include the above metrics
    fia.sp1 <- fia.sp[,c(1:12)]
    fia.sp2 <- fia.sp[,ftg.att]
    tree.data <- cbind(fia.sp1, fia.sp2)
    names(tree.data)[13:ncol(tree.data)] <- ftg.att
    
    # loop through each variable and do stuff 
    for (y in 13:ncol(tree.data)) {
      
      # pull out just metric and then filter for NA values 
      tree.age <- na.omit(tree.data[,y])
      
      # add some info the dataframe
      veg.age.cut$GROUP[q] <- sp.name
      veg.age.cut$VARIABLE[q] <- names(tree.data)[y]
      
      # find correlation and add it to matrix
      fia.ftg <- ftg.var.final[ftg.var.final$GROUP == 'Ponderosa pine group',]
      
      if (names(tree.data)[y] == "STDAGE") {
        veg.age.cut$COR[q] <- 1
        
      }
      
      if (names(tree.data)[y] != "STDAGE") {
        fia.var <- fia.ftg[fia.ftg$VARIABLE == names(tree.data)[y],]
        veg.age.cut$COR[q] <- fia.var$COR
      }
      
      # find cutoffs and add to dataframe
      veg.age.cut$OG.PERC[q] <- quantile(as.numeric(tree.age), 0.25)
      veg.age.cut$PERC.100[q] <- quantile(as.numeric(tree.age), 1)
      
      # find number of plots and add that too data 
      veg.age.cut$PLOTS[q] <- length(tree.age)
      
      # fix counter
      q <- q+1
    }
  }
  else if (sp.name == 'R3 Hardwoods') {
    
    # pull out forest type group
    fia.sp <- og.fia[og.fia$fgroupname == 'Woodland hardwoods group',]
    
    # find the metrics for that forest type group
    ftg.att <- ftg.var.final[ftg.var.final$GROUP == 'Woodland hardwoods group',][,2]
    
    # clean up og data to only include the above metrics
    fia.sp1 <- fia.sp[,c(1:12)]
    fia.sp2 <- fia.sp[,ftg.att]
    tree.data <- cbind(fia.sp1, fia.sp2)
    names(tree.data)[13:ncol(tree.data)] <- ftg.att
    
    # loop through each variable and do stuff 
    for (y in 13:ncol(tree.data)) {
      
      # pull out just metric and then filter for NA values 
      tree.age <- na.omit(tree.data[,y])
      
      # add some info the dataframe
      veg.age.cut$GROUP[q] <- sp.name
      veg.age.cut$VARIABLE[q] <- names(tree.data)[y]
      
      # find correlation and add it to matrix
      fia.ftg <- ftg.var.final[ftg.var.final$GROUP == 'Woodland hardwoods group',]
      
      if (names(tree.data)[y] == "STDAGE") {
        veg.age.cut$COR[q] <- 1
      }
      
      if (names(tree.data)[y] != "STDAGE") {
        fia.var <- fia.ftg[fia.ftg$VARIABLE == names(tree.data)[y],]
        veg.age.cut$COR[q] <- fia.var$COR
      }
      
      # find cutoffs and add to dataframe
      veg.age.cut$OG.PERC[q] <- quantile(as.numeric(tree.age), 0.25)
      veg.age.cut$PERC.100[q] <- quantile(as.numeric(tree.age), 1)
      
      # find number of plots and add that too data 
      veg.age.cut$PLOTS[q] <- length(tree.age)
      
      # fix counter
      q <- q+1
    }
  }
  else if (sp.name == 'R4 Elm / ash / cottonwood') {
    
    # pull out forest type group
    fia.sp <- og.fia[og.fia$fgroupname == 'Elm / ash / cottonwood group',]
    
    # find the metrics for that forest type group
    ftg.att <- ftg.var.final[ftg.var.final$GROUP == 'Elm / ash / cottonwood group',][,2]
    
    # clean up og data to only include the above metrics
    fia.sp1 <- fia.sp[,c(1:12)]
    fia.sp2 <- fia.sp[,ftg.att]
    tree.data <- cbind(fia.sp1, fia.sp2)
    names(tree.data)[13:ncol(tree.data)] <- ftg.att
    
    # loop through each variable and do stuff 
    for (y in 13:ncol(tree.data)) {
      
      # pull out just metric and then filter for NA values 
      tree.age <- na.omit(tree.data[,y])
      
      # add some info the dataframe
      veg.age.cut$GROUP[q] <- sp.name
      veg.age.cut$VARIABLE[q] <- names(tree.data)[y]
      
      # find correlation and add it to matrix
      fia.ftg <- ftg.var.final[ftg.var.final$GROUP == 'Elm / ash / cottonwood group',]
      
      if (names(tree.data)[y] == "STDAGE") {
        veg.age.cut$COR[q] <- 1
      }
      
      if (names(tree.data)[y] != "STDAGE") {
        fia.var <- fia.ftg[fia.ftg$VARIABLE == names(tree.data)[y],]
        veg.age.cut$COR[q] <- fia.var$COR
      }
      
      # find cutoffs and add to dataframe
      veg.age.cut$OG.PERC[q] <- quantile(as.numeric(tree.age), 0.25)
      veg.age.cut$PERC.100[q] <- quantile(as.numeric(tree.age), 1)
      
      # find number of plots and add that too data 
      veg.age.cut$PLOTS[q] <- length(tree.age)
      
      # fix counter
      q <- q+1
    }
  }
  else if (sp.name == 'R5 R6 Hardwoods') {
    
    # pull out forest type group
    fia.sp <- og.fia[og.fia$fgroupname == 'Western oak group',]
    
    # find the metrics for that forest type group
    ftg.att <- ftg.var.final[ftg.var.final$GROUP == 'Western oak group',][,2]
    
    # clean up og data to only include the above metrics
    fia.sp1 <- fia.sp[,c(1:12)]
    fia.sp2 <- fia.sp[,ftg.att]
    tree.data <- cbind(fia.sp1, fia.sp2)
    names(tree.data)[13:ncol(tree.data)] <- ftg.att
    
    # loop through each variable and do stuff 
    for (y in 13:ncol(tree.data)) {
      
      # pull out just metric and then filter for NA values 
      tree.age <- na.omit(tree.data[,y])
      
      # add some info the dataframe
      veg.age.cut$GROUP[q] <- sp.name
      veg.age.cut$VARIABLE[q] <- names(tree.data)[y]
      
      # find correlation and add it to matrix
      fia.ftg <- ftg.var.final[ftg.var.final$GROUP == 'Western oak group',]
      
      if (names(tree.data)[y] == "STDAGE") {
        veg.age.cut$COR[q] <- 1
      }
      
      if (names(tree.data)[y] != "STDAGE") {
        fia.var <- fia.ftg[fia.ftg$VARIABLE == names(tree.data)[y],]
        veg.age.cut$COR[q] <- fia.var$COR
      }
      
      # find cutoffs and add to dataframe
      veg.age.cut$OG.PERC[q] <- quantile(as.numeric(tree.age), 0.25)
      veg.age.cut$PERC.100[q] <- quantile(as.numeric(tree.age), 1)
      
      # find number of plots and add that too data 
      veg.age.cut$PLOTS[q] <- length(tree.age)
      
      # fix counter
      q <- q+1
    }
  }
  else if (sp.name == 'R8 Wet and rain forest') {
    
    # pull out forest type group
    fia.sp <- og.fia[og.fia$fgroupname == 'Tropical hardwoods group',]
    
    # find the metrics for that forest type group
    ftg.att <- ftg.var.final[ftg.var.final$GROUP == 'Tropical hardwoods group',][,2]
    
    # clean up og data to only include the above metrics
    fia.sp1 <- fia.sp[,c(1:12)]
    fia.sp2 <- fia.sp[,ftg.att]
    tree.data <- cbind(fia.sp1, fia.sp2)
    names(tree.data)[13:ncol(tree.data)] <- ftg.att
    
    # there are not enough plots in this group to calculate mature plots so all categories
    # are assigned NA
    veg.age.cut$GROUP[q] <- sp.name
    
    veg.age.cut$VARIABLE[q] <- NA
    veg.age.cut$COR[q] <- NA
    veg.age.cut$OG.PERC[q] <- NA
    veg.age.cut$PERC.100[q] <- NA
    veg.age.cut$PLOTS[q] <- nrow(tree.data)
    
    # fix counter
    q <- q+1
  }
  else {
    
    # find the metrics for that forest type group
    ftg.att <- veg.var.final[veg.var.final$GROUP == sp.name,][,2]
    
    # pull out forest type group
    fia.sp <- og.fia[og.fia$Vegtype == sp.name,]
    
    # clean up og data to only include the above metrics
    fia.sp1 <- fia.sp[,c(1:12)]
    fia.sp2 <- fia.sp[,ftg.att]
    tree.data <- cbind(fia.sp1, fia.sp2)
    names(tree.data)[13:ncol(tree.data)] <- ftg.att
    
    # loop through each variable and do stuff 
    for (y in 13:ncol(tree.data)) {
      
      # add some info the dataframe
      veg.age.cut$GROUP[q] <- sp.name
      veg.age.cut$VARIABLE[q] <- names(tree.data)[y]
      
      # find correlation and add it to matrix
      fia.ftg <- veg.var.final[veg.var.final$GROUP ==  sp.name,]
      
      fia.var <- fia.ftg[fia.ftg$VARIABLE == names(tree.data)[y],]
      veg.age.cut$COR[q] <- fia.var$COR
      
      # find cutoffs and add to dataframe
      veg.age.cut$OG.PERC[q] <- quantile(na.omit(as.numeric(tree.data[,y])), 0.25)
      veg.age.cut$PERC.100[q] <- quantile(na.omit(as.numeric(tree.data[,y])), 1)
      
      # find number of plots and add that too data 
      veg.age.cut$PLOTS[q] <- length(na.omit(tree.data[,y]))
      
      # fix counter
      q <- q+1
    }
  }
  # bookkeeping
  print(z)
}

# clean up veg data 
veg.age.cut <- veg.age.cut[veg.age.cut$GROUP != 'NA',]

# list to store stuff in
veg.walkdown.list <- list()

# loop through vegetation types and find walk down factor 
for (w in 1:length(unique(veg.age.cut$GROUP))) {
  
  # find first unique veg type
  veg.type <- unique(veg.age.cut$GROUP)[w]
  
  # pull out all FIA types for this veg type 
  fia.types <- og.fia[og.fia$Vegtype == veg.type,]
  fia.types <- unique(fia.types$Vegetation_Type)
  
  # find mean walkdown coefficient
  group.walkdown <- walkdown.csv[walkdown.csv$Vegetation.Type %in% fia.types,]
  group.coef <- round(mean(group.walkdown$Walkdown.Factor),2)
  
  # pull out veg.age.cut data 
  veg.cut.wd <- veg.age.cut[veg.age.cut$GROUP == veg.type,]
  veg.cut.wd$WALKDOWN <- group.coef
  
  # add to list 
  veg.walkdown.list[[w]] <- veg.cut.wd
}

# combine all data together 
veg.age.cut <- do.call(rbind, veg.walkdown.list)

# split data into positive correlation and negative correlation datasets
pos.cor <- as.data.frame(veg.age.cut[veg.age.cut$COR >= 0,])
neg.cor <- as.data.frame(veg.age.cut[veg.age.cut$COR < 0,])

# keep only complete values 
pos.cor <- pos.cor[pos.cor$OG.PERC > 0,]

neg.cor <- neg.cor[neg.cor$OG.PERC > 0,]

# calculate mature threshold for each dataset 
# you may get warning message about NAs introduced by coercion
# this is because there are NA values in the OG.PERC because we did not calculate them
# due to not having at least 10 plots with those values and MATURE.CUT is a numeric class
# https://www.statology.org/nas-introduced-by-coercion-in-r/
pos.cor$MATURE.CUT <- (pos.cor$OG.PERC/100)*(pos.cor$WALKDOWN * 100)
neg.cor$MATURE.CUT <- (neg.cor$OG.PERC/(neg.cor$WALKDOWN * 100)) * 100

# reattach values
veg.age.cut <- rbind(pos.cor, neg.cor)

# clean up veg data 
veg.age.cut <- veg.age.cut[veg.age.cut$GROUP != 'NA',]

# round values to make life easier
veg.age.cut$OG.PERC <- round(as.numeric(veg.age.cut$OG.PERC), 2)
veg.age.cut$PERC.100 <- round(as.numeric(veg.age.cut$PERC.100), 2)
veg.age.cut$MATURE.CUT <- round(as.numeric(veg.age.cut$MATURE.CUT), 2)

# write to disc
write.csv(veg.age.cut, 
          paste0(out.directory, 'FIA_VEG_THRESHOLDS_', ".csv"))
