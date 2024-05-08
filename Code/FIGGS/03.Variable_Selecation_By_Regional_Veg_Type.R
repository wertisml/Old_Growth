#------------ VARIABLE SELECTION BY REGIONAL-VEG TYPE ----------------------#

# book keeping
print('VARIABLE SELECTION BY REGIONAL VEGETATION TYPE.')

# set up list to hold stuff
veg.var.list <- list()

# loop through data and do stuff
for (i in 1:length(unique(fia.data$Vegtype))) {
  
  # pull out forest type group
  fia.sp <- fia.data[fia.data$Vegtype == unique(fia.data$Vegtype)[i],]
  
  # find variables that are most correlated to stand age -- use code from other R script to do variable selection
  fia.corr <- fia.sp %>% select(Stand_Age, tpadom, badom, ddiscore, QMDdom, HTquart, HTsd, snagbatot)
  
  # set up matrix
  # matrix to hold stuff
  fia.var <- matrix(0, ncol=1,nrow=8)
  fia.var <- as.data.frame(fia.var)
  fia.var$GROUP <- "NA"
  fia.var$VARIABLE <- "NA"
  fia.var$COR <- 0
  fia.var$NUM.PLOTS <- 0
  
  if (nrow(fia.sp) >= 10) {
    
    # set up the model and reduce the predictor variables
    fia.cor <- rem.cors(fia.corr, 0.7)
    cor.test <- as.data.frame(cor(na.omit(fia.cor))[,1])
    names(cor.test) <- 'cor'
    cor.test$var <- rownames(cor.test)
    cor.test.p <- na.omit(cor.test[cor.test$cor >= 0.20,])
    cor.test.n <- na.omit(cor.test[cor.test$cor <= -0.20,])
    cor.test <- rbind(cor.test.p, cor.test.n)
    nr <- nrow(cor.test)
    
    # check to make sure there are data in the correlation matrix
    if (nr > 0){
      # add data to matrix
      fia.var$GROUP[1:nr] <- unique(fia.sp$Vegtype)
      fia.var$VARIABLE[1:nr] <- cor.test$var
      fia.var$COR[1:nr] <- round(cor.test$cor,2)
      fia.var$NUM.PLOTS[1:nr] <- nrow(fia.sp)
      
      # clean up data
      fia.var <- fia.var[fia.var$VARIABLE != 'STDAGE',]
      fia.var <- fia.var[,c(2:5)]
      
      # add data to list
      veg.var.list[[i]] <- fia.var[fia.var$NUM.PLOTS > 0,]
    } 
    
    if (nr == 0) {
      
      # add data to matrix
      fia.var$GROUP[1] <- unique(fia.sp$Vegtype)
      fia.var$VARIABLE[1] <- NA
      fia.var$COR[1] <- NA
      fia.var$NUM.PLOTS[1] <- nrow(fia.sp)
      
      # clean up data
      fia.var <- fia.var[c(1),c(2:5)]
      
      # add data to list
      veg.var.list[[i]] <- fia.var
      
    }
  }
  
  # check if there enough rows to do anything
  if (nrow(fia.sp) < 10) {
    
    # add data to matrix
    fia.var$GROUP[1] <- unique(fia.sp$Vegtype)
    fia.var$VARIABLE[1] <- 'NA'
    fia.var$COR[1] <- 'NA'
    fia.var$NUM.PLOTS[1] <- nrow(fia.sp)
    
    # add data to list
    veg.var.list[[i]] <- fia.var[,c(2:5)]
  } 
  print(i)
}

# combine list 
veg.var.final <- do.call("rbind", veg.var.list)

# clean up data 
veg.var.final <- veg.var.final[veg.var.final$GROUP != 'NA',]

# write data to csv
write.csv(veg.var.final, 
          paste0(out.directory, "FIA_VEG_VAR_FINAL_", ".csv"))

