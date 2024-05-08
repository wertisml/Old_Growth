#------------ VARIABLE SELECTION BY FTG -----------------------------------------------#

# book keeping
print('VARIABLE SELECTION BY FOREST TYPE GROUP.')

# set up list to hold stuff
ftg.var.list <- list()

# loop through data and do stuff
for (i in 1:length(unique(fia.data$Vegetation_Type))) {
  
  # pull out forest type group
  fia.sp <- fia.data[fia.data$Vegetation_Type == unique(fia.data$Vegetation_Type)[i],]
  
  # find variables that are most correlated to stand age
  fia.corr <- fia.sp %>% select(Stand_Age, tpadom, badom, ddiscore, QMDdom, HTquart, HTsd, snagbatot)
  
  # set up the model and reduce the predictor variables
  fia.cor <- rem.cors(fia.corr, 0.7)
  cor.test <- as.data.frame(cor(na.omit(fia.cor))[,1])
  names(cor.test) <- 'cor'
  cor.test$var <- rownames(cor.test)
  cor.test.p <- na.omit(cor.test[cor.test$cor >= 0.20,])
  cor.test.n <- na.omit(cor.test[cor.test$cor <= -0.20,])
  cor.test <- rbind(cor.test.p, cor.test.n)
  nr <- nrow(cor.test)
  
  # set up matrix
  # matrix to hold stuff
  fia.var <- matrix(0, ncol=1,nrow=8)
  fia.var <- as.data.frame(fia.var)
  fia.var$GROUP <- "NA"
  fia.var$VARIABLE <- "NA"
  fia.var$COR <- 0
  fia.var$NUM.PLOTS <- 0
  
  # add data to matrix
  fia.var$GROUP[1:nr] <- unique(fia.data$Vegetation_Type)[i]
  fia.var$VARIABLE[1:nr] <- cor.test$var
  fia.var$COR[1:nr] <- round(cor.test$cor,2)
  fia.var$NUM.PLOTS[1:nr] <- nrow(fia.sp)
  
  # clean up data
  fia.var <- fia.var[c(2:nr),c(2:5)]
  
  # add data to list
  ftg.var.list[[i]] <- fia.var
  
}

# combine list 
ftg.var.final <- do.call("rbind", ftg.var.list)

# clean up data 
ftg.var.final <- ftg.var.final[ftg.var.final$GROUP != 'NA',]

# write data to csv
write.csv(ftg.var.final, 
          paste0(out.directory, 'FIA_FTG_VAR_FINAL_', ".csv"))

