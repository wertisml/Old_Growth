QMD <- function(tree, QMD_Diameter){ 
  
  tree %>% filter(Status == "Live", DIA >= QMD_Diameter) %>% ungroup() %>% 
    mutate(QMD = sqrt((DIA/TPA_UNADJ)/0.005454)) %>%
    select(QMD, num)
}

SDI <- function(tree, SDI_Diameter){ 
  
  SDI_18 <- tree %>% filter(Status == "Live", DIA >= SDI_Diameter) %>% ungroup() %>% 
    mutate(QMD = sqrt((DIA/TPA_UNADJ)/0.005454),
           SDI_18 = TPA_UNADJ * (QMD/18)**-1.605) %>%
    select(SDI_18, num)
  
  SDI_10 <- tree %>% filter(Status == "Live", DIA >= 10, num %in% SDI_18$num) %>% ungroup() %>% 
    mutate(QMD = sqrt((DIA/TPA_UNADJ)/0.005454),
           SDI_10 = TPA_UNADJ * (QMD/10)**-1.605) %>%
    select(SDI_10, num) 
  
  SDI_18 %>% 
    left_join(SDI_10) %>%
    mutate(SDI = SDI_18/SDI_10) %>%
    select(SDI, num)
  
}  

# function to classify OG for Region 8
classify_mog <- function(idx, tree, ccc){
  
  SDI_Percent <- defs$SDI[idx]
  SDI_Diameter <- defs$SDI_Diameter[idx]
  QMD_Diameter <- defs$QMD_Diameter[idx]
  QMD_Count <- defs$QMD_Count[idx]
  Habitat_Type_Codes <- defs$Habitat_Type_Codes[idx]
  OG_Type <- defs$OG_Type[idx]
  
  tree <- tree %>% mutate(num = row_number())
  
  tree %>% 
    left_join(QMD(tree, QMD_Diameter), by = join_by(num)) %>%
    left_join(SDI(tree, SDI_Diameter), by = join_by(num))%>% 
    mutate(class_SDI = fifelse(is.na(SDI_Percent) | SDI > SDI_Percent, TRUE,  FALSE, na=FALSE),
           class_QMD = fifelse(is.na(QMD_Count) | QMD > QMD_Count, TRUE, FALSE, na=FALSE)) %>%
    left_join(x=ccc,y=., by='cuid') %>%  
    mutate(# Calculates the stand age
      class_HTC = fifelse(as.numeric(HABTYPCD1) %in% unlist(strsplit(Habitat_Type_Codes, ",\\s*")), TRUE, FALSE, na = FALSE),
      community_abb = OG_Type
    ) %>% 
    dplyr::select(cuid, contains('class'), QMD, SDI,
                  contains('community')) %>%
    ungroup()
  
}
