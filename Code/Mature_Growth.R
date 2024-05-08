
snagbatot <- function(tree){
  
  tpa_dead <- tree %>% filter(DIA >= 5, Downed_Dead == "Standing") %>% 
    mutate(BAA = BA * (TPA_UNADJ/CONDPROP_UNADJ))
  
  round(sum(coalesce(tpa_dead$BAA, 0), na.rm = TRUE), 2)
  
}

tpa <- function(tree){ 
  
  tpa <- tree %>% filter(Status == "Live", DIA >= 1, CCLCD %in% c(1,2,3)) %>% ungroup() %>% mutate(TPA_ADJ = TPA_UNADJ/ CONDPROP_UNADJ) %>% select(TPA_ADJ)
  
  sum(coalesce((tpa$TPA_ADJ), 0), na.rm = TRUE)
  
}

DDI <- function(tree){
  
  data <- tree %>%
    mutate(
      dbh_class = cut(DIA, breaks = c(2, 9.8, 19.7, 39.4, 200)),
      TPA_ADJ = TPA_UNADJ/CONDPROP_UNADJ
    ) %>%
    summarise(
      class0 = sum(TPA_ADJ[as.numeric(dbh_class) == 1], na.rm=T) * 2.47,
      class1 = sum(TPA_ADJ[as.numeric(dbh_class) == 2], na.rm=T) * 2.47,
      class2 = sum(TPA_ADJ[as.numeric(dbh_class) == 3], na.rm=T) * 2.47,
      class3 = sum(TPA_ADJ[as.numeric(dbh_class) == 4], na.rm=T) * 2.47
    ) %>%
    mutate(
      index_0 = case_when(
        class0 < 200 ~ 0.005 * class0,
        TRUE ~ 1
      ),
      index_1 = case_when(
        class1 < 75 ~ 0.01333 * class1,
        TRUE ~ 1
      ),
      index_2 = case_when(
        class2 < 40 ~ 0.025 * class2,
        TRUE ~ 1
      ),
      index_3 = case_when(
        class3 < 30 ~ 0.03333 * class3,
        TRUE ~ 1
      ),
      DDI = (1 * index_0) + (2 * index_1) + (3 * index_2) + (4 * index_3)
    ) %>% 
    ungroup() %>%
    select(DDI)
  
  mean(data$DDI, na.rm=T)
}

HTquart <- function(tree){ 
  
  HT <- tree %>% filter(Status == "Live") %>% ungroup() %>%
    select(HT, TPA_UNADJ, CONDPROP_UNADJ) %>%
    mutate(TPA_ADJ = TPA_UNADJ/CONDPROP_UNADJ) %>% drop_na() 
  
  ht_list <- Map(function(ht, tpa) rep(ht, tpa), HT$HT, HT$TPA_ADJ) %>% unlist()

  mean(coalesce(ht_list[ht_list >= quantile(ht_list, 0.75, na.rm=TRUE)], 0), na.rm = TRUE)
  
}

HTsd <- function(tree){ 
  
  HT <- tree %>% filter(Status == "Live") %>% ungroup() %>%
    select(HT, TPA_UNADJ, CONDPROP_UNADJ) %>%
    mutate(TPA_ADJ = TPA_UNADJ/CONDPROP_UNADJ) %>% drop_na() 
  
  ht_list <- Map(function(ht, tpa) rep(ht, tpa), HT$HT, HT$TPA_ADJ) %>% unlist() 
  
  sd(ht_list, na.rm = TRUE)
  
}

Classify_Mature <- function(mature_age, tree, ccc){

  Mature_Age <- age$`Mature Age`[mature_age]
  Group <- age$`Vegetation Type`[mature_age]
  Vegtype <- age$Vegtype[mature_age]
  
  tree %>% 
    summarise( 
      tpadom = tpa(tree),
      ddiscore = DDI(tree) * 10, # trying to see if this helps with the ddiscore
      HTquart = HTquart(tree),
      HTsd = HTsd(tree),
      snagbatot = snagbatot(tree)
      ) %>%
    left_join(tree %>% 
                filter(DIA >= 1, 
                       Status == "Live",
                       CCLCD %in% c(1,2,3)) %>% 
                mutate(BAA = BA * (TPA_UNADJ/CONDPROP_UNADJ)) %>%
                group_by(cuid) %>%
                summarise(badom = sum(BAA, na.rm =T)),
              by='cuid') %>%
    left_join(tree %>% 
                filter(DIA >= 1, 
                       Status == "Live",
                       CCLCD %in% c(1,2,3)) %>% 
                mutate(TPA_ADJ = TPA_UNADJ/CONDPROP_UNADJ,
                       BAA = BA * (TPA_ADJ),
                       QMD = sqrt((sum(BAA)/sum(TPA_ADJ))/0.005454)) %>%
                summarise(QMDdom = mean(QMD, na.rm = TRUE)),
              by ='cuid') %>%
    left_join(ccc %>%
                mutate(Stand_Age = STDAGE + (2023-MEASYEAR)) %>%
                select(cuid, Stand_Age),
              join_by(cuid)) %>%
    mutate(Vegetation_Type = Group,
           Vegtype = Vegtype)
    
}
