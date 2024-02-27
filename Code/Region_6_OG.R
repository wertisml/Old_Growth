n_big_tpa <- function(tree, big_tree_size){ 
  
  tpa <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(TPA_UNADJ)
  live <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[live >= big_tree_size], 0), na.rm = TRUE)
  
}

n_big_dtpa <- function(tree, dead_tree_size){ 
  
  tpa <- tree %>% filter(Status == "Dead") %>% ungroup() %>% select(TPA_UNADJ)
  dead <- tree %>% filter(Status == "Dead") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[dead >= dead_tree_size], 0), na.rm = TRUE)
  
}

n_big_downed_tpa <- function(tree, downed_tree_size){ 
  
  tpa <- tree %>% filter(Downed_Dead == "Downed") %>% ungroup() %>% select(TPA_UNADJ)
  dead <- tree %>% filter(Downed_Dead == "Downed") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[dead >= downed_tree_size], 0), na.rm = TRUE)
  
}

DDI <- function(tree){
  
  # Density index
  TPH_C0 <- tree %>% filter(Status == "Live", DIA > 2, DIA < 10) %>% ungroup() %>% select(TPA_UNADJ) %>% summarise(TPA = sum(TPA_UNADJ))
  TPH_C1 <- tree %>% filter(Status == "Live", DIA >= 10, DIA < 20) %>% ungroup() %>% select(TPA_UNADJ) %>% summarise(TPA = sum(TPA_UNADJ))
  TPH_C2 <- tree %>% filter(Status == "Live", DIA >= 20, DIA < 40) %>% ungroup() %>% select(TPA_UNADJ) %>% summarise(TPA = sum(TPA_UNADJ))
  TPH_C3 <- tree %>% filter(Status == "Live", DIA >= 40) %>% ungroup() %>% select(TPA_UNADJ) %>% summarise(TPA = sum(TPA_UNADJ))
  
  Index_0 <- fifelse(TPH_C0$TPA < 200, 0.005 * TPH_C0$TPA, 1.0)
  Index_1 <- fifelse(TPH_C1$TPA < 75, 0.013333 * TPH_C1$TPA, 1.0)
  Index_2 <- fifelse(TPH_C2$TPA < 40, 0.025 * TPH_C2$TPA, 1.0)
  Index_3 <- fifelse(TPH_C3$TPA < 300, 0.03333 * TPH_C3$TPA, 1.0)
  
  DDI = (1 * Index_0) + (2 * Index_1) + (3 * Index_2) + (4 * Index_3)
}

classify_mog <- function(idx, tree, ccc){
  
  Stand_Age <- defs$Stand_Age[idx]
  Trees_Per_Acre <- defs$Tree_Per_Acre[idx]
  Large_Tree_Diameter <- defs$Large_Tree_Diameter[idx]
  Dead_Tree_Per_Acre <- defs$Dead_Tree_Per_Acre[idx]
  Dead_Tree_Diameter <- defs$Dead_Tree_Diameter[idx]
  Downed_Trees_Per_Acre <- defs$Downed_Trees_Per_Acre[idx]
  Downed_Tree_Diameter <- defs$Downed_Tree_Diameter[idx]
  Diameter_Diversity_Index <- defs$Diameter_Diversity_Index[idx]
  Site <- defs$Site[idx]
  OG_Type <- defs$OG_Type[idx]
  
  tree %>% 
    summarise( # Determines the diameter of the tree and how many big trees per acre there are 
      num_big_TPA = n_big_tpa(tree=., big_tree_size = defs$Large_Tree_Diameter[idx]),
      num_big_DTPA = n_big_dtpa(tree=., dead_tree_size = defs$Dead_Tree_Diameter[idx]),
      num_big_downed_TPA = n_big_downed_tpa(tree=., downed_tree_size = defs$Downed_Trees_Per_Acre[idx]),
      Diameter_Diversity_Index = DDI(tree=.)
    ) %>% 
    mutate(# Downed trees per acre
      class_Downed_TPA = fifelse(is.na(Downed_Trees_Per_Acre) | num_big_downed_TPA > Downed_Trees_Per_Acre, TRUE, FALSE, na=F),
      class_DTPA = fifelse(is.na(Dead_Tree_Per_Acre) | num_big_DTPA > Dead_Tree_Per_Acre, TRUE, FALSE, na=F)) %>%
    left_join(x=ccc,y=., by='cuid') %>%  
    mutate(# Calculates the stand age
      class_SA = fifelse(((STDAGE + 2023-MEASYEAR) > Stand_Age), TRUE, FALSE, na = FALSE),
      # are the number of live big trees over what we need for OG?
      class_TPA = fifelse(( num_big_TPA > Trees_Per_Acre ), TRUE, FALSE, na=F),
      # Do we have the correct number of live large tree diameters?
      class_LTD = fifelse(( num_big_TPA > Trees_Per_Acre ),TRUE, FALSE, na=F),
      # site = fifelse(SITECLCD <= 2, "H",
      #                fifelse(SITECLCD %in% c(3,4), "M", "L")),
      class_Site = fifelse(is.na(Site) |SITECLCD %in% as.numeric(unlist(strsplit(Site, ",\\s*"))), TRUE, FALSE, na=FALSE),
      Age = STDAGE + (2023-MEASYEAR), 
      Trees_Per_Acre = num_big_TPA,
      community_abb = OG_Type ) %>% 
    dplyr::select(cuid, contains('class'), Age, Trees_Per_Acre, SITECLCD, Diameter_Diversity_Index,
                  contains('community')) %>%
    ungroup()
  
}

