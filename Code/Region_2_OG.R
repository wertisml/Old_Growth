# function to classify MOG for a single community type
# Finds the plots with the correct dia of trees and number of trees
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

n_cull_tpa <- function(tree, Cull_Count){
  
  fifelse(sum(tree$TPA_UNADJ[sum(tree$CULL > 0, na.rm=T)]) > Cull_Count, 1, 0) 
}

# function to classify OG for Region 2
classify_mog <- function(idx, tree, ccc){
  
  Stand_Age <- defs$Stand_Age[idx]
  Trees_Per_Acre <- defs$Trees_Per_Acre[idx]
  Cull_or_Broken_Top_Per_Acre <- defs$Cull_or_Broken_Top_Per_Acre[idx]
  Dead_Trees_Per_Acre <- defs$Dead_Trees_Per_Acre[idx]
  Dead_Tree_Diameter <- defs$Dead_Tree_Diameter[idx]
  OG_Type <- defs$OG_Type[idx]

  tree %>% 
    summarise( # Determines the diameter of the tree and how many big trees per acre there are 
      num_big_tree_per_acre = n_big_tpa(tree=., big_tree_size = defs$Large_Tree_Diameter[idx]),
      num_big_dead_tree_per_acre = n_big_dtpa(tree=., dead_tree_size = defs$Dead_Tree_Diameter[idx]),
      cull_over_threshold = n_cull_tpa(tree=., Cull_Count = defs$Cull_or_Broken_Top_Per_Acre[idx])
      #Age = max(TOTAGE)
    ) %>% 
    left_join(x=ccc,y=., by='cuid') %>%  
    mutate(# Calculates the stand age
      class_SA = fifelse(( STDAGE + (2023-MEASYEAR) >= Stand_Age ), TRUE,  FALSE),
      #class_SA = fifelse(( max((STDAGE + (2023-MEASYEAR)), Age, na.rm=T) >= Stand_Age ), TRUE,  FALSE),
      # need to apply conversions to basal area to convert from m2/ha to ft2/acre
      # Needs to pass having above the threshold of large trees per acre and a basal area per acre
      # Over our threshold. which answers our Stand Basal Area question. 
      class_C = fifelse(( cull_over_threshold >= Cull_or_Broken_Top_Per_Acre ), TRUE, FALSE),
      # are the number of large tree diameters over what we need for OG
      class_LTD = fifelse(( num_big_tree_per_acre >= Trees_Per_Acre ),TRUE, FALSE),
      # are the number of large trees over what we need for OG
      class_NLT = fifelse(( num_big_tree_per_acre >= Trees_Per_Acre ),TRUE, FALSE),
      # need to apply conversions to basal area to convert from m2/ha to ft2/acre
      class_DTPA = fifelse(( num_big_dead_tree_per_acre >= Dead_Trees_Per_Acre), TRUE, FALSE),
      Age = STDAGE + (2023-MEASYEAR), 
      Cull_Per_Acre = cull_over_threshold,
      Dead_Trees_Per_Acre = num_big_dead_tree_per_acre,
      community_abb = OG_Type ) %>% 
    dplyr::select(cuid, contains('class'), num_big_tree_per_acre, Age, Cull_Per_Acre, Dead_Trees_Per_Acre,
                  contains('community')) %>%
    ungroup()
  
}


