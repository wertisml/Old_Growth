# function to classify MOG for a single community type
# Finds the plots with the correct dia of trees and number of trees
n_big_tpa <- function(tree, big_tree_size){ 
  
  tpa <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(TPA_UNADJ)
  live <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[live >= big_tree_size], 0), na.rm = TRUE)
  
}

# function to classify OG for Region 8
classify_mog <- function(idx, tree, ccc){
  
  Stand_Age <- defs$Stand_Age[idx]
  Stand_Basal_Area <- defs$Stand_Basal_Area[idx]
  Large_Tree_Diameter <- defs$Large_Tree_Diameter[idx]
  Trees_Per_Acre <- defs$Trees_Per_Acre[idx]
  Dead_Trees_Per_Acre <- defs$Dead_Trees_Per_Acre[idx]
  OG_Type <- defs$OG_Type[idx]
  
  tree %>% 
    summarise( # Determines the diameter of the tree and how many big trees per acre there are 
      num_big_tree_per_acre = n_big_tpa(tree=., big_tree_size = defs$Large_Tree_Diameter[idx])
    ) %>% 
    left_join(tree %>% 
                filter(DIA >= 5, Status == "Live") %>% 
                mutate(BAA = BA * TPA_UNADJ) %>%
                group_by(cuid) %>%
                summarise(BAA = sum(BAA)),
              by='cuid') %>%
    left_join(x=ccc,y=., by='cuid') %>% 
    mutate(# Calculates the stand age
      class_SA = fifelse(( STDAGE + (2023-MEASYEAR) > Stand_Age ), TRUE,  FALSE, na=FALSE),
      # need to apply conversions to basal area to convert from m2/ha to ft2/acre
      # Needs to pass having above the threshold of large trees per acre and a basal area per acre
      # Over our threshold. which answers our Stand Basal Area question. 
      class_SBA = fifelse(( BAA > Stand_Basal_Area ), TRUE, FALSE, na=FALSE),
      # are the number of big trees over what we need for OG, will be used later on
      class_LTD = fifelse(( num_big_tree_per_acre >= Trees_Per_Acre ),TRUE, FALSE, na=FALSE),
      # need to apply conversions to basal area to convert from m2/ha to ft2/acre
      class_DTPA = fifelse(( TPA_DEAD > Dead_Trees_Per_Acre ), TRUE, FALSE, na=FALSE),
      Age = STDAGE + (2023-MEASYEAR), 
      Stand_Basal_Area = BAA,
      Dead_Trees_Per_Acre = TPA_DEAD,
      community_abb = OG_Type
      ) %>% 
    dplyr::select(cuid, contains('class'), num_big_tree_per_acre, Age, Stand_Basal_Area,BAA, Dead_Trees_Per_Acre,
                  contains('community')) %>%
    ungroup()
  
}


