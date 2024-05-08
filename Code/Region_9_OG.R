# function to classify MOG for a single community type
# Finds the plots with the correct dia of trees and number of trees
n_big_tpa <- function(tree, big_tree_size){ 
  
  tpa <- tree %>% filter(Status == "Live") %>% ungroup() %>% mutate(TPA_ADJ = TPA_UNADJ/CONDPROP_UNADJ) %>% select(TPA_ADJ)
  live <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[live >= big_tree_size], 0), na.rm = TRUE)
  
}

# function to classify OG for Region 9
classify_mog <- function(idx, tree, ccc){
  
  Stand_Age <- defs$Stand_Age[idx]
  Trees_Per_Acre <- defs$Trees_Per_Acre[idx]
  Large_Tree_Diameter <- defs$Large_Tree_Diameter[idx]
  OG_Type <- defs$OG_Type[idx]
  
  tree %>% 
    summarise( # Determines the diameter of the tree and how many big trees per acre there are 
      num_big_tree_per_acre = n_big_tpa(tree=., big_tree_size = Large_Tree_Diameter)
    ) %>% 
    left_join(x=ccc,y=., by='cuid') %>%  
    mutate(# Calculates the stand age
      class_SA = fifelse(( STDAGE + (2023-MEASYEAR) > Stand_Age ), TRUE,  FALSE),
      # Do we have the correct number of live large tree diameters?
      class_LTD = fifelse(( num_big_tree_per_acre > Trees_Per_Acre ),TRUE, FALSE),
      # are the number of live big trees over what we need for OG?
      class_TPA = fifelse(( num_big_tree_per_acre > Trees_Per_Acre ), TRUE, FALSE),
      Age = STDAGE + (2023-MEASYEAR), 
      Trees_Per_Acre = num_big_tree_per_acre,
      community_abb = OG_Type ) %>% 
    dplyr::select(cuid, contains('class'), Age, Trees_Per_Acre,
                  contains('community')) %>%
    ungroup()
  
}
