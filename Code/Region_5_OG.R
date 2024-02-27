n_big_tpa <- function(tree, big_tree_size){ 
  
  tpa <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(TPA_UNADJ)
  live <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[live >= big_tree_size], 0), na.rm = TRUE)
  
}

classify_mog <- function(idx, tree, ccc){
  
  Stand_Age <- defs$Age[idx]
  Site_Productivity <- defs$Site[idx]
  Trees_Per_Acre <- defs$Trees_Per_Acre[idx]
  Large_Tree_Diameter <- defs$Large_Tree_Diameter[idx]
  Habitat_Type_Group <- defs$ECOSUBCD[idx]
  OG_Type <- defs$OG_Type[idx]
  
  tree %>% 
    summarise( # Determines the diameter of the tree and how many big trees per acre there are 
      num_big_tree_per_acre = n_big_tpa(tree=., big_tree_size = Large_Tree_Diameter),
      site_height = mean(HT, na.rm=T)
    ) %>% 
    left_join(x=ccc,y=., by='cuid') %>% 
    mutate(# Calculates the stand age
      class_SA = fifelse(( STDAGE + (2023-MEASYEAR) > Stand_Age ), TRUE,  FALSE, na=FALSE),
      # Do we have the correct number of live large tree diameters?
      class_LTD = fifelse(( num_big_tree_per_acre > Trees_Per_Acre ),TRUE, FALSE, na=FALSE),
      # Stand basal area
      site_productivity_num = site_height * (0.25489 + (29.377 / (STDAGE + (2023-MEASYEAR)))),
      site_productivity = fifelse(site_productivity_num < 45, "Low", "Productive"),
      class_SP = fifelse(( site_productivity == Site_Productivity ), TRUE, FALSE, na=FALSE),
      # are the number of live big trees over what we need for OG?
      class_TPA = fifelse(( num_big_tree_per_acre > Trees_Per_Acre ), TRUE, FALSE, na=FALSE),
      class_Eco = fifelse(is.na(ECOSUBCD) & is.na(Habitat_Type_Group), TRUE,
                          !is.na(ECOSUBCD) & (is.na(Habitat_Type_Group) 
                                              | ECOSUBCD == Habitat_Type_Group), FALSE),
      Age = STDAGE + (2023-MEASYEAR), 
      Trees_Per_Acre = num_big_tree_per_acre,
      community_abb = OG_Type ) %>% 
    dplyr::select(cuid, contains('class'), Age, Trees_Per_Acre, site_productivity_num,
                  contains('community')) %>%
    ungroup()
  
}







