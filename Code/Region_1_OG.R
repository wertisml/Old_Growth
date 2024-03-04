
HabTyp_Group <- function(data){

  data %>%
    select(HABTYPCD1, PLT_CN, HABTYPCD1_PUB_CD, REGION) %>%
    left_join(read_csv("Files/R1_HabTyp.csv") %>%
                filter(as.numeric(REGION) == as.numeric(data$REGION)) %>%
                mutate(Habitat_Code = as.character(Numeric_Code)),
              by = c("HABTYPCD1" = "Habitat_Code")) %>%
    select(PLT_CN, Group_Code, Scientific_Name)
  
}

n_big_tpa <- function(tree, big_tree_size){ 
  
  tpa <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(TPA_UNADJ)
  live <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[live >= big_tree_size], 0), na.rm = TRUE)
  
}

classify_mog <- function(idx, tree, ccc){
  
  Stand_Age <- defs$Age[idx]
  Stand_Basal_Area <- defs$Stand_Basal_Area[idx]
  Trees_Per_Acre <- defs$Tree_Per_Acre[idx]
  Large_Tree_Diameter <- defs$Large_Tree_Diameter[idx]
  Habitat_Type_Group <- defs$Habitat_Type_Group[idx]
  OG_Type <- defs$OG_Type[idx]
  
  tree %>% 
    summarise( # Determines the diameter of the tree and how many big trees per acre there are 
      num_big_tree_per_acre = n_big_tpa(tree=., big_tree_size = Large_Tree_Diameter),
    ) %>% 
    left_join(tree %>% 
                filter(DIA >= 5, Status == "Live") %>% 
                mutate(BAA = BA * TPA_UNADJ) %>%
                group_by(cuid) %>%
                summarise(BAA = sum(BAA)),
              by='cuid') %>%
    left_join(x=ccc,y=., by='cuid') %>% 
    left_join(HabTyp_Group(ccc),y=.,by="PLT_CN") %>%
    mutate(# Calculates the stand age
      class_SA = fifelse(( STDAGE + (2023-MEASYEAR) >= Stand_Age ), TRUE,  FALSE, na=FALSE),
      # Do we have the correct number of live large tree diameters?
      class_LTD = fifelse(( num_big_tree_per_acre >= Trees_Per_Acre ),TRUE, FALSE, na=FALSE),
      # Stand basal area
      class_SBA = fifelse(( BAA >= Stand_Basal_Area ), TRUE, FALSE, na=FALSE),
      # are the number of live big trees over what we need for OG?
      class_TPA = fifelse(( num_big_tree_per_acre >= Trees_Per_Acre ), TRUE, FALSE, na=FALSE),
      class_HTG = fifelse(Group_Code %in% unlist(strsplit(Habitat_Type_Group, ",\\s*")), TRUE, FALSE, na=FALSE),
      Age = STDAGE + (2023-MEASYEAR), 
      Trees_Per_Acre = num_big_tree_per_acre,
      community_abb = OG_Type ) %>% 
    dplyr::select(cuid, contains('class'), Age, BAA, Trees_Per_Acre, Group_Code,
                  contains('community')) %>%
    ungroup()
  
}

