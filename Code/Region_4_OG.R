# function to classify MOG for a single community type
# Finds the plots with the correct dia of trees and number of trees
n_big_tpa <- function(tree, big_tree_size){ 
  
  tpa <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(TPA_UNADJ)
  live <- tree %>% filter(Status == "Live") %>% ungroup() %>% select(DIA)
  
  sum(coalesce(tpa[live >= big_tree_size], 0), na.rm = TRUE)
}

check_condition <- function(data, rule) {
  eval(parse(text = rule), envir = data)
}

classify_mog <- function(idx, tree, ccc){
  
  Stand_Age <- defs$Stand_Age[idx]
  Trees_Per_Acre <- defs$Trees_Per_Acre[idx]
  Large_Tree_Diameter <- defs$Large_Tree_Diameter[idx]
  OG_Type <- defs$OG_Type[idx]
  
  rules <- data.frame(
    physclcd = defs$c.PHYSCLCD[idx],
    spcd = defs$t.spcd[idx],
    statecd = defs$p.statecd[idx],
    siteclcd = defs$c.siteclcd[idx],
    adforcd = defs$c.adforcd[idx],
    ecosubd = defs$p.ECOSUBCD[idx]
  )
  
  data <- tree %>% 
    summarise( # Determines the diameter of the tree and how many big trees per acre there are 
      num_big_tree_per_acre = n_big_tpa(tree=., big_tree_size = defs$Large_Tree_Diameter[idx])
    ) %>% 
    left_join(x=ccc,y=., by='cuid') %>%  
    left_join(x=tree %>% select(SPCD, cuid),y=.,by='cuid') %>%
    mutate(# Calculates the stand age
      class_SA = fifelse(( STDAGE + (2023-MEASYEAR) >= Stand_Age ), TRUE,  FALSE),
      # are the number of large tree diameters over what we need for OG
      class_LTD = fifelse(( num_big_tree_per_acre >= Trees_Per_Acre ),TRUE, FALSE),
      # are the number of large trees over what we need for OG
      class_TPA = fifelse(( num_big_tree_per_acre >= Trees_Per_Acre ),TRUE, FALSE),
      Age = STDAGE + (2023-MEASYEAR),
      Over_LTD = num_big_tree_per_acre,
      community_abb = OG_Type) %>% 
    dplyr::select(cuid, contains('class'), num_big_tree_per_acre, Age, Over_LTD,
                  contains('community'), SPCD, STATECD, PHYSCLCD, SITECLCD,
                  ADFORCD, ECOSUBCD) %>%
    ungroup() 
  
  # Create an empty list to store results
  result_list <- list()
  
  # Iterate over rows of sample
  for (i in seq_len(nrow(data))) {
    # Extract the i-th row
    current_row <- data[i,]
    
    # Check if all conditions are met for the current row
    row_conditions_met <- sapply(names(rules), function(col_name) {
      if (!is.na(rules[[col_name]])) {
        check_condition(current_row, rules[[col_name]])
      } else {
        TRUE  # NA values do not affect the outcome
      }
    })
    
    # Store the result for the current row
    result_list[[i]] <- row_conditions_met
  }
  
  # Convert the list to a data.table for easy manipulation
  result_dt <- as.data.table(do.call(rbind, result_list))
  
  result_dt <- do.call(rbind, result_list) %>%
    data.table() %>%
    mutate(class_conditions = fifelse(rowSums(.,na.rm = TRUE) == 6, TRUE, FALSE))
  
  data %>% cbind(result_dt %>% select(class_conditions))
}
