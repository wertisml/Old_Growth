Jamis_OG <- read_csv("C:\\Users\\wertisml\\Downloads\\FIA_COND_EF_all_mostrecent_0807_OMY.csv")

table(Jamis_OG %>% filter(REGION == 9) %>% select(class_fia1))

Jamis_9 <- Jamis_OG %>%
  select(cuid, class_fia1)

setwd("~/Old_Growth")

state_table <- read_csv('Files/state_table.csv')

Region <- state_table %>% filter(REGION == 9) %>% select(STATEAB)

# Construct the pattern for list.files using state initials
file_pattern <- paste0("FIA_COND_OMY_", Region$STATEAB, ".parquet$", collapse = "|")

# Get the matching files
matching_files <- list.files(path = "Files/rFIA", pattern = file_pattern, full.names = TRUE)

OG <- open_dataset(matching_files) %>% 
  select(cuid, Old_Growth) %>%
  collect()




