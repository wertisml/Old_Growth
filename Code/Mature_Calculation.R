library(tidyverse)
library(arrow)
library(furrr)

setwd("~/Old_Growth")

Data <- open_dataset("Files/Mature_Region") %>%
  collect() %>%
  as.data.frame() %>%
  mutate(REGION = as.numeric(REGION),
         number = row_number()) %>%
  drop_na() %>%
  filter(Vegtype != "R8 Wet and rain forest")

metrics <- read_csv("Files/Weights_Matrix.csv") %>% # FIA version
  fill(`Vegetation Class`, Walkdown)

# Data <- data_FS_m %>%
#   mutate(REGION = as.numeric(REGION),
#          number = row_number()) %>%
#   select(cuid, REGION, Vegtype, Vegetation_Type, tpadom, badom, ddiscore, QMDdom, HTquart,
#          HTsd, snagbatot, number)

#==================================================================#
#
#==================================================================#

# Function to calculate score
calculate_score <- function(data) {
  
  Threshold <- metrics %>%
    filter(grepl(data$Vegtype, metrics$`Vegetation Class`))
  
  indicator <- Threshold$Indicators
  threshold <- Threshold$Threshold
  weight <- Threshold$Weight
  values <- data %>% select(all_of(indicator))
  score <- rowSums(ifelse(values >= threshold, weight, 0))
  return(score)
}

Mature_pipeline <- function(data) {
  
  data <- data %>%
    select(-REGION, -Vegetation_Type) %>%
    nest(data = c(-number)) %>%
    mutate(calculate = future_map(data, calculate_score)) %>%
    select(-data) %>% 
    unnest(cols = c(calculate), names_repair = "minimal") %>% 
    rowwise()
  
}

plan(multisession, workers = 12)

Mature <- Mature_pipeline(Data) 

plan(sequential)


Output <- Mature %>%
  left_join(Data, join_by(number)) %>%
  arrange(cuid, desc(calculate), sum(!is.na(.))) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Mature = fifelse(calculate >= 0.5, 1, 0))

table(Output$Mature)

#write_parquet(Mature, "Files/Mature_Plots_FS_Version.parquet")












