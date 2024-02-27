library(tidyverse)
library(rFIA)

setwd("~/MOG")

state_table <- read_csv('Files/state_table.csv')

Region <- state_table %>% filter(REGION == 9)

options(timeout=3600)

for(i in 1:nrow(Region)){

  folder_path <- paste0("Files/rFIA/", Region[i,2])  # Adjust Your_Base_Path accordingly
  dir.create(folder_path, showWarnings = FALSE)
  
data <- getFIA(states = Region[i,2],
               dir = paste0("Files/rFIA/", Region[i,2]),
               common = T,
               nCores = 12)
}




