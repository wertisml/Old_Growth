library(arrow)
library(tidyverse)

setwd("~/Old_Growth")

# fix display to show full FIA plot cns
options(scipen=999)

# load fia csv from FIA that includes all NFS and BLM plots and OFE classifications adn remove row names if needed
fia.data <- open_dataset("Files/Mature_Region") %>%
  collect() %>%
  drop_na(Vegetation_Type, Vegtype)

head(fia.data)
nrow(fia.data)

# load walkdown and percentile file
walkdown.csv <- read.csv("Files/Mature_Age.csv")
walkdown.csv <- walkdown.csv[,1:4]

# set output directory
out.directory <- "~/Old_Growth/Files/FIGGS_mine/"

