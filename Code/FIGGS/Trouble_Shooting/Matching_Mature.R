
setwd("~/Old_Growth")

#===============================================================#
# Forest Servie Information
#===============================================================#

# Mature Classification
USFS_OG <- read_csv("Files/FIGGS/FIGSS_CLASSIFICATION_.csv") %>%
  mutate(MOG = fifelse(FINAL.CLASS >= 0.5, 1, 0, na=0))

# Mature metrics
FS_Mature <- read_csv("Files/FIGGS/mature_forest_input_data.csv")

# Join
USFS <- FS_Mature %>%
  left_join(USFS_OG) %>%
  filter(NFSregion %in% c(8,9)) %>%
  arrange(PLT_CN, desc(FINAL.CLASS), sum(!is.na(.))) %>%
  group_by(PLT_CN) %>%
  slice(1) %>%
  ungroup()

#===============================================================#
# My Classification Information
#===============================================================#

# All plots in ROI
cond <- open_dataset('Files/PLot_and_Cond_Regions.parquet') %>% 
  filter(REGION %in% c("08", "09"),
         FORTYPCD != 999) %>%
  select(cuid, MEASYEAR, PLT_CN, REGION) %>%
  collect() %>%
  arrange(cuid, desc(MEASYEAR), sum(!is.na(.))) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(CONDID = as.numeric(str_extract(cuid, "(?<=_)[^_]+$")))

# Mature metris and classification but does not contain any Old Growth plots
MY_Mature <- read_csv("Files/FIGGS_mine/FIGSS_CLASSIFICATION_.csv") %>%
  select(-`...1`, -REGION) %>%
  arrange(cuid, desc(FINAL.CLASS), sum(!is.na(.))) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Mature = fifelse(FINAL.CLASS >= 0.5, 1, 0, na=0),
         CONDID = as.numeric(str_extract(cuid, "(?<=_)[^_]+$")))

# Add PLT_CN to the corresponding plot
Data <- cond %>%
  left_join(MY_Mature, join_by(cuid, CONDID)) %>%
  replace_na(replace = list(Mature = 0))

#===============================================================#
# Join the USFS and my classifications
#===============================================================#

# Keep only the matching PLT_CN
selected_plots <- Data %>%
  select(PLT_CN, Mature, CONDID) %>%
  left_join(USFS %>%
              select(PLT_CN, MOG, CONDID),
            join_by(PLT_CN, CONDID)) %>%
  drop_na(MOG)

table(selected_plots$Mature, selected_plots$MOG)

matches <- selected_plots %>%
  filter(Mature == 1 & MOG == 1)
# I will need to go back in and figure out what is going on here 
FS_m <- selected_plots %>%
  filter(Mature == 0 & MOG == 1)

data_FS_m <- Data %>% 
  semi_join(FS_m %>% select(PLT_CN, CONDID)) %>%
  drop_na()

# FS_Version <- USFS %>% 
#   filter(PLT_CN %in% FS_m$PLT_CN)

my_m <- selected_plots %>%
  filter(Mature == 1 & MOG == 0)

data_my_m <- Data %>% 
  filter(PLT_CN %in% my_m$PLT_CN)

#===============================================================#
# Figure out why plots get dropped
#===============================================================#

# We have 2102 plots that are in the USFS Regions 8 & 9 but not in my data 
missing_plots <- USFS %>%
  filter(!PLT_CN %in% selected_plots$PLT_CN) %>%
  select(PLT_CN, CONDID, NFSregion)

missing_cond <- open_dataset('Files/PLot_and_Cond_Regions.parquet') %>% 
  filter(PLT_CN %in% missing_plots$PLT_CN) %>%
  select(cuid, MEASYEAR, PLT_CN, REGION, FORTYPCD) %>%
  collect() %>%
  arrange(cuid, desc(MEASYEAR), sum(!is.na(.))) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup()

Mature_Data <- open_dataset("Files/Mature_Region") %>%
  filter(cuid %in% missing_cond$cuid) %>%
  collect() %>%
  distinct(cuid, .keep_all = T)

MY_Mature %>% filter(cuid %in% Mature_Data$cuid)
