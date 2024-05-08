#------------ SPLIT OLD GROWTH FROM NON-OLD GROWTH -------------------------#

# book keeping
print('SPLITTING OLD GROWTH FROM NOT OLD GROWTH.')

og.fia.filter <- open_dataset("Files/OG_Regions") %>%
  filter(REGION %in% c("08", "09")) %>%
  select(cuid, Old_Growth) %>%
  collect() %>%
  as.data.frame()

og <- og.fia.filter %>%
  filter(Old_Growth == "Old Growth") %>%
  select(cuid)

og.fia <- fia.data %>% filter(cuid %in% og$cuid)

not.og.fia <- fia.data %>% filter(! cuid %in% og$cuid)
