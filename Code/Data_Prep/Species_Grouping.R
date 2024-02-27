
Forest_Type <- function(data, forest_code){
  
  out <- data %>% 
    select({{forest_code}}) %>%
    mutate(Group = ifelse({{forest_code}} >= 101 & {{forest_code}} <= 105, "WHITE/RED/JACK PINE GROUP", 
                   ifelse({{forest_code}} >= 121 & {{forest_code}} <= 129, "SPRUCE/FIR GROUP",
                   ifelse({{forest_code}} >= 141 & {{forest_code}} <= 142, "LONGLEAF/SLASH PINE GROUP",
                   ifelse({{forest_code}} >= 161 & {{forest_code}} <= 168, "LOBLOLLY/SHORTLEAF PINE GROUP",
                   ifelse({{forest_code}} >= 171 & {{forest_code}} <= 172, "OTHER EASTERN SOFTWOODS GROUP",
                   ifelse({{forest_code}} >= 182 & {{forest_code}} <= 185, "PINYON / JUNIPER GROUP",
                   ifelse({{forest_code}} >= 201 & {{forest_code}} <= 203, "DOUGLAS-FIR GROUP",
                   ifelse({{forest_code}} >= 221 & {{forest_code}} <= 226, "PONDEROSA PINE GROUP",
                   ifelse({{forest_code}} >= 241 & {{forest_code}} <= 241, "WESTERN WHITE PINE GROUP",
                   ifelse({{forest_code}} >= 261 & {{forest_code}} <= 271, "FIR/SPRUCE/MOUNTAIN HEMLOCK GROUP",
                   ifelse({{forest_code}} >= 281 & {{forest_code}} <= 281, "LODGEPOLE PINE GROUP",
                   ifelse({{forest_code}} >= 301 & {{forest_code}} <= 305, "HEMLOCK/SITKA SPRUCE GROUP",
                   ifelse({{forest_code}} >= 341 & {{forest_code}} <= 342, "REDWOOD GROUP",
                   ifelse({{forest_code}} >= 361 & {{forest_code}} <= 369, "OTHER WESTERN SOFTWOODS GROUP",
                   ifelse({{forest_code}} >= 371 & {{forest_code}} <= 385, "CALIFORNIA MIXED CONIFER GROUP",
                   ifelse({{forest_code}} >= 401 & {{forest_code}} <= 409, "OAK/PINE GROUP",
                   ifelse({{forest_code}} >= 501 & {{forest_code}} <= 520, "OAK/HICKORY GROUP",
                   ifelse({{forest_code}} >= 601 & {{forest_code}} <= 609, "OAK/GUM/CYPRESS GROUP",
                   ifelse({{forest_code}} >= 701 & {{forest_code}} <= 722, "ELM/ASH/COTTONWOOD GROUP",
                   ifelse({{forest_code}} >= 801 & {{forest_code}} <= 809, "MAPLE/BEECH/BIRCH GROUP",
                   ifelse({{forest_code}} >= 901 & {{forest_code}} <= 905, "ASPEN/BIRCH GROUP",
                   ifelse({{forest_code}} >= 911 & {{forest_code}} <= 912, "ALDER/MAPLE GROUP",
                   ifelse({{forest_code}} >= 921 & {{forest_code}} <= 935, "WESTERN OAK GROUP",
                   ifelse({{forest_code}} >= 941 & {{forest_code}} <= 943, "TANOAK/LAUREL GROUP",
                   ifelse({{forest_code}} >= 961 & {{forest_code}} <= 962, "OTHER HARDWOODS GROUP",
                   ifelse({{forest_code}} >= 971 & {{forest_code}} <= 976, "WOODLAND HARDWOODS GROUP",
                   ifelse({{forest_code}} >= 982 & {{forest_code}} <= 988, "TROPICAL HARDWOODS GROUP",
                   ifelse({{forest_code}} >= 991 & {{forest_code}} <= 995, "EXOTIC HARDWOODS GROUP", NA
                   ))))))))))))))))))))))))))))) %>%
    select(Group)
  
  return(out)
}
                                                             
types <- Forest_Type(Region_Plot, FORTYPCD)                                                                                
                                                                                
