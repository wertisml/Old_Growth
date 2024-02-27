#################################################################################################
#
# estimates of OG from cond-level classifications
#
# ratio-of-means estimator derived with Paul
#
#################################################################################################

rm(list=ls())

setwd("~/MOG")

library(rFIA)
library(sf)
library(data.table)
library(dplyr)
library(stringr)
library(parallel)
# library(foreach)
# library(doParallel)

options(scipen=999)

hex <- st_read('Files/gpkg/ef_hex.gpkg')

source('Code/old/ch3_tools.R')
source('Code/old/plotting_tools.R')


# ef <- st_read('./data/ef/ef_extent.gpkg')
# buf <- st_buffer(ef, dist=500)
# buff <- st_simplify(buf, dTolerance=1000)
# efhex <- st_intersection(hex,buff)
# st_write(efhex, './data/ef/ef_hex.gpkg')


#################################################################################################
#################################################################################################

# set up class variables

# start with plot table to get PLOT_STATUS_CD for each hex
plot <- fread('Files/old/FIA_PLOT_EF_all_mostrecent_0417.csv') %>% as.data.frame() %>% 
  select(puid, PLOT_STATUS_CD) %>% 
  # join with the OMY classification COND table
  left_join(y= get_omy(), by='puid') %>% 
  make_binary_old(colname='class_a') %>%
  make_binary_old(colname='class_b') %>%
  make_binary_old(colname='class_s1') %>% 
  make_binary_old(colname='class_ab') %>% 
  make_binary_old(colname='class_as1') %>% 
  make_binary_old(colname='class_bs1') %>% 
  make_binary_old(colname='class_abs1') %>% 
  mutate(
    og_a_prop = class_a_o * CONDPROP_UNADJ,
    og_b_prop = class_b_o * CONDPROP_UNADJ,
    og_s_prop = class_s1_o * CONDPROP_UNADJ,
    og_ab_prop = class_ab_o * CONDPROP_UNADJ,
    og_as_prop = class_as1_o * CONDPROP_UNADJ,
    og_bs_prop = class_bs1_o * CONDPROP_UNADJ,
    og_abs_prop = class_abs1_o * CONDPROP_UNADJ,
    forested_prop = forested * CONDPROP_UNADJ,
    forested_prop = replace(forested_prop, COND_STATUS_CD %in% c(3,4,5),NA)
  ) %>% 
  group_by(puid) %>% 
  # calculate proportion of each OG flavor and forest on each plot
  summarise(
    EMAP_HEX = unique(EMAP_HEX),
    MEASYEAR = unique(MEASYEAR),
    STATEAB = unique(STATEAB),
    PLOT_STATUS_CD = unique(PLOT_STATUS_CD),
    plot_prop_og_a = sum(og_a_prop, na.rm=T),
    plot_prop_og_b = sum(og_b_prop, na.rm=T),
    plot_prop_og_s = sum(og_s_prop, na.rm=T),
    plot_prop_og_ab = sum(og_ab_prop, na.rm=T),
    plot_prop_og_as = sum(og_as_prop, na.rm=T),
    plot_prop_og_bs = sum(og_bs_prop, na.rm=T),
    plot_prop_og_abs = sum(og_abs_prop, na.rm=T),
    plot_prop_forest = sum(forested_prop, na.rm=T),
    n_cond_og_a = sum(class_a == 'old', na.rm=T),
    n_cond_og_b = sum(class_b == 'old', na.rm=T),
    n_cond_og_s = sum(class_s1 == 'old', na.rm=T),
    n_cond_og_ab = sum(class_ab == 'old', na.rm=T),
    n_cond_og_as = sum(class_as1 == 'old', na.rm=T),
    n_cond_og_bs = sum(class_bs1 == 'old', na.rm=T),
    n_cond_og_abs = sum(class_abs1 == 'old', na.rm=T),
    n_cond_forested = sum(forested, na.rm=T)
  ) %>% 
  left_join(fread('Files/old/ef_hex_epa2_lut_0613.csv'), by='EMAP_HEX')
  

est <- plot %>% 
  group_by(EMAP_HEX) %>% 
  summarise(
  n = sum(n_cond_forested > 0),
  region = unique(EPA2_code),
  
  x_hat = mean(plot_prop_forest, na.rm=T),
  v_x_hat = var(plot_prop_forest) / n,
  
  y_hat_a = mean(plot_prop_og_a, na.rm=T),
  v_y_hat_a = var(plot_prop_og_a) / n,
  cov_xy_hat_a = cov(plot_prop_forest,plot_prop_og_a) / n,
  r_hat_a = y_hat_a / x_hat,
  v_r_hat_a = (1/x_hat^2) * (v_y_hat_a + (r_hat_a^2)*v_x_hat - 2*r_hat_a*cov_xy_hat_a),
  
  y_hat_b = mean(plot_prop_og_b, na.rm=T),
  v_y_hat_b = var(plot_prop_og_b) / n,
  cov_xy_hat_b = cov(plot_prop_forest,plot_prop_og_b) / n,
  r_hat_b = y_hat_b / x_hat,
  v_r_hat_b = (1/x_hat^2) * (v_y_hat_b + (r_hat_b^2)*v_x_hat - 2*r_hat_b*cov_xy_hat_b),
  
  y_hat_s = mean(plot_prop_og_s, na.rm=T),
  v_y_hat_s = var(plot_prop_og_s) / n,
  cov_xy_hat_s = cov(plot_prop_forest,plot_prop_og_s) / n,
  r_hat_s = y_hat_s / x_hat,
  v_r_hat_s = (1/x_hat^2) * (v_y_hat_s + (r_hat_s^2)*v_x_hat - 2*r_hat_s*cov_xy_hat_s),
  
  y_hat_ab = mean(plot_prop_og_ab, na.rm=T),
  v_y_hat_ab = var(plot_prop_og_ab) / n,
  cov_xy_hat_ab = cov(plot_prop_forest,plot_prop_og_ab) / n,
  r_hat_ab = y_hat_ab / x_hat,
  v_r_hat_ab = (1/x_hat^2) * (v_y_hat_ab + (r_hat_ab^2)*v_x_hat - 2*r_hat_ab*cov_xy_hat_ab),
  
  y_hat_as = mean(plot_prop_og_as, na.rm=T),
  v_y_hat_as = var(plot_prop_og_as) / n,
  cov_xy_hat_as = cov(plot_prop_forest,plot_prop_og_as) / n,
  r_hat_as = y_hat_as / x_hat,
  v_r_hat_as = (1/x_hat^2) * (v_y_hat_as + (r_hat_as^2)*v_x_hat - 2*r_hat_as*cov_xy_hat_as),
  
  y_hat_bs = mean(plot_prop_og_bs, na.rm=T),
  v_y_hat_bs = var(plot_prop_og_bs) / n,
  cov_xy_hat_bs = cov(plot_prop_forest,plot_prop_og_bs) / n,
  r_hat_bs = y_hat_bs / x_hat,
  v_r_hat_bs = (1/x_hat^2) * (v_y_hat_bs + (r_hat_bs^2)*v_x_hat - 2*r_hat_bs*cov_xy_hat_bs),
  
  y_hat_abs = mean(plot_prop_og_abs, na.rm=T),
  v_y_hat_abs = var(plot_prop_og_abs) / n,
  cov_xy_hat_abs = cov(plot_prop_forest,plot_prop_og_abs) / n,
  r_hat_abs = y_hat_abs / x_hat,
  v_r_hat_abs = (1/x_hat^2) * (v_y_hat_abs + (r_hat_abs^2)*v_x_hat - 2*r_hat_abs*cov_xy_hat_abs),
  
  ) %>% 
  select(EMAP_HEX,region,n,contains('r_'),x_hat, v_x_hat) 


## read in FIA biomass from Sean 2020
fia <- st_read('/Users/jamis/dropbox/gedi/usfia/data/CONUSbiohex2020/CONUSbiohex2020_epsg6933.shp', quiet = T) %>%
  st_drop_geometry() %>%
  # left_join(scalar) %>%
  dplyr::select(EMAP_HEX,AVG_INVYR,PROP_FORES,JENK_LIVE,SE_JENK_LI,SAMPLED_PL,EST_SAMPLE) %>%
  transmute(
    EMAP_HEX = EMAP_HEX,
    fia_are_ha=EST_SAMPLE,
    fia_pf = PROP_FORES,
    fia_mean = JENK_LIVE,
    fia_mean_se = SE_JENK_LI / 100 * fia_mean,
    fia_mean_for = fia_mean / fia_pf,
    fia_mean_for_se = fia_mean_se / fia_pf,
    fia_nplots = SAMPLED_PL,
    inv_year = AVG_INVYR
  )
fia$fia_mean_for[is.na(fia$fia_mean_for)] <- 0
fia$fia_mean_for_se[is.na(fia$fia_mean_for_se)] <- 0


## rename varialbes, merge with healey 2020 hex estimates
out <- hex %>% left_join(est, by='EMAP_HEX') %>% 
  rename(
    class_a_o_RATIO = r_hat_a,
    class_a_o_RATIO_VAR = v_r_hat_a,
    
    class_b_o_RATIO = r_hat_b,
    class_b_o_RATIO_VAR = v_r_hat_b,
    
    class_s_o_RATIO = r_hat_s,
    class_s_o_RATIO_VAR = v_r_hat_s,
    
    class_ab_o_RATIO = r_hat_ab,
    class_ab_o_RATIO_VAR = v_r_hat_ab,
    
    class_as_o_RATIO = r_hat_as,
    class_as_o_RATIO_VAR = v_r_hat_as,
    
    class_bs_o_RATIO = r_hat_bs,
    class_bs_o_RATIO_VAR = v_r_hat_bs,
    
    class_abs_o_RATIO = r_hat_abs,
    class_abs_o_RATIO_VAR = v_r_hat_abs,
    
    class_f_RATIO = x_hat,
    class_f_RATIO_VAR = v_x_hat,
  ) %>% 
  left_join(fia)


st_write(out, './data/fia/my_mog_estimtaes/myMOG_resHEX_reg89_cond0417_omy0718_ver0725.gpkg', append=F)


# eo <- st_read('./data/fia/my_mog_estimtaes/myMOG_resHEX_reg89_cond0417_omy0718_ver0718.gpkg')
# 
# 
# hs(eo$class_f_RATIO,out$class_f_RATIO)
# 
# par(mfrow=c(1,2))
# 
# hs(eo$fia_pf,eo$class_f_RATIO, xlab='FIA proportion forest', ylab='Jamis proportion forest',
#    main='NOT accounting for water')
# display_stats1(eo$fia_pf,eo$class_f_RATIO)
# abline(a=0,b=1)
# 
# hs(out$fia_pf,out$class_f_RATIO, xlab='FIA proportion forest', ylab='Jamis proportion forest',
#    main='YES accounting for water')
# display_stats1(out$fia_pf,out$class_f_RATIO)
# abline(a=0,b=1)
# 
# bks=seq(0,1,.05)
# hist(out$class_f_RATIO, main='Jamis estimate', xlab='proportion forest', ylim=c(0,700), breaks=bks)
# hist(out$fia_pf, main='M&H estimate', xlab='proportion forest', ylim=c(0,700),breaks=bks)
# 
# out$pf_dif <- out$fia_pf - out$class_f_RATIO
# 
# st_write(out, './data/fia/my_mog_estimtaes/prop_forest_dif_0725.gpkg', append=F)
# 
# hs(out$fia_are_ha, out$pf_dif, xlim=c(40000,85000), ylim=c(-.1,.3))
# 
# hs(out$fia_pf,out$fia_are_ha)

###########################################################################################################################
###########################################################################################################################

## making mean and standard error estimates for different groupings (total EF and by ecoregion)

est_og_flavors <- function(plot, ...){
  est <- plot %>% 
    group_by(!!! ensyms(...)) %>% 
    summarise(
      n = sum(n_cond_forested > 0),
      
      x_hat = mean(plot_prop_forest, na.rm=T),
      v_x_hat = var(plot_prop_forest) / n,
      
      y_hat_a = mean(plot_prop_og_a, na.rm=T),
      v_y_hat_a = var(plot_prop_og_a) / n,
      cov_xy_hat_a = cov(plot_prop_forest,plot_prop_og_a) / n,
      r_hat_a = y_hat_a / x_hat,
      v_r_hat_a = (1/x_hat^2) * (v_y_hat_a + (r_hat_a^2)*v_x_hat - 2*r_hat_a*cov_xy_hat_a),
      
      y_hat_b = mean(plot_prop_og_b, na.rm=T),
      v_y_hat_b = var(plot_prop_og_b) / n,
      cov_xy_hat_b = cov(plot_prop_forest,plot_prop_og_b) / n,
      r_hat_b = y_hat_b / x_hat,
      v_r_hat_b = (1/x_hat^2) * (v_y_hat_b + (r_hat_b^2)*v_x_hat - 2*r_hat_b*cov_xy_hat_b),
      
      y_hat_s = mean(plot_prop_og_s, na.rm=T),
      v_y_hat_s = var(plot_prop_og_s) / n,
      cov_xy_hat_s = cov(plot_prop_forest,plot_prop_og_s) / n,
      r_hat_s = y_hat_s / x_hat,
      v_r_hat_s = (1/x_hat^2) * (v_y_hat_s + (r_hat_s^2)*v_x_hat - 2*r_hat_s*cov_xy_hat_s),
      
      y_hat_ab = mean(plot_prop_og_ab, na.rm=T),
      v_y_hat_ab = var(plot_prop_og_ab) / n,
      cov_xy_hat_ab = cov(plot_prop_forest,plot_prop_og_ab) / n,
      r_hat_ab = y_hat_ab / x_hat,
      v_r_hat_ab = (1/x_hat^2) * (v_y_hat_ab + (r_hat_ab^2)*v_x_hat - 2*r_hat_ab*cov_xy_hat_ab),
      
      y_hat_as = mean(plot_prop_og_as, na.rm=T),
      v_y_hat_as = var(plot_prop_og_as) / n,
      cov_xy_hat_as = cov(plot_prop_forest,plot_prop_og_as) / n,
      r_hat_as = y_hat_as / x_hat,
      v_r_hat_as = (1/x_hat^2) * (v_y_hat_as + (r_hat_as^2)*v_x_hat - 2*r_hat_as*cov_xy_hat_as),
      
      y_hat_bs = mean(plot_prop_og_bs, na.rm=T),
      v_y_hat_bs = var(plot_prop_og_bs) / n,
      cov_xy_hat_bs = cov(plot_prop_forest,plot_prop_og_bs) / n,
      r_hat_bs = y_hat_bs / x_hat,
      v_r_hat_bs = (1/x_hat^2) * (v_y_hat_bs + (r_hat_bs^2)*v_x_hat - 2*r_hat_bs*cov_xy_hat_bs),
      
      y_hat_abs = mean(plot_prop_og_abs, na.rm=T),
      v_y_hat_abs = var(plot_prop_og_abs) / n,
      cov_xy_hat_abs = cov(plot_prop_forest,plot_prop_og_abs) / n,
      r_hat_abs = y_hat_abs / x_hat,
      v_r_hat_abs = (1/x_hat^2) * (v_y_hat_abs + (r_hat_abs^2)*v_x_hat - 2*r_hat_abs*cov_xy_hat_abs),
      
    ) %>% 
    select(n,contains('r_'),x_hat, v_x_hat,...) %>% 
    rename(
      class_a_o_RATIO = r_hat_a,
      class_a_o_RATIO_VAR = v_r_hat_a,
      
      class_b_o_RATIO = r_hat_b,
      class_b_o_RATIO_VAR = v_r_hat_b,
      
      class_s_o_RATIO = r_hat_s,
      class_s_o_RATIO_VAR = v_r_hat_s,
      
      class_ab_o_RATIO = r_hat_ab,
      class_ab_o_RATIO_VAR = v_r_hat_ab,
      
      class_as_o_RATIO = r_hat_as,
      class_as_o_RATIO_VAR = v_r_hat_as,
      
      class_bs_o_RATIO = r_hat_bs,
      class_bs_o_RATIO_VAR = v_r_hat_bs,
      
      class_abs_o_RATIO = r_hat_abs,
      class_abs_o_RATIO_VAR = v_r_hat_abs,
      
      class_f_RATIO = x_hat,
      class_f_RATIO_VAR = v_x_hat,
    ) %>% 
    mutate(
      class_a_o_RCI = sqrt(class_a_o_RATIO_VAR) / sqrt(n) * qnorm(0.975),
      class_b_o_RCI = sqrt(class_b_o_RATIO_VAR) / sqrt(n) * qnorm(0.975),
      class_s_o_RCI = sqrt(class_s_o_RATIO_VAR) / sqrt(n) * qnorm(0.975),
      class_ab_o_RCI = sqrt(class_ab_o_RATIO_VAR) / sqrt(n) * qnorm(0.975),
      class_as_o_RCI = sqrt(class_as_o_RATIO_VAR) / sqrt(n) * qnorm(0.975),
      class_bs_o_RCI = sqrt(class_bs_o_RATIO_VAR) / sqrt(n) * qnorm(0.975),
      class_abs_o_RCI = sqrt(class_abs_o_RATIO_VAR) / sqrt(n) * qnorm(0.975)
    ) %>% 
    select(!contains(c('VAR','_f_'))) %>% 
    # relocate(sort(names(.))) %>% 
    relocate(...) %>%
    mutate(across(c(starts_with("class")), ~ .x * 100)) %>% 
    as.data.frame()
    # select(...,contains(c('_a_','_b_','_s_','_abs_')))
  
  return(est)
}

all <- est_og_flavors(plot) %>% mutate(EPA2_code='all')

round(all$class_s_o_RATIO,1)
formatC(all$class_s_o_RCI,digits=1)

round(all$class_b_o_RATIO,1)
formatC(all$class_b_o_RCI,digits=1)

round(all$class_a_o_RATIO,1)
formatC(all$class_a_o_RCI,digits=1)

round(all$class_abs_o_RATIO,1)
formatC(all$class_abs_o_RCI,digits=1)



by_region <- est_og_flavors(plot, 'EPA2_code') %>% mutate(EPA2_code = as.character(EPA2_code))


og_est <- bind_rows(all,by_region) %>% relocate(EPA2_code) %>% 
  select(EPA2_code,class_a_o_RATIO,class_a_o_RCI,class_b_o_RATIO,class_b_o_RCI,class_s_o_RATIO,class_s_o_RCI,
         class_ab_o_RATIO,class_ab_o_RCI,class_as_o_RATIO,class_as_o_RCI,class_bs_o_RATIO,class_bs_o_RCI,
         class_abs_o_RATIO,class_abs_o_RCI)

og_est %>% arrange(desc(class_a_o_RATIO))
og_est %>% arrange(desc(class_b_o_RATIO))
og_est %>% arrange(desc(class_s_o_RATIO))
og_est %>% arrange(desc(class_abs_o_RATIO))



sg <- apply(og_est[,-1], 2, formatC, format='fg', digits=3)

out <- bind_cols(og_est[1],sg)

write.csv(out, './paper/draft/tables/ogp_estimation.csv', row.names = F, quote=F)


### south vs north estimations

est_og_flavors(plot %>% 
                 mutate(EPA2_NS = case_when(
                   EPA2_code %in% c(5.23,8.12,9.2) ~ 'N',
                   EPA2_code %in% c(8.3,8.4,8.5) ~ 'S',
                 )), 'EPA2_NS') %>% 
  select(EPA2_NS,class_a_o_RATIO,class_a_o_RCI,class_b_o_RATIO,class_b_o_RCI,class_s_o_RATIO,class_s_o_RCI,class_abs_o_RATIO,class_abs_o_RCI)



cor_flavors <- function(est,...){
  cc <- est %>% 
    filter(
      # fia_pf > 0.25,
      !is.na(region)
    ) %>% 
    st_drop_geometry() %>% 
    group_by(!!! ensyms(...)) %>%
    summarise(
      cor_ab = cor(class_a_o_RATIO,class_b_o_RATIO, use='complete.obs'),
      cor_as = cor(class_a_o_RATIO,class_s_o_RATIO, use='complete.obs'),
      cor_bs = cor(class_b_o_RATIO,class_s_o_RATIO, use='complete.obs'),
    ) 
  return(cc)
}

by_region <- cor_flavors(out, 'region') %>% mutate(region=as.character(region))
all <- cor_flavors(out) %>% mutate(region='all')

flavor_cors <- bind_rows(all,by_region) %>% relocate(region)

flavor_cors %>% arrange(desc(cor_ab))
flavor_cors %>% arrange(desc(cor_as))
flavor_cors %>% arrange(desc(cor_bs))

  
# ########################################################################################
# ########################################################################################
# # checking against post-strat
# 
## difference between proportion forest estimates 
# out$pf_dif <- out$fia_pf - out$class_f_RATIO
# dcr <- div_color_ramp(out$pf_dif, middle_range = .999)
# plot(out['pf_dif'], breaks=dcr$breaks, pal=dcr$cols)
# hs(out$class_f_RATIO,out$pf_dif)
# hs(out$fia_pf,out$pf_dif)
# hist(out$pf_dif, breaks=seq(-.3,.8,.01))
#
#
#
#
#
# pse <- st_read('./data/fia/my_mog_estimtaes/myMOG_resHEX_reg89_cond0417_omy0531_ver0531.gpkg')
# 
# foo <- pse %>% left_join(est, by='EMAP_HEX')
# 
# 
# 
# par(mfrow=c(3,2), cex.lab=1.5, cex.axis=1.4)
# 
# hs(foo$class_a_o_RATIO, foo$r_hat_a, main='temporal OG proportion estimates', xlab='rFIA post-strat estimate',ylab='horvitz-thompson-ratio-estimator', cex=1.1)
# display_stats1(foo$class_a_o_RATIO, foo$r_hat_a, cex=1.5)
# abline(a=0,b=1)
# 
# hs(foo$class_a_o_RATIO_VAR, foo$v_r_hat_a, main='temporal OG proportion variance', xlab='rFIA post-strat estimate variance',ylab='horvitz-thompson-ratio-estimator variance', cex=1.1)
# display_stats1(foo$class_a_o_RATIO_VAR, foo$v_r_hat_a, cex=1.5)
# abline(a=0,b=1)
# 
# hs(foo$class_b_o_RATIO, foo$r_hat_b, main='functional OG proportion estimates', xlab='rFIA post-strat estimate',ylab='horvitz-thompson-ratio-estimator', cex=1.1)
# display_stats1(foo$class_b_o_RATIO, foo$r_hat_b, cex=1.5)
# abline(a=0,b=1)
# 
# hs(foo$class_b_o_RATIO_VAR, foo$v_r_hat_b, main='functional OG proportion variance', xlab='rFIA post-strat estimate variance',ylab='horvitz-thompson-ratio-estimator variance', cex=1.1)
# display_stats1(foo$class_b_o_RATIO_VAR, foo$v_r_hat_b, cex=1.5)
# abline(a=0,b=1)
# 
# hs(foo$class_s1_o_RATIO, foo$r_hat_s1, main='physical OG proportion estimates', xlab='rFIA post-strat estimate',ylab='horvitz-thompson-ratio-estimator', cex=1.1)
# display_stats1(foo$class_s1_o_RATIO, foo$r_hat_s1, cex=1.5)
# abline(a=0,b=1)
# 
# hs(foo$class_s1_o_RATIO_VAR, foo$v_r_hat_s1, main='physical OG proportion variance', xlab='rFIA post-strat estimate variance',ylab='horvitz-thompson-ratio-estimator variance', cex=1.1)
# display_stats1(foo$class_s1_o_RATIO_VAR, foo$v_r_hat_s1, cex=1.5)
# abline(a=0,b=1)
# 
# ###############
# par(mfrow=c(3,1))
# 
# hist(foo$v_r_hat_a - foo$class_a_o_RATIO_VAR, breaks=seq(-.1,2.5,.005))
# abline(v=0, lwd=1,col='red')
# hist(foo$v_r_hat_b - foo$class_b_o_RATIO_VAR, breaks=seq(-.1,2.7,.005))
# abline(v=0, lwd=1,col='red')
# hist(foo$v_r_hat_s1 - foo$class_s1_o_RATIO_VAR, breaks=seq(-.1,3.5,.005))
# abline(v=0, lwd=1,col='red')
# 
# sum(abs(foo$r_hat_a - foo$class_a_o_RATIO) < .05, na.rm=T) / sum(!is.na(foo$r_hat_a - foo$class_a_o_RATIO))
# sum(abs(foo$r_hat_b - foo$class_b_o_RATIO) < .05, na.rm=T) / sum(!is.na(foo$r_hat_b - foo$class_b_o_RATIO))
# sum(abs(foo$r_hat_s1 - foo$class_s1_o_RATIO) < .1, na.rm=T) / sum(!is.na(foo$r_hat_s1 - foo$class_s1_o_RATIO))
# 
# #################################################################################################
# #################################################################################################
# 
# ## troubleshooting/investigating hexes with:
# ## 1) large differences between estimates
# ## 2) large differences in estimate variance
# 
# ## 1) estimate differences
# 
# # temporal estimate differences
# foo$diff <- foo$class_a_o_RATIO - foo$r_hat_a
# dcr <- div_color_ramp(foo$diff, middle_range = 1)
# plot(foo['diff'], breaks=dcr$breaks, pal=dcr$cols, border=NA)
# 
# plot(foo['r_hat_a'])
# plot(foo['class_a_o_RATIO'])
# 
# foo %>% 
#   st_drop_geometry() %>% 
#   filter(class_a_o_RATIO < 0.02 & r_hat_a > 0.05) %>% 
#   select(EMAP_HEX,class_a_o_RATIO,r_hat_a, n)
# 
# omy %>% 
#   left_join(y=plot[,c('puid','EMAP_HEX')], by='puid') %>% 
#   filter(EMAP_HEX == 11926) %>% 
#   filter(class_a == 'old')
# 
# 
# # functional estimate differences
# foo$diff <- foo$class_b_o_RATIO - foo$r_hat_b
# dcr <- div_color_ramp(foo$diff, middle_range = 1)
# plot(foo['diff'], breaks=dcr$breaks, pal=dcr$cols, border=NA)
# 
# 
# plot(foo['r_hat_b'])
# plot(foo['class_b_o_RATIO'])
# 
# # Physical estimate differences
# foo$diff <- foo$class_s1_o_RATIO - foo$r_hat_s1
# dcr <- div_color_ramp(foo$diff, middle_range = 1)
# plot(foo['diff'], breaks=dcr$breaks, pal=dcr$cols, border=NA)
# 
# plot(foo['r_hat_s1'])
# plot(foo['class_s1_o_RATIO'])
# 
# # lookng at forest types along missouri arkansas border where structure "line" is obvious
# 
# 
# ml <- omy %>% filter(EMAP_HEX %in% missouri_low)
# al <- omy %>% filter(EMAP_HEX %in% arkansas_low)
# 
# mf <- omy %>% filter(EMAP_HEX %in% missouri_forest)
# 
# table(mf$community_abb)
# 
# 
# table(ml$community_abb, ml$class_s1)
# table(al$community_abb, al$class_s1)
# 
# table(al$forest_group)
# table(ml$forest_group)
# 
# ## 2) estimat variance differences
# 
# d <- foo %>% 
#   filter(abs(v_r_hat_a - class_a_o_RATIO_VAR) > .25)
# 
# plot(d$class_a_o_RATIO, d$r_hat_a)
# abline(a=0,b=1)
# 
# hist(d$fia_pf)
# hist(d$n)
# hist(d$nPlots_x)
