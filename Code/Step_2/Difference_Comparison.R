library(sf)
library(remotes)
library(colortools)
library(tidyverse)

setwd("~/Old_Growth")

hex <-  st_read('Files/gpkg/ef_hex.gpkg')
OG_Forest <- st_read('Files/gpkg/R8_R9_hex_Old_Growth.gpkg')
out <- st_read('Files/gpkg/myMOG_resHEX_reg89_cond0417_omy0718_ver0725.gpkg')

div_color_ramp <- function(difference_var, middle_range=.5, include_all=F, Extreme=F, hot_col='orangered', cold_col='royalblue3') {
  
  if(middle_range==1){
    diff_range <- range(difference_var, na.rm=T)
    diff_range <- c(floor(diff_range[1]),ceiling(diff_range[2]))
  } else{
    tail <- (1-middle_range)/2
    quants <- quantile(difference_var,probs=c(tail,1-tail), na.rm=T)
    diff_range <- c(floor(quants[1]),ceiling(quants[2]))  
  }
  
  extreme_val <- max(abs(diff_range), na.rm=T)
  ncols <- extreme_val * 2 + 1
  if(ncols<21){ ncols <- 21}
  extreme_bks <- seq(-extreme_val,extreme_val,length.out=ncols)
  extreme_bks <- c(extreme_bks[extreme_bks < 0],0,extreme_bks[extreme_bks > 0])
  
  # add most positive or negative extreme into breaks
  if(min(difference_var, na.rm=T) < min(extreme_bks, na.rm=T)) {extreme_bks <- c(floor(min(difference_var, na.rm=T)),extreme_bks)}
  if(max(difference_var, na.rm=T) > max(extreme_bks, na.rm=T)) {extreme_bks <- c(extreme_bks,ceiling(max(difference_var, na.rm=T)))}
  
  hot_cols <- setColors(hot_col, 10)[c(2,1)]
  cold_cols <- setColors(cold_col, 10)[c(1,10)]
  
  # div_col_pal <- colorRampPalette(colors=c('#8470FF','#70C7FF','white','#EDBC64','#ED6473'))
  div_col_pal <- colorRampPalette(colors=c(cold_cols,'white',hot_cols))
  div_cols <- div_col_pal(length(extreme_bks))
  
  # find if max or min is less extreme
  less_extreme <- which.min(abs(diff_range))
  
  # if extremes are equally far from zero - return dcols and dbreaks
  if(abs(diff_range)[1] == abs(diff_range)[2]){
    dbks <- extreme_bks
    
    if(which.max(abs(range(difference_var, na.rm=T))) == 1){
      dcols <- div_cols[-length(div_cols)]
    } else{
      dcols <- div_cols[-1]
    }
  } else{
    # which which break point is closest to the max/min that is less extreme
    if(less_extreme == 2){
      stop <- which.min(abs(extreme_bks - diff_range[less_extreme]))
      start <- 1
      dcols <- div_cols[start:(stop-1)]
      dbks <- extreme_bks[start:(stop)]
    } else {
      stop <- ncols
      start <- which.min(abs(extreme_bks - diff_range[less_extreme]))
      dcols <- div_cols[(start+1):stop]
      dbks <- extreme_bks[(start):stop]
    }
  }
  
  if(include_all==T){
    # include the values at the extremes that are out of the breaks when inner 90% is used
    dbks <- c(floor(min(difference_var, na.rm=T)),dbks[2:(length(dbks)-1)],ceiling(max(difference_var, na.rm=T)))  
  }
  if(Extreme == T){
    dbks <- sort(c(-1/1.8^(seq_len(10) - 1), 0, 1/1.8^(seq_len(10) - 1))) # This is specifically for the minimum difference in the R8/R9 data 
  }
  
  l <- list(cols = dcols,
            breaks = dbks)
  
  return(l)
  
}

var1 <- OG_Forest$OG_RATIO
var2 <- out$class_ab_o_RATIO

dif <- var1-var2

hex$dif <- dif

hist(dif, 
     col = "skyblue",
     main = "Distribution of OG vs ab",
     xlab = "Difference",
     breaks = seq(from=-1, to=1, by=.05))


dcr <- div_color_ramp(hex$dif, include_all = F, Extreme = T, hot_col = 'mediumseagreen', cold_col = 'violet', middle_range = .99)
plot(hex['dif'],pal=dcr$cols, breaks=dcr$breaks, border = NA)

#=================================================================#
#
#=================================================================#

df <- data.table(OG_RATIO = OG_Forest$OG_RATIO %>% abs(),
                 class_ab_o_RATIO = out$class_ab_o_RATIO %>% abs()) %>%
  drop_na()

ggplot(df, aes(OG_RATIO, class_ab_o_RATIO)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot", x = "OG_RATIO", y = "class_ab_o_RATIO") +
  theme_minimal()

#==================================================================#
#
#==================================================================#

# Create an empty vector to store mean differences
mean_diffs <- numeric(5395)

# Generate 20 random samples and calculate mean differences
for (i in 1:5395) {
  
  # Randomly sample 50,000 entries from the original data
  Pre_data <- sample(df$OG_RATIO, 5395, replace = TRUE)
  Post_data <- sample(df$class_ab_o_RATIO, 5395, replace = TRUE)
  
  # Calculate the mean for the first half
  mean_1 <- mean(Pre_data[1:5395])
  
  # Calculate the mean for the second half
  mean_2 <- mean(Post_data[1:5395])
  
  # Compute the mean difference and store it
  mean_diffs[i] <- mean_2 - mean_1
}

# Create a histogram of mean differences
hist(mean_diffs, col = "skyblue", main = "Distribution of Mean Differences", xlab = "Mean Difference")

# Calculate the 2.5th and 97.5th percentiles
lower_bound <- quantile(mean_diffs, 0.025)
upper_bound <- quantile(mean_diffs, 0.975)

upper_bound - lower_bound

hist(df$OG_RATIO-df$class_ab_o_RATIO, col = "skyblue", main = "Distribution of OG vs ab", xlab = "Difference")

