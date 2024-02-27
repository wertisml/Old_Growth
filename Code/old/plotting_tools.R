
# plotting functions

# display_stats <- function(x, y, digits=2, cex=1, position='topleft'){
# 
#   mod <- lm(y~x)
#   r2 <- summary(mod)$r.squared
#   RMSE <- sqrt(mean((x-y)^2, na.rm=T))
#   m <- mod$coefficients[2]
# 
#   lt <- bquote(atop(italic(R)^2 == .(formatC(r2, format='f', digits = digits)),
#                     "    rmse" == .(formatC(RMSE, format='f', digits = digits))))
#                     # "slope" == .(format(m, digits = digits))))
#   # legend(position,legend=lt, bty='n', cex=cex, text.font=1)
# 
# 
#   rp = vector('expression',3)
#   rp[1] = substitute(expression(italic(R)^2 == VAL1),
#                      list(VAL1 = format(r2, digits = digits)))[2]
#   rp[2] = substitute(expression(rmse == VAL2),
#                      list(VAL2 = format(RMSE, digits = digits)))[2]
#   rp[3] = substitute(expression(slope == VAL3),
#                      list(VAL3 = format(m, digits = digits)))[2]
# 
#   legend(position, legend=rp, bty='n', cex=cex, text.font=1)
# 
# }


hist_stats <- function(variable, digits=2, cex=1, position='topright', text.col='black'){
  
  dd <- data.frame(var = variable) %>%
    summarise(mean = mean(var, na.rm=T),
              median = median(var, na.rm=T),
              q95 = quantile(var, probs=.95, na.rm=T),
              q99 = quantile(var, probs=.99, na.rm=T),
              max = max(var, na.rm=T),
              n=n())
  
  rp = vector('expression',6)
  rp[1] = substitute(expression(mean == VAL1), list(VAL1 = formatC(dd$mean, format='f', digits = digits)))[2]
  rp[2] = substitute(expression(median == VAL1), list(VAL1 = formatC(dd$median, format='f', digits = digits)))[2]
  rp[3] = substitute(expression(q95 == VAL1), list(VAL1 = formatC(dd$q95, format='f', digits = digits)))[2]
  rp[4] = substitute(expression(q99 == VAL1), list(VAL1 = formatC(dd$q99, format='f', digits = digits)))[2]
  rp[5] = substitute(expression(max == VAL1), list(VAL1 = formatC(dd$max, format='f', digits = digits)))[2]
  rp[6] = substitute(expression(N == VAL1), list(VAL1 = formatC(dd$n, format='f', digits = 0)))[2]
  legend(position, legend=rp, bty='n', cex=cex, text.font=1, text.col=text.col)
  
}


display_stats1 <- function(x, y, digits=2, cex=1, position='topleft', text.col='black', units=' '){
  
  mod <- lm(y~x)
  r2 <- summary(mod)$r.squared
  RMSE <- sqrt(mean((x-y)^2, na.rm=T))
  m <- mod$coefficients[2]
  bias = mean(y-x, na.rm=T)
  
  if(units=='Mg/ha'){
    unit = 'Mg/ha'
  } else if(units=='Tg'){
    unit <- 'Tg'
  } else if(units=='Pg') {
    unit <- 'Pg'
  } else {
    unit <- units
  }
  
  
  
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == VAL1),
                     list(VAL1 = formatC(r2, format='f', digits = digits)))[2]
  rp[2] = substitute(expression(RMSD == VAL2 ~ UNIT),
                     list(VAL2 = formatC(RMSE,format='f', digits = digits), UNIT=unit))[2]
  # rp[3] = substitute(expression(bias == VAL3),
  #                    list(VAL3 = formatC(bias,format='f', digits = (digits+1))))[2]
  rp[3] = substitute(expression(slope == VAL4),
                     list(VAL4 = formatC(m,format='f', digits = digits)))[2]
  
  legend(position, legend=rp, bty='n', cex=cex, text.font=1, text.col=text.col)
  
}


display_stats2 <- function(x, y, data, digits=2, cex=1, position='topleft', text.col='black', units='Mg/ha'){
  
  mod <- lm(y~x)
  r2 <- summary(mod)$r.squared
  # RMSE <- sqrt(mean((x-y)^2, na.rm=T))
  # m <- mod$coefficients[2]
  
  rse <- sqrt( sum(residuals(mod)^2) / mod$df.residual ) 
  
  if(units=='Mg/ha'){
    # unit <- parse(text="Mg ~ ha^-1")
    unit = 'Mg/ha'
  } else if(units=='Tg'){
    unit <- 'Tg'
  } else if(units=='Pg') {
    unit <- 'Pg'
  } else{
    unit <- ' '
  }
  
  
  
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == VAL1),
                     list(VAL1 = formatC(r2, format='f', digits = digits)))[2]
  rp[2] = substitute(expression(RSE == VAL2 ~ UNIT),
                     list(VAL2 = formatC(rse,format='f', digits = digits),
                          UNIT=unit))[2]
  # rp[2] = substitute(expression(slope == VAL3),
  #                    list(VAL3 = formatC(m,format='f', digits = digits)))[2]
  
  legend(position, legend=rp, bty='n', cex=cex, text.font=1, text.col=text.col)
  
}




write_stats <- function(y,fit){
  
  rmse <- function(y,fit){round(sqrt(mean((y-fit)^2)),2)}
  r2 <- function(y,fit){round(summary(lm(fit~y))$r.squared, 2)}
  
  RMSEp <- round(rmse(y, fit) / mean(y),2) * 100
  RMSE <- round(rmse(y, fit),2)
  R2 <- r2(y, fit)
  t <- paste0('RMSE = ',RMSE,'\nRMSE% = ',RMSEp, '\nR-sq = ',R2)
  writeLines(t)
  
}






# edited heat scatter plot - changed title position and size
hs <- function (x, y, pch = 19, cexplot = 0.5, nrcol = 30, grid = 100, 
          colpal = "heat", simulate = FALSE, daltonize = FALSE, cvd = "p", 
          alpha = NULL, rev = FALSE, xlim = NULL, ylim = NULL, xlab = NULL, 
          ylab = NULL, main = "heatscatter", cor = FALSE, method = "spearman", 
          only = "none", add.contour = FALSE, nlevels = 10, color.contour = "black", 
          greyscale = FALSE, log = "", ...) {
  library(LSD)
  if (is.null(xlab)) {
    xlab = deparse(substitute(x))
  }
  if (is.null(ylab)) {
    ylab = deparse(substitute(y))
  }
  if (!is.vector(x) | !is.vector(y)) 
    stop("First two argument must be numeric vectors!")
  if (length(x) != length(y)) 
    stop("Data vectors must be of the same length!")
  sound = which((!(is.na(x) | is.nan(x) | (x == Inf) | (x == 
                                                          -Inf))) & (!(is.na(y) | is.nan(y) | (y == Inf) | (y == 
                                                                                                              -Inf))))
  if (length(sound) == 0) 
    stop("There are no valid point pairs to plot!")
  x = x[sound]
  y = y[sound]
  if (!is.null(xlim)) {
    cut = x >= xlim[1] & x <= xlim[2]
    x = x[cut]
    y = y[cut]
  }
  if (!is.null(ylim)) {
    cut = y >= ylim[1] & y <= ylim[2]
    y = y[cut]
    x = x[cut]
  }
  if (log == "") {
    valid = 1:length(x)
  }
  else if (log == "x") {
    valid = which(x > 0)
  }
  else if (log == "y") {
    valid = which(y > 0)
  }
  else if (log %in% c("xy", "yx")) {
    valid = intersect(which(x > 0), which(y > 0))
  }
  x = x[valid]
  y = y[valid]
  if (cor) {
    main = paste(main, " cor = ", round(cor(x, y, method = method), 
                                        digits = 2))
  }
  plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
       main = "", type = "n", log = log, ...)
  heatscatterpoints(x, y, pch = pch, cexplot = cexplot, nrcol = nrcol, 
                    grid = grid, colpal = colpal, simulate = simulate, daltonize = daltonize, 
                    cvd = cvd, alpha = alpha, rev = rev, xlim = xlim, ylim = ylim, 
                    only = only, add.contour = add.contour, nlevels = nlevels, 
                    color.contour = color.contour, greyscale = greyscale, 
                    log = log, ...)
  mtext(paste(main), 3, .5, cex = 1, font=2)
}





# modeling and plottin tools

plot_fit <- function(y, fit, cex.text=1, xlab, ylab, main=NULL,regline=F,lims=NULL,...){
  # lims <- c(0, max(c(y,fit), na.rm=T)*1.1)
  if(is.null(lims)) lims <- range(c(y,fit), na.rm=T)
  hs(y=fit, x=y, xlab=xlab,ylab=ylab,main=paste0(main,' Obs vs Pred'), xlim=lims,ylim=lims, colpal = viridis(50))
  abline(a=0,b=1)
  display_stats1(y,fit, cex=cex.text, units='Mg/ha',...)
  if(regline) abline(lm(fit~y), lty='dotted',col='darkred', lwd=2)
}
 
plot_residuals <- function(y, fit, cex.text=1){
  # op <- par()
  # par(tcl=0.4, mgp=c(2.2,.4,0), pty='m', las=1,mar=c(4,4,2,1))
  # xlim <- c(0,500)
  # ylim <- c(-200,200)
  hs(x=y, y=(y-fit), xlab='y',ylab='Residuals: y - fit', main='Residuals', colpal = viridis(50))
  # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray50");grid(col='gray80')
  abline(h=0,col='black')
  # points(y, y-fit, col=ee$site_col, pch=20, cex=1.5)
  
  # legend('bottomright',unique(ee$site),pch=20,col=unique(ee$site_col), bty='n', cex=cex.text, ncol=2, pt.cex=2)
  # display_stats(y,fit, cex=1.5)
  # suppressWarnings(par(op))
}

hist_residuals <- function(y, fit){
  # op <- par()
  # par(tcl=0.4, mgp=c(2.2,.4,0), pty='m', las=1,mar=c(4,4,2,1))
  hist(y-fit,xlab=' ')
  # par(op)
}

plot_VI <- function(model, n=7, cex.axis=1.2){
  op <- par()
  par(mar=c(3,10,3,2), cex.axis=cex.axis, las=1)
  vi <- varImp(model)$importance %>% 
    arrange(desc(Overall)) %>% 
    slice(1:n) %>% 
    arrange(Overall)
  # nn <- row.names(vi)[(order(vi$Overall))]
  # vi <- vi[(order(vi$Overall)),]
  barplot(vi$Overall, names.arg = row.names(vi), horiz=T, xaxt='n')
  axis(1)
  # par(mar=c(4,4,3,2))
  par(op)
  
}


###################################################################################################


div_color_ramp <- function(difference_var, middle_range=.9, include_all=F, hot_col='orangered', cold_col='royalblue3') {
  
  library(colortools)
  
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
  
  
  l <- list(cols = dcols,
            breaks = dbks)
  
  return(l)
  
}


div_color_ramp2 <- function(difference_var, middle_range=.9, include_all=F, hot_col='orangered', cold_col='royalblue3') {
  
  library(colortools)
  
  if(middle_range==1){
    diff_range <- range(difference_var, na.rm=T)
    # diff_range <- c(floor(diff_range[1]),ceiling(diff_range[2]))
  } else{
    tail <- (1-middle_range)/2
    quants <- quantile(difference_var,probs=c(tail,1-tail), na.rm=T)
    # diff_range <- c(floor(quants[1]),ceiling(quants[2]))  
    diff_range <- c(quants[1],quants[2])
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
  
  
  l <- list(cols = dcols,
            breaks = dbks)
  
  return(l)
  
}


.my_image_scale <- function (z, col, breaks = NULL, key.pos, add.axis = TRUE, at = NULL, 
          ..., axes = FALSE, key.length, logz = FALSE) 
{
  if (!is.null(breaks) && length(breaks) != (length(col) + 
                                             1)) 
    stop("must have one more break than colour")
  zlim = range(z, na.rm = TRUE)
  if (is.null(breaks)) 
    breaks = seq(zlim[1], zlim[2], length.out = length(col) + 
                   1)
  if (is.character(key.length)) {
    kl = as.numeric(gsub(" cm", "", key.length))
    sz = if (key.pos %in% c(1, 3)) 
      dev.size("cm")[1]
    else dev.size("cm")[2]
    key.length = kl/sz
  }
  if (is.null(at)) {
    br = range(breaks)
    at = pretty(br)
    at = at[at > br[1] & at < br[2]]
  }
  kl_lim = function(r, kl) {
    m = mean(r)
    (r - m)/kl + m
  }
  if (key.pos %in% c(1, 3)) {
    ylim = c(0, 1)
    xlim = kl_lim(range(breaks), key.length)
    mar = c(0, ifelse(axes, 2.1, 1), 0, 1)
  }
  if (key.pos %in% c(2, 4)) {
    ylim = kl_lim(range(breaks), key.length)
    xlim = c(0, 1)
    mar = c(ifelse(axes, 2.1, 1), 0, 1.2, 0)
  }
  mar[key.pos] = 2.1
  par(mar = mar)
  poly = vector(mode = "list", length(col))
  for (i in seq(poly)) poly[[i]] = c(breaks[i], breaks[i + 
                                                         1], breaks[i + 1], breaks[i])
  plot(1, 1, t = "n", ylim = ylim, xlim = xlim, axes = FALSE, 
       xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  offset = 0.2
  offs = switch(key.pos, c(0, 0, -offset, -offset), c(0, 0, 
                                                      -offset, -offset), c(offset, offset, 0, 0), c(offset, 
                                                                                                    offset, 0, 0))
  ww=.6
  for (i in seq_along(poly)) {
    if (key.pos %in% c(1, 3)) 
      # polygon(poly[[i]], c(0, 0, 1, 1) + offs, col = col[i], 
      polygon(poly[[i]], c(0, 0, ww, ww) + offs, col = col[i],         
              border = NA)
    if (key.pos %in% c(2, 4)) 
      # polygon(c(0, 0, 1, 1) + offs, poly[[i]], col = col[i], 
      polygon(c(0, 0, ww, ww) + offs, poly[[i]], col = col[i], 
              border = NA)
  }
  bx = c(breaks[1], rep(tail(breaks, 1), 2), breaks[1])
  if (key.pos %in% c(1, 3)) 
    polygon(bx, c(0, 0, ww, ww) + offs, col = NA, border = "black")
  if (key.pos %in% c(2, 4)) 
    polygon(c(0, 0, ww, ww) + offs, bx, col = NA, border = "black")
  labels = if (logz) 
    parse(text = paste0("10^", at))
  else if (inherits(breaks, c("POSIXt", "Date"))) 
    format(at)
  else TRUE
  if (add.axis) 
    axis(key.pos, at = at, labels = labels)
}


######################################################
##  Ch3 PCA tools

## biplot for PCA
plot_pca <- function(pca, pcs=c(1,2), col_var=NULL){
  pc1 <- pcs[1]
  pc2 <- pcs[2]
  s <- summary(pca)
  
  xlims <- range(p$x[,pc1])[which.max(abs(range(p$x[,pc1])))]
  xlims <- sort(c(xlims, xlims*-1))
  
  ylims <- range(p$x[,pc2])[which.max(abs(range(p$x[,pc2])))]
  ylims <- sort(c(ylims, ylims*-1))
  
  if(is.null(col_var)) {
    colpal <- viridis(100)
    hs(p$x[,pc1], p$x[,pc2], 
       xlab=paste("PC ",pc1," (", round(s$importance[2,pc1]*100, 1), "%)", sep = ""), 
       ylab=paste("PC ",pc2," (", round(s$importance[2,pc2]*100, 1), "%)", sep = ""), 
       pch=20, colpal=colpal, cex=.8, las=1, xlim=xlims, ylim=ylims)
  } else {
    colpal <- magma(100)[round(ddd[[col_var]]*100,0)+1]
    plot(p$x[,pc1], p$x[,pc2], 
         xlab=paste("PC ",pc1," (", round(s$importance[2,pc1]*100, 1), "%)", sep = ""), 
         ylab=paste("PC ",pc2," (", round(s$importance[2,pc2]*100, 1), "%)", sep = ""), 
         pch=20, col=colpal, cex=.8, las=1, xlim=xlims, ylim=ylims)
    lt <- paste(seq(0,1,.2))
    legend('topleft',lt, fil=magma(100)[c(1,20,40,60,80,100)], bty='n', title=col_var, cex=.8)
  }
  
  abline(v=0, lty=2, col="black")
  abline(h=0, lty=2, col="black")
  
  ext_idx <- do.call(rbind,lapply(pcs,FUN=get_extremes, pca=pca, n_extremes=1))
  ext_vars <- matrix(rownames(p$rotation)[ext_idx], nrow=nrow(ext_idx), ncol=ncol(ext_idx), dimnames=dimnames(ext_idx))
  
  lx <- cbind(p$rotation[,pc1][ext_idx])
  ly <- cbind(p$rotation[,pc2][ext_idx])
  
  maxx <- lx[which.max(abs(lx))]
  maxy <- ly[which.max(abs(ly))]
  
  scalarx <- abs(par()$usr[1:2][which.max(abs(par()$usr[1:2]))] / maxx * .75)
  scalary <- abs(par()$usr[3:4][which.max(abs(par()$usr[3:4]))] / maxy * .75)
  
  lx <- lx * scalarx
  ly <- ly * scalary
  
  arrows(x0=0, x1=lx, y0=0, y1=ly, col="blue", length=0.04, lwd=1)
  text(lx, ly, labels=row.names(pca$rotation)[ext_idx], col="blue", cex=1, font=2)
}

# get most extreme loadings for a given principal component (x)
get_extremes <- function(x, pca, n_extremes=1){
  extremes <- matrix(c(sort(pca$rotation[,x], index.return=TRUE, decreasing=TRUE)$ix[1:n_extremes],sort(pca$rotation[,x], index.return=TRUE, decreasing=FALSE)$ix[1:n_extremes]),nrow=1)
  colnames(extremes) <- c(paste0('pos',1:n_extremes),paste0('neg',1:n_extremes))
  rownames(extremes) <- paste0('pc_',x)
  return(extremes)
}

## barplot of N most extreme variable loadings for a given principle component
pc_loadings <- function(pca, pc=1, n=5){
  
  pc1_load <- sort(pca$rotation[,pc])
  pc1_load <- pc1_load[c(c(1:n),c((length(pc1_load)-n):length(pc1_load)))]
  barplot(pc1_load, main=paste(n,"most pos/neg pc",pc,"loadings"), las=2)
  
}

## maps principle components
map_pc <- function(data, pc_var, reset=F){
  dcr <- div_color_ramp(data[[pc_var]],middle_range = 1)
  plot(data[pc_var], pal=dcr$cols, breaks=dcr$breaks, reset=reset, key.pos=4, border='grey65', lwd=.1)  
}