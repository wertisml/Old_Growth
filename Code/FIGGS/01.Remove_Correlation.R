# write up a function to use to remove correlated data
rem.cors <- function(frame, R) { 						
  
  # find the number of columns and rows in the dataframe
  b <- ncol(frame) 						
  n <- nrow(frame)
  
  # create an empty dataframe
  keep1 <- array(0, dim = c(1, b))
  
  # remove columns that are either all zeros or all the same value
  for (h in 1:b) {
    ifelse(sum(frame[,h]) != 0, keep1[,h] <- h, keep1[,h] <- 0)
    ifelse(sum(duplicated(frame[,h])) != n - 1 ,keep1[,h] <- h, keep1[,h] <- 0)
  }	
  
  # only keep the columns that have values
  g <- as.numeric(keep1[keep1 > 0])
  new <- frame[,g]
  
  # find the number of columns left after removing no data values
  c <- ncol(new)
  
  # create a correlation matrix (will be c x c dimensions)
  cor.mat <- cor(new, method = "pearson", use = "complete.obs")
  
  # make an empty matrix filled with zeros
  keep <- array(0, dim = c(1,c)) 
  
  # make an empty matrix filled with ones
  m <- matrix(1, nrow = n, ncol = c) 				
  
  # now we need to reorder the columns so that they are in the order of most correlated
  cor.mat.ord <- cor.mat[,order(-abs(cor.mat[1,]))]
  cor.mat.ord <- cor.mat.ord[order(-abs(cor.mat[,1])),]
  
  for (i in 2:c) {
    
    if (i == 2) {
      m[,i] <- m[,i]
    }
    
    if (i > 2) {
      
      red.mat <- m[,1:(i-1)]
      cor.index <- which(red.mat[1,] == 1)
      
      var.cors <- as.numeric(abs(cor.mat.ord[i, cor.index[2:length(cor.index)]]))
      
      ifelse(any(var.cors > R), m[,i] <- 0, m[,i] <- m[,i])
    }
  }
  
  # save the column names of the variables that we kept
  name.index <- which(m[1,] == 1)
  cor.names <- colnames(cor.mat.ord)
  
  final.names <- cor.names[name.index]
  
  # pull out the original data from the above variables
  og.index <- match(final.names, colnames(new))
  
  out <- new[,og.index]
  
  return(data.frame(out))
}

  