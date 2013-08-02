cai_findruns <- function(x, k) {
  xlen <- length(x)
  xsum <- sum(x[1:k])
  y <- vector(length=xlen-k+1)
  yi <- 0
  
  for (i in 1:(xlen-k+1)){
    if (i>1) {
      xsum <- xsum + x[i+k-1] - x[i-1]
    }
    if (k == xsum) {
      yi <- yi + 1
      y[yi] <- i
    }
  }
  
  y <- y[1:yi]
  return(y)
}