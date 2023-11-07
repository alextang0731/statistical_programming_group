netup <- function(d){
  h <- as.list(d)
  W <- list()
  b <- list()
  for (i in 1 : length(d)){
    W[[i]] <- runif(d[i], 0, 0.2)
    b[[i]] <- runif(d[i], 0, 0.2)
  }
  list(h = h, W = W, b = b)
}