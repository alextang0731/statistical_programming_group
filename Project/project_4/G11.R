#s2595944 Alex Tang
#s2120762 yuna choi
netup <- function(d){
  h <- as.list(d)
  W <- list()
  b <- list()
  for (i in 1 : (length(d)-1)){
    W[[i]] <- runif(d[i+1], 0, 0.2)
    b[[i]] <- runif(d[i+1], 0, 0.2)
  }
  list(h = h, W = W, b = b)
}

# ReLU function
relu <- function(x) {
  return(pmax(0, x))
}

out <- list()
forward <- function(nn,inp){
  output <- c()
  out[[1]] <- inp
  h <- nn$h
  w <- nn$W
  b <- nn$b
  for (l in 1:(length(nn$h)-1)){
    
    #W^l_j * h^l
    output <- unlist(lapply(w[[l]], function(W) sum(W * out[[l]])))
    
    #+b^l and apply ReLu function
    out[[l+1]] <- relu(output + b[[l]])
  }
  
  #SoftMax application
  results <-  exp(out[[length(out)]])/sum(exp(unlist(out[[length(out)]])))
  return(results)
}
