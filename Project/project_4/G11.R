###Group Members:
#Group 11
#1. Alex Tang s2592944
#2. Alim Hanif s2442474
#3. Yuna Choi s2120762

###Contributions:
#netup function : Alex
#forward function: Yuna and Alex
#backward and train function: Alim


#=======================================================#
#Overview:

#=======================================================#

#netup function
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


#forward function
forward <- function(nn,inp){
  out <- list()
  output <- c()
  out[[1]] <- inp
  h <- nn$h
  w <- nn$W
  b <- nn$b
  for (l in 1:(length(nn$h)-1)){
    
    #W^l_j * h^l
    output <- unlist(lapply(w[[l]], function(W) sum(W * out[[l]])))
    output <- output +  b[[l]]
    if (l == (length(nn$h)-1)){
      #SoftMax application
      out[[l+1]] <-  exp(output)/sum(exp(output))
    }else{
      #apply ReLu function
      out[[l+1]] <- pmax(0,(output))
    }
    
    
  }
  return(out)
}




