### Group Members:
# Group 11
# 1. Alex Tang s2592944
# 2. Alim Hanif s2442474
# 3. Yuna Choi s2120762

### Contributions:
# netup function : Alex
# forward function: Yuna and Alex
# backward and train function: Alim


# =======================================================#
# Overview:

# =======================================================#

# netup function
netup <- function(d) {
  # h <- as.list(d)
  h <- list()
  W <- list()
  b <- list()
  for (i in 1:(length(d) - 1)) {
    h[[i]] <- rep(c(0), d[i + 1])
    # W[[i]] <- runif(d[i+1], 0, 0.2)
    W[[i]] <- matrix(runif(d[i] * d[i + 1], 0, 0.2), ncol = d[i + 1])
    b[[i]] <- runif(d[i + 1], 0, 0.2)
  }
  list(h = h, W = W, b = b)
}

softmax <- function(X) {
  return(exp(X) / sum(exp(X)))
}

relu <- function(X) {
  result <- pmax(0, X)
  result <- matrix(result, ncol = ncol(X))
  return(result)
}

# forward function
forward <- function(nn, inp) {
  h <- nn$h
  w <- nn$W
  b <- nn$b
  h_prev <- matrix(unlist(iris[, 1:4]), ncol = 4)
  for (l in 1:length(h)) {
    if (l != length(h)) {
      h[[l]] <- relu(h_prev %*% w[[l]]) + b[[l]]
    } else {
      h[[l]] <- softmax(h_prev %*% w[[l]]) + b[[l]]
    }
    h_prev <- h[[l]]
  }
  return(h)
}


# forward <- function(nn,inp){
#   out <- list()
#   output <- c()
#   out[[1]] <- inp
#   h <- nn$h
#   w <- nn$W
#   b <- nn$b
#   for (l in 1:(length(nn$h)-1)){

#     #W^l_j * h^l
#     output <- unlist(lapply(w[[l]], function(W) sum(W * out[[l]])))
#     output <- output +  b[[l]]
#     if (l == (length(nn$h)-1)){
#       #SoftMax application
#       out[[l+1]] <-  exp(output)/sum(exp(output))
#     }else{
#       #apply ReLu function
#       out[[l+1]] <- pmax(0,(output))
#     }


#   }
#   return(out)
# }
