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
# The code generates and trains a neural network using stochastic gradient descent,
# involves iteratively updating the network's weights and biases based on the 
# gradients of the loss function, computed using small batches of training data.
# After that, it uses the "iris" dataset and trains a 4-8-7-3 network as an example.
# =======================================================#

set.seed(0)

# netup function
netup <- function(d) {
  # Function to 
  # Parameter:
  #   - 
  # Return:
  #   -
  
  #a vector of length d[l] which will contain the node values for layer l
  h <- list()
  
  #the weight matrix linking layer l to layer l+1
  #initialized the elements with Uniform(0, 0.2) random deviates
  W <- list()
  
  #the offset vector linking layer l to layer l+1
  #initialized the elements with Uniform(0, 0.2) random deviates
  b <- list()
  
  h[[1]] <- rep(c(0), d[1])
  for (i in 1:(length(d) - 1)) {
    h[[i + 1]] <- rep(c(0), d[i + 1])
    W[[i]] <-
      matrix(runif(d[i] * d[i + 1], 0, 0.2), ncol = d[i + 1])
    b[[i]] <- runif(d[i + 1], 0, 0.2)
  }
  list(h = h, W = W, b = b)
}

softmax <- function(X) {
  return(exp(X) / rowSums(exp(X)))
}

#ReLU transformation
relu <- function(X) {
  # Function to 
  # Parameter:
  #   - 
  # Return:
  #   -
  
  result <- pmax(0, X)
  result <- matrix(result, ncol = ncol(X))
  return(result)
}

# forward function
forward <- function(nn, inp) {
  # Function to 
  # Parameter:
  #   - 
  # Return:
  #   -
  
  h_prev <- inp
  nn$h[[1]] <- h_prev
  for (l in 1:length(nn$W)) {
    if (l != length(nn$W)) {
      
      #except last and first layer of nodes, it is applied the ReLU transformation
      nn$h[[l + 1]] <- relu((h_prev %*% nn$W[[l]]) + nn$b[[l]])
      
    } else {
      
      #transformation of last output layer value to be a probability
      nn$h[[l + 1]] <- softmax(relu((h_prev %*% nn$W[[l]]) + nn$b[[l]]))
      
    }
    
    h_prev <- nn$h[[l + 1]]
  }
  return(nn)
}

relu_derivative <- function(x) {
  # Function to 
  # Parameter:
  #   - 
  # Return:
  #   -
  
  #when it is smaller than or equal to zero, it will become 0
  x[x <= 0] <- 0
  x[x > 0] <- 1
  return(x)
}

#Backward function
backward <- function(nn, k) {
  # Function to 
  # Parameter:
  #   - 
  # Return:
  #   - 
  
  
  #number of weight and h
  n_weight <- length(nn$W)
  n_h <- length(nn$h)
  batch_size <- length(k)
  
  k_matrix <-
    matrix(0, nrow = nrow(nn$h[[n_h]]), ncol = ncol(nn$h[[n_h]]))
  
  #searching when j equal to k_i
  k_matrix[cbind(1:length(k), k)] <- 1
  
  dh <- list()
  dW <- list()
  db <- list()
  
  #cases when j not equal to k_i
  d_L <- nn$h[[n_weight + 1]]
  #cases when j equal to k_i
  d_L <- (d_L - k_matrix)
  
  #last layers nodes derivatives, including W and b
  dh[[n_weight + 1]] <- d_L 
  
  #average the derivatives for each i in the set
  dW[[n_weight]] <- ((t(nn$h[[n_weight]]) %*% d_L)/ batch_size)
  db[[n_weight]] <- colMeans(d_L)
  
  #the derivatives of intermediate layer(s) start from second last layer
  for (l in (n_weight):2) {
    
    #when h is smaller than zero, it will become 0
    dh[[l]] <-
      dh[[l + 1]] %*% t(nn$W[[l]]) * relu_derivative(nn$h[[l]])
    
    #average the derivatives for each i in the set
    dW[[l - 1]] <- ((t(nn$h[[l - 1]]) %*% dh[[l]])/ batch_size)
    db[[l - 1]] <- colMeans(dh[[l]])
    
  }
  nn$dh <- dh
  nn$dW <- dW
  nn$db <- db
  
  return(nn)
}



train <- function(nn,inp,k,eta = .01,mb = 10,nstep = 10000) {
  # Function to train the model
  # Parameter:
  #   - 
  # Return:
  #   - 
  
  grad_update <- function(x, y, step_size) {
    # Function to update the W and b
    # Parameter:
    #   - x : list of previous layer(s)
    #   - y : list of derivative layer(s)
    #   -step_size: the step size that is eta
    # Return:
    #   - return the updated values
    
    x - (step_size * y)
  }
  
  n_miss_event <- 0
  
  for (step in 1:nstep) {
    mb_indices <- sample(nrow(inp), mb)
    X_train_mb <- inp[mb_indices,]
    y_train_mb <- k[mb_indices]
    
    nn <- forward(nn, X_train_mb)
    nn <- backward(nn, y_train_mb)
    
    nn$W <- Map(grad_update, nn$W, nn$dW, eta)
    nn$b <- Map(grad_update, nn$b, nn$db, eta)
    
    # Compute Loss
    output_layer_idx <- length(nn$h)
    
    y_pred <- nn$h[output_layer_idx][[1]]
    y_pred_int <- apply(y_pred, 1, which.max)
    miss_event <- sum(y_train_mb != y_pred_int)

    n_miss_event <- n_miss_event + miss_event

    if (step %% 100 == 0) {
      miss_class <- n_miss_event / (step * mb)
      msg <- paste("Step: ", step, ". MissClass: ", miss_class)
      print(msg)
    }
  }
  return(nn)
}


main <- function() {
  data(iris)
  vocabs <- c(unique(iris[, 5]))
  iris$k <- match(iris[, 5], vocabs)
  
  #setup the
  d <- c(4, 8, 7, 3)
  # Best Setup so far
  #d <- c(4, 64, 32, 3)
  nn <- netup(d)
  print(nn)
  
  #ratio_train <- 0.8
  #idx_train <- sample(nrow(iris), ratio_train * nrow(iris))
  
  #train_df <- iris[idx_train, ]
  #test_df <- iris[-idx_train, ]
  train_df <- iris[-seq(5, nrow(iris), 5), ]
  test_df <- iris[seq(5, nrow(iris), 5), ]
  
  X_train <- matrix(unlist(train_df[, 1:4]), ncol = 4)
  y_train <- train_df$k
  X_test <- matrix(unlist(test_df[, 1:4]), ncol = 4)
  y_test <- test_df$k
  
  nn <-
    train(nn,inp = X_train,k = y_train,eta = .01,mb = 10,nstep = 10000)

  nn <- forward(nn, X_test)
  offset_layer <- length(nn$h)
  y_prob <- nn$h[[offset_layer]]
  y_pred <- apply(y_prob, 1, which.max)
  print(table(y_test,y_pred))
  miss_event <- sum(y_test != y_pred)
  print(paste("Misclassification Rate: ", miss_event/length(y_test)))
}

system.time(main())
# Rprof()
# main()
# Rprof(NULL)
# summaryRprof()

