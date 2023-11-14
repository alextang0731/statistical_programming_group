### Group Members:
# Group 11
# 1. Alex Tang s2592944
# 2. Alim Hanif s2442474
# 3. Yuna Choi s2120762

# Github Repository:
# https://github.com/alextang0731/statistical_programming_group
# File:
# /blob/main/Project/project_4/G11.R

### Contributions:
# netup function : Alex
# forward function: Yuna and Alex
# backward and train function: Alim

#Note: Everyone is contributed equally

# =======================================================#
# Overview:
# The code generates and trains a neural network using mini-batch gradient
# descent, involves iteratively updating the network's weights and biases
# based on the gradients of the loss function, computed using small batches
# of training data. After that, it uses the "iris" dataset and trains a
# 4-8-7-3 network as an example and the task is to predict the Species.


# Note: this code encode the labels into an integer [1,2,3]
#       as it is a multi-class label task.
# =======================================================#

# ====== Utilities ======

softmax <- function(X) {
  # Function to implement Softmax function: exp(x)/ sum(exp(x))
  # Arguments:
  #   - X: input vector/matrix
  # Return:
  #   - X after Softmax function has been implemented.
  return(exp(X) / rowSums(exp(X)))
}

relu <- function(X) {
  # Function to implement ReLU function: max(0, X)
  # Arguments:
  #   - X: input vector/matrix
  # Return:
  #   - X after ReLU function has been implemented.
  
  result <- pmax(0, X)
  result <- matrix(result, ncol = ncol(X))
  return(result)
}

relu_derivative <- function(x) {
  # Function to calculate the derivative of ReLU
  # Arguments:
  #   - X: input vector/matrix
  # Return:
  #   - X after derivative of ReLU function has been implemented.
  
  # when it is smaller than or equal to zero, it will become 0
  x[x <= 0] <- 0
  x[x > 0] <- 1
  return(x)
}

grad_update <- function(wb, grad, eta) {
  # Function to update the parameters with computed gradients
  # Arguments:
  #   - wb : list of parameters at the current time
  #   - grad : list of gradient to be implemented to wb
  #   - eta: the step size.
  # Return:
  #   - return the updated parameters
  
  return(wb - (eta * grad))
}

# ====== Main Functions ======

netup <- function(d) {
  # Function to setup the Neural Network (NN)
  # Arguments:
  #   - d: a vector of number of nodes in each layers
  # Return:
  #   - nn (list): Neural Network list that contains:
  #     -> h: vector of length d[l] contains the nodes for layer l
  #     -> W: the weight matrix linking layer l to layer l+1
  #     -> b: the list of vector bias
  
  # Setup placeholder for h, W, and b.
  h <- list()
  W <- list()
  b <- list()
  
  # Assign the matrix and vector to the placeholder
  h[[1]] <- rep(c(0), d[1])
  for (i in 1:(length(d) - 1)) {
    h[[i + 1]] <- rep(c(0), d[i + 1])
    W[[i]] <-
      matrix(runif(d[i] * d[i + 1], 0, 0.2), ncol = d[i + 1])
    b[[i]] <- runif(d[i + 1], 0, 0.2)
  }
  nn <- list(h = h, W = W, b = b)
  return(nn)
}

forward <- function(nn, inp) {
  # Function to forward-propagation in NN
  # Arguments:
  #   - nn: network list
  #   - inp: vector of input values for the first layer
  # Return:
  #   - nn: updated NN with computed offset (h) from inp data.
  
  h_prev <- inp
  nn$h[[1]] <- h_prev
  for (l in 1:length(nn$W)) {
    if (l != length(nn$W)) {
      # apply ReLU function on intermediate layers
      nn$h[[l + 1]] <- relu((h_prev %*% nn$W[[l]]) + nn$b[[l]])
    } else {
      # apply SoftMax function to the output layer
      nn$h[[l + 1]] <- softmax((h_prev %*% nn$W[[l]] + nn$b[[l]]))
    }
    
    h_prev <- nn$h[[l + 1]]
  }
  return(nn)
}

backward <- function(nn, k) {
  # Function to perform back-propagation (backward)
  #   with mini-batch gradient descent.
  # Arguments:
  #   - nn: network list from forward propagation function
  #   - k: output class k (label)
  # Return:
  #   - updated nn with gradient computation for w, b, and h
  
  # number of weight and h
  n_weight <- length(nn$W)
  n_h <- length(nn$h)
  batch_size <- length(k)
  
  # One hot encoding for k matrix (label)
  k_matrix <-
    matrix(0, nrow = nrow(nn$h[[n_h]]), ncol = ncol(nn$h[[n_h]]))
  k_matrix[cbind(1:length(k), k)] <- 1
  
  # Setup a placeholder for the gradients
  dh <- list()
  dW <- list()
  db <- list()
  
  # Get the Likelihood and subtract by 1 for true label
  d_L <- nn$h[[n_weight + 1]]
  d_L <- (d_L - k_matrix)
  
  # Compute the gradient for the output layer
  dh[[n_weight + 1]] <- d_L
  dW[[n_weight]] <- ((t(nn$h[[n_weight]]) %*% d_L) / batch_size)
  db[[n_weight]] <- colMeans(d_L)
  
  # Compute the gradient for the intermediate layers
  for (l in (n_weight):2) {
    # Compute the gradient of the offset, with relu derivative.
    dh[[l]] <-
      dh[[l + 1]] %*% t(nn$W[[l]]) * relu_derivative(nn$h[[l]])
    dW[[l - 1]] <- ((t(nn$h[[l - 1]]) %*% dh[[l]]) / batch_size)
    db[[l - 1]] <- colMeans(dh[[l]])
  }
  
  nn$dh <- dh
  nn$dW <- dW
  nn$db <- db
  
  return(nn)
}

train <- function(nn, inp, k, eta = .01, mb = 10, nstep = 10000) {
  # Function to train the NN model
  # Arguments:
  #   - nn: network list
  #   - inp: vector of input values for the first layer
  #   - k: output class k (label)
  #   - eta: eta: the step size. (default: .01)
  #   - mb: batch size. (default: 10)
  #   - nstep: number of step for training. (default: 10000)
  # Return:
  #   - nn: trained model from the input
  
  n_miss_event <- 0 # for display
  
  # Training loop for nsteps
  n_data <- nrow(inp)
  for (step in 1:nstep) {
    mb_indices <- sample(n_data, mb)
    X_train_mb <- inp[mb_indices, ]
    y_train_mb <- k[mb_indices]
    
    # Forward and Backward propagration
    nn <- forward(nn, X_train_mb)
    nn <- backward(nn, y_train_mb)
    
    # Update the parameters with computed gradient
    nn$W <- Map(grad_update, nn$W, nn$dW, eta)
    nn$b <- Map(grad_update, nn$b, nn$db, eta)
    
    # Disclaimer!!!
    # We wrote the code below to monitor the metrics during training.
    # We hard-coded it to be `FALSE`, because it is not required
    # in the practical 4 sheet and the data is SMALL.
    # We understand that monitoring the metrics during
    # training is crucial. We can stop the training
    # when the model behaves unexpectedly.
    
    is_display <- FALSE
    if (is_display) {
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
  }
  return(nn)
}

# === Demonstrate the Neural Network (NN) Model training on Iris Dataset ===

set.seed(0)
data(iris)
vocabs <- c(unique(iris[, 5]))
iris$k <- match(iris[, 5], vocabs)

# setup the Neural Network (NN) architecture
d <- c(4, 8, 7, 3)
nn <- netup(d)
offset_layer <- length(nn$h)

# Train & Test data splitting
train_df <- iris[-seq(5, nrow(iris), 5), ]
test_df <- iris[seq(5, nrow(iris), 5), ]

# Preprocess the data: Transform into matrix & vector
X_train <- matrix(unlist(train_df[, 1:4]), ncol = 4)
y_train <- train_df$k
X_test <- matrix(unlist(test_df[, 1:4]), ncol = 4)
y_test <- test_df$k
n_val_data <- length(y_test)


# Evaluate the model before training
nn <- forward(nn, X_test)
y_prob_pre <- nn$h[[offset_layer]]
y_pred_pre <- apply(y_prob_pre, 1, which.max)
miss_event_pre <- sum(y_test != y_pred_pre)

# Model Training (fitting)
nn <-
  train(nn, inp = X_train, k = y_train, eta = .01, mb = 10, nstep = 10000)

# Model Inference & calculate the miss-classification rate
nn <- forward(nn, X_test)
y_prob_post <- nn$h[[offset_layer]]
y_pred_post <- apply(y_prob_post, 1, which.max)
miss_event_post <- sum(y_test != y_pred_post)

print(paste("[Pre] Misclassification Rate: ", miss_event_pre / n_val_data))
print(paste("[Post] Misclassification Rate: ", miss_event_post / n_val_data))
