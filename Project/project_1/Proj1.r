## This following code will produce 2nd order Markov model for next-token prediction.
# Contributions:
# 1. All: prepare the data and github
# 2. Alex: Build pre-processing part (1-6)
# 3. Yuna: Build Pair and Triplet of the words
# 4. Alim: Build the Markov model, baseline, and create the documentation.

## ------------- Set up the parameters
m <- 1000 # threshold number of occurrences


## ------------- Set up the parameter above.

## Preparation

# Load the dataset
a <- scan("./resource/4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)

# Select the content by removing certain text by pattern i.e., removing text vefore "_("
a <- gsub("_(", "", a, fixed = TRUE) ## remove "_("


## Create a function to split the text and pre-processing. (tokenizer)
split_punct <- function(text_input) {
  ii <- grep(",|\\.|;|!|:|\\?", text_input) ## checking which string containing punctuation
  xs <- rep("", length(ii) + length(text_input)) ## create a new empty vector
  iis <- ii + 1:length(ii) ## stated where should punctuation go in xs?
  xs[iis] <- substr(text_input[ii], nchar(text_input[ii]), nchar(text_input[ii])) ## insert the punctuation
  xs[-iis] <- gsub(",|\\.|;|!|:|\\?", "", text_input) ## insert the string without punctuation
  return(xs)
}


## Text Preprocessing - question no 6
new_a <- split_punct(a) # applying the split_punct function
unique_words <- unique(tolower(new_a)) # generate a unique words from a
matching_index <- match(tolower(new_a), unique_words) # finding the index of unique words
count <- tabulate(matching_index) # counting the unique words
b <- unique_words[count > m] # listing out the element that appear many times
b_count <- count[count > m] # get the frequency of b



## Creating matrix 3D Triplet and 2D Pair
# To match the text a based on the most common words index.
matching_index <- match(tolower(new_a), b) # finding the index of common words
matching_shifted_one <- c(matching_index[-1]) # Shift the index vector by one place
matching_shifted_two <- c(matching_index[-(1:2)]) # Shift the index vector by one place

# To make the length of the vectors the same
matching_index <- matching_index[1:length(matching_shifted_two)] # Trim the last 2 index
matching_shifted_one <- matching_shifted_one[1:length(matching_shifted_two)] # Trim the last 1 index

# To make a 3D matrix (m, m, m)
T_matrix <- cbind(matching_index, matching_shifted_one, matching_shifted_two) # Create the T matrix (with NA)
common_triplets <- T_matrix[rowSums(is.na(T_matrix)) == 0, ] # Identify common word triplets (drop triplets that contain an NA)

# To make a 2D Matrix (m, m)
P_matrix <- cbind(matching_index, matching_shifted_one) # Create the P matrix
common_pairs <- P_matrix[rowSums(is.na(P_matrix)) == 0, ] # Identify common word pair (drop pairs that contain an NA)


## Create a 2nd-orders Markov Model
model_markov_2nd <- function(vocab, common_triplets, word1, word2) {
  # function to predict the 3rd column based on 1st and 2nd columns
  
  # Given word 1 and word 2, get the index
  idx_i <- which(vocab == word1)
  idx_j <- which(vocab == word2)
  
  # Match the index k[i] to the first column, and k[j] to the second column
  temp_match_1 <- match(common_triplets[, "matching_index"], idx_i)
  temp_match_2 <- match(common_triplets[, "matching_shifted_one"], idx_j)
  
  # Get the overlapping. ex: c(NA, NA, 1) + c(NA, 1, 1) = NA NA  2
  temp_match <- temp_match_1 + temp_match_2
  
  # Get the subset of matrix where k[i] and k[j] is not NA
  common_triplets_filter <- common_triplets[!is.na(temp_match), ]
  
  # Logic to return NA if there is no rows in the data. 
  # Later, we will use 1st-order of Markov model (pair) instead. 
  if (length(common_triplets_filter) == 0) {
    next_token <- NA # return NA if there's no matched row
  } else if (length(common_triplets_filter['matching_shifted_two']) == 1) {
    # There's no other choice but one value
    next_token <- b[common_triplets_filter['matching_shifted_two'][1]] # return the only value
  } else {
    # Sample from multiple options/values
    next_token <- sample(b[common_triplets_filter[, "matching_shifted_two"]], 1)   # return one value from sampling the choices
  }
  
  # Return the next token
  return(next_token)
}

## Create an 1st-orders Markov Model
model_markov_1st <- function(vocab, common_pairs, word1) {
  # function to predict the 2nd column based on 1st columns
  
  # Given word 1, get the index
  idx_i <- which(vocab == word1)
  
  # Match the index k[j] to the first column
  temp_match <- match(common_pairs[, "matching_index"], idx_i)
  
  # Get the subset of matrix where k[j] is not NA
  common_pairs_filter <- common_pairs[!is.na(temp_match), ]
  
  # Logic to return NA if there is no rows in the data. 
  # Later, we will use probability of the common words instead. 
  if (length(common_pairs_filter) == 0) {
    next_token <- NA  # return NA if there's no matched row
  } else if (length(common_pairs_filter['matching_shifted_one']) == 1) {
    # There's no other choice but one value
    next_token <- b[common_pairs_filter['matching_shifted_one'][1]] # return the only value
  } else {
    # Sample from multiple options/values
    next_token <- sample(b[common_pairs_filter[, "matching_shifted_one"]], 1)  # return one value from sampling the choices
  }
  
  # Return the next token
  return(next_token)
}

## Create a model that returns the word from the common words.
model_common_words <- function(vocab, vocab_freq) {
  # Get the sample from the vocab (unique)
  # Note: number of occurrence (frequency) will be the probability.
  next_token <- sample(vocab, 1, prob = vocab_freq)
  return(next_token)
}

## Final Models: combination of 2nd and 1st orders markov with common words.
# If the 2nd order unable to generate the prediction, it will use the 1st order
# If the 1st order unable to generate the prediction, it will use the common word.
model_next_token <- function(vocab, vocab_freq, common_pairs, common_triplets, word1, word2) {
  # A function to get prediction from the 2nd order markov
  next_token <- model_markov_2nd(vocab, common_triplets, word1, word2)
  model_source <- "triplet"
  
  # Logic to check whether or not the model give at least 1 prediction
  if (is.na(next_token) == TRUE) {
    # If no (there's no prediction)
    # Get the prediction from the 1st order markov
    next_token <- model_markov_1st(vocab, common_pairs, word2)
    model_source <- "pair"
  }
  
  # Logic to check whether or not the model give at least 1 prediction
  if ((is.na(next_token) == TRUE)) {
    # If no (there's no prediction)
    # Get the prediction from the common word
    next_token <- model_common_words(vocab, vocab_freq)
    model_source <- "common_word"
  }
  
  return(c(next_token, model_source))
}



## Tabulate the 50 samples of Markov Model
cat('2nd-order Markov Model')
for (x in 1:50) {
  # Collect a word from common word.
  word_i <- model_common_words(vocab=b, vocab_freq=b_count)
  # cat(word_i[1], '\t') # for debugging
  
  # Collect the second word from pair or common words
  word_j <- model_next_token(b, b_count, common_pairs, common_triplets, "", word_i[1])
  # cat(word_j[1], '\t') # for debugging
  
  # Collect the third word from the triplet, pair, or common word
  word_k <- model_next_token(b, b_count, common_pairs, common_triplets, word_i[1], word_j[1])
  
  cat(paste(x, word_i[1], word_j[1], word_k[1], '\n', sep=' '))
}

## Tabulate the 50 samples of Common Word Model
cat('Common Word Model')
for (x in 1:50) {
  # Collect a word from common word.
  word_i <- model_common_words(vocab=b, vocab_freq=b_count)
  # cat(word_i[1], '\t') # for debugging
  
  # Collect the second word from pair or common words
  word_j <- model_common_words(vocab=b, vocab_freq=b_count)
  # cat(word_j[1], '\t') # for debugging
  
  # Collect the third word from the triplet, pair, or common word
  word_k <- model_common_words(vocab=b, vocab_freq=b_count)
  
  # Print out the result using cat
  cat(paste(x, word_i[1], word_j[1], word_k[1], '\n', sep=' '))
}