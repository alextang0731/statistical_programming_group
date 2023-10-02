a <- scan("./resource/4300-0.txt",what="character",skip=73,nlines=32858-73) 
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("

split_punct <- function(x){ #function to separate the punctuation marks
  ii <- grep(",|\\.|;|!|:|\\?",x) ##checking which string containing punctuation
  xs <- rep("",length(ii)+length(x)) ##create a new empty vector
  iis <- ii+1:length(ii) ## stated where should punctuation go in xs? 
  xs[iis] <- substr(x[ii],nchar(x[ii]),nchar(x[ii])) ## insert the punctuation
  xs[-iis] <- gsub(",|\\.|;|!|:|\\?","",x) ## insert the string without punctuation
  return(xs)
}

new_a <- split_punct(a) #applying the split_punct function
unique_words <- unique(tolower(new_a)) #unique words
matching_index <- match(tolower(new_a), unique_words ) #finding the index of unique words
count <- tabulate(matching_index) #counting the unique words
m <- 500 #threshold number of occurrences
b <- unique_words[count>m] #listing out the element that appear many times
b_count <- count[count>m]
b

### test case: `from`: `1083`
#test_idx <- which(unique_words=='from')
#test_is_true <- count[test_idx] == 1083

#7 
matching_index <- match(tolower(new_a), b) #finding the index of common words

### test case: `the`
#test_match_index = matching_index[8]
#test_ground_truth = new_a[8]
#test_is_true_the = b[test_match_index] == test_ground_truth

matching_shifted_one <- c(matching_index[-1]) # Shift the index vector by one place
matching_shifted_two <- c(matching_index[-(1:2)]) # Shift the index vector by one place

# Trim the last 2 rows/index
matching_index = matching_index[1:length(matching_shifted_two)]

# Trim the last 1 row/index
matching_shifted_one = matching_shifted_one[1:length(matching_shifted_two)]


# Create the T matrix
T_matrix <- cbind(matching_index, matching_shifted_one, matching_shifted_two)

# Identify common word triplets (drop triplets that contain an NA)
common_triplets <- T_matrix[rowSums(is.na(T_matrix)) == 0, ]

# Create the P matrix
P_matrix <- cbind(matching_index, matching_shifted_one)

# Identify common word pair (drop pairs that contain an NA)
common_pairs<- P_matrix[rowSums(is.na(P_matrix)) == 0, ]



### test case "from" "the"
# idx_i = which(b=='from')
# idx_j = which(b=='?')
# 
# temp_match = match(common_triplets, c(idx_i, idx_j))
# 
# temp_match_1 = match(common_triplets[, 'matching_index'], idx_i)
# temp_match_2 = match(common_triplets[, 'matching_shifted_one'], idx_j)
# 
# temp_match = temp_match_1+temp_match_2
# 
# common_triplets_filter = common_triplets[!is.na(temp_match),]
# 
# if (length(common_triplets_filter)==0) {
#   cat('zero')
# } else {
#   cat('haha')
# }
# 
# b[common_triplets_filter[, 'matching_index']]
# b[common_triplets_filter[, 'matching_shifted_one']]
# b[common_triplets_filter[, 'matching_shifted_two']]

##
get_prob_of_3rd <- function(vocab, common_triplets, word1, word2) {
  # vocab = b
  
  idx_i = which(vocab==word1)
  idx_j = which(vocab==word2)
  
  temp_match_1 = match(common_triplets[, 'matching_index'], idx_i)
  temp_match_2 = match(common_triplets[, 'matching_shifted_one'], idx_j)
  
  temp_match = temp_match_1+temp_match_2
  
  common_triplets_filter = common_triplets[!is.na(temp_match),]
  
  # b[common_triplets_filter[, 'matching_index']]
  # b[common_triplets_filter[, 'matching_shifted_one']]
  # b[common_triplets_filter[, 'matching_shifted_two']]
  
  if (length(common_triplets_filter)==0) {
    next_token = NA
  } else {
    next_token = sample(b[common_triplets_filter[, 'matching_shifted_two']], 1)
  }
  
  return(next_token)
}

get_prob_of_2nd <- function(vocab, common_pairs, word1) {
  # vocab = b
  idx_i = which(vocab==word1)
  
  temp_match = match(common_pairs[, 'matching_index'], idx_i)
  
  
  common_pairs_filter = common_pairs[!is.na(temp_match),]
  
  # b[common_pairs_filter[, 'matching_index']]
  # b[common_pairs_filter[, 'matching_shifted_one']]
  
  if (length(common_pairs_filter)==0) {
    next_token = NA
  } else {
    next_token = sample(b[common_pairs_filter[, 'matching_shifted_one']], 1)
  }  
  
  return(next_token)
}

get_prob_of_1nd <- function(vocab, vocab_freq) {
  # vocab = b
  next_token = sample(vocab, 1, prob=vocab_freq)
  return(next_token)
}

model_next_token_func <- function(vocab, vocab_freq, common_pairs, common_triplets, word1, word2){
  next_token = get_prob_of_3rd(vocab, common_triplets, word1, word2)
  model_source = 'triplet'
  
  if (is.na(next_token) == TRUE){
    next_token = get_prob_of_2nd(vocab, common_pairs, word2)
    model_source = 'pair'
  }
  
  if ( (is.na(next_token) == TRUE)) {
    next_token = get_prob_of_1nd(vocab, vocab_freq)
    model_source = 'common_word'
  }
  
  return(c(next_token, model_source))
}
# Interactive GUI.
while (TRUE) {
  model_input <- readline(prompt="Model Inputs (2 words): ")
  word1 = strsplit(model_input, " ")[[1]]
  word1 = tail(word1, 1)[1]
  
  word2 = strsplit(model_input, " ")[[1]]
  word2 = tail(word2, 2)[1]
  
  
  result = model_next_token_func(b, b_count, common_pairs, common_triplets, word1, word2)
  predicted_word = result[1]
  source_model = result[2]
  
  cat(paste(source_model, predicted_word, sep=": "))
  
}

