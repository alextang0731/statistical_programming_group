###Group Members:
#1. Alex Tang s2592944
#2. Alim Hanif s
#3. Yuna Choi s

## This following code will produce 2nd order Markov model for next-token prediction.
# Contributions:
# 1. All: prepare the data and github
# 2. Alex: Build pre-processing part (1-6)
# 3. Yuna: Build Pair and Triplet of the words
# 4. Alim: Build the Markov model, baseline, and create the documentation.
# 5. Alim and Alex: Work for the additional question

#Remarks: Everyone works on equally contribution and everyone involved in every question discussion

## ------------- Set up the parameters
m <- 1000 # threshold number of occurrences

## Preparation
# Load the dataset
a <- scan("./resource/4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)

# Select the content by removing certain text by pattern i.e., removing text vefore "_("
a <- gsub("_(", "", a, fixed = TRUE) ## remove "_("


## Create a function to split the text and pre-processing. (tokenizer)
split_punct <- function(text_input) {
  ## Function to split the text into token/word with preprocessing.
  ## Parameters:
  ##    text_input(str): string input to be preprocessed.
  ## return: 
  ##    vector of preprocessed tokens.  
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
  ## Function to predict the 3rd column based on 1st and 2nd columns
  ## Parameters:
  ##    vocab(vector): vocabulary of tokens
  ##    common_triplets(vector): all the possible triplets
  ##    word1: the N-2 (k[i]) word of the predicted text (N) 
  ##    word2: the N-1 (k[j]) word of the predicted text (N)
  ## return: 
  ##    the next token/word after word1 and word 2. It returns NA when no matched triplet has found.  
  
  # Given word 1 and word 2, get their indices
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
  ## Function to predict the 2nd column based on 1st columns
  ## Parameters:
  ##    vocab(vector): vocabulary of tokens
  ##    common_pairs(vector): all the possible pairs
  ##    word1: the N-1 (k[j]) word of the predicted text (N) 
  ## return: 
  ##    the next token/word after word1. It returns NA when no matched pair has found. 
  
  
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
  ## Function to get the sample from the vocab
  ## Parameters:
  ##    vocab(vector): vocabulary of tokens (unique)
  ##    vocab_freq(vector): the frequency of the vocab in the document. It will be used to maintain the probability during sampling.
  ## return: 
  ##    a word from the common words
  next_token <- sample(vocab, 1, prob = vocab_freq) # sample a word from vocab, with maintain the probability.
  return(next_token)
}

## Final Models: combination of 2nd and 1st orders markov with common words.
# If the 2nd order unable to generate the prediction, it will use the 1st order
# If the 1st order unable to generate the prediction, it will use the common word.
model_next_token <- function(vocab, vocab_freq, common_pairs, common_triplets, word1, word2) {
  ## Function to combine the triplet, pairs, and common word models.
  ## Parameters:
  ##    vocab(vector): vocabulary of tokens
  ##    common_pairs(vector): all the possible pairs
  ##    common_triplets(vector): all the possible triplets
  ##    word1: the N-2 (k[i]) word of the predicted text (N) 
  ##    word2: the N-1 (k[j]) word of the predicted text (N)
  ## return: 
  ##    - the next token/word
  ##    - source of the model ['triplet', 'pair', 'common_word']  
  
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

caps <- function(word) {
  ## Function to capitalize the first letter
  ## Parameters:
  ##    word: the targeted string
  ## return: 
  ##    string with a capitalized first letter
  first_char = substr(word,1,1) # get the first character
  first_char = toupper(first_char) # capitalize it
  rest_chars = substr(word,2,10^8) # get the res of the characters
  result = paste(first_char, rest_chars, sep='') # combine the first and the rest chars
  return(result)
}

# modify b with capitalized first letter into variable b_caps
b_caps = caps(b)

# calculate how many words with capitalized first letter on the document
matching_caps_index <- match(new_a, b_caps, nomatch=0)
count_caps <- tabulate(matching_caps_index) # counting the words
# calculate the ratio of capitalized first letter over the frequency
caps_ratio = count_caps/b_count
# select only those words that at least 50% were written with capitalized first letter.
b_index_caps = caps_ratio>0.5

# Not being used due to not memory efficient.
# Create vocab mix (caps and non caps).
#b_mix = rep(length(b))
#b_mix[b_index_caps] = b_caps[b_index_caps]
#b_mix[!b_index_caps] = b[!b_index_caps]


# Hence, create a vocab for the b caps
b_caps_vocab = tolower(b_caps[b_index_caps])

# To validate the list of word contains capitalized first letter and lower case are the same.
is_caps_validation = length(count_caps) == length(b_count)
if (is_caps_validation){
  cat("Warning: There's a problem with the capitalized word")
}

## Tabulate the 50 samples of Markov Chain Model
cat('2nd-order Markov Model')
text_result = c()
text_result_caps = c()

# Iterate 50 times to create a sentence with 2nd order markov model.
for (x in 1:50) {
  if (x == 1) {
    # generate first word from the common words.
    next_word <- model_common_words(vocab=b, vocab_freq=b_count)
    text_result <- c(text_result, next_word)
    
    # logic to check whether the predicted word should be with capitalized or not.
    if (next_word %in% b_caps_vocab)
    {text_result_caps <- c(text_result_caps, caps(next_word))}
    else
    {text_result_caps <- c(text_result_caps, next_word)}
  } else if ( x == 2) {
    # generate second word from a pair.
    word_i = text_result[length(text_result)]
    next_word <- model_next_token(b, b_count, common_pairs, common_triplets, "", word_i)[1]
    text_result <- c(text_result, next_word)
    
    # logic to check whether the predicted word should be with capitalized or not.
    if (next_word %in% b_caps_vocab)
    {text_result_caps <- c(text_result_caps, caps(next_word))}
    else
    {text_result_caps <- c(text_result_caps, next_word)}
    
  } else {
    # generate thr rest of the words from a triplet.
    word_i = text_result[length(text_result)-1]
    word_j = text_result[length(text_result)]
    next_word <- model_next_token(b, b_count, common_pairs, common_triplets, word_i, word_j)[1]
    text_result <- c(text_result, next_word)
    
    # logic to check whether the predicted word should be with capitalized or not.
    if (next_word %in% b_caps_vocab)
    {text_result_caps <- c(text_result_caps, caps(next_word))}
    else
    {text_result_caps <- c(text_result_caps, next_word)}
  }
}

## Tabulate the 50 samples of Common Word Model
text_result_baseline = c()
text_result_baseline_caps = c()
for (x in 1:50) {
  # get the next word from common words.
  next_word <- model_common_words(vocab=b, vocab_freq=b_count)
  text_result_baseline <- c(text_result_baseline, next_word)
  
  # logic to check whether the predicted word should be with capitalized or not.
  if (next_word %in% b_caps_vocab)
  {text_result_baseline_caps <- c(text_result_baseline_caps, caps(next_word))}
  else
  {text_result_baseline_caps <- c(text_result_baseline_caps, next_word)}    
  
}

# To print the results.
cat('2nd-order markov model: ')
cat(text_result) 
# Output Sample: the we , their it . . she of ! fell of in do till the up is ! , : him the her till boylan the of take the of , , to at any , even other two near , and than went if me night the ?

cat(text_result_caps)
# Output Sample: the we , their it . . she of ! fell of in do till the up is ! , : him the her till Boylan the of take the of , , to at any , even other two near , and than went if me night the ?"

# Word `Boylan` uses capital at the first latter.

cat('/n')
cat('common words: ')
cat(text_result_baseline)
# Output Example: seen my he after his of up i it and , half seen the an come head took it , said and , of he and be of their that’s the a to its : their . what know well up of that ) the but it miss his .

cat(text_result_baseline_caps)
# Output Example: seen my he after his of up I it and , half seen the an come head took it , said and , of he and be of their That’s the a to its : their . what know well up of that ) the but it Miss his .
#note: `I`, `That’s`, and `Miss` are mostly started with capital letter.



