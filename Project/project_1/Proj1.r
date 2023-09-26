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
b

