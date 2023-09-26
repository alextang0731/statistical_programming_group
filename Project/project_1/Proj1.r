a <- scan("./resource/4300-0.txt",what="character",skip=73,nlines=32858-73) 
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("
split_punct = function(x){ #function to separate the punctuation marks
  ii <- grep(",|\\.|;|!|:|\\?",x)
  xs <- rep("",length(ii)+length(x))
  iis <- ii+1:length(ii)
  xs[iis] <- substr(x[ii],nchar(x[ii]),nchar(x[ii]))
  xs[-iis] <- gsub(",|\\.|;|!|:|\\?","",x)
  xs
}

unique = function(y){ #used to find the vector, b, of unique words in the Ulysses text, a
  
}