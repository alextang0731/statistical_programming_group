a <- scan("./resource/4300-0.txt",what="character",skip=73,nlines=32858-73) 
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("


split_punct = function(x){ #function to separate the punctuation marks
  ii <- grep(",|\\.|;|!|:|\\?",x) ##checking which string containing punctuation
  xs <- rep("",length(ii)+length(x)) ##create a new empty vector
  iis <- ii+1:length(ii) ## stated where should punctuation go in xs? 
  xs[iis] <- substr(x[ii],nchar(x[ii]),nchar(x[ii])) ## insert the punctuation
  xs[-iis] <- gsub(",|\\.|;|!|:|\\?","",x) ## insert the string without punctuation
}

new_a <- split_punct(a)
count <- tabulate(match(tolower(new_a),unique(tolower(new_a))))
m <- 1000
b <- new_a[count>m]
