#setwd("~/OneDrive - SEI/ESHackathon/4.Remote 2020/doi2abstract")
dois <- read.csv(file.choose(), header=TRUE)
attach(dois)
names(dois)
str(dois)

#start URL
#url <- "http://doi.org/10.1186/s13750-020-00191-y"
url <- as.character(doi[3])

#extract text from URL (no field codes)
library(htm2txt)
text <- gettxt(url)

#split text by line breaks
lines <- strsplit(text, "\n")[[1]]
if(any(lines %in% c(" ", ""))){
  lines <- lines[-which(lines %in% c(" ", ""))]
}

#i) check if the abstract is split into lines in the first 100 words
##detect the start of the abstract
get_abstract <- function(text){

  #find first mention of abstract
  header <- data.frame(lapply(sections$abstract, function(x){
    z <- grep(x, text, ignore.case = TRUE)
    rbind(z, nchar(trimws(tm::removeNumbers(tm::removePunctuation(text[z])))) - nchar(x))
  }))[1,1]

  wc1 <- length(strsplit(text[header], " ")[[1]])
  wc2 <- length(strsplit(text[header+1], " ")[[1]])

if(wc1>100 & wc1 <400){
  abstract <- text[header]
}else if(wc2 > 100 & wc2 <400){
abstract <- text[header+1]
}
  length(strsplit(text[header+1], " ")[[1]])

}

candidates <- data.frame(lapply(lookup, function(x) {

}))

##separate abstract from rest of the text at a maximum of 8 lines from the word 'Abstract'
library(ngram)
wordcount(lines[abstract+1])
##if line break occurs within the first 200 words of the abstract, extract the full abstract
library(stringr)
abstract <- if(wordcount(lines[abstract+1]) > 200){
  lines[abstract+1]
} else { #otherwise, extract the 10 lines after the first mention of the abstract
  abstract <- lines[abstract:(abstract+10)]
  abstract <- paste(abstract, collapse = ".") #then combine lines
  if(wordcount(abstract)<350){ #if the abstract is already less than 350 words, print abstract
    print(abstract)
  } else {
    word(string = abstract, start = 1, end = 350, sep = fixed(" ")) #otherwise, extract the first 350 words
    print(abstract)
  }
}



##suggestions
#at the moment, if the abstract is less than 200 words, it will look for more text.
#instead, it would be better to
  #i) check whether text occurrs before the words 'Introduction' or 'Background'
  #ii) if so, extract whole 'line' as the abstract
  #iii) otherwise, run the above code

##abstract scenarios
#1. sectioned abstracts using paper subsection titles (background/methods/results/discussion)
  #solution: extract all text between first occurrence of 'Abstract' and second occurrence of 'Introduction' or 'Background'
#2. very short abstract
  #solution: extract any text that occurrs between 'Abstract' and 'Background' or 'Introduction'
#3. missing abstract
  #provide suitable error message




#-----
#works for Environmental Evidence/BMC abstracts or those split by lines
#detect start of the abstract
abstract <- grep("abstract", tolower(lines))
abstract <- abstract[which.min(nchar(lines[abstract]))]
lines[abstract]
#separate abstract from rest of the text at a maximum of 8 lines from the word 'Abstract'
abstract <- lines[abstract:(abstract+8)]
library(ngram)
wordcount(abstract)
#combine lines into a single string
abstract <- paste(abstract, collapse = ".")
#if the string is longer than 350 words, just return the first 350 words
library(stringr)
abstract <- if(wordcount(abstract) > 350){
  word(string = abstract, start = 1, end = 350, sep = fixed(" "))
} else {
  abstract <- abstract
}
