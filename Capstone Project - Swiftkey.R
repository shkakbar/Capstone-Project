################# ~~~~~~~~~~~~~~~~~ ######## ~~~~~~~~~~~~~~~~~ #################
## title: Data Science Capstone Project
## author: "Akbarali Shaikh"
## date: "07/09/2019"
## Objective: Data Science - Capstone Project Submisssion
##   Step 1: Understanding the problem
##   Step 2: Download file
##   Step 3: Load file - only English 
##   Step 4: Clean the file
##   Step 5: Exploratory analysis
##   Step 6: Statistical modeling
##   Step 7: Predictive modeling
##   Step 8: Creative exploration
##   Step 9: Creating a data product
##   Step 10: Creating a short slide deck pitching your product
################# ~~~~~~~~~~~~~~~~~ ######## ~~~~~~~~~~~~~~~~~ #################

## Load Required Libraries
library(tm)
library(reshape2)
library(ggplot2)
##library(RWeka)

## Set default setting
set.seed(101)

#library(dplyr)

##Read file and load into sample Text file
################################-------------------------------------------
twitter_en <- readLines("./Train_DS/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)
blogs_en <- readLines("./Train_DS/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul=TRUE)
news_en <- readLines("./Train_DS/en_US/en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)

#sampleTwitter_en <- textFileSample1(twitter_en)
#sampleTwitter_en <- textFileSample1(blogs_en)
sampleTwitter_en <- textFileSample1(news_en)

textFileSample1 <- function(infile, outfile= "./Train_DS/en_US/alpha.txt", fraction=0.5) {
  nlines <- length(infile)
  selection <- rbinom(nlines, 1, fraction)
  close(nlines)
  conn <- file(outfile, "w+")
  
  for (i in 1:nlines) {
    if (selection[i]==1) { a<- cat(infile[i], file=conn, sep = "\n") }
  }
  close(conn)
}

# Data preprocessing
################################-------------------------------------------
## One of the first things required for natural language processing (NLP) 
  ## tasks is a corpus. In linguistics and NLP, 
  ## corpus (literally Latin for body) refers to a collection of texts. 
  ## Such collections may be formed of a single language of texts, 
  ## or can span multiple languages -- there are numerous reasons 
  ## for which multilingual corpora (the plural of corpus) may be useful. 
  ## Corpora may also consist of themed texts (historical, Biblical, etc.). 
  ## Corpora are generally solely used for statistical linguistic analysis 
  ## and hypothesis testing.

makeCorpus <- function(filepath) {
  conn <- file(filepath, "r")
  fulltext <- readLines(conn)
  close(conn)
  
  vs <- VectorSource(fulltext)
  Corpus(vs, readerControl=list(readPlain, language="en", load=TRUE))
}

news_corpus <- makeCorpus("./Train_DS/en_US/alpha.txt")



## Transformation - cleaning of text 
#################################################################
## .1. make all text to lower case
news_corpus_proc <- tm_map(news_corpus, content_transformer(tolower))

## .2. remove Stop Words - for e.g. top words are generally the most 
  ## common words in a language. such as “the”, “a”, “an”, “in”

countWords <- function(filepath, pattern) {
  conn <- file(filepath, "r")
  fulltext <- readLines(conn)
  close(conn)
  
  count <- 0
  for (i in 1:length(fulltext)) {
    findr <- gregexpr(pattern, fulltext[i])
    if (findr[[1]][1]>0) {
      count <- count + length(findr[[1]])
    }
  }
  count
}

news_file <- "./Train_DS/en_US/alpha.txt"

# Adding the length of the file (in lines) to count the last word in each
# line, which is not surrounded by spaces
totwords <- countWords(news_file, " * ") + 10148

# Count the ocurrences of very common stopwords
mystopwords <- c(" [Aa]nd ", " [Ff]or ", " [Ii]n ", " [Ii]s ", " [Ii]t ",
                 " [Nn]ot ", " [Oo]n ", " [Tt]he ", " [Tt]o ")
totstops <- sum(sapply(mystopwords,
                       function(x) { countWords(news_file, x) }))
totstops/totwords


## Main text
news_corpus_proc <- tm_map(news_corpus_proc, removeWords,
                           stopwords(kind="en"))

## Removing Punctuation & Numbers
news_corpus_proc <- tm_map(news_corpus_proc, removePunctuation)
news_corpus_proc <- tm_map(news_corpus_proc, removeNumbers)

## Remove whitespace
news_corpus_proc <- tm_map(news_corpus_proc, stripWhitespace)


## Data Exploration
################################################################## 
dtm <- DocumentTermMatrix(news_corpus_proc)

dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)


########################################################################


options(mc.cores=1)
twogramTokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min=2, max=2))
}

dtm2 <- DocumentTermMatrix(news_corpus_proc,
                           control=list(tokenize=twogramTokenizer))

# I need to remove most of the sparse elements, otherwise I cannot
# allocate memory for the matrix object
dtm2_ns <- removeSparseTerms(dtm2, 0.998)
dtm2.matrix <- as.matrix(dtm2_ns)

wordcount <- colSums(dtm2.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Two-grams in Corpus")
fig <- fig + ylab("Count")
print(fig)



################################-------------------------------------------
