library(dplyr)
# Set working Directory
setwd("/Users/oindrilasen/WORK_AREA/Data Science/Springboard/Exercise/Bag_of_Words")
#Read the data
tweets <-read.csv("tweets.csv",stringsAsFactors = FALSE)
glimpse(tweets)
#------------------------------------------------------------------
# Data Cleaning 
#------------------------------------------------------------------
# install library "tm"
#install.packages("tm")
# load library "tm"
library(tm)
# create a corpus
tweets_corpus <- Corpus(VectorSource(tweets$Tweet))
tweets_corpus
summary(tweets_corpus)
#inspect the first tweet
writeLines(as.character(tweets_corpus[[1]]))
#------------------------------------------------------------------
# Preprocessing
#------------------------------------------------------------------
getTransformations()
# to lower case
tweets_corpus <- tm_map(tweets_corpus, tolower)
# remove punctuation
tweets_corpus <- tm_map(tweets_corpus, removePunctuation)
# remove stop words
stopwords("english")
tweets_corpus <- tm_map(tweets_corpus, removeWords,c("apple",stopwords("english")))
# remove extra white space
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)
# Stem Document
tweets_corpus <- tm_map(tweets_corpus, stemDocument)
writeLines(as.character(tweets_corpus[[1]]))
# Document term matrix
dtm <- DocumentTermMatrix(tweets_corpus)
dtm
# check total frequency of words
freq <- colSums(as.matrix(dtm))
freq
# find the terms use at least 20 times
findFreqTerms(dtm,lowfreq = 20)
sparse_tweets <- removeSparseTerms(dtm, 0.995)
sparse_tweets
str(sparse_tweets)
# convert to data frame
sparse_tweets <- data.frame(words=names(freq), freq=freq)
  #data.frame(word=names(freq), freq=freq)
  #as.data.frame(as.matrix(sparse_tweets))
str(sparse_tweets)
head(sparse_tweets)
summary(sparse_tweets$freq)
# Plot terms frequencies
library(ggplot2)
ggplot(subset(sparse_tweets,freq>20), aes(words,freq))+
  geom_bar(stat="identity")+
  scale_y_continuous(breaks = c(0,50,30),limits = c(0,100))+
  theme(axis.text.x=element_text(angle=45, hjust=1))
# Word cloud
#install.packages("wordcloud")
library(wordcloud)
wordcloud(sparse_tweets$words,freq,min.freq = 20)
# add colours
wordcloud(sparse_tweets$words,freq,min.freq = 20, colors = brewer.pal(6,"Dark2"))
#install.packages("SnowballC")