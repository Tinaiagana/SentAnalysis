library(twitteR)
library(tm)
library(SnowballC)
library(syuzhet)
library(tidyverse)
consumer_key <- xxx
consumer_secret<-xxx
access_token <- xxx
access_secret <- xxx
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

tweets.EM <- searchTwitter(searchString = "Tesla + Stock -filter:retweets", n=2000,
            since ="2019-03-12", retryOnRateLimit = 80)

n.tweets.EM <- length(tweets.EM)
tweets.EM <- strip_retweets(tweets.EM)

tweets.EM.df <- twListToDF(tweets.EM)
head(tweets.EM.df)
head(tweets.EM.df$text)

#CLeanup data!

tweets.EM.df2 <- gsub("http.*","",tweets.EM.df$text)
tweets.EM.df2 <- gsub("http.*","",tweets.EM.df2)
tweets.EM.df2 <- gsub("#.*","",tweets.EM.df2)
tweets.EM.df2 <- gsub("@.*","",tweets.EM.df2)
tweets.EM.df2 <- gsub("@\\w+", "", tweets.EM.df2)
tweets.EM.df2 <- gsub('[[:punct:]]', '', tweets.EM.df2)
tweets.EM.df2 <- gsub('[[:cntrl:]]', '', tweets.EM.df2)
tweets.EM.df2 <- gsub("[[:digit:]]", "", tweets.EM.df2)
tweets.EM.df2 <- gsub("[ \t]{2,}", "", tweets.EM.df2)
tweets.EM.df2 <- gsub("^\\s+|\\s+$", "", tweets.EM.df2)
head(tweets.EM.df2)


#getting sentiment score for each tweet
word.EM.df <- as.vector(tweets.EM.df2)

emotion.EM.df <- get_nrc_sentiment(word.EM.df)

emotion.EM.df2 <- cbind(tweets.EM.df2, emotion.EM.df) 

head(emotion.EM.df2)

sent.value <- get_sentiment(word.EM.df)
most.positive <- word.EM.df[sent.value == max(sent.value)]
head(most.positive)

most.negative <- word.EM.df[sent.value == min(sent.value)]
head(most.negative)
positive.tweets <- word.EM.df[sent.value > 0]

head(positive.tweets)

negative.tweets <- word.EM.df[sent.value < 0]
head(negative.tweets)

neutral.tweets <- word.EM.df[sent.value == 0]
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)
table(category_senti)

