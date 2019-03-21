library(twitteR)
library(tm)
library(SnowballC)
library(syuzhet)
library(tidyverse)
library(wordcloud)
consumer_key <- "ZLX0zDj47f35gmRlPDmyxqLuJ"
consumer_secret<- "aWawo7GN7SJxFS1X8NBrJDpe4UuOZicQUzVEtf23AK6D6JJUr7"
access_token <- "107991817-ekKpeILEER2GrZaIr3H51uBMsTDYIL6CTtAi2xiD"
access_secret <- "iHlVgWV6TPF20AlmqZaAUyGDvCULXEUyMIr0L1jCRWwzA"
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

tweets <- searchTwitter(searchString = "Tesla + Stock -filter:retweets", n=2000,
                           since ="2019-03-15", retryOnRateLimit = 80)

n.tweets <- length(tweets.EM)
tweets <- strip_retweets(tweets)

tweets.df <- twListToDF(tweets)
head(tweets.df)
head(tweets.df$text)
write.csv(tweets.df,'tweetsEM.csv')

#CLeanup data!

tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("http.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@.*","",tweets.df2)
tweets.df2 <- gsub("@\\w+", "", tweets.df2)
tweets.df2 <- gsub('[[:punct:]]', '', tweets.df2)
tweets.df2 <- gsub('[[:cntrl:]]', '', tweets.df2)
tweets.df2 <- gsub("[[:digit:]]", "", tweets.df2)
tweets.df2 <- gsub("[ \t]{2,}", "", tweets.df2)
tweets.df2 <- gsub("^\\s+|\\s+$", "", tweets.df2)
head(tweets.df2)


## Making a wordcloud
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
tweetsC <- Corpus(VectorSource(tweets.df2))
head(tweets)


tdm <- TermDocumentMatrix(tweetsC)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


wordcloud(d$word,d$freq, min.freq=30, min.words=100,max.words=200,
          random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE,)



#getting sentiment score for each tweet

word.df <- as.vector(tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)

sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
head(most.positive)

most.negative <- word.df[sent.value == min(sent.value)]
head(most.negative)
positive.tweets <- word.df[sent.value > 0]

head(positive.tweets)

negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)
table(category_senti)

