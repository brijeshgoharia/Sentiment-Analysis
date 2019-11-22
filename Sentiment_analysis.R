rm(list=ls())
getwd()

#install all the required packages
install.packages("tm")
library(tm)
install.packages("SnowballC")
library("SnowballC")
install.packages("twitteR")
library(twitteR)
install.packages("SentimentAnalysis")
library(SentimentAnalysis)
install.packages("ROAuth")
library(ROAuth)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)
install.packages("httr")
library(httr)
install.packages("wordcloud")
library(wordcloud)
install.packages("sentimentr")
library(sentimentr)
install.packages("RCurl")
library(RCurl)
install.packages("syuzhet")
library(syuzhet)

#connect to twitter API

#oauth_endpoint(authorize = "https://api.twitter.com/oauth", access = "https://api.twitter.com/oauth/access_token")

# Authonitical keys
consumer_key <- 'fvyLg21twQGJwTzkRwfZ5KwWy'
consumer_secret <- 'JwNCdMbrpny3pe26fb541hx1mylfOidAGAnbvK3fuctUCW4Gkv'
access_token <- '2898687642-UEb54DLP1nHCz7Db8u4WdfyLvVUV9PQ2eHUsKmE'
access_secret <- 'APNBZbl1tOvmgy0dAfX4BathuvHs8yuyg4pWdNgybnE60'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- searchTwitter("Arya", n=2000, since = "2019-04-1", lang="en")
#Length of the tweets
n.tweet <- length(tweets)
n.tweet
#convert tweets into data frame
tweets.df <- twListToDF(tweets) 
head(tweets.df)
write.csv(tweets.df,"election2019.csv")

#Data Pre processing
tweets_text = tweets.df$text
#convert all text to lower case
tweets_text = tolower(tweets_text)
# Replace blank space (“rt”)
tweets_text = gsub("rt", "", tweets_text)
# Replace @UserName
tweets_text = gsub("@\\w+","",tweets_text)
# Remove punctuation
tweets_text = gsub("[[:punct:]]","",tweets_text)
# Remove links
tweets_text = gsub("http\\w+","",tweets_text)
# Remove tabs
tweets_text = gsub("[ |\t]{2,}","", tweets_text)

# Remove blank spaces at the beginning
tweets_text = gsub("^ ","", tweets_text)
tweets_text

#clean up by removing stop words
corpus = Corpus(VectorSource(tweets_text))
#corpus = orpus(VectorSource(tweets_text))
tweets.corpus = tm_map(corpus, removeWords, stopwords("english"))
tweets.corpus

#WordCloud
library(wordcloud)
pal = brewer.pal(8,"Dark2")
wordcloud(tweets.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#getting emotions using in-built function
library(syuzhet)
mysentiment_tweeter = get_nrc_sentiment((tweets_text))

#calculationg total score for each sentiment
Sentimentscores_tweeter<-data.frame(colSums(mysentiment_tweeter[,]))


names(Sentimentscores_tweeter)<-"Score"
Sentimentscores_tweeter<-cbind("sentiment"=rownames(Sentimentscores_tweeter),Sentimentscores_tweeter)
rownames(Sentimentscores_tweeter)<-NULL

*****************************************************************

#plotting the sentiments with scores
  library(ggplot2)

ggplot(data=Sentimentscores_tweeter,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tweeter")


