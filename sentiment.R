setwd("F:/Twitter")
## Install/Load all relevant packages.
#install.packages("twitteR")
#install.packages("RCurl")
#install.packages("ROAuth") 
#install.packages("plyr")
#install.packages("wordcloud")
#install.packages("devtools")
#install.packages("tm")

#library

library(base64enc)
library(twitteR)
library(RCurl)
library(bitops)
library(ROAuth)
library(plyr)
library(devtools)
library(wordcloud)
library(tm)
library(SnowballC)

# TwitteR: Provides an interface to the Twitter web API.
# ROAuth: Provides an interface to the OAuth 1.0 specification,allowing users to authenticate via OAuth to the server of their choice.

devtools::install_github("hadley/httr", force=T)

# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")


#Twitter keys obtained from twitter API as strings
consumer_key='X'
consumer_secret='X'
access_token='xx'
access_secret='xxx'


setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

# lets pull tweets
tweetList=searchTwitter("#PadmaavatRow",n=5000,lang="en",resultType = "recent")

#the tweets comes in the form of a List. To convert it to a Data frame we use the following command.
tweets.df = ldply(tweetList, function(t) t$toDataFrame())

# Save it in a csv file.
write.csv(tweets.df,file="PadmaavatRow.csv")

#Read the csv File
tweets=read.csv("PadmaavatRow.csv")
str(tweets)

tweetsText=tweets$text


# now we set out cleaning the tweets

# remove retweet entities
tweetsText = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', "",tweetsText)
# remove at people
tweetsText = gsub('@:\\w+', "", tweetsText)

#remove html links
tweetsText = gsub('http:\\w+', "", tweetsText)

# now we set out cleaning the tweets

corp=Corpus(VectorSource(tweetsText))

corp_clean=tm_map(corp,PlainTextDocument)
corp_clean=tm_map(corp,removePunctuation)
corp_clean=tm_map(corp_clean,content_transformer(tolower))
corp_clean=tm_map(corp_clean,removeNumbers)
corp_clean=tm_map(corp_clean,stemDocument)

corp_clean=tm_map(corp_clean,removeWords,stopwords("english"))
corp_clean=tm_map(corp_clean,removeWords,c("padmaavat","sanjayleelabhansali","padmavaticontroversi"))


# before we go to sentiment analysis let us see a word cloud of the tweets which will give us the keywords most used.

wordcloud(corp_clean,min.freq=50,random.order = F,max.words = 500,scale = c(6,0.5),colors = rainbow(50))






neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")
source('sentiment.R')
analysis=score.sentiment(tweetsText,pos,neg)

table(analysis$score)

