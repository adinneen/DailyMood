
#  Install Requried Packages
installed.packages("SnowballC") #Textmining
installed.packages("tm") #Textanalysis
installed.packages("rtweet") #extracting twitter data
installed.packages("syuzhet") #sentiment analysis
library(ggplot2)
library(lubridate)
 
# Load Requried Packages
library("SnowballC")
library("tm")
library("rtweet")
library("syuzhet")
library("stringr")
library("dplyr")
devtools::install_github("mkearney/rtweet")

options(repr.matrix.max.rows=600, repr.matrix.max.cols=200)

# Authonitical keys
#consumer_key <- 'XXX'
#consumer_secret <- 'XXX'
#access_token <- 'XXX'
#access_secret <- 'XXXX'
#setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Date YYYY-MM-DD | Lat : 35.99990 Long : -78.90669 )
# tweets <- searchTwitter("", n=30000, since = "2018-07-21", until = "2018-07-22", geocode='35.99990,-78.90669,20mi',  retryOnRateLimit=120)

# n.tweet <- length(tweets)
# n.tweet

#Making Tweets into a dataframe
#tweets.df <- twListToDF(tweets) 

#write.csv(tweets.df, file="NCtweets22july18.csv")
tweets.df <- read.csv("NCtweets22july18.csv")
nrow(tweets.df)

#head(tweets.df, 40)

#Cleaning up the tweets themselves to do sentiment analysis
tweets.df2 = gsub("&amp", "", tweets.df$text)
tweets.df2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df2)
tweets.df2 = gsub("@\\w+", "", tweets.df2)
tweets.df2 = gsub("[[:punct:]]", "", tweets.df2)
tweets.df2 = gsub("[[:digit:]]", "", tweets.df2)
tweets.df2 = gsub("http\\w+", "", tweets.df2)
tweets.df2 = gsub("[ \t]{2,}", "", tweets.df2)
tweets.df2 = gsub("^\\s+|\\s+$", "", tweets.df2) 

head(tweets.df2)

#Applying sentiment analysis library
word.df <- as.vector(tweets.df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df2, emotion.df) 
head(emotion.df2)

# Creating an overall positivity score
sent.value <- get_sentiment(word.df)
# Most positive tweet
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

# Most negative tweet
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 
most.positive

# Creating set of positive tweets
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

# Creating set of negative tweets
negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

# Creating set of neutral tweets
neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)

# Alternate way to classify as Positive, Negative or Neutral tweets
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)

# Creating Sentiment Dataset
category_senti2 <- cbind(emotion.df2, category_senti, sent.value)

# Adding back in other potentially useful variables
df <- cbind(tweets.df, category_senti2)

df1 <- df  %>% select('favorited','favoriteCount','created','isRetweet','retweetCount', 'retweeted', 'tweets.df2':'sent.value')

head(df1)

# Adding Neutral
df1$neutral <- ifelse(df1$category_senti=="Neutral", 1, 0)

# Fixing Dates | as.POSIXct() - %Y-%m-%d %H:%M:%S
class(df1$created)
df1$date <- as.POSIXct(df1$created)

summary(df1)

CPR <- df1  %>% group_by(time=floor_date(date, "15 minutes"))  %>% summarise(critical_positivity_ratio = sum(positive)/sum(negative))

head(CPR)

ggplot(data=CPR, aes(x=time, y=critical_positivity_ratio)) + geom_line() + geom_line(y=3, col="red")

#emotions <- df1  %>% group_by(time=floor_date(date, "15 minutes"))  %>% summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust), fear = sum(fear), joy = sum(joy), sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust))
moods <- df1  %>% group_by(time=floor_date(date, "15 minutes"))  %>% summarise(positive = sum(positive), negative = sum(negative), neutral = sum(neutral))


head(moods)

#mood <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
ggplot(data=moods, aes(x=time, y=neutral)) + geom_line(aes(col="neutral")) + geom_line(aes(x=time, y=positive, col="positive")) + 
geom_line(aes(x=time, y=negative, col="negative")) + 
scale_y_continuous("# of tweets") +
scale_fill_manual("Sentiment of Tweet")

