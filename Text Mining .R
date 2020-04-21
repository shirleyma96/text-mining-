###Obtaining and Analyzing coronavirus Twitter Data using R###

#1. Registering APRI using Twitter account
#https://apps.twitter.com

#2. Insert Values
api_key <- 'lh8bfUemQIITX20lqOsnojYTW'
api_secret <- 'CZuHzABkl3yeZ9zNb002FcYq9AhlOpKIwRZsjsNCzRHnRquMdI'
access_token <-'1000022532-3CImlmH3DBVCos1EUJYVDGmdS3p6dGVYWl2ckB2'
access_token_secret <-'JeeuxteF42HSxmOjRZMK0ZMkK8sJXa5Sj6GCHAY8tUsIy'

install.packages('twitteR')
library(twitteR)
setup_twitter_oauth(api_key, 
                    api_secret,
                    access_token, 
                    access_token_secret)

#Extracting tweets 
tweets <-searchTwitter("#coronavirus", n=1000, lang='en')
#Convert to data frame
fashion<-twListToDF(tweets)


write.csv(fashion, file = 'coronavirus.csv', row.names = F)

### DATA CLEANING AND PREPARATION
#1. Reading Data File
covid<-read.csv(file.choose('coronavirus.csv'), header=T)
str(covid)

#clean the text of special characters such as symbols and emoticons
covid$text <- sapply(covid$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#2. Building Corpus
install.packages('tm')
install.packages('NLP')
library(tm)
library(NLP)
corpus <-iconv(covid$text, to='utf-8-mac') #need only the first col text from file
corpus <- Corpus(VectorSource(corpus)) #corpus is a collection of texts
inspect(corpus[1:5]) #inspect the first five tweets

#3. Cleaning Data
#convert data to lower case for analysis
corpus <-tm_map(corpus, tolower) #convert all alphabet to lower case
inspect(corpus[1:5]) #inspect the first five tweets

#remove punctuations
corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:5]) #inspect the first five tweets

#remove numbers
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:5]) #inspect the first five tweets

#remove common words-they dont add any informational value
#use the stopwords function in english
#select stopwords(english) to see what words are removed
cleanset <-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#remove URLs (https://etc.)
#make use of function http
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
cleanset <-tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:100])

#tweets were pulled using ios so we can clean it and other related
#products from the text that aren't meaningful
cleanset <-tm_map(cleanset, removeWords, c('covid19','now','today','corona','coronavirus','covid',"amp",'will','like','know'))
inspect(cleanset[1:5])

#remove white spaces
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#lets now provide some structure to tweets by creating a matrix of rows/coloums
#this is called term document matrix (tdm)
#Create term document matrix

tdm <- TermDocumentMatrix(cleanset)
tdm 

matrix <- as.matrix(tdm) 
ws <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(ws),freq=ws)


#<<TermDocumentMatrix (terms: 2182, documents: 1000)>>
#  Non-/sparse entries: 10465/2171535
#Sparsity           : 100%
#Maximal term length: 24
#Weighting          : term frequency (tf)

tdm <- as.matrix(tdm)
tdm[1:40, 1:20]

###VISUALIZE TEXT DATA
w <- rowSums(tdm)
w <- subset(w, w>=30)
barplot(w, las = 2, col=rainbow(40), main = "Frequency of Words in coronavirus Tweets")

# Word Cloud
install.packages('wordcloud')
install.packages('RColorBrewer')
library(wordcloud)
library(RColorBrewer)
w <- sort(rowSums(tdm), decreasing=TRUE) #sort words in decreasing order
set.seed(9999)
wordcloud2(data=df, size=0.7, color='random-dark',shape = 'circle')

wordcloud(words = names(w), 
          freq=w, max.words = 300, 
          random.order =FALSE)  #words are specified in names in w dataframe, frequency is stored in w, random order=false

#specifying options in word cloud
#Specify that max words be no more than say, 200
#Freq for terms to be included in wordcloud (say they have to appear 5 times to be included)
#color words, specify scale (bigger words max size =3, smaller =0.2)
#rotate some words (rotation percentage = 30%)
par(mar = rep(0, 4))

wordcloud(words = names(w), 
          freq=w, 
          random.order =FALSE,
          max.words = 80, 
          min.freq = 10,
          colors = brewer.pal(7, 'Dark2'), 
          scale = c(2, 0.2), 
          rot.per = .3)  



#SENTIMENT ANALYSIS USING R
#load packages
install.packages('syuzhet')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('scales')
install.packages('dplyr')
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

#Reading Files
tweets <- iconv(covid$text, to="utf-8-mac")

#obtain sentiment scores for each 1000 tweets
s <-get_nrc_sentiment(tweets)
head(s) 

tail(s)
tweets[648]


#plot sentiment scores
par(mar = c(5.5, 4.1, 4.1, 2.1))
barplot(
  sort(colSums(s)), 
  las = 2,
  ylab = 'Total Count', 
  main ='Sentiment Scores for coronavirus Tweets')

