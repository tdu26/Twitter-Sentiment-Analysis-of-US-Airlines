setwd("D:/Stevens/summer 2016/BIA 658/Final/airline-twitter-sentiment")

# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)

mydata <- read.csv("tweets.csv")

mydataCorpus <- Corpus(VectorSource(mydata$text))
mydataCorpus <- tm_map(mydataCorpus, PlainTextDocument)

mydataCorpus <- tm_map(mydataCorpus, removePunctuation)
mydataCorpus <- tm_map(mydataCorpus, removeWords, c('flight', stopwords('english')))

mydataCorpus <- tm_map(mydataCorpus, stemDocument)

wordcloud(mydataCorpus, max.words = 150, random.order = TRUE, 
          colors = brewer.pal(8,"Paired"))

# mydataCorpus <- tm_map(mydataCorpus, removeWords, c('the', 'this', stopwords('english')))

# positive word cloud
positive <- mydata[which(mydata$airline_sentiment == "positive"),]
positive <- Corpus(VectorSource(positive$text))
positive <- tm_map(positive, PlainTextDocument)

positive <- tm_map(positive, removePunctuation)
positive <- tm_map(positive, removeWords, stopwords('english'))

positive <- tm_map(positive, stemDocument)

wordcloud(positive, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(8,"Set2"))

# Negative word could
negative <- mydata[which(mydata$airline_sentiment == "negative"),]
negative <- Corpus(VectorSource(negative$text))
negative <- tm_map(negative, PlainTextDocument)

negative <- tm_map(negative, removePunctuation)
negative <- tm_map(negative, removeWords, c('flight', stopwords('english')))

negative <- tm_map(negative, stemDocument)

wordcloud(negative, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(8,"Set2"))