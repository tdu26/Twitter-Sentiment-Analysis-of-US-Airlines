# US-Airline-Twitter-Sentiment-Analysis

The R scripts in this repository analyze how travelers in February 2015 expressed their feelings towards US major airlines. The snetiment reviews are from Twitter.com.
The data come from https://www.kaggle.com/crowdflower/twitter-airline-sentiment.

network.R:
Builds the social network clusters under different filtrations.

WordCloud.R:
Plots wordclouds based on the actual tweets to see what the “buzzwords” are among the tweetosphere.

Data Visualization (derived from Tableau Publice software):
 - sn70.png/sn80.png/sn90.png:
   social network cluster analysis on the major users to the Twitter network. Visualize the network under different filtrations by weighted degree. (70%, 80% and 90%)
 - postive.png/negative.png:
   plots the wordclouds that contain positive and negative comments. 
 - tweets.png:
   plots the wordcould that contains the most frequent words among all tweets.
 - by airline.png:
   visualizes the positive, negative and neutral comments among 6 major airline companies. The color and the size of the blocks show the sentiments by airline. 
 - general sentiment.png:
   gives a summary of how many negative, positive and neutral comments there are toward all airlines.

