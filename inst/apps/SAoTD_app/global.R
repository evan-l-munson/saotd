# Packages

library(shiny)
library(shinythemes)
library(SAoTD)
library(dplyr)

# Load the dataset from the choroplethrMaps package

data("raw_tweets")
TD <- raw_tweets

# Work Common to both hashtag and topic analysis
TD_Tidy <- SAoTD::Tidy(DataFrame = TD)

TD_Scores <- SAoTD::Scores(DataFrameTidy = TD_Tidy, 
                           HT_Topic = "hashtag")

TD_Corr <- SAoTD::Word.Corr(DataFrameTidy = TD_Tidy, 
                            number = 100, 
                            sort = TRUE)

TD_Bigram <- SAoTD::Bigram(DataFrame = TD)

# TD_Violin <- SAoTD::ViolinPlot(DataFrameTidyScores = TD_Scores, 
#                                HT_Topic = "hashtag")
# 
# TD_Time <- SAoTD::TimeScale(DataFrameTidyScores = TD_Scores, 
#                             HT_Topic = "hashtag")

