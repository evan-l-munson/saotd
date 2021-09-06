#' @title Twitter Data Set
#' 
#' @description 
#' Dataset from a [Twitter US Airline Sentiment](https://www.kaggle.com/crowdflower/twitter-airline-sentiment) Kaggle competition, from December 2017.  The dataset contains 14,487 tweets from 6 different hashtags (2,604 x #American, 2,220 x #Delta, 2,420 x #Southwest, 3,822 x #United, 2,913 x #US Airways, 504 x #Virgin America). 
#' 
#' @usage data(raw_tweets)
#' 
#' @format A \code{tribble} with 14,483 rows and 6 variables
#' 
#' \describe{
#'  \item{id}{ID of this status.}
#'  \item{hashtags}{Hashtag that the individual tweet was acquired from.}
#'  \item{screenName}{Screen name of the user who posted this status.}
#'  \item{text}{The text of the status.}
#'  \item{created_at}{When this status was created.}
#'  \item{key}{Unique key based on the tweets originators user id and the created date time group.}
#'  }
#'
"raw_tweets"