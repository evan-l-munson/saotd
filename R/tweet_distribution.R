
#' @title Twitter Hashtag or Topic Distribution
#'
#' @description Determines the scores distribution by hashtag or topic for Twitter data.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' @param binwidth The width of the bins.  Default is 1.
#' @param color The user selected color to highlight the bins.
#' @param fill The interior color of the bins.
#' 
#' @import ggplot2
#' 
#' @return A facet wrap ggplot.
#' 
#' @examples
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' Dist <- tweet_distribution(DataFrameTidyScores = score_data,
#'                      HT_Topic = "hashtag",
#'                      binwidth = 1,
#'                      color = "black", 
#'                      fill = "white")
#' Dist
#' }
#' @export

tweet_distribution <- function(DataFrameTidyScores, HT_Topic, binwidth = 1, color = "black", fill = "white") {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  binwidth <- dplyr::quo(binwidth)
  
  if(HT_Topic == "hashtag") {
    TD_HT_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
      ggplot2::geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
      ggplot2::facet_wrap(~hashtag, ncol = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Score Distribution Across all #Hashtags") +
      ggplot2:: xlab('Sentiment') +
      ggplot2::ylab('Count') +
      ggplot2::theme_bw()
    return(TD_HT_Distribution)
  } else {
    TD_Topic_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
      ggplot2::geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
      ggplot2::facet_wrap(~Topic, ncol = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Score Distribution Across all Topics") +
      ggplot2::xlab('Sentiment') +
      ggplot2:: ylab('Count') +
      ggplot2::theme_bw()
    return(TD_Topic_Distribution)
  }
}