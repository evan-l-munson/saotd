
#' @title Twitter Hashtag or Topic Distribution
#'
#' @description Determines the scores distribution by hashtag or topic for 
#'   Twitter data.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd 
#'   and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic 
#'   data select:  "topic".
#' @param bin_width The width of the bins.  Default is 1.
#' @param color The user selected color to highlight the bins.
#' @param fill The interior color of the bins.
#' 
#' @importFrom ggplot2 ggplot geom_histogram facet_wrap theme ggtitle xlab ylab theme_bw
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
#'                      bin_width = 1,
#'                      color = "black", 
#'                      fill = "white")
#' Dist
#' }
#' @export

tweet_distribution <- function(DataFrameTidyScores, 
                               HT_Topic, 
                               bin_width = 1, 
                               color = "black", 
                               fill = "black") {
  # input checks
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using 
         hashtags, or topic for analysis looking at topics.')
  }
  
  # configure defusing operators for packages checking
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  bin_width <- dplyr::quo(bin_width)
  
  # function main body
  if(HT_Topic == "hashtag") {
    
    TD_HT_Distribution <- DataFrameTidyScores %>%
      tidyr::unnest(
        cols = hashtags, 
        keep_empty = FALSE) %>% 
      ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
      ggplot2::geom_histogram(
        stat = "count", 
        binwidth = bin_width, 
        colour = color, 
        fill = fill) +
      ggplot2::facet_wrap(~hashtags, ncol = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Score Distribution Across all #Hashtags") +
      ggplot2::xlab('Sentiment') +
      ggplot2::ylab('Count') +
      ggplot2::theme_bw()
    
    return(TD_HT_Distribution)
    
  } else {
    
    TD_Topic_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
      ggplot2::geom_histogram(
        stat = "count",
        binwidth = bin_width,
        colour = color,
        fill = fill) +
      ggplot2::facet_wrap(~Topic, ncol = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Score Distribution Across all Topics") +
      ggplot2::xlab('Sentiment') +
      ggplot2::ylab('Count') +
      ggplot2::theme_bw()
    
    return(TD_Topic_Distribution)
    
  }
}