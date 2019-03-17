
#' @title Twitter Data Timeseries Plot.
#'
#' @description Displays the Twitter data sentiment scores through time.  The sentiment scores by hashtag or topic are summed per day and plotted to show the change in sentiment through time.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @importFrom dplyr summarize group_by quo
#' @import ggplot2
#' 
#' @return A ggplot plot.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_time <- tweet_time(DataFrameTidyScores = score_data,
#'                       HT_Topic = "hashtag")
#' ht_time
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_time <- tweet_time(DataFrameTidyScores = score_data,
#'                          HT_Topic = "topic") 
#' topic_time                    
#' }
#' @export

tweet_time <- function(DataFrameTidyScores, HT_Topic) {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  hashtag <- dplyr::quo(hashtag)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  DayScore <- dplyr::quo(DayScore)
  Topic <- dplyr::quo(Topic)
  
  if(HT_Topic == "hashtag") {
    TD_HT_TimeScale <- DataFrameTidyScores %>% 
      dplyr::group_by(hashtag, date) %>% 
      dplyr::summarise(DayScore = sum(TweetSentimentScore)) %>% 
      ggplot2::ggplot(ggplot2::aes(x = factor(date), y = DayScore, colour = hashtag)) + 
      ggplot2::geom_point() +
      ggplot2::geom_path(ggplot2::aes(group=1)) +
      ggplot2::geom_hline(yintercept = 0, color = "black") +
      ggplot2::facet_wrap(~hashtag, ncol = 2, scales = "free_y") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across all #Hashtags") +
      ggplot2::xlab('Day') +
      ggplot2::ylab('Daily Sentiment Score') +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(TD_HT_TimeScale)
  } else {
    TD_Topic_TimeScale <- DataFrameTidyScores %>% 
      dplyr::group_by(Topic, date) %>% 
      dplyr::summarise(DayScore = sum(TweetSentimentScore)) %>% 
      ggplot2::ggplot(ggplot2::aes(x = factor(date), y = DayScore, colour = Topic)) + 
      ggplot2::geom_point() +
      ggplot2::geom_path(ggplot2::aes(group=1)) +
      ggplot2::geom_hline(yintercept = 0, color = "black") +
      ggplot2::facet_wrap(~Topic, ncol = 2, scales = "free_y") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across all Topics") +
      ggplot2::xlab('Day') +
      ggplot2::ylab('Daily Sentiment Score') +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(TD_Topic_TimeScale)
  }
}
