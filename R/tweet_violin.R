
#' @title Twitter Data Violin Plot
#'
#' @description Displays the distribution scores of either hashtag or topic Twitter data.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @import ggplot2
#' @importFrom dplyr quo
#' @importFrom stats median
#' 
#' @return A ggplot violin plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_violin <- tweet_violin(DataFrameTidyScores = score_data,
#'                           HT_Topic = "hashtag")
#' ht_violin
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_violin <- tweet_violin(DataFrameTidyScores = score_data,
#'                              HT_Topic = "topic") 
#' topic_violin                    
#' }
#' @export

tweet_violin <- function(DataFrameTidyScores, HT_Topic) {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  hashtag <- dplyr::quo(hashtag)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  Topic <- dplyr::quo(Topic)
  
  if(HT_Topic == "hashtag") {
    TD_HT_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(ggplot2::aes(hashtag, TweetSentimentScore)) +
      ggplot2::geom_violin(scale = "area") +
      ggplot2::stat_summary(fun.y = stats::median, geom = "point", shape = 23, size = 2) +
      ggplot2::ggtitle("Sentiment Scores Across each #Hashtag") +
      ggplot2::xlab('#Hashtag') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    return(TD_HT_ViolinPlot)
  } else{
    TD_Topic_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(ggplot2::aes(Topic, TweetSentimentScore)) +
      ggplot2::geom_violin(scale = "area") +
      ggplot2::stat_summary(fun.y = stats::median, geom = "point", shape = 23, size = 2) +
      ggplot2::ggtitle("Sentiment Scores Across each Topic") +
      ggplot2::xlab('Topic') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    return(TD_Topic_ViolinPlot)
  }
}
