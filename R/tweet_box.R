
#' @title Twitter Data Box Plot
#'
#' @description Displays the distribution scores of either hashtag or topic 
#'  Twitter data.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd 
#'  and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic 
#'   data select:  "topic".
#' 
#' @importFrom dplyr quo mutate
#' @import ggplot2
#' @importFrom tidyr unnest
#' 
#' @return A ggplot box plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_box <- tweet_box(DataFrameTidyScores = score_data,
#'                     HT_Topic = "hashtag")
#' ht_box
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_box <- tweet_box(DataFrameTidyScores = score_data,
#'                        HT_Topic = "topic") 
#' topic_box                    
#' }
#' @export

tweet_box <- function(DataFrameTidyScores, 
                      HT_Topic) {
  
  # input checks
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using 
         hashtags, or topic for analysis looking at topics.')
  }
  
  # configure defusing operators for packages checking
  hashtags <- dplyr::quo(hashtags)
  Topic <- dplyr::quo(Topic)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)

  # function main body
  if(HT_Topic == "hashtag") {
    
    TD_HT_BoxPlot <- DataFrameTidyScores %>% 
      tidyr::unnest(
        cols = hashtags, 
        keep_empty = FALSE) %>% 
      dplyr::mutate(
        hashtags = tolower(hashtags)) %>% 
      ggplot2::ggplot(ggplot2::aes(hashtags, TweetSentimentScore)) +
      ggplot2::geom_boxplot() +
      theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across each #Hashtag") +
      ggplot2::xlab('#Hashtag') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    
    return(TD_HT_BoxPlot)
    
  } else {
    
    TD_Topic_BoxPlot <- DataFrameTidyScores %>% 
      dplyr::mutate(
        Topic = as.character(Topic)) %>% 
      ggplot2::ggplot(ggplot2::aes(Topic, TweetSentimentScore)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across each Topic") +
      ggplot2::xlab('Topic') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    
    return(TD_Topic_BoxPlot)
    
  }
}
