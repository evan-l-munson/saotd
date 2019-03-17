
#' @title Score Tidy Twitter Data
#'
#' @description Function to Calculate Sentiment Scores that will account for sentiment by hashtag or topic.
#'
#' @param DataFrameTidy DataFrame of Twitter Data that has been tidy'd.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic"
#' 
#' @importFrom dplyr mutate inner_join group_by count quo
#' @importFrom plyr rename
#' @importFrom tidyr spread
#' @importFrom lubridate as_date
#' @importFrom tidytext get_sentiments
#' 
#' @return A Scored DataFrame.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag")
#' score_data
#' }
#' @export

tweet_scores <- function(DataFrameTidy, HT_Topic) {
  
  if(!is.data.frame(DataFrameTidy)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  text <- dplyr::quo(text)
  method <- dplyr::quo(method)
  hashtag <- dplyr::quo(hashtag)
  created <- dplyr::quo(created)
  key <- dplyr::quo(key)
  Sentiment <- dplyr::quo(Sentiment)
  n <- dplyr::quo(n)
  positive <- dplyr::quo(positive)
  negative <- dplyr::quo(negative)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  Topic <- dplyr::quo(Topic)
  
  
  Bing <- tidytext::get_sentiments(lexicon = "bing") %>% 
    plyr::rename(c("word" = "Token", "sentiment" = "Sentiment"))
  
  if(HT_Topic == "hashtag") {
    TD_Hashtag_Scores <- DataFrameTidy %>% 
      dplyr::inner_join(Bing, by = "Token") %>% 
      dplyr::mutate(method = "Bing") %>% 
      dplyr::group_by(text, method, hashtag, created, key, Sentiment) %>% 
      dplyr::count(method, hashtag, created, key, Sentiment) %>%  
      tidyr::spread(Sentiment, n, fill = 0) %>% 
      dplyr::mutate(TweetSentimentScore = positive - negative) %>% 
      dplyr::mutate(TweetSentiment = ifelse(TweetSentimentScore == 0, "neutral",
                                            ifelse(TweetSentimentScore > 0, "positive", "negative"))) %>% 
      dplyr::mutate(date = lubridate::as_date(created))
    return(TD_Hashtag_Scores)
  } else {
    TD_Topic_Scores <- DataFrameTidy %>% 
      dplyr::inner_join(Bing, by = "Token") %>% 
      dplyr::mutate(method = "Bing") %>% 
      dplyr::group_by(text, method, Topic, created, key, Sentiment) %>% 
      dplyr::count(method, Topic, created, key, Sentiment) %>%  
      tidyr::spread(Sentiment, n, fill = 0) %>% 
      dplyr::mutate(TweetSentimentScore = positive - negative) %>% 
      dplyr::mutate(TweetSentiment = ifelse(TweetSentimentScore == 0, "neutral",
                                            ifelse(TweetSentimentScore > 0, "positive", "negative"))) %>% 
      dplyr::mutate(date = lubridate::as_date(created))
    return(TD_Topic_Scores)
  }
}
