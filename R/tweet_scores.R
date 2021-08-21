
#' @title Score Tidy Twitter Data
#'
#' @description Function to Calculate Sentiment Scores that will account for 
#'   sentiment by hashtag or topic.
#'
#' @param DataFrameTidy Data Frame of Twitter Data that has been tidy'd.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic 
#'   data select:  "topic"
#' 
#' @importFrom dplyr mutate inner_join group_by count quo
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

tweet_scores <- function(DataFrameTidy, 
                         HT_Topic) {
  
  # input checks
  if(!is.data.frame(DataFrameTidy)) {
    stop('The input for this function is a data frame.')
  }
  
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either "hashtag" for analysis 
         using hashtags, or "topic" for analysis looking at topics.')
  }
  
  # configure defusing operators for packages checking
  text <- dplyr::quo(text)
  method <- dplyr::quo(method)
  hashtags <- dplyr::quo(hashtags)
  created_at <- dplyr::quo(created_at)
  key <- dplyr::quo(key)
  sentiment <- dplyr::quo(sentiment)
  n <- dplyr::quo(n)
  positive <- dplyr::quo(positive)
  negative <- dplyr::quo(negative)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  Topic <- dplyr::quo(Topic)
  
  # function main body
  if(HT_Topic == "hashtag") {
    
    TD_Hashtag_Scores <- DataFrameTidy %>% 
      dplyr::inner_join(
        y = tidytext::get_sentiments(lexicon = "bing"), 
        by = c("Token" = "word")) %>% 
      dplyr::mutate(method = "Bing") %>% 
      dplyr::group_by(text,
                      method,
                      hashtags,
                      created_at,
                      key,
                      sentiment) %>% 
      dplyr::count(method,
                   hashtags,
                   created_at,
                   key,
                   sentiment) %>%  
      tidyr::spread(key = sentiment, 
                    value = n, 
                    fill = 0) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        TweetSentimentScore = positive - negative,
        TweetSentiment = dplyr::if_else(
          TweetSentimentScore == 0, "neutral",
          dplyr::if_else(
            TweetSentimentScore > 0, "positive", "negative")),
        date = lubridate::as_date(created_at))
    
    return(TD_Hashtag_Scores)
    
  } else if (HT_Topic == "topic") {
    
    TD_Topic_Scores <- DataFrameTidy %>% 
      dplyr::inner_join(
        y = tidytext::get_sentiments(lexicon = "bing"), 
        by = c("Token" = "word")) %>% 
      dplyr::mutate(method = "Bing") %>% 
      dplyr::group_by(text,
                      method,
                      Topic,
                      created_at,
                      key,
                      sentiment) %>% 
      dplyr::count(method,
                   Topic,
                   created_at,
                   key,
                   sentiment) %>%  
      tidyr::spread(key = sentiment, 
                    value = n, 
                    fill = 0) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        TweetSentimentScore = positive - negative,
        TweetSentiment = dplyr::if_else(
          TweetSentimentScore == 0, "neutral",
          dplyr::if_else(
            TweetSentimentScore > 0, "positive", "negative")),
        date = lubridate::as_date(created_at))

    return(TD_Topic_Scores)
    
  } else {
    
    message("Input \"hashtag\" or \"topic\" in the HT_Topic varriable")
    
  }
  
}
