
# tweet_acquire -----------------------------------------------------------------

#' @title Acquire Twitter Tweets   
#'
#' @description Function will enable a user to access the Twitter API throught the 
#' [Twitter Developers Account](https://dev.twitter.com/) site.
#' Once a user has a Twitter developers account and has recieved their individual consumer key, 
#' consumer secret key, access token, and access secret they can 
#' acquire Tweets based on a list of hashtags and a requested number of entires per hashtag.
#' 
#' @param consumer_key Twitter Application management consumer key.
#' @param consumer_secret Twitter Application management consumer secret key.
#' @param access_token Twitter Application management access token.
#' @param access_secret Twitter Application management access secret key.
#' @param HT A single hashtag or a list of hashtags the user has specified.
#' @param num_tweets Number of Tweets to be acquired per each hashtag.
#' @param file_name User desired output .RData file name.
#' @param distinct Logical.  If distinct = TRUE, the function removes multiple Tweets that originate from the same Twitter id at the exact same time.
#' @importFrom twitteR setup_twitter_oauth twListToDF searchTwitter
#' @importFrom dplyr mutate distinct quo
#' @importFrom purrr map_df
#' 
#' @return A DataFrame.
#' 
#' @examples 
#' \donttest{
#' consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
#' consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_token <- "XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' 
#' test_tweets <- file.path(tempdir(), "test_tweets.RData")
#' 
#' hashtags <- c("#job", "#Friday", "#fail", "#icecream", "#random", "#kitten", "#airline")
#' 
#' tweet_acquire(consumer_key = consumer_key, 
#'               consumer_secret = consumer_secret, 
#'               access_token = access_token, 
#'               access_secret = access_secret, 
#'               HT = hashtags, 
#'               num_tweets = 10, 
#'               file_name = test_tweets,
#'               distinct = TRUE)
#'         
#' load("test_tweets.RData")
#' }
#' @export 

tweet_acquire <- function(consumer_key, 
                          consumer_secret, 
                          access_token, 
                          access_secret, 
                          HT, 
                          num_tweets, 
                          file_name, 
                          distinct = TRUE) {
  
  options(httr_oauth_cache = TRUE)
  
  screenName <- dplyr::quo(screenName)
  created <- dplyr::quo(created)
  key <- dplyr::quo(key)
  
  twitteR::setup_twitter_oauth(consumer_key,
                               consumer_secret,
                               access_token,
                               access_secret)
  
  twitter_data <- list()
  for (i in HT) {
    twitter_data[[i]] <- twitteR::twListToDF(twitteR::searchTwitter(i, 
                                                                    n = num_tweets, 
                                                                    lang = "en")) %>% 
      dplyr::mutate(hashtag = substr(i, 2, nchar(i)))
  }
  
  raw_tweets <- purrr::map_df(twitter_data, rbind) %>% 
    dplyr::mutate(key = paste(screenName, created)) %>% 
    dplyr::distinct(key, .keep_all = distinct)
  
  save(raw_tweets, file = file_name)
  
}
