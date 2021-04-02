
# tweet_acquire -----------------------------------------------------------------

#' @title Acquire Twitter Tweets
#'
#' @description Function will enable a user to access the Twitter API through 
#'   the [Twitter Developers Account](https://dev.twitter.com/) site.  Once a 
#'   user has a Twitter developers account and has received their individual 
#'   consumer key, consumer secret key, access token, and access secret they can 
#'   acquire Tweets based on a list of hashtags and a requested number of 
#'   entries per query.
#' 
#' @param twitter_app The name of user created Twitter Application.
#' @param consumer_api_key Twitter Application management consumer API key.
#' @param consumer_api_secret_key Twitter Application management consumer API 
#'   secret key.  Application must have \code{Read and write} access level and
#'   \code{Callback URL} of \code{http://127.0.0.1:1410}.
#' @param access_token Twitter Application management access token 
#'   (apps.twitter.com).
#' @param access_secret Twitter Application management access secret token 
#'   (apps.twitter.com).
#' @param query A single query or a list of queries the user has specified.  
#'   Character string, not to exceed 500 characters.  To search for tweets 
#'   containing at least one of multiple possible terms, separate each search 
#'   term with spaces and "OR" (in caps).  For example, the search \code{q =
#'   "data science"} looks for tweets containing both "data" and "science" 
#'   located anywhere in the tweets and in any order.  When "OR" is entered 
#'   between search terms, \code{query = "data OR science"}, Twitter's REST API 
#'   should return any tweet that contains either "data" or "science."
#' @param num_tweets Number of Tweets to be acquired per each hashtag.
#' @param distinct Logical.  If distinct = TRUE, the function removes multiple 
#'   Tweets that originate from the same Twitter id at the exact same time.
#' @importFrom rtweet create_token search_tweets
#' @importFrom dplyr mutate distinct quo
#' 
#' @return A DataFrame with tweets and meta data.
#' 
#' @examples 
#' \donttest{
#' twitter_app <- "super_app"
#' consumer_api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
#' consumer_api_secret_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_token <- "XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_token_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' 
#' tweets <- tweet_acquire(witter_app = twitter_app,
#'                         consumer_api_key = consumer_api_key,
#'                         consumer_api_secret_key = consumer_api_secret_key,
#'                         access_token = access_token,
#'                         access_token_secret = access_token_secret,
#'                         query = "#icecream",
#'                         num_tweets = 1000,
#'                         distinct = TRUE)
#' }
#' @export 

tweet_acquire <- function(twitter_app,
                          consumer_api_key,
                          consumer_api_secret_key,
                          access_token,
                          access_token_secret,
                          query,
                          num_tweets,
                          distinct = TRUE) {
  
  
  
  
  screenName <- dplyr::quo(screen_name)   
  created <- dplyr::quo(created_at)
  key <- dplyr::quo(key)
  
  ## authenticate via web browser
  user_token <- rtweet::create_token(
    app = twitter_app,
    consumer_key = consumer_api_key,
    consumer_secret = consumer_api_secret_key,
    access_token = access_token,
    access_secret = access_token_secret
  )
  
  raw_tweets <- rtweet::search_tweets(token = user_token,
                                      q = query,
                                      n = num_tweets) %>%
    dplyr::mutate(key = paste(screen_name, created_at)) %>%
    dplyr::distinct(key, .keep_all = distinct) %>% 
    dplyr::mutate(query = query)
  
}