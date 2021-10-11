
# tweet_acquire ----------------------------------------------------------------

#' @title Acquire Twitter Tweets
#'
#' @description Function will enable a user to access the Twitter API through
#'   the [Twitter Developers Account](https://dev.twitter.com/) site.  Once a
#'   user has a Twitter developers account and has received their individual
#'   consumer key, consumer secret key, access token, and access secret they
#'   can acquire Tweets based on a list of hashtags and a requested number of
#'   entries per query.
#'
#' @param twitter_app The name of user created Twitter Application.
#' @param consumer_api_key Twitter Application management consumer API key.
#' @param consumer_api_secret_key Twitter Application management consumer API
#'   secret key.  Application must have \code{Read and write} access level and
#'   \code{Callback URL} of \code{http://127.0.0.1:1410}.
#' @param access_token Twitter Application management access token 
#'   (apps.twitter.com).
#' @param access_token_secret Twitter Application management access secret 
#'   token (apps.twitter.com).
#' @param query A single query or a list of queries the user has specified.  
#'   Character string, not to exceed 500 characters.  To search for tweets 
#'   containing at least one of multiple possible terms, separate each search 
#'   term with spaces and "OR" (in caps).  For example, the search \code{q =
#'   "data science"} looks for tweets containing both "data" and "science" 
#'   located anywhere in the tweets and in any order.  When "OR" is entered 
#'   between search terms, \code{query = "data OR science"}, Twitter's REST API 
#'   should return any tweet that contains either "data" or "science."
#' @param num_tweets Number of Tweets to be acquired per each hashtag.
#' @param reduced_tweets Logical.  If reduced_tweets = TRUE, the data frame 
#'   returned to the user will be significantly reduced specifically for use in 
#'   the `saotd` package.  If reduced_tweets = FALSE, the full results from the 
#'   Twitter API will be returned.
#' @param distinct Logical.  If distinct = TRUE, the function removes multiple 
#'   Tweets that originate from the same Twitter id at the exact same time.
#'
#' @importFrom rtweet create_token search_tweets
#' @importFrom dplyr mutate distinct
#'
#' @return A Data Frame with tweets and meta data.
#'
#' @examples 
#' \dontrun{
#' twitter_app <- "super_app"
#' consumer_api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
#' consumer_api_secret_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_token <- "XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_token_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#'
#' tweets <- tweet_acquire(
#'   twitter_app = "twitter_app",
#'   consumer_api_key = consumer_api_key,
#'   consumer_api_secret_key = consumer_api_secret_key,
#'   access_token = access_token,
#'   access_token_secret = access_token_secret,
#'   query = "#icecream",
#'   num_tweets = 100,
#'   distinct = TRUE)
#'
#' Or the Twitter API keys and tokens can be saved as an .Renviron file in the 
#' working directory.  If using a `.Renviron` file, the data should be saved 
#' like the below example:
#'
#' consumer_api_key=XXXXXXXXXXXXXXXXXXXXXXXXX
#' consumer_api_secret_key=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' access_token=XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' access_token_secret=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#'
#' The `tweet_acquire` function would access the keys and tokens using the 
#' `Sys.getenv()` function and would appear like the below example:
#'
#' tweets <- tweet_acquire(
#'   twitter_app = "twitter_app",
#'   consumer_api_key = Sys.getenv('consumer_api_key'),
#'   consumer_api_secret_key = Sys.getenv('consumer_api_secret_key'),
#'   access_token = Sys.getenv('access_token'),
#'   access_token_secret = Sys.getenv('access_token_secret'),
#'   query = "#icecream",
#'   num_tweets = 100,
#'   distinct = TRUE)
#' 
#' }
#' @export 

tweet_acquire <- function(twitter_app,
                          consumer_api_key,
                          consumer_api_secret_key,
                          access_token,
                          access_token_secret,
                          query,
                          num_tweets,
                          reduced_tweets = TRUE,
                          distinct = TRUE) {
  
  # configure defusing operators for packages checking
  screen_name <- dplyr::quo(screen_name)
  created_at <- dplyr::quo(created_at)
  key <- dplyr::quo(key)
  user_id <- dplyr::quo(user_id)
  status_id <- dplyr::quo(status_id)
  text <- dplyr::quo(text)
  hashtags <- dplyr::quo(hashtags)
  location <- dplyr::quo(location)

  ## authenticate via web browser
  user_token <- rtweet::create_token(
    app = twitter_app,
    consumer_key = consumer_api_key,
    consumer_secret = consumer_api_secret_key,
    access_token = access_token,
    access_secret = access_token_secret)

  # pull and format tweets
  raw_tweets <-
    rtweet::search_tweets(
      token = user_token,
      q = query,
      n = num_tweets) %>%
    dplyr::mutate(
      key = paste(screen_name, created_at),
      query = query) %>%
    dplyr::distinct(key, .keep_all = distinct)
  
  if (reduced_tweets == TRUE) {
    
    reduced <- raw_tweets %>%
      dplyr::select(user_id,
                    status_id,
                    created_at,
                    screen_name,
                    text,
                    hashtags,
                    location,
                    key,
                    query)

    return(reduced)

  } else if (reduced_tweets == FALSE) {

    return(raw_tweets)

  } else {

    message("Please make a selection for 'reduced_tweets")

  }

}
