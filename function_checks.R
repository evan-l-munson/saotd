library(saotd)

# tweet_acquire -----------------------------------------------------------

puppies <- saotd::tweet_acquire(
  twitter_app = "saotd_spot2ring", 
              consumer_api_key = Sys.getenv('consumer_api_key'), 
              consumer_api_secret_key = Sys.getenv('consumer_api_secret_key'), 
              access_token = Sys.getenv('access_token'), 
              access_token_secret = Sys.getenv('access_token_secret'), 
              query = "#puppies", 
              num_tweets = 100, 
              distinct = TRUE)


# tweet_tidy --------------------------------------------------------------

tidy_puppy <- saotd::tweet_tidy(DataFrame = puppies)


# unigram -----------------------------------------------------------------

uni_tweet <- saotd::unigram(DataFrame = puppies)

