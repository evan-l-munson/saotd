
# test out tweet_acquire

puppies <- tweet_acquire(
  twitter_app = "saotd_spot2ring", 
              consumer_api_key = Sys.getenv('consumer_api_key'), 
              consumer_api_secret_key = Sys.getenv('consumer_api_secret_key'), 
              access_token = Sys.getenv('access_token'), 
              access_token_secret = Sys.getenv('access_token_secret'), 
              query = "#puppies", 
              num_tweets = 100, 
              distinct = TRUE)


user_token <- rtweet::create_token(
  app = "saotd_spot2ring",
  consumer_key = Sys.getenv('consumer_api_key'),
  consumer_secret = Sys.getenv('consumer_api_secret_key'),
  access_token = Sys.getenv('access_token'),
  access_secret = Sys.getenv('access_token_secret')
)

raw_tweets <- rtweet::search_tweets(token = user_token,
                                    q = "#puppies",
                                    n = 50) %>% 
  dplyr::mutate(key = paste(screen_name, created_at)) %>% 
  dplyr::distinct(key, .keep_all = TRUE) %>% 
  dplyr::mutate(query = "#puppies")

