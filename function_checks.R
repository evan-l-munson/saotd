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
              reduced_tweets = TRUE,
              distinct = TRUE)


# tweet_tidy --------------------------------------------------------------

tidy_puppy <- saotd::tweet_tidy(DataFrame = puppies)


# unigram -----------------------------------------------------------------

uni_tweet <- saotd::unigram(DataFrame = puppies)
uni_tweet

# bigrams -----------------------------------------------------------------

bi_tweet <- saotd::bigram(DataFrame = puppies)
bi_tweet

# trigrams ----------------------------------------------------------------

tri_tweets <- saotd::trigram(DataFrame = puppies)
tri_tweets

# merge terms -------------------------------------------------------------

bad_puppies <- saotd::merge_terms(DataFrame = puppies, 
                                  term = "puppies", 
                                  term_replacement = "bad_puppies")

  

# Bigram network ----------------------------------------------------------

saotd::bigram_network(BiGramDataFrame = bi_tweet, number = 5)


# word corr ---------------------------------------------------------------

  tidy_puppy %>%
    dplyr::group_by(Token) %>%
    dplyr::filter(dplyr::n() >= 10) %>%
    widyr::pairwise_cor(Token, key, sort = TRUE)
  
  
  
corr_puppies <- saotd::word_corr(DataFrameTidy = tidy_puppy, 
                                 number = 10, 
                                 sort = TRUE)  


# Word Corr Network -------------------------------------------------------

saotd::word_corr_network(WordCorr = corr_puppies)

# number topics -----------------------------------------------------------

num_puppies <- saotd::number_topics(DataFrame = puppies, num_cores = 4)


# tweet topics ------------------------------------------------------------

topics_puppies <- saotd::tweet_topics(DataFrame = puppies, clusters = 5)


# Score -------------------------------------------------------------------

score_puppies_ht <- saotd::tweet_scores(
  DataFrameTidy = tidy_puppy, 
  HT_Topic = "hashtag")

tidy_topics <- saotd::tweet_tidy(
  DataFrame = topics_puppies)

score_puppies_topic <- saotd::tweet_scores(
  DataFrameTidy = tidy_topics, 
  HT_Topic = "topic")
