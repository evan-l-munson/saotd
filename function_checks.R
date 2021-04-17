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
saotd::unigram(DataFrame = test_unigram_df)


# bigrams -----------------------------------------------------------------

bi_tweet <- saotd::bigram(DataFrame = puppies)


# trigrams ----------------------------------------------------------------

tri_tweets <- saotd::trigram(DataFrame = puppies)

# merge terms -------------------------------------------------------------

bad_puppies <- saotd::merge_terms(DataFrame = puppies, 
                                  term = "puppies", 
                                  term_replacement = "good_puppies")

new_merge <- puppies %>% 
  dplyr::mutate(text = stringr::str_replace(string = text, 
                                                pattern = "puppies", 
                                                replacement = "good_puppies"))

new_merge_2 <- puppies %>% 
  dplyr::mutate(text = gsub(x = text,
                            pattern = "puppies", 
                            replacement = "good_puppies", 
                            ignore.case = TRUE))
                
  all.equal(bad_puppies, new_merge_2)
  

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
