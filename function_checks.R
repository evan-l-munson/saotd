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

  Bigram_Network <- bi_tweet %>% 
    dplyr::filter(n > 5) %>% 
    igraph::graph_from_data_frame() %>% 
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(
      ggplot2::aes(
        edge_alpha = 1,
        edge_width = scales::rescale(n, to=c(1,10))),
      edge_colour = "royalblue",
      show.legend = TRUE) +
    ggraph::geom_node_point(
      colour = "black", 
      size = 3, show.legend = TRUE) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name), 
      repel = TRUE, show.legend = TRUE) +
    ggplot2::ggtitle("Bi-Gram Network") 
  
  
  BiGramDataFrame = bi_tweet
  number = 5
  layout = "fr"
  edge_color = "green"
  node_color = "black"
  node_size = 3
  set_seed = 1234

  set.seed(set_seed)
      
  TD_Bigram_Network <- BiGramDataFrame %>% 
    dplyr::filter(n > number) %>% 
    igraph::graph_from_data_frame() %>% 
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        edge_alpha = 1,
        edge_width = scales::rescale(n, to=c(1,10))),
      edge_colour = "green",
      show.legend = TRUE) +
    ggraph::geom_node_point(
      colour = node_color, 
      size = node_size) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name), 
      repel = TRUE) +
    ggplot2::ggtitle("Bi-Gram Network") +
    ggplot2::theme_void()
  TD_Bigram_Network
  

# word corr ---------------------------------------------------------------

  tidy_puppy %>%
    dplyr::group_by(Token) %>%
    dplyr::filter(dplyr::n() >= 10) %>%
    widyr::pairwise_cor(Token, key, sort = TRUE)
  
  
  
corr_puppies <- saotd::word_corr(DataFrameTidy = tidy_puppy, 
                                 number = 10, 
                                 sort = TRUE)  
