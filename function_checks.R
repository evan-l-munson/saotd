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

  # tidy_puppy %>%
  #   dplyr::group_by(Token) %>%
  #   dplyr::filter(dplyr::n() >= 10) %>%
  #   widyr::pairwise_cor(Token, key, sort = TRUE)
  
  
  
corr_puppies <- saotd::word_corr(DataFrameTidy = tidy_puppy, 
                                 number = 10, 
                                 sort = TRUE)  


# Word Corr Network -------------------------------------------------------

saotd::word_corr_network(WordCorr = corr_puppies)

# Correlation = 0.15
# layout = "fr"
# edge_color = "royalblue"
# node_color = "black"
# node_size = 2
# set_seed = 1234
# 
# corr_puppies %>%
#   dplyr::filter(correlation > Correlation) %>%
#   igraph::graph_from_data_frame() %>%
#   ggraph::ggraph(layout = layout) +
#   ggraph::geom_edge_link(
#     ggplot2::aes(
#       edge_alpha = Correlation, 
#       edge_width = Correlation),
#     edge_colour = edge_color,
#     show.legend = TRUE) +
#   ggraph::geom_node_point(colour = node_color, 
#                           size = node_size) +
#   ggraph::geom_node_text(ggplot2::aes(label = name), 
#                          repel = TRUE) +
#   ggplot2::ggtitle("Word Correlation Network") +
#   ggplot2::theme_void()

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


# Max Score ---------------------------------------------------------------

max_score_HT <- 
  saotd::tweet_max_scores(
    DataFrameTidyScores = score_puppies_ht,
    HT_Topic = "hashtag")
max_score_HT

# unnest list
# https://stackoverflow.com/questions/52286482/function-to-extract-all-list-elements-from-a-dataframe-column-into-individual-co
# https://stackoverflow.com/questions/44832059/r-how-to-extract-a-list-from-a-dataframe

max_score_HT_sel <-
  saotd::tweet_max_scores(
    DataFrameTidyScores = score_puppies_ht,
    HT_Topic = "hashtag",
    HT_Topic_Selection = "puppy")
max_score_HT_sel

max_score_topic <- saotd::tweet_max_scores(
  DataFrameTidyScores = score_puppies_topic, 
  HT_Topic = "topic")
max_score_topic

max_score_topic_sel <- saotd::tweet_max_scores(
  DataFrameTidyScores = score_puppies_topic, 
  HT_Topic = "topic", 
  HT_Topic_Selection = 4)
max_score_topic_sel


# Min Scores --------------------------------------------------------------

min_score_HT <- 
  saotd::tweet_min_scores(
    DataFrameTidyScores = score_puppies_ht,
    HT_Topic = "hashtag")
min_score_HT

# unnest list
# https://stackoverflow.com/questions/52286482/function-to-extract-all-list-elements-from-a-dataframe-column-into-individual-co
# https://stackoverflow.com/questions/44832059/r-how-to-extract-a-list-from-a-dataframe

min_score_HT_sel <-
  saotd::tweet_min_scores(
    DataFrameTidyScores = score_puppies_ht,
    HT_Topic = "hashtag",
    HT_Topic_Selection = "puppy")
min_score_HT_sel

min_score_topic <- saotd::tweet_min_scores(
  DataFrameTidyScores = score_puppies_topic, 
  HT_Topic = "topic")
min_score_topic

min_score_topic_sel <- saotd::tweet_min_scores(
  DataFrameTidyScores = score_puppies_topic, 
  HT_Topic = "topic", 
  HT_Topic_Selection = 4)
min_score_topic_sel


# posneg_words ------------------------------------------------------------

pos_neg <- saotd::posneg_words(
  DataFrameTidy = tidy_puppy, 
  num_words = 6, 
  # filterword = "illegal"
  )
pos_neg


# Tweet Corpus Dist -------------------------------------------------------

corp_dist <- saotd::tweet_corpus_distribution(
  DataFrameTidyScores = score_puppies)
corp_dist


# tweet distributions -----------------------------------------------------

bin_width = 10
color = "black"
fill = "grey"

xxx <- score_puppies_ht %>%
  tidyr::unnest(
    cols = hashtags, 
    keep_empty = FALSE) %>% 
  dplyr::group_by(hashtags, TweetSentimentScore) %>% 
  dplyr::count() %>% 
  dplyr::filter(!is.na(hashtags)) %>% 
  dplyr::ungroup() %>% 
  dplyr::slice_max(n, n = 2) %>% 
  ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
  ggplot2::geom_histogram(
    stat = "count", 
    bin = bin_width, 
    colour = color, 
    fill = fill) +
  ggplot2::facet_wrap(~hashtags, ncol = 2) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("Sentiment Score Distribution Across all #Hashtags") +
  ggplot2::xlab('Sentiment') +
  ggplot2::ylab('Count') +
  ggplot2::theme_bw()


tweet_dist_ht <- 
  saotd::tweet_distribution(
    DataFrameTidyScores = score_puppies_ht, 
    HT_Topic = "hashtag")
tweet_dist_ht

tweet_dist_topic <- 
  saotd::tweet_distribution(
    DataFrameTidyScores = score_puppies_topic,
    bin_width = 5,
    HT_Topic = "topic")
tweet_dist_topic


# tweet_box ---------------------------------------------------------------

# works for topics
box_topic <- saotd::tweet_box(
  DataFrameTidyScores = score_puppies_topic, 
  HT_Topic = "topic")
box_topic

box_ht <- saotd::tweet_box(
  DataFrameTidyScores = score_puppies_ht, 
  HT_Topic = "hashtag")
box_ht

wth <- score_puppies_ht %>% 
  tidyr::unnest(
    cols = hashtags, 
    keep_empty = FALSE) %>% 
  dplyr::mutate(
    hashtags = tolower(hashtags)) %>% 
  dplyr::group_by(hashtags) %>% 
  dplyr::count()


# tweet_violin ------------------------------------------------------------

v_ht <- saotd::tweet_violin(
  DataFrameTidyScores = score_puppies_ht, 
  HT_Topic = "hashtag")
v_ht

v_topic <- saotd::tweet_violin(
  DataFrameTidyScores = score_puppies_topic, 
  HT_Topic = "topic")
v_topic


# tweet_time --------------------------------------------------------------

time_topic <- saotd::tweet_time(
  DataFrameTidyScores = score_puppies_topic,
  HT_Topic = "topic")
time_topic

ht_sel <- c("dog", "puppy", "cat")
time_ht <- saotd::tweet_time(
  DataFrameTidyScores = score_puppies_ht, 
  HT_Topic = "hashtag")
time_ht

# need to add an additional if/else statement for the hashtag section.  The 
#   intent is to allow a user to either run everything with all hashtags, or a 
#   user to provide a list of hashtags which will enable them to have a much 
#   better view and focus on selected hashtags.



hashtag_selection <- c("dog", "puppy", "cat")

xxx <- ddd %>% 
  tidyr::unnest(
    cols = hashtags, 
    keep_empty = FALSE) %>% 
  dplyr::group_by(hashtags, date) %>% 
  dplyr::summarise(
    DayScore = sum(TweetSentimentScore)) %>% 
  dplyr::filter(
    hashtags %in% hashtag_selection) %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = factor(date), 
      y = DayScore, 
      colour = hashtags)) + 
  ggplot2::geom_point() +
  ggplot2::geom_path(ggplot2::aes(group=1)) +
  ggplot2::geom_hline(yintercept = 0, color = "black") +
  ggplot2::facet_wrap(~hashtags, ncol = 2, scales = "free_y") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("Sentiment Scores Across all #Hashtags") +
  ggplot2::xlab('Day') +
  ggplot2::ylab('Daily Sentiment Score') +
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1))
xxx



aaa <- score_puppies_ht
bbb <- score_puppies_ht %>% 
  dplyr::mutate(date = date + 1)
ccc <- score_puppies_ht %>% 
  dplyr::mutate(date = date + 5)
ddd <- rbind(aaa, bbb, ccc)
