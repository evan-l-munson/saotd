## ----warning=FALSE, message=FALSE----------------------------------------
library(saotd)
library(tidytext)
library(dplyr)
library(stringr)
library(knitr)
library(utils)

## ----eval=FALSE----------------------------------------------------------
#  consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
#  consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#  access_token <- "XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#  access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#  
#  hashtags <- c("#job", "#Friday", "#fail", "#icecream", "#random", "#kitten", "#airline")
#  
#  tweet_acquire(consumer_key = consumer_key,
#          consumer_secret = consumer_secret,
#          access_token = access_token,
#          access_secret = access_secret,
#          HT = hashtags,
#          num_tweets = 1000,
#          file_name = "test_tweets.RData",
#          distinct = TRUE)
#  
#  load("test_tweets.RData")

## ------------------------------------------------------------------------
data("raw_tweets")
TD <- raw_tweets

## ------------------------------------------------------------------------
TD_Tidy <- saotd::tweet_tidy(DataFrame = TD)

TD_Tidy$Token[3:8] %>% 
  knitr::kable("html")

## ----message=FALSE-------------------------------------------------------
saotd::unigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Uni-Grams")

## ----message=FALSE-------------------------------------------------------
saotd::bigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Bi-Grams")

## ----message=FALSE-------------------------------------------------------
saotd::trigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Tri-Grams")

## ----message=FALSE, message=FALSE, error=FALSE---------------------------
TD_Merge <- merge_terms(DataFrame = TD, term = "cancelled flight", term_replacement = "cancelled_flight")

## ----message=FALSE-------------------------------------------------------
saotd::unigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Uni-Grams")

## ----message=FALSE-------------------------------------------------------
saotd::bigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Bi-Grams")

## ----message=FALSE-------------------------------------------------------
saotd::trigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Tri-Grams")

## ----fig.align='center'--------------------------------------------------
TD_Bigram <- saotd::bigram(DataFrame = TD_Merge)

saotd::bigram_network(BiGramDataFrame = TD_Bigram,
                      number = 90,
                      layout = "fr",
                      edge_color = "blue",
                      node_color = "black",
                      node_size = 3,
                      set_seed = 1234)

## ----fig.align='center'--------------------------------------------------
TD_Corr <- saotd::word_corr(DataFrameTidy = TD_Tidy, 
                            number = 200, 
                            sort = TRUE)

saotd::word_corr_network(WordCorr = TD_Corr, 
                         Correlation = .1, 
                         layout = "fr", 
                         edge_color = "blue", 
                         node_color = "black", 
                         node_size = 1)

## ------------------------------------------------------------------------
TD_Scores <- saotd::tweet_scores(DataFrameTidy = TD_Tidy, 
                                 HT_Topic = "hashtag")

## ----fig.align='center'--------------------------------------------------
saotd::posneg_words(DataFrameTidy = TD_Tidy, 
                    num_words = 10)

## ----fig.align='center'--------------------------------------------------
saotd::posneg_words(DataFrameTidy = TD_Tidy, 
                    num_words = 10, 
                    filterword = "fail")

## ------------------------------------------------------------------------
saotd::tweet_max_scores(DataFrameTidyScores = TD_Scores,
                        HT_Topic = "hashtag")

## ------------------------------------------------------------------------
saotd::tweet_min_scores(DataFrameTidyScores = TD_Scores, 
                        HT_Topic = "hashtag")

## ------------------------------------------------------------------------
saotd::tweet_max_scores(DataFrameTidyScores = TD_Scores, 
                        HT_Topic = "hashtag", 
                        HT_Topic_Selection = "kitten")

## ----fig.align='center', warning=FALSE, message=FALSE, results="hide"----
saotd::number_topics(DataFrame = TD, 
                     num_cores = 2L, 
                     min_clusters = 2, 
                     max_clusters = 12, 
                     skip = 1, 
                     set_seed = 1234)

## ----fig.align='center', warning=FALSE, message=FALSE, results='asis'----
TD_Topics <- saotd::tweet_topics(DataFrame = TD, 
                                 clusters = 5, 
                                 method = "Gibbs", 
                                 set_seed = 1234, 
                                 num_terms = 10)

## ------------------------------------------------------------------------
TD_Topics <- TD_Topics %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^1$", "luggage")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^2$", "gate_delay")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^3$", "customer_service")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^4$", "enjoy")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^5$", "other_delay"))

## ------------------------------------------------------------------------
TD_Topics_Tidy <- saotd::tweet_tidy(DataFrame = TD_Topics)
TD_Topics_Scores <- saotd::tweet_scores(DataFrameTidy = TD_Topics_Tidy, 
                                        HT_Topic = "topic")

## ------------------------------------------------------------------------
saotd::tweet_max_scores(DataFrameTidyScores = TD_Topics_Scores,
                        HT_Topic = "topic")

## ------------------------------------------------------------------------
saotd::tweet_min_scores(DataFrameTidyScores = TD_Topics_Scores,  
                        HT_Topic = "topic")

## ------------------------------------------------------------------------
saotd::tweet_max_scores(DataFrameTidyScores = TD_Topics_Scores, 
                        HT_Topic = "topic", 
                        HT_Topic_Selection = "United")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_corpus_distribution(DataFrameTidyScores = TD_Scores, 
                                 color = "black", 
                                 fill = "white")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_distribution(DataFrameTidyScores = TD_Scores, 
                          binwidth = 1,
                          HT_Topic = "hashtag", 
                          color = "black", 
                          fill = "white")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_box(DataFrameTidyScores = TD_Scores, 
                 HT_Topic = "hashtag")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_violin(DataFrameTidyScores = TD_Scores,
                    HT_Topic = "hashtag")

## ----fig.align='center', warning=FALSE, message=FALSE--------------------
saotd::tweet_time(DataFrameTidyScores = TD_Scores, 
                  HT_Topic = "hashtag")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_corpus_distribution(DataFrameTidyScores = TD_Topics_Scores, 
                                 color = "black", 
                                 fill = "white")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_distribution(DataFrameTidyScores = TD_Topics_Scores, 
                          binwidth = 1,
                          HT_Topic = "topic", 
                          color = "black", 
                          fill = "white")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_box(DataFrameTidyScores = TD_Topics_Scores, 
                 HT_Topic = "topic")

## ----fig.align='center'--------------------------------------------------
saotd::tweet_violin(DataFrameTidyScores = TD_Topics_Scores,
                    HT_Topic = "topic")

## ----fig.align='center', warning=FALSE, message=FALSE--------------------
saotd::tweet_time(DataFrameTidyScores = TD_Topics_Scores, 
                  HT_Topic = "topic")

