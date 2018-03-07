## ----warning=FALSE, message=FALSE----------------------------------------
library(SAoTD)
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
#  Acquire(consumer_key = consumer_key,
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
TD_Tidy <- SAoTD::Tidy(DataFrame = TD)

TD_Tidy$Token[11:20] %>% 
  knitr::kable("html")

## ----message=FALSE-------------------------------------------------------
SAoTD::Unigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Uni-Grams")

## ----message=FALSE-------------------------------------------------------
SAoTD::Bigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Bi-Grams")

## ----message=FALSE-------------------------------------------------------
SAoTD::Trigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Tri-Grams")

## ----message=FALSE, message=FALSE, error=FALSE---------------------------
TD_Merge <- Merge.Terms(DataFrame = TD, term = "ice cream", term_replacement = "icecream")

## ----message=FALSE-------------------------------------------------------
SAoTD::Unigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Uni-Grams")

## ----message=FALSE-------------------------------------------------------
SAoTD::Bigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Bi-Grams")

## ----message=FALSE-------------------------------------------------------
SAoTD::Trigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Tri-Grams")

## ----fig.align='center'--------------------------------------------------
TD_Corr <- SAoTD::Word.Corr(DataFrameTidy = TD_Tidy, 
                            number = 200, 
                            sort = TRUE)

SAoTD::Word.Corr.Plot(WordCorr = TD_Corr, 
                      Correlation = .1, 
                      layout = "fr", 
                      edge_color = "blue", 
                      node_color = "black", 
                      node_size = 1)

## ------------------------------------------------------------------------
TD_Scores <- SAoTD::Scores(DataFrameTidy = TD_Tidy, 
                           HT_Topic = "hashtag")

## ----fig.align='center'--------------------------------------------------
SAoTD::PosNeg.Words(DataFrameTidy = TD_Tidy, 
                    num_words = 10)

## ----fig.align='center'--------------------------------------------------
SAoTD::PosNeg.Words(DataFrameTidy = TD_Tidy, 
                    num_words = 10, 
                    filterword = "fail")

## ------------------------------------------------------------------------
SAoTD::Max.Scores(DataFrameTidyScores = TD_Scores,
                  HT_Topic = "hashtag")

## ------------------------------------------------------------------------
SAoTD::Min.Scores(DataFrameTidyScores = TD_Scores, 
                  HT_Topic = "hashtag")

## ------------------------------------------------------------------------
SAoTD::Max.Scores(DataFrameTidyScores = TD_Scores, 
                  HT_Topic = "hashtag", 
                  HT_Topic_Selection = "kitten")

## ----fig.align='center', warning=FALSE, message=FALSE, results="hide"----
SAoTD::Number.Topics(DataFrame = TD, 
                     num_cores = 2L, 
                     min_clusters = 2, 
                     max_clusters = 12, 
                     skip = 1, 
                     set_seed = 1234)

## ----fig.align='center', warning=FALSE, message=FALSE, results='asis'----
TD_Topics <- SAoTD::Tweet.Topics(DataFrame = TD, 
                                 clusters = 5, 
                                 method = "Gibbs", 
                                 set_seed = 1234, 
                                 num_terms = 10)

## ------------------------------------------------------------------------
TD_Topics <- TD_Topics %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^1$", "travel")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^2$", "recreation")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^3$", "hiring")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^4$", "cats")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^5$", "random"))

## ------------------------------------------------------------------------
TD_Topics_Tidy <- SAoTD::Tidy(DataFrame = TD_Topics)
TD_Topics_Scores <- SAoTD::Scores(DataFrameTidy = TD_Topics_Tidy, 
                                  HT_Topic = "topic")

## ------------------------------------------------------------------------
SAoTD::Max.Scores(DataFrameTidyScores = TD_Topics_Scores,
                  HT_Topic = "topic")

## ------------------------------------------------------------------------
SAoTD::Min.Scores(DataFrameTidyScores = TD_Topics_Scores, 
                  HT_Topic = "topic")

## ------------------------------------------------------------------------
SAoTD::Max.Scores(DataFrameTidyScores = TD_Topics_Scores, 
                  HT_Topic = "topic", 
                  HT_Topic_Selection = "cats")

## ----fig.align='center'--------------------------------------------------
SAoTD::Corups.Distribution(DataFrameTidyScores = TD_Scores, 
                           binwidth = 1, 
                           color = "black", 
                           fill = "white")

## ----fig.align='center'--------------------------------------------------
SAoTD::Distribution(DataFrameTidyScores = TD_Scores, 
                    HT_Topic = "hashtag", 
                    binwidth = 1, 
                    color = "black", 
                    fill = "white")

## ----fig.align='center'--------------------------------------------------
SAoTD::BoxPlot(DataFrameTidyScores = TD_Scores, 
               HT_Topic = "hashtag")

## ----fig.align='center'--------------------------------------------------
SAoTD::ViolinPlot(DataFrameTidyScores = TD_Scores,
                  HT_Topic = "hashtag")

## ----fig.align='center', warning=FALSE, message=FALSE--------------------
SAoTD::TimeScale(DataFrameTidyScores = TD_Scores, 
                 HT_Topic = "hashtag")

## ----fig.align='center', warning=FALSE, message=FALSE--------------------
SAoTD::WorldMap(DataFrame = TD, 
                HT_Topic = "hashtag")

## ----fig.align='center'--------------------------------------------------
SAoTD::Corups.Distribution(DataFrameTidyScores = TD_Topics_Scores, 
                           binwidth = 1, 
                           color = "black", 
                           fill = "white")

## ----fig.align='center'--------------------------------------------------
SAoTD::Distribution(DataFrameTidyScores = TD_Topics_Scores, 
                    HT_Topic = "topic", 
                    binwidth = 1, 
                    color = "black", 
                    fill = "white")

## ----fig.align='center'--------------------------------------------------
SAoTD::BoxPlot(DataFrameTidyScores = TD_Topics_Scores, 
               HT_Topic = "topic")

## ----fig.align='center'--------------------------------------------------
SAoTD::ViolinPlot(DataFrameTidyScores = TD_Topics_Scores,
                  HT_Topic = "topic")

## ----fig.align='center', warning=FALSE, message=FALSE--------------------
SAoTD::TimeScale(DataFrameTidyScores = TD_Topics_Scores, 
                 HT_Topic = "topic")

## ----fig.align='center', warning=FALSE, message=FALSE--------------------
SAoTD::WorldMap(DataFrame = TD_Topics, 
                HT_Topic = "topic")

