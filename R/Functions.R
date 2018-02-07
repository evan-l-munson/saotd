
# Packages ----------------------------------------------------------------

library(data.table)
library(readxl)
#library(utils)
library(twitteR)
library(httr)
library(readr)
library(plyr)
library(tidyverse)
library(tidytext)
library(lubridate)
library(topicmodels)
library(ggraph)
library(igraph)
library(ldatuning)
library(widyr)

# Bing Lexicon ------------------------------------------------------------

Bing <- as.data.frame(get_sentiments("bing")) %>% 
  plyr::rename(c("word" = "Token", "sentiment" = "Sentiment"))

# Acquire Tweets ----------------------------------------------------------

#' @title Acquire Twitter Tweets
#'
#' @description Function will enable a user to access the twitter API throught the 
#' [Twitter Application Management](https://apps.twitter.com/) site.
#' Once a user has a twitter developers account and has recieved their individual consumer key, 
#' consumer secret key, access token, and access secret key and acquire tweets they can 
#' acquire tweets based on a list of hashtags and a requested number of entires per hashtag.
#'
#' @param consumer_key Twitter Application management consumer key.
#' @param consumer_secret Twitter Application management consumer secret key.
#' @param access_token Twitter Application management access token.
#' @param access_secret Twitter Application management access secret key.
#' @param HT A single hashtag or a list of hashtags the user has specified.
#' @param file_name User desired output .RData file name.
#' @param distinct Logical.  If distinct = TRUE, the function removes multiple tweets that originate from the same twitter id at the exact same time.
#' 
#' @return A DataFrame.
#' 
#' @examples 
#' consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
#' consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_token <- "XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' 
#' hashtags <- c("#job", "#Friday", "#fail", "#icecream", "#random", "#kitten", "#airline")
#' 
#' Acquire(consumer_key = consumer_key, 
#'         consumer_secret = consumer_secret, 
#'         access_token = access_token, 
#'         access_secret = access_secret, 
#'         HT = hashtags, 
#'         num_tweets = 10, 
#'         file_name = "test_tweets.RData"
#'         distinct = TRUE)
#'         
#' load("test_tweets.RData")
#' View(raw_tweets)
#' 
#' @export

Acquire <- function(consumer_key, consumer_secret, access_token, access_secret, HT, num_tweets, file_name, distinct = TRUE) {
  
  twitteR::setup_twitter_oauth(consumer_key, 
                               consumer_secret, 
                               access_token, 
                               access_secret)
  
  twitter_data <- list()
  for (i in ht) {
    twitter_data[[i]] <- twitteR::twListToDF(twitteR::searchTwitter(i, 
                                                                    n = num_tweets, 
                                                                    lang = "en")) %>% 
      dplyr::mutate(hashtag = substr(i, 2, nchar(i)))
  }
  
  raw_tweets <- purrr::map_df(twitter_data, rbind) %>% 
    dplyr::mutate(key = paste(screenName, created)) %>% 
    dplyr::distinct(key, .keep_all = distinct)
  
    save(raw_tweets, file = file_name)
    
}

# Tidy and Scores ---------------------------------------------------------

#' @title Tidy Twitter Data
#'
#' @description Function to Tidy Twitter Data and remove all emoticons whilie maintaiing actual tweet.
#'
#' @param DataFrame DataFrame of Twitter Data.
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- TD.Tidy(DataFrame = data)
#' tidy_data
#' 
#' @export

Tidy <- function(DataFrame) {
  reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
  
  TD_Tidy <- DataFrame %>%
    dplyr::mutate(cleantext = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "#", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "http", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "[:punct:]", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(output = word, input = cleantext, token = "words", drop = TRUE) %>% 
    dplyr::filter(!word %in% stop_words$word) %>% 
    plyr::rename(c("word" = "Token"))
  return(TD_Tidy)
}

#' @title Score Tidy Twitter Data
#'
#' @description Function to Calculate Sentiment Scores that will account for sentiment by hashtag or topic.
#'
#' @param DataFrameTidy DataFrame of Twitter Data that has been tidy'd.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic"
#' 
#' @return A Scored DataFrame.
#' 
#' @examples 
#' library(SAoTD)
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' score_data <- TD.Scores(DataFrameTidy = tidy_data, 
#'                         HT_Topic = "hashtag")
#' score_data
#' 
#' @export

Scores <- function(DataFrameTidy, HT_Topic) {
  if(HT_Topic == "hashtag") {
    TD_Hashtag_Scores <- DataFrameTidy %>% 
      dplyr::inner_join(Bing, by = "Token") %>% 
      dplyr::mutate(method = "Bing") %>% 
      dplyr::group_by(text, method, hashtag, created, key, Sentiment) %>% 
      dplyr::count(method, hashtag, created, key, Sentiment) %>%  
      tidyr::spread(Sentiment, n, fill = 0) %>% 
      dplyr::mutate(TweetSentimentScore = positive - negative) %>% 
      dplyr::mutate(TweetSentiment = ifelse(TweetSentimentScore == 0, "neutral",
                                            ifelse(TweetSentimentScore > 0, "positive", "negative"))) %>% 
      dplyr::mutate(date = lubridate::as_date(created))
    return(TD_Hashtag_Scores)
  } else {
    TD_Topic_Scores <- DataFrameTidy %>% 
      dplyr::inner_join(Bing, by = "Token") %>% 
      dplyr::mutate(method = "Bing") %>% 
      dplyr::group_by(text, method, Topic, created, key, Sentiment) %>% 
      dplyr::count(method, Topic, created, key, Sentiment) %>%  
      tidyr::spread(Sentiment, n, fill = 0) %>% 
      dplyr::mutate(TweetSentimentScore = positive - negative) %>% 
      dplyr::mutate(TweetSentiment = ifelse(TweetSentimentScore == 0, "neutral",
                                            ifelse(TweetSentimentScore > 0, "positive", "negative"))) %>% 
      dplyr::mutate(date = lubridate::as_date(created))
    return(TD_Topic_Scores)
  }
}

#' @title Merge Terms
#'
#' @description Function to merge terms within a dataframe and prevent redundancy in the analysis.  
#' For example many users may refer to the same entity in multiple different ways: 
#' President Trump, The U.S. President, POTUS, Trump, President Donald Trump, Donald Trump, etc.  
#' While each entry is different, they all refer to the same individual.  Using Merge Terms will allow all be converted into a single term. 
#'
#' @param DataFrame DataFrame of Twitter Data.
#' @param term Term selected for merging.
#' @param term_replacement Desired replacement term.
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' library(SAoTD)
#' data <- twitter_data
#' data <- Merge.Terms(DataFrame = data, 
#'                     term = "ice cream", 
#'                     term_replacement = "ice_cream")
#' data 
#' 
#' @export

Merge.Terms <- function(DataFrame, term, term_replacement){
  for(i in 1: length(DataFrame$text)){
    DataFrame[i, "text"] <- DataFrame[i, "text"] %>% 
      gsub(pattern = as.character(term),
           replacement = as.character(term_replacement),
           ignore.case = TRUE)   
  }
  DataFrame <- DataFrame
}

# LDA ---------------------------------------------------------------------

#' @title Number Topics
#'
#' @description Determines the optimal number of Latent topics within a dataframe by tuning the Latent Dirichlet Allocation (LDA) model parameters.  
#' Uses the `ldatuning` package and outputs an ldatuning plot.
#'
#' @param DataFrame DataFrame of Twitter Data.
#' @param num_cores The number of CPU cores to processes models simultaneously (2L for dual core processor).
#' @param min_clusters Lower range for the number of clusters.
#' @param max_clusters Upper range for the number of clusters.
#' @param skip Integer; The number of clusters to skip between entries.
#' @param set_seed Seed for reproducable results.
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' library(SAoTD)
#' data <- twitter_data
#' LDA_Topic_Plot <- Number.Topics(DataFrame = data,
#'                                 num_cores = 2L,
#'                                 min_clusters = 2,
#'                                 max_clusters = 12, 
#'                                 skip = 2,
#'                                 set_seed = 1234)
#'
#' LDA_Topic_Plot 
#' 
#' @export

Number.Topics <- function(DataFrame, num_cores, min_clusters = 2, max_clusters = 12, skip = 2, set_seed = 1234) {
  lda_prep <- DataFrame %>% 
    dplyr::mutate(text = base::iconv(DataFrame$text, "latin1", "ASCII", sub="")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% # Remove hashtag
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% # Remove punctuation
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%  # Remove links
    dplyr::group_by(key) %>%
    tidytext::unnest_tokens(word, text) %>% 
    dplyr::anti_join(stop_words) %>% 
    dplyr::count(key, word, sort = TRUE) %>% 
    tidytext::cast_dtm(key, word, n) # create DTM

  # Compute Values
  values <- ldatuning::FindTopicsNumber(lda_prep, 
                                        topics = seq(from = min_clusters, to = max_clusters, by = skip),
                                        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                        method = "Gibbs",
                                        mc.cores = num_cores,
                                        verbose = TRUE)
  
  # Plot
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(values["topics"], base::apply(columns, 2, function(column) {scales::rescale(column, to = c(0, 1), from = range(column))}))
  values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
  values$group <- values$variable %in% c("Griffiths2004", "Deveaud2014")
  values$group <- base::factor(values$group, levels = c(FALSE, TRUE), labels = c("minimize", "maximize"))
  p <- ggplot(values, aes_string(x = "topics", y = "value", group = "variable"))
  p <- p + geom_line()
  p <- p + geom_point(aes_string(shape = "variable"), size = 3)
  p <- p + guides(size = FALSE, shape = guide_legend(title = "metrics:"))
  p <- p + scale_x_continuous(breaks = values$topics)
  p <- p + labs(x = "number of topics", y = NULL)
  p <- p + facet_grid(group ~ .)
  p <- p + theme_bw() %+replace% theme(panel.grid.major.y = element_blank(), 
                                       panel.grid.minor.y = element_blank(), 
                                       panel.grid.major.x = element_line(colour = "grey70"), 
                                       panel.grid.minor.x = element_blank(), 
                                       legend.key = element_blank(), 
                                       strip.text.y = element_text(angle = 90))
}

#' @title Tweet Topics
#'
#' @description Determines the Latent topics within a dataframe by using Latent Dirichlet Allocation (LDA) model parameters.  
#' Uses the `ldatuning` package and outputs an ldatuning plot.  
#' Prepares tweet text, creates DTM, conducts LDA, display data terms associated with each topic.
#'
#' @param DataFrame DataFrame of Twitter Data.
#' @param num_cores The number of CPU cores to processes models simultaneously (2L for dual core processor).
#' @param min_clusters Lower range for the number of clusters.
#' @param max_clusters Upper range for the number of clusters.
#' @param skip Integer; The number of clusters to skip between entries.
#' @param set_seed Seed for reproducable results.
#' 
#' @return Returns LDA topics.
#' 
#' @examples 
#' library(SAoTD)
#' data <- twitter_data
#' LDA_data <- Tweet.Topics(DataFrame = data,
#'                          clusters = 8,
#'                          method = "Gibbs",
#'                          set_seed = 1234,
#'                          num_terms = 10)
#'
#' LDA_data
#' 
#' @export

Tweet.Topics <- function(DataFrame, clusters, method = "Gibbs", set_seed = 1234, num_terms = 10) {
  lda_prep <- DataFrame %>% 
    dplyr::mutate(text = iconv(DataFrame$text, "latin1", "ASCII", sub="")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% # Remove hashtag
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% # Remove punctuation
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%  # Remove links
    dplyr::group_by(key) %>%
    tidytext::unnest_tokens(word, text) %>% 
    dplyr::anti_join(stop_words) %>% 
    dplyr::count(key, word, sort = TRUE) %>% 
    tidytext::cast_dtm(key, word, n)
  
  # Run LDA using Gibbs sampling
  ldaout <- topicmodels::LDA(lda_prep, k = clusters, method = method, control = list(seed = set_seed))
  
  ldaout_topics <- as.matrix(topicmodels::topics(ldaout))
  
  ldaout_terms <- as.matrix(topicmodels::terms(ldaout, num_terms))
  
  # probabilities associated with each topic assignment
  topicProbabilities <- as.data.frame(ldaout@gamma)
  data.topics <- topics(ldaout, 1)
  data.terms <- as.data.frame(terms(ldaout, num_terms), stringsAsFactors = FALSE)
  print(data.terms)
  View(data.terms)
  
  # Creates a dataframe to store the Lesson Number and the most likely topic
  tweettopics.df <- as.data.frame(data.topics)
  tweettopics.df <- dplyr::transmute(tweettopics.df, LessonId = rownames(tweettopics.df), Topic = data.topics)
  tweettopics.df$ArticleNo <- as.character(tweettopics.df$LessonId)
  
  # Clean up and rename coluns to match previous dataframes
  tweettopics <- tweettopics.df %>% 
    dplyr::select(c("ArticleNo", "Topic")) %>% 
    plyr::rename(c("ArticleNo" = "key"))
  
  # Join original twitter data frame with tweet topics
  Tweet.Topics <- dplyr::inner_join(DataFrame, tweettopics, by = "key")
  
  return(Tweet.Topics)
}

# Min / Max Scores --------------------------------------------------------

#' @title Twitter Data Minimum Scores
#'
#' @description Determines the minimum scores for either the entire dataset or the minimum scores associated with a hashtag or topic analysis.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' @param HT_Topic_Selection THe hashtag or topic to be investigated.  NULL will find min across entire dataframe.
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' library(SAoTD)
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' score_data <- TD.Scores(DataFrameTidy = tidy_data, 
#'                         HT_Topic = "hashtag")
#' min_scores <- TD.Min.Scores(DataFrameTidyScores = score_data, 
#'                             HT_Topic = "hashtag")
#'                             
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' score_data <- TD.Scores(DataFrameTidy = tidy_data, 
#'                         HT_Topic = "hashtag")
#' min_scores <- TD.Min.Scores(DataFrameTidyScores = score_data, 
#'                             HT_Topic = "hashtag",
#'                             HT_Topic_Selection = "icecream")
#' 
#' @export

Min.Scores <- function(DataFrameTidyScores, HT_Topic, HT_Topic_Seletion = NULL) {
  if(HT_Topic == "hashtag" & is.null(HT_Topic_Seletion)) {
    TD_HT_noSel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
      head()
    return(TD_HT_noSel_Min_Scores)
  } else if(HT_Topic == "hashtag" & !is.null(HT_Topic_Seletion)) {
    TD_HT_Sel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(hashtag == HT_Topic_Seletion) %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
      head()
    return(TD_HT_Sel_Min_Scores)
  } else if(HT_Topic == "topic" & is.null(HT_Topic_Seletion)) {
    TD_Topic_noSel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
      head()
    return(TD_Topic_noSel_Min_Scores)
  } else {
    TD_Topic_Sel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(Topic == HT_Topic_Seletion) %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
      head()
    return(TD_Topic_Sel_Min_Scores)
  }
}

#' @title Twitter Data Maximum Scores
#'
#' @description Determines the Maximum scores for either the entire dataset or the Maximum scores associated with a hashtag or topic analysis.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' @param HT_Topic_Selection THe hashtag or topic to be investigated.  NULL will find min across entire dataframe.
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' library(SAoTD)
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' score_data <- TD.Scores(DataFrameTidy = tidy_data, 
#'                         HT_Topic = "hashtag")
#' min_scores <- TD.Max.Scores(DataFrameTidyScores = score_data, 
#'                             HT_Topic = "hashtag")
#'                             
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' score_data <- TD.Scores(DataFrameTidy = tidy_data, 
#'                         HT_Topic = "hashtag")
#' min_scores <- TD.Max.Scores(DataFrameTidyScores = score_data, 
#'                             HT_Topic = "hashtag",
#'                             HT_Topic_Selection = "icecream")
#' 
#' @export

Max.Scores <- function(DataFrameTidyScores, HT_Topic, HT_Topic_Seletion = NULL) {
  if(HT_Topic == "hashtag" & is.null(HT_Topic_Seletion)) {
    TD_HT_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(-TweetSentimentScore) %>% 
      head()
    return(TD_HT_noSel_Max_Scores)
  } else if(HT_Topic == "hashtag" & !is.null(HT_Topic_Seletion)) {
    TD_HT_Sel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(hashtag == HT_Topic_Seletion) %>% 
      dplyr::arrange(-TweetSentimentScore) %>% 
      head()
    return(TD_HT_Sel_Max_Scores)
  } else if(HT_Topic == "topic" & is.null(HT_Topic_Seletion)) {
    TD_Topic_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(-TweetSentimentScore) %>% 
      head()
    return(TD_Topic_noSel_Max_Scores)
  } else {
    TD_Topic_Sel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(Topic == HT_Topic_Seletion) %>% 
      dplyr::arrange(-TweetSentimentScore) %>% 
      head()
    return(TD_Topic_Sel_Max_Scores)
  }
}

# Word Grams --------------------------------------------------------------

#' @title Twitter Positive and Negative Words
#'
#' @description Determines and displays the most positive and negative words within the twitter data.
#' 
#' @param DataFrameTidy DataFrame of Twitter Data that has been tidy'd.
#' @param num_words Desired number of words to be returned.
#' @param filterword Word or words to be removed
#' 
#' @return A ggplot
#' 
#' @examples 
#' library(SAoTD)
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' posneg <- TD.PosNeg.Words(DataFrameTidy = tidy_data,
#'                           n = 10)
#'                           
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' posneg <- TD.PosNeg.Words(DataFrameTidy = tidy_data,
#'                           n = 10,
#'                           filterword = "fail")
#'                           
#' data <- twitter_data
#' tidy_data <- TD.Tidy(DataFrame = data)
#' posneg <- TD.PosNeg.Words(DataFrameTidy = tidy_data,
#'                           n = 10,
#'                           filterword = c("fail", "urgent")                           
#'                           
#' @export

# Most common positive and negative words
# If I want this to be more general, add conditional statements and have a seperate code chunk for each lexicon
PosNeg.Words <- function(DataFrameTidy, num_words, filterword = NULL) {
  TD_PosNeg_Words <- DataFrameTidy %>%  
    dplyr::inner_join(eval(as.name("Bing")), by = "Token") %>% 
    dplyr::filter(!(Token %in% filterword)) %>% 
    dplyr::count(Token, Sentiment) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(Sentiment) %>%
    dplyr::top_n(num_words, n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Token = reorder(Token, n)) %>%
    ggplot2::ggplot(aes(Token, n, fill = Sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~Sentiment, scales = "free_y") +
    labs(y = "Count",
         x = NULL) +
    ggtitle('Most common positive and negative words utilizing the Bing Lexicon') +
    coord_flip()
  return(TD_PosNeg_Words)
}

# Uni-Gram
Unigram <- function(DataFrame){
  TD_Unigram <- DataFrame %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(word, text) %>%  
    dplyr::filter(!word %in% c(stop_words$word, '[0-9]+')) %>% 
    dplyr::count(word, sort = TRUE)
}

# Bi-Gram
Bigram <- function(DataFrame){
  TD_Bigram <- DataFrame %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>% 
    dplyr::filter(!word1 %in% c(stop_words$word, '[0-9]+')) %>% 
    dplyr::filter(!word2 %in% c(stop_words$word, '[0-9]+')) %>%
    dplyr::count(word1, word2, sort = TRUE)
}

# Tri-Gram
Trigram <- function(DataFrame) {
  TD_Trigram <- DataFrame %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(trigram, text, token = "ngrams", n=3) %>%  
    tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
    dplyr::filter(!word1 %in% c(stop_words$word, '[0-9]+')) %>% 
    dplyr::filter(!word2 %in% c(stop_words$word, '[0-9]+')) %>%
    dplyr::filter(!word3 %in% c(stop_words$word, '[0-9]+')) %>%
    dplyr::count(word1, word2, word3, sort = TRUE)
}

# Bi-Gram Network
# acceptable Layouts:  'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 'randomly', 'fr', 'kk', 'drl', 'lgl'
Bigram.Network <- function(BiGramDataFrame, number = 300, layout = "fr", edge_color = "royalblue", node_color = "black", node_size = 3,  seed = 1234) {
  TD_Bigram_Network <- BiGramDataFrame %>% 
    dplyr::filter(n > number) %>% 
    igraph::graph_from_data_frame()
  
  set.seed(seed)
  
  ggraph::ggraph(TD_Bigram_Network, layout = layout) +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = edge_color, show.legend = TRUE, end_cap = circle(.07, 'inches')) +
    geom_node_point(colour = node_color, size = node_size) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
    ggtitle("Bi-Gram Network") +
    theme_void()
}

# Word Correlations -------------------------------------------------------

# Word Correlations
Word.Corr <- function(DataFrameTidy, n, sort = TRUE) {
  TD_Word_Correlation <- DataFrameTidy %>%
    dplyr::group_by(Token) %>%
    dplyr::filter(n() >= n) %>%
    widyr::pairwise_cor(Token, key, sort = sort)
}

# Word Correlations Plot
# acceptable Layouts:  'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 'randomly', 'fr', 'kk', 'drl', 'lgl'
Word.Corr.Plot <- function(WordCorr, Correlation = 0.15, layout = "fr", edge_color = "royalblue", node_color = "black", node_size = 2,  seed = 1234) {
  set.seed(seed)
  
  WordCorr %>%
    filter(correlation > Correlation) %>%
    graph_from_data_frame() %>%
    ggraph::ggraph(layout = layout) +
    geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = edge_color, show.legend = TRUE) +
    geom_node_point(colour = node_color, size = node_size) +
    geom_node_text(aes(label = name), repel = TRUE) +
    ggtitle("Word Correlation Network") +
    theme_void()
}

# Sentiment Distributions -------------------------------------------------

# TweetSentiment Corpus Distribution
Corups.Distribution <- function(DataFrameTidyScores, binwidth = 1, colour = "black", fill = "white") {
  TD_Corups_Distribution <- DataFrameTidyScores %>% 
    ggplot2::ggplot(aes(TweetSentimentScore)) +
    geom_histogram(binwidth = binwidth, colour = colour, fill = fill) +
    theme(legend.position = "none") +
    ggtitle("Sentiment Score Distribution") +
    xlab('Sentiment') +
    ylab('Count')
  return(TD_Corups_Distribution)
}

# TweetSentiScore Distribution by each Hashtag or Topic
# For HT_Topic select:  "hashtag" or "topic"
Distribution <- function(DataFrameTidyScores, HT_Topic, binwidth = 1, color = "black", fill = "white") {
  if(HT_Topic == "hashtag") {
    TD_HT_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(aes(TweetSentimentScore)) +
      geom_histogram(binwidth = binwidth, colour = color, fill = fill) +
      facet_wrap(~hashtag, ncol = 2) +
      theme(legend.position = "none") +
      ggtitle("Sentiment Score Distribution Across all #Hashtags") +
      xlab('Sentiment') +
      ylab('Count')
    return(TD_HT_Distribution)
  } else {
    TD_Topic_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(aes(TweetSentimentScore)) +
      geom_histogram(binwidth = binwidth, colour = color, fill = fill) +
      facet_wrap(~Topic, ncol = 2) +
      theme(legend.position = "none") +
      ggtitle("Sentiment Score Distribution Across all Topics") +
      xlab('Sentiment') +
      ylab('Count')
    return(TD_Topic_Distribution)
  }
}

# Visualizations ----------------------------------------------------------

# Box Plot select between hashtag or topic
# For HT_Topic select:  "hashtag" or "topic"
BoxPlot <- function(DataFrameTidyScores, HT_Topic) {
  if(HT_Topic == "hashtag") {
    TD_HT_BoxPlot <- DataFrameTidyScores %>% 
      ggplot2::ggplot(aes(hashtag, TweetSentimentScore)) +
      ggplot2::geom_boxplot() +
      theme(legend.position = "none") +
      ggtitle("Sentiment Scores Across each #Hashtag") +
      xlab('#Hashtag') +
      ylab('Sentiment') +
      coord_flip()
    return(TD_HT_BoxPlot)
  } else {
    TD_Topic_BoxPlot <- DataFrameTidyScores %>% 
      ggplot2::ggplot(aes(Topic, TweetSentimentScore)) +
      ggplot2::geom_boxplot() +
      theme(legend.position = "none") +
      ggtitle("Sentiment Scores Across each Topic") +
      xlab('Topic') +
      ylab('Sentiment') +
      coord_flip()
    return(TD_Topic_BoxPlot)
  }
}

# Violin Plot
# For HT_Topic select:  "hashtag" or "topic"
ViolinPlot <- function(DataFrameTidyScores, HT_Topic) {
  if(HT_Topic == "hashtag") {
    TD_HT_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(aes(hashtag, TweetSentimentScore)) +
      geom_violin(scale = "area") +
      stat_summary(fun.y = median, geom = "point", shape = 23, size = 2) +
      ggtitle("Sentiment Scores Across each #Hashtag") +
      xlab('#Hashtag') +
      ylab('Sentiment') +
      coord_flip()
    return(TD_HT_ViolinPlot)
  } else{
    TD_Topic_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(aes(Topic, TweetSentimentScore)) +
      geom_violin(scale = "area") +
      stat_summary(fun.y = median, geom = "point", shape = 23, size = 2) +
      ggtitle("Sentiment Scores Across each Topic") +
      xlab('Topic') +
      ylab('Sentiment') +
      coord_flip()
    return(TD_Topic_ViolinPlot)
  }
}

# Sentiment Timescale facet wrap
# For HT_Topic select:  "hashtag" or "topic"
TimeScale <- function(DataFrameTidyScores, HT_Topic) {
  if(HT_Topic == "hashtag") {
    TD_HT_TimeScale <- DataFrameTidyScores %>% 
      dplyr::group_by(hashtag, date) %>% 
      dplyr::summarise(DayScore = sum(TweetSentimentScore)) %>% 
      ggplot2::ggplot(aes(x = factor(date), y = DayScore, colour = hashtag)) + 
      geom_point() +
      geom_path(aes(group=1)) +
      geom_hline(yintercept = 0, color = "black") +
      facet_wrap(~hashtag, ncol = 2, scales = "free_y") +
      theme(legend.position = "none") +
      ggtitle("Sentiment Scores Across all #Hashtags") +
      xlab('Day') +
      ylab('Daily Sentiment Score') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(TD_HT_TimeScale)
  } else {
    TD_Topic_TimeScale <- DataFrameTidyScores %>% 
      dplyr::group_by(Topic, date) %>% 
      dplyr::summarise(DayScore = sum(TweetSentimentScore)) %>% 
      ggplot2::ggplot(aes(x = factor(date), y = DayScore, colour = Topic)) + 
      geom_point() +
      geom_path(aes(group=1)) +
      geom_hline(yintercept = 0, color = "black") +
      facet_wrap(~Topic, ncol = 2, scales = "free_y") +
      theme(legend.position = "none") +
      ggtitle("Sentiment Scores Across all Topics") +
      xlab('Day') +
      ylab('Daily Sentiment Score') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(TD_Topic_TimeScale)
  }
}

# World Map of Tweets by hashtag
# For HT_Topic select:  "hashtag" or "topic"
WorldMap <- function(DataFrame, HT_Topic) {
  if(HT_Topic == "hashtag") {
    TD_HT_WorldMap <- ggplot2::map_data("world") %>% 
      ggplot2::ggplot() + 
      geom_polygon(aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
      geom_jitter(data = DataFrame,
                  aes(x = as.numeric(longitude),
                      y = as.numeric(latitude),
                      colour = hashtag)) + 
      ggtitle("World Map of Tweets") +
      theme(legend.position = "bottom") +
      scale_fill_continuous(guide = guide_legend(title = NULL)) +
      coord_quickmap()
    return(TD_HT_WorldMap)
  } else {
    TD_Topic_WorldMap <- ggplot2::map_data("world") %>% 
      ggplot2::ggplot() + 
      geom_polygon(aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
      geom_jitter(data = DataFrame,
                  aes(x = as.numeric(longitude),
                      y = as.numeric(latitude),
                      colour = Topic)) + 
      ggtitle("World Map of Tweets") +
      theme(legend.position = "bottom") +
      scale_fill_continuous(guide = guide_legend(title = NULL)) +
      coord_quickmap()
    return(TD_Topic_WorldMap)
  }
}

