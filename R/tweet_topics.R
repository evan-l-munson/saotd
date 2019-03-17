
#' @title Tweet Topics
#'
#' @description Determines the Latent topics within a dataframe by using Latent Dirichlet Allocation (LDA) model parameters.  
#' Uses the `ldatuning` package and outputs an ldatuning plot.  
#' Prepares Tweet text, creates DTM, conducts LDA, display data terms associated with each topic.
#'
#' @param DataFrame DataFrame of Twitter Data.
#' @param clusters The number of latent clusters.
#' @param method method = "Gibbs"
#' @param set_seed Seed for reproducable results.
#' @param num_terms The desired number of terms to be returned for each topic.
#' 
#' @importFrom dplyr mutate group_by anti_join inner_join count select transmute quo
#' @importFrom stringr str_replace_all
#' @importFrom plyr rename
#' @importFrom tidytext cast_dtm 
#' @importFrom topicmodels LDA topics terms
#' 
#' @return Returns LDA topics.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' LDA_data <- tweet_topics(DataFrame = data,
#'                          clusters = 8,
#'                          method = "Gibbs",
#'                          set_seed = 1234,
#'                          num_terms = 10)
#'
#' LDA_data
#' }
#' @export

tweet_topics <- function(DataFrame, clusters, method = "Gibbs", set_seed = 1234, num_terms = 10) {
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  if(!is.numeric(clusters)) {
    stop('The input must be a numerical value.')
  }
  
  text <- dplyr::quo(text)
  key <- dplyr::quo(key)
  word <- dplyr::quo(word)
  n <- dplyr::quo(n)
  
  lda_prep <- DataFrame %>% 
    dplyr::mutate(text = base::iconv(DataFrame$text, "latin1", "ASCII", sub="")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% # Remove hashtag
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% # Remove punctuation
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%  # Remove links
    dplyr::group_by(key) %>%
    tidytext::unnest_tokens(word, text) %>% 
    dplyr::anti_join(tidytext::stop_words) %>% 
    dplyr::count(key, word, sort = TRUE) %>% 
    tidytext::cast_dtm(key, word, n)
  
  # Run LDA using Gibbs sampling
  ldaout <- topicmodels::LDA(lda_prep, k = clusters, method = method, control = list(seed = set_seed))
  
  ldaout_topics <- as.matrix(topicmodels::topics(ldaout))
  
  ldaout_terms <- as.matrix(topicmodels::terms(ldaout, num_terms))
  
  # probabilities associated with each topic assignment
  topicProbabilities <- as.data.frame(ldaout@gamma)
  data.topics <- topicmodels::topics(ldaout, 1)
  data.terms <- as.data.frame(topicmodels::terms(ldaout, num_terms), stringsAsFactors = FALSE)
  print(data.terms)
  #View(data.terms)
  
  # Creates a dataframe to store the Lesson Number and the most likely topic
  tweettopics.df <- as.data.frame(data.topics)
  tweettopics.df <- dplyr::transmute(tweettopics.df, LessonId = rownames(tweettopics.df), Topic = data.topics)
  tweettopics.df$ArticleNo <- as.character(tweettopics.df$LessonId)
  
  # Clean up and rename coluns to match previous dataframes
  tweettopics <- tweettopics.df %>% 
    dplyr::select(c("ArticleNo", "Topic")) %>% 
    plyr::rename(c("ArticleNo" = "key"))
  
  # Join original Twitter data frame with Tweet topics
  tweet_topics <- dplyr::inner_join(DataFrame, tweettopics, by = "key")
  
  invisible()
  return(tweet_topics)
}
