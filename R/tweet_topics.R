
#' @title Tweet Topics
#'
#' @description Determines the Latent topics within a data frame by using Latent 
#'   Dirichlet Allocation (LDA) model parameters.  Uses the `ldatuning` package 
#'   and outputs an ldatuning plot.  Prepares Tweet text, creates DTM, conducts 
#'   LDA, display data terms associated with each topic.
#'
#' @param DataFrame Data Frame of Twitter Data.
#' @param clusters The number of latent clusters.
#' @param method method = "Gibbs"
#' @param set_seed Seed for reproducible results.
#' @param num_terms The desired number of terms to be returned for each topic.
#' 
#' @importFrom dplyr mutate group_by anti_join inner_join count select transmute quo rename
#' @importFrom stringr str_replace_all
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

tweet_topics <- function(DataFrame, 
                         clusters, 
                         method = "Gibbs", 
                         num_terms = 10,
                         set_seed = 1234) {
  
  # input checks
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  if(!is.numeric(clusters)) {
    stop('The input must be a numerical value.')
  }
  
  # configure defusing operators for packages checking
  text <- dplyr::quo(text)
  key <- dplyr::quo(key)
  word <- dplyr::quo(word)
  n <- dplyr::quo(n)
  
  # web url
  wu <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
  
  # function main body
  lda_prep <- DataFrame %>% 
    dplyr::mutate(
      text = base::iconv(
        x = DataFrame$text, 
        from = "latin1", 
        to = "ASCII", 
        sub=""), 
      text = stringr::str_replace_all(
        string = text, 
        pattern = "#", 
        replacement = ""), # Remove hashtag
      text = stringr::str_replace_all(
        string = text, 
        pattern = "[:punct:]", 
        replacement = ""), # Remove punctuation
      text = stringr::str_replace_all(
        string = text, 
        pattern = "RT", 
        replacement = ""), # Remove retweet note
      text = stringr::str_replace_all(
        string = text, 
        pattern = "&amp", 
        replacement = ""), # Remove Accelerated Mobile Pages (AMP) note
      text = stringr::str_replace_all(
        string = text, 
        pattern = wu, 
        replacement = "")) %>%  # Remove links
    dplyr::group_by(key) %>%
    tidytext::unnest_tokens(word, text) %>% 
    dplyr::anti_join(tidytext::stop_words) %>% 
    dplyr::count(key, word, sort = TRUE) %>% 
    tidytext::cast_dtm(key, word, n)
  
  # Run LDA using Gibbs sampling
  ldaout <- topicmodels::LDA(
    lda_prep, 
    k = clusters, 
    method = method, 
    control = list(seed = set_seed))
  
  ldaout_topics <- as.matrix(topicmodels::topics(ldaout))
  
  ldaout_terms <- as.matrix(topicmodels::terms(ldaout, num_terms))
  
  # probabilities associated with each topic assignment
  topicProbabilities <- as.data.frame(ldaout@gamma)
  data.topics <- topicmodels::topics(ldaout, 1)
  data.terms <- as.data.frame(
    topicmodels::terms(ldaout, 
                       num_terms), 
    stringsAsFactors = FALSE)
  print(data.terms)
  
  #View(data.terms)
  # Creates a data frame to store the Lesson Number and the most likely topic
  tweettopics.df <- as.data.frame(data.topics)
  tweettopics.df <- dplyr::transmute(
    tweettopics.df, 
    LessonId = rownames(tweettopics.df), 
    Topic = data.topics)
  tweettopics.df$ArticleNo <- as.character(tweettopics.df$LessonId)
  
  # Clean up and rename columns to match previous data frames
  tweettopics <- tweettopics.df %>% 
    dplyr::select(c("ArticleNo", "Topic")) %>% 
    dplyr::rename(c("key" ="ArticleNo"))
  
  # Join original Twitter data frame with Tweet topics
  tweet_topics <- dplyr::inner_join(
    x = DataFrame, 
    y = tweettopics, 
    by = "key")
  
  invisible()
  
  return(tweet_topics)
  
}
