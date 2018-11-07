


# tweet_acquire -----------------------------------------------------------------

#' @title Acquire Twitter Tweets   
#'
#' @description Function will enable a user to access the Twitter API throught the 
#' [Twitter Developers Account](https://dev.twitter.com/) site.
#' Once a user has a Twitter developers account and has recieved their individual consumer key, 
#' consumer secret key, access token, and access secret they can 
#' acquire Tweets based on a list of hashtags and a requested number of entires per hashtag.
#' 
#' @param consumer_key Twitter Application management consumer key.
#' @param consumer_secret Twitter Application management consumer secret key.
#' @param access_token Twitter Application management access token.
#' @param access_secret Twitter Application management access secret key.
#' @param HT A single hashtag or a list of hashtags the user has specified.
#' @param num_tweets Number of Tweets to be acquired per each hashtag.
#' @param file_name User desired output .RData file name.
#' @param distinct Logical.  If distinct = TRUE, the function removes multiple Tweets that originate from the same Twitter id at the exact same time.
#' @importFrom twitteR setup_twitter_oauth twListToDF searchTwitter
#' @importFrom dplyr mutate distinct quo
#' @importFrom purrr map_df
#' 
#' @return A DataFrame.
#' 
#' @examples 
#' \dontrun{
#' consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
#' consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_token <- "XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' 
#' hashtags <- c("#job", "#Friday", "#fail", "#icecream", "#random", "#kitten", "#airline")
#' 
#' tweet_acquire(consumer_key = consumer_key, 
#'         consumer_secret = consumer_secret, 
#'         access_token = access_token, 
#'         access_secret = access_secret, 
#'         HT = hashtags, 
#'         num_tweets = 10, 
#'         file_name = "test_tweets.RData",
#'         distinct = TRUE)
#'         
#' load("test_tweets.RData")
#' }
#' @export 

tweet_acquire <- function(consumer_key, consumer_secret, access_token, access_secret, HT, num_tweets, file_name, distinct = TRUE) {
  
  options(httr_oauth_cache = TRUE)
  
  screenName <- dplyr::quo(screenName)
  created <- dplyr::quo(created)
  key <- dplyr::quo(key)
  
  twitteR::setup_twitter_oauth(consumer_key,
                               consumer_secret,
                               access_token,
                               access_secret)
  
  twitter_data <- list()
  for (i in HT) {
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

# Explore -----------------------------------------------------------------

#' @title Tidy Twitter Data
#'
#' @description Function to Tidy Twitter Data and remove all emoticons, punctuation, weblinks while maintaiing actual Tweet.
#'
#' @param DataFrame DataFrame of Twitter Data.
#' 
#' @importFrom dplyr mutate filter quo
#' @importFrom stringr str_replace_all
#' @importFrom tidytext unnest_tokens
#' @importFrom plyr rename
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- tweet_tidy(DataFrame = data)
#' tidy_data
#' }
#' @export

tweet_tidy <- function(DataFrame) {
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  text <- dplyr::quo(text)
  cleantext <- dplyr::quo(cleantext)
  word <- dplyr::quo(word)

  reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
  
  TD_Tidy <- DataFrame %>%
    dplyr::mutate(cleantext = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "#", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "http", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "[:punct:]", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(output = word, input = cleantext, token = "words", drop = TRUE) %>% 
    dplyr::filter(!word %in% tidytext::stop_words$word) %>% 
    plyr::rename(c("word" = "Token"))
  return(TD_Tidy)
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
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' data <- merge_terms(DataFrame = data, 
#'                     term = "ice cream", 
#'                     term_replacement = "ice_cream")
#' data 
#' }
#' @export

merge_terms <- function(DataFrame, term, term_replacement){
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  for(i in 1: length(DataFrame$text)){
    DataFrame[i, "text"] <- DataFrame[i, "text"] %>% 
      gsub(pattern = as.character(term),
           replacement = as.character(term_replacement),
           ignore.case = TRUE)   
  }
  DataFrame <- DataFrame
}

#' @title Twitter Uni-Grams
#'
#' @description Determines and displays the text Uni-Grams within the Twitter data in sequence from the most used to the least used.  A Uni-Gram is a single word.
#' 
#' @param DataFrame DataFrame of Twitter Data.
#' 
#' @importFrom dplyr count mutate filter quo
#' @importFrom stringr str_replace_all
#' @importFrom tidytext unnest_tokens 
#' 
#' @return A tribble.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' TD_Unigram <- unigram(DataFrame = data)
#' TD_Unigram
#' }             
#' @export

unigram <- function(DataFrame){
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  text <- dplyr::quo(text)
  word <- dplyr::quo(word)
  
  TD_Unigram <- DataFrame %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(word, text) %>%  
    dplyr::filter(!word %in% c(tidytext::stop_words$word, '[0-9]+')) %>% 
    dplyr::count(word, sort = TRUE)
  return(TD_Unigram)
}

#' @title Twitter Bi-Grams
#'
#' @description Determines and displays the text Bi-Grams within the Twitter data in sequence from the most used to the least used.  A Bi-Gram is a combination of two consecutive words.
#' 
#' @param DataFrame DataFrame of Twitter Data.
#' 
#' @importFrom dplyr count mutate filter quo
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#' @importFrom tidytext unnest_tokens 
#' 
#' @return A tribble.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' TD_Bigram <- bigram(DataFrame = data)
#' TD_Bigram
#' }                
#' @export

bigram <- function(DataFrame){
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  text <- dplyr::quo(text)
  word <- dplyr::quo(word)
  bigram <- dplyr::quo(bigram)
  word1 <- dplyr::quo(word1)
  word2 <- dplyr::quo(word2)
  
  TD_Bigram <- DataFrame %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%  
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>% 
    dplyr::filter(!word1 %in% c(tidytext::stop_words$word, '[0-9]+')) %>% 
    dplyr::filter(!word2 %in% c(tidytext::stop_words$word, '[0-9]+')) %>%
    dplyr::count(word1, word2, sort = TRUE)
  return(TD_Bigram)
}

#' @title Twitter Tri-Grams
#'
#' @description Determines and displays the text Tri-Grams within the Twitter data in sequence from the most used to the least used.  A Tri-Gram is a combination of three consecutive words.
#' 
#' @param DataFrame DataFrame of Twitter Data.
#' 
#' @importFrom dplyr count mutate filter quo
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#' @importFrom tidytext unnest_tokens 
#' 
#' @return A tribble.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' TD_Trigram <- trigram(DataFrame = data)
#' TD_Trigram
#' }
#' @export

trigram <- function(DataFrame) {
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  text <- dplyr::quo(text)
  word <- dplyr::quo(word)
  trigram <- dplyr::quo(trigram)
  word1 <- dplyr::quo(word1)
  word2 <- dplyr::quo(word2)
  word3 <- dplyr::quo(word3)
  
  TD_Trigram <- DataFrame %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(trigram, text, token = "ngrams", n=3) %>%  
    tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
    dplyr::filter(!word1 %in% c(tidytext::stop_words$word, '[0-9]+')) %>% 
    dplyr::filter(!word2 %in% c(tidytext::stop_words$word, '[0-9]+')) %>%
    dplyr::filter(!word3 %in% c(tidytext::stop_words$word, '[0-9]+')) %>%
    dplyr::count(word1, word2, word3, sort = TRUE)
  return(TD_Trigram)
}

#' @title Twitter Bi-Gram Network
#'
#' @description Displays the Bi-Gram Network.  Bi-Gram networks builds on computed Bi-Grams.  Bi-Gram networks serve as a visualization tool that displays the relationships between the words simultaneously as opposed to a tabular display of Bi-Gram words.
#' 
#' @param BiGramDataFrame DataFrame of Bi-Grams.
#' @param number The minimum desired number of Bi-Gram occurances to be displayed (number = 300, would display all Bi-Grams that have at least 300 instances.)
#' @param layout Desired layout from the `ggraph` package.  Acceptable layouts:  "star", "circle", "gem", "dh", "graphopt", "grid", "mds", "randomly", "fr", "kk", "drl", "lgl"
#' @param edge_color User desired edge color.
#' @param node_color User desired node color.
#' @param node_size User desired node size.
#' @param set_seed Seed for reproducable results.
#' 
#' @importFrom dplyr filter quo
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom scales rescale
#' @import ggplot2
#'   
#' @return A ggraph plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' TD_Bigram <- bigram(DataFrame = data)
#' TD_Bigram_Network <- bigram_network(BiGramDataFrame = TD_Bigram,
#'                                     number = 300,
#'                                     layout = "fr",
#'                                     edge_color = "royalblue",
#'                                     node_color = "black",
#'                                     node_size = 3,
#'                                     set_seed = 1234)
#'
#' TD_Bigram_Network
#' }
#' @export

bigram_network <- function(BiGramDataFrame, number = 300, layout = "fr", edge_color = "royalblue", node_color = "black", node_size = 3,  set_seed = 1234) {
  
  if(!is.data.frame(BiGramDataFrame)) {
    stop('The input for this function is a Bigram data frame.')
  }
  if(!(("word1" %in% colnames(BiGramDataFrame)) & ("word2" %in% colnames(BiGramDataFrame)) & ("n" %in% colnames(BiGramDataFrame)))) {
    stop('The data frame is not properly constructed.  The data frame must have three columns: word1, word2 and n.')
  }
  if(number < 1) {
    stop('You must choose number of Bi-Grams greater than 1.')
  }
  
  n <- dplyr::quo(n)
  name <- dplyr::quo(name)
  
  TD_Bigram_Network <- BiGramDataFrame %>% 
    dplyr::filter(n > number) %>% 
    igraph::graph_from_data_frame()
  
  set.seed(set_seed)
  
  TD_Bigram_Network %>% 
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_link(ggplot2::aes(edge_alpha = 1, edge_width = scales::rescale(n, to=c(1,10))), edge_colour = edge_color, show.legend = TRUE) +
    ggraph::geom_node_point(colour = node_color, size = node_size) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    ggplot2::ggtitle("Bi-Gram Network") +
    ggplot2::theme_void()
}

#' @title Twitter Word Correlations
#'
#' @description The word correlation displays the mutual relationship between words.
#' 
#' @param DataFrameTidy DataFrame of Twitter Data that has been tidy'd.
#' @param number The number of word instances to be included.
#' @param sort Rank order the results from most to least correlated.
#' 
#' @importFrom dplyr group_by filter quo
#' @importFrom widyr pairwise_cor
#' 
#' @return A tribble
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' TD_Word_Corr <- word_corr(DataFrameTidy = tidy_data, 
#'                           number = 500,
#'                           sort = TRUE)
#'
#' TD_Word_Corr
#' }                    
#' @export

word_corr <- function(DataFrameTidy, number, sort = TRUE) {
  
  if(!is.data.frame(DataFrameTidy)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("Token" %in% colnames(DataFrameTidy)) & ("key" %in% colnames(DataFrameTidy)))) {
    stop('The data frame is not properly constructed.  The data frame must contain at minimum the columns: Token and key.')
  }
  if(number <= 1) {
    stop('Must choose number of Correlation pairs greater than 1.')
  }
  
  Token <- dplyr::quo(Token)
  n <- dplyr::quo(n)
  key <- dplyr::quo(key)
  
  TD_Word_Correlation <- DataFrameTidy %>%
    dplyr::group_by(Token) %>%
    dplyr::filter(n() >= number) %>%
    widyr::pairwise_cor(Token, key, sort = sort)
  return(TD_Word_Correlation)
}

#' @title Twitter Word Correlations Plot
#'
#' @description The word correlation network displays the mutual relationship between words.  The correlation network shows higher correlations with a thicker and darker edge color.
#' 
#' @param WordCorr DataFrame of Word Correlations.
#' @param Correlation Minimum level of correlation to be displayed.
#' @param layout Desired layout from the `ggraph` package.  Acceptable layouts:  "star", "circle", "gem", "dh", "graphopt", "grid", "mds", "randomly", "fr", "kk", "drl", "lgl"
#' @param edge_color User desired edge color.
#' @param node_color User desired node color.
#' @param node_size User desired node size.
#' @param set_seed Seed for reproducable results.
#' 
#' @importFrom dplyr filter quo
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @import ggplot2
#' 
#' @return An igraph plot
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' TD_Word_Corr <- word_corr(DataFrameTidy = tidy_data, 
#'                           number = 500,
#'                           sort = TRUE)
#' TD_Word_Corr_Network <- word_corr_network(WordCorr = TD_Word_Corr,
#'                                        Correlation = 0.15,
#'                                        layout = "fr",
#'                                        edge_color = "royalblue",
#'                                        node_color = "black",
#'                                        node_size = 2,
#'                                        set_seed = 1234)
#'
#' TD_Word_Corr_Network
#' }
#' @export

word_corr_network <- function(WordCorr, Correlation = 0.15, layout = "fr", edge_color = "royalblue", node_color = "black", node_size = 2,  set_seed = 1234) {
  
  if(!is.data.frame(WordCorr)) {
    stop('The input for this function is a Correlation data frame.')
  }
  if(Correlation <= 0) {
    stop('A correlation value between 0 and 1 must be selected.')
  }
  if(Correlation > 1) {
    stop('A correlation value between 0 and 1 must be selected.')
  }
  
  correlation <- dplyr::quo(correlation)
  name <- dplyr::quo(name)
  
  set.seed(set_seed)
  
  WordCorr %>%
    dplyr::filter(correlation > Correlation) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_link(ggplot2::aes(edge_alpha = correlation, edge_width = correlation), edge_colour = edge_color, show.legend = TRUE) +
    ggraph::geom_node_point(colour = node_color, size = node_size) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    ggplot2::ggtitle("Word Correlation Network") +
    ggplot2::theme_void()
}

# Topic Analysis ----------------------------------------------------------


#' @title Number Topics
#'
#' @description Determines the optimal number of Latent topics within a dataframe by tuning the Latent Dirichlet Allocation (LDA) model parameters.  
#' Uses the `ldatuning` package and outputs an ldatuning plot.  __This process can be time consuming depending on the size of the input dataframe.__
#'
#' @param DataFrame DataFrame of Twitter Data.
#' @param num_cores The number of CPU cores to processes models simultaneously (2L for dual core processor).
#' @param min_clusters Lower range for the number of clusters.
#' @param max_clusters Upper range for the number of clusters.
#' @param skip Integer; The number of clusters to skip between entries.
#' @param set_seed Seed for reproducable results.
#' 
#' @importFrom dplyr mutate group_by count anti_join quo
#' @importFrom stringr str_replace_all
#' @importFrom tidytext unnest_tokens cast_dtm 
#' @importFrom ldatuning FindTopicsNumber
#' @importFrom scales rescale
#' @importFrom reshape2 melt
#' @import ggplot2
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' LDA_Topic_Plot <- number_topics(DataFrame = data,
#'                                 num_cores = 2L,
#'                                 min_clusters = 2,
#'                                 max_clusters = 12, 
#'                                 skip = 2,
#'                                 set_seed = 1234)
#'
#' LDA_Topic_Plot 
#' }
#' @export

number_topics <- function(DataFrame, num_cores, min_clusters = 2, max_clusters = 12, skip = 2, set_seed = 1234) {
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  set_seed
  
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
  p <- ggplot2::ggplot(values, aes_string(x = "topics", y = "value", group = "variable"))
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
  return(p)
}

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
#' \dontrun{
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
  
  return(tweet_topics)
}

# Sentiment Calculation ---------------------------------------------------

#' @title Score Tidy Twitter Data
#'
#' @description Function to Calculate Sentiment Scores that will account for sentiment by hashtag or topic.
#'
#' @param DataFrameTidy DataFrame of Twitter Data that has been tidy'd.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic"
#' 
#' @importFrom dplyr mutate inner_join group_by count quo
#' @importFrom plyr rename
#' @importFrom tidyr spread
#' @importFrom lubridate as_date
#' @importFrom tidytext get_sentiments
#' 
#' @return A Scored DataFrame.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag")
#' score_data
#' }
#' @export

tweet_scores <- function(DataFrameTidy, HT_Topic) {

  if(!is.data.frame(DataFrameTidy)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }

  text <- dplyr::quo(text)
  method <- dplyr::quo(method)
  hashtag <- dplyr::quo(hashtag)
  created <- dplyr::quo(created)
  key <- dplyr::quo(key)
  Sentiment <- dplyr::quo(Sentiment)
  n <- dplyr::quo(n)
  positive <- dplyr::quo(positive)
  negative <- dplyr::quo(negative)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  Topic <- dplyr::quo(Topic)
  
  
  Bing <- tidytext::get_sentiments(lexicon = "bing") %>% 
    plyr::rename(c("word" = "Token", "sentiment" = "Sentiment"))
  
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

#' @title Twitter Positive and Negative Words
#'
#' @description Determines and displays the most positive and negative words within the twitter data.
#' 
#' @param DataFrameTidy DataFrame of Twitter Data that has been tidy'd.
#' @param num_words Desired number of words to be returned.
#' @param filterword Word or words to be removed
#' 
#' @importFrom dplyr mutate inner_join group_by count filter ungroup top_n quo
#' @importFrom plyr rename
#' @importFrom tidytext get_sentiments
#' @importFrom stats reorder
#' @import ggplot2
#' 
#' @return A ggplot
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' posneg <- posneg_words(DataFrameTidy = tidy_data,
#'                        n = 10)
#' posneg
#'                           
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' posneg <- posneg_words(DataFrameTidy = tidy_data,
#'                        n = 10,
#'                        filterword = "fail")
#' posneg
#'                           
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' posneg <- posneg_words(DataFrameTidy = tidy_data,
#'                        n = 10,
#'                        filterword = c("fail", "urgent"))            
#' posneg
#' }
#' @export

posneg_words <- function(DataFrameTidy, num_words, filterword = NULL) {
  
  if(!is.data.frame(DataFrameTidy)) {
    stop('The input for this function is a data frame.')
  }
  if(!is.numeric(num_words)) {
    stop('Enter a number.')
  }
  
  Token <- dplyr::quo(Token)
  Sentiment <- dplyr::quo(Sentiment)
  n <- dplyr::quo(n)
  
  Bing <- tidytext::get_sentiments(lexicon = "bing") %>% 
    plyr::rename(c("word" = "Token", "sentiment" = "Sentiment"))
  
  TD_PosNeg_Words <- DataFrameTidy %>%  
    dplyr::inner_join(Bing, by = "Token") %>% 
    dplyr::filter(!(Token %in% filterword)) %>% 
    dplyr::count(Token, Sentiment) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(Sentiment) %>%
    dplyr::top_n(num_words, n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Token = stats::reorder(Token, n)) %>%
    ggplot2::ggplot(ggplot2::aes(Token, n, fill = Sentiment)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~Sentiment, scales = "free_y") +
    ggplot2::labs(y = "Count",
         x = NULL) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle('Most common positive and negative words utilizing the Bing Lexicon') +
    ggplot2::coord_flip()
  return(TD_PosNeg_Words)
}

#' @title Twitter Data Minimum Scores
#'
#' @description Determines the minimum scores for either the entire dataset or the minimum scores associated with a hashtag or topic analysis.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' @param HT_Topic_Selection THe hashtag or topic to be investigated.  NULL will find min across entire dataframe.
#' 
#' @importFrom dplyr arrange filter quo
#' @importFrom utils head
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag")
#' min_scores <- tweet_min_scores(DataFrameTidyScores = score_data, 
#'                                HT_Topic = "hashtag")
#'                             
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag")
#' min_scores <- tweet_min_scores(DataFrameTidyScores = score_data, 
#'                                HT_Topic = "hashtag",
#'                                HT_Topic_Selection = "icecream")
#' }
#' @export

tweet_min_scores <- function(DataFrameTidyScores, HT_Topic, HT_Topic_Selection = NULL) {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag" & is.null(HT_Topic_Selection)) {
    TD_HT_noSel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange((TweetSentimentScore)) %>% 
      utils::head()
    return(TD_HT_noSel_Min_Scores)
  } else if(HT_Topic == "hashtag" & !is.null(HT_Topic_Selection)) {
    TD_HT_Sel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(hashtag == HT_Topic_Selection) %>% 
      dplyr::arrange((TweetSentimentScore)) %>% 
      utils::head()
    return(TD_HT_Sel_Min_Scores)
  } else if(HT_Topic == "topic" & is.null(HT_Topic_Selection)) {
    TD_Topic_noSel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange((TweetSentimentScore)) %>% 
      utils::head()
    return(TD_Topic_noSel_Min_Scores)
  } else {
    TD_Topic_Sel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(Topic == HT_Topic_Selection) %>% 
      dplyr::arrange((TweetSentimentScore)) %>% 
      utils::head()
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
#' @importFrom dplyr arrange filter quo
#' @importFrom plyr desc
#' @importFrom utils head
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag")
#' min_scores <- tweet_max_scores(DataFrameTidyScores = score_data, 
#'                                HT_Topic = "hashtag")
#'                             
#' data <- twitter_data
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag")
#' min_scores <- tweet_max_scores(DataFrameTidyScores = score_data, 
#'                                HT_Topic = "hashtag",
#'                                HT_Topic_Selection = "icecream")
#' }
#' @export

tweet_max_scores <- function(DataFrameTidyScores, HT_Topic, HT_Topic_Selection = NULL) {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag" & is.null(HT_Topic_Selection)) {
    TD_HT_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(plyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    return(TD_HT_noSel_Max_Scores)
  } else if(HT_Topic == "hashtag" & !is.null(HT_Topic_Selection)) {
    TD_HT_Sel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(hashtag == HT_Topic_Selection) %>% 
      dplyr::arrange(plyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    return(TD_HT_Sel_Max_Scores)
  } else if(HT_Topic == "topic" & is.null(HT_Topic_Selection)) {
    TD_Topic_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(plyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    return(TD_Topic_noSel_Max_Scores)
  } else {
    TD_Topic_Sel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(Topic == HT_Topic_Selection) %>% 
      dplyr::arrange(plyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    return(TD_Topic_Sel_Max_Scores)
  }
}

# Visualization -----------------------------------------------------------

#' @title Twitter Corpus Distribution
#'
#' @description Determines the scores distribution for the entire Twitter data corpus.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param binwidth The width of the bins.  Default is 1.
#' @param color The user selected color to highlight the bins.
#' @param fill The interior color of the bins.
#' 
#' @importFrom dplyr quo
#' @import ggplot2
#' 
#' @return A ggplot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' Corp_Dist <- tweet_corpus_distribution(DataFrameTidyScores = score_data,
#'                                        binwidth = 1,
#'                                        color = "black", 
#'                                        fill = "white")
#' Corp_Dist
#' }
#' @export

tweet_corpus_distribution <- function(DataFrameTidyScores, binwidth = 1, color = "black", fill = "white") {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  TD_Corpus_Distribution <- DataFrameTidyScores %>% 
    ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
    ggplot2::geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Sentiment Score Distribution") +
    ggplot2::xlab('Sentiment') +
    ggplot2::ylab('Count') +
    ggplot2::theme_bw()
  return(TD_Corpus_Distribution)
}

#' @title Twitter Hashtag or Topic Distribution
#'
#' @description Determines the scores distribution by hashtag or topic for Twitter data.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' @param binwidth The width of the bins.  Default is 1.
#' @param color The user selected color to highlight the bins.
#' @param fill The interior color of the bins.
#' 
#' @import ggplot2
#' 
#' @return A facet wrap ggplot.
#' 
#' @examples
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' Dist <- tweet_distribution(DataFrameTidyScores = score_data,
#'                      HT_Topic = "hashtag",
#'                      binwidth = 1,
#'                      color = "black", 
#'                      fill = "white")
#' Dist
#' }
#' @export

tweet_distribution <- function(DataFrameTidyScores, HT_Topic, binwidth = 1, color = "black", fill = "white") {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag") {
    TD_HT_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
      ggplot2::geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
      ggplot2::facet_wrap(~hashtag, ncol = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Score Distribution Across all #Hashtags") +
      ggplot2:: xlab('Sentiment') +
      ggplot2::ylab('Count') +
      ggplot2::theme_bw()
    return(TD_HT_Distribution)
  } else {
    TD_Topic_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
      ggplot2::geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
      ggplot2::facet_wrap(~Topic, ncol = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Score Distribution Across all Topics") +
      ggplot2::xlab('Sentiment') +
      ggplot2:: ylab('Count') +
      ggplot2::theme_bw()
    return(TD_Topic_Distribution)
  }
}

#' @title Twitter Data Box Plot
#'
#' @description Displays the distribution scores of either hashtag or topic Twitter data.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @importFrom dplyr quo
#' @import ggplot2
#' 
#' @return A ggplot box plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_box <- tweet_box(DataFrameTidyScores = score_data,
#'                     HT_Topic = "hashtag")
#' ht_box
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_box <- tweet_box(DataFrameTidyScores = score_data,
#'                        HT_Topic = "topic") 
#' topic_box                    
#' }
#' @export

tweet_box <- function(DataFrameTidyScores, HT_Topic) {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag") {
    TD_HT_BoxPlot <- DataFrameTidyScores %>% 
      ggplot2::ggplot(ggplot2::aes(hashtag, TweetSentimentScore)) +
      ggplot2::geom_boxplot() +
      theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across each #Hashtag") +
      ggplot2::xlab('#Hashtag') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    return(TD_HT_BoxPlot)
  } else {
    TD_Topic_BoxPlot <- DataFrameTidyScores %>% 
      ggplot2::ggplot(ggplot2::aes(Topic, TweetSentimentScore)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across each Topic") +
      ggplot2::xlab('Topic') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    return(TD_Topic_BoxPlot)
  }
}

#' @title Twitter Data Violin Plot
#'
#' @description Displays the distribution scores of either hashtag or topic Twitter data.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @import ggplot2
#' @importFrom dplyr quo
#' @importFrom stats median
#' 
#' @return A ggplot violin plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_violin <- tweet_violin(DataFrameTidyScores = score_data,
#'                           HT_Topic = "hashtag")
#' ht_violin
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_violin <- tweet_violin(DataFrameTidyScores = score_data,
#'                              HT_Topic = "topic") 
#' topic_violin                    
#' }
#' @export

tweet_violin <- function(DataFrameTidyScores, HT_Topic) {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  hashtag <- dplyr::quo(hashtag)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  Topic <- dplyr::quo(Topic)
  
  if(HT_Topic == "hashtag") {
    TD_HT_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(ggplot2::aes(hashtag, TweetSentimentScore)) +
      ggplot2::geom_violin(scale = "area") +
      ggplot2::stat_summary(fun.y = stats::median, geom = "point", shape = 23, size = 2) +
      ggplot2::ggtitle("Sentiment Scores Across each #Hashtag") +
      ggplot2::xlab('#Hashtag') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    return(TD_HT_ViolinPlot)
  } else{
    TD_Topic_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(ggplot2::aes(Topic, TweetSentimentScore)) +
      ggplot2::geom_violin(scale = "area") +
      ggplot2::stat_summary(fun.y = stats::median, geom = "point", shape = 23, size = 2) +
      ggplot2::ggtitle("Sentiment Scores Across each Topic") +
      ggplot2::xlab('Topic') +
      ggplot2::ylab('Sentiment') +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    return(TD_Topic_ViolinPlot)
  }
}

#' @title Twitter Data Timeseries Plot.
#'
#' @description Displays the Twitter data sentiment scores through time.  The sentiment scores by hashtag or topic are summed per day and plotted to show the change in sentiment through time.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @importFrom dplyr summarize group_by quo
#' @import ggplot2
#' 
#' @return A ggplot plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_time <- tweet_time(DataFrameTidyScores = score_data,
#'                       HT_Topic = "hashtag")
#' ht_time
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_time <- tweet_time(DataFrameTidyScores = score_data,
#'                          HT_Topic = "topic") 
#' topic_time                    
#' }
#' @export

tweet_time <- function(DataFrameTidyScores, HT_Topic) {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  hashtag <- dplyr::quo(hashtag)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  DayScore <- dplyr::quo(DayScore)
  Topic <- dplyr::quo(Topic)
  
  if(HT_Topic == "hashtag") {
    TD_HT_TimeScale <- DataFrameTidyScores %>% 
      dplyr::group_by(hashtag, date) %>% 
      dplyr::summarise(DayScore = sum(TweetSentimentScore)) %>% 
      ggplot2::ggplot(ggplot2::aes(x = factor(date), y = DayScore, colour = hashtag)) + 
      ggplot2::geom_point() +
      ggplot2::geom_path(ggplot2::aes(group=1)) +
      ggplot2::geom_hline(yintercept = 0, color = "black") +
      ggplot2::facet_wrap(~hashtag, ncol = 2, scales = "free_y") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across all #Hashtags") +
      ggplot2::xlab('Day') +
      ggplot2::ylab('Daily Sentiment Score') +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(TD_HT_TimeScale)
  } else {
    TD_Topic_TimeScale <- DataFrameTidyScores %>% 
      dplyr::group_by(Topic, date) %>% 
      dplyr::summarise(DayScore = sum(TweetSentimentScore)) %>% 
      ggplot2::ggplot(ggplot2::aes(x = factor(date), y = DayScore, colour = Topic)) + 
      ggplot2::geom_point() +
      ggplot2::geom_path(ggplot2::aes(group=1)) +
      ggplot2::geom_hline(yintercept = 0, color = "black") +
      ggplot2::facet_wrap(~Topic, ncol = 2, scales = "free_y") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Sentiment Scores Across all Topics") +
      ggplot2::xlab('Day') +
      ggplot2::ylab('Daily Sentiment Score') +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(TD_Topic_TimeScale)
  }
}

#' @title Twitter Data Worldmap Plot.
#'
#' @description Displays the location of a Tweet across the globe by hashtag or topic.
#'
#' @param DataFrame DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @import ggplot2
#' @import maps
#' @importFrom dplyr quo
#' 
#' @return A ggplot plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_map <- tweet_worldmap(DataFrameTidyScores = score_data,
#'                          HT_Topic = "hashtag")
#' ht_map
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_map <- tweet_worldmap(DataFrameTidyScores = score_data,
#'                       HT_Topic = "topic") 
#' topic_map                    
#' }
#' @export

tweet_worldmap <- function(DataFrame, HT_Topic) {
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  long <- dplyr::quo(long)
  lat <- dplyr::quo(lat)
  group <- dplyr::quo(group)
  longitude <- dplyr::quo(longitude)
  latitude <- dplyr::quo(latitude)
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)

  if(HT_Topic == "hashtag") {
    TD_HT_WorldMap <- ggplot2::map_data("world") %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
      ggplot2::geom_jitter(data = DataFrame,
                           ggplot2::aes(x = as.numeric(longitude),
                                        y = as.numeric(latitude),
                                        colour = hashtag)) + 
      ggplot2::ggtitle("World Map of Tweets") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_fill_continuous(guide = guide_legend(title = NULL)) +
      ggplot2::coord_quickmap()
    return(TD_HT_WorldMap)
  } else {
    TD_Topic_WorldMap <- ggplot2::map_data("world") %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
      ggplot2::geom_jitter(data = DataFrame,
                           ggplot2::aes(x = as.numeric(longitude),
                                        y = as.numeric(latitude),
                                        colour = Topic)) + 
      ggplot2::ggtitle("World Map of Tweets") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_fill_continuous(guide = guide_legend(title = NULL)) +
      ggplot2::coord_quickmap()
    return(TD_Topic_WorldMap)
  }
}

