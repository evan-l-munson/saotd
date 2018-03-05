


# Acquire -----------------------------------------------------------------

#' @title Acquire Twitter Tweets
#'
#' @description Function will enable a user to access the twitter API throught the 
#' [Twitter Developers Account](https://dev.twitter.com/) site.
#' Once a user has a twitter developers account and has recieved their individual consumer key, 
#' consumer secret key, access token, and access secret key and acquire tweets they can 
#' acquire tweets based on a list of hashtags and a requested number of entires per hashtag.

#' @param consumer_key Twitter Application management consumer key.
#' @param consumer_secret Twitter Application management consumer secret key.
#' @param access_token Twitter Application management access token.
#' @param access_secret Twitter Application management access secret key.
#' @param HT A single hashtag or a list of hashtags the user has specified.
#' @param num_tweets Number of tweets to be acquired per each hashtag.
#' @param file_name User desired output .RData file name.
#' @param distinct Logical.  If distinct = TRUE, the function removes multiple tweets that originate from the same twitter id at the exact same time.
#' 
#' @importFrom twitteR setup_twitter_oauth twListToDF searchTwitter
#' @importFrom dplyr mutate distinct quo
#' @importFrom purrr map_df
#' 
#' @return A DataFrame.
#' 
#' @examples \dontrun{
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
#'         file_name = "test_tweets.RData",
#'         distinct = TRUE)
#'         
#' load("test_tweets.RData")
#' }
#' @export

Acquire <- function(consumer_key, consumer_secret, access_token, access_secret, HT, num_tweets, file_name, distinct = TRUE) {
  
  screenName <- dplyr::quo(screenName)
  created <- dplyr::quo(created)
  key <- dplyr::quo(key)
  
  twitteR::setup_twitter_oauth(consumer_key,
                               consumer_secret,
                               access_token,
                               access_secret)
  
  options("httr_oauth_cache")
  options(httr_oauth_cache = TRUE)
  
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
#' @description Function to Tidy Twitter Data and remove all emoticons whilie maintaiing actual tweet.
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' tidy_data
#' }
#' @export

Tidy <- function(DataFrame) {
  
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' data <- Merge.Terms(DataFrame = data, 
#'                     term = "ice cream", 
#'                     term_replacement = "ice_cream")
#' data 
#' }
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' TD_Unigram <- Unigram(DataFrame = data)
#' TD_Unigram
#' }             
#' @export

Unigram <- function(DataFrame){
  
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' TD_Bigram <- Bigram(DataFrame = data)
#' TD_Bigram
#' }                
#' @export

Bigram <- function(DataFrame){
  
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' TD_Trigram <- Trigram(DataFrame = data)
#' TD_Trigram
#' }
#' @export

Trigram <- function(DataFrame) {
  
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
#' @importFrom dplyr filter
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @import ggplot2
#'   
#' @return A ggraph plot.
#' 
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' TD_Bigram <- Bigram(DataFrame = data)
#' TD_Bigram_Network <- Bigram.Network(BiGramDataFrame = TD_Bigram,
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

Bigram.Network <- function(BiGramDataFrame, number = 300, layout = "fr", edge_color = "royalblue", node_color = "black", node_size = 3,  set_seed = 1234) {
  TD_Bigram_Network <- BiGramDataFrame %>% 
    dplyr::filter("n" > number) %>% 
    igraph::graph_from_data_frame()
  
  set.seed(set_seed)
  
  TD_Bigram_Network %>% 
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_link(aes(edge_alpha = "n", edge_width = "n"), edge_colour = edge_color, show.legend = TRUE) +
    ggraph::geom_node_point(colour = node_color, size = node_size) +
    ggraph::geom_node_text(aes(label = "name"), repel = TRUE) +
    ggplot2::ggtitle("Bi-Gram Network") +
    theme_void()
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' TD_Word_Corr <- Word.Corr(DataFrameTidy = tidy_data, 
#'                           number = 500,
#'                           sort = TRUE)
#'
#' TD_Word_Corr
#' }                    
#' @export

Word.Corr <- function(DataFrameTidy, number, sort = TRUE) {
  
  Token <- dplyr::quo(Token)
  n <- dplyr::quo(n)
  key <- dplyr::quo(key)
  
  TD_Word_Correlation <- DataFrameTidy %>%
    dplyr::group_by(Token) %>%
    dplyr::filter(n() >= number) %>%
    widyr::pairwise_cor(Token, key, sort = sort)
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' TD_Word_Corr <- Word.Corr(DataFrameTidy = tidy_data, 
#'                           number = 500,
#'                           sort = TRUE)
#' TD_Word_Corr_Network <- Word.Corr.Plot(WordCorr = TD_Word_Corr,
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

Word.Corr.Plot <- function(WordCorr, Correlation = 0.15, layout = "fr", edge_color = "royalblue", node_color = "black", node_size = 2,  set_seed = 1234) {
  
  correlation <- dplyr::quo(correlation)
  name <- dplyr::quo(name)
  
  set.seed(set_seed)
  
  WordCorr %>%
    dplyr::filter(correlation > Correlation) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = edge_color, show.legend = TRUE) +
    ggraph::geom_node_point(colour = node_color, size = node_size) +
    ggraph::geom_node_text(aes(label = name), repel = TRUE) +
    ggplot2::ggtitle("Word Correlation Network") +
    theme_void()
}

# Topic Analysis ----------------------------------------------------------

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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' LDA_Topic_Plot <- Number.Topics(DataFrame = data,
#'                                 num_cores = 2L,
#'                                 min_clusters = 2,
#'                                 max_clusters = 12, 
#'                                 skip = 2,
#'                                 set_seed = 1234)
#'
#' LDA_Topic_Plot 
#' }
#' @export

Number.Topics <- function(DataFrame, num_cores, min_clusters = 2, max_clusters = 12, skip = 2, set_seed = 1234) {
  
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
#' Prepares tweet text, creates DTM, conducts LDA, display data terms associated with each topic.
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
#' @importFrom topicmodels topics terms
#' 
#' @return Returns LDA topics.
#' 
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' LDA_data <- Tweet.Topics(DataFrame = data,
#'                          clusters = 8,
#'                          method = "Gibbs",
#'                          set_seed = 1234,
#'                          num_terms = 10)
#'
#' LDA_data
#' }
#' @export

Tweet.Topics <- function(DataFrame, clusters, method = "Gibbs", set_seed = 1234, num_terms = 10) {
  
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
  
  # Join original twitter data frame with tweet topics
  Tweet.Topics <- dplyr::inner_join(DataFrame, tweettopics, by = "key")
  
  return(Tweet.Topics)
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag")
#' score_data
#' }
#' @export

Scores <- function(DataFrameTidy, HT_Topic) {
  
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
  
#data("Bing")
  
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' posneg <- PosNeg.Words(DataFrameTidy = tidy_data,
#'                        n = 10)
#' posneg
#'                           
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' posneg <- PosNeg.Words(DataFrameTidy = tidy_data,
#'                        n = 10,
#'                        filterword = "fail")
#' posneg
#'                           
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' posneg <- PosNeg.Words(DataFrameTidy = tidy_data,
#'                        n = 10,
#'                        filterword = c("fail", "urgent"))            
#' posneg
#' }
#' @export

PosNeg.Words <- function(DataFrameTidy, num_words, filterword = NULL) {
  
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
    ggplot2::ggplot(aes(Token, n, fill = Sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~Sentiment, scales = "free_y") +
    labs(y = "Count",
         x = NULL) +
    theme_bw() +
    ggtitle('Most common positive and negative words utilizing the Bing Lexicon') +
    coord_flip()
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                     HT_Topic = "hashtag")
#' min_scores <- Min.Scores(DataFrameTidyScores = score_data, 
#'                          HT_Topic = "hashtag")
#'                             
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag")
#' min_scores <- Min.Scores(DataFrameTidyScores = score_data, 
#'                          HT_Topic = "hashtag",
#'                          HT_Topic_Selection = "icecream")
#' }
#' @export

Min.Scores <- function(DataFrameTidyScores, HT_Topic, HT_Topic_Selection = NULL) {
  
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag" & is.null(HT_Topic_Selection)) {
    TD_HT_noSel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
      utils::head()
    return(TD_HT_noSel_Min_Scores)
  } else if(HT_Topic == "hashtag" & !is.null(HT_Topic_Selection)) {
    TD_HT_Sel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(hashtag == HT_Topic_Selection) %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
      utils::head()
    return(TD_HT_Sel_Min_Scores)
  } else if(HT_Topic == "topic" & is.null(HT_Topic_Selection)) {
    TD_Topic_noSel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
      utils::head()
    return(TD_Topic_noSel_Min_Scores)
  } else {
    TD_Topic_Sel_Min_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(Topic == HT_Topic_Selection) %>% 
      dplyr::arrange(TweetSentimentScore) %>% 
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
#' @importFrom dplyr arrange filter desc quo
#' @importFrom utils head
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag")
#' min_scores <- Max.Scores(DataFrameTidyScores = score_data, 
#'                         HT_Topic = "hashtag")
#'                             
#' data <- twitter_data
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag")
#' min_scores <- Max.Scores(DataFrameTidyScores = score_data, 
#'                          HT_Topic = "hashtag",
#'                          HT_Topic_Selection = "icecream")
#' }
#' @export

Max.Scores <- function(DataFrameTidyScores, HT_Topic, HT_Topic_Selection = NULL) {
  
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag" & is.null(HT_Topic_Selection)) {
    TD_HT_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(dplyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    return(TD_HT_noSel_Max_Scores)
  } else if(HT_Topic == "hashtag" & !is.null(HT_Topic_Selection)) {
    TD_HT_Sel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(hashtag == HT_Topic_Selection) %>% 
      dplyr::arrange(dplyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    return(TD_HT_Sel_Max_Scores)
  } else if(HT_Topic == "topic" & is.null(HT_Topic_Selection)) {
    TD_Topic_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(dplyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    return(TD_Topic_noSel_Max_Scores)
  } else {
    TD_Topic_Sel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(Topic == HT_Topic_Selection) %>% 
      dplyr::arrange(dplyr::desc(TweetSentimentScore)) %>% 
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                     HT_Topic = "hashtag") 
#' Corp_Dist <- Corpus.Distribution(DataFrameTidyScores = score_data,
#'                                  binwidth = 1,
#'                                  color = "black", 
#'                                  fill = "white")
#' Corp_Dist
#' }
#' @export

Corups.Distribution <- function(DataFrameTidyScores, binwidth = 1, color = "black", fill = "white") {
  
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  TD_Corups_Distribution <- DataFrameTidyScores %>% 
    ggplot2::ggplot(aes(TweetSentimentScore)) +
    geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
    theme(legend.position = "none") +
    ggtitle("Sentiment Score Distribution") +
    xlab('Sentiment') +
    ylab('Count') +
    theme_bw()
  return(TD_Corups_Distribution)
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                     HT_Topic = "hashtag") 
#' Dist <- Distribution(DataFrameTidyScores = score_data,
#'                      HT_Topic = "hashtag",
#'                      binwidth = 1,
#'                      color = "black", 
#'                      fill = "white")
#' Dist
#' }
#' @export

Distribution <- function(DataFrameTidyScores, HT_Topic, binwidth = 1, color = "black", fill = "white") {
  
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag") {
    TD_HT_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(aes(TweetSentimentScore)) +
      geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
      facet_wrap(~hashtag, ncol = 2) +
      theme(legend.position = "none") +
      ggtitle("Sentiment Score Distribution Across all #Hashtags") +
      xlab('Sentiment') +
      ylab('Count') +
      theme_bw()
    return(TD_HT_Distribution)
  } else {
    TD_Topic_Distribution <- DataFrameTidyScores %>% 
      ggplot2::ggplot(aes(TweetSentimentScore)) +
      geom_histogram(stat = "count", binwidth = binwidth, colour = color, fill = fill) +
      facet_wrap(~Topic, ncol = 2) +
      theme(legend.position = "none") +
      ggtitle("Sentiment Score Distribution Across all Topics") +
      xlab('Sentiment') +
      ylab('Count') +
      theme_bw()
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                     HT_Topic = "hashtag") 
#' ht_box <- Boxplot(DataFrameTidyScores = score_data,
#'                   HT_Topic = "hashtag")
#' ht_box
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                     HT_Topic = "topic") 
#' topic_box <- Boxplot(DataFrameTidyScores = score_data,
#'                      HT_Topic = "topic") 
#' topic_box                    
#' }
#' @export

BoxPlot <- function(DataFrameTidyScores, HT_Topic) {
  
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  if(HT_Topic == "hashtag") {
    TD_HT_BoxPlot <- DataFrameTidyScores %>% 
      ggplot2::ggplot(aes(hashtag, TweetSentimentScore)) +
      ggplot2::geom_boxplot() +
      theme(legend.position = "none") +
      ggtitle("Sentiment Scores Across each #Hashtag") +
      xlab('#Hashtag') +
      ylab('Sentiment') +
      theme_bw() +
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
      theme_bw() +
      coord_flip()
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag") 
#' ht_violin <- ViolinPlot(DataFrameTidyScores = score_data,
#'                         HT_Topic = "hashtag")
#' ht_violin
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "topic") 
#' topic_violin <- ViolinPlot(DataFrameTidyScores = score_data,
#'                            HT_Topic = "topic") 
#' topic_violin                    
#' }
#' @export

ViolinPlot <- function(DataFrameTidyScores, HT_Topic) {
  
  hashtag <- dplyr::quo(hashtag)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  Topic <- dplyr::quo(Topic)
  
  if(HT_Topic == "hashtag") {
    TD_HT_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(aes(hashtag, TweetSentimentScore)) +
      geom_violin(scale = "area") +
      stat_summary(fun.y = stats::median, geom = "point", shape = 23, size = 2) +
      ggtitle("Sentiment Scores Across each #Hashtag") +
      xlab('#Hashtag') +
      ylab('Sentiment') +
      theme_bw() +
      coord_flip()
    return(TD_HT_ViolinPlot)
  } else{
    TD_Topic_ViolinPlot <- DataFrameTidyScores %>% 
      ggplot2:: ggplot(aes(Topic, TweetSentimentScore)) +
      geom_violin(scale = "area") +
      stat_summary(fun.y = stats::median, geom = "point", shape = 23, size = 2) +
      ggtitle("Sentiment Scores Across each Topic") +
      xlab('Topic') +
      ylab('Sentiment') +
      theme_bw() +
      coord_flip()
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
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag") 
#' ht_time <- TimeScale(DataFrameTidyScores = score_data,
#'                      HT_Topic = "hashtag")
#' ht_time
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "topic") 
#' topic_time <- TimeScale(DataFrameTidyScores = score_data,
#'                         HT_Topic = "topic") 
#' topic_time                    
#' }
#' @export

TimeScale <- function(DataFrameTidyScores, HT_Topic) {
  
  hashtag <- dplyr::quo(hashtag)
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  DayScore <- dplyr::quo(DayScore)
  Topic <- dplyr::quo(Topic)
  
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

#' @title Twitter Data Worldmap Plot.
#'
#' @description Displays the location of a tweet across the globe by hashtag or topic.
#'
#' @param DataFrame DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @import ggplot2
#' @importFrom dplyr quo
#' 
#' @return A ggplot plot.
#' 
#' @examples \dontrun{
#' library(SAoTD)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "hashtag") 
#' ht_map <- WorldMap(DataFrameTidyScores = score_data,
#'                    HT_Topic = "hashtag")
#' ht_map
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- Scores(DataFrameTidy = tidy_data, 
#'                      HT_Topic = "topic") 
#' topic_map <- WorldMap(DataFrameTidyScores = score_data,
#'                       HT_Topic = "topic") 
#' topic_map                    
#' }
#' @export

WorldMap <- function(DataFrame, HT_Topic) {
  
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

