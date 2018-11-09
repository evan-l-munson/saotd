
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
