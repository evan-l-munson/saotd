
#' @title Twitter Tri-Grams
#'
#' @description Determines and displays the text Tri-Grams within the Twitter 
#'   data in sequence from the most used to the least used.  A Tri-Gram is a 
#'   combination of three consecutive words.
#' 
#' @param DataFrame Data Frame of Twitter Data.
#' 
#' @importFrom dplyr count mutate filter quo
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#' @importFrom tidytext unnest_tokens 
#' 
#' @return A tribble.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' TD_Trigram <- trigram(DataFrame = data)
#' TD_Trigram
#' }
#' @export

trigram <- function(DataFrame) {
  
  # input checking
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  # configure defusing operators for packages checking
  text <- dplyr::quo(text)
  word <- dplyr::quo(word)
  trigram <- dplyr::quo(trigram)
  word1 <- dplyr::quo(word1)
  word2 <- dplyr::quo(word2)
  word3 <- dplyr::quo(word3)
  
  # web url
  wu <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
  
  # function main body
  TD_Trigram <- DataFrame %>% 
    dplyr::mutate(
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
        replacement = ""),
      text = stringr::str_replace_all(
        string = text, 
        pattern = "#", 
        replacement = ""),
      text = stringr::str_replace_all(
        string = text, 
        pattern = "[:punct:]", 
        replacement = ""),
      text = stringr::str_replace_all(
        string = text, 
        pattern = "[^[:alnum:]///' ]", 
        replacement = "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(
      output = trigram, 
      input = text, 
      token = "ngrams", 
      n = 3) %>%  
    tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
    dplyr::filter(!word1 %in% c(tidytext::stop_words$word, '[0-9]+')) %>% 
    dplyr::filter(!word2 %in% c(tidytext::stop_words$word, '[0-9]+')) %>%
    dplyr::filter(!word3 %in% c(tidytext::stop_words$word, '[0-9]+')) %>%
    dplyr::count(word1, word2, word3, sort = TRUE)
  
  return(TD_Trigram)
  
}
