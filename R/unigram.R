
#' @title Twitter Uni-Grams
#'
#' @description Determines and displays the text Uni-Grams within the Twitter 
#'   data in sequence from the most used to the least used.  A Uni-Gram is a 
#'   single word.
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
#' \donttest{
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
        pattern = "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", 
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
    tidytext::unnest_tokens(word, text) %>%  
    dplyr::filter(!word %in% c(tidytext::stop_words$word, '[0-9]+')) %>% 
    dplyr::count(word, sort = TRUE)
  
  return(TD_Unigram)
  
}
