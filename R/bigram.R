
#' @title Twitter Bi-Grams
#'
#' @description Determines and displays the text Bi-Grams within the Twitter 
#'   data in sequence from the most used to the least used.  A Bi-Gram is a 
#'   combination of two consecutive words.
#'
#' @param DataFrame Data Frame of Twitter Data.
#'
#' @importFrom dplyr count mutate filter quo
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#' @importFrom tidytext unnest_tokens 
#'
#' @return A tibble.
#'
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' TD_Bigram <- bigram(DataFrame = data)
#' TD_Bigram
#' }
#' @export

bigram <- function(DataFrame) {
  
  # input checking
  if (!is.data.frame(DataFrame)) {
    stop("The input for this function is a data frame.")
  }

  # configure defusing operators for packages checking
  text <- dplyr::quo(text)
  word <- dplyr::quo(word)
  bigram <- dplyr::quo(bigram)
  word1 <- dplyr::quo(word1)
  word2 <- dplyr::quo(word2)

  # web url
  wu <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"

  #function main body
    TD_Bigram <- DataFrame %>%
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
      output = bigram,
      input = text,
      token = "ngrams",
      n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!word1 %in% c(tidytext::stop_words$word, "[0-9]+")) %>%
    dplyr::filter(!word2 %in% c(tidytext::stop_words$word, "[0-9]+")) %>%
    dplyr::count(word1, word2, sort = TRUE)

  return(TD_Bigram)

}
