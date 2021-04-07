
#' @title Tidy Twitter Data
#'
#' @description Function to Tidy Twitter Data.  This function will remove a 
#'   significant amount of the original twitter metadata, as it is not needed 
#'   to determine the sentiment fo the tweets. This function will remove all 
#'   emoticons, punctuation, weblinks while maintaining actual Tweet text.
#'
#' @param DataFrame DataFrame of Twitter Data.
#' 
#' @importFrom dplyr mutate filter quo rename select
#' @importFrom stringr str_replace_all
#' @importFrom tidytext unnest_tokens
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' 
#' data <- raw_tweets
#' tidy_data <- tweet_tidy(DataFrame = data)
#' tidy_data
#' 
#' }
#' @export

tweet_tidy <- function(DataFrame) {
  
  # input checks
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  # configure defusing operators for packages checking
  text <- dplyr::quo(text)
  cleantext <- dplyr::quo(cleantext)
  word <- dplyr::quo(word)
  
  # function main body
  TD_Tidy <- DataFrame %>%
    dplyr::select(c(user_id, 
                  status_id, 
                  created_at,
                  screen_name, 
                  text, 
                  hashtags, 
                  location, 
                  key, 
                  query)) %>% 
    dplyr::mutate(
      cleantext = stringr::str_replace_all(
        string = text, 
        pattern = "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", 
        replacement = ""),
      cleantext = stringr::str_replace_all(
        string = cleantext, 
        pattern = "#", 
        replacement = ""),
      cleantext = stringr::str_replace_all(
        string = cleantext, 
        pattern = "http", 
        replacement = ""),
      cleantext = stringr::str_replace_all(
        string = cleantext, 
        pattern = "RT", 
        replacement = ""), # Remove retweet note
      cleantext = stringr::str_replace_all(
        string = cleantext, 
        pattern = "[:punct:]", 
        replacement = ""),
      cleantext = stringr::str_replace_all(
        string = cleantext, 
        pattern = "[^[:alnum:]///' ]", 
        replacement = "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(
      output = word,
      input = cleantext,
      token = "words",
      drop = TRUE) %>% 
    dplyr::filter(!word %in% tidytext::stop_words$word) %>% 
    dplyr::rename(Token = word)
  
  return(TD_Tidy)
  
}
