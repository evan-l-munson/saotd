
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
  
  # reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
  
  retained_col <- c("user_id", 
                    "status_id", 
                    "created_at", 
                    "screen_name", 
                    "text", 
                    "hashtags", 
                    "location", 
                    "key", 
                    "query")
  
  TD_Tidy <- DataFrame %>%
    dplyr::select(retained_col) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "#", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "http", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "[:punct:]", "")) %>% 
    dplyr::mutate(cleantext = stringr::str_replace_all(cleantext, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
    tidytext::unnest_tokens(output = word, 
                            input = cleantext, 
                            token = "words", 
                            drop = TRUE) %>% 
    dplyr::filter(!word %in% tidytext::stop_words$word) %>% 
    dplyr::rename(Token = word)
  # return(TD_Tidy)
}
