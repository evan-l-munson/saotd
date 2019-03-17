
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
#' \donttest{
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
