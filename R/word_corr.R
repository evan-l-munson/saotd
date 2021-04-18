
#' @title Twitter Word Correlations
#'
#' @description The word correlation displays the mutual relationship between 
#'   words.
#' 
#' @param DataFrameTidy Data Frame of Twitter Data that has been tidy'd.
#' @param number The number of word instances to be included.
#' @param sort Rank order the results from most to least correlated.
#' 
#' @importFrom dplyr group_by filter quo n
#' @importFrom widyr pairwise_cor
#' 
#' @return A Tibble.
#' 
#' @examples 
#' \donttest{
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

word_corr <- function(DataFrameTidy, 
                      number, 
                      sort = TRUE) {
  
  # input checks
  if(!is.data.frame(DataFrameTidy)) {
    stop('The input for this function is a data frame.')
  }
  
  if(!(("Token" %in% colnames(DataFrameTidy)) & 
       ("key" %in% colnames(DataFrameTidy)))) {
    stop('The data frame is not properly constructed.  
         The data frame must contain at minimum the columns: Token and key.')
  }
  
  if(number <= 1) {
    stop('Must choose number of Correlation pairs greater than 1.')
  }
  
  # configure defusing operators for packages checking
  Token <- dplyr::quo(Token)
  n <- dplyr::quo(n)
  key <- dplyr::quo(key)
  
  # function main body
  TD_Word_Correlation <- DataFrameTidy %>%
    dplyr::group_by(Token) %>%
    dplyr::filter(dplyr::n() >= number) %>%
    widyr::pairwise_cor(Token, key, sort = sort)
  
  return(TD_Word_Correlation)
  
}
