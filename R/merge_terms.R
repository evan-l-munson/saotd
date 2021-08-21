
#' @title Merge Terms
#'
#' @description Function to merge terms within a data frame and prevent 
#'   redundancy in the analysis.  For example many users may refer to the same
#'   entity in multiple different ways: President Trump, The U.S. President, 
#'   POTUS, Trump, President Donald Trump, Donald Trump, etc.  While each entry 
#'   is different, they all refer to the same individual.  Using Merge Terms 
#'   will allow all be converted into a single term. 
#'
#' @param DataFrame Data Frame of Twitter Data.
#' @param term Term selected for merging.
#' @param term_replacement Desired replacement term.
#' @param ignore_case True is the default setting and will ignore case 
#'   sensitivity of the selected terms.  Selecting FALSE will maintain 
#'   case sensitivity.
#' 
#' @return A Tibble with user selected term replacement.
#' 
#' @importFrom dplyr mutate
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' data <- merge_terms(DataFrame = data, 
#'                     term = "ice cream", 
#'                     term_replacement = "ice_cream")
#' data 
#' }
#' @export

merge_terms <- function(DataFrame, 
                        term, 
                        term_replacement,
                        ignore_case = TRUE){
  
  # input checking
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  # function main body
  merging <-  DataFrame %>% 
    dplyr::mutate(
      text = gsub(x = term,
                  pattern = term, 
                  replacement = term_replacement, 
                  ignore.case = ignore_case))
  
  return(merging)
  
}
