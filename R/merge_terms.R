
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
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' data <- merge_terms(DataFrame = data, 
#'                     term = "ice cream", 
#'                     term_replacement = "ice_cream")
#' data 
#' }
#' @export

merge_terms <- function(DataFrame, term, term_replacement){
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  for(i in 1: length(DataFrame$text)){
    DataFrame[i, "text"] <- DataFrame[i, "text"] %>% 
      gsub(pattern = as.character(term),
           replacement = as.character(term_replacement),
           ignore.case = TRUE)   
  }
  DataFrame <- DataFrame
}
