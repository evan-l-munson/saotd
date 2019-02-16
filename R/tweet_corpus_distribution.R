
#' @title Twitter Corpus Distribution
#'
#' @description Determines the scores distribution for the entire Twitter data corpus.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd and scored.
#' @param binwidth The width of the bins.  Default is 1.
#' @param color The user selected color to highlight the bins.
#' @param fill The interior color of the bins.
#' 
#' @importFrom dplyr quo
#' @import ggplot2
#' 
#' @return A ggplot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' Corp_Dist <- tweet_corpus_distribution(DataFrameTidyScores = score_data,
#'                                        binwidth = 1,
#'                                        color = "black", 
#'                                        fill = "white")
#' Corp_Dist
#' }
#' @export

tweet_corpus_distribution <- function(DataFrameTidyScores, binwidth = 1, color = "black", fill = "white") {
  
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  
  TweetSentimentScore <- dplyr::quo(TweetSentimentScore)
  
  TD_Corpus_Distribution <- DataFrameTidyScores %>% 
    ggplot2::ggplot(ggplot2::aes(TweetSentimentScore)) +
    ggplot2::geom_col(binwidth = binwidth, colour = color, fill = fill) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Sentiment Score Distribution") +
    ggplot2::xlab('Sentiment') +
    ggplot2::ylab('Count') +
    ggplot2::theme_bw()
  return(TD_Corpus_Distribution)
}
