
#' @title Twitter Data Maximum Scores
#'
#' @description Determines the Maximum scores for either the entire dataset or 
#'   the Maximum scores associated with a hashtag or topic analysis.
#'
#' @param DataFrameTidyScores DataFrame of Twitter Data that has been tidy'd 
#'   and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic 
#'   data select:  "topic".
#' @param HT_Topic_Selection THe hashtag or topic to be investigated.  NULL will 
#'   find min across entire data frame.
#' 
#' @importFrom dplyr arrange filter quo desc
#' @importFrom tidyr unnest 
#' @importFrom utils head
#' 
#' @return A Tibble.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag")
#' min_scores <- tweet_max_scores(DataFrameTidyScores = score_data, 
#'                                HT_Topic = "hashtag")
#'                             
#' data <- twitter_data
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag")
#' min_scores <- tweet_max_scores(DataFrameTidyScores = score_data, 
#'                                HT_Topic = "hashtag",
#'                                HT_Topic_Selection = "icecream")
#' }
#' @export

tweet_max_scores <- function(DataFrameTidyScores, 
                             HT_Topic, 
                             HT_Topic_Selection = NULL) {
  
  # input checks
  if(!is.data.frame(DataFrameTidyScores)) {
    stop('The input for this function is a data frame.')
  }
  
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using 
         hashtags, or topic for analysis looking at topics.')
  }
  
  # configure defusing operators for packages checking
  # hashtag <- rlang::enquo(hashtag)
  Topic <- rlang::enquo(Topic)
  TweetSentimentScore <- rlang::enquo(TweetSentimentScore)
  HT_Topic_Selection <- rlang::enquo(HT_Topic_Selection)

  # function main body
  if(HT_Topic == "hashtag" & is.null(HT_Topic_Selection)) {
    
    TD_HT_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(plyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    
    return(TD_HT_noSel_Max_Scores)
    
  } else if(HT_Topic == "hashtag" & !is.null(HT_Topic_Selection)) {
    
    TD_HT_Sel_Max_Scores <- DataFrameTidyScores %>%
      tidyr::unnest(cols = {{hashtag}}, keep_empty = FALSE) %>% 
      dplyr::filter(hashtag == {{HT_Topic_Selection}}) %>% 
      dplyr::arrange(dplyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    
    return(TD_HT_Sel_Max_Scores)
    
  } else if(HT_Topic == "topic" & is.null(HT_Topic_Selection)) {
    
    TD_Topic_noSel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::arrange(dplyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    
    return(TD_Topic_noSel_Max_Scores)
    
  } else {
    
    TD_Topic_Sel_Max_Scores <- DataFrameTidyScores %>% 
      dplyr::filter(Topic == HT_Topic_Selection) %>% 
      dplyr::arrange(plyr::desc(TweetSentimentScore)) %>% 
      utils::head()
    
    return(TD_Topic_Sel_Max_Scores)
  }
  
}
