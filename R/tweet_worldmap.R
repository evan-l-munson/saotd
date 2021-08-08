
#' @title Twitter Data Worldmap Plot.
#'
#' @description Displays the location of a Tweet across the globe by hashtag or topic.
#'
#' @param DataFrame DataFrame of Twitter Data that has been tidy'd and scored.
#' @param HT_Topic If using hashtag data select:  "hashtag".  If using topic data select:  "topic".
#' 
#' @import ggplot2
#' @import maps
#' @importFrom dplyr quo
#' 
#' @return A ggplot plot.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "hashtag") 
#' ht_map <- tweet_worldmap(DataFrameTidyScores = score_data,
#'                          HT_Topic = "hashtag")
#' ht_map
#'                
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' score_data <- tweet_scores(DataFrameTidy = tidy_data, 
#'                            HT_Topic = "topic") 
#' topic_map <- tweet_worldmap(DataFrameTidyScores = score_data,
#'                       HT_Topic = "topic") 
#' topic_map                    
#' }
#' @noRd

tweet_worldmap <- function(DataFrame, HT_Topic) {
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  if(!(("hashtag" %in% HT_Topic) | ("topic" %in% HT_Topic))) {
    stop('HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.')
  }
  
  long <- dplyr::quo(long)
  lat <- dplyr::quo(lat)
  group <- dplyr::quo(group)
  longitude <- dplyr::quo(longitude)
  latitude <- dplyr::quo(latitude)
  hashtag <- dplyr::quo(hashtag)
  Topic <- dplyr::quo(Topic)
  
  if(HT_Topic == "hashtag") {
    TD_HT_WorldMap <- ggplot2::map_data("world") %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
      ggplot2::geom_jitter(data = DataFrame,
                           ggplot2::aes(x = as.numeric(longitude),
                                        y = as.numeric(latitude),
                                        colour = hashtag)) + 
      ggplot2::ggtitle("World Map of Tweets") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_fill_continuous(guide = guide_legend(title = NULL)) +
      ggplot2::coord_quickmap()
    return(TD_HT_WorldMap)
  } else {
    TD_Topic_WorldMap <- ggplot2::map_data("world") %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
      ggplot2::geom_jitter(data = DataFrame,
                           ggplot2::aes(x = as.numeric(longitude),
                                        y = as.numeric(latitude),
                                        colour = Topic)) + 
      ggplot2::ggtitle("World Map of Tweets") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_fill_continuous(guide = guide_legend(title = NULL)) +
      ggplot2::coord_quickmap()
    return(TD_Topic_WorldMap)
  }
}

