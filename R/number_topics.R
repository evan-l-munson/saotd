
#' @title Number Topics
#'
#' @description Determines the optimal number of Latent topics within a dataframe by tuning the Latent Dirichlet Allocation (LDA) model parameters.  
#' Uses the `ldatuning` package and outputs an ldatuning plot.  __This process can be time consuming depending on the size of the input dataframe.__
#'
#' @param DataFrame DataFrame of Twitter Data.
#' @param num_cores The number of CPU cores to processes models simultaneously (2L for dual core processor).
#' @param min_clusters Lower range for the number of clusters.
#' @param max_clusters Upper range for the number of clusters.
#' @param skip Integer; The number of clusters to skip between entries.
#' @param set_seed Seed for reproducable results.
#' 
#' @importFrom dplyr mutate group_by count anti_join quo
#' @importFrom stringr str_replace_all
#' @importFrom tidytext unnest_tokens cast_dtm 
#' @importFrom ldatuning FindTopicsNumber
#' @importFrom scales rescale
#' @importFrom reshape2 melt
#' @import ggplot2
#' 
#' @return A Tidy DataFrame.
#' 
#' @examples 
#' \donttest{
#' library(saotd)
#' data <- raw_tweets
#' LDA_Topic_Plot <- number_topics(DataFrame = data,
#'                                 num_cores = 2L,
#'                                 min_clusters = 2,
#'                                 max_clusters = 12, 
#'                                 skip = 2,
#'                                 set_seed = 1234)
#'
#' LDA_Topic_Plot 
#' }
#' @export

number_topics <- function(DataFrame, num_cores, min_clusters = 2, max_clusters = 12, skip = 2, set_seed = 1234) {
  
  if(!is.data.frame(DataFrame)) {
    stop('The input for this function is a data frame.')
  }
  
  set_seed
  
  text <- dplyr::quo(text)
  key <- dplyr::quo(key)
  word <- dplyr::quo(word)
  n <- dplyr::quo(n)
  
  lda_prep <- DataFrame %>% 
    dplyr::mutate(text = base::iconv(DataFrame$text, "latin1", "ASCII", sub="")) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% # Remove hashtag
    dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% # Remove punctuation
    dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
    dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
    dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%  # Remove links
    dplyr::group_by(key) %>%
    tidytext::unnest_tokens(word, text) %>% 
    dplyr::anti_join(tidytext::stop_words) %>% 
    dplyr::count(key, word, sort = TRUE) %>% 
    tidytext::cast_dtm(key, word, n) # create DTM
  
  # Compute Values
  values <- ldatuning::FindTopicsNumber(lda_prep, 
                                        topics = seq(from = min_clusters, to = max_clusters, by = skip),
                                        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                        method = "Gibbs",
                                        mc.cores = num_cores,
                                        verbose = TRUE)
  
  # Plot
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(values["topics"], base::apply(columns, 2, function(column) {scales::rescale(column, to = c(0, 1), from = range(column))}))
  values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
  values$group <- values$variable %in% c("Griffiths2004", "Deveaud2014")
  values$group <- base::factor(values$group, levels = c(FALSE, TRUE), labels = c("minimize", "maximize"))
  p <- ggplot2::ggplot(values, aes_string(x = "topics", y = "value", group = "variable"))
  p <- p + geom_line()
  p <- p + geom_point(aes_string(shape = "variable"), size = 3)
  p <- p + guides(size = FALSE, shape = guide_legend(title = "metrics:"))
  p <- p + scale_x_continuous(breaks = values$topics)
  p <- p + labs(x = "number of topics", y = NULL)
  p <- p + facet_grid(group ~ .)
  p <- p + theme_bw() %+replace% theme(panel.grid.major.y = element_blank(), 
                                       panel.grid.minor.y = element_blank(), 
                                       panel.grid.major.x = element_line(colour = "grey70"), 
                                       panel.grid.minor.x = element_blank(), 
                                       legend.key = element_blank(), 
                                       strip.text.y = element_text(angle = 90))
  invisible()
  return(p)
}
