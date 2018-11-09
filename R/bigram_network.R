
#' @title Twitter Bi-Gram Network
#'
#' @description Displays the Bi-Gram Network.  Bi-Gram networks builds on computed Bi-Grams.  Bi-Gram networks serve as a visualization tool that displays the relationships between the words simultaneously as opposed to a tabular display of Bi-Gram words.
#' 
#' @param BiGramDataFrame DataFrame of Bi-Grams.
#' @param number The minimum desired number of Bi-Gram occurances to be displayed (number = 300, would display all Bi-Grams that have at least 300 instances.)
#' @param layout Desired layout from the `ggraph` package.  Acceptable layouts:  "star", "circle", "gem", "dh", "graphopt", "grid", "mds", "randomly", "fr", "kk", "drl", "lgl"
#' @param edge_color User desired edge color.
#' @param node_color User desired node color.
#' @param node_size User desired node size.
#' @param set_seed Seed for reproducable results.
#' 
#' @importFrom dplyr filter quo
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom scales rescale
#' @import ggplot2
#'   
#' @return A ggraph plot.
#' 
#' @examples 
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' TD_Bigram <- bigram(DataFrame = data)
#' TD_Bigram_Network <- bigram_network(BiGramDataFrame = TD_Bigram,
#'                                     number = 300,
#'                                     layout = "fr",
#'                                     edge_color = "royalblue",
#'                                     node_color = "black",
#'                                     node_size = 3,
#'                                     set_seed = 1234)
#'
#' TD_Bigram_Network
#' }
#' @export

bigram_network <- function(BiGramDataFrame, number = 300, layout = "fr", edge_color = "royalblue", node_color = "black", node_size = 3,  set_seed = 1234) {
  
  if(!is.data.frame(BiGramDataFrame)) {
    stop('The input for this function is a Bigram data frame.')
  }
  if(!(("word1" %in% colnames(BiGramDataFrame)) & ("word2" %in% colnames(BiGramDataFrame)) & ("n" %in% colnames(BiGramDataFrame)))) {
    stop('The data frame is not properly constructed.  The data frame must have three columns: word1, word2 and n.')
  }
  if(number < 1) {
    stop('You must choose number of Bi-Grams greater than 1.')
  }
  
  n <- dplyr::quo(n)
  name <- dplyr::quo(name)
  
  TD_Bigram_Network <- BiGramDataFrame %>% 
    dplyr::filter(n > number) %>% 
    igraph::graph_from_data_frame()
  
  set.seed(set_seed)
  
  TD_Bigram_Network %>% 
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_link(ggplot2::aes(edge_alpha = 1, edge_width = scales::rescale(n, to=c(1,10))), edge_colour = edge_color, show.legend = TRUE) +
    ggraph::geom_node_point(colour = node_color, size = node_size) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    ggplot2::ggtitle("Bi-Gram Network") +
    ggplot2::theme_void()
}
