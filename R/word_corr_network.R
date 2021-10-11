
#' @title Twitter Word Correlations Plot
#'
#' @description The word correlation network displays the mutual relationship 
#'   between words.  The correlation network shows higher correlations with a 
#'   thicker and darker edge color.
#'
#' @param WordCorr Data Frame of Word Correlations.
#' @param Correlation Minimum level of correlation to be displayed.
#' @param layout Desired layout from the `ggraph` package.
#'   Acceptable layouts:  "star", "circle", "gem", "dh", "graphopt", "grid", 
#'   "mds", "randomly", "fr", "kk", "drl", "lgl"
#' @param edge_color User desired edge color.
#' @param node_color User desired node color.
#' @param node_size User desired node size.
#' @param set_seed Seed for reproducible results.
#'
#' @importFrom dplyr filter quo
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @import ggplot2
#'
#' @return An igraph plot
#'
#' @examples
#' \dontrun{
#' library(saotd)
#' data <- raw_tweets
#' tidy_data <- Tidy(DataFrame = data)
#' TD_Word_Corr <- word_corr(DataFrameTidy = tidy_data,
#'                           number = 500,
#'                           sort = TRUE)
#' TD_Word_Corr_Network <- word_corr_network(WordCorr = TD_Word_Corr,
#'                                        Correlation = 0.15,
#'                                        layout = "fr",
#'                                        edge_color = "royalblue",
#'                                        node_color = "black",
#'                                        node_size = 2,
#'                                        set_seed = 1234)
#'
#' TD_Word_Corr_Network
#' }
#' @export

word_corr_network <- function(WordCorr,
                              Correlation = 0.15,
                              layout = "fr",
                              edge_color = "royalblue",
                              node_color = "black",
                              node_size = 2,
                              set_seed = 1234) {

  # input checks
  if (!is.data.frame(WordCorr)) {
    stop("The input for this function is a Correlation data frame.")
  }

  if (Correlation <= 0) {
    stop("A correlation value between 0 and 1 must be selected.")
  }

  if (Correlation > 1) {
    stop("A correlation value between 0 and 1 must be selected.")
  }

  # configure defusing operators for packages checking
  correlation <- dplyr::quo(correlation)
  name <- dplyr::quo(name)

  set.seed(set_seed)

  # function main body
  corr_network <- WordCorr %>%
    dplyr::filter(correlation > Correlation) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        edge_alpha = Correlation,
        edge_width = Correlation),
      edge_colour = edge_color,
      show.legend = TRUE) +
    ggraph::geom_node_point(colour = node_color,
                            size = node_size) +
    ggraph::geom_node_text(ggplot2::aes(label = name),
                           repel = TRUE) +
    ggplot2::ggtitle("Word Correlation Network") +
    ggplot2::theme_void()

  return(corr_network)

}
