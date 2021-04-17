
testthat::context("Word Correlation Network Diagram")

# Test Data
test_WordCorr_df <- tibble::tribble(
  ~user_id, 
  ~status_id, 
  ~created_at, 
  ~screen_name, 
  ~text, 
  ~hashtags, 
  ~location, 
  ~key, 
  ~query,
  as.character(12344), 
  as.character(098098), 
  as.POSIXct("2021-04-07 01:15:33"), 
  as.character("cool123"), 
  "I am happy and joyful", 
  as.character("dog"), 
  as.character("Phoenix AZ"), 
  as.character("dude123 2021-04-07 01:15:33"), 
  as.character("#puppies"),
  as.character(987234),
  as.character(90898), 
  as.POSIXct("2021-04-07 01:16:43"), 
  as.character("sweet123"), 
  "I am sad and annoyed", 
  as.character("dog"), 
  as.character("Denver CO"), 
  as.character("sweet123 2021-04-07 01:16:43"), 
  as.character("#puppies"),
  as.character(23443), 
  as.character(5645), 
  as.POSIXct("2021-04-08 01:17:41"), 
  as.character("happy123"), 
  "I am supremely happy and gratefully annoyed",
  as.character("cat"), 
  as.character("Ouray CO"), 
  as.character("happy123 2021-04-08 01:17:41"), 
  as.character("#cat"),
  as.character(098787), 
  as.character(8765), 
  as.POSIXct("2021-04-09 06:17:45"), 
  as.character("yota123"), 
  "I am super duper happy and joyful",
  as.character("kittie"), 
  as.character("Tucson AZ"), 
  as.character("yota123 2021-04-09 06:17:45"), 
  as.character("#kittie")
)

incorrect_WordCorr_df <- tibble::tribble(
  ~item1, ~item2, ~c,
  "joyful", "happy", as.double(0.577),
  "happy", "joyful", as.double(0.577),
  "annoyed", "happy", as.double(-0.577),
  "happy", "annoyed", as.double(-0.577),
  "annoyed", "joyful", as.double(-1.000),
  "joyful", "annoyed", as.double(-1.000)
)

test_WordCorr_Tidy_df <- saotd::tweet_tidy(DataFrame = test_WordCorr_df)
test <- saotd::word_corr(DataFrameTidy = test_WordCorr_Tidy_df, 
                         number = 2) %>%
  dplyr::mutate(correlation = round(x = correlation, 
                                    digits = 3))


p <- saotd::word_corr_network(WordCorr = test, 
                              Correlation = .1)

# Tests
testthat::test_that("The word_corr_network function is working as properly", {

  testthat::expect_error(object = saotd::word_corr_network(WordCorr = text), 
               "The input for this function is a Correlation data frame.")
  
  testthat::expect_error(object = saotd::word_corr_network(WordCorr = test, 
                                                 Correlation = 0), 
               "A correlation value between 0 and 1 must be selected.")
  
  testthat::expect_error(object = saotd::word_corr_network(WordCorr = test, 
                                                 Correlation = 1.1), 
               "A correlation value between 0 and 1 must be selected.")

})

testthat::test_that("The word_corr_network retunrs ggplot object", {

  testthat::expect_is(p, "ggplot")
  
})

