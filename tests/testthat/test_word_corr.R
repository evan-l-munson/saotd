
testthat::context("Word Correlation")

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

correct_WordCorr_df <- dplyr::tribble(
  ~item1, ~item2, ~correlation,
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
  dplyr::mutate(correlation = round(x = correlation, digits = 3))
  
# Tests
testthat::test_that("Word Correlations has correct input dataframe", {

  testthat::expect_error(object = saotd::word_corr(DataFrameTidy = text), 
                         "The input for this function is a data frame.")
  
  testthat::expect_error(
    object = saotd::word_corr(DataFrameTidy = correct_WordCorr_df), 
    "The data frame is not properly constructed.  
         The data frame must contain at minimum the columns: Token and key.")
  
  testthat::expect_error(
    object = saotd::word_corr(DataFrameTidy = test_WordCorr_Tidy_df, 
                              number = 1), 
    "Must choose number of Correlation pairs greater than 1.")

})

testthat::test_that("Word Correlations are properly computed", {
  
  testthat::expect_equal(test, correct_WordCorr_df)
  
})


