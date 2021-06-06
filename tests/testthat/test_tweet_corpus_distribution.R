
testthat::context("Plot Score Distribution")

# Test Data

test_HT_df <- dplyr::data_frame(
  text = c("I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!
           I really hate my love to hate on my stupid dog, he is the worst friend anyone could ever ask for!",
           "cats are the best most amazing friends anyone could ask for "),
  hashtags = c("dog", "cat"),
  created_at = lubridate::as_datetime(c('2018-02-09 17:56:30', '2018-02-10 18:46:10')),
  key = c("coolguy123", "crazycatperson1234"))

test_HT_Tidy <- 
  saotd::tweet_tidy(
    DataFrame = test_HT_df)

test_HT_Tidy_Scores <- 
  saotd::tweet_scores(
    DataFrameTidy = test_HT_Tidy,
    HT_Topic = "hashtag")

p <- saotd::tweet_corpus_distribution(
  DataFrameTidyScores = test_HT_Tidy_Scores)

# Tests
testthat::test_that("The tweet_corpus_distribution function properly ingests data frame", {

  testthat::expect_error(
    object = saotd::tweet_corpus_distribution(
      DataFrameTidyScores = text), 
    "The input for this function is a data frame.")

})

testthat::test_that("The tweet_corpus_distribution plot retunrs ggplot object", {

  testthat::expect_is(p, "ggplot")

})

