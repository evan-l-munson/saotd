
# Test Data

# Data for hashtag without "HT_Topic_Selection"
test_HT_df <- dplyr::tibble(
  text = c("I really love my dog, he is the best most amazing friend anyone 
           could ever ask for!",
           "cats are the best most amazing friends anyone could ask for except 
           when they are being miserable horrible terrible demon spawn"),
  hashtags = c("dog", "cat"), 
  created_at = lubridate::as_datetime(c('2018-02-09 17:56:30', 
                                        '2018-02-10 18:46:10')),
  key = c("coolguy123", "crazycatperson1234"))

test_HT_Tidy_df <- 
  saotd::tweet_tidy(
    DataFrame = test_HT_df)

test_HT_Scores_Tidy_df <- 
  saotd::tweet_scores(
    DataFrameTidy = test_HT_Tidy_df, 
    HT_Topic = "hashtag")

test_HT <- 
  saotd::tweet_max_scores(
    DataFrameTidyScores = test_HT_Scores_Tidy_df, 
    HT_Topic = "hashtag")

test_HT <- test_HT[[1, 8]]

check_HT <- 2

# Data for hashtag with "HT_Topic_Selection"
test_HT_selection <- 
  saotd::tweet_max_scores(
    DataFrameTidyScores = test_HT_Scores_Tidy_df, 
    HT_Topic = "hashtag", HT_Topic_Selection = "dog")

test_HT_selection <- test_HT_selection[[1, 8]]

check_HT_selection <- 2

# Data for topic without "HT_Topic_Selection"
test_Topic_df <- dplyr::tibble(
  text = c("I really love my dog, he is the best most amazing friend anyone 
           could ever ask for!",
           "cats are the best most amazing friends anyone could ask for except 
           when they are being miserable horrible terrible demon spawn"),
  Topic = c("dog", "cat"), 
  created_at = lubridate::as_datetime(c('2018-02-09 17:56:30', 
                                        '2018-02-10 18:46:10')),
  key = c("coolguy123", "crazycatperson1234"))

test_Topic_Tidy_df <- 
  saotd::tweet_tidy(
    DataFrame = test_Topic_df)

test_Topic_Scores_Tidy_df <- 
  saotd::tweet_scores(
    DataFrameTidy = test_Topic_Tidy_df, 
    HT_Topic = "topic")

test_Topic <- 
  saotd::tweet_max_scores(
    DataFrameTidyScores = test_Topic_Scores_Tidy_df, 
    HT_Topic = "topic")

test_Topic <- test_Topic[[1, 8]]

check_Topic <- 2

# Data for topic without "HT_Topic_Selection"
test_Topic_selection <- 
  saotd::tweet_max_scores(
    DataFrameTidyScores = test_Topic_Scores_Tidy_df, 
    HT_Topic = "topic", HT_Topic_Selection = "dog")

test_Topic_selection <- test_Topic_selection[[1, 8]]

check_Topic_selection <- 2

# Tests
testthat::test_that("tweet_max_scores function properly ingests data frame", {
  
  testthat::expect_error(
    object = saotd::tweet_max_scores(
      DataFrameTidyScores = text), 
    "The input for this function is a data frame.")
  
  testthat::expect_error(
    object = saotd::tweet_max_scores(
      DataFrameTidyScores = test_HT_Scores_Tidy_df, 
      HT_Topic = "HT"), 
    "HT_Topic requires an input of either hashtag for analysis using 
         hashtags, or topic for analysis looking at topics.")
  
})

testthat::test_that("The tweet_max_scores function using hashtags properly 
                    computes scores", {
  
  testthat::expect_equal(test_HT, check_HT)
  
})

testthat::test_that("The tweet_max_scores function using topics properly 
                    computes scores", {
  
  testthat::expect_equal(test_Topic, check_Topic)
  
})

testthat::test_that("The tweet_max_scores function using hashtags and a hashtag 
                    selection properly computes scores", {
                      
  testthat::expect_equal(test_HT_selection, check_HT_selection)
  
})

testthat::test_that("The Max.Scores function using topics and a topic selection 
                    properly computes scores", {
  
  testthat::expect_equal(test_Topic_selection, check_Topic_selection)
  
})
