# Test Data

test_HT_df <- dplyr::data_frame(
  text = "I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!",
  hashtag = "dog", 
  created = lubridate::as_datetime('2018-02-09 17:56:30'),
  key = "coolguy123")

test_HT_Tidy_df <- SAoTD::Tidy(DataFrame = test_HT_df)
test_HT_Scores_Tidy_df <- SAoTD::Scores(DataFrameTidy = test_HT_Tidy_df, HT_Topic = "hashtag")
test_HT <- test_HT_Scores_Tidy_df$TweetSentiment

check_HT <- "positive"

test_Topic_df <- dplyr::data_frame(
  text = "I really hate my love to hate on my stupid dog, he is the worst friend anyone could ever ask for!",
  Topic = "dog", 
  created = lubridate::as_datetime('2018-02-09 17:56:30'),
  key = "coolguy123")

test_Topic_Tidy_df <- SAoTD::Tidy(DataFrame = test_Topic_df)
test_Topic_Scores_Tidy_df <- SAoTD::Scores(DataFrameTidy = test_Topic_Tidy_df, HT_Topic = "topic")
test_Topic <- test_Topic_Scores_Tidy_df$TweetSentiment

check_Topic <- "negative"

# Tests
test_that("The Scores function properly ingests data frame", {
  
  expect_error(object = SAoTD::Scores(DataFrameTidy = text), "The input for this function is a data frame.")
  expect_error(object = SAoTD::Scores(DataFrameTidy = test_Scores_Tidy_df, HT_Topic = "HT"), "HT_Topic requires an input of either \"hashtag\" for analysis using hashtags, or \"topic\" for analysis looking at topics.")
  
})

test_that("The Scores function computes the scores correctly for hashtags", {
  
  expect_equal(test_HT, check_HT)
  
})

test_that("The Scores function computes the scores correctly for topics", {
  
  expect_equal(test_Topic, check_Topic)
  
})