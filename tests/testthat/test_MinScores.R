# Test Data

# Data for hashtag without "HT_Topic_Selection"
test_HT_df <- dplyr::data_frame(
  text = "I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!",
  hashtag = "dog", 
  created = lubridate::as_datetime('2018-02-09 17:56:30'),
  key = "coolguy123")

test_HT_Tidy_df <- SAoTD::Tidy(DataFrame = test_HT_df)
test_HT_Scores_Tidy_df <- SAoTD::Scores(DataFrameTidy = test_HT_Tidy_df, HT_Topic = "hashtag")
test_HT <- SAoTD::Min.Scores(DataFrameTidyScores = test_HT_Scores_Tidy_df, HT_Topic = "hashtag")
test_HT <- test_HT$TweetSentiment

check_HT <- "positive"

# Data for hashtag with "HT_Topic_Selection"
test_HT_selection <- SAoTD::Min.Scores(DataFrameTidyScores = test_HT_Scores_Tidy_df, HT_Topic = "hashtag", HT_Topic_Selection = "dog")
test_HT_selection <- test_HT_selection$TweetSentiment

check_HT_selection <- "positive"

# Data for topic without "HT_Topic_Selection"
test_Topic_df <- dplyr::data_frame(
  text = "I really love to hate on my stupid dog, he is the worst friend anyone could ever ask for!",
  Topic = "dog", 
  created = lubridate::as_datetime('2018-02-09 17:56:30'),
  key = "coolguy123")

test_Topic_Tidy_df <- SAoTD::Tidy(DataFrame = test_Topic_df)
test_Topic_Scores_Tidy_df <- SAoTD::Scores(DataFrameTidy = test_Topic_Tidy_df, HT_Topic = "topic")
test_Topic <- SAoTD::Min.Scores(DataFrameTidyScores = test_Topic_Scores_Tidy_df, HT_Topic = "topic")
test_Topic <- test_Topic$TweetSentiment

check_Topic <- "negative"

# Data for topic without "HT_Topic_Selection"
test_Topic_selection <- SAoTD::Min.Scores(DataFrameTidyScores = test_Topic_Scores_Tidy_df, HT_Topic = "topic", HT_Topic_Selection = "dog")
test_Topic_selection <- test_Topic_selection$TweetSentiment

check_Topic_selection <- "negative"

# Tests
test_that("The Min.Scores function properly ingests data frame", {
  
  expect_error(object = SAoTD::Min.Scores(DataFrameTidyScores = text), "The input for this function is a data frame.")
  expect_error(object = SAoTD::Min.Scores(DataFrameTidyScores = test_HT_Scores_Tidy_df, HT_Topic = "HT"), "HT_Topic requires an input of either hashtag for analysis using hashtags, or topic for analysis looking at topics.")
  
})

test_that("The Min.Scores function using hashtags properly computes scores", {
  
  expect_equal(test_HT, check_HT)
  
})

test_that("The Min.Scores function using topics properly computes scores", {
  
  expect_equal(test_Topic, check_Topic)
  
})

test_that("The Min.Scores function using hashtags and a hashtag selection properly computes scores", {
  
  expect_equal(test_HT_selection, check_HT_selection)
  
})

test_that("The Min.Scores function using topics and a topic selection properly computes scores", {
  
  expect_equal(test_Topic_selection, check_Topic_selection)
  
})