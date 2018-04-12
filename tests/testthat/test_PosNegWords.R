# Test Data

test_HT_df <- dplyr::data_frame(
  text = "I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!  
  I really hate my love to hate on my stupid dog, he is the worst friend anyone could ever ask for!",
  hashtag = "dog", 
  created = lubridate::as_datetime('2018-02-09 17:56:30'),
  key = "coolguy123")

test_HT_Tidy_df <- SAoTD::Tidy(DataFrame = test_HT_df)
test_HT_PosNeg <- SAoTD::PosNeg.Words(DataFrameTidy = test_HT_Tidy_df, num_words = 1)
test_HT <- test_HT_PosNeg$data[2]

check_HT <- dplyr::data_frame(
  Sentiment = as.character(c("negative", "positive")))

test_Topic_df <- dplyr::data_frame(
  text = "I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!  
  I really hate my love to hate on my stupid dog, he is the worst friend anyone could ever ask for!",
  Topic = "dog", 
  created = lubridate::as_datetime('2018-02-09 17:56:30'),
  key = "coolguy123")

test_Topic_Tidy_df <- SAoTD::Tidy(DataFrame = test_Topic_df)
test_Topic_PosNeg <- SAoTD::PosNeg.Words(DataFrameTidy = test_Topic_Tidy_df, num_words = 1)
test_Topic <- test_Topic_PosNeg$data[2]

check_Topic <- dplyr::data_frame(
  Sentiment = as.character(c("negative", "positive")))

# Tests
test_that("The Scores function properly ingests data frame", {
  
  expect_error(object = SAoTD::PosNeg.Words(DataFrameTidy = text), "The input for this function is a data frame.")
  expect_error(object = SAoTD::PosNeg.Words(DataFrameTidy = test_HT_Tidy_df, num_words = "two"), "Enter a number.")
  
})

test_that("The PosNeg.Word function computes the scores correctly for hashtags", {
  
  expect_equal(test_HT, check_HT)
  
})

test_that("The Scores function computes the scores correctly for topics", {
  
  expect_equal(test_Topic, check_Topic)
  
})

test_that("The PosNeg.Word plot using hashtags retunrs ggplot object", {
  
  expect_is(test_HT_PosNeg, "ggplot")
  
})

test_that("The PosNeg.Word plot using topics retunrs ggplot object", {
  
  expect_is(test_Topic_PosNeg, "ggplot")
  
})