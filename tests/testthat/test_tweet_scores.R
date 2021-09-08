
testthat::context("Compute Tweet Sentiment Scores")

# build HT data frame
text1 <- "I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!"

test_HT_df <- tibble::tribble(
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
  as.POSIXct("2018-02-09 17:56:30"), 
  as.character("coolguy123"), 
  text1, 
  as.character("puppies"), 
  as.character("Phoenix AZ"), 
  as.character("coolguy123 2018-02-09 17:56:30"), 
  as.character("#puppies")
)

# Tidy, score and pull out the TweetSentiment for HT
test_HT_Tidy_df <- 
  saotd::tweet_tidy(
    DataFrame = test_HT_df)

test_HT_Scores_Tidy_df <-
  saotd::tweet_scores(
    DataFrameTidy = test_HT_Tidy_df, 
    HT_Topic = "hashtag")

test_HT <- test_HT_Scores_Tidy_df[[9]]

check_HT <- "positive"

# build Topic data frame
text2 <- "I really love to hate on my stupid dog, he is the worst friend anyone could ever ask for!"
 
test_Topic_df <- tibble::tribble(
  ~user_id, 
  ~status_id, 
  ~created_at, 
  ~screen_name, 
  ~text, 
  ~Topic, 
  ~location, 
  ~key, 
  ~query,
  as.character(12344), 
  as.character(098098), 
  as.POSIXct("2018-02-09 17:56:30"), 
  as.character("coolguy123"), 
  text2, 
  as.character("puppies"), 
  as.character("Phoenix AZ"), 
  as.character("coolguy123 2018-02-09 17:56:30"), 
  as.character("#puppies")
)

# Tidy, score and pull out the TweetSentiment for HT
test_Topic_Tidy_df <- 
  saotd::tweet_tidy(
    DataFrame = test_Topic_df)

test_Topic_Scores_Tidy_df <- 
  saotd::tweet_scores(
    DataFrameTidy = test_Topic_Tidy_df, 
    HT_Topic = "topic")

test_Topic <- test_Topic_Scores_Tidy_df[[9]]

check_Topic <- "negative"

# Tests
testthat::test_that("The tweet_scores function properly ingests data frame", {
  
  testthat::expect_error(object = saotd::tweet_scores(
    DataFrameTidy = text), 
    "The input for this function is a data frame.")
  
  testthat::expect_error(object = saotd::tweet_scores(
    DataFrameTidy = test_HT_Scores_Tidy_df, 
    HT_Topic = "HT"), 
    "HT_Topic requires an input of either \"hashtag\" for analysis 
         using hashtags, or \"topic\" for analysis looking at topics.")
  
})

testthat::test_that("The tweet_scores function computes the scores correctly for hashtags", {
  
  testthat::expect_equal(test_HT, check_HT)
  
})

test_that("The tweet_scores function computes the scores correctly for topics", {
  
  expect_equal(test_Topic, check_Topic)
  
})
