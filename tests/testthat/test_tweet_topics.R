
testthat::context("Compute the numer of Tweet Topics")

# Test Data
# build text strings
text1 <- "I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!"
text2 <- "cats are the best most amazing friends anyone could ask for"
text3 <- "if you are looking for a job, come on down to the local Tire Exchange"

# assemble TEST tribble
test_df <- tibble::tribble(
  ~text, ~hashtag, ~key,
  text1, "dog", "coolguy123",
  text2, "cat", "crazycatperson1234",
  text3, "job", "tireworld876"
)

# compute TEST topics
test_TweetTopics <- saotd::tweet_topics(
  DataFrame = test_df, 
  clusters = 3, 
  num_terms = 5)

# assemble CHECK tribble
check_TweetTopics <- tibble::tribble(
  ~text, ~hashtag, ~key, ~Topic,
  text1, "dog", "coolguy123", as.integer(1),
  text2, "cat", "crazycatperson1234", as.integer(3),
  text3, "job", "tireworld876", as.integer(2)
)

# Tests
testthat::test_that("The tweet_topics function properly accepts input items", {
  
  testthat::expect_error(object = saotd::tweet_topics(DataFrame = text), 
                         "The input for this function is a data frame.")
  
  testthat::expect_error(object = saotd::tweet_topics(DataFrame = test_df, 
                                                      clusters = "two"), 
                         "The input must be a numerical value.")
  
})

testthat::test_that("The tweet_topics is being computed correctly", {
  
  testthat::expect_identical(test_TweetTopics, check_TweetTopics)
  
})

