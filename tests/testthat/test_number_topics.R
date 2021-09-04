
testthat::context("Compute the numer of Tweet Topics")

# Test Data

# build text strings
text1 <- "I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!"
text2 <- "cats are the best most amazing friends anyone could ask for"
text3 <- "if you are looking for a job, come on down to the local Tire Exchange"

# assemble tribble
test_df <- tibble::tribble(
  ~text, ~hashtag, ~key,
  text1, "dog", "coolguy123",
  text2, "cat", "crazycatperson1234",
  text3, "job", "tireworld876"
)

# computer number of topics
test_NumberTopics <- saotd::number_topics(
  DataFrame = test_df, 
  num_cores = 1L, 
  min_clusters = 2, 
  max_clusters = 4, 
  skip = 1, 
  set_seed = 1234)

# Tests
testthat::test_that("The number_topics function properly ingests data frame", {
  
  testthat::expect_error(object = saotd::number_topics(DataFrame = text), 
                         "The input for this function is a data frame.")
 
})

testthat::test_that("The number_topics plot retunrs ggplot object", {
  
  testthat::expect_is(test_NumberTopics, "ggplot")
  
})