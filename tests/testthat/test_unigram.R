
# Test Data
text <- "I really love my dog, he is the best friend anyone could ever ask for!"
test_unigram_df <- as.data.frame(x = text)

correct_unigram_df <- tibble::tribble(
  ~word, ~n,
  "dog", as.integer(1),
  "friend", as.integer(1),
  "love", as.integer(1)) %>%
  as.data.frame()

# tests
testthat::test_that("unigrams are computed properly", {
  
  testthat::expect_equal(saotd::unigram(DataFrame = test_unigram_df), 
               correct_unigram_df)
  
  testthat::expect_error(object = saotd::unigram(DataFrame = text), 
               "The input for this function is a data frame.")
  
})
