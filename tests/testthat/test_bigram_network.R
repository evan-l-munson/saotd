
testthat::context("Plot Bigram Network")

# Test data
text <- "This is the website for “R for Data Science”. This book will teach you 
  how to do data science with R: You’ll learn how to get your data into R, get 
  it into the most useful structure, transform it, visualise it and model it." 
test_text_df <- as.data.frame(x = text)

test_bigram_df <- saotd::bigram(DataFrame = test_text_df)

incorrect_bigram_df <- tibble::tribble(
  ~word, ~word2, ~n,
  "data", "science", as.integer(2),
  "structure", "transform", as.integer(1),
  "youll", "learn", as.integer(1)
)

p <- saotd::bigram_network(BiGramDataFrame = test_bigram_df, 
                           number = 1)

# Tests
testthat::test_that("The bigram_network function is working as properly", {

  testthat::expect_error(
    object = saotd::bigram_network(
      BiGramDataFrame = text), 
               "The input for this function is a Bigram data frame.")
  
  testthat::expect_error(
    object = saotd::bigram_network(
      BiGramDataFrame = test_bigram_df, 
      number = 0),
    "You must choose number of Bi-Grams greater than 1.")
  
  testthat::expect_error(
    object = saotd::bigram_network(
      BiGramDataFrame = incorrect_bigram_df), 
    'The data frame is not properly constructed.  
         The data frame must have three columns: word1, word2 and n.')
  
})

testthat::test_that("The bigram_network plot retunrs ggplot object", {
  
  testthat::expect_is(p, "ggplot")
  
})

