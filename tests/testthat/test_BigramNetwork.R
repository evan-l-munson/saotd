# Test data

text <- "This is the website for “R for Data Science”. 
  This book will teach you how to do data science with R: 
You’ll learn how to get your data into R, get it into the most useful structure, transform it, visualise it and model it." 
test_text_df <- as.data.frame(x = text)

test_bigram_df <- SAoTD::Bigram(DataFrame = test_text_df)

inorrect_bigram_df <- dplyr::tribble(
  ~word, ~word2, ~n,
  "data", "science", as.integer(2),
  "structure", "transform", as.integer(1),
  "youll", "learn", as.integer(1)
)

p <- SAoTD::Bigram.Network(BiGramDataFrame = test_bigram_df, number = 1)

# Tests
test_that("The Bigram.Network function is working as properly", {

  expect_error(object = SAoTD::Bigram.Network(BiGramDataFrame = text), "The input for this function is a Bigram data frame.")
  expect_error(object = SAoTD::Bigram.Network(BiGramDataFrame = test_bigram_df, number = 0), "You must choose number of Bi-Grams greater than 1.")
  
})

test_that("The Bigram.Network plot retunrs ggplot object", {
  
    expect_is(p, "ggplot")
})

