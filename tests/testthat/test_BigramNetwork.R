test_that("The Bigram.Network function is working as properly", {
  
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

  expect_error(object = SAoTD::Bigram.Network(BiGramDataFrame = text), "The input for this function is a Bigram data frame.")
  expect_error(object = SAoTD::Bigram.Network(BiGramDataFrame = inorrect_bigram_df), "The data frame is not properly constructed.  The data frame must have three columns: word1, word2 and n.")
  expect_error(object = SAoTD::Bigram.Network(BiGramDataFrame = test_bigram_df, number = 0), "You must choose number of Bi-Grams greater than 1.")
  
})