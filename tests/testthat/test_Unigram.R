test_that("Unigrams are computed properly", {
  
  text <- "I really love my dog, he is the best friend anyone could ever ask for!"
  test_unigram_df <- as.data.frame(x = text)
  
  correct_unigram_df <- tribble(
    ~word, ~n,
    "dog", as.integer(1),
    "friend", as.integer(1),
    "love", as.integer(1)
  )
  
  expect_identical(SAoTD::Unigram(DataFrame = test_unigram_df), correct_unigram_df)
})

