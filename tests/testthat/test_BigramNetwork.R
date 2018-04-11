test_that("The Bigram.Network function is working as properly", {
  
  text <- "I really love my dog, he is the best friend anyone could ever ask for!"

  expect_error(object = SAoTD::Bigram.Network(BiGramDataFrame = text), "The input for this function is a data frame.")
  
})