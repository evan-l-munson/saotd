test_that("The Tidy function is working as properly", {

  text1 <- "I really love my dog, he is the best friend anyone could ever ask for!"
  text2 <- "RT I want @coolguy24 to meet me for #icecream!!!! https://t.co/v4nta536rd"
  text <- c(text1, text2)
  test_Tidy_df <- as.data.frame(text)

  true_Tidy_df <- dplyr::tribble(
    ~text, ~Token,
    text1, as.character("love"),
    text1, as.character("dog"),
    text1, as.character("friend"),
    text2, as.character("coolguy24"),
    text2, as.character("meet"),
    text2, as.character("icecream")
  )

  true <- true_Tidy_df$Token
  test <- saotd::Tidy(DataFrame = test_Tidy_df)
  test <- test$Token

  expect_equal(test, true)
  expect_error(object = saotd::Tidy(DataFrame = text), "The input for this function is a data frame.")

})
