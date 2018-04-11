# Test Data

test_WordCorr_df <- dplyr::data_frame(
  text = c("I am happy and joyful",
           "I am sad and annoyed",
           "I am supremely happy and gratefully annoyed",
           "I am super duper happy and joyful"),
  key = c("coolguy123",
          "whoknowswhat45847",
          "al;sdkjf8978",
          "kalsdfj9087"))

correct_WordCorr_df <- dplyr::tribble(
  ~item1, ~item2, ~correlation,
  "joyful", "happy", as.double(0.577), 
  "happy", "joyful", as.double(0.577), 
  "annoyed", "happy", as.double(-0.577),
  "happy", "annoyed", as.double(-0.577),
  "annoyed", "joyful", as.double(-1.000),
  "joyful", "annoyed", as.double(-1.000)
)

test_WordCorr_Tidy_df <- SAoTD::Tidy(DataFrame = test_WordCorr_df)
test <- SAoTD::Word.Corr(DataFrameTidy = test_WordCorr_Tidy_df, number = 2) %>% 
  dplyr::mutate(correlation = round(x = correlation, digits = 3))
  
# Tests
test_that("Word Correlations has correct input dataframe", {

  expect_error(object = SAoTD::Bigram(DataFrame = text), "The input for this function is a data frame.")

})

test_that("Word Correlations are properly computed", {
  
  expect_equal(test, correct_WordCorr_df)
})


