
# Create text for test
text <- "I really love my dog, he is the best friend anyone could ever ask for!"
test_MergeTerms_df <- as.data.frame(x = text, stringsAsFactors = FALSE)

# Function input is a dataframe, so format test as a data frame
correct_MergeTerms_df <- tibble::tribble(
  ~text,
  "I really love my dog, he is the BFF anyone could ever ask for!")

# It is easier to check a string than directly compare dataframes.
check <- correct_MergeTerms_df[[1]]

test <- saotd::merge_terms(
  DataFrame = test_MergeTerms_df, 
  term = "best friend", 
  term_replacement = "BFF")
test <- test[[1]]

# tests
testthat::test_that("Merge.Terms is being properly computed", {

  testthat::expect_equal(test, check)
  
  testthat::expect_error(object = saotd::merge_terms(DataFrame = text), 
               "The input for this function is a data frame.")

})

