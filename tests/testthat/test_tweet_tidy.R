
testthat::context("Tidy Tweet Data")

# Test Data
text1 <- "Really love my dog, he is the best friend anyone could ever ask for!"
text2 <- "RT I want @coolguy24 to meet me for #icecream!!!! https://t.co/v4nta"
# text <- c(text1, text2)
# test_Tidy_df <- as.data.frame(text)

test_Tidy_df <- tibble::tribble(
  ~user_id, 
    ~status_id, 
    ~created_at, 
    ~screen_name, 
    ~text, 
    ~hashtags, 
    ~location, 
    ~key, 
    ~query,
  as.character(12344), 
    as.character(098098), 
    as.POSIXct("2021-04-07 01:15:33"), 
    as.character("cool123"), 
    text1, 
    as.character("dog"), 
    as.character("Phoenix AZ"), 
    as.character("dude123 2021-04-07 01:15:33"), 
    as.character("#puppies"),
  as.character(987234), 
    as.character(90898), 
    as.POSIXct("2021-04-07 01:16:43"), 
    as.character("sweet123"), 
    text2, 
    as.character("dog"), 
    as.character("Denver CO"), 
    as.character("sweet123 2021-04-07 01:16:43"), 
    as.character("#puppies")
)

true_Tidy_df <- tibble::tribble(
  ~text, ~Token,
  text1, as.character("love"),
  text1, as.character("dog"),
  text1, as.character("friend"),
  text2, as.character("coolguy24"),
  text2, as.character("meet"),
  text2, as.character("icecream")
)

true <- true_Tidy_df$Token
test <- saotd::tweet_tidy(DataFrame = test_Tidy_df)
test <- test$Token

# tests
testthat::test_that("The tweet_tidy function is working as properly", {
  
  testthat::expect_equal(test, true)
  
  testthat::expect_error(object = saotd::tweet_tidy(DataFrame = text),
               "The input for this function is a data frame.")
  
})
