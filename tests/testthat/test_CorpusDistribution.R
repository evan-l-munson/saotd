context("test_CorpusDistribution")

# Test Data

test_HT_df <- dplyr::data_frame(
  text = c("I really love and hate my dog, he is the best most amazing friend anyone could ever ask for!
           I really hate my love to hate on my stupid dog, he is the worst friend anyone could ever ask for!",
           "cats are the best most amazing friends anyone could ask for "),
  hashtag = c("dog", "cat"),
  created = lubridate::as_datetime(c('2018-02-09 17:56:30', '2018-02-10 18:46:10')),
  key = c("coolguy123", "crazycatperson1234"))

test_HT_Tidy <- saotd::Tidy(DataFrame = test_HT_df)
test_HT_Tidy_Scores <- saotd::Scores(DataFrameTidy = test_HT_Tidy, HT_Topic = "hashtag")

p <- saotd::Corpus.Distribution(DataFrameTidyScores = test_HT_Tidy_Scores)

# Tests
test_that("The Corpus.Distribution function properly ingests data frame", {

  expect_error(object = saotd::Corpus.Distribution(DataFrameTidyScores = text), "The input for this function is a data frame.")

})

test_that("The Corpus.Distribution plot retunrs ggplot object", {

  expect_is(p, "ggplot")

})

