# Test Data

consumer_key <- "2W3PTf95nChNkMX7zWk4hGteE"
consumer_secret <- "0cxvrWIppfFp563oP7femw9PdU1vbArBHb6xJypqJCfPJimLkT"
access_token <- "880808907282808833-2UEmMZsfod9hZnRiUyuXY8XHd9Vha5f"
access_secret <- "ahpTze65PwZMRVHpMi2mYx1vR37gthrVh97hBEK0uaVGy"

SAoTD::Acquire(consumer_key = consumer_key, 
                               consumer_secret = consumer_secret, 
                               access_token = access_token, 
                               access_secret = access_secret, 
                               HT = c("job", "cat"), 
                               num_tweets = 10, 
                               file_name = "test_Acquire.RData", 
                               distinct = TRUE)

data("raw_tweets")
test_Acquire <- raw_tweets

# Tests

test_that("The function accepts the proper inputs from users", {
  
  expect_equal(is.data.frame(test_Acquire), TRUE)


})
