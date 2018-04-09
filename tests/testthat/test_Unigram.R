test_that("Unigrams are computed properly", {
  
})

text <- "I really love my dog, he is the best friend anyone could ever ask for!"

df_test <- as.data.frame(x = text)


SAoTD::Unigram(DataFrame = df_test)


unigram_test <- df_test %>% 
  dplyr::mutate(text = stringr::str_replace_all(text, "RT", "")) %>% # Remove retweet note
  dplyr::mutate(text = stringr::str_replace_all(text, "&amp", "")) %>% # Remove Accelerated Mobile Pages (AMP) note
  dplyr::mutate(text = stringr::str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% 
  dplyr::mutate(text = stringr::str_replace_all(text, "#", "")) %>% 
  dplyr::mutate(text = stringr::str_replace_all(text, "[:punct:]", "")) %>% 
  dplyr::mutate(text = stringr::str_replace_all(text, "[^[:alnum:]///' ]", "")) %>%  # Remove Emojis
  tidytext::unnest_tokens(word, text) %>%  
  dplyr::filter(!word %in% c(tidytext::stop_words$word, '[0-9]+')) %>% 
  dplyr::count(word, sort = TRUE)