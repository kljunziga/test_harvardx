library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata %>% filter(str_detect(title, 'Pride and Prejudice'))

index <- str_detect(!is.na(gutenberg_metadata$title), "Pride and Prejudice")
gutenberg_metadata[index,]

sum(index)


gutenberg_works(title == "Pride and Prejudice")

temp <- gutenberg_download(1342)
words <- temp %>% unnest_tokens(word, text) 
words %>% nrow()

words %>%filter(!word %in% stop_words$word ) %>% nrow()



words[!str_detect(words$word, "\\d"),] %>% nrow()


words[!str_detect(words$word, "\\d"),] %>% filter(!word %in% stop_words$word ) %>% group_by(word) %>% summarize(n = n()) %>%
  arrange(n) %>% filter (n>100) %>% nrow()



afinn <- get_sentiments("afinn")

words %>% inner_join(afinn, by = "word") %>% 
  select(word, value) %>% filter(value==4) %>% nrow()


