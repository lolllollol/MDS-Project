library(tidyverse)
library(tidytext)
library(ggplot2)

book <- read_csv("around_world_80_days.csv") %>%
  select(chapter, text)

data("stop_words")

tidy_book <- book %>%
  unnest_tokens(word, text) %>%     # turn text into individual words
  anti_join(stop_words, by = "word")

bing <- get_sentiments("bing")

book_sentiments <- tidy_book %>%
  inner_join(bing, by = "word") 

sentiment_by_chapter <- book_sentiments %>%
  count(chapter, sentiment)

ggplot(sentiment_by_chapter,
       aes(x = chapter, y = n, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Sentiment Across Chapters (Bing Lexicon)",
       x = "Chapter",
       y = "Word Count") +
  theme_minimal()

net <- sentiment_by_chapter %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

ggplot(net, aes(x = chapter, y = net_sentiment)) +
  geom_col() +
  labs(title = "Net Sentiment by Chapter",
       x = "Chapter",
       y = "Net Sentiment (positive - negative)") +
  theme_minimal()

nrc <- get_sentiments("nrc")

nrc_words <- tidy_book %>%
  inner_join(nrc, by = "word") %>%
  count(chapter, sentiment)

ggplot(nrc_words, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Emotion Distribution Across the Entire Book",
       x = "Emotion",
       y = "Word Count") +
  theme_minimal()
