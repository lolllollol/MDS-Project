library(tidyverse)
library(tidytext)
library(topicmodels)

# 1. Read CSV
df <- read_csv("around_world_80_days.csv") 
# Expecting df$text column

# 2. Tokenize and remove stopwords
tokens <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# (Optional) Remove common narrative filler
tokens <- tokens %>% 
  filter(!word %in% c("mr", "mrs", "said"))

# 3. Document-Term Matrix
dtm <- tokens %>%
  count(chapter, word) %>%
  cast_dtm(document = chapter, term = word, value = n)

# 4. Fit LDA (choose k topics)
k <- 5
lda_model <- LDA(dtm, k = k, control = list(seed = 42))

# 5. Extract top terms per topic
topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta)

print(top_terms)
