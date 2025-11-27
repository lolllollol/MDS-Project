pacman::p_load(stringr, dplyr)
library(purrr)
library(ggplot2)
library(igraph)
library(ggraph)

data <- read.csv("around_world_80_days.csv") %>% select(-X)

alias_list <- list(
  "Phileas Fogg"       = c("Phileas Fogg", "Mr. Fogg",'fogg'),
  "Passepartout"       = c("Passepartout", "Jean Passepartout"),
  "Aouda"              = c("Aouda"),
  "Detective Fix"      = c("Detective Fix", "Fix"),
  "James Forster"      = c("James Forster"),
  "Sir Francis Cromarty" = c("Sir Francis Cromarty"),
  "Colonel Stamp Proctor" = c("Colonel Stamp Proctor"),
  "Captain Andrew Speedy" = c("Captain Andrew Speedy"),
  "John Bunsby"        = c("John Bunsby"),
  "Mr. Batulcar"       = c("Mr. Batulcar"),
  "General Grant"      = c("General Grant")
)

ex_number_fogg <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("fogg", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

fogg <- ex_number_fogg(data)

ex_number_passepartout <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("passepartout", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

passepartout <- ex_number_passepartout(data)

ex_number_aouda <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("aouda", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

aouda <- ex_number_aouda(data)

ex_number_fix <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("fix", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

fix <- ex_number_fix(data)

ex_number_forster <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("forster", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

forster <- ex_number_forster(data)

ex_number_cromarty <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("cromarty", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

cromarty <- ex_number_cromarty(data)

ex_number_proctor <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("proctor", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

proctor <- ex_number_proctor(data)

ex_number_speedy <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("speedy", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

speedy <- ex_number_speedy(data)

ex_number_bunsby <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("bunsby", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

bunsby <- ex_number_bunsby(data)

ex_number_batulcar <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("batulcar", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

batulcar <- ex_number_batulcar(data)

ex_number_grant <- function(data){
  total <- 0
  for (i in 1:37) {
    ch <- data[i,]
    matches <- str_extract_all(ch, regex("grant", ignore_case = TRUE))[[2]]
    total <- total + length(matches)
  }
  return(total)
}

grant <- ex_number_grant(data)

count_character <- function(data, pattern) {
  data %>%
    mutate(text = as.character(text)) %>%
    pull(text) %>%
    str_count(regex(pattern, ignore_case = TRUE)) %>%
    sum()
}

alias_patterns <- lapply(alias_list, function(x) paste0("\\b(", paste(x, collapse="|"), ")\\b"))

character_counts <- map_int(alias_patterns, ~count_character(data, .x))

character_counts_df <- data.frame(
  character = names(alias_patterns),
  count = character_counts
)

character_counts_df

ggplot(character_counts_df, aes(x = reorder(character, count), y = count)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Character Mentions in *Around the World in 80 Days*",
    x = "Character",
    y = "Number of Mentions"
  ) +
  theme_minimal(base_size = 14)

df2 <- character_counts_df %>%
  mutate(perc = count / sum(count),
         label = paste0(character, "\n", scales::percent(perc)))

ggplot(df2, aes(x = "", y = perc, fill = character)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(perc)), 
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  labs(title = "Character Mentions (Pie Chart with Percentages)")

chapter_character_presence <- function(chapter_text, pattern){
  str_detect(chapter_text, regex(pattern, ignore_case = TRUE))
}

chapter_texts <- data$text 

presence_matrix <- sapply(alias_patterns, function(pattern){
  str_detect(chapter_texts, regex(pattern, ignore_case = TRUE))
})

colnames(presence_matrix) <- names(alias_patterns)

co_occurrence <- t(presence_matrix) %*% presence_matrix
diag(co_occurrence) <- 0

g <- graph_from_adjacency_matrix(
  co_occurrence,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g <- igraph::delete.vertices(g, degree(g) == 0)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.3) +
  geom_node_point(size = 8, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width(range = c(0.2, 3)) +
  theme_void() +
  labs(title = "Character Co-Occurrence Network: Around the World in 80 Days")

