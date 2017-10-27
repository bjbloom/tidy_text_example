rm(list = ls())

library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(stringr)
library(tidyr)

data(reviews, package = "LDAvisData")
data("stop_words", package = "tidytext")

reviews_df <- data_frame(reviews)
reviews_df$names <- names(reviews)

reviews_df <- reviews_df %>% 
  mutate(reviews = str_to_lower(reviews)) %>% 
  mutate(reviews = str_replace_all(reviews, '[^a-z]', ' '))

df <- reviews_df %>% 
  unnest_tokens(word, reviews) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(names, word) %>% 
  ungroup()

#Simple word frequency, but filter for most common words as input to dtm
word_freq_df <- df %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  filter(count > 20)

word_freq_df %>% 
  mutate(word = reorder(word, count)) %>% 
  top_n(10, count) %>%
  ggplot(aes(word, count)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Word Frequency in Movie Reviews") +
  coord_flip()

#Bi-gram frequencies easy to do in tidytext as well
#come back to this (maybe)
bigram_df <- reviews_df %>% 
  unnest_tokens(bigram, reviews, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE)

bigram_word_freq <- bigram_df %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigrams, word1, word2, sep = " ")

bigram_word_freq %>% 
  mutate(bigrams = reorder(bigrams, n)) %>% 
  top_n(10, n) %>%
  ggplot(aes(bigrams, n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Bi-gram Frequency in Movie Reviews") +
  coord_flip()

#Use filtered word frequency data frame to get dtm
df <- df %>% 
  inner_join(word_freq_df, by = "word") %>% 
  select(names, word, n)

#Cast as document-term matrix
df_dtm <- df %>% 
  cast_dtm(names, word, n)

#Topic model
review_lda <- LDA(df_dtm, k = 20, method = "Gibbs", control = list(seed = 1234))

#Broom-like method to return probability of word being in topic
review_lda_word <- tidy(review_lda)

#Broom-like method to return probability of topic given a document
review_lda_topic <- tidy(review_lda, matrix = "gamma")

#Look at the most common words per topic, for just one topic
review_lda_word %>% 
  filter(topic == 1) %>% 
  top_n(10, beta) %>% 
  #arrange(desc(beta)) %>% 
  mutate(term = reorder(term, -beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", fill = "blue", show.legend = F) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 15)) +
  theme(plot.title = element_text(hjust = 0.5))

#Output topic probabilites for each document, limit to 5 and threshold for gamma

classifications <- review_lda_topic %>% 
  group_by(document) %>% 
  filter(gamma > 0.05) %>% #set threshold (otherwise lots of topics/document)
  top_n(5, gamma) %>%      #take up to 5 topics per document, as long as gamma > 0.05
  arrange(desc(gamma)) %>% 
  mutate(gamma_rank = row_number()) %>% #rank order gamma per document
  ungroup()

#Re-join to original data set to return to original text of review
#Can use this to filter by topic for review

review_topics <- classifications %>% 
  left_join(reviews_df, by = c("document" = "names")) %>% 
  select(document, topic, gamma, reviews, gamma_rank)
