library(tidyverse)
library(tidytext)
library(wordcloud2)
library(forcats)
library(gutenbergr)
library(dplyr)
library(readtext) 
library(ggplot2)
library(SnowballC)
library(topicmodels)
library(stm)
library(ldatuning)
library(knitr)
library(LDAvis)
library(stringr)

# leaves of grass by walt whitman
leaves_of_grass <- gutenberg_download(1322)

# divide into documents, each representing one chapter (or 'book' in the case of leaves of grass)
by_chapter <- leaves_of_grass %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^BOOK ", ignore_case = FALSE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

# find document-word counts
word_counts_docs <- by_chapter_word %>%
  anti_join(stop_words) %>%
  filter(word != "thee") %>%
  filter(word != "thy") %>%
  filter(word != "thou") %>%
  count(document, word, sort = TRUE) %>%
  ungroup()
#stop words to filter out include: thee, thy, thou

#word_counts over all
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  filter(word != "thee") %>%
  filter(word != "thy") %>%
  filter(word != "thou") %>%
  count(word, sort = TRUE) %>%
  ungroup()

# word cloud
wordcloud2(word_counts,
           color = ifelse(word_counts[, 2] > 140, 'black', 'gray'))

#bargraph of counts
word_counts %>%
  filter(n > 88) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + #
  geom_col() +
  labs(x = "Word Counts", y = NULL) + 
  theme_minimal()

frequencies <- word_counts %>%
  count(word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

frequencies %>%
  slice_max(proportion, n = 5) %>%
  ungroup() %>%
  ggplot(aes(proportion, fct_reorder(word, proportion), fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol = 3, scales = "free") +
  labs(y = NULL, x = NULL)

######
## topic modeling ##
#####

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

temp <- textProcessor(by_chapter$text, 
                      metadata = by_chapter,  
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=FALSE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=FALSE)
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

stemmed_leaves <- by_chapter %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(word != "thee") %>%
  filter(word != "thy") %>%
  filter(word != "thou") %>%
  mutate(stem = wordStem(word))

stemmed_dtm <- by_chapter %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(word != "thee") %>%
  filter(word != "thy") %>%
  filter(word != "thou") %>%
  mutate(stem = wordStem(word)) %>%
  count(word, stem, sort = TRUE) %>%
  cast_dtm(word, stem, n)

chapters_lda <- LDA(chapters_dtm, k = 34, control = list(seed = 1234))

docs <- temp$documents
meta <- temp$meta
vocab <- temp$vocab

banned_stm <- stm(documents=docs,
                  data=meta,
                  vocab=vocab,
                  K=34,
                  max.em.its=25,
                  verbose = FALSE)

plot.STM(banned_stm, n = 5)

toLDAvis(mod = banned_stm, docs = docs)

banned_stm_10 <- stm(documents=docs,
                     data=meta,
                     vocab=vocab,
                     K=10,
                     max.em.its=25,
                     verbose = FALSE)

plot.STM(banned_stm_10, n = 5)

toLDAvis(mod = banned_stm_10, docs = docs)

k_metrics_chapters <- FindTopicsNumber(
  chapters_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics_chapters)
#10 or 15

nrc <- get_sentiments("nrc")
sentiment_nrc <- inner_join(by_chapter_word, nrc, by = "word")

summary_nrc <- sentiment_nrc %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "nrc") %>%
  relocate(lexicon)

summary_nrc

nrc_counts <- sentiment_nrc %>% 
  count(sentiment, sort = TRUE)

nrc_counts

nrc_counts %>% 
  mutate(sentiment = reorder(sentiment,n)) %>% 
  ggplot(aes(n, sentiment)) + 
  geom_col() +
  labs(x = "NRC Sentiment", y = NULL) + 
  theme_minimal()


# Use the filter() and grepl() functions introduced in Unit 1. Section 3b to filter for rows in our ts_forum_data data frame that contain the terms “agree” and “time” and another term or terms of your choosing. Select a random sample of 10 posts using the sample_n() function for your terms and answer the following questions:
#   
whitman_quotes_love <- by_chapter %>%
  select(text) %>% 
  filter(grepl('love', text)) 

whitman_quotes_love <- sample_n(whitman_quotes, 10)

whitman_quotes_love

whitman_quotes_men <- by_chapter %>%
  select(text) %>% 
  filter(grepl('love', text)) %>% 
  filter(grepl('man', text))
  
whitman_quotes_men <- sample_n(whitman_quotes_men, 10)

whitman_quotes_men

whitman_quotes_women <- by_chapter %>%
  select(text) %>% 
  filter(grepl('love', text)) %>% 
  filter(grepl('woman', text))

whitman_quotes_women <- sample_n(whitman_quotes_women, 10)

whitman_quotes_women

whitman_quotes_sex <- by_chapter %>%
  select(text) %>% 
  filter(grepl('sex', text))

whitman_quotes_sex <- sample_n(whitman_quotes_sex, 10)

whitman_quotes_sex

whitman_quotes_death <- by_chapter %>%
  select(text) %>% 
  filter(grepl('death', text))

whitman_quotes_death <- sample_n(whitman_quotes_death, 10)

whitman_quotes_death



whitman_bigrams <- leaves_of_grass %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

whitman_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- whitman_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 != "thou") %>%
  filter(word2 != "thou")  %>%
  filter(word1 != "walt")

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

leaves_of_grass %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word, 
         word1 != "thou",
         word2!= "thou",
         word1 != "walt") %>%
  count(word1, word2, word3, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf
