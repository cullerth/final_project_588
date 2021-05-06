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
  unnest_tokens(word, text)

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
  count(document, word, sort = TRUE) %>%
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

frequencies <- word_counts_docs %>%
  count(document, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

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
                      stem=TRUE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords="thee", "thy", "thou")
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


chapters_lda <- LDA(chapters_dtm, k = 34, control = list(seed = 1234))


