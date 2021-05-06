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

## which books to use?
## random number generator to choose 5 from the top 20 list i've been using...
## https://www.calculator.net/random-number-generator.html
## 3, 16, 10, 4, 13
## which tranlsates to...
## Huck Finn, Communist Manifesto, Ulysses, The Awakening, Les Mis

## looks like the gutenbergr package is back in business!
# books <- gutenberg_download(c(76,71,4300,36,135))

##alternatively: 
# books <- readtext("banned_books/*")

titles <- c("	The Awakening, and Selected Short Stories", 
            "Adventures of Huckleberry Finn",
            "The Communist Manifesto", 
            "Ulysses",
            "Les Misérables")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")


books_tidy <- books %>% 
  # rename(book = doc_id) %>%
  na.omit() %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

ulysses_tidy <- ulysses %>% 
  rename(book = doc_id) %>%
  na.omit() %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
## make custom list of stopwords to include all character names, project gutenberg words, any other proper nouns that show up a lot

books_counts <- books_tidy %>%
  count(word, sort = TRUE) 

ulysses_counts <- ulysses_tidy %>%
  filter(word != "project") %>%
  filter(word != "gutenberg") %>%
  filter(word != "chapter") %>%
  count(word, sort = TRUE) 

#wordcloud
wordcloud2(ulysses_counts,
           color = ifelse(ulysses_counts[, 2] > 170, 'black', 'gray'))

wordcloud2(books_counts,
           color = ifelse(books_counts[, 2] > 240, 'black', 'gray'))

#bargraph of counts
ulysses_counts %>%
  filter(n > 170) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + #
  geom_col() +
  labs(x = "Word Counts", y = NULL) + 
  theme_minimal()

books_counts %>%
  filter(n > 240) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + #
  geom_col() +
  labs(x = "Word Counts", y = NULL) + 
  theme_minimal()

ulysses_frequencies <- ulysses_tidy %>%
  filter(word != "project") %>%
  filter(word != "gutenberg") %>%
  filter(word != "chapter") %>%
  count(book, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

books_frequencies <- books_tidy %>%
  count(title, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

#frequencies graph
ulysses_frequencies %>%
  slice_max(proportion, n = 5) %>%
  ungroup() %>%
  ggplot(aes(proportion, fct_reorder(word, proportion), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 3, scales = "free") +
  labs(y = NULL, x = NULL)

books_frequencies %>%
  slice_max(proportion, n = 5) %>%
  ungroup() %>%
  ggplot(aes(proportion, fct_reorder(word, proportion), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 3, scales = "free") +
  labs(y = NULL, x = NULL)

######
## topic modeling ##
#####

## add to the following by using the textbook's example of chunking/chapters as novels using novels : https://www.tidytextmining.com/topicmodeling.html 

ulysses_dtm <- ulysses_tidy %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

books_dtm <- books_tidy %>%
  count(title, word) %>%
  cast_dtm(title, word, n)

temp_ulysses <- textProcessor(ulysses$text, 
                      metadata = ulysses,  
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=TRUE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=NULL)
#change to temp_ulysses once adding more books

temp <- textProcessor(books$text, 
                     metadata = books,  
                     lowercase=TRUE, 
                     removestopwords=TRUE, 
                     removenumbers=TRUE,  
                     removepunctuation=TRUE, 
                     wordLengths=c(3,Inf),
                     stem=TRUE,
                     onlycharacter= FALSE, 
                     striphtml=TRUE, 
                     customstopwords=NULL)

meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

stemmed_ulysses <- ulysses %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

stemmed_books <- books %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

stemmed_dtm_ulysses <- ulysses %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(word, stem, sort = TRUE) %>%
  cast_dtm(word, stem, n)

stemmed_dtm_books <- books %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(word, stem, sort = TRUE) %>%
  cast_dtm(word, stem, n)

ulysses_lda <- LDA(ulysses_dtm, 
                  k = 10, 
                  control = list(seed = 588)
)

ulysses_lda

books_lda <- LDA(books_dtm, 
              k = 5, 
              control = list(seed = 588)
)

books_lda

docs <- temp$documents
meta <- temp$meta
vocab <- temp$vocab

banned_stm_ulysses <- stm(documents=docs,
                  data=meta,
                  vocab=vocab,
                  K=10,
                  max.em.its=25,
                  verbose = FALSE)
# Error in base::rowSums(x, na.rm = na.rm, dims = dims, ...) : 'x' must be an array of at least two dimensions

banned_stm_books <- stm(documents=docs,
                          data=meta,
                          vocab=vocab,
                          K=5,
                          max.em.its=25,
                          verbose = FALSE)

## plot.STM(banned_stm_ulysses, n = 5)

plot.STM(banned_stm_books, n = 5)

k_metrics_ulysses <- FindTopicsNumber(
  ulysses_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics_ulysses)
## 10

k_metrics_books <- FindTopicsNumber(
  bookss_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics_books)


## toLDAvis(mod = banned_stm_ulysses, docs = docs)


leaves_of_grass <- gutenberg_download(1322)

# divide into documents, each representing one chapter
by_chapter <- leaves_of_grass %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^BOOK I ", ignore_case = FALSE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_lda <- LDA(chapters_dtm, k = 5, control = list(seed = 1234))

chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")

chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

chapters_gamma




