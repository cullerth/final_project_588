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

## which books to use?
## random number generate to choose 5 from the top 20 list i've been using...
## https://www.calculator.net/random-number-generator.html
## 3, 16, 10, 4, 13
## which tranlsates to...
## Huck Finn, Communist Manifesto, Ulysses, The Awakening, Les Mis

ulysses <- readtext("banned_books/ulysses-by-james-joyce.txt")
## revisit gutenbergr package, try to do these one at a time; will process out project gutenberg front matter so I don't have to do that manually
# hgwells <- gutenberg_download(c(35, 36, 5230, 159))
# titles <- c("Twenty Thousand Leagues under the Sea", 
#             "The War of the Worlds",
#             "Pride and Prejudice", 
#             "Great Expectations")
# books <- gutenberg_works(title %in% titles) %>%
#   gutenberg_download(meta_fields = "title")
# ulysses_gb <- gutenberg_download(meta_fields = "Ulysses")

ulysses_tidy <- ulysses %>% 
  rename(book = doc_id) %>%
  na.omit() %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
## make custom list of stopwords to include all character names, project gutenberg words, any other proper nouns that show up a lot

ulysses_counts <- ulysses_tidy %>%
  filter(word != "project") %>%
  filter(word != "gutenberg") %>%
  filter(word != "chapter") %>%
  count(word, sort = TRUE) 

#wordcloud
wordcloud2(ulysses_counts,
           color = ifelse(ulysses_counts[, 2] > 170, 'black', 'gray'))

#bargraph of counts
ulysses_counts %>%
  filter(n > 170) %>% 
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

#frequencies graph
ulysses_frequencies %>%
  slice_max(proportion, n = 5) %>%
  ungroup() %>%
  ggplot(aes(proportion, fct_reorder(word, proportion), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 3, scales = "free") +
  labs(y = NULL, x = NULL)

######
## topic modeling ##
#####

## add to the following by using the textbook's example of chunking/chapters as novels using novels : https://www.tidytextmining.com/topicmodeling.html 

ulysses_dtm <- ulysses_tidy %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

temp <- textProcessor(ulysses$text, 
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

meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

stemmed_ulysses <- ulysses %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

stemmed_dtm_ulysses <- ulysses %>%
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

## plot.STM(banned_stm_ulysses, n = 5)

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

## toLDAvis(mod = banned_stm_ulysses, docs = docs)



