---
title: "Jesse is 28"
author: "Sophia Ng"
date: "2/28/2020"
output:
  pdf_document: default
  html_document: default
---
# Happy Birthday JESSE

![Jesse](/Jesse/img/IMG_7571.jpeg)

Dear Jesse,

Check out my code at https://tinyurl.com/jesseis28

Thank you in advance for cooking my hamburgers very-"still bleeding"-rare in George's absence. Thank you for your coding advice. Happy birthday!


Best,

Sophia

![AM](/Jesse/img/am.jpeg)

```{r setup}
library(genius)

arc_mon_am <- genius_album(artist = "Arctic Monkeys", album = "AM")
```

# Sentiment Analysis

```{r sentiment}
library(tidyverse)
library(tidytext)
library(topicmodels)

tidy_monkey <- arc_mon_am %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()  

tidy_monkey
```
![1](/Jesse/img/1.png)


# Sentiment Analysis II

```{r sentiment_2}
library(textdata)

get_sentiments("nrc")

tidy_monkey_2 <- arc_mon_am %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()  

tidy_monkey_2
```
![2](/Jesse/img/2.png)


# Tidying the Data with tm package

```{r corpus, echo=TRUE, message=FALSE, warning=FALSE}
library(tm)

# Create raw corpus from genius lyrics 
corpus_raw <- Corpus(VectorSource(arc_mon_am$lyric))

# Transform everything to lowercase
corpus <- tm_map(corpus_raw,content_transformer(tolower))

# Strip whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation) 

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Stem the document
corpus <- tm_map(corpus, stemDocument)

# Create document term matrix
dtm <- DocumentTermMatrix(corpus)

# Tidy dtm 
corpus_tidy <- tidy(dtm)
  corpus_tidy %>% 
  bind_tf_idf(term, document, count) %>% 
  arrange(desc(tf_idf))
```


# Latent Dirichlet Allocation (LDA) 

```{r LDA}
# Deletes rows with zero entry because each row needs to contain at least one non-zero entry
raw.sum <- apply(dtm, 1, FUN=sum)
dtm <- dtm[raw.sum!=0,]

# LDA 
output <- LDA(dtm, k = 3, control = list(seed = 1234))
beta <- tidy(output, matrix = "beta")
filter(beta, topic==1)%>% arrange(desc(beta))
filter(beta, topic==2)%>% arrange(desc(beta))
filter(beta, topic==3)%>% arrange(desc(beta))
round(head(posterior(output, dtm)$topics), digits = 3)

# Use dplyr’s top_n() to find the 10 terms that are most common within each of the 3 topics
monkey_top_terms <- beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Create ggplot
g_monkey_top_terms <- monkey_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

g_monkey_top_terms
```
![3](/Jesse/img/3.png)


# In Defense of Wordclouds

```{r wordcloud, warning=FALSE}
library(wordcloud)

# Wordcloud for AM
monkey_cloud <- wordcloud(corpus, max.words = 70, random.order = FALSE, ordered.clouds = TRUE)
```

![4](/Jesse/img/4.png)
