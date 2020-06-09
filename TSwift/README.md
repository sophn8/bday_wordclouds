---
title: "TSwift"
author: "Sophia Ng"
date: "6/8/2020"
output: html_document
---

![old_taylor](/TSwift/img/old_taylor.jpg)


# Is T. Swift getting angstier? 

From country starlet to pop star to angsty singer, Taylor Swift's 2017 studio album, "Reputation" earned her a reputation for being edgy. As described by [Billboard](https://www.billboard.com/photos/429884/taylor-swift-style-evolution-photos), "During her Reputation era, Swift shows she’s not to be messed with. She shimmers on stage with her sequined snakeskin top, black shorts and black Christian Louboutin leather boots."

Has Taylor Swift evolved into a more edgy singer? Is it a one-off thing? What can the data-- in this case, her lyrics-- actually tell us? 

To conduct my research, I turned to R. 


## Loading the data

I used the Josiah Parry's [Genius](https://github.com/JosiahParry/genius) package, along with tidyverse and tidytext to conduct my analysis. 

```{r setup, message = FALSE, warning = FALSE}
library(tidyverse)
library(tidytext)
library(genius)

tswift <- tribble(
 ~artist, ~album,
 "Taylor Swift", "Taylor Swift",
 "Taylor Swift", "Fearless",
 "Taylor Swift", "Speak Now",
 "Taylor Swift", "Red",
 "Taylor Swift", "1989",
 "Taylor Swift", "Reputation",
 "Taylor Swift", "Lover"
)

tswift_lyrics <- tswift %>%
 add_genius(artist, album)

tswift_lyrics_2 <- tswift_lyrics%>%
  unnest_tokens(word, lyric)

# factor albums in descending release order
tswift_lyrics_2$album <- factor(tswift_lyrics_2$album, levels = c("Lover", "Reputation", "1989", "Red", "Speak Now", "Fearless", "Taylor Swift"))
```


## A Holistic Look at Taylor Swift's Discography

I decided that a sentiment analysis would be the most optimal method to research my question: has Taylor Swift become more angsty? 

[Merriam-Webster](https://www.merriam-webster.com/dictionary/angsty) defines "angsty" as "feeling, showing, or expressing anxiety, apprehension, or insecurity." I connotate "angst" as a negative sentiment. 

I use the `nrc` and `bing` lexicons to further understand Taylor Swift's lyrics as sentiments. The graphs are based on Taylor Swift's entire discography:

 - Taylor Swift (2006)
 - Fearless (2008)
 - Speak Now (2010)
 - Red (2012)
 - 1989 (2014)
 - Reputation (2017) 
 - Lover (2019)

```{r}
# nrc lexicon
sent_tswift_nrc <- tswift_lyrics %>%
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

# by specific sentiments 
sent_tswift_nrc

# bing lexicon
sent_tswift_bing <- tswift_lyrics %>%
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

# binary view 
sent_tswift_bing
```
![1](/TSwift/img/1.png)

# Disaggregating by Album 

My goal is to understand how Taylor Swift's sentiments changed over the course of her discography. To do that, I disaggregated my analysis by album. 

```{r}
tswift_lyrics_bing <- tswift_lyrics_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(album, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# factor albums 
# tswift_lyrics_bing$album <- factor(tswift_lyrics_bing$album, levels = c("Lover", "Reputation", "1989", "Red", "Speak Now", "Fearless", "Taylor Swift"))

tswift_lyrics_bing_2 <- ggplot(tswift_lyrics_bing, aes(index, sentiment, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, ncol = 3, scales = "free_x")

tswift_lyrics_bing_3 <- ggplot(tswift_lyrics_bing, aes(index, sentiment, fill = album)) +
  geom_col(show.legend = FALSE) 

# aggregate sentiment stacked barplot
tswift_lyrics_bing_3

# facet_wrap by album
tswift_lyrics_bing_2
```

![2](/TSwift/img/2.png)

![3](/TSwift/img/3.png)


The `bing` lexicon allows us to see unigram text as a positive or negative binary. This isn't as illustrative of the other lexicons. However, to answer our question-- is Taylor Swift getting more angsty?-- the `bing` lexicon offers a straightforward answer. 

Taylor Swift's discography is quite neutral in tone. Based on this analysis, her lyrics are a little more positive than negative. 

```{r}
tswift_lyrics_2_bing_sent <- tswift_lyrics_2 %>%
  inner_join(get_sentiments("bing"))

# tswift bing sentiment
tswift_bing_plot <- tswift_lyrics_2_bing_sent %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 8000)) +
  coord_flip() +
  ggtitle("Tswift's discography - bing sentiment")

tswift_bing_plot
```

![4](/TSwift/img/4.png)

In my pursuit of understanding Taylor Swift's lyrical angst over time, I settled upon looking at the raw numbers. A raw number analysis isn't that telling, however. As Taylor Swift's musical prowess matured, so did the number of tracks per record and words per song. I do note a particular peak with "1989" with 304 individual words being connotated as negative. On the other hand, "Fearless" was the least negative with only 114 words presumed to be negative. 

```{r}
# one method - count raw number of negatives
tswift_lyrics_2_bing_sent_album <- tswift_lyrics_2_bing_sent %>%
  subset(sentiment=="negative")%>%
  group_by(album) %>%
  summarise(sentiment = n()) %>%
  ungroup()

tswift_lyrics_2_bing_sent_album_plot <- tswift_lyrics_2_bing_sent_album %>%
  ggplot(aes(x = album, y = sentiment)) + 
    geom_bar(stat = "identity") + 
    ggtitle("Negative Word Count Per Album")

tswift_lyrics_2_bing_sent_album_plot
```

![5](/TSwift/img/5.png)

Another angle to answer my question is to consider the ratios of negative and positive words in relation to total lyrics per album. Taylor Swift's latest studio album, "Lover" was actually more positive than negative. Overall, there are neutral (or even) sentiments, on average, within each of her albums.  

```{r}
# negative words as a stacked barchart
tswift_lyrics_2_bing_sent_grouped <- tswift_lyrics_2_bing_sent%>%
  group_by(album, sentiment)%>%
  summarise(count = n())

tswift_lyrics_2_bing_sent_grouped_plot <- ggplot(tswift_lyrics_2_bing_sent_grouped, 
                                                 aes(fill=sentiment, y=count, x=album)) + 
                                            geom_bar(position="stack", stat="identity") +
                                            ggtitle("Binary sentiment by album")

tswift_lyrics_2_bing_sent_grouped_plot
```

![6](/TSwift/img/6.png)



## Conclusion

The Billboard article cited "Reputation" as Taylor Swift's angsty turning point. However, my analysis suggests that Swift's angst might actually have been more lyrically present via her "1989" record. 
