library(tidyverse)
library(tidytext)
library(topicmodels)
library(gridExtra)

##http://tidytextmining.com/topicmodeling.html


scopus <- read.csv("scopus.csv", stringsAsFactors = FALSE)
scopus$Year <- as.factor(scopus$Year) 

#split title characters into its own variable, 'word', remove N/As
#remove stop words
scopus1 <- scopus %>%
  na.omit() %>%
  unnest_tokens(word, Title) %>%
  count(Year, word, sort = TRUE) %>%
  filter(Year != 2017)%>%
  anti_join(stop_words) %>%
  ungroup()


#most frequent terms from titles over time

top <- scopus1 %>%
  group_by(Year)%>% 
  arrange(desc(Year), desc(n)) %>%
  top_n(5)

top_2016 <- top %>% 
  filter(Year == 2016) %>% arrange(n) %>%
  ggplot(aes(x =reorder(word,n),y = n)) + geom_bar(stat = "identity") +
  coord_flip() + ggtitle ("Frequent Publication Title Terms, 2016 ")

top_2015 <- top %>% 
  filter(Year == 2015) %>% arrange(n) %>%
  ggplot(aes(x =reorder(word,n),y = n)) + geom_bar(stat = "identity") +
  coord_flip() + ggtitle ("Frequent Publication Title Terms, 2015 ")

top_2014 <- top %>% 
  filter(Year == 2014) %>% arrange(n) %>%
  ggplot(aes(x =reorder(word,n),y = n)) + geom_bar(stat = "identity") +
  coord_flip() + ggtitle ("Frequent Publication Title Terms, 2014 ")

top_2013 <- top %>% 
  filter(Year == 2013) %>% arrange(n) %>%
  ggplot(aes(x =reorder(word,n),y = n)) + geom_bar(stat = "identity") +
  coord_flip() + ggtitle ("Frequent Publication Title Terms, 2013 ")

top_2012 <- top %>% 
  filter(Year == 2012) %>% arrange(n) %>%
  ggplot(aes(x =reorder(word,n),y = n)) + geom_bar(stat = "identity") +
  coord_flip()+ ggtitle ("Frequent Publication Title Terms, 2012 ")

top_2011 <- top %>% 
  filter(Year == 2011) %>% arrange(n) %>%
  ggplot(aes(x =reorder(word,n),y = n)) + geom_bar(stat = "identity") +
  coord_flip()+ ggtitle ("Frequent Publication Title Terms, 2011 ")


grid.arrange(top_2016, top_2015, top_2014, top_2013, nrow = 2)

##treemap

library(portfolio)
map.market(id=top$Year, area=top$n, group=top$word, color=top$n, main = "Tufts Social Science, 2011-2017")



# topic modeling,
# first cast df to 'document term matrix'

scopus1$Year <- as.integer(scopus1$Year)
scopus_dtm <- scopus1 %>% filter(Year == 5) %>% cast_dtm(Year, word, n)

## Run LDA

scopus_dtm <- LDA(scopus_dtm, k = 10, control = list(seed = 11091987))

## Convert back to DF

scopus_dtm <- tidy(scopus_dtm)

top_terms <- scopus_dtm %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Bar chart of clustered topics
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()

