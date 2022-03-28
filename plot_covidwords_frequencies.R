# Plot with the frequencies of the words Corona, Covid and SARS-Cov-2 in the corpus.
options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)
library(stringr)
library(tidyverse)

# Attention, path must fit!
textdata <- read.csv("data_git/all_articles2.csv", sep = ";")

colnames(textdata) <- c('doc_id', 'id', 'url', 'date', 'title', 'text')

# Delete articles where Corona, Covid and SARS-Cov-2 do not appear in the text or title and articles where there is no text
del_idx = c()
for (i in 1:nrow(textdata)){
  word_counts <- sum(str_count(tolower(textdata$text[i]), c("corona", "covid", "sars-cov-2")))
  word_counts_title <- sum(str_count(tolower(textdata$title[i]), c("corona", "covid", "sars-cov-2")))
  if (((word_counts+word_counts_title) < 1) | (textdata$text[i] == "")) {
    del_idx <- append(del_idx, FALSE) 
  }
  else {
    del_idx <- append(del_idx, TRUE)
  }
}

textdata_new <- textdata[del_idx, ]

# Test
#sum(str_count("Coronavirus, Corona, Coronapandemie, Coronavirus_Infektion, SARS-CoV-2, Covid-19", c("Corona", "Covid", "SARS-CoV-2")))

# calculate word counts in text and title
word_cnt <- c(0,0,0,0,0,0,0)
for (i in 1:nrow(textdata)){ 
  word_counts <- sum(str_count(tolower(textdata$text[i]), c("corona", "covid", "sars-cov-2")))
  word_counts_title <- sum(str_count(tolower(textdata$title[i]), c("corona", "covid", "sars-cov-2")))
  if ((word_counts+word_counts_title) > 5) {
    word_cnt[7] <- word_cnt[7] + 1
  }
  else {
    word_cnt[word_counts+word_counts_title+1] <- word_cnt[word_counts+word_counts_title+1] +1
  }
}

# vector with labels according to the number of word counts
num_of_words <- c(0,1,2,3,4,5,"6+")

# save word counts and the corresponding labels
covid_words <- data.frame(num_of_words, word_cnt)

# Barplot:
ggplot(covid_words, aes(num_of_words, word_cnt)) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.2)) +
  scale_y_continuous(name = "Number of articles") + scale_x_discrete(name = "Number of Covid-words") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"))
#+ labs(title = "Frequency of Covid-words in Articles")
