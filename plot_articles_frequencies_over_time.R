options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)
library(tidyverse)

# Plot article frequencies over time
print(format(as.Date("2019-12-01 23:42:00"), "%m %Y"))

textdata <- read.csv("data_git/all_articles.csv", sep = ";")

colnames(textdata) <- c('doc_id', 'id', 'url', 'date', 'title', 'text')


textdata2 <- textdata
textdata2$date <- format(as.Date(textdata2$date), "%Y %m")
head(textdata2$date,5)

articles_cnt <- count(textdata2, textdata2$date)
head(articles_cnt,5)

colnames(articles_cnt) <- c('date', 'counts')
articles_ges <- sum(articles_cnt$counts)


ggplot(articles_cnt, aes(date, counts)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
  scale_y_continuous(name = "Number of articles") + labs(title = "Number of Articles over time")


ggplot(articles_cnt, aes(date, counts)) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
  scale_y_continuous(name = "Number of articles") + labs(title = "Number of Articles over time")

