options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)
library(tidyverse)

# Plot article frequencies over time

# Save the month and year of the date separately as a column
textdata_new$months <- format(as.Date(textdata_new$date), "%Y %m")
#head(textdata_new$months,5)

# aggregation of articles over months
articles_cnt <- count(textdata_new, textdata_new$months)
#head(articles_cnt,5)

colnames(articles_cnt) <- c('months', 'counts')
# control: sum of all articles
#articles_ges <- sum(articles_cnt$counts)



# barplot:
ggplot(articles_cnt, aes(months, counts)) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
  scale_y_continuous(name = "Number of articles") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"))
  #+ labs(title = "Number of Articles over time")

# alternative: dot plot
ggplot(articles_cnt, aes(date, counts)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
  scale_y_continuous(name = "Number of articles") + labs(title = "Number of Articles over time")