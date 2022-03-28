options(stringsAsFactors = FALSE)
library(quanteda)

# Vector with the names of all saved files
filenames <- c("coronavirus1","coronavirus2","coronavirus3","coronavirus4","coronavirus5","coronavirus6","coronavirus7","coronavirus8",
               "covid_1","covid_2","covid_3","covid_4","covid_5","covid_6","covid_7",
               "covid-19_1", "covid-19_2","covid-19_3","covid-19_4","covid-19_5","covid-19_6","covid-19_7",
               "corona_1","corona_2","sarscov2_1","sarscov2_2", "febmarch22")

# Merge all data into one dataframe
all_articles <- data.frame()
for(i in filenames){
  articles <- read.csv(paste("data_git/", i, ".csv", sep= ""), sep = ";")
  all_articles <- rbind(all_articles, articles)
}

colnames(all_articles) <- c('id', 'url', 'date', 'title', 'text')

# Saving the data in one csv file
write.csv2(all_articles, file = "data_git/all_articles2.csv")