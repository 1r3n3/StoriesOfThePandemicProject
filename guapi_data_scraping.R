install.packages("guardianapi")
install.packages("devtools")
devtools::install_github("evanodell/guardianapi")

library(usethis)
library(devtools)
library(guardianapi)

# GU_API_KEY: Either store the key as a system environment variable or use gu_api_key() to store it locally in R

# Scraping of articles over specified period:
corona_articles <- gu_content(query = "SARS-CoV-2", from_date = "2022-02-01", to_date = "2022-03-15")
# used querys: "Coronavirus", "Covid-19", "Covid", "SARS-CoV-2", "Corona"


# To view the downloaded data: 
tibble::glimpse(corona_articles)
#corona_articles$web_url
#corona_articles$headline
#corona_articles3$body_text[2]
#corona_articles$web_publication_date

# Saving the relevant information in a data frame
articles <- data.frame(
  url = corona_articles$web_url,
  date = corona_articles$web_publication_date,
  title = corona_articles$headline,
  body = corona_articles$body_text
)

# Viewing the data:
head(articles,5)

# Sometimes several data were merged and then saved:
# all_articles <- rbind(all_articles,articles)

# Saving the data in a csv file, Designation of the filename according to the query and numbering
write.csv2(articles, file = "data/coronavirus1.csv")



