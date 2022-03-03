install.packages("guardianapi")
install.packages("devtools")
devtools::install_github("evanodell/guardianapi")

library(usethis)
library(devtools)
library(guardianapi)

# GU_API_KEY: Key entweder als Systemumgebungsvariable speichern oder mit Hilfe gu_api_key() lokal in R

corona_articles <- gu_content(query = "Covid-19", from_date = "2021-11-01", to_date = "2022-01-31")
# "coronavirus", "Covid-19", "Covid", "SARS-CoV-2", "Corona"

corona_articles1 <- corona_articles


# Zum Anschauen der heruntergeladenen Daten: 
tibble::glimpse(corona_articles)
#corona_articles$web_url
#corona_articles$headline
#corona_articles3$body_text[2]
#corona_articles$web_publication_date

# Abspeichern der relevanten Informationen in einem Dataframe
articles1 <- data.frame(
  url = corona_articles$web_url,
  date = corona_articles$web_publication_date,
  title = corona_articles$headline,
  body = corona_articles$body_text
)

# Falls notwendig: Anhängen von Daten an bestehendes Dataframe
all_articles <- rbind(all_articles,articles4)

write.csv2(articles1, file = "data/covid-19_7.csv") # covid-19_5

# Anschauen der Daten:
View(articles)
head(articles,5)


