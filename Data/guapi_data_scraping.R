install.packages("guardianapi")
install.packages("devtools")
devtools::install_github("evanodell/guardianapi")

library(usethis)
library(devtools)
library(guardianapi)

# GU_API_KEY: Key entweder als Systemumgebungsvariable speichern oder mit Hilfe gu_api_key() lokal in R

corona_articles <- gu_content(query = "coronavirus", from_date = "2020-05-01", to_date = "2020-05-31")
# "coronavirus", "Covid-19", "Covid", "Sars-Cov-2", "Corona"

# Zum Anschauen der heruntergeladenen Daten: 

#tibble::glimpse(corona_articles3)
#corona_articles$web_url
#corona_articles$headline
#corona_articles3$body_text[2]
#corona_articles$web_publication_date

# Abspeichern der relevanten Informationen in einem Dataframe
articles <- data.frame(
  url = corona_articles$web_url,
  date = corona_articles$web_publication_date,
  title = corona_articles$headline,
  body = corona_articles$body_text
)

# Falls notwendig: Anhängen von Daten an bestehendes Dataframe
# all_articles <- rbind(all_articles,articles)

write.csv2(articles, file = "data/coronavirus1.csv")

# Anschauen der Daten:
View(articles)
Head(articles,5)


