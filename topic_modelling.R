options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)
library(stringr)

# Ausgangspunkt: corpus_tokens aber ohne corpus_sentences erstellt

# hier aufpassen, dass der Ordner passt! 
textdata <- read.csv("data_git/all_articles2.csv", sep = ";")

colnames(textdata) <- c('doc_id', 'id', 'url', 'date', 'title', 'text')

# Herausfiltern der Artikel, wo Corona, Covid und SARS-Cov-2 weder im Text 
# noch im Titel vorkommt und Artikel, in denen es keinen Text gibt
del_idx = c()
for (i in 1:nrow(textdata)){ # hier anstelle on nrow(textdata): einfach mal 10 eingeben und dann wordcounts ausgeben lassen
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


# Create corpus
guardian_corpus <- corpus(textdata_new$text, docnames = textdata_new$doc_id,
                          docvars = data.frame(year = substr(textdata_new$date,0,4)))


# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")

# read an extended stop word list
stopwords_extended <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# Preprocessing of the corpus of sentences
corpus_tokens <- guardian_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% # remove_separators = TRUE
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T) %>% 
  tokens_remove("")



# Create DTM, but remove terms which occur in less than 1% of all documents
DTM <- corpus_tokens %>%
  dfm() %>%
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop")
# have a look at the number of documents and terms in the matrix
dim(DTM)

# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

# Meist verwendeten Terme anzeigen lassen
topfeatures(DTM, 100, decreasing = TRUE)

# ausgewählte Terme rauslöschen aus DTM
terms_to_delete <- c("â")
DTM <- DTM[,!(colnames(DTM) %in% terms_to_delete)]



# load package topicmodels
require(topicmodels)
# number of topics
K <- 5
# compute the LDA model, inference via n iterations of
# Gibbs sampling
topicModel <- LDA(DTM, K, method = "Gibbs", control = list(iter = 500,
                                                           seed = 1, verbose = 25))

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")



##########################################################

# alternativ: DTM <- DocumentTermMatrix(corpus_tokens, control = list(bounds = list(global = c(minimumFrequency, Inf))))

library(ldatuning)
# create models with different number of topics
result <- FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)


FindTopicsNumber_plot(result)


# number of topics
K <- 20
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))


terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")







#install.packages("doParallel")
#cluster <- makeCluster(10)
#library(doParallel)
#cores <- detectCores()
#c1 <- makeCluster(cores[1]-1)
#registerDoParallel(c1)