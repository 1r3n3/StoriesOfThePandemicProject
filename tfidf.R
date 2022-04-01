options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)
library(stringr)

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

# Create corpus
guardian_corpus <- corpus(textdata_new$text, docnames = textdata_new$doc_id,
                          docvars = data.frame(year = substr(textdata_new$date,0,4)))

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")

# read an extended stop word list
stopwords_extended <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# Preprocessing of the corpus 
corpus_tokens <- guardian_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
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
textdata_new <- textdata_new[sel_idx, ]

# Display most used terms
topfeatures(DTM, 100, decreasing = TRUE)

# delete selected terms from DTM 
# terms, which contains "â" or"œ" or "@" or "."
terms_to_delete <- c()
for (i in 1:dim(DTM)[2]){
  if (grepl("â",colnames(DTM)[i], fixed = TRUE) | grepl("œ",colnames(DTM)[i], fixed = TRUE) | grepl(".",colnames(DTM)[i], fixed = TRUE) | grepl("@",colnames(DTM)[i], fixed = TRUE)) {
    terms_to_delete <- append(terms_to_delete, colnames(DTM)[i])
  }
}

DTM <- DTM[,!(colnames(DTM) %in% terms_to_delete)]


# Compute IDF: log(N / n_i)
number_of_docs <- nrow(DTM)
term_in_docs <- colSums(DTM > 0)
idf <- log(number_of_docs/term_in_docs)

sort(idf, decreasing = T)[1:50] 

# Compute TF
tf <- as.vector(colSums(DTM))

# Compute TF-IDF
tf_idf <- tf * idf
names(tf_idf) <- colnames(DTM)

top30terms <- sort(tf_idf, decreasing = T)[1:30]

require(wordcloud2)
words <- names(top30terms)

# visualize the terms as wordcloud
wordcloud2(data.frame(words, top30terms), shuffle = FALSE)


