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


# Preprocessing with separation of sentences (for cooccurrence analysis):
# separate sentences
corpus_sentences <- corpus_reshape(guardian_corpus, to = "sentences")

# Preprocessing of the corpus_sentences
corpus_tokens <- corpus_sentences %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T) %>% 
  tokens_remove("")




# Not used / considered in this work:

# calculate multi-word unit candidates
#sotu_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens2, min_count = 1000)
# Display Multi-word unit candidates
#sotu_collocations[1:150,]
#tail(sotu_collocations,50)

#sotu_collocations <- sotu_collocations[1:500, ]
# Merge corpus_tokens with sotu_collocations
#corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)