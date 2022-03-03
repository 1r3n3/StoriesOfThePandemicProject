options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)

# hier aufpassen, dass der Ordner passt! 
textdata <- read.csv("data_git/all_articles.csv", sep = ";")

colnames(textdata) <- c('doc_id', 'id', 'url', 'date', 'title', 'text')

head(textdata,2)
# Create corpus
guardian_corpus <- corpus(textdata$text, docnames = textdata$doc_id,
                          docvars = data.frame(year = substr(textdata$date,0,4)))
# original corpus length
ndoc(guardian_corpus)

substr(texts(guardian_corpus)[1],0,200)

# separate sentences
corpus_sentences <- corpus_reshape(guardian_corpus, to = "sentences")

ndoc(corpus_sentences)
texts(corpus_sentences)[1]
texts(corpus_sentences)[2]

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")

# read an extended stop word list
stopwords_extended <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# Preprocessing of the corpus of sentences
corpus_tokens <- corpus_sentences %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)


# calculate multi-word unit candidates
sotu_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,
                                                               min_count = 25)
sotu_collocations <- sotu_collocations[1:250, ]
corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)



# create DTM 
#   --> TF-IDF
#   --> TopicModel
#         --> Topic Anteile insgesamt und dann deren Anteile über die Zeit
#         --> wordcloud für jedes Topic (in einem Graph dargestellt) = Map of Knowledge??


# create binDTM: 
#   --> co-occurrences
#         --> Result graph = Map of Knowledge ?