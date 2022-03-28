options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)
library(stringr)
require(igraph)

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

# if cooc analysis with only a subset of the data
# select articles only from 2019 - Dezember 2020
#subset_textdata <- textdata_new %>% filter(textdata_new$date >= "2021-01-01" & textdata_new$date < "2022-04-01" )
# alternativ: subset_textdata <- textdata[which(textdata$date > "2019-12-01" & textdata$date < "2020-07-01")]

# Create corpus from subset_textdata
#guardian_corpus <- corpus(subset_textdata$text, docnames = subset_textdata$doc_id,
                        #docvars = data.frame(year = substr(subset_textdata$date,0,4)))

# Create corpus
guardian_corpus <- corpus(textdata_new$text, docnames = textdata_new$doc_id,
                          docvars = data.frame(year = substr(textdata_new$date,0,4)))

# separate sentences
corpus_sentences <- corpus_reshape(guardian_corpus, to = "sentences")

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")

# read an extended stop word list
stopwords_extended <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")

# Preprocessing of the corpus of sentences
corpus_tokens <- corpus_sentences %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% # remove_separators = TRUE
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T) %>% 
  tokens_remove("")


minimumFrequency <- 50
# Create DTM, prune vocabulary and set binary values for
# presence/absence of types
binDTM <- corpus_tokens %>%
  dfm() %>%
  dfm_trim(min_docfreq = minimumFrequency) %>%
  dfm_weight("boolean")

memory.limit(48000)

# Read in the source code for the co-occurrence calculation
source("calculateCoocStatistics.R")
# Definition of a parameter for the representation of the
# co-occurrences of a concept
numberOfCoocs <- 15
# Determination of the term of which co-competitors are to
# be measured.
coocTerm <- "covid-19" # vaccine

coocs <- calculateCoocStatistics(coocTerm, binDTM, measure = "LOGLIK")

# Display the numberOfCoocs main terms
#print(coocs[1:numberOfCoocs])

resultGraph <- data.frame(from = character(), to = character(),
                          sig = numeric(0))


# The structure of the temporary graph object is equal to
# that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(),
                       sig = numeric(0))
# Fill the data.frame to produce the correct number of
# lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all
# lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the
# respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]
# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)
# Iteration over the most significant numberOfCoocs
# co-occurrences of the search term
for (i in 1:numberOfCoocs) {
  # Calling up the co-occurrence calculation for term i
  # from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure = "LOGLIK")
  # print the co-occurrences
  coocs2[1:10]
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(),
                         sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  # Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[,
                                                               1]), ])
}

# Sample of some examples from resultGraph
#resultGraph[sample(nrow(resultGraph), 6), ]


require(igraph)
# set seed for graph plot
set.seed(1)
# Create the graph object as undirected graph
graphNetwork <- graph.data.frame(resultGraph, directed = F)
# Identification of all nodes with less than 2 edges
verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
# These edges are removed from the graph

graphNetwork <- delete.vertices(graphNetwork, verticesToRemove)
# Assign colors to nodes (search term blue, others orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange')
# Set edge colors
E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
# scale significance between 1 and 10 for edge width
E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))
# Set edges with radius
E(graphNetwork)$curved <- 0.15
# Size the nodes by their degree of networking (scaled between 5 and 15)
V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork)), to = c(5, 15))
# Define the frame and spacing for the plot
par(mai=c(0,0,1,0))
# Final Plot
plot(
  graphNetwork,
  layout = layout.fruchterman.reingold, # Force Directed Layout
  main = paste(coocTerm, ' Graph'),
  vertex.label.family = "sans",
  vertex.label.cex = 0.8,
  vertex.shape = "circle",
  vertex.label.dist = 0.5, # Labels of the nodes moved slightly
  vertex.frame.color = adjustcolor("darkgray", alpha.f = .5),
  vertex.label.color = 'black', # Color of node names
  vertex.label.font = 2, # Font of node names
  vertex.label = V(graphNetwork)$name, # node names
  vertex.label.cex = 1 # font size of node names
)



###########################################################################
# code for some tests with cooccurrence counts

# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM

as.matrix(coocCounts[202:205, 202:205])

coocTerm <- "covid-19"
k <- nrow(binDTM)
ki <- sum(binDTM[, coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]

########## MI: log(k*kij / (ki * kj) ########
mutualInformationSig <- log(k * kij/(ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig,
                                                   decreasing = TRUE)]
########## DICE: 2 X&Y / X + Y ##############
dicesig <- 2 * kij/(ki + kj)
dicesig <- dicesig[order(dicesig, decreasing = TRUE)]
########## Log Likelihood ###################
logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) +
                 (kij * log(kij)) + (k - ki - kj + kij) * log(k - ki - kj +
                                                                kij) + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj -
                                                                                                                       kij) - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing = T)]

# Put all significance statistics in one Data-Frame
resultOverView <- data.frame(names(sort(kij, decreasing = T)[1:10]),
                             sort(kij, decreasing = T)[1:10], names(mutualInformationSig[1:10]),
                             mutualInformationSig[1:10], names(dicesig[1:10]), dicesig[1:10],
                             names(logsig[1:10]), logsig[1:10], row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms",
                              "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)

