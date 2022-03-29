options(stringsAsFactors = FALSE)
library(quanteda)
library(dplyr)
library(stringr)
library(tm)

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


#########################################################################
# Topic Modelling

# load package topicmodels
require(topicmodels)
# number of topics
K <- 21
# compute the LDA model, inference via n iterations of
# Gibbs sampling
topicModel <- LDA(DTM, K, method = "Gibbs", control = list(iter = 500,
                                                           seed = 1, verbose = 25))

# reduce alpha to get a more peaky/more even distribution of topics in the model
#topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = "_")

tmresult <- posterior(topicModel)
theta <- tmresult$topics
beta <- tmresult$terms


######################################################################
# Find the best number of topics for topic modelling

library(ldatuning)
# create models with different number of topics
result <- FindTopicsNumber(
  DTM,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014", "Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)


FindTopicsNumber_plot(result)



###########################################################################
# Creating a map of knowledge with the results of topic modelling 

topic_headlines  <- numeric(K)
for (i in 1:K){
  topic_headlines[i] <- paste0("Topic", i)
}


# Topic Ranking:
# approach 1
topic_proportions <- colSums(theta) / dim(DTM)[1]
names(topic_proportions) <- topicNames
sop <- sort(topic_proportions, decreasing = TRUE)
paste(round(sop,5), ":", names(sop))

# approach 2:
countsofprimarytopics <- rep(0,K)
names(countsofprimarytopics) <- topicNames
for (i in 1:dim(DTM)[1]){
  topicsperdoc <- theta[i,]
  primarytopic <- order(topicsperdoc, decreasing = TRUE)[1]
  countsofprimarytopics[primarytopic] <- countsofprimarytopics[primarytopic] +1
}

sort(countsofprimarytopics, decreasing = TRUE)
topic_proportions <- countsofprimarytopics



# Creating the map:
resultGraph <- data.frame(from = character(), to = character(),
                          sig = numeric(0))

central_term <- "Corpus"
# The structure of the temporary graph object is equal to
# that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(),
                       sig = numeric(0))
# Fill the data.frame to produce the correct number of
# lines
tmpGraph[1:K, 3] <- topic_proportions
# Entry of the search word into the first column in all
# lines
tmpGraph[, 1] <- central_term
# Entry of the co-occurrences into the second column of the
# respective line
tmpGraph[, 2] <- topic_headlines
# Set the significances
tmpGraph[, 3] <- topic_proportions
# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)


numb_terms <- 6 # Number of terms to be displayed per topic
# Iteration over all K topics
for (i in 1:K) {
  # Calling up the co-occurrence calculation for term i
  # from the search words co-occurrences
  new_central_term <- topic_headlines[i]
  edges <- sort(beta[i,], decreasing = TRUE) [1:numb_terms]
  nodes <- names(edges)
  

  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(),
                         sig = numeric(0))
  tmpGraph[1:numb_terms, 3] <- edges
  tmpGraph[, 1] <- new_central_term
  tmpGraph[, 2] <- nodes
  tmpGraph[, 3] <- edges
  # Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[,
                                                               1]), ])
}


# set seed for graph plot
set.seed(1)
# Create the graph object as undirected graph
graphNetwork <- graph.data.frame(resultGraph, directed = F)
# Assign colors to nodes (search term blue, others orange)
#V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == central_term, 'cornflowerblue', 'orange')
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == central_term, 'cornflowerblue',ifelse(grepl("Topic", V(graphNetwork)$name, fixed = TRUE), 'mediumseagreen' ,'orange'))
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
  #main = paste(central_term, ' Graph'),
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



##########################################################################
# Plotting the Topic Proportions over time
library(ggplot2)
library(reshape2)
library(TraMineR)
library(RColorBrewer) 

nb.cols <- K
# extend any list of colors
# available color palettes: http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# append month information for aggregation
textdata_new$month <- format(as.Date(textdata_new$date), "%Y %m")
#textdata_new$month <- paste0(substr(textdata_new$date, 0, 7), "0")

# get mean topic proportions per month
topic_proportion_per_month <- aggregate(theta, by = list(month = textdata_new$month), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_month)[2:(K+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_month, id.vars = "month")

# plot topic proportions per month as bar plot
ggplot(vizDataFrame, aes(x=month, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = mycolors, name = "Topics:") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 11)) + 
  guides(fill=guide_legend(ncol = 1))



########################################################################
# Plotting the Topic Proportions over time 

# Splitting of the period into 2 or 4 intervals:
numb_of_intervals <- 4

# append year information for aggregation
if (numb_of_intervals == 2) {
  textdata_new$period <- ifelse(substr(textdata_new$date, 0, 4) == "2019" | substr(textdata_new$date, 0, 4) == "2020", "2019 12 - 2020 12", "2021 01 - 2022 03")
  mycolors <- c("coral1", "cyan3")
} else if (numb_of_intervals == 4) {
  textdata_new$period <- ifelse(substr(textdata_new$date, 0, 7) < "2020-07", "2019 12 - 2020 06", 
                                  ifelse(substr(textdata_new$date, 0, 7) < "2021-01", "2020 07 - 2020 12",
                                         ifelse(substr(textdata_new$date, 0, 7) < "2021-07", "2021 01 - 2021 06","2021 07 - 2022 03")))
  mycolors <- c("coral1", "olivedrab4", "cyan3", "mediumorchid")
} else print("Unvalid number of intervals")

# get mean topic proportions per year
topic_proportion_per_period <- aggregate(theta, by = list(period = textdata_new$period), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_period)[2:(K+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(cbind(data.frame(topic_proportion_per_period), period = factor(1:numb_of_intervals)), variable.name = "topic", id.vars = "period") 

# Plotten
ggplot(data = vizDataFrame, aes(topic, value, fill = period), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  scale_fill_manual(values = mycolors, name = "Periods:") +
  facet_wrap(~ period, ncol = numb_of_intervals) + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 11))
