# Plots with word frequencies in corpus

# Create DTM, but remove terms which occur in less than 0,1% of all documents
DTM <- corpus_tokens %>%
  dfm() %>%
  dfm_trim(min_docfreq = 0.001, max_docfreq = 0.99, docfreq_type = "prop")
# have a look at the number of documents and terms in the matrix
dim(DTM)

# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
#textdata_new <- textdata_new[sel_idx, ]


terms_to_delete <- c("â")
DTM <- DTM[,!(colnames(DTM) %in% terms_to_delete)]


# Show most used terms
topfeatures(DTM, 100, decreasing = TRUE)

#########################################################################
# Plot wordcloud with most frequent words in corpus
require(wordcloud2)
top20terms <- topfeatures(DTM, 100, decreasing = TRUE)[1:30]
words <- names(top20terms)
# visualize the terms as wordcloud
wordcloud2(data.frame(words, top20terms), shuffle = FALSE)

#########################################################################
# Plot frequency of selected words over time


# check if terms exists in DTM before processing
DTM[,"protest"]
# terms:
# high word frequencies (> 5000): government, lockdown, hospital, infection, test, protest

# Impfung, Maßnahmen, Soziales/Soziale Interaktion/wie Menschen damit umgegangen sind
# Nebenerscheinungen: Hamsterkäufe, Demos, 

# Maßnahmen: mask, distance, test, vaccine/vaccination/vaccinate, wash
# Allgemeines: government, lockdown, demonstrate/demonstration/resistance, science, who
# Krankheit: infection, symptom, test, antigen, pcr, proof, rapid, certificate, quarantine, 
#           center, recover, result, hospital, ventilator, intensive, disease, mask, spread
# different vaccines: biontech, vaccine/vaccination/vaccinate, moderna, pfizer, oxford, astrazeneca, booster
# different virus names: Covid, Covid-19, Coronavirus, Corona, Virus, variant, pandemic
# variants of virus: alpha (uk), beta (africa), gamma (brazil), delta (india), omicron, variant
# politics: johnson, trump, biden, putin, jinping, ghebreyesus (who), merkel, who
# country: china, america, uk, usa, europe, germany, india, africa, australia, russia, italy

pos_colors <- c('black','red','blue','green', "magenta", "cyan", "orange","salmon4", "lightskyblue1", "gray") 
terms_to_observe <- c("coronavirus", "covid-19","sars-cov-2", "covid", "pandemic", "virus")
term_colors <- pos_colors[1:length(terms_to_observe)]

DTM_reduced <- as.matrix(DTM[,terms_to_observe])

# Monat des Artikels abspeichern für Aggregation
textdata_new$month <- substr(textdata_new$date,0,7)

counts_per_month <- aggregate(DTM_reduced, by = list(months = textdata_new$month),sum)

# give x and y values beautiful names
months <- counts_per_month$months
frequencies <- counts_per_month[, terms_to_observe]
rownames(frequencies) <- months

#plot multiple frequencies
matplot(frequencies, type = "l", col=term_colors, ylab = "word frequencies",xaxt='n')
axis(side=1,at=1:nrow(frequencies),tick=1:28,labels=months,las=2)
# add legend to the plot
l <- length(terms_to_observe)

legend("topright",legend = terms_to_observe, col=term_colors, text.col=term_colors, lty = 1:l)


#dev.off()



#########################################################################
# Plot (relative) frequency of selected words in a piechart

# virus names:
#words <- c("coronavirus", "covid-19", "sars-cov-2", "covid", "pandemic", "virus")
# variants of virus:
#words <- c("alpha", "beta", "gamma", "delta", "omicron")
# countries:
#words <- c("china", "uk", "europe", "america", "australia", "india", "russia", "usa")
# politicians:
# words <- c("johnson", "biden", "trump", "putin", "jinping", "merkel", "macron")
# vaccines:
words <- c("astrazeneca", "oxford", "biontech", "pfizer", "moderna", "janssen", "novavax", "sputnik")
word_cnt <- numeric(length(words))
for (i in 1:nrow(textdata_new)){ 
  word_counts <- str_count(tolower(textdata_new$text[i]), words)
  word_counts_title <- str_count(tolower(textdata_new$title[i]), words)
  
  word_cnt <- word_cnt + (word_counts + word_counts_title)
}

# list with possible colors:
pos_colors <- c('red','gray','green', "magenta", "orange", "cyan", "blue", "salmon4", "lightskyblue1", "gray") 
# select colors according to number of selected words
term_colors <- pos_colors[1:length(words)]

# alternative for color selection:
nb.cols <- K
# extend any list of colors
# available color palettes: http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


# calculate relative word frequencies
sum_covid_words <- sum(word_cnt)
word_cnt_percentage <- word_cnt/sum_covid_words

# save absolute and relative word frequencies in dataframe
covid_words <- data.frame(words, word_cnt, word_cnt_percentage)

# Processing of data for the creation of the pie chart
piedata <- covid_words %>%
  dplyr::arrange(desc(words)) %>%
  dplyr::mutate(Position = (cumsum(word_cnt_percentage)-0.5*word_cnt_percentage))

# Plot piechart
ggplot(piedata, aes("", word_cnt_percentage, fill = words)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values=term_colors) + 
  theme_void() +
  geom_text(aes(y=Position, label = round(word_cnt_percentage*100,2)), color = "black", size = 6) + labs(fill = "Vaccines:")


# alternative: plot word frequencies as barplot
#ggplot(covid_words, aes(words, word_cnt)) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.2)) +
  #scale_y_continuous(name = "Number of articles") + labs(title = "Frequency of Covid-words in Articles")
